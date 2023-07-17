#include "clang/AST/AST.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Comment.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Driver/Options.h"
#include "clang/Frontend/ASTConsumers.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Index/USRGeneration.h"
#include "clang/Lex/PPCallbacks.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Rewrite/Frontend/FixItRewriter.h"
#include "clang/Tooling/AllTUsExecution.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Execution.h"
#include "clang/Tooling/ReplacementsYaml.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Signals.h"

#include <functional>
#include <map>
#include <memory>
#include <mutex>
#include <tuple>

using namespace clang::tooling;
using namespace clang::ast_matchers;
using namespace clang;
using namespace llvm;

std::mutex MU;
std::vector<tooling::TranslationUnitReplacements> TURs;

using ReplacementsMap = std::map<std::string, tooling::Replacements>;

std::string_view FunctionDefCommentHeader =
    "\n//"
    "-----------------------------------------------------"
    "-------------------------\n/**\n  \n*/\n//---\n";

std::string NormalizeFilePath(const std::string &path) {
  llvm::SmallString<128> normalized(path);
  llvm::sys::path::remove_dots(normalized, /*remove_dot_dot=*/true);

  // Canonicalize directory separators (forward slashes considered canonical.)
  std::replace(normalized.begin(), normalized.end(), '\\', '/');

  return normalized.str().str();
}

tooling::Replacement
CreateReplacementFromSourceLocation(const SourceManager &Sources,
                                    SourceLocation Start, unsigned Length,
                                    StringRef ReplacementText) {
  const auto &&[FileID, Offset] = Sources.getDecomposedLoc(Start);
  const FileEntry *Entry = Sources.getFileEntryForID(FileID);
  auto FilePath =
      std::string(Entry ? NormalizeFilePath(Entry->getName().str()) : "");
  return Replacement{FilePath, Offset, Length, ReplacementText};
}

tooling::TranslationUnitReplacements MergeReplacements(
    const std::vector<tooling::TranslationUnitReplacements> &TURs) {
  tooling::TranslationUnitReplacements Result;

  for (auto &TUR : TURs) {

    Result.Replacements.insert(std::end(Result.Replacements),
                               std::begin(TUR.Replacements),
                               std::end(TUR.Replacements));
  }

  std::sort(std::begin(Result.Replacements), std::end(Result.Replacements),
            [](const Replacement &lhs, const Replacement &rhs) {
              return std::tuple{lhs.getFilePath(), lhs.getOffset(),
                                lhs.getLength()} <=
                     std::tuple{rhs.getFilePath(), rhs.getOffset(),
                                rhs.getLength()};
            });

  Result.Replacements.erase(
      std::unique(std::begin(Result.Replacements),
                  std::end(Result.Replacements),
                  [](const Replacement &lhs, const Replacement &rhs) {
                    return std::tuple{lhs.getFilePath(), lhs.getOffset(),
                                      lhs.getLength()} ==
                           std::tuple{rhs.getFilePath(), rhs.getOffset(),
                                      rhs.getLength()};
                  }),
      std::end(Result.Replacements));

  return Result;
}

class FunctionDeclMatchHandler : public MatchFinder::MatchCallback {
public:
  FunctionDeclMatchHandler(ReplacementsMap &Replacements)
      : Replacements{Replacements} {}

  void run(const MatchFinder::MatchResult &Result) override {
    auto Decl = Result.Nodes.getNodeAs<clang::CXXMethodDecl>("fnDecl");

    auto &&SM = Result.SourceManager;

    if (std::string FileName =
            NormalizeFilePath(SM->getFilename(Decl->getBeginLoc()).str());
        !std::empty(FileName)) {

      auto Expansion =
          SM->getSLocEntry(SM->getFileID(Decl->getSourceRange().getBegin()))
              .getExpansion();

      // if (Decl->getParent() && !Decl->getParent()->isLambda() &&
      //     Decl->getCanonicalDecl() != Decl)

      if (Decl->getParent() && !Decl->getParent()->isLambda() &&
          Decl->getCanonicalDecl() != Decl &&
          !Expansion.isMacroArgExpansion() // &&
          //! Expansion.isMacroBodyExpansion() &&
          //! Expansion.isFunctionMacroExpansion()
      ) {

        // llvm::errs() << FileName << " " << Decl->getQualifiedNameAsString()
        //              << "\n";

        if (!Result.Context->getRawCommentForDeclNoCache(Decl)) {
          auto &&DE = Result.SourceManager->getDiagnostics();
          const unsigned ID = DE.getCustomDiagID(
              clang::DiagnosticsEngine::Warning, "No Comment!");

          DE.Report(Decl->getBeginLoc(), ID)
              << "Add " << FunctionDefCommentHeader
              << FixItHint::CreateInsertion(Decl->getBeginLoc(),
                                            FunctionDefCommentHeader);

          Replacements[FileName.c_str()].add(
              CreateReplacementFromSourceLocation(*SM, Decl->getBeginLoc(), 0,
                                                  FunctionDefCommentHeader));

          // Replacements[FileName.c_str()].add(Replacement(
          //     *SM, Decl->getBeginLoc(), 0, FunctionDefCommentHeader));
        }
      }
    }
  }

private:
  ReplacementsMap &Replacements;
};

class XASTConsumer : public ASTConsumer {
public:
  XASTConsumer(ReplacementsMap &Replacements)
      : Replacements{Replacements}, Handler{Replacements} {

    Matcher.addMatcher(cxxMethodDecl(isDefinition(), unless(isImplicit()),
                                     // isExpansionInMainFile(),
                                     unless(isExpansionInSystemHeader()))
                           .bind("fnDecl"),
                       &Handler);
  }

  void HandleTranslationUnit(ASTContext &Context) override {
    Matcher.matchAST(Context);
  }

private:
  FunctionDeclMatchHandler Handler;
  MatchFinder Matcher;
  ReplacementsMap &Replacements;
};

// For each source file provided to the tool, a new FrontendAction is created.
class XFrontendAction : public ASTFrontendAction {

public:
  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 StringRef File) override {
    return std::make_unique<XASTConsumer>(Replacements);
  }

  bool BeginSourceFileAction(CompilerInstance &CI) override { return true; }

  void EndSourceFileAction() override {
    tooling::TranslationUnitReplacements TUR;

    for (const auto &[Key, Value] : Replacements)
      for (const auto &Entry : Value) {
        TUR.Replacements.push_back(Entry);
      }

    std::scoped_lock Lock{MU};
    TURs.push_back(TUR);
  }

private:
  ReplacementsMap Replacements;
  // XFixItOptions Options;
};

class XFrontendActionFactory : public tooling::FrontendActionFactory {
public:
  std::unique_ptr<FrontendAction> create() override {
    return std::make_unique<XFrontendAction>();
  }
};

// llvm::StringSet VisitedHeaders;
//
// class Find_Includes : public PPCallbacks {
// private:
//   SourceManager &SM;
//   DiagnosticsEngine &DE;
//   size_t I = 0;
//
// public:
//   Find_Includes(SourceManager &SM, DiagnosticsEngine &DE) : SM{SM}, DE{DE}
//   {}
//
//   void InclusionDirective(SourceLocation HashLoc, const Token &IncludeTok,
//                           StringRef FileName, bool IsAngled,
//                           CharSourceRange FilenameRange,
//                           OptionalFileEntryRef File, StringRef SearchPath,
//                           StringRef RelativePath, const Module *Imported,
//                           SrcMgr::CharacteristicKind FileType) override {
//
//     const unsigned ID =
//     DE.getCustomDiagID(clang::DiagnosticsEngine::Warning,
//                                            "Incorrect Include Style");
//
//     auto SrcFileName = SM.getFilename(FilenameRange.getBegin());
//
//     SmallString<512> Src = SrcFileName;
//     llvm::sys::path::replace_extension(Src, ".h");
//
//     if (FileType == SrcMgr::CharacteristicKind::C_User) {
//
//       // if (!llvm::sys::fs::equivalent(Src, SearchPath + "/" + FileName))
//       {
//       //   if (SM.isInMainFile(HashLoc) &&
//       //       (SrcFileName.contains("\\Source\\2D") ||
//       //        SrcFileName.contains("\\Source\\3D")) &&
//       //       !SrcFileName.contains("\\ProtSys\\") &&
//       //       !SrcFileName.contains("\\Impl\\") &&
//       !FileName.contains("..")
//       &&
//       //       !FileName.contains("\\Impl\\") &&
//       //       !SrcFileName.contains("\\RasterOut\\")) {
//
//       //    // llvm::outs() << FileName << "\n";
//       //    // llvm::outs() << SM.getFilename(FilenameRange.getBegin()) <<
//       //    "\n\n";
//
//       //    SmallString<512> path = SearchPath;
//       //    path += "/";
//       //    path += FileName;
//
//       //    llvm::sys::fs::make_absolute(path);
//       //    path = llvm::sys::path::convert_to_slash(
//       //        path, llvm::sys::path::Style::windows_backslash);
//
//       //    if (auto pos = path.find("/Source/"); pos != -1) {
//
//       //      path = path.substr(pos + 8);
//
//       //      const size_t offset =
//       //          path.startswith("2D/") || path.startswith("3D/") ? 3 : 0;
//       //      path = path.substr(offset);
//
//       //      std::string Replacement = (llvm::Twine("<") + path +
//       ">").str();
//       //      // std::string Replacement = (llvm::Twine("<") + path +
//       //      ">").str();
//
//       //      if (FileName != path && !path.startswith("UI/Include/"))
//       //        DE.Report(FilenameRange.getBegin(), ID)
//       //            <<
//       FixItHint::CreateReplacement(FilenameRange.getAsRange(),
//       //                                            Replacement);
//       //    }
//       //  }
//       //}
//
//       if (llvm::sys::fs::equivalent(Src, SearchPath + "/" + FileName)) {
//
//         if (SM.isInMainFile(HashLoc)) {
//           ++I;
//
//           std::string Replacement =
//               (llvm::Twine("\"") + llvm::sys::path::filename(FileName) +
//               "\"")
//                   .str();
//
//           if (I > 1)
//             DE.Report(FilenameRange.getBegin(), ID) <<
//             FixItHint::CreateRemoval(
//                 clang::SourceRange(HashLoc, FilenameRange.getEnd()));
//           else if (llvm::sys::path::filename(FileName) != FileName) {
//             DE.Report(FilenameRange.getBegin(), ID)
//                 << FixItHint::CreateReplacement(FilenameRange.getAsRange(),
//                                                 Replacement);
//           }
//         } else {
//           std::lock_guard lock(Mutex);
//
//           if (auto SrcInclude = SM.getFilename(FilenameRange.getBegin());
//               SrcInclude.ends_with(".h") &&
//               !VisitedHeaders.contains(SrcInclude)) {
//             VisitedHeaders.insert(SrcInclude);
//             DE.Report(FilenameRange.getBegin(), ID) <<
//             FixItHint::CreateRemoval(
//                 clang::SourceRange(HashLoc, FilenameRange.getEnd()));
//           }
//         }
//       }
//     }
//   }
// };
//
// class Include_Matching_Action : public PreprocessOnlyAction {
// private:
//   std::unique_ptr<clang::FixItRewriter> Rewriter;
//   XFixItOptions Options;
//
// private:
//   bool BeginSourceFileAction(CompilerInstance &CI) override {
//     Preprocessor &PP = CI.getPreprocessor();
//
//     Rewriter = std::make_unique<clang::FixItRewriter>(
//         CI.getDiagnostics(), CI.getSourceManager(), CI.getLangOpts(),
//         &Options);
//
//     PP.addPPCallbacks(std::make_unique<Find_Includes>(CI.getSourceManager(),
//                                                       CI.getDiagnostics()));
//     return true;
//   }
//
//   void EndSourceFileAction() override { Rewriter->WriteFixedFiles(); }
// };

int main(int argc, const char **argv) {
  llvm::sys::PrintStackTraceOnErrorSignal(argv[0]);

  const char *Overview = R"(
  check syntax across a whole C/C++ project
  loop-convert --executor=all-TUs compile_commands.json > clangd.dex
  )";

  tooling::ExecutorName.setInitialValue("all-TUs");

  auto Executor = clang::tooling::createExecutorFromCommandLineArgs(
      argc, argv, llvm::cl::getGeneralCategory(), Overview);

  if (!Executor) {
    llvm::errs() << llvm::toString(Executor.takeError()) << "\n";
    return 1;
  }

  auto Err =
      Executor->get()->execute(std::make_unique<XFrontendActionFactory>());

  // auto Err =
  // Executor->get()->execute(newFrontendActionFactory<Include_Matching_Action>());

  tooling::TranslationUnitReplacements TUR = MergeReplacements(TURs);
  yaml::Output YAML(llvm::outs());
  YAML << TUR;

  if (Err) {
    llvm::errs() << llvm::toString(std::move(Err)) << "\n";
  }
}