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

int GetRangeSize(const SourceManager &Sources, const CharSourceRange &Range,
                 const LangOptions &LangOpts) {
  SourceLocation SpellingBegin = Sources.getSpellingLoc(Range.getBegin());
  SourceLocation SpellingEnd = Sources.getSpellingLoc(Range.getEnd());
  std::pair<FileID, unsigned> Start = Sources.getDecomposedLoc(SpellingBegin);
  std::pair<FileID, unsigned> End = Sources.getDecomposedLoc(SpellingEnd);
  if (Start.first != End.first)
    return -1;
  if (Range.isTokenRange())
    End.second += Lexer::MeasureTokenLength(SpellingEnd, Sources, LangOpts);
  return End.second - Start.second;
}

std::string NormalizeFilePath(const std::string &path) {
  llvm::SmallString<128> normalized(path);
  llvm::sys::path::remove_dots(normalized, /*remove_dot_dot=*/true);

  // Canonicalize directory separators (forward slashes considered canonical.)
  std::replace(normalized.begin(), normalized.end(), '\\', '/');

  return normalized.str().str();
}

bool IsAssociatedInclude(std::string_view Src, std::string_view Include) {
  return llvm::sys::fs::equivalent(Src, Include);
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

// class FunctionDeclMatchHandler : public MatchFinder::MatchCallback {
// public:
//   FunctionDeclMatchHandler(ReplacementsMap &Replacements, CompilerInstance
//   &CI)
//       : Replacements{Replacements}, CI{CI} {}
//
//   void run(const MatchFinder::MatchResult &Result) override {
//     auto Decl = Result.Nodes.getNodeAs<clang::CXXMethodDecl>("fnDecl");
//
//     auto &&SM = Result.SourceManager;
//
//     if (std::string FileName =
//             NormalizeFilePath(SM->getFilename(Decl->getBeginLoc()).str());
//         !std::empty(FileName)) {
//
//       auto Expansion =
//           SM->getSLocEntry(SM->getFileID(Decl->getSourceRange().getBegin()))
//               .getExpansion();
//
//       if (Decl->getParent() && !Decl->getParent()->isLambda() &&
//           Decl->getCanonicalDecl() != Decl &&
//           !Expansion.isMacroArgExpansion()) {
//
//         if (!Result.Context->getRawCommentForDeclNoCache(Decl)) {
//           auto &&DE = Result.SourceManager->getDiagnostics();
//           const unsigned ID = DE.getCustomDiagID(
//               clang::DiagnosticsEngine::Warning, "No Comment!");
//
//           DE.Report(Decl->getBeginLoc(), ID)
//               << "Add " << FunctionDefCommentHeader
//               << FixItHint::CreateInsertion(Decl->getBeginLoc(),
//                                             FunctionDefCommentHeader);
//
//           auto err = Replacements[FileName.c_str()].add(
//               CreateReplacementFromSourceLocation(*SM, Decl->getBeginLoc(),
//               0,
//                                                   FunctionDefCommentHeader));
//
//           llvm::errs() << Decl->getNameAsString() << "\n";
//         } else {
//           // auto RawComment =
//           // Result.Context->getRawCommentForDeclNoCache(Decl); auto Comment
//           =
//           // RawComment->getRawText(*SM);
//
//           // if (!Comment.contains("/**")) {
//           //   llvm::errs() << Comment << "\n"
//           //                << Decl->getNameAsString() << "\n\n";
//
//           //  auto Range =
//           // CharSourceRange::getTokenRange(RawComment->getSourceRange());
//
//           //  Replacements[FileName.c_str()].add(
//           //      CreateReplacementFromSourceLocation(
//           //          *SM, RawComment->getBeginLoc(),
//           //          GetRangeSize(*SM, Range, CI.getLangOpts()), ""));
//           //}
//         }
//       }
//     }
//   }
//
// private:
//   ReplacementsMap &Replacements;
//   CompilerInstance &CI;
// };

// class XASTConsumer : public ASTConsumer {
// public:
//   XASTConsumer(ReplacementsMap &Replacements, CompilerInstance &CI)
//       : Replacements{Replacements}, Handler{Replacements, CI} {
//
//     Matcher.addMatcher(cxxMethodDecl(isDefinition(), unless(isImplicit()),
//                                      unless(isExpansionInSystemHeader()))
//                            .bind("fnDecl"),
//                        &Handler);
//   }
//
//   void HandleTranslationUnit(ASTContext &Context) override {
//     Matcher.matchAST(Context);
//   }
//
// private:
//   //FunctionDeclMatchHandler Handler;
//   MatchFinder Matcher;
//   ReplacementsMap &Replacements;
// };

class Find_Includes : public PPCallbacks {
public:
  Find_Includes(CompilerInstance &CI, ReplacementsMap &Replacements)
      : CI{CI}, Replacements{Replacements} {}

  void InclusionDirective(SourceLocation HashLoc, const Token &IncludeTok,
                          StringRef FileName, bool IsAngled,
                          CharSourceRange FilenameRange,
                          OptionalFileEntryRef File, StringRef SearchPath,
                          StringRef RelativePath, const Module *Imported,
                          SrcMgr::CharacteristicKind FileType) override {

    auto &DE = CI.getDiagnostics();
    auto &SM = CI.getSourceManager();

    const unsigned ID = DE.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                                           "Incorrect Include Style");

    auto SrcFileName = SM.getFilename(FilenameRange.getBegin());

    SmallString<512> Src = SrcFileName;
    llvm::sys::path::replace_extension(Src, ".h");

    SrcFileName = NormalizeFilePath(SrcFileName.str()).c_str();

    if (llvm::sys::fs::equivalent(Src, SearchPath + "/" + FileName)) {
      if (SM.isInMainFile(HashLoc)) {
        ++I;

        std::string Replacement =
            (llvm::Twine("\"") + llvm::sys::path::filename(FileName) + "\"")
                .str();

        if (I > 1) {
          DE.Report(FilenameRange.getBegin(), ID) << FixItHint::CreateRemoval(
              clang::SourceRange(HashLoc, FilenameRange.getEnd()));

          auto Range = CharSourceRange::getTokenRange(
              clang::SourceRange(HashLoc, FilenameRange.getEnd()));

          auto err = Replacements[SrcFileName.str()].add(
              CreateReplacementFromSourceLocation(
                  SM, HashLoc, GetRangeSize(SM, Range, CI.getLangOpts()), ""));

        } else if (llvm::sys::path::filename(FileName) != FileName) {
          {
            DE.Report(FilenameRange.getBegin(), ID)
                << FixItHint::CreateReplacement(FilenameRange.getAsRange(),
                                                Replacement);

            auto err = Replacements[SrcFileName.str()].add(
                CreateReplacementFromSourceLocation(
                    SM, FilenameRange.getBegin(),
                    GetRangeSize(SM, FilenameRange, CI.getLangOpts()),
                    Replacement));
          }
        }

      } else {
        if (FileType == SrcMgr::CharacteristicKind::C_User &&
            !SrcFileName.ends_with(".cpp")) {
          DE.Report(FilenameRange.getBegin(), ID) << FixItHint::CreateRemoval(
              clang::SourceRange(HashLoc, FilenameRange.getEnd()));

          auto Range = CharSourceRange::getTokenRange(
              clang::SourceRange(HashLoc, FilenameRange.getEnd()));

          auto err = Replacements[SrcFileName.str()].add(
              CreateReplacementFromSourceLocation(
                  SM, HashLoc, GetRangeSize(SM, Range, CI.getLangOpts()), ""));
        }
      }
    } else if (FileType == SrcMgr::CharacteristicKind::C_User) {

      if (!llvm::sys::fs::equivalent(Src, SearchPath + "/" + FileName)) {
        // if (SM.isInMainFile(HashLoc)) {

        SmallString<512> path = SearchPath;
        path += "/";
        path += FileName;
        llvm::sys::fs::make_absolute(path);
        llvm::sys::path::remove_dots(path, true);

        path = llvm::sys::path::convert_to_slash(
            path, llvm::sys::path::Style::windows_backslash);

        if (auto pos = path.find(".conan"); pos == -1)
          if (auto pos = path.find("AfxCompat.h"); pos == -1)
            if (auto pos = path.find("AfxExCompat.h"); pos == -1)
              if (auto pos = path.find("BCGIncludeFull.h"); pos == -1)

                if (auto pos = path.find("/Source/"); pos != -1) {
                  path = path.substr(pos + 8);

                  size_t offset =
                      (path.startswith("2D/") || path.startswith("3D/") ||
                       path.startswith("UI/"))
                          ? 3
                      : path.startswith("API/")    ? 4
                      : path.startswith("Text/")   ? 5
                      : path.startswith("Tests/")  ? 6
                      : path.startswith("Render/") ? 7
                                                   : 0;
                  path = path.substr(offset);

                  if (auto pos = path.find("/"); pos != -1) {

                    offset = (path.substr(0, pos + 1) ==
                                      path.substr(pos + 1, pos + 1)
                                  ? pos + 1
                                  : 0);

                    path = path.substr(offset);

                    std::string Replacement =
                        (llvm::Twine("<") + path + ">").str();

                    if (FileName != path || !IsAngled) {
                      DE.Report(FilenameRange.getBegin(), ID)
                          << FixItHint::CreateReplacement(
                                 FilenameRange.getAsRange(), Replacement);

                      auto err = Replacements[SrcFileName.str()].add(
                          CreateReplacementFromSourceLocation(
                              SM, FilenameRange.getBegin(),
                              GetRangeSize(SM, FilenameRange, CI.getLangOpts()),
                              Replacement));
                    }
                  }
                }
        //}
      }
    }
  }

private:
  CompilerInstance &CI;
  ReplacementsMap &Replacements;
  size_t I = 0;
};

// For each source file provided to the tool, a new FrontendAction is created.
// class XFrontendAction : public ASTFrontendAction {
class XFrontendAction : public PreprocessOnlyAction {

public:
  // std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
  // StringRef File) override {
  // return std::make_unique<XASTConsumer>(Replacements, CI);
  //}

  bool BeginSourceFileAction(CompilerInstance &CI) override {

    CI.getLangOpts().CommentOpts.ParseAllComments = true;

    Preprocessor &PP = CI.getPreprocessor();
    PP.addPPCallbacks(std::make_unique<Find_Includes>(CI, Replacements));

    return true;
  }

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
};

class XFrontendActionFactory : public tooling::FrontendActionFactory {
public:
  std::unique_ptr<FrontendAction> create() override {
    return std::make_unique<XFrontendAction>();
  }
};

int main(int argc, const char **argv) {
  llvm::sys::PrintStackTraceOnErrorSignal(argv[0]);

  const char *Overview = R"(
  check includes across a whole C/C++ project
  include-fixer --filter=CompressPathTest.cpp
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

  tooling::TranslationUnitReplacements TUR = MergeReplacements(TURs);
  yaml::Output YAML(llvm::outs());
  YAML << TUR;

  if (Err) {
    llvm::errs() << llvm::toString(std::move(Err)) << "\n";
  }
}