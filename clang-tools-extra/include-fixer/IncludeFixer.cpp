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

using namespace clang::tooling;
using namespace clang::ast_matchers;
using namespace clang;
using namespace llvm;

class XFixItOptions : public clang::FixItOptions {
public:
  XFixItOptions() {
    InPlace = true;
    // InPlace = false;
    FixWhatYouCan = false;
    FixOnlyWarnings = false;
    Silent = false;
  }

  std::string RewriteFilename(const std::string &Filename, int &fd) override {
    const auto NewFilename = Filename + ".fixed";
    llvm::errs() << "Rewriting FixIts from " << Filename << " to "
                 << NewFilename << "\n";
    fd = -1;
    return NewFilename;
  }
};

class FunctionDeclMatchHandler : public MatchFinder::MatchCallback {
public:
  void run(const MatchFinder::MatchResult &Result) override {
    auto Decl = Result.Nodes.getNodeAs<clang::CXXMethodDecl>("fnDecl");

    auto &&SM = Result.SourceManager;

    auto Expansion =
        SM->getSLocEntry(SM->getFileID(Decl->getSourceRange().getBegin()))
            .getExpansion();

    if (Decl->getParent() && !Decl->getParent()->isLambda() &&
        Decl->getCanonicalDecl() != Decl && !Expansion.isMacroArgExpansion() &&
        !Expansion.isMacroBodyExpansion() &&
        !Expansion.isFunctionMacroExpansion()) {

      // auto &&SM = Result.SourceManager;

      if (!Result.Context->getRawCommentForDeclNoCache(Decl)) {
        auto &&DE = Result.SourceManager->getDiagnostics();
        const unsigned ID = DE.getCustomDiagID(
            clang::DiagnosticsEngine::Warning, "No Comment!");

        std::string Code =
            "\n//"
            "-----------------------------------------------------"
            "-------------------------\n/**\n  \n*/\n//---\n";
        DE.Report(Decl->getBeginLoc(), ID)
            << "Add " << Code
            << FixItHint::CreateInsertion(Decl->getBeginLoc(), Code);

        // llvm::outs() << Decl->getNameAsString() << "\n";
      }
    }
  }
};

class XASTConsumer : public ASTConsumer {
public:
  XASTConsumer() {

    Matcher.addMatcher(cxxMethodDecl(isDefinition(), unless(isImplicit()),
                                     isExpansionInMainFile())
                           .bind("fnDecl"),
                       &Handler);
  }

  void HandleTranslationUnit(ASTContext &Context) override {
    // Matcher.matchAST(Context);
  }

private:
  FunctionDeclMatchHandler Handler;
  MatchFinder Matcher;
};

// For each source file provided to the tool, a new FrontendAction is created.
class XFrontendAction : public ASTFrontendAction {
public:
  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 StringRef File) override {
    return std::make_unique<XASTConsumer>();
  }

  bool BeginSourceFileAction(CompilerInstance &CI) override {
    // Preprocessor &PP = CI.getPreprocessor();

    Rewriter = std::make_unique<clang::FixItRewriter>(
        CI.getDiagnostics(), CI.getSourceManager(), CI.getLangOpts(), &Options);

    return true;
  }

  void EndSourceFileAction() override {
    // Rewriter->WriteFixedFiles();
  }

private:
  std::unique_ptr<clang::FixItRewriter> Rewriter;
  XFixItOptions Options;
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

  if (Err) {
    llvm::errs() << llvm::toString(std::move(Err)) << "\n";
  }
}