#include "clang/AST/AST.h"
#include "clang/AST/ASTContext.h"
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

// StatementMatcher LoopMatcher =
//     forStmt(hasLoopInit(declStmt(hasSingleDecl(varDecl(hasInitializer(
//                 integerLiteral(equals(0))))))) /*, isExpansionInMainFile()*/)
//         .bind("forLoop");

// class LoopPrinter : public MatchFinder::MatchCallback {
// public:
//   virtual void run(const MatchFinder::MatchResult &Result)  {
//     if (const ForStmt *FS =
//     Result.Nodes.getNodeAs<clang::ForStmt>("forLoop"))
//       FS->dump();
//   }
// };

//// Apply a custom category to all command-line options so that they are the
//// only ones displayed.
// static llvm::cl::OptionCategory MyToolCategory("my-tool options");
//
//// CommonOptionsParser declares HelpMessage with a description of the common
//// command-line options related to the compilation database and input files.
//// It's nice to have this help message in all tools.
// static cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);
//
//// A help message for this specific tool can be added afterwards.
// static cl::extrahelp MoreHelp("\nMore help text...\n");

// class FunctionDeclMatchHandler : public MatchFinder::MatchCallback {
// public:
//   void run(const MatchFinder::MatchResult &Result) override {}
// };
//
// class XASTConsumer : public ASTConsumer {
// public:
//   XASTConsumer() {
//     Matcher.addMatcher(functionDecl(isDefinition(), unless(isImplicit()),
//                                     isExpansionInMainFile())
//                            .bind("fnDecl"),
//                        &Handler);
//
//     // Matcher.addMatcher(declRefExpr().bind("declRef"), &Handler);
//     // Matcher.addMatcher(memberExpr().bind("memberRef"), &Handler);
//     // Matcher.addMatcher(cxxConstructExpr().bind("cxxConstructExpr"),
//     // &Handler);
//   }
//
//   void HandleTranslationUnit(ASTContext &Context) override {
//     // Matcher.matchAST(Context);
//     // Handler.finalize(Context.getSourceManager());
//   }
//
// private:
//   FunctionDeclMatchHandler Handler;
//   MatchFinder Matcher;
// };
//
//// For each source file provided to the tool, a new FrontendAction is created.
// class XFrontendAction : public ASTFrontendAction {
// public:
//   std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance & /*CI*/,
//                                                  StringRef /*File*/) override
//                                                  {
//     return std::make_unique<XASTConsumer>();
//   }
// };
//
// class XFrontendActionFactory : public tooling::FrontendActionFactory {
// public:
//   std::unique_ptr<FrontendAction> create() override {
//     return std::make_unique<XFrontendAction>();
//   }
// };

class MyFixItOptions : public clang::FixItOptions {
public:
  MyFixItOptions() {
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

class Find_Includes : public PPCallbacks {
private:
  SourceManager &SM;
  DiagnosticsEngine &DE;
  size_t I = 0;

public:
  Find_Includes(SourceManager &SM, DiagnosticsEngine &DE) : SM{SM}, DE{DE} {}

  void InclusionDirective(SourceLocation HashLoc, const Token &IncludeTok,
                          StringRef FileName, bool IsAngled,
                          CharSourceRange FilenameRange,
                          OptionalFileEntryRef File, StringRef SearchPath,
                          StringRef RelativePath, const Module *Imported,
                          SrcMgr::CharacteristicKind FileType) override {

    SmallString<512> Src = SM.getFilename(FilenameRange.getBegin());
    llvm::sys::path::replace_extension(Src, ".h");

    if (FileType == SrcMgr::CharacteristicKind::C_User) {
      if (llvm::sys::fs::equivalent(Src, SearchPath + "/" + FileName) &&
          SM.isInMainFile(HashLoc)) {

        ++I;

        std::string Replacement =
            (llvm::Twine("\"") + llvm::sys::path::filename(FileName) + "\"")
                .str();

        // llvm::outs() << FileName << " " << Replacement << " "
        //              << FilenameRange.getAsRange().printToString(SM) << "\n";

        const unsigned ID = DE.getCustomDiagID(
            clang::DiagnosticsEngine::Warning, "I findz a badness");

        if (I > 1)
          DE.Report(FilenameRange.getBegin(), ID)
              << SM.getFilename(FilenameRange.getBegin()) << Replacement
              << FixItHint::CreateRemoval(
                     clang::SourceRange(HashLoc, FilenameRange.getEnd()));
        else if (llvm::sys::path::filename(FileName) != FileName) {
          DE.Report(FilenameRange.getBegin(), ID)
              << SM.getFilename(FilenameRange.getBegin()) << Replacement
              << FixItHint::CreateReplacement(FilenameRange.getAsRange(),
                                              Replacement);
        }
      }
    }
  }
};

class Include_Matching_Action : public PreprocessOnlyAction {
private:
  std::unique_ptr<clang::FixItRewriter> Rewriter;
  MyFixItOptions Options;

private:
  bool BeginSourceFileAction(CompilerInstance &CI) override {
    Preprocessor &PP = CI.getPreprocessor();

    Rewriter = std::make_unique<clang::FixItRewriter>(
        CI.getDiagnostics(), CI.getSourceManager(), CI.getLangOpts(), &Options);

    PP.addPPCallbacks(std::make_unique<Find_Includes>(CI.getSourceManager(),
                                                      CI.getDiagnostics()));
    return true;
  }

  void EndSourceFileAction() override { Rewriter->WriteFixedFiles(); }
};

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

  // auto Err =
  //     Executor->get()->execute(std::make_unique<XFrontendActionFactory>());

  auto Err = Executor->get()->execute(
      newFrontendActionFactory<Include_Matching_Action>());

  if (Err) {
    llvm::errs() << llvm::toString(std::move(Err)) << "\n";
  }
}