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

std::string FixupWithCase(StringRef Name) {
  static llvm::Regex Splitter(
      "([a-z0-9A-Z]*)(_+)|([A-Z]?[a-z0-9]+)([A-Z]|$)|([A-Z]+)([A-Z]|$)");

  SmallVector<StringRef, 8> Substrs;
  Name.split(Substrs, "_", -1, false);

  SmallVector<StringRef, 8> Words;
  SmallVector<StringRef, 8> Groups;
  for (auto Substr : Substrs) {
    while (!Substr.empty()) {
      Groups.clear();
      if (!Splitter.match(Substr, &Groups))
        break;

      if (!Groups[2].empty()) {
        Words.push_back(Groups[1]);
        Substr = Substr.substr(Groups[0].size());
      } else if (!Groups[3].empty()) {
        Words.push_back(Groups[3]);
        Substr = Substr.substr(Groups[0].size() - Groups[4].size());
      } else if (!Groups[5].empty()) {
        Words.push_back(Groups[5]);
        Substr = Substr.substr(Groups[0].size() - Groups[6].size());
      }
    }
  }

  if (Words.empty())
    return Name.str();

  SmallString<128> Fixup;

  for (auto const &Word : Words) {
    if (&Word == &Words.front()) {
      Fixup += Word.lower();
    } else {
      Fixup += toupper(Word.front());
      Fixup += Word.substr(1).lower();
    }
  }

  return Fixup.str().str();
}

std::string NormalizeFilePath(const std::string &path) {
  llvm::SmallString<128> normalized(path);
  llvm::sys::path::remove_dots(normalized, /*remove_dot_dot=*/true);
  std::replace(normalized.begin(), normalized.end(), '\\', '/');

  return normalized.str().str();
}

std::string GetSrcFileName(const clang::SourceManager &SM,
                           clang::SourceLocation LOC) {
  const auto SrcFileNameOrigin = SM.getFilename(LOC);

  const auto PresumedLoc = SM.getPresumedLoc(LOC).getFilename();
  const auto SrcFileName = !SrcFileNameOrigin.empty()
                               ? NormalizeFilePath(SrcFileNameOrigin.data())
                           : PresumedLoc ? NormalizeFilePath(PresumedLoc)
                                         : "";

  return SrcFileName;
}

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

tooling::Replacement
CreateReplacementFromSourceLocation(const SourceManager &Sources,
                                    SourceLocation Start, unsigned Length,
                                    StringRef ReplacementText) {
  const auto &&[FileID, Offset] = Sources.getDecomposedLoc(Start);
  Replacement result;

  if (Sources.getFileEntryRefForID(FileID)) {
    const FileEntryRef Entry = *Sources.getFileEntryRefForID(FileID);
    auto FilePath =
        std::string(Entry ? NormalizeFilePath(Entry.getName().str()) : "");
    result = Replacement{FilePath, Offset, Length, ReplacementText};
  } else if (auto FilePath = GetSrcFileName(Sources, Start);
             !FilePath.empty() && Start.isMacroID()) {

    auto Offset = Sources.getFileOffset(
        Sources.getSpellingLoc(Sources.getMacroArgExpandedLocation(Start)));
    result = Replacement{FilePath, Offset, Length, ReplacementText};
  }
  return result;
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

bool IsFormated(const clang::FieldDecl *Decl) {
  return !Decl ||
         (Decl->getName().starts_with("m_") ||
          Decl->getName().starts_with("_") || Decl->getName().ends_with("_"));
}

class FunctionDeclMatchHandler : public MatchFinder::MatchCallback {
public:
  FunctionDeclMatchHandler(ReplacementsMap &Replacements, CompilerInstance &CI)
      : Replacements{Replacements}, CI{CI} {}

  void run(const MatchFinder::MatchResult &Result) override {

    auto *FieldDecl = Result.Nodes.getNodeAs<clang::FieldDecl>("fieldDecl");
    auto *MemberExpr = Result.Nodes.getNodeAs<clang::MemberExpr>("memberExpr");
    auto *CXXConstructorDecl =
        Result.Nodes.getNodeAs<clang::CXXConstructorDecl>("cxxConstructorDecl");

    auto &&SM = *Result.SourceManager;

    const auto Loc = CXXConstructorDecl ? CXXConstructorDecl->getBeginLoc()
                                        : FieldDecl->getBeginLoc();

    const auto SrcFileName = GetSrcFileName(SM, Loc);

    if (SrcFileName.find("/Source/") != std::string::npos) {
      std::vector<clang::SourceRange> Ranges;

      if (CXXConstructorDecl && !CXXConstructorDecl->isDefaulted()) {

        for (auto *I : CXXConstructorDecl->inits()) {

          if (auto *Decl = I->getMember();
              Decl && !I->isInClassMemberInitializer()) {

            if (Decl->getAccess() == clang::AccessSpecifier::AS_private ||
                Decl->getAccess() == clang::AccessSpecifier::AS_private)

              if (!IsFormated(Decl)) {

                if (I->getSourceOrder() != -1)
                  Ranges.emplace_back(I->getSourceRange());

                Ranges.emplace_back(
                    clang::SourceRange(Decl->getLocation(), Decl->getEndLoc()));
              }
          }
        }
      }

      {
        {
          auto &&DE = Result.SourceManager->getDiagnostics();

          auto ID =
              DE.getCustomDiagID(clang::DiagnosticsEngine::Warning, "add m_");

          if (FieldDecl)
            if (auto *Decl = FieldDecl->getCanonicalDecl())
              if (Decl->getAccess() == clang::AccessSpecifier::AS_private ||
                  Decl->getAccess() == clang::AccessSpecifier::AS_private)
                if (Decl && !IsFormated(Decl)) {
                  if (Decl) {
                    Ranges.emplace_back(clang::SourceRange(Decl->getLocation(),
                                                           Decl->getEndLoc()));
                  }
                  if (MemberExpr) {
                    Ranges.emplace_back(MemberExpr->getMemberLoc());
                  }
                }

          for (auto SourceRange : Ranges) {

            auto Range = CharSourceRange::getTokenRange(SourceRange);

            if (SourceRange.getBegin().isMacroID()) {
              // Range = SM.getImmediateExpansionRange(SourceRange.getBegin());
            }

            if (auto SrcFileName = GetSrcFileName(SM, Range.getBegin());
                !std::empty(SrcFileName) &&
                SrcFileName.find("/Source/") != std::string::npos) {

              auto Size = GetRangeSize(SM, Range, CI.getLangOpts());

              if (Size > 0) {

                std::string Code{"m_"};
                Code += std::string_view{SM.getCharacterData(Range.getBegin()),
                                         (size_t)Size};

                DE.Report(Range.getBegin(), ID)
                    << FixItHint::CreateReplacement(Range, Code);

                (void)Replacements[SrcFileName.c_str()].add(
                    CreateReplacementFromSourceLocation(
                        SM, Range.getBegin(),
                        GetRangeSize(SM, Range, CI.getLangOpts()), Code));
              }
            }
          }
        }
      }
    }
  }

private:
  ReplacementsMap &Replacements;
  CompilerInstance &CI;
};

class XASTConsumer : public ASTConsumer {
public:
  XASTConsumer(ReplacementsMap &Replacements, CompilerInstance &CI)
      : Replacements{Replacements}, Handler{Replacements, CI} {

    Matcher.addMatcher(
        traverse(TK_IgnoreUnlessSpelledInSource,
                 memberExpr(hasDeclaration(fieldDecl().bind("fieldDecl")))
                     .bind("memberExpr")),
        &Handler);

    Matcher.addMatcher(
        traverse(TK_IgnoreUnlessSpelledInSource,
                 cxxConstructorDecl().bind("cxxConstructorDecl")),
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

class XFrontendAction : public ASTFrontendAction {
public:
  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 StringRef File) override {
    return std::make_unique<XASTConsumer>(Replacements, CI);
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