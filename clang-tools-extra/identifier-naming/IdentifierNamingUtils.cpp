//#include "clang/AST/AST.h"
//#include "clang/AST/ASTContext.h"
//#include "clang/AST/Comment.h"
//#include "clang/ASTMatchers/ASTMatchFinder.h"
//#include "clang/ASTMatchers/ASTMatchers.h"
//#include "clang/Driver/Options.h"
//#include "clang/Frontend/ASTConsumers.h"
//#include "clang/Frontend/CompilerInstance.h"
//#include "clang/Frontend/FrontendActions.h"
//#include "clang/Index/USRGeneration.h"
//#include "clang/Lex/PPCallbacks.h"
//#include "clang/Lex/Preprocessor.h"
//#include "clang/Rewrite/Frontend/FixItRewriter.h"
//#include "clang/Tooling/AllTUsExecution.h"
//#include "clang/Tooling/CommonOptionsParser.h"
//#include "clang/Tooling/Execution.h"
//#include "clang/Tooling/ReplacementsYaml.h"
//#include "clang/Tooling/Tooling.h"
//#include "llvm/ADT/StringMap.h"
//#include "llvm/ADT/StringSet.h"
//#include "llvm/Support/CommandLine.h"
//#include "llvm/Support/Signals.h"
//
//#include <functional>
//#include <map>
//#include <memory>
//#include <mutex>
//#include <tuple>
//
//using namespace clang::tooling;
//using namespace clang::ast_matchers;
//using namespace clang;
//using namespace llvm;
//
//std::mutex MU;
//std::vector<tooling::TranslationUnitReplacements> TURs;
//
//using ReplacementsMap = std::map<std::string, tooling::Replacements>;
//
//// std::string_view FunctionDefCommentHeader =
////     "\n//"
////     "-----------------------------------------------------"
////     "-------------------------\n/**\n  \n*/\n//---\n";
//
//int GetRangeSize(const SourceManager &Sources, const CharSourceRange &Range,
//                 const LangOptions &LangOpts) {
//  SourceLocation SpellingBegin = Sources.getSpellingLoc(Range.getBegin());
//  SourceLocation SpellingEnd = Sources.getSpellingLoc(Range.getEnd());
//  std::pair<FileID, unsigned> Start = Sources.getDecomposedLoc(SpellingBegin);
//  std::pair<FileID, unsigned> End = Sources.getDecomposedLoc(SpellingEnd);
//  if (Start.first != End.first)
//    return -1;
//  if (Range.isTokenRange())
//    End.second += Lexer::MeasureTokenLength(SpellingEnd, Sources, LangOpts);
//  return End.second - Start.second;
//}
//
//std::string NormalizeFilePath(const std::string &path) {
//  llvm::SmallString<128> normalized(path);
//  llvm::sys::path::remove_dots(normalized, /*remove_dot_dot=*/true);
//
//  // Canonicalize directory separators (forward slashes considered canonical.)
//  std::replace(normalized.begin(), normalized.end(), '\\', '/');
//
//  return normalized.str().str();
//}
//
//// bool IsAssociatedInclude(std::string_view Src, std::string_view Include) {
////   return llvm::sys::fs::equivalent(Src, Include);
//// }
//
//tooling::Replacement
//CreateReplacementFromSourceLocation(const SourceManager &Sources,
//                                    SourceLocation Start, unsigned Length,
//                                    StringRef ReplacementText) {
//  const auto &&[FileID, Offset] = Sources.getDecomposedLoc(Start);
//  Replacement result;
//
//  if (Sources.getFileEntryRefForID(FileID)) {
//    const FileEntryRef Entry = *Sources.getFileEntryRefForID(FileID);
//    auto FilePath =
//        std::string(Entry ? NormalizeFilePath(Entry.getName().str()) : "");
//
//    // const auto FilePath = Sources.getFilename(Sources.getFileLoc(Start));
//    // const auto Offset = Sources.getFileOffset(Start);
//
//    result = Replacement{FilePath, Offset, Length, ReplacementText};
//  }
//  return result;
//}
//
//tooling::TranslationUnitReplacements MergeReplacements(
//    const std::vector<tooling::TranslationUnitReplacements> &TURs) {
//  tooling::TranslationUnitReplacements Result;
//
//  for (auto &TUR : TURs) {
//
//    Result.Replacements.insert(std::end(Result.Replacements),
//                               std::begin(TUR.Replacements),
//                               std::end(TUR.Replacements));
//  }
//
//  std::sort(std::begin(Result.Replacements), std::end(Result.Replacements),
//            [](const Replacement &lhs, const Replacement &rhs) {
//              return std::tuple{lhs.getFilePath(), lhs.getOffset(),
//                                lhs.getLength()} <=
//                     std::tuple{rhs.getFilePath(), rhs.getOffset(),
//                                rhs.getLength()};
//            });
//
//  Result.Replacements.erase(
//      std::unique(std::begin(Result.Replacements),
//                  std::end(Result.Replacements),
//                  [](const Replacement &lhs, const Replacement &rhs) {
//                    return std::tuple{lhs.getFilePath(), lhs.getOffset(),
//                                      lhs.getLength()} ==
//                           std::tuple{rhs.getFilePath(), rhs.getOffset(),
//                                      rhs.getLength()};
//                  }),
//      std::end(Result.Replacements));
//
//  return Result;
//}
//
//class FunctionDeclMatchHandler : public MatchFinder::MatchCallback {
//public:
//  FunctionDeclMatchHandler(ReplacementsMap &Replacements, CompilerInstance &CI)
//      : Replacements{Replacements}, CI{CI} {}
//
//  void run(const MatchFinder::MatchResult &Result) override {
//    // auto memberExpr =
//    // Result.Nodes.getNodeAs<clang::MemberExpr>("memberExpr"); auto
//    // implicitCastExpr =
//    //     Result.Nodes.getNodeAs<clang::ImplicitCastExpr>("implicitCastExpr");
//
//    // auto varDecl = Result.Nodes.getNodeAs<clang::VarDecl>("varDecl");
//    // auto callExpr = Result.Nodes.getNodeAs<clang::CallExpr>("callExpr");
//    // auto declRefExpr =
//    //    Result.Nodes.getNodeAs<clang::DeclRefExpr>("declRefExpr"
//    //
//
//    auto FieldDecl = Result.Nodes.getNodeAs<clang::FieldDecl>("fieldDecl");
//    auto MemberExpr = Result.Nodes.getNodeAs<clang::MemberExpr>("memberExpr");
//    auto CXXConstructorDecl =
//        Result.Nodes.getNodeAs<clang::CXXConstructorDecl>("cxxConstructorDecl");
//
//    // auto ifStmt = Result.Nodes.getNodeAs<clang::IfStmt>("ifStmt");
//
//    // auto declRefExpr1 =
//    //     Result.Nodes.getNodeAs<clang::DeclRefExpr>("declRefExpr1");
//    // auto declRefExpr2 =
//    //     Result.Nodes.getNodeAs<clang::DeclRefExpr>("declRefExpr2");
//    // auto binaryOperator =
//    //     Result.Nodes.getNodeAs<clang::BinaryOperator>("binaryOperator");
//    // auto rootBinaryOperator =
//    //     Result.Nodes.getNodeAs<clang::BinaryOperator>("RootBinaryOperator");
//
//    auto Loc = CXXConstructorDecl ? CXXConstructorDecl->getBeginLoc()
//                                  : FieldDecl->getBeginLoc();
//
//    auto &&SM = *Result.SourceManager;
//
//    const auto SrcFileNameOrigin =
//        // SM.getFilename(declRefExpr->getBeginLoc())
//        // SM.getFilename(SM.getFileLoc(ifStmt->getBeginLoc()));
//        // SM.getFilename(SM.getFileLoc(FieldDecl->getBeginLoc()));
//        SM.getFilename(Loc);
//
//    // const auto SrcFileNameOrigin =
//    //     SM.getFilename(callExpr->getSourceRange().getBegin());
//
//    const auto SrcFileName = !SrcFileNameOrigin.empty()
//                                 ? NormalizeFilePath(SrcFileNameOrigin.data())
//                                 : "";
//
//    // llvm::errs() << callExpr->du << "\n\n";
//
//    // llvm::errs() <<
//    // llvm::StringRef{SM.getCharacterData(declRefExpr->getBeginLoc())} <<
//    // "\n\n";
//
//    // ifStmt->dump();
//    // ifStmt->getLParenLoc().dump(SM);
//
//    if (SrcFileName.find("/Source/") != std::string::npos
//        //&& callExp->getAsString().find("c3d::") == std::string::npos
//        //&& !llvm::StringRef{SM.getCharacterData(callExpr->getBeginLoc())}
//        //&&
//        //! llvm::StringRef{SM.getCharacterData(declRefExpr->getBeginLoc())}.starts_with("c3d::")
//    )
//
//    {
//      std::vector<clang::SourceRange> Ranges;
//
//      if (CXXConstructorDecl) {
//        for (auto *I : CXXConstructorDecl->inits()) {
//
//          if (auto *Decl = I->getMember(); !CXXConstructorDecl->decls_empty() &&
//                                           Decl &&
//                                           !I->isInClassMemberInitializer()) {
//
//            if (Decl->getAccess() == clang::AccessSpecifier::AS_private ||
//                Decl->getAccess() == clang::AccessSpecifier::AS_private)
//
//              if (!Decl->getName().starts_with("m_")) {
//                Ranges.emplace_back(I->getSourceRange());
//
//                Ranges.emplace_back(
//                    clang::SourceRange(Decl->getLocation(), Decl->getEndLoc()));
//              }
//          }
//        }
//      } else {
//        // fields.emplace_back(FieldDecl);
//      }
//
//      {
//        {
//          auto &&DE = Result.SourceManager->getDiagnostics();
//
//          auto ID =
//              DE.getCustomDiagID(clang::DiagnosticsEngine::Warning, "add m_");
//
//          if (FieldDecl)
//            if (auto *Decl = FieldDecl->getCanonicalDecl())
//              if (Decl->getAccess() == clang::AccessSpecifier::AS_private ||
//                  Decl->getAccess() == clang::AccessSpecifier::AS_private)
//                if (Decl && !Decl->getName().starts_with("m_")) {
//                  if (Decl)
//                    Ranges.emplace_back(clang::SourceRange(Decl->getLocation(),
//                                                           Decl->getEndLoc()));
//                  if (MemberExpr)
//                    Ranges.emplace_back(MemberExpr->getSourceRange());
//                }
//
//          for (auto SourceRange : Ranges) {
//
//            SmallString<32> Code{"m_"};
//            Code += SM.getCharacterData(SourceRange.getBegin())[0];
//
//            DE.Report(SourceRange.getBegin(), ID)
//                << FixItHint::CreateInsertion(SourceRange.getBegin(), Code);
//
//            auto Range = CharSourceRange::getTokenRange(SourceRange);
//            if (!std::empty(SrcFileName)) {
//
//              (void)Replacements[SrcFileName.c_str()].add(
//                  CreateReplacementFromSourceLocation(
//                      SM, SourceRange.getBegin(),
//                      1 /*GetRangeSize(SM, Range, CI.getLangOpts())*/, Code));
//            }
//          }
//        }
//      }
//
//      //  auto ID =
//      //      DE.getCustomDiagID(clang::DiagnosticsEngine::Warning,
//      //                               "useless comparation");
//
//      // if (declRefExpr1->getDecl() == declRefExpr2->getDecl()) {
//      //   ifStmt->getLParenLoc().dump(SM);
//
//      //  auto SourceRange = clang::SourceRange(
//      //      binaryOperator->getSourceRange().getBegin().getLocWithOffset(-3),
//      //      binaryOperator->getSourceRange().getEnd());
//
//      //  if (rootBinaryOperator->getLHS() == binaryOperator) {
//      //    SourceRange = clang::SourceRange(
//      //        binaryOperator->getSourceRange().getBegin(),
//      //        binaryOperator->getSourceRange().getEnd().getLocWithOffset(3));
//      //  }
//
//      //  DE.Report(SourceRange.getBegin(), ID)
//      //      << FixItHint::CreateRemoval(SourceRange);
//
//      //  auto Range = CharSourceRange::getTokenRange(SourceRange);
//
//      //  if (!std::empty(SrcFileName)) {
//      //    (void)Replacements[SrcFileName.c_str()].add(
//      //        CreateReplacementFromSourceLocation(
//      //            SM, SourceRange.getBegin(),
//      //            GetRangeSize(SM, Range, CI.getLangOpts()), ""));
//      //  }
//      //}
//
//      // varDecl->dump();
//      // varDecl->getLocation().dump(SM);
//
//      // auto &&DE = Result.SourceManager->getDiagnostics();
//      // auto ID =
//      //     DE.getCustomDiagID(clang::DiagnosticsEngine::Warning, "use
//      //     c3d::");
//
//      // auto InstertBegin = declRefExpr->getBeginLoc();
//      //  auto InstertBegin = declRefExpr->getBeginLoc();
//      //   auto InstertBegin = callExpr->getBeginLoc();
//      // auto Code = "c3d::";
//
//      // DE.Report(InstertBegin, ID)
//      //     << FixItHint::CreateInsertion(InstertBegin, Code);
//
//      if (
//          // NormalizeFilePath(SM.getFilename(callExpr->getBeginLoc()).str());
//          !std::empty(SrcFileName)) {
//
//        //(void)Replacements[SrcFileName.c_str()].add(
//        //    CreateReplacementFromSourceLocation(SM, InstertBegin, 0,
//        //    Code));
//        //(void)Replacements[SrcFileName.c_str()].add(
//        //    CreateReplacementFromSourceLocation(SM, InstertBegin, 0,
//        //    Code));
//
//        // const auto &&[FileID, Offset] =
//        // SM.getDecomposedLoc(InstertBegin); const FileEntryRef Entry =
//        // *SM.getFileEntryRefForID(FileID);
//        //  auto FilePath =
//        //  std::string(Entry ? NormalizeFilePath(Entry.getName().str()) :
//        //  "");
//
//        // llvm::errs() << Offset << "\n\n";
//        //  FileID->
//
//        //(void)Replacements[SrcFileName.c_str()].add({});
//
//        // ifStmt->dump();
//      }
//    }
//
//    // if (SrcFileName.find("/Source/") != std::string::npos &&
//    //     varDecl->getType().getAsString().find("c3d::") == std::string::npos)
//    //     {
//    //   // varDecl->dump();
//    //   // varDecl->getLocation().dump(SM);
//
//    //  auto &&DE = Result.SourceManager->getDiagnostics();
//    //  auto ID =
//    //      DE.getCustomDiagID(clang::DiagnosticsEngine::Warning, "use c3d::");
//
//    //  auto InstertBegin = varDecl->getTypeSpecStartLoc().getLocWithOffset(-1);
//    //  auto Code = " c3d::";
//
//    //  DE.Report(InstertBegin, ID)
//    //      << FixItHint::CreateInsertion(InstertBegin, Code);
//
//    //  if (std::string FileName =
//    //          NormalizeFilePath(SM.getFilename(varDecl->getBeginLoc()).str());
//    //      !std::empty(FileName)) {
//
//    //    (void)Replacements[FileName.c_str()].add(
//    //        CreateReplacementFromSourceLocation(SM, InstertBegin, 1, Code));
//    //  }
//    //}
//
//    //    //if (SrcFileName.contains("Source\\") && memberExpr->getQualifier()
//    //    && memberExpr->getQualifier()->getKind() ==
//    //        clang::NestedNameSpecifier::Super) {
//
//    //  const CXXBaseSpecifier *Base = *implicitCastExpr->path().begin();
//    //  const auto *RD =
//    //      cast<CXXRecordDecl>(Base->getType()->castAs<RecordType>()->getDecl());
//
//    //  auto &&DE = Result.SourceManager->getDiagnostics();
//    //  const unsigned ID =
//    //  DE.getCustomDiagID(clang::DiagnosticsEngine::Warning,
//    //                                         "__super is ms-extension");
//
//    //  const auto &&DC = implicitCastExpr->getBestDynamicClassType();
//
//    //  const std::string BaseClass = "BaseClass";
//    //  // const std::string BaseClass = DC->getNameAsString() + "Base";
//    //  const std::string Code =
//    //      std::string{DC->isStruct() ? "\nprivate:\n" : ""} + "\nusing " +
//    //      BaseClass + " = " +
//    //      // Base->getType()->getCanonicalTypeInternal().getAsString() + ";";
//    //      RD->getName().str() + ";\n" +
//    //      std::string{DC->isStruct() ? "\npublic:\n" : ""};
//
//    //  auto InstertBegin = DC->getBraceRange().getBegin().getLocWithOffset(1);
//
//    //  // auto M = cxxRecordDecl(anyOf(
//    //  //     has(typeAliasDecl(hasName("bazBase")).bind("xxx")), anything()));
//
//    //  DE.Report(InstertBegin, ID)
//    //      << FixItHint::CreateInsertion(InstertBegin, Code);
//
//    //  // if (std::string FileName = NormalizeFilePath(
//    //  //         SM.getFilename(memberExpr->getBeginLoc()).str());
//    //  //     !std::empty(FileName)) {
//
//    //  //  auto err = Replacements[FileName.c_str()].add(
//    //  //      CreateReplacementFromSourceLocation(SM, InstertBegin, 1, Code));
//    //  //}
//
//    //  auto Range = CharSourceRange::getTokenRange(clang::SourceRange(
//    //      memberExpr->getQualifierLoc().getBeginLoc(),
//    //      memberExpr->getQualifierLoc().getEndLoc().getLocWithOffset(-2)));
//
//    //  std::string Replacement = "/* __super */ ";
//    //  // Replacement +=
//    //  //     implicitCastExpr->getType().getBaseTypeIdentifier()->getName();
//    //  // const CXXBaseSpecifier *Base = *implicitCastExpr->path().begin();
//    //  // const auto *RD =
//    //  //
//    //  cast<CXXRecordDecl>(Base->getType()->castAs<RecordType>()->getDecl());
//    //  // Replacement += RD->getName();
//    //  Replacement += BaseClass;
//
//    //  DE.Report(memberExpr->getQualifierLoc().getBeginLoc(), ID)
//    //      << FixItHint::CreateReplacement(Range, Replacement);
//
//    //  if (std::string FileName = NormalizeFilePath(
//    //          SM.getFilename(memberExpr->getBeginLoc()).str());
//    //      !std::empty(FileName)) {
//
//    //    (void)Replacements[FileName.c_str()].add(
//    //        CreateReplacementFromSourceLocation(
//    //            SM, Range.getBegin(), GetRangeSize(SM, Range,
//    //            CI.getLangOpts()), Replacement));
//    //  }
//
//    //  if (std::string FileName =
//    //          NormalizeFilePath(SM.getFilename(DC->getBeginLoc()).str());
//    //      !std::empty(FileName) && DC->getBraceRange().isValid()) {
//
//    //    (void)Replacements[FileName.c_str()].add(
//    //        CreateReplacementFromSourceLocation(SM, InstertBegin, 1, Code));
//    //  }
//    //}
//
//    // llvm::errs() << SM << "\n\n";
//
//    // if (std::string FileName =
//    //         NormalizeFilePath(SM->getFilename(Decl->getBeginLoc()).str());
//    //     !std::empty(FileName)) {
//
//    //  auto Expansion =
//    //      SM->getSLocEntry(SM->getFileID(Decl->getSourceRange().getBegin()))
//    //          .getExpansion();
//
//    //  if (Decl->getParent() && !Decl->getParent()->isLambda() &&
//    //      Decl->getCanonicalDecl() != Decl &&
//    //      !Expansion.isMacroArgExpansion()) {
//
//    //    if (!Result.Context->getRawCommentForDeclNoCache(Decl)) {
//    //      auto &&DE = Result.SourceManager->getDiagnostics();
//    //      const unsigned ID = DE.getCustomDiagID(
//    //          clang::DiagnosticsEngine::Warning, "No Comment!");
//
//    //      DE.Report(Decl->getBeginLoc(), ID)
//    //          << "Add " << FunctionDefCommentHeader
//    //          << FixItHint::CreateInsertion(Decl->getBeginLoc(),
//    //                                        FunctionDefCommentHeader);
//
//    //      auto err = Replacements[FileName.c_str()].add(
//    //          CreateReplacementFromSourceLocation(*SM, Decl->getBeginLoc(), 0,
//    //                                              FunctionDefCommentHeader));
//
//    //      llvm::errs() << Decl->getNameAsString() << "\n";
//    //    } else {
//    //      auto RawComment = Result.Context->getRawCommentForDeclNoCache(Decl);
//    //      auto Comment = RawComment->getRawText(*SM);
//
//    //      if (!Comment.contains("/**")) {
//    //        llvm::errs() << Comment << "\n"
//    //                     << Decl->getNameAsString() << "\n\n";
//
//    //        auto Range =
//    //            CharSourceRange::getTokenRange(RawComment->getSourceRange());
//
//    //        Replacements[FileName.c_str()].add(
//    //            CreateReplacementFromSourceLocation(
//    //                *SM, RawComment->getBeginLoc(),
//    //                GetRangeSize(*SM, Range, CI.getLangOpts()), ""));
//    //      }
//    //    }
//    //  }
//    //}
//  }
//
//private:
//  ReplacementsMap &Replacements;
//  CompilerInstance &CI;
//};
//
//class XASTConsumer : public ASTConsumer {
//public:
//  XASTConsumer(ReplacementsMap &Replacements, CompilerInstance &CI)
//      : Replacements{Replacements}, Handler{Replacements, CI} {
//
//    // Matcher.addMatcher(cxxMethodDecl(isDefinition(), unless(isImplicit()),
//    //                                  unless(isExpansionInSystemHeader()))
//    //                        .bind("fnDecl"),
//    //                    &Handler);
//
//    // Matcher.addMatcher(cxxMemberCallExpr(hasDescendant(
//    //                        memberExpr(hasDescendant(implicitCastExpr().bind(
//    //                                       "implicitCastExpr")))
//    //                            .bind("memberExpr"))),
//    //                    &Handler);
//
//    // Matcher.addMatcher(
//    //     varDecl(hasType(namedDecl(hasParent(namespaceDecl(hasName("c3d"))))))
//    //         .bind("varDecl"),
//    //     &Handler);
//
//    // Matcher.addMatcher(callExpr(hasDeclaration(functionDecl(
//    //                                 hasParent(namespaceDecl(hasName("c3d"))))))
//    //                        .bind("callExpr"),
//    //                    &Handler);
//
//    // Matcher.addMatcher(declRefExpr(hasDeclaration(varDecl(hasParent(
//    //                                    namespaceDecl(hasName("c3d"))))))
//    //                        .bind("declRefExpr"),
//    //                    &Handler);
//
//    // Matcher.addMatcher(
//    //     traverse(
//    //         TK_IgnoreUnlessSpelledInSource,
//    //         ifStmt(hasCondition(
//    //                    binaryOperator(
//    //                        hasOperatorName("&&"),
//    //                        has(binaryOperator(
//    //                                hasOperatorName(">="),
//    //                                has(declRefExpr(hasType(isUnsignedInteger()))
//    //                                        .bind("declRefExpr1")),
//    //                                has(integerLiteral(equals(0))))
//    //                                .bind("binaryOperator")),
//    //                        has(binaryOperator(
//    //                            hasAnyOperatorName("<"),
//    //                            has(declRefExpr(hasType(isUnsignedInteger()))
//    //                                    .bind("declRefExpr2")))))
//    //                        .bind("RootBinaryOperator")))
//    //             .bind("ifStmt")),
//    //     &Handler);
//
//    Matcher.addMatcher(
//        traverse(TK_IgnoreUnlessSpelledInSource,
//                 memberExpr(hasDeclaration(
//                                fieldDecl(/*anyOf(isPrivate(), isProtected())*/)
//                                    .bind("fieldDecl")))
//                     .bind("memberExpr")),
//        &Handler);
//
//    Matcher.addMatcher(
//        traverse(TK_IgnoreUnlessSpelledInSource,
//                 cxxConstructorDecl().bind("cxxConstructorDecl")),
//        &Handler);
//  }
//
//  void HandleTranslationUnit(ASTContext &Context) override {
//    Matcher.matchAST(Context);
//  }
//
//private:
//  FunctionDeclMatchHandler Handler;
//  MatchFinder Matcher;
//  ReplacementsMap &Replacements;
//};
//
//class Find_Includes : public PPCallbacks {
//public:
//  Find_Includes(CompilerInstance &CI, ReplacementsMap &Replacements)
//      : CI{CI}, Replacements{Replacements} {}
//
//  void InclusionDirective(SourceLocation HashLoc, const Token &IncludeTok,
//                          StringRef FileName, bool IsAngled,
//                          CharSourceRange FilenameRange,
//                          OptionalFileEntryRef File, StringRef SearchPath,
//                          StringRef RelativePath, const clang::Module *Imported,
//                          SrcMgr::CharacteristicKind FileType) override {
//
//    auto &DE = CI.getDiagnostics();
//    auto &SM = CI.getSourceManager();
//
//    const unsigned ID = DE.getCustomDiagID(clang::DiagnosticsEngine::Warning,
//                                           "Incorrect Include Style");
//
//    auto SrcFileName = SM.getFilename(FilenameRange.getBegin());
//
//    SmallString<512> Src = SrcFileName;
//    llvm::sys::path::replace_extension(Src, ".h");
//
//    SrcFileName = NormalizeFilePath(SrcFileName.str()).c_str();
//
//    if (llvm::sys::fs::equivalent(Src, SearchPath + "/" + FileName)) {
//      if (SM.isInMainFile(HashLoc)) {
//        ++I;
//
//        std::string Replacement =
//            (llvm::Twine("\"") + llvm::sys::path::filename(FileName) + "\"")
//                .str();
//
//        if (I > 1) {
//          DE.Report(FilenameRange.getBegin(), ID) << FixItHint::CreateRemoval(
//              clang::SourceRange(HashLoc, FilenameRange.getEnd()));
//
//          auto Range = CharSourceRange::getTokenRange(
//              clang::SourceRange(HashLoc, FilenameRange.getEnd()));
//
//          auto err = Replacements[SrcFileName.str()].add(
//              CreateReplacementFromSourceLocation(
//                  SM, HashLoc, GetRangeSize(SM, Range, CI.getLangOpts()), ""));
//
//        } else if (llvm::sys::path::filename(FileName) != FileName) {
//          {
//            DE.Report(FilenameRange.getBegin(), ID)
//                << FixItHint::CreateReplacement(FilenameRange.getAsRange(),
//                                                Replacement);
//
//            auto err = Replacements[SrcFileName.str()].add(
//                CreateReplacementFromSourceLocation(
//                    SM, FilenameRange.getBegin(),
//                    GetRangeSize(SM, FilenameRange, CI.getLangOpts()),
//                    Replacement));
//          }
//        }
//
//      } else {
//        if (FileType == SrcMgr::CharacteristicKind::C_User &&
//            !SrcFileName.ends_with(".cpp")) {
//          DE.Report(FilenameRange.getBegin(), ID) << FixItHint::CreateRemoval(
//              clang::SourceRange(HashLoc, FilenameRange.getEnd()));
//
//          auto Range = CharSourceRange::getTokenRange(
//              clang::SourceRange(HashLoc, FilenameRange.getEnd()));
//
//          auto err = Replacements[SrcFileName.str()].add(
//              CreateReplacementFromSourceLocation(
//                  SM, HashLoc, GetRangeSize(SM, Range, CI.getLangOpts()), ""));
//        }
//      }
//    } else if (FileType == SrcMgr::CharacteristicKind::C_User) {
//
//      if (!llvm::sys::fs::equivalent(Src, SearchPath + "/" + FileName)) {
//        // if (SM.isInMainFile(HashLoc)) {
//
//        SmallString<512> path = SearchPath;
//        path += "/";
//        path += FileName;
//        llvm::sys::fs::make_absolute(path);
//        llvm::sys::path::remove_dots(path, true);
//
//        path = llvm::sys::path::convert_to_slash(
//            path, llvm::sys::path::Style::windows_backslash);
//
//        if (auto pos = path.find(".conan"); pos == -1)
//          if (auto pos = path.find("AfxCompat.h"); pos == -1)
//            if (auto pos = path.find("AfxExCompat.h"); pos == -1)
//              if (auto pos = path.find("BCGIncludeFull.h"); pos == -1)
//
//                if (auto pos = path.find("/Source/"); pos != -1) {
//                  path = path.substr(pos + 8);
//
//                  size_t offset =
//                      (path.starts_with("2D/") || path.starts_with("3D/") ||
//                       path.starts_with("UI/"))
//                          ? 3
//                      : path.starts_with("API/")    ? 4
//                      : path.starts_with("Text/")   ? 5
//                      : path.starts_with("Tests/")  ? 6
//                      : path.starts_with("Render/") ? 7
//                                                    : 0;
//                  path = path.substr(offset);
//
//                  if (auto pos = path.find("/"); pos != -1) {
//
//                    offset = (path.substr(0, pos + 1) ==
//                                      path.substr(pos + 1, pos + 1)
//                                  ? pos + 1
//                                  : 0);
//
//                    path = path.substr(offset);
//
//                    std::string Replacement =
//                        (llvm::Twine("<") + path + ">").str();
//
//                    if (FileName != path || !IsAngled) {
//                      DE.Report(FilenameRange.getBegin(), ID)
//                          << FixItHint::CreateReplacement(
//                                 FilenameRange.getAsRange(), Replacement);
//
//                      auto err = Replacements[SrcFileName.str()].add(
//                          CreateReplacementFromSourceLocation(
//                              SM, FilenameRange.getBegin(),
//                              GetRangeSize(SM, FilenameRange, CI.getLangOpts()),
//                              Replacement));
//                    }
//                  }
//                }
//        //}
//      }
//    }
//  }
//
//private:
//  CompilerInstance &CI;
//  ReplacementsMap &Replacements;
//  size_t I = 0;
//};
//
//// For each source file provided to the tool, a new FrontendAction is created.
//class XFrontendAction : public ASTFrontendAction {
//  // class XFrontendAction : public PreprocessOnlyAction {
//
//public:
//  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
//                                                 StringRef File) override {
//    return std::make_unique<XASTConsumer>(Replacements, CI);
//  }
//
//  bool BeginSourceFileAction(CompilerInstance &CI) override {
//
//    // CI.getLangOpts().CommentOpts.ParseAllComments = true;
//
//    // Preprocessor &PP = CI.getPreprocessor();
//    // PP.addPPCallbacks(std::make_unique<Find_Includes>(CI, Replacements));
//
//    return true;
//  }
//
//  void EndSourceFileAction() override {
//    tooling::TranslationUnitReplacements TUR;
//
//    for (const auto &[Key, Value] : Replacements)
//      for (const auto &Entry : Value) {
//        TUR.Replacements.push_back(Entry);
//      }
//
//    std::scoped_lock Lock{MU};
//    TURs.push_back(TUR);
//  }
//
//private:
//  ReplacementsMap Replacements;
//};
//
//class XFrontendActionFactory : public tooling::FrontendActionFactory {
//public:
//  std::unique_ptr<FrontendAction> create() override {
//    return std::make_unique<XFrontendAction>();
//  }
//};
//
//int main(int argc, const char **argv) {
//  llvm::sys::PrintStackTraceOnErrorSignal(argv[0]);
//
//  const char *Overview = R"(
//  check includes across a whole C/C++ project
//  include-fixer --filter=CompressPathTest.cpp
//  )";
//
//  tooling::ExecutorName.setInitialValue("all-TUs");
//
//  auto Executor = clang::tooling::createExecutorFromCommandLineArgs(
//      argc, argv, llvm::cl::getGeneralCategory(), Overview);
//
//  if (!Executor) {
//    llvm::errs() << llvm::toString(Executor.takeError()) << "\n";
//    return 1;
//  }
//
//  auto Err =
//      Executor->get()->execute(std::make_unique<XFrontendActionFactory>());
//
//  tooling::TranslationUnitReplacements TUR = MergeReplacements(TURs);
//  yaml::Output YAML(llvm::outs());
//  YAML << TUR;
//
//  if (Err) {
//    llvm::errs() << llvm::toString(std::move(Err)) << "\n";
//  }
//}