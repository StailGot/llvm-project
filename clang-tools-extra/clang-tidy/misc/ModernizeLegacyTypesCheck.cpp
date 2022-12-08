//===--- ModernizeLegacyTypesCheck.cpp - clang-tidy -----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "ModernizeLegacyTypesCheck.h"
#include "clang/AST/ASTContext.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Lex/Lexer.h"

using namespace clang::ast_matchers;

namespace clang {
namespace tidy {
namespace misc {

//static const std::string Type = "CRect";
//static const std::string LType = "LRect";

//static const std::string Type = "CPoint";
//static const std::string LType = "LPoint";

static const std::string Type = "CSize";
static const std::string LType = "LSize";

ModernizeLegacyTypesCheck::ModernizeLegacyTypesCheck(StringRef Name,
                                                     ClangTidyContext *Context)
    : ClangTidyCheck(Name, Context),
      Inserter(Options.getLocalOrGlobal("IncludeStyle",
                                        utils::IncludeSorter::IS_LLVM),
               areDiagsSelfContained()) {}

void ModernizeLegacyTypesCheck::storeOptions(
    ClangTidyOptions::OptionMap &Opts) {
  Options.store(Opts, "IncludeStyle", Inserter.getStyle());
}

void ModernizeLegacyTypesCheck::registerMatchers(MatchFinder *Finder) {

 const auto AllowedMethod = 
    //hasAncestor(cxxMethodDecl(unless(isVirtual())))
    hasAncestor(cxxMethodDecl())
   ;

  const auto CRectParam =
    [](auto bind, auto && ... Constraints)
  {
    return parmVarDecl(hasDescendant(typeLoc(loc(qualType(hasDeclaration(recordDecl(hasName(Type)))))).bind(bind)), Constraints...);
  };
  
  const auto CRectVar =
    [](auto bind, auto && ... Constraints)
  {
    return varDecl(unless(parmVarDecl()), hasDescendant(typeLoc(loc(qualType(hasDeclaration(recordDecl(hasName(Type)))))).bind(bind)), Constraints...);
  };

  // void Foo( CRect & r)
  //           ^~~~~
  Finder->addMatcher(CRectParam(Type, AllowedMethod), this);
  Finder->addMatcher(CRectVar(Type, AllowedMethod), this);

  //const auto CopyAssignCall = 
  //  cxxOperatorCallExpr( hasOverloadedOperatorName("="), hasArgument(1, declRefExpr(to(CRectParam(Type)))) );

  // CRect r1 = r;
  //           ^~~
  // CRect r2(r);
  //         ^~~
  // r2 = r;
  //    ^~~
  //const auto UseOfCRectParam = 
  //    declRefExpr( to(CRectParam(Type, AllowedMethod)),
  //      anyOf(
  //        hasParent(cxxOperatorCallExpr( unless(hasOverloadedOperatorName("=")) ))
  //          , hasParent(varDecl())
  //          , hasParent(cxxConstructExpr())
  //          , hasParent(CopyAssignCall)
  //      )
  //  ).bind("UseOfCRectParam");
  
  //Finder->addMatcher(UseOfCRectParam, this);

  const auto ExternMemberCall =
    cxxMemberCallExpr(
            isExpansionInMainFile(), hasDeclaration(cxxMethodDecl(ofClass(isExpansionInSystemHeader()))
        )
    );

  const auto ThisMemberCall =
    cxxMemberCallExpr(
            hasDescendant(memberExpr(hasDescendant(cxxThisExpr()))),
            //hasDeclaration(cxxMethodDecl(isVirtual())))
            hasDeclaration(cxxMethodDecl()))
    ;

  const auto MemberCall =
      declRefExpr( anyOf(to(CRectParam("", AllowedMethod)), to(CRectVar("", AllowedMethod)) )
        //, hasParent(ThisMemberCall)
        , hasParent(ExternMemberCall)
    ).bind("UseOfParam");

  Finder->addMatcher(MemberCall, this);

  const auto MemberCallRef =
      declRefExpr( anyOf(to(CRectParam("", AllowedMethod)), to(CRectVar("", AllowedMethod)) )
        //, hasParent( unaryOperator(anyOf(hasParent(ThisMemberCall), hasParent(ExternMemberCall))) )
        , hasParent( unaryOperator(hasParent(ExternMemberCall)) )
    ).bind("UseOfParamRef");

  Finder->addMatcher(MemberCallRef, this);

  const auto ThisMemberCallNonCast =
    cxxMemberCallExpr(
            hasDescendant(memberExpr(hasDescendant(cxxThisExpr())))
            //, hasDeclaration(cxxMethodDecl( unless(isVirtual()) ))
            , hasDeclaration(cxxMethodDecl())
    );

  const auto InAsIs =
    hasParent(ThisMemberCallNonCast);

  const auto InImplicitCastExpr =
    hasParent(implicitCastExpr( hasImplicitDestinationType(isConstQualified()), hasParent(ThisMemberCallNonCast)));

  //const auto InConstructExpr =
  //  hasParent(implicitCastExpr(hasImplicitDestinationType(isConstQualified()), hasParent(cxxConstructExpr(hasParent(ThisMemberCallNonCast)))));

  //const auto ConstUseOfLocalCRectParamRef =
  //  traverse( TK_AsIs,
  //    declRefExpr(to(CRectVar(Type)),
  //      anyOf(InConstructExpr, InImplicitCastExpr) ).bind("ConstUseOfLocalParamRef")
  //  );

  //Finder->addMatcher(ConstUseOfLocalCRectParamRef, this);

  //const auto UseOfLocalCRectParamRef =
  //  traverse( TK_AsIs,
  //    declRefExpr(to(CRectVar(Type)), InAsIs ).bind("UseOfLocalParamRef")
  //  );

  //Finder->addMatcher(UseOfLocalCRectParamRef, this);

  const auto UseOfLocalCRectParamRef =
    traverse( TK_AsIs,
      declRefExpr(to(CRectVar(Type)), hasAncestor(cxxMemberCallExpr(
        hasDescendant(memberExpr(hasDescendant(cxxThisExpr()))), hasDeclaration(cxxMethodDecl()))))
    );

  Finder->addMatcher(UseOfLocalCRectParamRef, this);
}

void ModernizeLegacyTypesCheck::registerPPCallbacks(
    const SourceManager &SM, Preprocessor *PP, Preprocessor *ModuleExpanderPP) {
  Inserter.registerPreprocessor(PP);
}

void ModernizeLegacyTypesCheck::check(const MatchFinder::MatchResult &Result) {
  SourceManager &SM = *Result.SourceManager;

  // update method decl & impl
  if (const auto *E = Result.Nodes.getNodeAs<TypeLoc>(Type)) {
    CharSourceRange Range = Lexer::makeFileCharRange(
        CharSourceRange::getTokenRange(E->getSourceRange()), SM, LangOptions());

    auto Diag = diag(Range.getBegin(), "replace '"+Type+"' with '"+LType+"'")
                << FixItHint::CreateReplacement(Range, LType)
                << Inserter.createMainFileIncludeInsertion("<Sys/Geometry.h>");
  }

  if (const auto *E = Result.Nodes.getNodeAs<DeclRefExpr>("UseOfParam")) {
    CharSourceRange Range = Lexer::makeFileCharRange(
        CharSourceRange::getTokenRange(E->getSourceRange()), SM, LangOptions());

    auto Diag =
        diag(Range.getBegin(), "use kompas::type_cast'")
        << FixItHint::CreateInsertion(Range.getBegin(),
                                      "kompas::type_cast<"+Type+">(")
        << FixItHint::CreateInsertion(Range.getEnd(), ")")
        << Inserter.createMainFileIncludeInsertion("<Sys/TypeCast.hpp>");
  }

  if (const auto *E =
          Result.Nodes.getNodeAs<DeclRefExpr>("UseOfLocalParamRef")) {
    CharSourceRange Range = Lexer::makeFileCharRange(
        CharSourceRange::getTokenRange(E->getSourceRange()), SM, LangOptions());

    auto Diag = diag(Range.getBegin(), "use reinterpret_cast'")
                << FixItHint::CreateInsertion(
                       Range.getBegin(),
                       "/*TODO: unsafe*/ *reinterpret_cast<"+LType+"*>(&")
                << FixItHint::CreateInsertion(Range.getEnd(), ")");
  }

  if (const auto *E = Result.Nodes.getNodeAs<DeclRefExpr>("ConstUseOfLocalParamRef")) {
    CharSourceRange Range = Lexer::makeFileCharRange(
        CharSourceRange::getTokenRange(E->getSourceRange()), SM, LangOptions());

    auto Diag =
        diag(Range.getBegin(), "use kompas::type_cast'")
        << FixItHint::CreateInsertion(Range.getBegin(),
                                      "kompas::type_cast<"+LType+">(")
        << FixItHint::CreateInsertion(Range.getEnd(), ")")
        << Inserter.createMainFileIncludeInsertion("<Sys/TypeCast.hpp>");
  }

  if (const auto *E =
          Result.Nodes.getNodeAs<DeclRefExpr>("UseOfParamRef")) {
    CharSourceRange Range = Lexer::makeFileCharRange(
        CharSourceRange::getTokenRange(E->getSourceRange()), SM, LangOptions());

    auto Diag = diag(Range.getBegin(), "use reinterpret_cast'")
                << FixItHint::CreateInsertion(
                       Range.getBegin().getLocWithOffset(-1),
                       "/*TODO: unsafe*/ reinterpret_cast<"+Type+"*>(")
                << FixItHint::CreateInsertion(Range.getEnd(), ")");
  }
}

} // namespace misc
} // namespace tidy
} // namespace clang
