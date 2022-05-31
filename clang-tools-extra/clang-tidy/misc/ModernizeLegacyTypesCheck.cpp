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

using namespace clang::ast_matchers;

namespace clang {
namespace tidy {
namespace misc {

void ModernizeLegacyTypesCheck::registerMatchers(MatchFinder *Finder) {
  Finder->addMatcher(
      implicitCastExpr(
          unless(hasParent(explicitCastExpr())),
          has(unaryOperator(hasOperatorName("&"),
                            has(expr(hasType(namedDecl(hasName("TRect"))))))),
          hasImplicitDestinationType(
              pointsTo(namedDecl(anyOf(hasName("tagRECT"), hasName("RECT"))))))
          .bind("RECT"),
      this);

  Finder->addMatcher(
      implicitCastExpr(
          unless(hasParent(explicitCastExpr())),
          has(unaryOperator(hasOperatorName("&"),
                            has(expr(hasType(namedDecl(hasName("TSize"))))))),
          hasImplicitDestinationType(
              pointsTo(namedDecl(anyOf(hasName("tagSIZE"), hasName("SIZE"))))))
          .bind("SIZE"),
      this);

  Finder->addMatcher(
      implicitCastExpr(
          unless(hasParent(explicitCastExpr())),
          has(unaryOperator(hasOperatorName("&"),
                            has(expr(hasType(namedDecl(hasName("TPoint"))))))),
          hasImplicitDestinationType(pointsTo(
              namedDecl(anyOf(hasName("tagPOINT"), hasName("POINT"))))))
          .bind("POINT"),
      this);
}

void ModernizeLegacyTypesCheck::check(const MatchFinder::MatchResult &Result) {
  if (const auto *MatchedCastExpr =
          Result.Nodes.getNodeAs<ImplicitCastExpr>("RECT")) {
    diag(MatchedCastExpr->getBeginLoc(), "Use explicit cast to RECT")
        << MatchedCastExpr->getExprLoc();

    diag(MatchedCastExpr->getBeginLoc(), "insert '(RECT*)'",
         DiagnosticIDs::Note)
        << FixItHint::CreateInsertion(MatchedCastExpr->getBeginLoc(),
                                      "(RECT*)");
  }

  if (const auto *MatchedCastExpr =
          Result.Nodes.getNodeAs<ImplicitCastExpr>("SIZE")) {
    diag(MatchedCastExpr->getBeginLoc(), "Use explicit cast to SIZE")
        << MatchedCastExpr->getExprLoc();

    diag(MatchedCastExpr->getBeginLoc(), "insert '(SIZE*)'",
         DiagnosticIDs::Note)
        << FixItHint::CreateInsertion(MatchedCastExpr->getBeginLoc(),
                                      "(SIZE*)");
  }

  if (const auto *MatchedCastExpr =
          Result.Nodes.getNodeAs<ImplicitCastExpr>("POINT")) {
    diag(MatchedCastExpr->getBeginLoc(), "Use explicit cast to POINT")
        << MatchedCastExpr->getExprLoc();

    diag(MatchedCastExpr->getBeginLoc(), "insert '(POINT*)'",
         DiagnosticIDs::Note)
        << FixItHint::CreateInsertion(MatchedCastExpr->getBeginLoc(),
                                      "(POINT*)");
  }
}

} // namespace misc
} // namespace tidy
} // namespace clang
