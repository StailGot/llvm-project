//===--- IncludeNormalizeCheck.cpp - clang-tidy ---------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "IncludeNormalizeCheck.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Lex/PPCallbacks.h"
#include "clang/Lex/Preprocessor.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringSet.h"

using namespace clang::ast_matchers;

namespace clang {
namespace tidy {
namespace readability {

class IncludeNormalizePPCallbacks : public PPCallbacks {
public:
  explicit IncludeNormalizePPCallbacks(const SourceManager &SM,
                                       ClangTidyCheck &Check);

  void InclusionDirective(SourceLocation HashLoc, const Token &IncludeTok,
                          StringRef FileName, bool IsAngled,
                          CharSourceRange FilenameRange,
                          Optional<FileEntryRef> File, StringRef SearchPath,
                          StringRef RelativePath, const Module *Imported,
                          SrcMgr::CharacteristicKind FileType) override;

private:
  ClangTidyCheck &Check;
  const SourceManager &SM;
};

void IncludeNormalizeCheck::registerPPCallbacks(
    const SourceManager &SM, Preprocessor *PP, Preprocessor *ModuleExpanderPP) {
  PP->addPPCallbacks(
      ::std::make_unique<IncludeNormalizePPCallbacks>(SM, *this));
}

IncludeNormalizePPCallbacks::IncludeNormalizePPCallbacks(
    const SourceManager &SM, ClangTidyCheck &Check)
    : SM(SM), Check(Check) {}

void IncludeNormalizePPCallbacks::InclusionDirective(
    SourceLocation HashLoc, const Token &IncludeTok, StringRef FileName,
    bool IsAngled, CharSourceRange FilenameRange, Optional<FileEntryRef> File,
    StringRef SearchPath, StringRef RelativePath, const Module *Imported,
    SrcMgr::CharacteristicKind FileType) {

  {
    SmallString<512> Src = SM.getFilename(FilenameRange.getBegin());
    llvm::sys::path::replace_extension(Src, ".h");

    if (FileType == SrcMgr::CharacteristicKind::C_User)
      if (llvm::sys::fs::equivalent(Src, SearchPath + "/" + FileName) &&
          llvm::sys::path::filename(FileName) != FileName) {

        std::string Replacement =
            (llvm::Twine("\"") + llvm::sys::path::filename(FileName) + "\"")
                .str();
        Check.diag(FilenameRange.getBegin(),
                   "Use simple name for paired include "
                   "'%0'; consider using '%1' instead")
            << SM.getFilename(FilenameRange.getBegin()) << Replacement
            << FixItHint::CreateReplacement(FilenameRange.getAsRange(),
                                            Replacement);
      }
  }
}

} // namespace readability
} // namespace tidy
} // namespace clang
