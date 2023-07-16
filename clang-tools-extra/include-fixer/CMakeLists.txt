set(LLVM_LINK_COMPONENTS support)

add_clang_executable(include-fixer
  IncludeFixer.cpp
  )

target_link_libraries(include-fixer
  PRIVATE
  clangAST
  clangASTMatchers
  clangBasic
  clangFrontend
  clangSerialization
  clangTooling
  clangRewriteFrontend
  )
