set(LLVM_LINK_COMPONENTS support)

add_clang_executable(identifier-naming
  IdentifierNaming.cpp
  IdentifierNamingUtils.cpp
  )

target_link_libraries(identifier-naming
  PRIVATE
  clangAST
  clangASTMatchers
  clangBasic
  clangFrontend
  clangSerialization
  clangTooling
  clangRewriteFrontend
  )
