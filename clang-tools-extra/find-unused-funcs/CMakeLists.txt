set(LLVM_LINK_COMPONENTS
  FrontendOpenMP
  Support
  )

add_clang_executable(find-unused-funcs
  ToolTemplate.cpp
  )

clang_target_link_libraries(find-unused-funcs
  PRIVATE
  clangAST
  clangASTMatchers
  clangBasic
  clangFrontend
  clangTooling
  clangToolingRefactoring
  )
