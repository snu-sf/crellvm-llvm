add_llvm_library(LLVMLLVMBerry
  Structure.cpp
  ValidationUnit.cpp
  )

add_dependencies(LLVMLLVMBerry LLVMBitWriter)
