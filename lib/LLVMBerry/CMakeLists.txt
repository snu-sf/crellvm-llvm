add_llvm_library(LLVMLLVMBerry
  Structure.cpp
  Infrules.cpp
  ValidationUnit.cpp
  )

add_dependencies(LLVMLLVMBerry LLVMBitWriter)
