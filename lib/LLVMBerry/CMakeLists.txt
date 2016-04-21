add_llvm_library(LLVMLLVMBerry
  Infrules.cpp
  Structure.cpp
  ValidationUnit.cpp
  Hintgen.cpp
  )

add_dependencies(LLVMLLVMBerry LLVMBitWriter)
