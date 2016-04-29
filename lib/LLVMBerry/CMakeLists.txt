add_llvm_library(LLVMLLVMBerry
  Infrules.cpp
  Structure.cpp
  Hintgen.cpp
  ValidationUnit.cpp
  Dictionary.cpp
  )

add_dependencies(LLVMLLVMBerry LLVMBitWriter)
