add_llvm_library(LLVMLLVMBerry
  Infrules.cpp
  Structure.cpp
  Hintgen.cpp
  ValidationUnit.cpp
  Hintgen.cpp
  )

add_dependencies(LLVMLLVMBerry LLVMBitWriter)
