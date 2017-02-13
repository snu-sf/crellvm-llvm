add_llvm_library(LLVMLLVMBerry
  Infrules.cpp
  Structure.cpp
  Hintgen.cpp
  ValidationUnit.cpp
  Dictionary.cpp
  InstCombine/InfrulesAddSub.cpp
  InstCombine/InfrulesAndOrXor.cpp
  InstCombine/InfrulesCasts.cpp
  InstCombine/InfrulesCompares.cpp
  InstCombine/InfrulesLoadStoreAlloca.cpp
  InstCombine/InfrulesMulDivRem.cpp
  InstCombine/InfrulesSelect.cpp
  )

add_dependencies(LLVMLLVMBerry LLVMBitWriter)
