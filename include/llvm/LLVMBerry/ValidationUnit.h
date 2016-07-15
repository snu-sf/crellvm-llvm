#ifndef VALIDATION_UNIT_H
#define VALIDATION_UNIT_H

#include <boost/any.hpp>
#include <memory>
#include <string>
#include <stack>
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/LLVMBerry/Structure.h"
#include "llvm/LLVMBerry/Dictionary.h"

namespace llvmberry {

extern std::string defaultOutputDir;

class ValidationUnit {
public:
  typedef llvmberry::Dictionary Dictionary;
  enum RETURN_CODE { COMMIT = 0, ABORT };

  CoreHint &getHint();
  const std::string &getOptimizationName() const;
  const std::string &getDescription() const;
  void setOptimizationName(const std::string &name);
  void setDescription(const std::string &str);
  void setReturnCode(RETURN_CODE return_code);
  void intrude(std::function<void(Dictionary &, CoreHint &)> func);

private:
  ValidationUnit(const std::string &optname, llvm::Function *func);
  ~ValidationUnit();
  void begin();
  void commit();
  void abort();

  std::string _filename;
  std::string _optname;
  std::string *_srcfile_buffer;
  llvm::Function *_func;
  CoreHint _corehint;
  Dictionary _data;
  RETURN_CODE _return_code;

public:
  static ValidationUnit *GetInstance();
  static void SetDefaultFunction(llvm::Function *defaultFunc);
  static void Begin(const std::string &optname);
  static void Begin(const std::string &optname, llvm::Function *func);
  static bool BeginIfNotExists(const std::string &optname,
                               llvm::Function *func);
  static void End();
  static void Abort();
  static bool EndIfExists();
  static bool Exists();

private:
  static ValidationUnit *_Instance;
  static int _Counter;
  static llvm::Function *_DefaultFunc;
};

} // llvmberry

#endif
