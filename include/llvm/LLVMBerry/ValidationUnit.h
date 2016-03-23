#ifndef VALIDATION_UNIT_H
#define VALIDATION_UNIT_H

#include <boost/any.hpp>
#include <memory>
#include <stack>
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/LLVMBerry/Structure.h"

namespace llvmberry {

extern std::string defaultOutputDir;

class ValidationUnit {
public:
  typedef std::map<std::string, boost::any> Dictionary;
  enum RETURN_CODE { COMMIT = 0, ABORT };

  CoreHint &getHint();
  const std::string &getOptimizationName();
  void setReturnCode(RETURN_CODE return_code);
  void intrude(std::function<void(Dictionary &, CoreHint &)> func);

private:
  ValidationUnit(const std::string &optname, llvm::Function *func);
  ~ValidationUnit();
  void begin();
  void commit();
  void abort();
  // TODO: abort leaves src.bc

  std::string _filename;
  std::string _optname;
  llvm::Function *_func;
  CoreHint _corehint;
  Dictionary _data;
  RETURN_CODE _return_code;

public:
  static ValidationUnit *GetInstance();
  static void Begin(const std::string &optname, llvm::Function *func);
  static bool BeginIfNotExists(const std::string &optname,
                               llvm::Function *func);
  static bool HasBegun();
  static void End();
  static bool EndIfExists();

private:
  static bool Exists();
  static ValidationUnit *_Instance;
  static int _Counter;
};

} // llvmberry

#endif
