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

class ValidationUnit {
public:
  typedef llvmberry::Dictionary Dictionary;
  enum PASS { NOTHING = 0, GVN, MEM2REG, PRE, INSTCOMBINE };

  CoreHint &getHint();
  const std::string &getOptimizationName() const;
  const std::string &getDescription() const;
  void setOptimizationName(const std::string &name);
  void setDescription(const std::string &str);
  void setIsAborted();
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
  bool isAborted;

public:
  static ValidationUnit *GetInstance();
  static void StartPass(PASS pass);
  static PASS GetCurrentPass();
  static void EndPass();
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
  static PASS _CurrentPass;
};

extern std::string defaultOutputDir;
extern std::vector<std::string> optWhiteList;
extern bool optWhiteListEnabled;
extern std::vector<ValidationUnit::PASS> optPassWhiteList;
extern bool optPassWhiteListEnabled;

void setWhiteList(const std::string &str);
void setPassWhiteList(const std::string &str);

} // llvmberry

#endif
