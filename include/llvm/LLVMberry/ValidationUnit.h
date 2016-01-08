#include <set>
#include <boost/any.hpp>
#include "llvm/LLVMberry/Structure.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Bitcode/ReaderWriter.h"

namespace llvmberry {

class ValidationUnit {
  typedef std::map<std::string, boost::any> Dictionary;

public:
  ValidationUnit(const std::string &_optname, llvm::Function *func);
  void intrude(std::function<void(Dictionary &, structure::hints &)> func);
  void commit();
  void abort();
  // TODO: abort leaves src.bc
  
  structure::hints& getHint() { return _hints; }
  
  static ValidationUnit *getInstance();

private:
  std::string _filename;
  llvm::Function *_func;
  structure::hints _hints;
  Dictionary _data;

  void uninstall();

  static int _counter;
  static ValidationUnit *_instance;
  // TODO: comment on _instance: singleton
  // TODO: LLVM compiler is single-threaded.
};

} // llvmberry
