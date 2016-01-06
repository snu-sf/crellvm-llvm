#include <set>
#include "llvm/Hint/Structure.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"

namespace hintgen {
  //namespace factory {

class HintFactory {
private:
  static HintFactory theOne;
  std::string targetFileName;
  std::string fid;
  std::string mid;
  std::string opt;
  std::vector<structure::hints *> instances;

public:
  //std::vector<structure::hints *> instances;
  static HintFactory& getDefaultInstance() { return theOne; }
  structure::hints* createHintsInstance(int fileNumber, const llvm::BasicBlock& bb);
  structure::hints* createHintsInstance(int fileNumber, const llvm::Instruction& instr);

  void destroyHintsInstance(structure::hints *h);
  //std::vector<structure::hints *> getHintInstance() { return instances; }

  void setOpt(const std::string& opt_name) { opt = opt_name; }
  void setIds(int index, const llvm::BasicBlock& bb);
  //void setIds(int index, const llvm::Instruction& instr);
};

  //}
}
