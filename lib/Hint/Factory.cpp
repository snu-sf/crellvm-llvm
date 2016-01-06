#include "llvm/Hint/Factory.h"
#include <cassert>

namespace hintgen {
  //namespace factory {
HintFactory HintFactory::theOne;

structure::hints* HintFactory::createHintsInstance(int fileNumber, const llvm::BasicBlock& bb) {
  //structure::hints* h = new structure::hints(function_id, module_id, opt_name);
  HintFactory::setIds(fileNumber, bb);

  structure::hints* h = new structure::hints(fid, mid, opt);

  assert(instances.size() == 0);
  instances.push_back(h);

  return h;
}

structure::hints* HintFactory::createHintsInstance(int fileNumber, const llvm::Instruction& instr) {
  return HintFactory::createHintsInstance(fileNumber, *instr.getParent());
}

void HintFactory::destroyHintsInstance(structure::hints *h) {
  assert(!instances.empty());
  instances.erase(std::find(instances.begin(), instances.end(), h));
}

void HintFactory::setIds(int index, const llvm::BasicBlock& bb) {
  const llvm::BasicBlock* hintTargetBlock = &bb;
  const llvm::Function* hintTargetFunc = hintTargetBlock->getParent();
  const llvm::Module* hintTargetModule = hintTargetFunc->getParent();

  targetFileName.clear();
  mid = hintTargetModule->getModuleIdentifier();
  targetFileName = mid;

  targetFileName = targetFileName.substr(0, targetFileName.find_last_of("."));

  targetFileName += ".";
  fid = hintTargetFunc->getName().str();
  targetFileName += fid;

  std::stringstream ss;
  ss << targetFileName << "." << index << ".json";

  targetFileName = ss.str();
}

//void HintFactory::setIds(int index, const Instruction& instr) {
//  HintFactory::setIds(index, *instr.getParent());
//}
  //} // factory
} // hintgen
