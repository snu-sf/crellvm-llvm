#include <boost/any.hpp>
#include "llvm/LLVMBerry/Dictionary.h"
#include "llvm/LLVMBerry/ValidationUnit.h"

namespace llvmberry {

SimplifyInstArg::SimplifyInstArg(){
  this->activated = false;
  this->isSwapped = false;
}

void SimplifyInstArg::setHintGenFunc(std::string _microoptName, std::function<void(llvm::Instruction *)> _hintGenFunc){
  this->activated = true;
  this->microoptName = _microoptName;
  this->hintGenFunc = _hintGenFunc;
}

void SimplifyInstArg::generateHint(llvm::Instruction *arg) const{
  assert(this->activated);
  auto &func = this->hintGenFunc;
  ValidationUnit::GetInstance()->intrude([&func, &arg]
      (llvmberry::ValidationUnit::Dictionary &data, llvmberry::CoreHint &hint){
    func(arg);
  });
}

std::string SimplifyInstArg::getMicroOptName() const{
  return this->microoptName;
}

bool SimplifyInstArg::isActivated() const{
  return activated;
}

StripPointerCastsArg::StripPointerCastsArg(){
  strippedValues = TyStrippedValues(new TyStrippedValuesObj());
}

FindAvailableLoadedValueArg::FindAvailableLoadedValueArg(){
  orthogonalStores = TyOrthogonalStores(new TyOrthogonalStoresObj());
  ptr1EquivalentValues = TyPtrEqValues(new TyPtrEqValuesObj());
  ptr2EquivalentValues = TyPtrEqValues(new TyPtrEqValuesObj());
  isLoadStore = false;
  loadstoreStoreInst = nullptr;
}

Mem2RegAllocaArg::Mem2RegAllocaArg()
  : allocas(new TyAllocasObj()) {}

Mem2RegInstrIndexArg::Mem2RegInstrIndexArg()
  : instrIndex(new TyInstrIndexObj()) {}

Mem2RegTermIndexArg::Mem2RegTermIndexArg()
  : termIndex(new TyTermIndexObj()) {}

Mem2RegValuesArg::Mem2RegValuesArg()
  : values(new TyValuesObj()) {}

Mem2RegStoreItemArg::Mem2RegStoreItemArg()
  : storeItem(new TyStoreItemObj()) {}
} // llvmberry
