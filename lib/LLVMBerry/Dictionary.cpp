#include <boost/any.hpp>
#include "llvm/LLVMBerry/Dictionary.h"
#include "llvm/LLVMBerry/ValidationUnit.h"

namespace llvmberry {

SimplifyAndInstArg::SimplifyAndInstArg(){
  this->activated = false;
  this->isSwapped = false;
}

void SimplifyAndInstArg::setHintGenFunc(std::string _microoptName, std::function<void(llvm::Instruction *)> _hintGenFunc){
  this->activated = true;
  this->microoptName = _microoptName;
  this->hintGenFunc = _hintGenFunc;
}

void SimplifyAndInstArg::generateHint(llvm::Instruction *arg) const{
  assert(this->activated);
  auto &func = this->hintGenFunc;
  ValidationUnit::GetInstance()->intrude([&func, &arg]
      (llvmberry::ValidationUnit::Dictionary &data, llvmberry::CoreHint &hint){
    func(arg);
  });
}

std::string SimplifyAndInstArg::getMicroOptName() const{
  return this->microoptName;
}

bool SimplifyAndInstArg::isActivated() const{
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

RewriteSingleStoreAllocaArg::RewriteSingleStoreAllocaArg() {
  allocas = TyAllocas(new TyAllocasObj());
}


}
