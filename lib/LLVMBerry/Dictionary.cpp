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

Mem2RegAllocaArg::Mem2RegAllocaArg() {
  allocas = TyAllocas(new TyAllocasObj());
}

Mem2RegInstrIndexArg::Mem2RegInstrIndexArg() {
  instrIndex = TyInstrIndex(new TyInstrIndexObj());
}

Mem2RegTermIndexArg::Mem2RegTermIndexArg() {
  termIndex = TyTermIndex(new TyTermIndexObj());
}

Mem2RegStoreValArg::Mem2RegStoreValArg() {
  storeVal = TyStoreVal(new TyStoreValObj());
}

Mem2RegStoreExprArg::Mem2RegStoreExprArg() {
  storeExpr = TyStoreExpr(new TyStoreExprObj());
}

Mem2RegStoreOp0Arg::Mem2RegStoreOp0Arg() {
  storeOp0 = TyStoreOp0(new TyStoreOp0Obj());
}

Mem2RegValuesArg::Mem2RegValuesArg() {
  values  = TyValues(new TyValuesObj());
}
}
