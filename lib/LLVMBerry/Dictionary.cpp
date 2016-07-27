#include <boost/any.hpp>
#include "llvm/LLVMBerry/Dictionary.h"
#include "llvm/LLVMBerry/ValidationUnit.h"

namespace llvmberry {

SimplifyInstArg::SimplifyInstArg() {
  this->activated = false;
  this->isSwapped = false;
  this->aborted = false;
}

void SimplifyInstArg::setHintGenFunc(
    std::string _microoptName,
    std::function<void(llvm::Instruction *)> _hintGenFunc) {
  if (this->aborted)
    return;
  this->activated = true;
  this->microoptName = _microoptName;
  this->hintGenFunc = _hintGenFunc;
}

void SimplifyInstArg::abort() { this->aborted = true; }

void SimplifyInstArg::generateHint(llvm::Instruction *arg) const {
  assert(this->activated);
  if (!this->aborted) {
    auto &func = this->hintGenFunc;
    ValidationUnit::GetInstance()->intrude([&func, &arg](
        llvmberry::ValidationUnit::Dictionary &data,
        llvmberry::CoreHint &hint) { func(arg); });
  }
}

std::string SimplifyInstArg::getMicroOptName() const {
  return this->microoptName;
}

bool SimplifyInstArg::isActivated() const { return !aborted && activated; }

StripPointerCastsArg::StripPointerCastsArg() {
  strippedValues = TyStrippedValues(new TyStrippedValuesObj());
}

FindAvailableLoadedValueArg::FindAvailableLoadedValueArg() {
  orthogonalStores = TyOrthogonalStores(new TyOrthogonalStoresObj());
  ptr1EquivalentValues = TyPtrEqValues(new TyPtrEqValuesObj());
  ptr2EquivalentValues = TyPtrEqValues(new TyPtrEqValuesObj());
  isLoadStore = false;
  loadstoreStoreInst = nullptr;
}

VisitICmpArg::VisitICmpArg() {
  swapOps = false;
}

Mem2RegArg::Mem2RegArg()
    : allocas(new TyAllocasObj()), instrIndex(new TyInstrIndexObj()),
      termIndex(new TyTermIndexObj()), values(new TyValuesObj()),
      storeItem(new TyStoreItemObj()) {}

} // llvmberry
