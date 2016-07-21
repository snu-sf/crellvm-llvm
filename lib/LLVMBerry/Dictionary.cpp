#include <boost/any.hpp>
#include "llvm/LLVMBerry/Dictionary.h"
#include "llvm/LLVMBerry/ValidationUnit.h"

namespace llvmberry {

SimplifyInstArg::SimplifyInstArg() {
  this->activated = false;
  this->isSwapped = false;
}

void SimplifyInstArg::setHintGenFunc(
    std::string _microoptName,
    std::function<void(llvm::Instruction *)> _hintGenFunc) {
  this->activated = true;
  this->microoptName = _microoptName;
  this->hintGenFunc = _hintGenFunc;
}

void SimplifyInstArg::generateHint(llvm::Instruction *arg) const {
  assert(this->activated);
  auto &func = this->hintGenFunc;
  ValidationUnit::GetInstance()->intrude(
      [&func, &arg](llvmberry::ValidationUnit::Dictionary &data,
                    llvmberry::CoreHint &hint) { func(arg); });
}

std::string SimplifyInstArg::getMicroOptName() const {
  return this->microoptName;
}

bool SimplifyInstArg::isActivated() const { return activated; }

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

// PassDictionary

void PassDictionary::Create() {
  assert(!_Instance && "PassDictionary Instance already exists!");
  _Instance = new PassDictionary();
}

PassDictionary &PassDictionary::GetInstance() { return *_Instance; }

void PassDictionary::Destroy() {
  assert(_Instance && "PassDictionary Instance doesn't exist!");
  delete _Instance;
  _Instance = nullptr;
}

GVNReplaceArg::GVNReplaceArg() {
  isGVNReplace = false;
  BB = nullptr;
  VNptr = nullptr;
}

PassDictionary *PassDictionary::_Instance = nullptr;
} // llvmberry
