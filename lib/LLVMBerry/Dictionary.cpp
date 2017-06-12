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

void SimplifyInstArg::abort() {
  this->aborted = true;
}

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

bool SimplifyInstArg::isActivated() const {
  return !aborted && activated;
}

SelectIcmpConstArg::SelectIcmpConstArg() {
  this->activated = false;
  this->isGtToLt = false;
  this->isUnsigned = false;
  this->selectCommutative = false;
  this->Z = nullptr;
  this->Y = nullptr;
  this->X = nullptr;
  this->C = nullptr;
  this->Cprime = nullptr;
}

StripPointerCastsArg::StripPointerCastsArg() {
  strippedValues = TyStrippedValues(new TyStrippedValuesObj());
}

FindAvailableLoadedValueArg::FindAvailableLoadedValueArg() {
  orthogonalInsns = TyOrthogonalInsns(new TyOrthogonalInsnsObj());
  ptr1EquivalentValues = TyPtrEqValues(new TyPtrEqValuesObj());
  ptr2EquivalentValues = TyPtrEqValues(new TyPtrEqValuesObj());
  isLoadStore = false;
  loadstoreStoreInst = nullptr;
}

IndicesArg::IndicesArg()
    : instrIndices(new TyInstrIndicesObj()),
      termIndices(new TyTermIndicesObj()),
      useIndices(new TyUseIndicesObj()) {}

VisitICmpArg::VisitICmpArg() {
  swapOps = false;
}

Mem2RegArg::Mem2RegArg()
    : storeItem(new TyStoreItemObj()), mem2regCmd(new TyMem2RegCmdObj()),
      recentInstr(new TyRecentInstrObj()), instrWorkList(new TyInstrWorkListObj()), 
      replaceItem(new TyReplaceObj()), replaceTag(new TyReplaceTObj()) {}

bool Mem2RegArg::equalsIfConsVar(std::shared_ptr<TyExpr> e1,
                            std::shared_ptr<TyExpr> e2) {
  if (ConsVar *cv1 = dynamic_cast<ConsVar *>(e1->get().get())) {
    if (ConsVar *cv2 = dynamic_cast<ConsVar *>(e2->get().get())) {
      return TyRegister::isSame(cv1->getTyReg(), cv2->getTyReg());
    }
  }

  return false;
}

void Mem2RegArg::replaceLessthanUndef(std::string key,
                                      std::shared_ptr<TyValue> newVal) {
  if (mem2regCmd->find(key) == mem2regCmd->end())
    return;

  std::vector<std::shared_ptr<TyLessthanUndef>> &vec =
      mem2regCmd->find(key)->second.lessUndef;

  for(size_t i = 0; i < vec.size(); i++) {
    vec[i]->updateRhs(newVal);
  }
}

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
  GVNptr = nullptr;
  VNptr = nullptr;
}

PassDictionary *PassDictionary::_Instance = nullptr;
} // llvmberry
