#include <boost/any.hpp>
#include "llvm/Crellvm/Dictionary.h"
#include "llvm/Crellvm/ValidationUnit.h"

namespace crellvm {

SimplifyInstArg::SimplifyInstArg() {
  this->activated = false;
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
        crellvm::ValidationUnit::Dictionary &data,
        crellvm::CoreHint &hint) { func(arg); });
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
    : storeItem(new TyStoreItemObj()),
      recentInstr(new TyRecentInstrObj()), instrWorkList(new TyInstrWorkListObj()), 
      replaceItem(new TyReplaceObj()), replaceTag(new TyReplaceTObj()) {}
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

GVNArg::GVNArg()
  : isGVNReplace(false), VNptr(nullptr), VET(new TyVETobj), InvT(new TyInvTobj),
    CT(new TyCTobj), CTInv(new TyCTInvobj), CallPHIs(new TyCallPHIobj), VNCnt(new TyVNCntobj) {}

LICMHoistOrSinkCond::LICMHoistOrSinkCond() {
  useAA = false;
  CurLoop = nullptr;
}

PassDictionary *PassDictionary::_Instance = nullptr;
} // crellvm
