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
      replaceItem(new TyReplaceObj()) {}

bool Mem2RegArg::equalsIfConsVar(std::shared_ptr<TyExpr> e1,
                            std::shared_ptr<TyExpr> e2) {
  if (ConsVar *cv1 = dynamic_cast<ConsVar *>(e1->get_i().get())) {
    if (ConsVar *cv2 = dynamic_cast<ConsVar *>(e2->get_i().get())) {
      return TyRegister::isSame(cv1->getTyReg(), cv2->getTyReg());
    }
  }

  return false;
}

bool Mem2RegArg::isUndef(std::shared_ptr<TyExpr> e) {
  if (ConsConst *cc = dynamic_cast<ConsConst *>(e->get_i().get()))
    if (ConsConstUndef *ccu =
          dynamic_cast<ConsConstUndef *>(cc->getTyConst().get()))
      return true;

  return false;
}

void Mem2RegArg::replaceCmdRhs(std::string which, std::string key,
                               std::shared_ptr<TyExpr> newExpr) {
  if (mem2regCmd->find(key) == mem2regCmd->end())
    return;

  if (which == "Lessdef") {

    std::shared_ptr<TyExpr> keyRhs = ConsVar::make(key, Physical);
    std::shared_ptr<TyExpr> keyLhs = ConsVar::make(key, Ghost);

    std::vector<std::shared_ptr<TyPropagateLessdef>> &vec =
      mem2regCmd->find(key)->second.lessdef;

    std::string phiKey = "";
    if (ConsVar *cv = dynamic_cast<ConsVar *>(newExpr->get_i().get()))
      phiKey = std::string(cv->getTyReg()->getName());

    for(size_t i = 0; i < vec.size(); i++) {
      if (equalsIfConsVar(vec[i]->getRhs(), keyRhs)) {
        vec[i]->updateRhs(newExpr);

        if (phiKey != "")
          (*mem2regCmd.get())[phiKey].lessdef.push_back(vec[i]);
      }
      
      if (equalsIfConsVar(vec[i]->getLhs(), keyLhs)) {
        if (isUndef(vec[i]->getRhs())) {
          vec[i]->updateRhs(newExpr);
        }
      }
    }
  } else if (which == "IntroGhost") {
    std::shared_ptr<TyExpr> keyExpr = ConsVar::make(key, Physical);
    std::vector<std::shared_ptr<TyIntroGhost>> &vec =
      mem2regCmd->find(key)->second.ghost;

    for(size_t i = 0; i < vec.size(); i++) {
      if (equalsIfConsVar(vec[i]->getExpr(), keyExpr)) {
        vec[i]->updateExpr(newExpr);
      }
    }
  }
  return;
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
  BB = nullptr;
  VNptr = nullptr;
}

PassDictionary *PassDictionary::_Instance = nullptr;
} // llvmberry
