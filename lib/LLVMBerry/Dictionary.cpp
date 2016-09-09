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
    : allocas(new TyAllocasObj()), diffBlocks(new TyDiffblocksObj()),
      instrIndex(new TyInstrIndexObj()), termIndex(new TyTermIndexObj()),
      values(new TyValuesObj()), reachedEdge(new TyReachedEdgeObj()),
      storeItem(new TyStoreItemObj()), mem2regCmd(new TyMem2RegCmdObj()),
      transTgt(new TyTransTgtObj()), blocks(new TyBlocksObj()),
      strVec(new TyStrVecObj()), blockPairVec(new TyBlockPairVecObj()),
      SImap(new TySImapObj()), isReachable(new TyReachableObj()) {}

bool Mem2RegArg::equalsIfConsVar(std::shared_ptr<TyExpr> e1,
                            std::shared_ptr<TyExpr> e2) {
  if (ConsVar *cv1 = dynamic_cast<ConsVar *>(e1.get())) {
    if (ConsVar *cv2 = dynamic_cast<ConsVar *>(e2.get())) {
      return TyRegister::isSame(cv1->getTyReg(), cv2->getTyReg());
    }
  }
  return false;
}

bool Mem2RegArg::isUndef(std::shared_ptr<TyExpr> e) {
  if (ConsConst *cc = dynamic_cast<ConsConst *>(e.get()))
    if (ConsConstUndef *ccu =
          dynamic_cast<ConsConstUndef *>(cc->getTyConst().get()))
      return true;

  return false;
}

void Mem2RegArg::replaceCmdRhs(std::string which, std::string key,
                               std::shared_ptr<TyExpr> newExpr) {
  std::cout<<"replaceCmdRhsbegin: "+which+", "+key<<std::endl;
  if (mem2regCmd->find(key) == mem2regCmd->end())
    return;

  std::cout<<"replaceCmdRhsstart: "+which+", "+key<<std::endl;
  if (which == "Lessdef") {
    std::cout<<"lessdef replace:"+key<<std::endl;

    std::shared_ptr<TyExpr> keyRhs = ConsVar::make(key, Physical);
    std::shared_ptr<TyExpr> keyLhs = ConsVar::make(key, Ghost);

    std::vector<std::shared_ptr<TyPropagateLessdef>> &vec =
      mem2regCmd->find(key)->second.lessdef;

    std::string phiKey = "";
    if (ConsVar *cv = dynamic_cast<ConsVar *>(newExpr.get()))
      phiKey = cv->getTyReg()->getName();

    for(size_t i = 0; i < vec.size(); i++) {
      if (equalsIfConsVar(vec[i]->getRhs(), keyRhs)) {
        std::cout<<"checkrhs: "<<vec[i]->getRhs()<<", "<<newExpr<<std::endl;
        vec[i]->updateRhs(newExpr);

        if (phiKey != "")
          (*mem2regCmd.get())[phiKey].lessdef.push_back(vec[i]);

        std::cout<<"check: "<<vec[i]->getRhs()<<std::endl;
      }
      
      if (equalsIfConsVar(vec[i]->getLhs(), keyLhs)) {
        std::cout<<"checklhs: "<<vec[i]->getLhs()<<", "<<newExpr<<std::endl;

        if (isUndef(vec[i]->getRhs())) {
          std::cout<<"undefreplace check"<<std::endl;
          vec[i]->updateRhs(newExpr);
        }

        std::cout<<"check: "<<vec[i]->getLhs()<<std::endl;
      }
    }
  } else if (which == "Transitivity_e1") {
    std::cout<<"trans1 replace:"+key<<std::endl;

    std::shared_ptr<TyExpr> keyExpr = ConsVar::make(key, Physical);

    std::vector<std::pair<std::shared_ptr<TyPosition>,
                         std::shared_ptr<TyTransitivity>>> &vec =
      mem2regCmd->find(key)->second.transSrc;

    for(size_t i = 0; i < vec.size(); i++) {
      if (equalsIfConsVar(vec[i].second->getExpr2(), keyExpr)) {
        std::cout<<"check: "<<vec[i].second->getExpr1()<<", "<<newExpr<<std::endl;
        vec[i].second->updateExpr1(newExpr);
        std::cout<<"check: "<<vec[i].second->getExpr1()<<std::endl;
      }
    }
  } else if (which == "Transitivity_e2") {
    std::cout<<"trans2 replace:"+key<<std::endl;

    std::shared_ptr<TyExpr> keyExpr = ConsVar::make(key, Physical);

    std::vector<std::pair<std::shared_ptr<TyPosition>,
                         std::shared_ptr<TyTransitivity>>> &vec =
      mem2regCmd->find(key)->second.transSrc;

    for(size_t i = 0; i < vec.size(); i++) {
      if (equalsIfConsVar(vec[i].second->getExpr2(), keyExpr)) {
        std::cout<<"check: "<<vec[i].second->getExpr2()<<", "<<newExpr<<std::endl;
        vec[i].second->updateExpr2(newExpr);
        std::cout<<"check: "<<vec[i].second->getExpr2()<<std::endl;
      }
    }
  } else if (which == "Transitivity_e3") {
    std::cout<<"trans3 replace:"+key<<std::endl;

    std::shared_ptr<TyExpr> keyExpr = ConsVar::make(key, Physical);

    std::vector<std::pair<std::shared_ptr<TyPosition>,
                         std::shared_ptr<TyTransitivity>>> &vec =
      mem2regCmd->find(key)->second.transSrc;

    for(size_t i = 0; i < vec.size(); i++) {
      if (equalsIfConsVar(vec[i].second->getExpr2(), keyExpr)) {
        std::cout<<"check: "<<vec[i].second->getExpr3()<<", "<<newExpr<<std::endl;
        vec[i].second->updateExpr3(newExpr);
        std::cout<<"check: "<<vec[i].second->getExpr3()<<std::endl;
      }
    }
  } else if (which == "TransitivityTgt_e2") {
    std::cout<<"transTgt2 replace:"+key<<std::endl;
    std::string phiKey = "";
    if (ConsVar *cv = dynamic_cast<ConsVar *>(newExpr.get()))
      phiKey = "%" + std::string(cv->getTyReg()->getName());

    std::shared_ptr<TyExpr> keyExpr = ConsVar::make(key, Physical);

    std::shared_ptr<TyExpr> keyExprGhost = ConsVar::make(key, Ghost);

    std::vector<std::shared_ptr<TyTransitivityTgt>> &vec =
      mem2regCmd->find(key)->second.transTgt;

    for(size_t i = 0; i < vec.size(); i++) {
      if (equalsIfConsVar(vec[i]->getExpr2(), keyExpr)) {
        std::cout<<"check: "<<vec[i]->getExpr2()<<", "<<newExpr<<std::endl;
        vec[i]->updateExpr2(newExpr);

        if (phiKey != "")
          (*mem2regCmd.get())[phiKey].transTgt.push_back(vec[i]);
        std::cout<<"check: "<<vec[i]->getExpr2()<<std::endl;
      }

      if (equalsIfConsVar(vec[i]->getExpr2(), keyExprGhost)) {
        std::cout<<"check: "<<vec[i]->getExpr2()<<", "<<newExpr<<std::endl;
        vec[i]->updateExpr2(newExpr);

        if (phiKey != "")
          (*mem2regCmd.get())[phiKey].transTgt.push_back(vec[i]);
        std::cout<<"check: "<<vec[i]->getExpr2()<<std::endl;
      }

    }
  } else if (which == "TransitivityTgt_e3") {
    std::cout<<"transTgt3 replace:"+key<<std::endl;

    std::string phiKey = "";
    if (ConsVar *cv = dynamic_cast<ConsVar *>(newExpr.get()))
      phiKey = cv->getTyReg()->getName();

    std::shared_ptr<TyExpr> keyExpr = ConsVar::make(key, Physical);

    std::vector<std::shared_ptr<TyTransitivityTgt>> &vec =
      mem2regCmd->find(key)->second.transTgt;

    for(size_t i = 0; i < vec.size(); i++) {
      if (equalsIfConsVar(vec[i]->getExpr3(), keyExpr)) {
        std::cout<<"check: "<<vec[i]->getExpr3()<<", "<<newExpr<<std::endl;
        vec[i]->updateExpr3(newExpr);

        if (phiKey != "")
          (*mem2regCmd.get())[phiKey].transTgt.push_back(vec[i]);
        std::cout<<"check: "<<vec[i]->getExpr3()<<std::endl;
      }
    }
  } else if (which == "IntroGhost") {
    std::cout<<"ghost replace:"+key<<std::endl;

    std::shared_ptr<TyExpr> keyExpr = ConsVar::make(key, Physical);
    std::vector<std::shared_ptr<TyIntroGhost>> &vec =
      mem2regCmd->find(key)->second.ghost;

    for(size_t i = 0; i < vec.size(); i++) {
      if (equalsIfConsVar(vec[i]->getExpr(), keyExpr)) {
        std::cout<<"check: "<<vec[i]->getExpr()<<", "<<newExpr<<std::endl;
        vec[i]->updateExpr(newExpr);
        std::cout<<"check: "<<vec[i]->getExpr()<<std::endl;
      }
    }
  }
}

void Mem2RegArg::replaceTransTgtPrev() {
  std::cout<<"TransTgtPrev"<<std::endl;
  std::vector<std::shared_ptr<TyTransitivityTgt>> &vec = *transTgt.get();

  for(size_t i = 0; i < vec.size(); i++) {
    if (ConsVar *cv = dynamic_cast<ConsVar *>(vec[i]->getExpr2().get())) {
      vec[i]->updateExpr2(std::shared_ptr<TyExpr>
                            (new ConsVar(std::shared_ptr<TyRegister>
                                          (new TyRegister(cv->getTyReg()->getName(),
                                                          Previous)))));
    }
  }
  std::cout<<"TransTgtPrevEnd"<<std::endl;
}

void Mem2RegArg::replaceLessthanUndef(std::string key,
                                      std::shared_ptr<TyValue> newVal) {
  if (mem2regCmd->find(key) == mem2regCmd->end())
    return;

  std::cout<<"LessthanUndef"<<std::endl;
  std::vector<std::shared_ptr<TyLessthanUndef>> &vec =
      mem2regCmd->find(key)->second.lessUndef;

  for(size_t i = 0; i < vec.size(); i++) {
    vec[i]->updateRhs(newVal);
  }
  std::cout<<"LessthanUndefEnd"<<std::endl;
}

  void Mem2RegArg::replaceLessthanUndefTgt(std::string key,
                                        std::shared_ptr<TyValue> newVal) {
    if (mem2regCmd->find(key) == mem2regCmd->end())
      return;

    std::cout<<"LessthanUndefTgt"<<std::endl;
    std::vector<std::shared_ptr<TyLessthanUndefTgt>> &vec =
            mem2regCmd->find(key)->second.lessUndefTgt;

    for(size_t i = 0; i < vec.size(); i++) {
      vec[i]->updateRhs(newVal);
    }
    std::cout<<"LessthanUndefEndTgt"<<std::endl;
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
