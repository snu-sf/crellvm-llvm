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
      storeItem(new TyStoreItemObj()), mem2regCmd(new TyMem2RegCmdObj()),
      transTgt(new TyTransTgtObj()), blocks(new TyBlocksObj()) {}

bool Mem2RegArg::equalsIfConsVar(std::shared_ptr<TyExpr> e1,
                            std::shared_ptr<TyExpr> e2) {
  if (ConsVar *cv1 = dynamic_cast<ConsVar *>(e1.get())) {
    if (ConsVar *cv2 = dynamic_cast<ConsVar *>(e2.get())) {
      return TyRegister::isSame(cv1->get_TyReg(), cv2->get_TyReg());
    }
  }
  return false;
}

bool Mem2RegArg::isUndef(std::shared_ptr<TyExpr> e) {
  if (ConsConst *cc = dynamic_cast<ConsConst *>(e.get()))
    if (ConsConstUndef *ccu =
          dynamic_cast<ConsConstUndef *>(cc->get_TyConst().get()))
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
      phiKey = cv->get_TyReg()->getName();

    for(size_t i = 0; i < vec.size(); i++) {
      if (equalsIfConsVar(vec[i]->get_rhs(), keyRhs)) {
        std::cout<<"checkrhs: "<<vec[i]->get_rhs()<<", "<<newExpr<<std::endl;
        vec[i]->update_rhs(newExpr);

        if (phiKey != "")
          (*mem2regCmd.get())[phiKey].lessdef.push_back(vec[i]);

        std::cout<<"check: "<<vec[i]->get_rhs()<<std::endl;
      }
      
      if (equalsIfConsVar(vec[i]->get_lhs(), keyLhs)) {
        std::cout<<"checklhs: "<<vec[i]->get_lhs()<<", "<<newExpr<<std::endl;

        if (isUndef(vec[i]->get_rhs())) {
          std::cout<<"undefreplace check"<<std::endl;
          vec[i]->update_rhs(newExpr);
        }

        std::cout<<"check: "<<vec[i]->get_lhs()<<std::endl;
      }
    }
  } else if (which == "Transitivity_e1") {
    std::cout<<"trans1 replace:"+key<<std::endl;

    std::shared_ptr<TyExpr> keyExpr = ConsVar::make(key, Physical);

    std::vector<std::pair<std::shared_ptr<TyPosition>,
                         std::shared_ptr<TyTransitivity>>> &vec =
      mem2regCmd->find(key)->second.transSrc;

    for(size_t i = 0; i < vec.size(); i++) {
      if (equalsIfConsVar(vec[i].second->get_expr2(), keyExpr)) {
        std::cout<<"check: "<<vec[i].second->get_expr1()<<", "<<newExpr<<std::endl;
        vec[i].second->update_expr1(newExpr);
        std::cout<<"check: "<<vec[i].second->get_expr1()<<std::endl;
      }
    }
  } else if (which == "Transitivity_e2") {
    std::cout<<"trans2 replace:"+key<<std::endl;

    std::shared_ptr<TyExpr> keyExpr = ConsVar::make(key, Physical);

    std::vector<std::pair<std::shared_ptr<TyPosition>,
                         std::shared_ptr<TyTransitivity>>> &vec =
      mem2regCmd->find(key)->second.transSrc;

    for(size_t i = 0; i < vec.size(); i++) {
      if (equalsIfConsVar(vec[i].second->get_expr2(), keyExpr)) {
        std::cout<<"check: "<<vec[i].second->get_expr2()<<", "<<newExpr<<std::endl;
        vec[i].second->update_expr2(newExpr);
        std::cout<<"check: "<<vec[i].second->get_expr2()<<std::endl;
      }
    }
  } else if (which == "Transitivity_e3") {
    std::cout<<"trans3 replace:"+key<<std::endl;

    std::shared_ptr<TyExpr> keyExpr = ConsVar::make(key, Physical);

    std::vector<std::pair<std::shared_ptr<TyPosition>,
                         std::shared_ptr<TyTransitivity>>> &vec =
      mem2regCmd->find(key)->second.transSrc;

    for(size_t i = 0; i < vec.size(); i++) {
      if (equalsIfConsVar(vec[i].second->get_expr2(), keyExpr)) {
        std::cout<<"check: "<<vec[i].second->get_expr3()<<", "<<newExpr<<std::endl;
        vec[i].second->update_expr3(newExpr);
        std::cout<<"check: "<<vec[i].second->get_expr3()<<std::endl;
      }
    }
  } else if (which == "TransitivityTgt_e2") {
    std::cout<<"transTgt2 replace:"+key<<std::endl;

    std::string phiKey = "";
    if (ConsVar *cv = dynamic_cast<ConsVar *>(newExpr.get()))
      phiKey = cv->get_TyReg()->getName();

    std::shared_ptr<TyExpr> keyExpr = ConsVar::make(key, Physical);

    std::vector<std::shared_ptr<TyTransitivityTgt>> &vec =
      mem2regCmd->find(key)->second.transTgt;

    for(size_t i = 0; i < vec.size(); i++) {
      if (equalsIfConsVar(vec[i]->get_expr2(), keyExpr)) {
        std::cout<<"check: "<<vec[i]->get_expr2()<<", "<<newExpr<<std::endl;
        vec[i]->update_expr2(newExpr);

        if (phiKey != "")
          (*mem2regCmd.get())[phiKey].transTgt.push_back(vec[i]);
        std::cout<<"check: "<<vec[i]->get_expr2()<<std::endl;
      }
    }
  } else if (which == "TransitivityTgt_e3") {
    std::cout<<"transTgt3 replace:"+key<<std::endl;

    std::string phiKey = "";
    if (ConsVar *cv = dynamic_cast<ConsVar *>(newExpr.get()))
      phiKey = cv->get_TyReg()->getName();

    std::shared_ptr<TyExpr> keyExpr = ConsVar::make(key, Physical);

    std::vector<std::shared_ptr<TyTransitivityTgt>> &vec =
      mem2regCmd->find(key)->second.transTgt;

    for(size_t i = 0; i < vec.size(); i++) {
      if (equalsIfConsVar(vec[i]->get_expr3(), keyExpr)) {
        std::cout<<"check: "<<vec[i]->get_expr3()<<", "<<newExpr<<std::endl;
        vec[i]->update_expr3(newExpr);

        if (phiKey != "")
          (*mem2regCmd.get())[phiKey].transTgt.push_back(vec[i]);
        std::cout<<"check: "<<vec[i]->get_expr3()<<std::endl;
      }
    }
  } else if (which == "IntroGhost") {
    std::cout<<"ghost replace:"+key<<std::endl;

    std::shared_ptr<TyExpr> keyExpr = ConsVar::make(key, Physical);
    std::vector<std::shared_ptr<TyIntroGhost>> &vec =
      mem2regCmd->find(key)->second.ghost;

    for(size_t i = 0; i < vec.size(); i++) {
      if (equalsIfConsVar(vec[i]->get_expr(), keyExpr)) {
        std::cout<<"check: "<<vec[i]->get_expr()<<", "<<newExpr<<std::endl;
        vec[i]->update_expr(newExpr);
        std::cout<<"check: "<<vec[i]->get_expr()<<std::endl;
      }
    }
  }
}

void Mem2RegArg::replaceTransTgtPrev() {
  std::cout<<"TransTgtPrev"<<std::endl;
  std::vector<std::shared_ptr<TyTransitivityTgt>> &vec = *transTgt.get();

  for(size_t i = 0; i < vec.size(); i++) {
    if (ConsVar *cv = dynamic_cast<ConsVar *>(vec[i]->get_expr2().get())) {
      vec[i]->update_expr2(std::shared_ptr<TyExpr>
                            (new ConsVar(std::shared_ptr<TyRegister>
                                          (new TyRegister(cv->get_TyReg()->getName(),
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
    vec[i]->update_rhs(newVal);
  }
  std::cout<<"LessthanUndefEnd"<<std::endl;
}
} // llvmberry
