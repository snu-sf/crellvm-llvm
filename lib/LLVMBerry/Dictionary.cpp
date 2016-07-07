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

Mem2RegArg::Mem2RegArg()
    : allocas(new TyAllocasObj()), instrIndex(new TyInstrIndexObj()),
      termIndex(new TyTermIndexObj()), values(new TyValuesObj()),
      storeItem(new TyStoreItemObj()), mem2regCmd(new TyMem2RegCmdObj()),
      transTgt(new TyTransTgtObj()) {}

bool Mem2RegArg::equalsIfConsVar(std::shared_ptr<TyExpr> e1,
                            std::shared_ptr<TyExpr> e2) {
    if (ConsVar *cv1 = dynamic_cast<ConsVar *>(e1.get())) {
      if (ConsVar *cv2 = dynamic_cast<ConsVar *>(e2.get())) {
        return TyRegister::isSame(cv1->get_TyReg(), cv2->get_TyReg());
      }
    }

    return false;
}

void Mem2RegArg::Tuple::replaceCmdRhs(std::string which, std::string key,
                                          std::shared_ptr<TyExpr> newExpr) {
  //assert(mem2regCmd->find(key) != mem2regCmd->end());
  if (which == "Lessdef") {
    std::cout<<"lessdef replace:"+key<<std::endl;

    std::shared_ptr<TyExpr> keyExpr = ConsVar::make(key, Physical);

    std::vector<std::shared_ptr<TyPropagateLessdef>> &vec =
      this->lessdef;

    for(size_t i = 0; i < vec.size(); i++) {
      if (equalsIfConsVar(vec[i]->get_rhs(), keyExpr)) {
        std::cout<<"check: "<<vec[i]->get_rhs()<<", "<<newExpr<<std::endl;
        vec[i]->update_rhs(newExpr);
        std::cout<<"check: "<<vec[i]->get_rhs()<<std::endl;
      }
    }
  } else if (which == "Transitivity_e1") {
    std::cout<<"trans replace:"+key<<std::endl;

    std::shared_ptr<TyExpr> keyExpr = ConsVar::make(key, Physical);

    std::vector<std::pair<std::shared_ptr<TyPosition>,
                         std::shared_ptr<TyTransitivity>>> &vec =
      this->transSrc;

    for(size_t i = 0; i < vec.size(); i++) {
      if (equalsIfConsVar(vec[i].second->get_expr2(), keyExpr)) {
        std::cout<<"check: "<<vec[i].second->get_expr1()<<", "<<newExpr<<std::endl;
        vec[i].second->update_expr1(newExpr);
        std::cout<<"check: "<<vec[i].second->get_expr1()<<std::endl;
      }
    }
  } else if (which == "Transitivity_e2") {
    std::cout<<"trans replace:"+key<<std::endl;

    std::shared_ptr<TyExpr> keyExpr = ConsVar::make(key, Physical);

    std::vector<std::pair<std::shared_ptr<TyPosition>,
                         std::shared_ptr<TyTransitivity>>> &vec =
      this->transSrc;

    for(size_t i = 0; i < vec.size(); i++) {
      if (equalsIfConsVar(vec[i].second->get_expr2(), keyExpr)) {
        std::cout<<"check: "<<vec[i].second->get_expr2()<<", "<<newExpr<<std::endl;
        vec[i].second->update_expr2(newExpr);
        std::cout<<"check: "<<vec[i].second->get_expr2()<<std::endl;
      }
    }
  } else if (which == "Transitivity_e3") {
    std::cout<<"trans replace:"+key<<std::endl;

    std::shared_ptr<TyExpr> keyExpr = ConsVar::make(key, Physical);

    std::vector<std::pair<std::shared_ptr<TyPosition>,
                         std::shared_ptr<TyTransitivity>>> &vec =
      this->transSrc;

    for(size_t i = 0; i < vec.size(); i++) {
      if (equalsIfConsVar(vec[i].second->get_expr2(), keyExpr)) {
        std::cout<<"check: "<<vec[i].second->get_expr3()<<", "<<newExpr<<std::endl;
        vec[i].second->update_expr3(newExpr);
        std::cout<<"check: "<<vec[i].second->get_expr3()<<std::endl;
      }
    }
  } else if (which == "TransitivityTgt") {
    std::cout<<"transTgt replace:"+key<<std::endl;

    std::shared_ptr<TyExpr> keyExpr = ConsVar::make(key, Physical);

    std::vector<std::shared_ptr<TyTransitivityTgt>> &vec =
      this->transTgt;

    for(size_t i = 0; i < vec.size(); i++) {
      if (equalsIfConsVar(vec[i]->get_expr3(), keyExpr)) {
        std::cout<<"check: "<<vec[i]->get_expr3()<<", "<<newExpr<<std::endl;
        vec[i]->update_expr3(newExpr);
        std::cout<<"check: "<<vec[i]->get_expr3()<<std::endl;
      }
    }
  } else if (which == "IntroGhost") {
    std::cout<<"ghost replace:"+key<<std::endl;

    std::shared_ptr<TyExpr> keyExpr = ConsVar::make(key, Physical);
    std::vector<std::shared_ptr<TyIntroGhost>> &vec =
      this->ghost;

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
  std::vector<std::shared_ptr<TyTransitivityTgt>> &vec = *this->transTgt.get();

  for(size_t i = 0; i < vec.size(); i++) {
    if (ConsVar *cv = dynamic_cast<ConsVar *>(vec[i]->get_expr2().get())) {
      vec[i]->update_expr2(std::shared_ptr<TyExpr>
                            (new ConsVar(std::shared_ptr<TyRegister>
                                          (new TyRegister(cv->get_TyReg()->getName(),
                                                          Previous)))));
//      cv->updateTyReg(std::shared_ptr<TyRegister>
//                       (new TyRegister(cv->get_TyReg()->getName(), Previous)));
    }
  }
}

} // llvmberry
