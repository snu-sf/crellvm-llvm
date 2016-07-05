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
      storeItem(new TyStoreItemObj()), mem2regCmd(new TyMem2RegCmdObj()) {}

bool Mem2RegArg::equalsIfConsVar(std::shared_ptr<TyExpr> e1,
                            std::shared_ptr<TyExpr> e2) {
    if (ConsVar *cv1 = dynamic_cast<ConsVar *>(e1.get())) {
      if (ConsVar *cv2 = dynamic_cast<ConsVar *>(e2.get())) {
        return TyRegister::isSame(cv1->get_TyReg(), cv2->get_TyReg());
      }
    }

    return false;
}

void Mem2RegArg::CmdTriple::replaceCmdRhs(std::string key, std::shared_ptr<TyExpr> newExpr) {
  //assert(mem2regCmd->find(key) != mem2regCmd->end());
 
  std::cout<<"replace:"+key<<std::endl;

  std::shared_ptr<TyExpr> keyExpr = ConsVar::make(key, Physical);

  std::vector<std::shared_ptr<TyPropagateLessdef>> &vec = //mem2regCmd->find(key)->second.lessdef;
                                                          this->lessdef;
  for(size_t i = 0; i < vec.size(); i++) {
    if (equalsIfConsVar(vec[i]->get_rhs(), keyExpr)) {
      std::cout<<"check: "<<vec[i]->get_rhs()<<", "<<newExpr<<std::endl;
      vec[i]->update_rhs(newExpr);
      std::cout<<"check: "<<vec[i]->get_rhs()<<std::endl;
    }
  }
}

} // llvmberry
