#ifndef DICTIONARY_H
#define DICTIONARY_H

#include "llvm/IR/Instructions.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/LLVMBerry/Structure.h"

#include <memory>


namespace llvmberry {

enum DictKeys {
  // Common
  ArgForStripPointerCasts,
  ArgForFindAvailableLoadedValue,
  // InstSimplify
  ArgForSimplifyAndInst,
  ArgForSimplifyOrInst,
  // InstCombine
  ArgForVisitMul,
  ArgForFoldSelectOpOp,
  // Mem2Reg
  ArgForMem2RegAlloca,
  ArgForMem2RegInstrIndex,
  ArgForMem2RegTermIndex,
  ArgForMem2RegStoreVal,
  ArgForMem2RegStoreExpr,
  ArgForMem2RegStoreOp0, 
  ArgForMem2RegValues,
  // GVN
  ArgForFindLeader
};

/*
 * Using Traits
 * DictKeysTraits class defines which type to use for dictionary value
 * per each key type (in DictKeys)
 * If you define
 *   DEFINE_TRAITS(A, B)
 * then Dictionary will use B as a value type for the key A
 * (where A must be in DictKeys enum)
 */
#define DEFINE_TRAITS(KEYVAL, CLASSNAME) \
    template<> struct DictKeysTraits<KEYVAL> { \
      typedef CLASSNAME ty; \
    }
template<DictKeys key>
struct DictKeysTraits {
  // DictKeysTraits class has no member for arbitrary type
};


// Used in InstructionSimplify.cpp : SimplifyAndInst(), SimplifyOrInst()
struct SimplifyInstArg {
public:
  SimplifyInstArg();
  void setHintGenFunc(std::string microoptName, std::function<void(llvm::Instruction *)> hintGenFunc);
  void generateHint(llvm::Instruction *arg) const;
  bool isActivated() const;
  std::string getMicroOptName() const;
  bool isSwapped;
private:
  bool activated;
  std::string microoptName;
  std::function<void(llvm::Instruction *)> hintGenFunc;
};
DEFINE_TRAITS(ArgForSimplifyAndInst, SimplifyInstArg);
DEFINE_TRAITS(ArgForSimplifyOrInst, SimplifyInstArg);

// lib/IR/Value.cpp : Value::stripPointerCasts(), stripPointerCastsAndOffsets()
struct StripPointerCastsArg {
public:
  typedef std::vector<llvm::Value *> TyStrippedValuesObj;
  typedef std::shared_ptr<TyStrippedValuesObj> TyStrippedValues;
  TyStrippedValues strippedValues;

  StripPointerCastsArg();
};
DEFINE_TRAITS(ArgForStripPointerCasts, StripPointerCastsArg);

// lib/Analysis/Loads.cpp : FindAvailableLoadedValueArg
struct FindAvailableLoadedValueArg {
public: 
  typedef std::vector<std::pair<
      llvmberry::StripPointerCastsArg::TyStrippedValues, 
      std::pair<llvm::StoreInst *, std::string> > > TyOrthogonalStoresObj;
  typedef llvmberry::StripPointerCastsArg::TyStrippedValuesObj TyPtrEqValuesObj;
  typedef std::shared_ptr<TyOrthogonalStoresObj> TyOrthogonalStores;
  typedef std::shared_ptr<TyPtrEqValuesObj> TyPtrEqValues;
  TyOrthogonalStores orthogonalStores;
  TyPtrEqValues ptr1EquivalentValues;
  TyPtrEqValues ptr2EquivalentValues;
  bool isLoadStore;
  llvm::StoreInst *loadstoreStoreInst;
  
  FindAvailableLoadedValueArg();
};
DEFINE_TRAITS(ArgForFindAvailableLoadedValue, FindAvailableLoadedValueArg);

// lib/Transforms/Utils/PromoteMemoryToRegister.cpp : Alloca
struct Mem2RegAllocaArg {
public:
  typedef std::vector<llvm::AllocaInst *> TyAllocasObj;
  typedef std::shared_ptr<TyAllocasObj> TyAllocas;
  TyAllocas allocas;

  Mem2RegAllocaArg();
};
DEFINE_TRAITS(ArgForMem2RegAlloca, Mem2RegAllocaArg);

// lib/Transforms/Utils/PromoteMemoryToRegister.cpp : InstrIndex
struct Mem2RegInstrIndexArg {
public:
  typedef std::map<const llvm::Instruction*, unsigned> TyInstrIndexObj;
  typedef std::shared_ptr<TyInstrIndexObj> TyInstrIndex;
  TyInstrIndex instrIndex;

  Mem2RegInstrIndexArg();
};
DEFINE_TRAITS(ArgForMem2RegInstrIndex, Mem2RegInstrIndexArg);

// lib/Transforms/Utils/PromoteMemoryToRegister.cpp : TermIndex
struct Mem2RegTermIndexArg {
public:
  typedef std::map<std::string, unsigned> TyTermIndexObj;
  typedef std::shared_ptr<TyTermIndexObj> TyTermIndex;
  TyTermIndex termIndex;

  Mem2RegTermIndexArg();
};
DEFINE_TRAITS(ArgForMem2RegTermIndex, Mem2RegTermIndexArg);

// lib/Transforms/Utils/PromoteMemoryToRegister.cpp : StoreVal
struct Mem2RegStoreValArg {
public:
  typedef std::map<const llvm::Instruction*,
                   std::shared_ptr<TyValue>> TyStoreValObj;
  typedef std::shared_ptr<TyStoreValObj> TyStoreVal;
  TyStoreVal storeVal;

  Mem2RegStoreValArg();
};
DEFINE_TRAITS(ArgForMem2RegStoreVal, Mem2RegStoreValArg);

// lib/Transforms/Utils/PromoteMemoryToRegister.cpp : StoreExpr
struct Mem2RegStoreExprArg {
public:
  typedef std::map<const llvm::Instruction*,
                   std::shared_ptr<TyExpr>> TyStoreExprObj;
  typedef std::shared_ptr<TyStoreExprObj> TyStoreExpr;
  TyStoreExpr storeExpr;

  Mem2RegStoreExprArg();
};
DEFINE_TRAITS(ArgForMem2RegStoreExpr, Mem2RegStoreExprArg);

// lib/Transforms/Utils/PromoteMemoryToRegister.cpp : StoreOp0
struct Mem2RegStoreOp0Arg {
public:
  typedef std::map<const llvm::Instruction*,
                   std::string> TyStoreOp0Obj;
  typedef std::shared_ptr<TyStoreOp0Obj> TyStoreOp0;
  TyStoreOp0 storeOp0;

  Mem2RegStoreOp0Arg();
};
DEFINE_TRAITS(ArgForMem2RegStoreOp0, Mem2RegStoreOp0Arg);

// lib/Transforms/Utils/PromoteMemoryToRegister.cpp : Values
struct Mem2RegValuesArg {
public:
  typedef std::map<std::string,
                   std::shared_ptr<TyExpr>> TyValuesObj;
  typedef std::shared_ptr<TyValuesObj> TyValues;
  TyValues values;

  Mem2RegValuesArg();
};
DEFINE_TRAITS(ArgForMem2RegValues, Mem2RegValuesArg);

// lib/Transforms/InstCombine/InstCombineMulDivRem.cpp : visitMul
struct VisitMulArg {
public:
  bool needsTransitivity;
};
DEFINE_TRAITS(ArgForVisitMul, VisitMulArg);

// lib/Transforms/InstCombine/InstCombineSelect.cpp : FoldSelectOpOp
struct FoldSelectOpOpArg {
public:
  enum OperandCases{
  XY_XZ, YX_ZX, XY_ZX, YX_XZ
  };
  OperandCases the_case;
};
DEFINE_TRAITS(ArgForFoldSelectOpOp, FoldSelectOpOpArg);

// lib/Transforms/Scalar/GVN.cpp : findLeader
struct FindLeaderArg {
public:
  const llvm::BasicBlock *BB;
};
DEFINE_TRAITS(ArgForFindLeader, FindLeaderArg);




class Dictionary {
private:
  std::map<DictKeys, boost::any> data;

public:
  template<DictKeys key>
  void assertExists() {
    assert(data.find(key) != data.end() && "Dictionary does not contain the key.");
  }

  template<DictKeys key>
  std::shared_ptr<typename DictKeysTraits<key>::ty> get() {
    assertExists<key>();
    std::shared_ptr<typename DictKeysTraits<key>::ty> ptr = boost::any_cast<std::shared_ptr<
          typename DictKeysTraits<key>::ty> >(data[key]);
    return ptr;
  }

  template<DictKeys key>
  std::shared_ptr<typename DictKeysTraits<key>::ty> create() {
    auto ptr = std::shared_ptr<typename DictKeysTraits<key>::ty>
        (new typename DictKeysTraits<key>::ty());
    data[key] = ptr;
    return ptr;
  }

  template<DictKeys key>
  void set(std::shared_ptr<typename DictKeysTraits<key>::ty> &t) {
    data[key] = t;
  }

  template<DictKeys key>
  void erase() {
    assertExists<key>();
    data.erase(key);
  }
};

}

#endif
