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
  ArgForSimplifyXorInst,
  // InstCombine
  ArgForVisitMul,
  ArgForFoldSelectOpOp,
  ArgForLoadLoadStore,
  // Mem2Reg
  ArgForMem2Reg,
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
#define DEFINE_TRAITS(KEYVAL, CLASSNAME)                                       \
  template <> struct DictKeysTraits<KEYVAL> { typedef CLASSNAME ty; }
template <DictKeys key> struct DictKeysTraits {
  // DictKeysTraits class has no member for arbitrary type
};

// Used in InstructionSimplify.cpp : SimplifyAndInst(), SimplifyOrInst()
struct SimplifyInstArg {
public:
  SimplifyInstArg();
  void setHintGenFunc(std::string microoptName,
                      std::function<void(llvm::Instruction *)> hintGenFunc);
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
DEFINE_TRAITS(ArgForSimplifyXorInst, SimplifyInstArg);

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
      std::pair<llvm::StoreInst *, std::string>>> TyOrthogonalStoresObj;
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

// lib/Transforms/Utils/PromoteMemoryToRegister.cpp : Mem2RegArg
struct Mem2RegArg {
public:
  typedef std::vector<llvm::AllocaInst *> TyAllocasObj;
  typedef std::shared_ptr<TyAllocasObj> TyAllocas;
  TyAllocas allocas;

  typedef std::map<const llvm::Instruction *, unsigned> TyInstrIndexObj;
  typedef std::shared_ptr<TyInstrIndexObj> TyInstrIndex;
  TyInstrIndex instrIndex;

  typedef std::map<std::string, unsigned> TyTermIndexObj;
  typedef std::shared_ptr<TyTermIndexObj> TyTermIndex;
  TyTermIndex termIndex;

  typedef std::map<std::string, std::shared_ptr<TyExpr>> TyValuesObj;
  typedef std::shared_ptr<TyValuesObj> TyValues;
  TyValues values;

  struct Triple {
    std::shared_ptr<TyValue> value;
    std::shared_ptr<TyExpr> expr;
    std::string op0;
  };

  typedef std::map<const llvm::Instruction *, Triple> TyStoreItemObj;
  typedef std::shared_ptr<TyStoreItemObj> TyStoreItem;
  TyStoreItem storeItem;

  typedef std::map<std::string, std::vector<std::shared_ptr<TyPropagateLessdef>>> TyMem2RegCmdsObj;
  typedef std::shared_ptr<TyMem2RegCmdsObj> TyMem2RegCmds;
  TyMem2RegCmds mem2regCmds;

  void replaceCmdRhs(std::string key, std::shared_ptr<TyExpr> newExpr);

  Mem2RegArg();
};
DEFINE_TRAITS(ArgForMem2Reg, Mem2RegArg);

// lib/Transforms/InstCombine/InstCombineMulDivRem.cpp : visitMul
struct VisitMulArg {
public:
  bool needsTransitivity;
};
DEFINE_TRAITS(ArgForVisitMul, VisitMulArg);

// lib/Transforms/InstCombine/InstCombineSelect.cpp : FoldSelectOpOp
struct FoldSelectOpOpArg {
public:
  enum OperandCases { XY_XZ, YX_ZX, XY_ZX, YX_XZ };
  OperandCases the_case;
};
DEFINE_TRAITS(ArgForFoldSelectOpOp, FoldSelectOpOpArg);

// lib/Transform/InstCombine/InstCombineLoadStoreAlloca.cpp : visitLoadInst
struct LoadLoadStoreArg {
public:
  llvm::Instruction *v1_inst;
  llvm::Value *v1;
  llvm::Value *ptr1;
  llvm::Value *ptr1src;
  llvm::Value *ptr2;
  llvm::Value *ptr2src;
  std::shared_ptr<TyPosition> v2_org_position;
};
DEFINE_TRAITS(ArgForLoadLoadStore, LoadLoadStoreArg);

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
  template <DictKeys key> void assertExists() {
    assert(data.find(key) != data.end() &&
           "Dictionary does not contain the key.");
  }

  template <DictKeys key>
  std::shared_ptr<typename DictKeysTraits<key>::ty> get() {
    assertExists<key>();
    std::shared_ptr<typename DictKeysTraits<key>::ty> ptr =
        boost::any_cast<std::shared_ptr<typename DictKeysTraits<key>::ty>>(
            data[key]);
    return ptr;
  }

  template <DictKeys key>
  std::shared_ptr<typename DictKeysTraits<key>::ty> create() {
    auto ptr = std::shared_ptr<typename DictKeysTraits<key>::ty>(
        new typename DictKeysTraits<key>::ty());
    data[key] = ptr;
    return ptr;
  }

  template <DictKeys key>
  void set(std::shared_ptr<typename DictKeysTraits<key>::ty> &t) {
    data[key] = t;
  }

  template <DictKeys key> void erase() {
    assertExists<key>();
    data.erase(key);
  }
};
}

#endif
