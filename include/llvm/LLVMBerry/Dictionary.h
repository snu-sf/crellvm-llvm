#ifndef DICTIONARY_H
#define DICTIONARY_H

#include "llvm/IR/Instructions.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"

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
  ArgForRewriteSingleStoreAlloca, 
  // GVN
  ArgForFindLeader
};

#define DEFINE_TRAITS(KEYVAL, CLASSNAME) \
    template<> struct DictKeysTraits<KEYVAL> { \
      typedef CLASSNAME ty; \
    }
template<DictKeys key>
struct DictKeysTraits {
  typedef int ty; // To be precise, ty here must be 'unspecified' type
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

// lib/Transforms/Utils/PromoteMemoryToRegister.cpp : rewriteSingleStoreAlloca
struct RewriteSingleStoreAllocaArg {
public:
  typedef std::vector<llvm::AllocaInst *> TyAllocasObj;
  typedef std::shared_ptr<TyAllocasObj> TyAllocas;
  TyAllocas allocas;

  RewriteSingleStoreAllocaArg();
};
DEFINE_TRAITS(ArgForRewriteSingleStoreAlloca, RewriteSingleStoreAllocaArg);

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
