#ifndef DICTIONARY_H
#define DICTIONARY_H

#include "llvm/IR/Instructions.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Dominators.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/LLVMBerry/Structure.h"
#include "llvm/LLVMBerry/Infrules.h"

#include <memory>
#include <tuple>

namespace llvmberry {

enum DictKeys {
  // Common
  ArgForStripPointerCasts,
  ArgForFindAvailableLoadedValue,
  ArgForIndices,
  // InstSimplify
  ArgForSimplifyAndInst,
  ArgForSimplifyOrInst,
  ArgForSimplifyShiftInst,
  ArgForSimplifyXorInst,
  // InstCombine
  ArgForVisitMul,
  ArgForFoldSelectOpOp,
  ArgForLoadLoadStore,
  ArgForSelectIcmpConst,
  ArgForVisitICmp,
  // Mem2Reg
  ArgForMem2Reg,
  // GVN
  ArgForGVNReplace,
  // LICM
  ArgForHoistOrSinkCond
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

// Used in InstructionSimplify.cpp : SimplifyAndInst(), SimplifyOrInst(),
// SimplifyXorInst(), SimplifyShiftInst()
struct SimplifyInstArg {
public:
  SimplifyInstArg();
  void setHintGenFunc(std::string microoptName,
                      std::function<void(llvm::Instruction *)> hintGenFunc);
  void abort();
  void generateHint(llvm::Instruction *arg) const;
  bool isActivated() const;
  std::string getMicroOptName() const;

private:
  bool activated;
  bool aborted;
  llvm::Instruction *I;
  std::string microoptName;
  std::function<void(llvm::Instruction *)> hintGenFunc;
};
DEFINE_TRAITS(ArgForSimplifyAndInst, SimplifyInstArg);
DEFINE_TRAITS(ArgForSimplifyOrInst, SimplifyInstArg);
DEFINE_TRAITS(ArgForSimplifyXorInst, SimplifyInstArg);
DEFINE_TRAITS(ArgForSimplifyShiftInst, SimplifyInstArg);

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
      std::pair<llvm::Instruction *, std::string>>> TyOrthogonalInsnsObj;
      // Note that there are two kinds of orthogonal instructions : 
      // store, and 'call'.

  typedef llvmberry::StripPointerCastsArg::TyStrippedValuesObj TyPtrEqValuesObj;
  typedef std::shared_ptr<TyOrthogonalInsnsObj> TyOrthogonalInsns;
  typedef std::shared_ptr<TyPtrEqValuesObj> TyPtrEqValues;
  TyOrthogonalInsns orthogonalInsns;
  TyPtrEqValues ptr1EquivalentValues;
  TyPtrEqValues ptr2EquivalentValues;
  bool isLoadStore;
  llvm::StoreInst *loadstoreStoreInst;

  FindAvailableLoadedValueArg();
};
DEFINE_TRAITS(ArgForFindAvailableLoadedValue, FindAvailableLoadedValueArg);

struct IndicesArg {
public:
  typedef std::map<const llvm::Instruction *, unsigned> TyInstrIndicesObj;
  typedef std::shared_ptr<TyInstrIndicesObj> TyInstrIndices;
  TyInstrIndices instrIndices;

  typedef std::map<llvm::StringRef, unsigned> TyTermIndicesObj;
  typedef std::shared_ptr<TyTermIndicesObj> TyTermIndices;
  TyTermIndices termIndices;

  typedef std::tuple<llvm::BasicBlock*,
                     llvm::Instruction*,
                     unsigned> UseTuple;

  typedef std::map<llvm::Instruction*,
                   std::vector<UseTuple>> TyUseIndicesObj;
  typedef std::shared_ptr<TyUseIndicesObj> TyUseIndices;
  TyUseIndices useIndices;

  IndicesArg();
};
DEFINE_TRAITS(ArgForIndices, IndicesArg);

// lib/Transforms/Utils/PromoteMemoryToRegister.cpp : Mem2RegArg
struct Mem2RegArg {
public:
  struct StoreTriple {
    std::shared_ptr<TyValue> value;
    std::shared_ptr<TyExpr> expr;
    std::string op0;
  };

  typedef std::map<const llvm::Instruction *, StoreTriple> TyStoreItemObj;
  typedef std::shared_ptr<TyStoreItemObj> TyStoreItem;
  TyStoreItem storeItem;

  struct RenamePassTuple {
    std::shared_ptr<TyExpr> instrL;
    std::shared_ptr<TyExpr> instrR;
    std::shared_ptr<TyPosition> instrPos;
    std::string op0;
    std::string op1;
    llvm::BasicBlock* instrBB;
    bool check;
  };

  typedef std::map<unsigned, RenamePassTuple> TyRecentInstrObj;
  typedef std::shared_ptr<TyRecentInstrObj> TyRecentInstr;
  TyRecentInstr recentInstr;

  typedef std::vector<TyRecentInstrObj> TyInstrWorkListObj;
  typedef std::shared_ptr<TyInstrWorkListObj> TyInstrWorkList;
  TyInstrWorkList instrWorkList;

  typedef std::vector<std::shared_ptr<TyExpr>> TyReplaceObj;
  typedef std::shared_ptr<TyReplaceObj> TyReplace;
  TyReplace replaceItem;

  typedef std::vector<std::shared_ptr<TyExpr>> TyReplaceTObj;
  typedef std::shared_ptr<TyReplaceTObj> TyReplaceT;
  TyReplaceT replaceTag;
  
  Mem2RegArg();
};
DEFINE_TRAITS(ArgForMem2Reg, Mem2RegArg);

// lib/Transforms/InstCombine/InstCombineMulDivRem.cpp : visitMul
struct VisitMulArg {
public:
  bool needsComm;
};
DEFINE_TRAITS(ArgForVisitMul, VisitMulArg);

// lib/Transforms/InstCombine/InstCombineSelect.cpp : FoldSelectOpOp
struct FoldSelectOpOpArg {
public:
  enum OperandCases { XY_XZ, YX_ZX, XY_ZX, YX_XZ };
  OperandCases the_case;
};
DEFINE_TRAITS(ArgForFoldSelectOpOp, FoldSelectOpOpArg);

// lib/Transforms/Instcombine/InstCombineCompares.cpp
struct VisitICmpArg {
public:
  VisitICmpArg();
  bool swapOps;
};
DEFINE_TRAITS(ArgForVisitICmp, VisitICmpArg);

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

// lib/Transform/InstCombine/InstCombineSelect.cpp : visitSelectInstWithICmp
struct SelectIcmpConstArg {
public:
  SelectIcmpConstArg();
  bool activated;
  bool isGtToLt;
  bool isUnsigned;
  bool selectCommutative;
  llvm::SelectInst *Z;
  llvm::ICmpInst *Y;
  llvm::Value *X;
  llvm::ConstantInt *C, *Cprime;
  std::shared_ptr<TyPosition> Y_org_pos;
};

DEFINE_TRAITS(ArgForSelectIcmpConst, SelectIcmpConstArg);

// lib/Transforms/Scalar/GVN.cpp : processInstruction, findLeader
struct GVNReplaceArg {
public:
  GVNReplaceArg();
  bool isGVNReplace;
  boost::any GVNptr;
  boost::any VNptr;
};
DEFINE_TRAITS(ArgForGVNReplace, GVNReplaceArg);

// lib/Transforms/Scalar/LICM.cpp : to record whether sinking of hoisting
// an instruction can be validated
struct LICMHoistOrSinkCond {
public:
  LICMHoistOrSinkCond();
  bool useAA;
  const llvm::Loop *CurLoop;
};
DEFINE_TRAITS(ArgForHoistOrSinkCond, LICMHoistOrSinkCond);

class Dictionary {
private:
  std::map<DictKeys, boost::any> data;

public:
  template <DictKeys key> bool exists() {
    return data.find(key) != data.end();
  }

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

// PassDictionary: a dictionary shared throughout a pass

class PassDictionary : public Dictionary {
private:
  static PassDictionary *_Instance;

public:
  static void Create();
  static PassDictionary &GetInstance();
  static void Destroy();
};
}

#endif
