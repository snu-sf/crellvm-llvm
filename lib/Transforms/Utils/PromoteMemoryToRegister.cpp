//===- PromoteMemoryToRegister.cpp - Convert allocas to registers ---------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file promotes memory references to be register references.  It promotes
// alloca instructions which only have loads and stores as uses.  An alloca is
// transformed by using iterated dominator frontiers to place PHI nodes, then
// traversing the function in depth-first order to rewrite loads and stores as
// appropriate.
//
//===----------------------------------------------------------------------===//

#include "llvm/Transforms/Utils/PromoteMemToReg.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/AliasSetTracker.h"
#include "llvm/Analysis/InstructionSimplify.h"
#include "llvm/Analysis/IteratedDominanceFrontier.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Module.h"
#include "llvm/Transforms/Utils/Local.h"
#include <algorithm>
#include "llvm/LLVMBerry/ValidationUnit.h"
#include "llvm/LLVMBerry/Infrules.h"
#include "llvm/LLVMBerry/Hintgen.h"
#include "llvm/LLVMBerry/Dictionary.h"
using namespace llvm;

#define DEBUG_TYPE "mem2reg"

STATISTIC(NumLocalPromoted, "Number of alloca's promoted within one block");
STATISTIC(NumSingleStore,   "Number of alloca's promoted with a single store");
STATISTIC(NumDeadAlloca,    "Number of dead alloca's removed");
STATISTIC(NumPHIInsert,     "Number of PHI nodes inserted");

bool llvm::isAllocaPromotable(const AllocaInst *AI) {
  // FIXME: If the memory unit is of pointer or integer type, we can permit
  // assignments to subsections of the memory unit.
  unsigned AS = AI->getType()->getAddressSpace();

  // Only allow direct and non-volatile loads and stores...
  for (const User *U : AI->users()) {
    if (const LoadInst *LI = dyn_cast<LoadInst>(U)) {
      // Note that atomic loads can be transformed; atomic semantics do
      // not have any meaning for a local alloca.
      if (LI->isVolatile())
        return false;
    } else if (const StoreInst *SI = dyn_cast<StoreInst>(U)) {
      if (SI->getOperand(0) == AI)
        return false; // Don't allow a store OF the AI, only INTO the AI.
      // Note that atomic stores can be transformed; atomic semantics do
      // not have any meaning for a local alloca.
      if (SI->isVolatile())
        return false;
    } else if (const IntrinsicInst *II = dyn_cast<IntrinsicInst>(U)) {
      if (II->getIntrinsicID() != Intrinsic::lifetime_start &&
          II->getIntrinsicID() != Intrinsic::lifetime_end)
        return false;
    } else if (const BitCastInst *BCI = dyn_cast<BitCastInst>(U)) {
      if (BCI->getType() != Type::getInt8PtrTy(U->getContext(), AS))
        return false;
      if (!onlyUsedByLifetimeMarkers(BCI))
        return false;
    } else if (const GetElementPtrInst *GEPI = dyn_cast<GetElementPtrInst>(U)) {
      if (GEPI->getType() != Type::getInt8PtrTy(U->getContext(), AS))
        return false;
      if (!GEPI->hasAllZeroIndices())
        return false;
      if (!onlyUsedByLifetimeMarkers(GEPI))
        return false;
    } else {
      return false;
    }
  }

  return true;
}

namespace {

struct AllocaInfo {
  SmallVector<BasicBlock *, 32> DefiningBlocks;
  SmallVector<BasicBlock *, 32> UsingBlocks;

  StoreInst *OnlyStore;
  BasicBlock *OnlyBlock;
  bool OnlyUsedInOneBlock;

  Value *AllocaPointerVal;
  DbgDeclareInst *DbgDeclare;

  void clear() {
    DefiningBlocks.clear();
    UsingBlocks.clear();
    OnlyStore = nullptr;
    OnlyBlock = nullptr;
    OnlyUsedInOneBlock = true;
    AllocaPointerVal = nullptr;
    DbgDeclare = nullptr;
  }

  /// Scan the uses of the specified alloca, filling in the AllocaInfo used
  /// by the rest of the pass to reason about the uses of this alloca.
  void AnalyzeAlloca(AllocaInst *AI) {
    clear();

    // As we scan the uses of the alloca instruction, keep track of stores,
    // and decide whether all of the loads and stores to the alloca are within
    // the same basic block.
    for (auto UI = AI->user_begin(), E = AI->user_end(); UI != E;) {
      Instruction *User = cast<Instruction>(*UI++);

      if (StoreInst *SI = dyn_cast<StoreInst>(User)) {
        // Remember the basic blocks which define new values for the alloca
        DefiningBlocks.push_back(SI->getParent());
        AllocaPointerVal = SI->getOperand(0);
        OnlyStore = SI;
      } else {
        LoadInst *LI = cast<LoadInst>(User);
        // Otherwise it must be a load instruction, keep track of variable
        // reads.
        UsingBlocks.push_back(LI->getParent());
        AllocaPointerVal = LI;
      }

      if (OnlyUsedInOneBlock) {
        if (!OnlyBlock)
          OnlyBlock = User->getParent();
        else if (OnlyBlock != User->getParent())
          OnlyUsedInOneBlock = false;
      }
    }

    DbgDeclare = FindAllocaDbgDeclare(AI);
  }
};

// Data package used by RenamePass()
class RenamePassData {
public:
  typedef std::vector<Value *> ValVector;

  RenamePassData() : BB(nullptr), Pred(nullptr), Values() {}
  RenamePassData(BasicBlock *B, BasicBlock *P, const ValVector &V)
      : BB(B), Pred(P), Values(V) {}
  BasicBlock *BB;
  BasicBlock *Pred;
  ValVector Values;

  void swap(RenamePassData &RHS) {
    std::swap(BB, RHS.BB);
    std::swap(Pred, RHS.Pred);
    Values.swap(RHS.Values);
  }
};

/// \brief This assigns and keeps a per-bb relative ordering of load/store
/// instructions in the block that directly load or store an alloca.
///
/// This functionality is important because it avoids scanning large basic
/// blocks multiple times when promoting many allocas in the same block.
class LargeBlockInfo {
  /// \brief For each instruction that we track, keep the index of the
  /// instruction.
  ///
  /// The index starts out as the number of the instruction from the start of
  /// the block.
  DenseMap<const Instruction *, unsigned> InstNumbers;

public:

  /// This code only looks at accesses to allocas.
  static bool isInterestingInstruction(const Instruction *I) {
    return (isa<LoadInst>(I) && isa<AllocaInst>(I->getOperand(0))) ||
           (isa<StoreInst>(I) && isa<AllocaInst>(I->getOperand(1)));
  }

  /// Get or calculate the index of the specified instruction.
  unsigned getInstructionIndex(const Instruction *I) {
    assert(isInterestingInstruction(I) &&
           "Not a load/store to/from an alloca?");

    // If we already have this instruction number, return it.
    DenseMap<const Instruction *, unsigned>::iterator It = InstNumbers.find(I);
    if (It != InstNumbers.end())
      return It->second;

    // Scan the whole block to get the instruction.  This accumulates
    // information for every interesting instruction in the block, in order to
    // avoid gratuitus rescans.
    const BasicBlock *BB = I->getParent();
    unsigned InstNo = 0;
    for (BasicBlock::const_iterator BBI = BB->begin(), E = BB->end(); BBI != E;
         ++BBI)
      if (isInterestingInstruction(BBI))
        InstNumbers[BBI] = InstNo++;
    It = InstNumbers.find(I);

    assert(It != InstNumbers.end() && "Didn't insert instruction?");
    return It->second;
  }

  void deleteValue(const Instruction *I) { InstNumbers.erase(I); }

  void clear() { InstNumbers.clear(); }
};

struct PromoteMem2Reg {
  /// The alloca instructions being promoted.
  std::vector<AllocaInst *> Allocas;
  DominatorTree &DT;
  DIBuilder DIB;

  /// An AliasSetTracker object to update.  If null, don't update it.
  AliasSetTracker *AST;

  /// A cache of @llvm.assume intrinsics used by SimplifyInstruction.
  AssumptionCache *AC;

  /// Reverse mapping of Allocas.
  DenseMap<AllocaInst *, unsigned> AllocaLookup;

  /// \brief The PhiNodes we're adding.
  ///
  /// That map is used to simplify some Phi nodes as we iterate over it, so
  /// it should have deterministic iterators.  We could use a MapVector, but
  /// since we already maintain a map from BasicBlock* to a stable numbering
  /// (BBNumbers), the DenseMap is more efficient (also supports removal).
  DenseMap<std::pair<unsigned, unsigned>, PHINode *> NewPhiNodes;

  /// For each PHI node, keep track of which entry in Allocas it corresponds
  /// to.
  DenseMap<PHINode *, unsigned> PhiToAllocaMap;

  /// If we are updating an AliasSetTracker, then for each alloca that is of
  /// pointer type, we keep track of what to copyValue to the inserted PHI
  /// nodes here.
  std::vector<Value *> PointerAllocaValues;

  /// For each alloca, we keep track of the dbg.declare intrinsic that
  /// describes it, if any, so that we can convert it to a dbg.value
  /// intrinsic if the alloca gets promoted.
  SmallVector<DbgDeclareInst *, 8> AllocaDbgDeclares;

  /// The set of basic blocks the renamer has already visited.
  ///
  SmallPtrSet<BasicBlock *, 16> Visited;

  /// Contains a stable numbering of basic blocks to avoid non-determinstic
  /// behavior.
  DenseMap<BasicBlock *, unsigned> BBNumbers;

  /// Lazily compute the number of predecessors a block has.
  DenseMap<const BasicBlock *, unsigned> BBNumPreds;

public:
  PromoteMem2Reg(ArrayRef<AllocaInst *> Allocas, DominatorTree &DT,
                 AliasSetTracker *AST, AssumptionCache *AC)
      : Allocas(Allocas.begin(), Allocas.end()), DT(DT),
        DIB(*DT.getRoot()->getParent()->getParent(), /*AllowUnresolved*/ false),
        AST(AST), AC(AC) {}

  void run();

private:
  void RemoveFromAllocasList(unsigned &AllocaIdx) {
    Allocas[AllocaIdx] = Allocas.back();
    Allocas.pop_back();
    --AllocaIdx;
  }

  unsigned getNumPreds(const BasicBlock *BB) {
    unsigned &NP = BBNumPreds[BB];
    if (NP == 0)
      NP = std::distance(pred_begin(BB), pred_end(BB)) + 1;
    return NP - 1;
  }

  void ComputeLiveInBlocks(AllocaInst *AI, AllocaInfo &Info,
                           const SmallPtrSetImpl<BasicBlock *> &DefBlocks,
                           SmallPtrSetImpl<BasicBlock *> &LiveInBlocks);
  void RenamePass(BasicBlock *BB, BasicBlock *Pred,
                  RenamePassData::ValVector &IncVals,
                  std::vector<RenamePassData> &Worklist);
  bool QueuePhiNode(BasicBlock *BB, unsigned AllocaIdx, unsigned &Version);
};

} // end of anonymous namespace

static void removeLifetimeIntrinsicUsers(AllocaInst *AI) {
  // Knowing that this alloca is promotable, we know that it's safe to kill all
  // instructions except for load and store.

  for (auto UI = AI->user_begin(), UE = AI->user_end(); UI != UE;) {
    Instruction *I = cast<Instruction>(*UI);
    ++UI;
    if (isa<LoadInst>(I) || isa<StoreInst>(I))
      continue;

    if (!I->getType()->isVoidTy()) {
      // The only users of this bitcast/GEP instruction are lifetime intrinsics.
      // Follow the use/def chain to erase them now instead of leaving it for
      // dead code elimination later.
      for (auto UUI = I->user_begin(), UUE = I->user_end(); UUI != UUE;) {
        Instruction *Inst = cast<Instruction>(*UUI);
        ++UUI;
        Inst->eraseFromParent();
      }
    }
    I->eraseFromParent();
  }
}

/// \brief Rewrite as many loads as possible given a single store.
///
/// When there is only a single store, we can use the domtree to trivially
/// replace all of the dominated loads with the stored value. Do so, and return
/// true if this has successfully promoted the alloca entirely. If this returns
/// false there were some loads which were not dominated by the single store
/// and thus must be phi-ed with undef. We fall back to the standard alloca
/// promotion algorithm in that case.
static bool rewriteSingleStoreAlloca(AllocaInst *AI, AllocaInfo &Info,
                                     LargeBlockInfo &LBI,
                                     DominatorTree &DT,
                                     AliasSetTracker *AST) {
  StoreInst *OnlyStore = Info.OnlyStore;
  bool StoringGlobalVal = !isa<Instruction>(OnlyStore->getOperand(0));
  BasicBlock *StoreBB = OnlyStore->getParent();
  int StoreIndex = -1;

  // Clear out UsingBlocks.  We will reconstruct it here if needed.
  Info.UsingBlocks.clear();
  for (auto UI = AI->user_begin(), E = AI->user_end(); UI != E;) {
    Instruction *UserInst = cast<Instruction>(*UI++);
    if (!isa<LoadInst>(UserInst)) {
      assert(UserInst == OnlyStore && "Should only have load/stores");
      continue;
    }
    LoadInst *LI = cast<LoadInst>(UserInst);

    // Okay, if we have a load from the alloca, we want to replace it with the
    // only value stored to the alloca.  We can do this if the value is
    // dominated by the store.  If not, we use the rest of the mem2reg machinery
    // to insert the phi nodes as needed.
    if (!StoringGlobalVal) { // Non-instructions are always dominated.
      if (LI->getParent() == StoreBB) {
        // If we have a use that is in the same block as the store, compare the
        // indices of the two instructions to see which one came first.  If the
        // load came before the store, we can't handle it.
        if (StoreIndex == -1)
          StoreIndex = LBI.getInstructionIndex(OnlyStore);

        if (unsigned(StoreIndex) > LBI.getInstructionIndex(LI)) {
          // Can't handle this load, bail out.
          Info.UsingBlocks.push_back(StoreBB);
          continue;
        }

      } else if (LI->getParent() != StoreBB &&
                 !DT.dominates(StoreBB, LI->getParent())) {
        // If the load and store are in different blocks, use BB dominance to
        // check their relationships.  If the store doesn't dom the use, bail
        // out.
        Info.UsingBlocks.push_back(LI->getParent());
        continue;
      }
    }

    // Otherwise, we *can* safely rewrite this load.
    Value *ReplVal = OnlyStore->getOperand(0);

    llvmberry::ValidationUnit::GetInstance()->intrude
            ([&AI, &OnlyStore, &LI, &DT]
              (llvmberry::Dictionary &data, llvmberry::CoreHint &hints) {
      //        <src>          |     <tgt>
      // %x = alloca i32       | nop
      // store i32 1, %x       | nop
      // %a = load i32 %x      | nop
      // %b = load i32 %x      | nop
      // %c = add i32 %a, %b   | %c = add i32 1, 1
      // ret i32 %c            | ret i32 %c

      // prepare variables
      auto &allocas = *(data.get<llvmberry::ArgForMem2RegAlloca>()->allocas);
      auto &instrIndex = *(data.get<llvmberry::ArgForMem2RegInstrIndex>()->instrIndex);
      auto &termIndex = *(data.get<llvmberry::ArgForMem2RegTermIndex>()->termIndex);
      auto &storeVal = *(data.get<llvmberry::ArgForMem2RegStoreVal>()->storeVal);
      auto &storeExpr = *(data.get<llvmberry::ArgForMem2RegStoreExpr>()->storeExpr);
      auto &storeOp0 = *(data.get<llvmberry::ArgForMem2RegStoreOp0>()->storeOp0);
      auto &values = *(data.get<llvmberry::ArgForMem2RegValues>()->values);
      std::string Ralloca = llvmberry::getVariable(*AI);
      std::string Rstore = llvmberry::getVariable(*(OnlyStore->getOperand(1)));
      std::string Rload = llvmberry::getVariable(*LI);
      std::string keySI = std::to_string(instrIndex[OnlyStore])+Rstore;

      // find end user of LI
      llvm::Value::use_iterator UI = LI->use_begin(), E = LI->use_end();
      llvm::Instruction *end;

      // end is LI if there is no user of LI
      end = LI;

      // repeat until find end user of LI
      for (; UI != E; UI++) {
        llvm::Use &U = *UI;
        llvm::Instruction *endsub = dyn_cast<Instruction>(U.getUser());

        if (instrIndex[end] < instrIndex[endsub])
          end = dyn_cast<Instruction>(U.getUser());
      }

      // set index of end user
      int endIndex = -1;
      if (isa<TerminatorInst>(*end))
        // end user is end of block
        endIndex = termIndex[llvmberry::getBasicBlockIndex(end->getParent())];
      else if (end == LI)
        // no user of LI
        endIndex = instrIndex[end] + 1;
      else
        endIndex = instrIndex[end];

      // propagate noalias
      for (auto i = allocas.begin(); i != allocas.end(); ++i) {
        AllocaInst *AItmp = *i;

        if (AI==AItmp) continue;

        std::string Rtmp = llvmberry::getVariable(*AItmp);

        if (instrIndex[AI]<instrIndex[AItmp]) {
          PROPAGATE(NOALIAS(POINTER(AI),
                            POINTER(AItmp),
                            SRC),
                    BOUNDS(llvmberry::TyPosition::make
                            (SRC, *AItmp, instrIndex[AItmp], ""),
                           llvmberry::TyPosition::make
                            (SRC, *end, endIndex, "")));

          INFRULE(llvmberry::TyPosition::make
                   (SRC, *AItmp, instrIndex[AItmp], ""),
                  llvmberry::ConsDiffblockNoalias::make
                   (VAL(AI, Physical),
                    VAL(AItmp, Physical),
                    POINTER(AI),
                    POINTER(AItmp)));
        } else {
          PROPAGATE(NOALIAS(POINTER(AItmp),
                            POINTER(AI),
                            SRC),
                    BOUNDS(llvmberry::TyPosition::make
                            (SRC, *AI, instrIndex[AI], ""),
                           llvmberry::TyPosition::make
                            (SRC, *end, endIndex, "")));

          INFRULE(llvmberry::TyPosition::make
                   (SRC, *AI, instrIndex[AI], ""),
                  llvmberry::ConsDiffblockNoalias::make
                   (VAL(AItmp, Physical),
                    VAL(AI, Physical),
                    POINTER(AItmp),
                    POINTER(AI)));
        }
      }

      // - store and loads are in the same block
      //   and store exists before loads
      // - store and loads are not in the same block
      //   and store block domindates load block
      if ((LI->getParent() == OnlyStore->getParent() && 
           instrIndex[OnlyStore] < instrIndex[LI]) ||
          (LI->getParent() != OnlyStore->getParent() &&
           DT.dominates(OnlyStore->getParent(), LI->getParent()))) {
        // Step1: propagate store instruction
        //        <src>          |     <tgt>
        // %x = alloca i32       | nop
        // store i32 1, %x       | nop
        // %a = load i32 %x      | %a = load i32 1
        // %b = load i32 %x      | %b = load i32 1
        // %c = add i32 %a, %b   | %c = add i32 %a, %b
        // ret i32 %c            | ret i32 %c
        PROPAGATE(LESSDEF(INSN(*OnlyStore),
                          VAR(Rstore, Ghost),
                          SRC),
                  BOUNDS(llvmberry::TyPosition::make
                          (SRC, *OnlyStore, instrIndex[OnlyStore], ""),
                         llvmberry::TyPosition::make
                          (SRC, *LI, instrIndex[LI], "")));

        PROPAGATE(LESSDEF(VAR(Rstore, Ghost),
                          values[keySI],
                          TGT),
                  BOUNDS(llvmberry::TyPosition::make
                          (SRC, *OnlyStore, instrIndex[OnlyStore], ""),
                         llvmberry::TyPosition::make
                          (SRC, *LI, instrIndex[LI], "")));

        if (storeOp0[OnlyStore] == "%") {
          // stored value is constant (hasName() is NULL)
          INFRULE(llvmberry::TyPosition::make
                   (SRC, *OnlyStore, instrIndex[OnlyStore], ""),
                  llvmberry::ConsIntroGhost::make
                   (storeVal[OnlyStore],
                    REGISTER(Rstore, Ghost)));

          INFRULE(llvmberry::TyPosition::make
                   (SRC, *OnlyStore, instrIndex[OnlyStore], ""),
                  llvmberry::ConsTransitivity::make
                   (INSN(*OnlyStore),
                    storeExpr[OnlyStore],
                    VAR(Rstore, Ghost)));
        } else {
          // stored value is another register
          INFRULE(llvmberry::TyPosition::make
                   (SRC, *OnlyStore, instrIndex[OnlyStore], ""),
                  llvmberry::ConsIntroGhost::make
                   (ID(storeOp0[OnlyStore], Ghost),
                    REGISTER(Rstore, Ghost)));

          INFRULE(llvmberry::TyPosition::make
                   (SRC, *OnlyStore, instrIndex[OnlyStore], ""),
                  llvmberry::ConsTransitivity::make
                   (INSN(*OnlyStore),
                    storeExpr[OnlyStore],
                    VAR(storeOp0[OnlyStore], Ghost)));
                    
          INFRULE(llvmberry::TyPosition::make
                   (SRC, *OnlyStore, instrIndex[OnlyStore], ""),
                  llvmberry::ConsTransitivity::make
                   (INSN(*OnlyStore),
                    VAR(storeOp0[OnlyStore], Ghost),
                    VAR(Rstore, Ghost)));
                    
          INFRULE(llvmberry::TyPosition::make
                   (SRC, *OnlyStore, instrIndex[OnlyStore], ""),
                  llvmberry::ConsTransitivityTgt::make
                   (VAR(Rstore, Ghost),
                    VAR(storeOp0[OnlyStore], Ghost),
                    values[keySI]));
        }

        // Step2: propagate load instruction
        //        <src>          |     <tgt>
        // %a = load i32 1       | nop
        // %b = load i32 1       | nop
        // %c = add i32 %a, %b   | %c = add i32 1, 1
        // ret i32 %c            | ret i32 %c
        PROPAGATE(LESSDEF(VAR(Rload, Physical),
                          VAR(Rload, Ghost),
                          SRC),
                  BOUNDS(llvmberry::TyPosition::make
                          (SRC, *LI, instrIndex[LI], ""),
                         llvmberry::TyPosition::make
                          (SRC, *end, endIndex, "")));

        PROPAGATE(LESSDEF(VAR(Rload, Ghost),
                          values[keySI],
                          TGT),
                  BOUNDS(llvmberry::TyPosition::make
                          (SRC, *LI, instrIndex[LI], ""),
                         llvmberry::TyPosition::make
                          (SRC, *end, endIndex, "")));
        INFRULE(llvmberry::TyPosition::make
                 (SRC, *LI, instrIndex[LI], ""),
                llvmberry::ConsIntroGhost::make
                 (ID(Rstore, Ghost),
                  REGISTER(Rload, Ghost)));

        INFRULE(llvmberry::TyPosition::make
                 (SRC, *LI, instrIndex[LI], ""),
                llvmberry::ConsTransitivity::make
                 (INSN(*OnlyStore),
                  VAR(Rstore, Ghost),
                  VAR(Rload, Ghost)));
                  
        INFRULE(llvmberry::TyPosition::make
                 (SRC, *LI, instrIndex[LI], ""),
                llvmberry::ConsTransitivity::make
                 (VAR(Rload, Physical),
                  INSN(*OnlyStore),
                  VAR(Rload, Ghost)));
                  
        INFRULE(llvmberry::TyPosition::make
                 (SRC, *LI, instrIndex[LI], ""),
                llvmberry::ConsTransitivityTgt::make
                 (VAR(Rload, Ghost),
                  VAR(Rstore, Ghost),
                  values[keySI]));
      }

      // propagate maydiff
      llvmberry::propagateMaydiffGlobal(Rload, llvmberry::Physical);
      llvmberry::propagateMaydiffGlobal(Rload, llvmberry::Previous);

      // add nop
      hints.addNopPosition
        (llvmberry::TyPosition::make
          (llvmberry::Target, *LI, instrIndex[LI]-1, ""));
    });

    // If the replacement value is the load, this must occur in unreachable
    // code.
    if (ReplVal == LI)
      ReplVal = UndefValue::get(LI->getType());
    LI->replaceAllUsesWith(ReplVal);

    if (AST && LI->getType()->isPointerTy())
      AST->deleteValue(LI);
    LI->eraseFromParent();
    LBI.deleteValue(LI);
  }

  // Finally, after the scan, check to see if the store is all that is left.
  if (!Info.UsingBlocks.empty())
    return false; // If not, we'll have to fall back for the remainder.

  // Record debuginfo for the store and remove the declaration's
  // debuginfo.
  if (DbgDeclareInst *DDI = Info.DbgDeclare) {
    DIBuilder DIB(*AI->getParent()->getParent()->getParent(),
                  /*AllowUnresolved*/ false);
    ConvertDebugDeclareToDebugValue(DDI, Info.OnlyStore, DIB);
    DDI->eraseFromParent();
    LBI.deleteValue(DDI);
  }

  // add alloca and store into maydiff
  llvmberry::ValidationUnit::GetInstance()->intrude
         ([&AI, &OnlyStore]
            (llvmberry::Dictionary &data, 
             llvmberry::CoreHint &hints) {
    auto &allocas = *(data.get<llvmberry::ArgForMem2RegAlloca>()->allocas);
    auto &instrIndex = *(data.get<llvmberry::ArgForMem2RegInstrIndex>()->instrIndex);
    std::string Ralloca = llvmberry::getVariable(*AI);

    // propagate maydiff
    llvmberry::propagateMaydiffGlobal(Ralloca, llvmberry::Physical);
    llvmberry::propagateMaydiffGlobal(Ralloca, llvmberry::Previous);

    // add nop
    hints.addNopPosition
      (llvmberry::TyPosition::make
        (llvmberry::Target, *AI, instrIndex[AI]-1, ""));

    hints.addNopPosition
      (llvmberry::TyPosition::make
        (llvmberry::Target, *OnlyStore, instrIndex[OnlyStore]-1, ""));

    // remove dead alloca from dictionary
    std::vector<AllocaInst*>::iterator position = std::find(allocas.begin(), allocas.end(), AI);
    if (position != allocas.end()) 
      allocas.erase(position);
  });

  // Remove the (now dead) store and alloca.
  Info.OnlyStore->eraseFromParent();
  LBI.deleteValue(Info.OnlyStore);

  if (AST)
    AST->deleteValue(AI);
  AI->eraseFromParent();
  LBI.deleteValue(AI);
  return true;
}

/// Many allocas are only used within a single basic block.  If this is the
/// case, avoid traversing the CFG and inserting a lot of potentially useless
/// PHI nodes by just performing a single linear pass over the basic block
/// using the Alloca.
///
/// If we cannot promote this alloca (because it is read before it is written),
/// return true.  This is necessary in cases where, due to control flow, the
/// alloca is potentially undefined on some control flow paths.  e.g. code like
/// this is potentially correct:
///
///   for (...) { if (c) { A = undef; undef = B; } }
///
/// ... so long as A is not used before undef is set.
static void promoteSingleBlockAlloca(AllocaInst *AI, const AllocaInfo &Info,
                                     LargeBlockInfo &LBI,
                                     AliasSetTracker *AST) {
  // The trickiest case to handle is when we have large blocks. Because of this,
  // this code is optimized assuming that large blocks happen.  This does not
  // significantly pessimize the small block case.  This uses LargeBlockInfo to
  // make it efficient to get the index of various operations in the block.

  // Walk the use-def list of the alloca, getting the locations of all stores.
  typedef SmallVector<std::pair<unsigned, StoreInst *>, 64> StoresByIndexTy;
  StoresByIndexTy StoresByIndex;

  for (User *U : AI->users())
    if (StoreInst *SI = dyn_cast<StoreInst>(U)) {
      StoresByIndex.push_back(std::make_pair(LBI.getInstructionIndex(SI), SI));

      llvmberry::ValidationUnit::GetInstance()->intrude
         ([&AI, &SI]
           (llvmberry::Dictionary &data, llvmberry::CoreHint &hints) {
        //        <src>           |     <tgt>
        // %x = alloca i32        | nop
        // store i32 1, %x        | nop
        // %a = load i32 %x       | nop
        // call void @bar(i32 %a) | call void @bar(i32 1)
        // store i32 2, %x        | nop
        // %b = load i32 %x       | nop
        // call void @bar(i32 %b) | call void @bar(i32 2)
        // store i32 3, %x        | nop
        // %c = load i32 %x       | nop
        // ret i32 %c             | ret i32 3

        // prepare variables
        auto &allocas = *(data.get<llvmberry::ArgForMem2RegAlloca>()->allocas);
        auto &instrIndex = *(data.get<llvmberry::ArgForMem2RegInstrIndex>()->instrIndex);
        auto &termIndex = *(data.get<llvmberry::ArgForMem2RegTermIndex>()->termIndex);
        auto &storeVal = *(data.get<llvmberry::ArgForMem2RegStoreVal>()->storeVal);
        auto &storeExpr = *(data.get<llvmberry::ArgForMem2RegStoreExpr>()->storeExpr);
        auto &storeOp0 = *(data.get<llvmberry::ArgForMem2RegStoreOp0>()->storeOp0);
        auto &values = *(data.get<llvmberry::ArgForMem2RegValues>()->values);
        std::string Ralloca = llvmberry::getVariable(*AI);
        std::string Rstore = llvmberry::getVariable(*(SI->getOperand(1)));
        std::string keySI = std::to_string(instrIndex[SI])+Rstore;

        // find next store, and end user of alloca
        llvm::Instruction* next;
        llvm::Instruction* end = AI; 

        int checkNext = 1;
        for (auto UI = AI->user_begin(), E = AI->user_end(); UI != E;) {
          Instruction *tmpInst = cast<Instruction>(*UI++);
          if (isa<StoreInst>(tmpInst) && (instrIndex[tmpInst]>instrIndex[SI] || 
              (instrIndex[tmpInst]>instrIndex[SI] && instrIndex[next]>instrIndex[tmpInst]))) {
            checkNext = 0;
            next = tmpInst;
          } else if (instrIndex[end] < instrIndex[tmpInst]) 
            end = tmpInst;
        }
        if (checkNext == 1) next = end;

        // set index of next store
        int nextIndex;
        if (isa<TerminatorInst>(*next))
          nextIndex = termIndex[llvmberry::getBasicBlockIndex(next->getParent())];
        else {
          nextIndex = instrIndex[next];
          if (isa<StoreInst>(end))
            // no user of SI
            nextIndex = -1;
        }

        // set index of end user
        int endIndex;
        if (isa<TerminatorInst>(*end))
          endIndex = termIndex[llvmberry::getBasicBlockIndex(end->getParent())];
        else
          endIndex = instrIndex[end];

        // propagate noalias
        for (auto i = allocas.begin(); i != allocas.end(); ++i) {
          AllocaInst *AItmp = *i;

          if (AI==AItmp) continue;

          if (instrIndex[AI]<instrIndex[AItmp]) {
            PROPAGATE(NOALIAS(POINTER(AI),
                              POINTER(AItmp),
                              SRC),
                      BOUNDS(llvmberry::TyPosition::make
                              (SRC, *AItmp, instrIndex[AItmp], ""),
                             llvmberry::TyPosition::make
                              (SRC, *end, endIndex, "")));

            INFRULE(llvmberry::TyPosition::make
                     (SRC, *AItmp, instrIndex[AItmp], ""),
                    llvmberry::ConsDiffblockNoalias::make
                     (VAL(AI, Physical),
                      VAL(AItmp, Physical),
                      POINTER(AI),
                      POINTER(AItmp)));
          } else {
            PROPAGATE(NOALIAS(POINTER(AItmp),
                              POINTER(AI),
                              SRC),
                      BOUNDS(llvmberry::TyPosition::make
                              (SRC, *AI, instrIndex[AI], ""),
                             llvmberry::TyPosition::make
                              (SRC, *end, endIndex, "")));

            INFRULE(llvmberry::TyPosition::make
                     (SRC, *AI, instrIndex[AI], ""),
                    llvmberry::ConsDiffblockNoalias::make
                     (VAL(AItmp, Physical),
                      VAL(AI, Physical),
                      POINTER(AItmp),
                      POINTER(AI)));
          }
        }

        // Step1: propagate store instruction
        //        <src>           |     <tgt>
        // %x = alloca i32        | nop
        // store i32 1, %x        | nop
        // %a = load i32 %x       | %a = load i32 1
        // call void @bar(i32 %a) | call void @bar(i32 %a)
        // store i32 2, %x        | nop
        // %b = load i32 %x       | %b = load i32 2
        // call void @bar(i32 %b) | call void @bar(i32 %b)
        // store i32 3, %x        | nop
        // %c = load i32 %x       | %c = load i32 3
        // ret i32 %c             | ret i32 %c
        if (nextIndex != -1) {
          // stored value is register
          if (storeOp0[SI] != "%") {
            if (storeExpr[SI] == values[keySI]) {
              // stored value will not be changed in another iteration
              PROPAGATE(LESSDEF(INSN(*SI),
                                VAR(Rstore, Ghost),
                                SRC),
                        BOUNDS(llvmberry::TyPosition::make
                                (SRC, *SI, instrIndex[SI], ""),
                               llvmberry::TyPosition::make
                                (SRC, *next, nextIndex, "")));

              PROPAGATE(LESSDEF(VAR(Rstore, Ghost),
                                storeExpr[SI],
                                TGT),
                        BOUNDS(llvmberry::TyPosition::make
                                (SRC, *SI, instrIndex[SI], ""),
                               llvmberry::TyPosition::make
                                (SRC, *next, nextIndex, "")));

              INFRULE(llvmberry::TyPosition::make
                       (SRC, *SI, instrIndex[SI], ""),
                      llvmberry::ConsIntroGhost::make
                       (storeVal[SI],
                        REGISTER(Rstore, Ghost)));

              INFRULE(llvmberry::TyPosition::make
                       (SRC, *SI, instrIndex[SI], ""),
                      llvmberry::ConsTransitivity::make
                       (INSN(*SI),
                        storeExpr[SI],
                        VAR(Rstore, Ghost)));
            } else {
              // stored value will be changed in another iteration
              PROPAGATE(LESSDEF(INSN(*SI),
                                VAR(Rstore, Ghost),
                                SRC),
                        BOUNDS(llvmberry::TyPosition::make
                                (SRC, *SI, instrIndex[SI], ""),
                               llvmberry::TyPosition::make
                                (SRC, *next, nextIndex, "")));

              PROPAGATE(LESSDEF(VAR(Rstore, Ghost),
                                values[keySI],
                                TGT),
                        BOUNDS(llvmberry::TyPosition::make
                                (SRC, *SI, instrIndex[SI], ""),
                               llvmberry::TyPosition::make
                                (SRC, *next, nextIndex, "")));

              INFRULE(llvmberry::TyPosition::make
                       (SRC, *SI, instrIndex[SI], ""),
                      llvmberry::ConsIntroGhost::make
                       (ID(storeOp0[SI], Ghost),
                        REGISTER(Rstore, Ghost)));

              INFRULE(llvmberry::TyPosition::make
                       (SRC, *SI, instrIndex[SI], ""),
                      llvmberry::ConsTransitivity::make
                       (INSN(*SI),
                        VAR(storeOp0[SI], Physical),
                        VAR(storeOp0[SI], Ghost)));
                        
              INFRULE(llvmberry::TyPosition::make
                       (SRC, *SI, instrIndex[SI], ""),
                      llvmberry::ConsTransitivity::make
                       (INSN(*SI),
                        VAR(storeOp0[SI], Ghost),
                        VAR(Rstore, Ghost)));
                        
              INFRULE(llvmberry::TyPosition::make
                       (SRC, *SI, instrIndex[SI], ""),
                      llvmberry::ConsTransitivityTgt::make
                       (VAR(Rstore, Ghost),
                        VAR(storeOp0[SI], Ghost),
                        values[keySI]));
            }
          } else {
            // stored value is constant
            PROPAGATE(LESSDEF(INSN(*SI),
                              VAR(Rstore, Ghost),
                              SRC),
                      BOUNDS(llvmberry::TyPosition::make
                              (SRC, *SI, instrIndex[SI], ""),
                             llvmberry::TyPosition::make
                              (SRC, *next, nextIndex, "")));

            PROPAGATE(LESSDEF(VAR(Rstore, Ghost),
                              values[keySI],
                              TGT),
                      BOUNDS(llvmberry::TyPosition::make
                              (SRC, *SI, instrIndex[SI], ""),
                             llvmberry::TyPosition::make
                              (SRC, *next, nextIndex, "")));

            INFRULE(llvmberry::TyPosition::make
                     (SRC, *SI, instrIndex[SI], ""),
                    llvmberry::ConsIntroGhost::make
                     (storeVal[SI],
                      REGISTER(Rstore, Ghost)));

            INFRULE(llvmberry::TyPosition::make
                     (SRC, *SI, instrIndex[SI], ""),
                    llvmberry::ConsTransitivity::make
                     (INSN(*SI),
                      values[keySI],
                      VAR(Rstore, Ghost)));
          }
        }
      });     
    }

  // Sort the stores by their index, making it efficient to do a lookup with a
  // binary search.
  std::sort(StoresByIndex.begin(), StoresByIndex.end(), less_first());

  // Walk all of the loads from this alloca, replacing them with the nearest
  // store above them, if any.
  for (auto UI = AI->user_begin(), E = AI->user_end(); UI != E;) {
    LoadInst *LI = dyn_cast<LoadInst>(*UI++);
    if (!LI)
      continue;

    unsigned LoadIdx = LBI.getInstructionIndex(LI);

    // Find the nearest store that has a lower index than this load.
    StoresByIndexTy::iterator I =
        std::lower_bound(StoresByIndex.begin(), StoresByIndex.end(),
                         std::make_pair(LoadIdx,
                                        static_cast<StoreInst *>(nullptr)),
                         less_first());

    if (I == StoresByIndex.begin())
      // If there is no store before this load, the load takes the undef value.
      LI->replaceAllUsesWith(UndefValue::get(LI->getType()));
    else {
      llvmberry::ValidationUnit::GetInstance()->intrude
         ([&AI, &LI, &I]
           (llvmberry::Dictionary &data, llvmberry::CoreHint &hints) {
        // prepare variables
        StoreInst* SI = std::prev(I)->second;
        auto &instrIndex = *(data.get<llvmberry::ArgForMem2RegInstrIndex>()->instrIndex);
        auto &termIndex = *(data.get<llvmberry::ArgForMem2RegTermIndex>()->termIndex);
        auto &storeExpr = *(data.get<llvmberry::ArgForMem2RegStoreExpr>()->storeExpr);
        auto &storeOp0 = *(data.get<llvmberry::ArgForMem2RegStoreOp0>()->storeOp0);
        auto &values = *(data.get<llvmberry::ArgForMem2RegValues>()->values);
        std::string Rstore = llvmberry::getVariable(*(SI->getOperand(1)));
        std::string Rload = llvmberry::getVariable(*LI);

        // find end user of load
        llvm::Value::use_iterator UI = LI->use_begin(), E = LI->use_end();
        llvm::Instruction *end;

        // end is LI if there is no user of LI
        end = LI;

        // repeat until find end user of LI
        for (; UI != E; UI++) {
          llvm::Use &U = *UI;
          llvm::Instruction *endsub = dyn_cast<Instruction>(U.getUser());

          if (instrIndex[end] < instrIndex[endsub])
            end = dyn_cast<Instruction>(U.getUser());
        }

        // set index of end user
        int endIndex = -1;
        if (isa<TerminatorInst>(*end)) {
          // end user is end of block
          endIndex = termIndex[llvmberry::getBasicBlockIndex(end->getParent())];
        } else if (end == LI) {
          // no user of LI
          endIndex = -1;
        } else {
          endIndex = instrIndex[end];
        }

        // Step2: propagate load instruction
        //        <src>           |     <tgt>
        // %a = load i32 1        | nop
        // call void @bar(i32 %a) | call void @bar(i32 1)
        // %b = load i32 2        | nop
        // call void @bar(i32 %b) | call void @bar(i32 2)
        // %c = load i32 3        | nop
        // ret i32 %c             | ret i32 3

        // add hints when at least one user of load is exists
        if (endIndex != -1) {
          if (storeOp0[SI] == "%") {
            // stored value is constant
            PROPAGATE(LESSDEF(VAR(Rload, Physical),
                              VAR(Rload, Ghost),
                              SRC),
                      BOUNDS(llvmberry::TyPosition::make
                              (SRC, *LI, instrIndex[LI], ""),
                             llvmberry::TyPosition::make
                              (SRC, *end, endIndex, "")));

            PROPAGATE(LESSDEF(VAR(Rload, Ghost),
                              storeExpr[SI],
                              TGT),
                      BOUNDS(llvmberry::TyPosition::make
                              (SRC, *LI, instrIndex[LI], ""),
                             llvmberry::TyPosition::make
                              (SRC, *end, endIndex, "")));

            INFRULE(llvmberry::TyPosition::make
                     (SRC, *LI, instrIndex[LI], ""),
                    llvmberry::ConsIntroGhost::make
                     (ID(Rstore, Ghost),
                      REGISTER(Rload, Ghost)));

            INFRULE(llvmberry::TyPosition::make
                     (SRC, *LI, instrIndex[LI], ""),
                    llvmberry::ConsTransitivity::make
                     (VAR(Rload, Physical),
                      INSN(*SI),
                      VAR(Rstore, Ghost)));
                      
            INFRULE(llvmberry::TyPosition::make
                     (SRC, *LI, instrIndex[LI], ""),
                    llvmberry::ConsTransitivity::make
                     (VAR(Rload, Physical),
                      VAR(Rstore, Ghost),
                      VAR(Rload, Ghost)));
                      
            INFRULE(llvmberry::TyPosition::make
                     (SRC, *LI, instrIndex[LI], ""),
                    llvmberry::ConsTransitivityTgt::make
                     (VAR(Rload, Ghost),
                      VAR(Rstore, Ghost),
                      storeExpr[SI]));
          } else {
            // stored value is register
            PROPAGATE(LESSDEF(VAR(Rload, Physical),
                              VAR(Rload, Ghost),
                              SRC),
                      BOUNDS(llvmberry::TyPosition::make
                              (SRC, *LI, instrIndex[LI], ""),
                             llvmberry::TyPosition::make
                              (SRC, *end, endIndex, "")));

            INFRULE(llvmberry::TyPosition::make
                     (SRC, *LI, instrIndex[LI], ""),
                    llvmberry::ConsIntroGhost::make
                     (ID(Rstore, Ghost),
                      REGISTER(Rload, Ghost)));

            INFRULE(llvmberry::TyPosition::make
                     (SRC, *LI, instrIndex[LI], ""),
                    llvmberry::ConsTransitivity::make
                     (VAR(Rload, Physical),
                      INSN(*SI),
                      VAR(Rstore, Ghost)));
                      
            INFRULE(llvmberry::TyPosition::make
                     (SRC, *LI, instrIndex[LI], ""),
                    llvmberry::ConsTransitivity::make
                     (VAR(Rload, Physical),
                      VAR(Rstore, Ghost),
                      VAR(Rload, Ghost)));

            PROPAGATE(LESSDEF(VAR(Rload, Ghost),
                              values[Rload],
                              TGT),
                      BOUNDS(llvmberry::TyPosition::make
                              (SRC, *LI, instrIndex[LI], ""),
                             llvmberry::TyPosition::make
                              (SRC, *end, endIndex, "")));

            INFRULE(llvmberry::TyPosition::make
                     (SRC, *LI, instrIndex[LI], ""),
                    llvmberry::ConsTransitivityTgt::make
                     (VAR(Rload, Ghost),
                      VAR(Rstore, Ghost),
                      values[Rload]));
          }
        }

        // propagate maydiff
        llvmberry::propagateMaydiffGlobal(Rload, llvmberry::Physical);
        llvmberry::propagateMaydiffGlobal(Rload, llvmberry::Previous);

        hints.addNopPosition
          (llvmberry::TyPosition::make
            (llvmberry::Target, *LI, instrIndex[LI]-1, ""));
      });

      // Otherwise, there was a store before this load, the load takes its value.
      LI->replaceAllUsesWith(std::prev(I)->second->getOperand(0));
    }

    if (AST && LI->getType()->isPointerTy())
      AST->deleteValue(LI);
    LI->eraseFromParent();
    LBI.deleteValue(LI);
  }

  // Remove the (now dead) stores and alloca.
  while (!AI->use_empty()) {
    StoreInst *SI = cast<StoreInst>(AI->user_back());
    // Record debuginfo for the store before removing it.
    if (DbgDeclareInst *DDI = Info.DbgDeclare) {
      DIBuilder DIB(*AI->getParent()->getParent()->getParent(),
                    /*AllowUnresolved*/ false);
      ConvertDebugDeclareToDebugValue(DDI, SI, DIB);
    }

    llvmberry::ValidationUnit::GetInstance()->intrude
           ([&SI]
              (llvmberry::Dictionary &data,
               llvmberry::CoreHint &hints) {
      auto &instrIndex = *(data.get<llvmberry::ArgForMem2RegInstrIndex>()->instrIndex);
      std::string Rstore = llvmberry::getVariable(*SI->getOperand(1));

      hints.addNopPosition
        (llvmberry::TyPosition::make
          (llvmberry::Target, *SI, instrIndex[SI]-1, ""));
    });

    SI->eraseFromParent();
    LBI.deleteValue(SI);
  }

  llvmberry::ValidationUnit::GetInstance()->intrude
         ([&AI]
            (llvmberry::Dictionary &data,
             llvmberry::CoreHint &hints) {
    auto &allocas = *(data.get<llvmberry::ArgForMem2RegAlloca>()->allocas);
    auto &instrIndex = *(data.get<llvmberry::ArgForMem2RegInstrIndex>()->instrIndex);
    std::string Ralloca = llvmberry::getVariable(*AI);

    // propagate maydiff
    llvmberry::propagateMaydiffGlobal(Ralloca, llvmberry::Physical);
    llvmberry::propagateMaydiffGlobal(Ralloca, llvmberry::Previous);

    hints.addNopPosition
      (llvmberry::TyPosition::make
        (llvmberry::Target, *AI, instrIndex[AI]-1, ""));

    std::vector<AllocaInst*>::iterator position = std::find(allocas.begin(), allocas.end(), AI);
    if (position != allocas.end()) {
      allocas.erase(position);
    }
  });

  if (AST)
    AST->deleteValue(AI);
  AI->eraseFromParent();
  LBI.deleteValue(AI);

  // The alloca's debuginfo can be removed as well.
  if (DbgDeclareInst *DDI = Info.DbgDeclare) {
    DDI->eraseFromParent();
    LBI.deleteValue(DDI);
  }

  ++NumLocalPromoted;
}

void PromoteMem2Reg::run() {
  Function &F = *DT.getRoot()->getParent();

  if (AST)
    PointerAllocaValues.resize(Allocas.size());
  AllocaDbgDeclares.resize(Allocas.size());

  AllocaInfo Info;
  LargeBlockInfo LBI;
  IDFCalculator IDF(DT);

  llvmberry::name_instructions(F);
  llvmberry::ValidationUnit::Begin("mem2reg", &F);
  llvmberry::ValidationUnit::GetInstance()->intrude
          ([]
            (llvmberry::Dictionary &data, llvmberry::CoreHint &hints) {
      data.create<llvmberry::ArgForMem2RegAlloca>();
      data.create<llvmberry::ArgForMem2RegInstrIndex>();
      data.create<llvmberry::ArgForMem2RegTermIndex>();
      data.create<llvmberry::ArgForMem2RegStoreVal>();
      data.create<llvmberry::ArgForMem2RegStoreExpr>();
      data.create<llvmberry::ArgForMem2RegStoreOp0>();
      data.create<llvmberry::ArgForMem2RegValues>();
  });

  // memorize indices of alloca, store, load, and terminator instructions
  for (unsigned tmpNum = 0; tmpNum != Allocas.size(); ++tmpNum) {
    AllocaInst *AItmp = Allocas[tmpNum];
    DominatorTree &Domtree = DT;

    llvmberry::ValidationUnit::GetInstance()->intrude
            ([&AItmp, &Domtree]
              (llvmberry::Dictionary &data, llvmberry::CoreHint &hints) {
      // initialize dictionaries
      auto &allocas = *(data.get<llvmberry::ArgForMem2RegAlloca>()->allocas);
      auto &instrIndex = *(data.get<llvmberry::ArgForMem2RegInstrIndex>()->instrIndex);
      auto &termIndex = *(data.get<llvmberry::ArgForMem2RegTermIndex>()->termIndex);
      auto &storeVal = *(data.get<llvmberry::ArgForMem2RegStoreVal>()->storeVal);
      auto &storeExpr = *(data.get<llvmberry::ArgForMem2RegStoreExpr>()->storeExpr);
      auto &storeOp0 = *(data.get<llvmberry::ArgForMem2RegStoreOp0>()->storeOp0);
      auto &values = *(data.get<llvmberry::ArgForMem2RegValues>()->values);
      std::string bname = llvmberry::getBasicBlockIndex(AItmp->getParent());
      
      allocas.push_back(AItmp);
      instrIndex[AItmp] = llvmberry::getCommandIndex(*AItmp);
      termIndex[bname] = llvmberry::getTerminatorIndex(AItmp->getParent()->getTerminator());

      // initialize with information of each alloca's store and alloca
      for (auto UI = AItmp->user_begin(), E = AItmp->user_end(); UI != E;) {
        Instruction *tmpInst = cast<Instruction>(*UI++);

        // ignore if user of alloca is not load or store
        if (!(isa<LoadInst>(tmpInst) || isa<StoreInst>(tmpInst))) 
          continue;

        instrIndex[tmpInst] = llvmberry::getCommandIndex(*tmpInst);

        if (isa<StoreInst>(tmpInst)) {
          StoreInst* SI = dyn_cast<StoreInst>(tmpInst);
          std::string keySI = std::to_string(instrIndex[SI])+"%"+
                              std::string(SI->getOperand(1)->getName());

          storeVal[SI] = std::move(llvmberry::TyValue::make(*(SI->getOperand(0))));
          storeExpr[SI] = std::move(llvmberry::makeExpr_fromStoreInst(SI));
          storeOp0[SI] = "%" + std::string(SI->getOperand(0)->getName());
          values[keySI] = storeExpr[SI];
        }

        llvm::Value::use_iterator UI2 = tmpInst->use_begin(), E2 = tmpInst->use_end();
        Instruction *tmpUseinst;

        // save information of store's user
        for (; UI2 != E2; UI2++) {
          llvm::Use &U = *UI2;

          if ((tmpUseinst = dyn_cast<Instruction>(U.getUser())))
            instrIndex[tmpUseinst] = llvmberry::getCommandIndex(*tmpUseinst);
        }
      }

      // update dictionary named "values" until there is no more change
      bool hasChanged = true;
      while (hasChanged) {
        hasChanged = false;

        // check every saved information
        std::map<const Instruction*, unsigned>::const_iterator it;
        for (it = instrIndex.begin(); it != instrIndex.end(); ++it) {
          const Instruction *tmpInst = it->first;

          if (!(isa<LoadInst>(tmpInst) || isa<StoreInst>(tmpInst)))
            continue;

          // if instruction store/load other register,
          // data in dictionary can be changed 
          // to the stored/loaded value of that register
          if (isa<LoadInst>(tmpInst)) {
            const LoadInst* LI = dyn_cast<LoadInst>(tmpInst);
            std::string keyLI = "%"+std::string(LI->getName());
            unsigned nearestStore = 0;
            const BasicBlock *SB = NULL;

            std::map<const Instruction*, unsigned>::const_iterator it2;
            for (it2=instrIndex.begin(); it2!=instrIndex.end(); ++it2) {
              if (isa<StoreInst>(it2->first) && 
                  LI->getOperand(0) == it2->first->getOperand(1)) {
                const StoreInst* SI = dyn_cast<StoreInst>(it2->first);

                if ((LI->getParent() == SI->getParent()) &&
                    instrIndex[SI] < instrIndex[LI] &&
                    instrIndex[SI] > nearestStore) {
                  // if store and load is in the same block,
                  // find nearest store of load
                  nearestStore = instrIndex[SI];
                  SB = SI->getParent();
                } else if ((LI->getParent() != SI->getParent()) &&
                           Domtree.dominates(SI->getParent(), LI->getParent())) {
                  // if store and load is not in the same block,
                  // store block should dominate load block
                  if ((SB == SI->getParent()) && instrIndex[SI] > nearestStore)
                    nearestStore = instrIndex[SI];
                  else if (Domtree.dominates(SB, SI->getParent())) {
                    nearestStore = instrIndex[SI];
                    SB = SI->getParent();
                  }
                }
              }
            }

            // update dictionary if previous value is different with
            // current value
            std::string keyNew = std::to_string(nearestStore)+"%"+
                                 std::string(LI->getOperand(0)->getName());
            if (values[keyLI] != values[keyNew] &&
                values[keyNew] != NULL) {
              values[keyLI] = values[keyNew];
              hasChanged = true;
            }
          } else {
            // the case of store is same as load
            const StoreInst* SI = dyn_cast<StoreInst>(tmpInst);
            std::string keySI = std::to_string(instrIndex[SI])+"%"+
                                std::string(SI->getOperand(1)->getName());
            std::string keyNew = "%"+std::string(SI->getOperand(0)->getName());

            if (values[keySI] != values[keyNew] &&
                values[keyNew] != NULL) {
              values[keySI] = values[keyNew];
              hasChanged = true;
            }
          }
        }
      }
    }); 
  }

  for (unsigned AllocaNum = 0; AllocaNum != Allocas.size(); ++AllocaNum) {
    AllocaInst *AI = Allocas[AllocaNum];

    assert(isAllocaPromotable(AI) && "Cannot promote non-promotable alloca!");
    assert(AI->getParent()->getParent() == &F &&
           "All allocas should be in the same function, which is same as DF!");

    removeLifetimeIntrinsicUsers(AI);

    // hints to propagate alloca, private for every alloca
    llvmberry::ValidationUnit::GetInstance()->intrude
            ([&AI]
              (llvmberry::Dictionary &data, 
               llvmberry::CoreHint &hints) {
      std::string Ralloca = llvmberry::getVariable(*AI);
      auto &instrIndex = *(data.get<llvmberry::ArgForMem2RegInstrIndex>()->instrIndex);
      auto &termIndex = *(data.get<llvmberry::ArgForMem2RegTermIndex>()->termIndex);

      llvm::Value::use_iterator UI = AI->use_begin(), E = AI->use_end();
      llvm::Instruction *userEnd;

      // find block name of alloca's end user
      userEnd = AI;
      for (; UI != E; UI++) {
        llvm::Use &U = *UI;
        userEnd = dyn_cast<Instruction>(U.getUser());
      }
      std::string endBname = llvmberry::getBasicBlockIndex(userEnd->getParent());

      // propagate alloca, private
      PROPAGATE(ALLOCA(REGISTER(Ralloca, Physical),
                       SRC),
                BOUNDS(llvmberry::TyPosition::make
                        (SRC, *AI, instrIndex[AI], ""),
                       llvmberry::TyPosition::make
                        (SRC, *userEnd, termIndex[endBname], "")));

      PROPAGATE(PRIVATE(REGISTER(Ralloca, Physical),
                        SRC),
                BOUNDS(llvmberry::TyPosition::make
                        (SRC, *AI, instrIndex[AI], ""),
                       llvmberry::TyPosition::make
                        (SRC, *userEnd, termIndex[endBname], "")));
    });

    if (AI->use_empty()) {
      // hints when alloca has no use
      llvmberry::ValidationUnit::GetInstance()->intrude
              ([&AI]
                (llvmberry::Dictionary &data, 
                 llvmberry::CoreHint &hints) {
        auto &allocas = *(data.get<llvmberry::ArgForMem2RegAlloca>()->allocas);
        auto &instrIndex = *(data.get<llvmberry::ArgForMem2RegInstrIndex>()->instrIndex);
        std::string Ralloca = llvmberry::getVariable(*AI);

        // propagate maydiff
        llvmberry::propagateMaydiffGlobal(Ralloca, llvmberry::Physical);

        hints.addNopPosition
          (llvmberry::TyPosition::make
            (llvmberry::Target, *AI, instrIndex[AI]-1, ""));

        std::vector<AllocaInst*>::iterator position = std::find(allocas.begin(), allocas.end(), AI);
        if (position != allocas.end()) {
          allocas.erase(position);
        }
      });

      // If there are no uses of the alloca, just delete it now.
      if (AST)
        AST->deleteValue(AI);
      AI->eraseFromParent();

      // Remove the alloca from the Allocas list, since it has been processed
      RemoveFromAllocasList(AllocaNum);
      ++NumDeadAlloca;
      continue;
    }

    // Calculate the set of read and write-locations for each alloca.  This is
    // analogous to finding the 'uses' and 'definitions' of each variable.
    Info.AnalyzeAlloca(AI);

    // If there is only a single store to this value, replace any loads of
    // it that are directly dominated by the definition with the value stored.
    if (Info.DefiningBlocks.size() == 1) {
      if (rewriteSingleStoreAlloca(AI, Info, LBI, DT, AST)) {
        // The alloca has been processed, move on.
        RemoveFromAllocasList(AllocaNum);
        ++NumSingleStore;
        continue;
      }
    }

    // If the alloca is only read and written in one basic block, just perform a
    // linear sweep over the block to eliminate it.
    if (Info.OnlyUsedInOneBlock) {
      promoteSingleBlockAlloca(AI, Info, LBI, AST);

      // The alloca has been processed, move on.
      RemoveFromAllocasList(AllocaNum);
      continue;
    }

    // If we haven't computed a numbering for the BB's in the function, do so
    // now.
    if (BBNumbers.empty()) {
      unsigned ID = 0;
      for (auto &BB : F)
        BBNumbers[&BB] = ID++;
    }

    // If we have an AST to keep updated, remember some pointer value that is
    // stored into the alloca.
    if (AST)
      PointerAllocaValues[AllocaNum] = Info.AllocaPointerVal;

    // Remember the dbg.declare intrinsic describing this alloca, if any.
    if (Info.DbgDeclare)
      AllocaDbgDeclares[AllocaNum] = Info.DbgDeclare;

    // Keep the reverse mapping of the 'Allocas' array for the rename pass.
    AllocaLookup[Allocas[AllocaNum]] = AllocaNum;

    // At this point, we're committed to promoting the alloca using IDF's, and
    // the standard SSA construction algorithm.  Determine which blocks need PHI
    // nodes and see if we can optimize out some work by avoiding insertion of
    // dead phi nodes.


    // Unique the set of defining blocks for efficient lookup.
    SmallPtrSet<BasicBlock *, 32> DefBlocks;
    DefBlocks.insert(Info.DefiningBlocks.begin(), Info.DefiningBlocks.end());

    // Determine which blocks the value is live in.  These are blocks which lead
    // to uses.
    SmallPtrSet<BasicBlock *, 32> LiveInBlocks;
    ComputeLiveInBlocks(AI, Info, DefBlocks, LiveInBlocks);

    // At this point, we're committed to promoting the alloca using IDF's, and
    // the standard SSA construction algorithm.  Determine which blocks need phi
    // nodes and see if we can optimize out some work by avoiding insertion of
    // dead phi nodes.
    IDF.setLiveInBlocks(LiveInBlocks);
    IDF.setDefiningBlocks(DefBlocks);
    SmallVector<BasicBlock *, 32> PHIBlocks;
    IDF.calculate(PHIBlocks);
    if (PHIBlocks.size() > 1)
      std::sort(PHIBlocks.begin(), PHIBlocks.end(),
                [this](BasicBlock *A, BasicBlock *B) {
                  return BBNumbers.lookup(A) < BBNumbers.lookup(B);
                });

    unsigned CurrentVersion = 0;
    for (unsigned i = 0, e = PHIBlocks.size(); i != e; ++i)
      QueuePhiNode(PHIBlocks[i], AllocaNum, CurrentVersion);
  }

  if (Allocas.empty()) {
    llvmberry::ValidationUnit::End();

    return; // All of the allocas must have been trivial!
  }

  LBI.clear();

  // Set the incoming values for the basic block to be null values for all of
  // the alloca's.  We do this in case there is a load of a value that has not
  // been stored yet.  In this case, it will get this null value.
  //
  RenamePassData::ValVector Values(Allocas.size());
  for (unsigned i = 0, e = Allocas.size(); i != e; ++i)
    Values[i] = UndefValue::get(Allocas[i]->getAllocatedType());

  // Walks all basic blocks in the function performing the SSA rename algorithm
  // and inserting the phi nodes we marked as necessary
  //
  std::vector<RenamePassData> RenamePassWorkList;
  RenamePassWorkList.emplace_back(F.begin(), nullptr, std::move(Values));
  do {
    RenamePassData RPD;
    RPD.swap(RenamePassWorkList.back());
    RenamePassWorkList.pop_back();
    // RenamePass may add new worklist entries.
    RenamePass(RPD.BB, RPD.Pred, RPD.Values, RenamePassWorkList);
  } while (!RenamePassWorkList.empty());

  // The renamer uses the Visited set to avoid infinite loops.  Clear it now.
  Visited.clear();

  // Remove the allocas themselves from the function.
  for (unsigned i = 0, e = Allocas.size(); i != e; ++i) {
    Instruction *A = Allocas[i];

    // If there are any uses of the alloca instructions left, they must be in
    // unreachable basic blocks that were not processed by walking the dominator
    // tree. Just delete the users now.
    if (!A->use_empty())
      A->replaceAllUsesWith(UndefValue::get(A->getType()));
    if (AST)
      AST->deleteValue(A);
    A->eraseFromParent();
  }

  const DataLayout &DL = F.getParent()->getDataLayout();

  // Remove alloca's dbg.declare instrinsics from the function.
  for (unsigned i = 0, e = AllocaDbgDeclares.size(); i != e; ++i)
    if (DbgDeclareInst *DDI = AllocaDbgDeclares[i])
      DDI->eraseFromParent();

  // Loop over all of the PHI nodes and see if there are any that we can get
  // rid of because they merge all of the same incoming values.  This can
  // happen due to undef values coming into the PHI nodes.  This process is
  // iterative, because eliminating one PHI node can cause others to be removed.
  bool EliminatedAPHI = true;
  while (EliminatedAPHI) {
    EliminatedAPHI = false;

    // Iterating over NewPhiNodes is deterministic, so it is safe to try to
    // simplify and RAUW them as we go.  If it was not, we could add uses to
    // the values we replace with in a non-deterministic order, thus creating
    // non-deterministic def->use chains.
    for (DenseMap<std::pair<unsigned, unsigned>, PHINode *>::iterator
             I = NewPhiNodes.begin(),
             E = NewPhiNodes.end();
         I != E;) {
      PHINode *PN = I->second;

      // If this PHI node merges one value and/or undefs, get the value.
      if (Value *V = SimplifyInstruction(PN, DL, nullptr, &DT, AC)) {
        if (AST && PN->getType()->isPointerTy())
          AST->deleteValue(PN);
        PN->replaceAllUsesWith(V);
        PN->eraseFromParent();
        NewPhiNodes.erase(I++);
        EliminatedAPHI = true;
        continue;
      }
      ++I;
    }
  }

  // At this point, the renamer has added entries to PHI nodes for all reachable
  // code.  Unfortunately, there may be unreachable blocks which the renamer
  // hasn't traversed.  If this is the case, the PHI nodes may not
  // have incoming values for all predecessors.  Loop over all PHI nodes we have
  // created, inserting undef values if they are missing any incoming values.
  //
  for (DenseMap<std::pair<unsigned, unsigned>, PHINode *>::iterator
           I = NewPhiNodes.begin(),
           E = NewPhiNodes.end();
       I != E; ++I) {
    // We want to do this once per basic block.  As such, only process a block
    // when we find the PHI that is the first entry in the block.
    PHINode *SomePHI = I->second;
    BasicBlock *BB = SomePHI->getParent();
    if (&BB->front() != SomePHI)
      continue;

    // Only do work here if there the PHI nodes are missing incoming values.  We
    // know that all PHI nodes that were inserted in a block will have the same
    // number of incoming values, so we can just check any of them.
    if (SomePHI->getNumIncomingValues() == getNumPreds(BB))
      continue;

    // Get the preds for BB.
    SmallVector<BasicBlock *, 16> Preds(pred_begin(BB), pred_end(BB));

    // Ok, now we know that all of the PHI nodes are missing entries for some
    // basic blocks.  Start by sorting the incoming predecessors for efficient
    // access.
    std::sort(Preds.begin(), Preds.end());

    // Now we loop through all BB's which have entries in SomePHI and remove
    // them from the Preds list.
    for (unsigned i = 0, e = SomePHI->getNumIncomingValues(); i != e; ++i) {
      // Do a log(n) search of the Preds list for the entry we want.
      SmallVectorImpl<BasicBlock *>::iterator EntIt = std::lower_bound(
          Preds.begin(), Preds.end(), SomePHI->getIncomingBlock(i));
      assert(EntIt != Preds.end() && *EntIt == SomePHI->getIncomingBlock(i) &&
             "PHI node has entry for a block which is not a predecessor!");

      // Remove the entry
      Preds.erase(EntIt);
    }

    // At this point, the blocks left in the preds list must have dummy
    // entries inserted into every PHI nodes for the block.  Update all the phi
    // nodes in this block that we are inserting (there could be phis before
    // mem2reg runs).
    unsigned NumBadPreds = SomePHI->getNumIncomingValues();
    BasicBlock::iterator BBI = BB->begin();
    while ((SomePHI = dyn_cast<PHINode>(BBI++)) &&
           SomePHI->getNumIncomingValues() == NumBadPreds) {
      Value *UndefVal = UndefValue::get(SomePHI->getType());
      for (unsigned pred = 0, e = Preds.size(); pred != e; ++pred)
        SomePHI->addIncoming(UndefVal, Preds[pred]);
    }
  }

  NewPhiNodes.clear();
  
  llvmberry::ValidationUnit::End();
}

/// \brief Determine which blocks the value is live in.
///
/// These are blocks which lead to uses.  Knowing this allows us to avoid
/// inserting PHI nodes into blocks which don't lead to uses (thus, the
/// inserted phi nodes would be dead).
void PromoteMem2Reg::ComputeLiveInBlocks(
    AllocaInst *AI, AllocaInfo &Info,
    const SmallPtrSetImpl<BasicBlock *> &DefBlocks,
    SmallPtrSetImpl<BasicBlock *> &LiveInBlocks) {

  // To determine liveness, we must iterate through the predecessors of blocks
  // where the def is live.  Blocks are added to the worklist if we need to
  // check their predecessors.  Start with all the using blocks.
  SmallVector<BasicBlock *, 64> LiveInBlockWorklist(Info.UsingBlocks.begin(),
                                                    Info.UsingBlocks.end());

  // If any of the using blocks is also a definition block, check to see if the
  // definition occurs before or after the use.  If it happens before the use,
  // the value isn't really live-in.
  for (unsigned i = 0, e = LiveInBlockWorklist.size(); i != e; ++i) {
    BasicBlock *BB = LiveInBlockWorklist[i];
    if (!DefBlocks.count(BB))
      continue;

    // Okay, this is a block that both uses and defines the value.  If the first
    // reference to the alloca is a def (store), then we know it isn't live-in.
    for (BasicBlock::iterator I = BB->begin();; ++I) {
      if (StoreInst *SI = dyn_cast<StoreInst>(I)) {
        if (SI->getOperand(1) != AI)
          continue;

        // We found a store to the alloca before a load.  The alloca is not
        // actually live-in here.
        LiveInBlockWorklist[i] = LiveInBlockWorklist.back();
        LiveInBlockWorklist.pop_back();
        --i, --e;
        break;
      }

      if (LoadInst *LI = dyn_cast<LoadInst>(I)) {
        if (LI->getOperand(0) != AI)
          continue;

        // Okay, we found a load before a store to the alloca.  It is actually
        // live into this block.
        break;
      }
    }
  }

  // Now that we have a set of blocks where the phi is live-in, recursively add
  // their predecessors until we find the full region the value is live.
  while (!LiveInBlockWorklist.empty()) {
    BasicBlock *BB = LiveInBlockWorklist.pop_back_val();

    // The block really is live in here, insert it into the set.  If already in
    // the set, then it has already been processed.
    if (!LiveInBlocks.insert(BB).second)
      continue;

    // Since the value is live into BB, it is either defined in a predecessor or
    // live into it to.  Add the preds to the worklist unless they are a
    // defining block.
    for (pred_iterator PI = pred_begin(BB), E = pred_end(BB); PI != E; ++PI) {
      BasicBlock *P = *PI;

      // The value is not live into a predecessor if it defines the value.
      if (DefBlocks.count(P))
        continue;

      // Otherwise it is, add to the worklist.
      LiveInBlockWorklist.push_back(P);
    }
  }
}

/// \brief Queue a phi-node to be added to a basic-block for a specific Alloca.
///
/// Returns true if there wasn't already a phi-node for that variable
bool PromoteMem2Reg::QueuePhiNode(BasicBlock *BB, unsigned AllocaNo,
                                  unsigned &Version) {
  // Look up the basic-block in question.
  PHINode *&PN = NewPhiNodes[std::make_pair(BBNumbers[BB], AllocaNo)];

  // If the BB already has a phi node added for the i'th alloca then we're done!
  if (PN)
    return false;

  // Create a PhiNode using the dereferenced type... and add the phi-node to the
  // BasicBlock.
  PN = PHINode::Create(Allocas[AllocaNo]->getAllocatedType(), getNumPreds(BB),
                       Allocas[AllocaNo]->getName() + "." + Twine(Version++),
                       BB->begin());
  ++NumPHIInsert;
  PhiToAllocaMap[PN] = AllocaNo;

  if (AST && PN->getType()->isPointerTy())
    AST->copyValue(PointerAllocaValues[AllocaNo], PN);

  return true;
}

/// \brief Recursively traverse the CFG of the function, renaming loads and
/// stores to the allocas which we are promoting.
///
/// IncomingVals indicates what value each Alloca contains on exit from the
/// predecessor block Pred.
void PromoteMem2Reg::RenamePass(BasicBlock *BB, BasicBlock *Pred,
                                RenamePassData::ValVector &IncomingVals,
                                std::vector<RenamePassData> &Worklist) {
NextIteration:
  // If we are inserting any phi nodes into this BB, they will already be in the
  // block.
  if (PHINode *APN = dyn_cast<PHINode>(BB->begin())) {
    // If we have PHI nodes to update, compute the number of edges from Pred to
    // BB.
    if (PhiToAllocaMap.count(APN)) {
      // We want to be able to distinguish between PHI nodes being inserted by
      // this invocation of mem2reg from those phi nodes that already existed in
      // the IR before mem2reg was run.  We determine that APN is being inserted
      // because it is missing incoming edges.  All other PHI nodes being
      // inserted by this pass of mem2reg will have the same number of incoming
      // operands so far.  Remember this count.
      unsigned NewPHINumOperands = APN->getNumOperands();

      unsigned NumEdges = std::count(succ_begin(Pred), succ_end(Pred), BB);
      assert(NumEdges && "Must be at least one edge from Pred to BB!");

      // Add entries for all the phis.
      BasicBlock::iterator PNI = BB->begin();
      do {
        unsigned AllocaNo = PhiToAllocaMap[APN];

        // Add N incoming values to the PHI node.
        for (unsigned i = 0; i != NumEdges; ++i)
          APN->addIncoming(IncomingVals[AllocaNo], Pred);

        // The currently active variable for this block is now the PHI.
        IncomingVals[AllocaNo] = APN;

        // Get the next phi node.
        ++PNI;
        APN = dyn_cast<PHINode>(PNI);
        if (!APN)
          break;

        // Verify that it is missing entries.  If not, it is not being inserted
        // by this mem2reg invocation so we want to ignore it.
      } while (APN->getNumOperands() == NewPHINumOperands);
    }
  }

  // Don't revisit blocks.
  if (!Visited.insert(BB).second)
    return;

  for (BasicBlock::iterator II = BB->begin(); !isa<TerminatorInst>(II);) {
    Instruction *I = II++; // get the instruction, increment iterator

    if (LoadInst *LI = dyn_cast<LoadInst>(I)) {
      AllocaInst *Src = dyn_cast<AllocaInst>(LI->getPointerOperand());
      if (!Src)
        continue;

      DenseMap<AllocaInst *, unsigned>::iterator AI = AllocaLookup.find(Src);
      if (AI == AllocaLookup.end())
        continue;

      Value *V = IncomingVals[AI->second];

      // Anything using the load now uses the current value.
      LI->replaceAllUsesWith(V);
      if (AST && LI->getType()->isPointerTy())
        AST->deleteValue(LI);
      BB->getInstList().erase(LI);
    } else if (StoreInst *SI = dyn_cast<StoreInst>(I)) {
      // Delete this instruction and mark the name as the current holder of the
      // value
      AllocaInst *Dest = dyn_cast<AllocaInst>(SI->getPointerOperand());
      if (!Dest)
        continue;

      DenseMap<AllocaInst *, unsigned>::iterator ai = AllocaLookup.find(Dest);
      if (ai == AllocaLookup.end())
        continue;

      // what value were we writing?
      IncomingVals[ai->second] = SI->getOperand(0);
      // Record debuginfo for the store before removing it.
      if (DbgDeclareInst *DDI = AllocaDbgDeclares[ai->second])
        ConvertDebugDeclareToDebugValue(DDI, SI, DIB);
      BB->getInstList().erase(SI);
    }
  }

  // 'Recurse' to our successors.
  succ_iterator I = succ_begin(BB), E = succ_end(BB);
  if (I == E)
    return;

  // Keep track of the successors so we don't visit the same successor twice
  SmallPtrSet<BasicBlock *, 8> VisitedSuccs;

  // Handle the first successor without using the worklist.
  VisitedSuccs.insert(*I);
  Pred = BB;
  BB = *I;
  ++I;

  for (; I != E; ++I)
    if (VisitedSuccs.insert(*I).second)
      Worklist.emplace_back(*I, Pred, IncomingVals);

  goto NextIteration;
}

void llvm::PromoteMemToReg(ArrayRef<AllocaInst *> Allocas, DominatorTree &DT,
                           AliasSetTracker *AST, AssumptionCache *AC) {
  // If there is nothing to do, bail out...
  if (Allocas.empty())
    return;

  PromoteMem2Reg(Allocas, DT, AST, AC).run();
}
