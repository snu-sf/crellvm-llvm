//===- InstCombineLoadStoreAlloca.cpp -------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the visit functions for load, store and alloca.
//
//===----------------------------------------------------------------------===//

#include "InstCombineInternal.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/Loads.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/Local.h"

#include "llvm/LLVMBerry/ValidationUnit.h"
#include "llvm/LLVMBerry/Structure.h"
#include "llvm/LLVMBerry/Infrules.h"
#include "llvm/LLVMBerry/Hintgen.h"
#include "llvm/LLVMBerry/Dictionary.h"
#include <vector>
#include <memory>

using namespace llvm;

#define DEBUG_TYPE "instcombine"

STATISTIC(NumDeadStore,    "Number of dead stores eliminated");
STATISTIC(NumGlobalCopies, "Number of allocas copied from constant global");

/// pointsToConstantGlobal - Return true if V (possibly indirectly) points to
/// some part of a constant global variable.  This intentionally only accepts
/// constant expressions because we can't rewrite arbitrary instructions.
static bool pointsToConstantGlobal(Value *V) {
  if (GlobalVariable *GV = dyn_cast<GlobalVariable>(V))
    return GV->isConstant();

  if (ConstantExpr *CE = dyn_cast<ConstantExpr>(V)) {
    if (CE->getOpcode() == Instruction::BitCast ||
        CE->getOpcode() == Instruction::AddrSpaceCast ||
        CE->getOpcode() == Instruction::GetElementPtr)
      return pointsToConstantGlobal(CE->getOperand(0));
  }
  return false;
}

/// isOnlyCopiedFromConstantGlobal - Recursively walk the uses of a (derived)
/// pointer to an alloca.  Ignore any reads of the pointer, return false if we
/// see any stores or other unknown uses.  If we see pointer arithmetic, keep
/// track of whether it moves the pointer (with IsOffset) but otherwise traverse
/// the uses.  If we see a memcpy/memmove that targets an unoffseted pointer to
/// the alloca, and if the source pointer is a pointer to a constant global, we
/// can optimize this.
static bool
isOnlyCopiedFromConstantGlobal(Value *V, MemTransferInst *&TheCopy,
                               SmallVectorImpl<Instruction *> &ToDelete) {
  // We track lifetime intrinsics as we encounter them.  If we decide to go
  // ahead and replace the value with the global, this lets the caller quickly
  // eliminate the markers.

  SmallVector<std::pair<Value *, bool>, 35> ValuesToInspect;
  ValuesToInspect.push_back(std::make_pair(V, false));
  while (!ValuesToInspect.empty()) {
    auto ValuePair = ValuesToInspect.pop_back_val();
    const bool IsOffset = ValuePair.second;
    for (auto &U : ValuePair.first->uses()) {
      Instruction *I = cast<Instruction>(U.getUser());

      if (LoadInst *LI = dyn_cast<LoadInst>(I)) {
        // Ignore non-volatile loads, they are always ok.
        if (!LI->isSimple()) return false;
        continue;
      }

      if (isa<BitCastInst>(I) || isa<AddrSpaceCastInst>(I)) {
        // If uses of the bitcast are ok, we are ok.
        ValuesToInspect.push_back(std::make_pair(I, IsOffset));
        continue;
      }
      if (GetElementPtrInst *GEP = dyn_cast<GetElementPtrInst>(I)) {
        // If the GEP has all zero indices, it doesn't offset the pointer. If it
        // doesn't, it does.
        ValuesToInspect.push_back(
            std::make_pair(I, IsOffset || !GEP->hasAllZeroIndices()));
        continue;
      }

      if (auto CS = CallSite(I)) {
        // If this is the function being called then we treat it like a load and
        // ignore it.
        if (CS.isCallee(&U))
          continue;

        // Inalloca arguments are clobbered by the call.
        unsigned ArgNo = CS.getArgumentNo(&U);
        if (CS.isInAllocaArgument(ArgNo))
          return false;

        // If this is a readonly/readnone call site, then we know it is just a
        // load (but one that potentially returns the value itself), so we can
        // ignore it if we know that the value isn't captured.
        if (CS.onlyReadsMemory() &&
            (CS.getInstruction()->use_empty() || CS.doesNotCapture(ArgNo)))
          continue;

        // If this is being passed as a byval argument, the caller is making a
        // copy, so it is only a read of the alloca.
        if (CS.isByValArgument(ArgNo))
          continue;
      }

      // Lifetime intrinsics can be handled by the caller.
      if (IntrinsicInst *II = dyn_cast<IntrinsicInst>(I)) {
        if (II->getIntrinsicID() == Intrinsic::lifetime_start ||
            II->getIntrinsicID() == Intrinsic::lifetime_end) {
          assert(II->use_empty() && "Lifetime markers have no result to use!");
          ToDelete.push_back(II);
          continue;
        }
      }

      // If this is isn't our memcpy/memmove, reject it as something we can't
      // handle.
      MemTransferInst *MI = dyn_cast<MemTransferInst>(I);
      if (!MI)
        return false;

      // If the transfer is using the alloca as a source of the transfer, then
      // ignore it since it is a load (unless the transfer is volatile).
      if (U.getOperandNo() == 1) {
        if (MI->isVolatile()) return false;
        continue;
      }

      // If we already have seen a copy, reject the second one.
      if (TheCopy) return false;

      // If the pointer has been offset from the start of the alloca, we can't
      // safely handle this.
      if (IsOffset) return false;

      // If the memintrinsic isn't using the alloca as the dest, reject it.
      if (U.getOperandNo() != 0) return false;

      // If the source of the memcpy/move is not a constant global, reject it.
      if (!pointsToConstantGlobal(MI->getSource()))
        return false;

      // Otherwise, the transform is safe.  Remember the copy instruction.
      TheCopy = MI;
    }
  }
  return true;
}

/// isOnlyCopiedFromConstantGlobal - Return true if the specified alloca is only
/// modified by a copy from a constant global.  If we can prove this, we can
/// replace any uses of the alloca with uses of the global directly.
static MemTransferInst *
isOnlyCopiedFromConstantGlobal(AllocaInst *AI,
                               SmallVectorImpl<Instruction *> &ToDelete) {
  MemTransferInst *TheCopy = nullptr;
  if (isOnlyCopiedFromConstantGlobal(AI, TheCopy, ToDelete))
    return TheCopy;
  return nullptr;
}

static Instruction *simplifyAllocaArraySize(InstCombiner &IC, AllocaInst &AI) {
  // Check for array size of 1 (scalar allocation).
  if (!AI.isArrayAllocation()) {
    // i32 1 is the canonical array size for scalar allocations.
    if (AI.getArraySize()->getType()->isIntegerTy(32))
      return nullptr;

    // Canonicalize it.
    Value *V = IC.Builder->getInt32(1);
    AI.setOperand(0, V);
    return &AI;
  }

  // Convert: alloca Ty, C - where C is a constant != 1 into: alloca [C x Ty], 1
  if (const ConstantInt *C = dyn_cast<ConstantInt>(AI.getArraySize())) {
    Type *NewTy = ArrayType::get(AI.getAllocatedType(), C->getZExtValue());
    AllocaInst *New = IC.Builder->CreateAlloca(NewTy, nullptr, AI.getName());
    New->setAlignment(AI.getAlignment());

    // Scan to the end of the allocation instructions, to skip over a block of
    // allocas if possible...also skip interleaved debug info
    //
    BasicBlock::iterator It = New;
    while (isa<AllocaInst>(*It) || isa<DbgInfoIntrinsic>(*It))
      ++It;

    // Now that I is pointing to the first non-allocation-inst in the block,
    // insert our getelementptr instruction...
    //
    Type *IdxTy = IC.getDataLayout().getIntPtrType(AI.getType());
    Value *NullIdx = Constant::getNullValue(IdxTy);
    Value *Idx[2] = {NullIdx, NullIdx};
    Instruction *GEP =
        GetElementPtrInst::CreateInBounds(New, Idx, New->getName() + ".sub");
    IC.InsertNewInstBefore(GEP, *It);

    // Now make everything use the getelementptr instead of the original
    // allocation.
    return IC.ReplaceInstUsesWith(AI, GEP);
  }

  if (isa<UndefValue>(AI.getArraySize()))
    return IC.ReplaceInstUsesWith(AI, Constant::getNullValue(AI.getType()));

  // Ensure that the alloca array size argument has type intptr_t, so that
  // any casting is exposed early.
  Type *IntPtrTy = IC.getDataLayout().getIntPtrType(AI.getType());
  if (AI.getArraySize()->getType() != IntPtrTy) {
    Value *V = IC.Builder->CreateIntCast(AI.getArraySize(), IntPtrTy, false);
    AI.setOperand(0, V);
    return &AI;
  }

  return nullptr;
}

Instruction *InstCombiner::visitAllocaInst(AllocaInst &AI) {
  if (auto *I = simplifyAllocaArraySize(*this, AI))
    return I;

  if (AI.getAllocatedType()->isSized()) {
    // If the alignment is 0 (unspecified), assign it the preferred alignment.
    if (AI.getAlignment() == 0)
      AI.setAlignment(DL.getPrefTypeAlignment(AI.getAllocatedType()));

    // Move all alloca's of zero byte objects to the entry block and merge them
    // together.  Note that we only do this for alloca's, because malloc should
    // allocate and return a unique pointer, even for a zero byte allocation.
    if (DL.getTypeAllocSize(AI.getAllocatedType()) == 0) {
      // For a zero sized alloca there is no point in doing an array allocation.
      // This is helpful if the array size is a complicated expression not used
      // elsewhere.
      if (AI.isArrayAllocation()) {
        AI.setOperand(0, ConstantInt::get(AI.getArraySize()->getType(), 1));
        return &AI;
      }

      // Get the first instruction in the entry block.
      BasicBlock &EntryBlock = AI.getParent()->getParent()->getEntryBlock();
      Instruction *FirstInst = EntryBlock.getFirstNonPHIOrDbg();
      if (FirstInst != &AI) {
        // If the entry block doesn't start with a zero-size alloca then move
        // this one to the start of the entry block.  There is no problem with
        // dominance as the array size was forced to a constant earlier already.
        AllocaInst *EntryAI = dyn_cast<AllocaInst>(FirstInst);
        if (!EntryAI || !EntryAI->getAllocatedType()->isSized() ||
            DL.getTypeAllocSize(EntryAI->getAllocatedType()) != 0) {
          AI.moveBefore(FirstInst);
          return &AI;
        }

        // If the alignment of the entry block alloca is 0 (unspecified),
        // assign it the preferred alignment.
        if (EntryAI->getAlignment() == 0)
          EntryAI->setAlignment(
              DL.getPrefTypeAlignment(EntryAI->getAllocatedType()));
        // Replace this zero-sized alloca with the one at the start of the entry
        // block after ensuring that the address will be aligned enough for both
        // types.
        unsigned MaxAlign = std::max(EntryAI->getAlignment(),
                                     AI.getAlignment());
        EntryAI->setAlignment(MaxAlign);
        if (AI.getType() != EntryAI->getType())
          return new BitCastInst(EntryAI, AI.getType());
        return ReplaceInstUsesWith(AI, EntryAI);
      }
    }
  }

  if (AI.getAlignment()) {
    // Check to see if this allocation is only modified by a memcpy/memmove from
    // a constant global whose alignment is equal to or exceeds that of the
    // allocation.  If this is the case, we can change all users to use
    // the constant global instead.  This is commonly produced by the CFE by
    // constructs like "void foo() { int A[] = {1,2,3,4,5,6,7,8,9...}; }" if 'A'
    // is only subsequently read.
    SmallVector<Instruction *, 4> ToDelete;
    if (MemTransferInst *Copy = isOnlyCopiedFromConstantGlobal(&AI, ToDelete)) {
      unsigned SourceAlign = getOrEnforceKnownAlignment(
          Copy->getSource(), AI.getAlignment(), DL, &AI, AC, DT);
      if (AI.getAlignment() <= SourceAlign) {
        DEBUG(dbgs() << "Found alloca equal to global: " << AI << '\n');
        DEBUG(dbgs() << "  memcpy = " << *Copy << '\n');
        for (unsigned i = 0, e = ToDelete.size(); i != e; ++i)
          EraseInstFromFunction(*ToDelete[i]);
        Constant *TheSrc = cast<Constant>(Copy->getSource());
        Constant *Cast
          = ConstantExpr::getPointerBitCastOrAddrSpaceCast(TheSrc, AI.getType());
        Instruction *NewI = ReplaceInstUsesWith(AI, Cast);
        EraseInstFromFunction(*Copy);
        ++NumGlobalCopies;
        return NewI;
      }
    }
  }

  // At last, use the generic allocation site handler to aggressively remove
  // unused allocas.
  return visitAllocSite(AI);
}

/// \brief Helper to combine a load to a new type.
///
/// This just does the work of combining a load to a new type. It handles
/// metadata, etc., and returns the new instruction. The \c NewTy should be the
/// loaded *value* type. This will convert it to a pointer, cast the operand to
/// that pointer type, load it, etc.
///
/// Note that this will create all of the instructions with whatever insert
/// point the \c InstCombiner currently is using.
static LoadInst *combineLoadToNewType(InstCombiner &IC, LoadInst &LI, Type *NewTy,
                                      const Twine &Suffix = "") {
  Value *Ptr = LI.getPointerOperand();
  unsigned AS = LI.getPointerAddressSpace();
  SmallVector<std::pair<unsigned, MDNode *>, 8> MD;
  LI.getAllMetadata(MD);

  LoadInst *NewLoad = IC.Builder->CreateAlignedLoad(
      IC.Builder->CreateBitCast(Ptr, NewTy->getPointerTo(AS)),
      LI.getAlignment(), LI.getName() + Suffix);
  MDBuilder MDB(NewLoad->getContext());
  for (const auto &MDPair : MD) {
    unsigned ID = MDPair.first;
    MDNode *N = MDPair.second;
    // Note, essentially every kind of metadata should be preserved here! This
    // routine is supposed to clone a load instruction changing *only its type*.
    // The only metadata it makes sense to drop is metadata which is invalidated
    // when the pointer type changes. This should essentially never be the case
    // in LLVM, but we explicitly switch over only known metadata to be
    // conservatively correct. If you are adding metadata to LLVM which pertains
    // to loads, you almost certainly want to add it here.
    switch (ID) {
    case LLVMContext::MD_dbg:
    case LLVMContext::MD_tbaa:
    case LLVMContext::MD_prof:
    case LLVMContext::MD_fpmath:
    case LLVMContext::MD_tbaa_struct:
    case LLVMContext::MD_invariant_load:
    case LLVMContext::MD_alias_scope:
    case LLVMContext::MD_noalias:
    case LLVMContext::MD_nontemporal:
    case LLVMContext::MD_mem_parallel_loop_access:
      // All of these directly apply.
      NewLoad->setMetadata(ID, N);
      break;

    case LLVMContext::MD_nonnull:
      // This only directly applies if the new type is also a pointer.
      if (NewTy->isPointerTy()) {
        NewLoad->setMetadata(ID, N);
        break;
      }
      // If it's integral now, translate it to !range metadata.
      if (NewTy->isIntegerTy()) {
        auto *ITy = cast<IntegerType>(NewTy);
        auto *NullInt = ConstantExpr::getPtrToInt(
            ConstantPointerNull::get(cast<PointerType>(Ptr->getType())), ITy);
        auto *NonNullInt =
            ConstantExpr::getAdd(NullInt, ConstantInt::get(ITy, 1));
        NewLoad->setMetadata(LLVMContext::MD_range,
                             MDB.createRange(NonNullInt, NullInt));
      }
      break;

    case LLVMContext::MD_range:
      // FIXME: It would be nice to propagate this in some way, but the type
      // conversions make it hard. If the new type is a pointer, we could
      // translate it to !nonnull metadata.
      break;
    }
  }
  return NewLoad;
}

/// \brief Combine a store to a new type.
///
/// Returns the newly created store instruction.
static StoreInst *combineStoreToNewValue(InstCombiner &IC, StoreInst &SI, Value *V) {
  Value *Ptr = SI.getPointerOperand();
  unsigned AS = SI.getPointerAddressSpace();
  SmallVector<std::pair<unsigned, MDNode *>, 8> MD;
  SI.getAllMetadata(MD);

  StoreInst *NewStore = IC.Builder->CreateAlignedStore(
      V, IC.Builder->CreateBitCast(Ptr, V->getType()->getPointerTo(AS)),
      SI.getAlignment());
  for (const auto &MDPair : MD) {
    unsigned ID = MDPair.first;
    MDNode *N = MDPair.second;
    // Note, essentially every kind of metadata should be preserved here! This
    // routine is supposed to clone a store instruction changing *only its
    // type*. The only metadata it makes sense to drop is metadata which is
    // invalidated when the pointer type changes. This should essentially
    // never be the case in LLVM, but we explicitly switch over only known
    // metadata to be conservatively correct. If you are adding metadata to
    // LLVM which pertains to stores, you almost certainly want to add it
    // here.
    switch (ID) {
    case LLVMContext::MD_dbg:
    case LLVMContext::MD_tbaa:
    case LLVMContext::MD_prof:
    case LLVMContext::MD_fpmath:
    case LLVMContext::MD_tbaa_struct:
    case LLVMContext::MD_alias_scope:
    case LLVMContext::MD_noalias:
    case LLVMContext::MD_nontemporal:
    case LLVMContext::MD_mem_parallel_loop_access:
      // All of these directly apply.
      NewStore->setMetadata(ID, N);
      break;

    case LLVMContext::MD_invariant_load:
    case LLVMContext::MD_nonnull:
    case LLVMContext::MD_range:
      // These don't apply for stores.
      break;
    }
  }

  return NewStore;
}

/// \brief Combine loads to match the type of value their uses after looking
/// through intervening bitcasts.
///
/// The core idea here is that if the result of a load is used in an operation,
/// we should load the type most conducive to that operation. For example, when
/// loading an integer and converting that immediately to a pointer, we should
/// instead directly load a pointer.
///
/// However, this routine must never change the width of a load or the number of
/// loads as that would introduce a semantic change. This combine is expected to
/// be a semantic no-op which just allows loads to more closely model the types
/// of their consuming operations.
///
/// Currently, we also refuse to change the precise type used for an atomic load
/// or a volatile load. This is debatable, and might be reasonable to change
/// later. However, it is risky in case some backend or other part of LLVM is
/// relying on the exact type loaded to select appropriate atomic operations.
static Instruction *combineLoadToOperationType(InstCombiner &IC, LoadInst &LI) {
  // FIXME: We could probably with some care handle both volatile and atomic
  // loads here but it isn't clear that this is important.
  if (!LI.isSimple())
    return nullptr;

  if (LI.use_empty())
    return nullptr;

  Type *Ty = LI.getType();
  const DataLayout &DL = IC.getDataLayout();

  // Try to canonicalize loads which are only ever stored to operate over
  // integers instead of any other type. We only do this when the loaded type
  // is sized and has a size exactly the same as its store size and the store
  // size is a legal integer type.
  if (!Ty->isIntegerTy() && Ty->isSized() &&
      DL.isLegalInteger(DL.getTypeStoreSizeInBits(Ty)) &&
      DL.getTypeStoreSizeInBits(Ty) == DL.getTypeSizeInBits(Ty)) {
    if (std::all_of(LI.user_begin(), LI.user_end(), [&LI](User *U) {
          auto *SI = dyn_cast<StoreInst>(U);
          return SI && SI->getPointerOperand() != &LI;
        })) {
      LoadInst *NewLoad = combineLoadToNewType(
          IC, LI,
          Type::getIntNTy(LI.getContext(), DL.getTypeStoreSizeInBits(Ty)));
      // Replace all the stores with stores of the newly loaded value.
      for (auto UI = LI.user_begin(), UE = LI.user_end(); UI != UE;) {
        auto *SI = cast<StoreInst>(*UI++);
        IC.Builder->SetInsertPoint(SI);
        combineStoreToNewValue(IC, *SI, NewLoad);
        IC.EraseInstFromFunction(*SI);
      }
      assert(LI.use_empty() && "Failed to remove all users of the load!");
      // Return the old load so the combiner can delete it safely.
      return &LI;
    }
  }

  // Fold away bit casts of the loaded value by loading the desired type.
  // We can do this for BitCastInsts as well as casts from and to pointer types,
  // as long as those are noops (i.e., the source or dest type have the same
  // bitwidth as the target's pointers).
  if (LI.hasOneUse())
    if (auto* CI = dyn_cast<CastInst>(LI.user_back())) {
      if (CI->isNoopCast(DL)) {
        LoadInst *NewLoad = combineLoadToNewType(IC, LI, CI->getDestTy());
        CI->replaceAllUsesWith(NewLoad);
        IC.EraseInstFromFunction(*CI);
        return &LI;
      }
    }

  // FIXME: We should also canonicalize loads of vectors when their elements are
  // cast to other types.
  return nullptr;
}

static Instruction *unpackLoadToAggregate(InstCombiner &IC, LoadInst &LI) {
  // FIXME: We could probably with some care handle both volatile and atomic
  // stores here but it isn't clear that this is important.
  if (!LI.isSimple())
    return nullptr;

  Type *T = LI.getType();
  if (!T->isAggregateType())
    return nullptr;

  assert(LI.getAlignment() && "Alignement must be set at this point");

  if (auto *ST = dyn_cast<StructType>(T)) {
    // If the struct only have one element, we unpack.
    if (ST->getNumElements() == 1) {
      LoadInst *NewLoad = combineLoadToNewType(IC, LI, ST->getTypeAtIndex(0U),
                                               ".unpack");
      return IC.ReplaceInstUsesWith(LI, IC.Builder->CreateInsertValue(
        UndefValue::get(T), NewLoad, 0, LI.getName()));
    }
  }

  if (auto *AT = dyn_cast<ArrayType>(T)) {
    // If the array only have one element, we unpack.
    if (AT->getNumElements() == 1) {
      LoadInst *NewLoad = combineLoadToNewType(IC, LI, AT->getElementType(),
                                               ".unpack");
      return IC.ReplaceInstUsesWith(LI, IC.Builder->CreateInsertValue(
        UndefValue::get(T), NewLoad, 0, LI.getName()));
    }
  }

  return nullptr;
}

// If we can determine that all possible objects pointed to by the provided
// pointer value are, not only dereferenceable, but also definitively less than
// or equal to the provided maximum size, then return true. Otherwise, return
// false (constant global values and allocas fall into this category).
//
// FIXME: This should probably live in ValueTracking (or similar).
static bool isObjectSizeLessThanOrEq(Value *V, uint64_t MaxSize,
                                     const DataLayout &DL) {
  SmallPtrSet<Value *, 4> Visited;
  SmallVector<Value *, 4> Worklist(1, V);

  do {
    Value *P = Worklist.pop_back_val();
    P = P->stripPointerCasts();

    if (!Visited.insert(P).second)
      continue;

    if (SelectInst *SI = dyn_cast<SelectInst>(P)) {
      Worklist.push_back(SI->getTrueValue());
      Worklist.push_back(SI->getFalseValue());
      continue;
    }

    if (PHINode *PN = dyn_cast<PHINode>(P)) {
      for (Value *IncValue : PN->incoming_values())
        Worklist.push_back(IncValue);
      continue;
    }

    if (GlobalAlias *GA = dyn_cast<GlobalAlias>(P)) {
      if (GA->mayBeOverridden())
        return false;
      Worklist.push_back(GA->getAliasee());
      continue;
    }

    // If we know how big this object is, and it is less than MaxSize, continue
    // searching. Otherwise, return false.
    if (AllocaInst *AI = dyn_cast<AllocaInst>(P)) {
      if (!AI->getAllocatedType()->isSized())
        return false;

      ConstantInt *CS = dyn_cast<ConstantInt>(AI->getArraySize());
      if (!CS)
        return false;

      uint64_t TypeSize = DL.getTypeAllocSize(AI->getAllocatedType());
      // Make sure that, even if the multiplication below would wrap as an
      // uint64_t, we still do the right thing.
      if ((CS->getValue().zextOrSelf(128)*APInt(128, TypeSize)).ugt(MaxSize))
        return false;
      continue;
    }

    if (GlobalVariable *GV = dyn_cast<GlobalVariable>(P)) {
      if (!GV->hasDefinitiveInitializer() || !GV->isConstant())
        return false;

      uint64_t InitSize = DL.getTypeAllocSize(GV->getType()->getElementType());
      if (InitSize > MaxSize)
        return false;
      continue;
    }

    return false;
  } while (!Worklist.empty());

  return true;
}

// If we're indexing into an object of a known size, and the outer index is
// not a constant, but having any value but zero would lead to undefined
// behavior, replace it with zero.
//
// For example, if we have:
// @f.a = private unnamed_addr constant [1 x i32] [i32 12], align 4
// ...
// %arrayidx = getelementptr inbounds [1 x i32]* @f.a, i64 0, i64 %x
// ... = load i32* %arrayidx, align 4
// Then we know that we can replace %x in the GEP with i64 0.
//
// FIXME: We could fold any GEP index to zero that would cause UB if it were
// not zero. Currently, we only handle the first such index. Also, we could
// also search through non-zero constant indices if we kept track of the
// offsets those indices implied.
static bool canReplaceGEPIdxWithZero(InstCombiner &IC, GetElementPtrInst *GEPI,
                                     Instruction *MemI, unsigned &Idx) {
  if (GEPI->getNumOperands() < 2)
    return false;

  // Find the first non-zero index of a GEP. If all indices are zero, return
  // one past the last index.
  auto FirstNZIdx = [](const GetElementPtrInst *GEPI) {
    unsigned I = 1;
    for (unsigned IE = GEPI->getNumOperands(); I != IE; ++I) {
      Value *V = GEPI->getOperand(I);
      if (const ConstantInt *CI = dyn_cast<ConstantInt>(V))
        if (CI->isZero())
          continue;

      break;
    }

    return I;
  };

  // Skip through initial 'zero' indices, and find the corresponding pointer
  // type. See if the next index is not a constant.
  Idx = FirstNZIdx(GEPI);
  if (Idx == GEPI->getNumOperands())
    return false;
  if (isa<Constant>(GEPI->getOperand(Idx)))
    return false;

  SmallVector<Value *, 4> Ops(GEPI->idx_begin(), GEPI->idx_begin() + Idx);
  Type *AllocTy = GetElementPtrInst::getIndexedType(
      cast<PointerType>(GEPI->getOperand(0)->getType()->getScalarType())
          ->getElementType(),
      Ops);
  if (!AllocTy || !AllocTy->isSized())
    return false;
  const DataLayout &DL = IC.getDataLayout();
  uint64_t TyAllocSize = DL.getTypeAllocSize(AllocTy);

  // If there are more indices after the one we might replace with a zero, make
  // sure they're all non-negative. If any of them are negative, the overall
  // address being computed might be before the base address determined by the
  // first non-zero index.
  auto IsAllNonNegative = [&]() {
    for (unsigned i = Idx+1, e = GEPI->getNumOperands(); i != e; ++i) {
      bool KnownNonNegative, KnownNegative;
      IC.ComputeSignBit(GEPI->getOperand(i), KnownNonNegative,
                        KnownNegative, 0, MemI);
      if (KnownNonNegative)
        continue;
      return false;
    }

    return true;
  };

  // FIXME: If the GEP is not inbounds, and there are extra indices after the
  // one we'll replace, those could cause the address computation to wrap
  // (rendering the IsAllNonNegative() check below insufficient). We can do
  // better, ignoring zero indicies (and other indicies we can prove small
  // enough not to wrap).
  if (Idx+1 != GEPI->getNumOperands() && !GEPI->isInBounds())
    return false;

  // Note that isObjectSizeLessThanOrEq will return true only if the pointer is
  // also known to be dereferenceable.
  return isObjectSizeLessThanOrEq(GEPI->getOperand(0), TyAllocSize, DL) &&
         IsAllNonNegative();
}

// If we're indexing into an object with a variable index for the memory
// access, but the object has only one element, we can assume that the index
// will always be zero. If we replace the GEP, return it.
template <typename T>
static Instruction *replaceGEPIdxWithZero(InstCombiner &IC, Value *Ptr,
                                          T &MemI) {
  if (GetElementPtrInst *GEPI = dyn_cast<GetElementPtrInst>(Ptr)) {
    unsigned Idx;
    if (canReplaceGEPIdxWithZero(IC, GEPI, &MemI, Idx)) {
      Instruction *NewGEPI = GEPI->clone();
      NewGEPI->setOperand(Idx,
        ConstantInt::get(GEPI->getOperand(Idx)->getType(), 0));
      NewGEPI->insertBefore(GEPI);
      MemI.setOperand(MemI.getPointerOperandIndex(), NewGEPI);
      return NewGEPI;
    }
  }

  return nullptr;
}

Instruction *InstCombiner::visitLoadInst(LoadInst &LI) {
  Value *Op = LI.getOperand(0);

  // Try to canonicalize the loaded type.
  if (Instruction *Res = combineLoadToOperationType(*this, LI))
    return Res;

  // Attempt to improve the alignment.
  unsigned KnownAlign = getOrEnforceKnownAlignment(
      Op, DL.getPrefTypeAlignment(LI.getType()), DL, &LI, AC, DT);
  unsigned LoadAlign = LI.getAlignment();
  unsigned EffectiveLoadAlign =
      LoadAlign != 0 ? LoadAlign : DL.getABITypeAlignment(LI.getType());

  if (KnownAlign > EffectiveLoadAlign)
    LI.setAlignment(KnownAlign);
  else if (LoadAlign == 0)
    LI.setAlignment(EffectiveLoadAlign);

  // Replace GEP indices if possible.
  if (Instruction *NewGEPI = replaceGEPIdxWithZero(*this, Op, LI)) {
      Worklist.Add(NewGEPI);
      return &LI;
  }

  // None of the following transforms are legal for volatile/atomic loads.
  // FIXME: Some of it is okay for atomic loads; needs refactoring.
  if (!LI.isSimple()) return nullptr;

  if (Instruction *Res = unpackLoadToAggregate(*this, LI))
    return Res;

  // Do really simple store-to-load forwarding and load CSE, to catch cases
  // where there are several consecutive memory accesses to the same location,
  // separated by a few arithmetic operations.
  BasicBlock::iterator BBI = &LI;
  AAMDNodes AATags;
  llvmberry::ValidationUnit::Begin("load_load", LI.getParent()->getParent());
  llvmberry::ValidationUnit::GetInstance()->intrude([](
      llvmberry::Dictionary &data, llvmberry::CoreHint &hints) {
    data.create<llvmberry::ArgForFindAvailableLoadedValue>();
  });

  if (Value *AvailableVal = FindAvailableLoadedValue(Op, LI.getParent(), BBI,
                                                     6, AA, &AATags)) {
    if (LoadInst *NLI = dyn_cast<LoadInst>(AvailableVal)) {
      unsigned KnownIDs[] = {
        LLVMContext::MD_tbaa,
        LLVMContext::MD_alias_scope,
        LLVMContext::MD_noalias,
        LLVMContext::MD_range,
        LLVMContext::MD_invariant_load,
        LLVMContext::MD_nonnull,
      };
      combineMetadata(NLI, &LI, KnownIDs);
    };
    
    llvmberry::ValidationUnit::GetInstance()->intrude([&LI, &AvailableVal, &Op, this](
        llvmberry::Dictionary &data, llvmberry::CoreHint &hints) {
      
      auto falv_arg = data.get<llvmberry::ArgForFindAvailableLoadedValue>();
      auto lls_arg = data.create<llvmberry::ArgForLoadLoadStore>();

      auto &orthogonalStores = falv_arg->orthogonalStores;
      auto &ptr1EquivalentValues = falv_arg->ptr1EquivalentValues;
      auto &ptr2EquivalentValues = falv_arg->ptr2EquivalentValues;
      Value *ptr1src = ptr1EquivalentValues->back();
      Value *ptr2src = ptr2EquivalentValues->back();
      Value *ptr1 = nullptr;
      Value *ptr2 = Op;
      Instruction *v1_inst = nullptr;
      Value *v1 = nullptr;
      auto DT = this->DT;

      // If isLoadStore is true, then the equivalent value came from `store v1, %ptr1` instruction
      // If isLoadStore is false, the equivalent value came from `v1 = load %ptr1` instruction
      bool isLoadStore = falv_arg->isLoadStore;
      if (isLoadStore) {
        llvmberry::ValidationUnit::GetInstance()->setOptimizationName("load_store");
        StoreInst *si = falv_arg->loadstoreStoreInst;
        assert(si && "FindAvailableLoadedValueArgs::loadstoreStoreInst must be set "
                "after FindAvailableLoadeDvalue() is called");
        v1_inst = si;
        v1 = si->getValueOperand();
        ptr1 = si->getPointerOperand();
      } else {
        LoadInst *li = dyn_cast<LoadInst>(AvailableVal);
        v1_inst = li;
        v1 = v1_inst;
        assert(v1_inst);
        ptr1 = li->getPointerOperand();
      }
      // Step 0. prove that ptr2src and ptr1src are equivalent
      if (ptr1src == ptr2src) {
        // They are equivalent.
        INFRULE(INSTPOS(SRC, v1_inst), llvmberry::ConsIntroEq::make(VAL(ptr1src, Physical)));
        llvmberry::propagateLessdef(v1_inst, &LI, ptr1src, ptr1src, SRC);
      } else {
        // They are identical in representation, but distinct
        // ex)
        // %ptr1src = getelementptr %a, 1
        // %ptr2src = getelementptr %a, 1
        if (Instruction *i1 = dyn_cast<Instruction>(ptr1src)) {
          Instruction *i2 = dyn_cast<Instruction>(ptr2src);

          if (PHINode *phi1 = dyn_cast<PHINode>(i1)) {
            PHINode *phi2 = dyn_cast<PHINode>(i2);
            assert(phi2 && "ptr2src must be PHI node");
            std::string phi1name = llvmberry::getVariable(*phi1);
            std::string phi2name = llvmberry::getVariable(*phi2);
            for (unsigned i = 0; i < phi1->getNumIncomingValues(); i++) {
              BasicBlock *bbi1 = phi1->getIncomingBlock(i);
              Value *vi1 = phi1->getIncomingValue(i);
              
              INFRULE(
                llvmberry::TyPosition::make(SRC, *phi1, bbi1->getName()),
                llvmberry::ConsTransitivity::make(
                  VAR(phi1name, Physical), 
                  EXPR(vi1, Physical), 
                  VAR(phi2name, Physical)));
              INFRULE(
                llvmberry::TyPosition::make(SRC, *phi1, bbi1->getName()),
                llvmberry::ConsTransitivity::make(
                  VAR(phi2name, Physical), 
                  EXPR(vi1, Physical), 
                  VAR(phi1name, Physical)));
            }
            PROPAGATE(
              LESSDEF(VAR(phi1name, Physical), VAR(phi2name, Physical), SRC),
              BOUNDS(
                llvmberry::TyPosition::make(SRC, *phi1),
                llvmberry::TyPosition::make(SRC, LI)));
            
            PROPAGATE(
              LESSDEF(VAR(phi2name, Physical), VAR(phi1name, Physical), SRC),
              BOUNDS(
                llvmberry::TyPosition::make(SRC, *phi1),
                llvmberry::TyPosition::make(SRC, LI)));
          } else {
            assert(i2 && "ptr2src must be Instruction");
            llvmberry::propagateInstruction(i1, v1_inst, llvmberry::Source, true);
            llvmberry::propagateInstruction(i2, v1_inst, llvmberry::Source, true);
            llvmberry::propagateInstruction(i1, &LI, llvmberry::Source, true);
            llvmberry::propagateInstruction(i2, &LI, llvmberry::Source, true);
            
            std::string ptr1srcname = llvmberry::getVariable(*i1);
            std::string ptr2srcname = llvmberry::getVariable(*i2);
            
            // i1 >= i2 >= ptr2srcname
            if (GetElementPtrInst *gepi1 = dyn_cast<GetElementPtrInst>(i1)){
              GetElementPtrInst *gepi2 = dyn_cast<GetElementPtrInst>(i2);
              if(gepi1->isInBounds() != gepi2->isInBounds()){
                // Apply gep_inbounds_remove
                // XXX: This code only deals with the case when (gepi2->isInBounds() == true) &&
                //      (gepi1->isInBounds() == false).
                INFRULE(INSTPOS(SRC, &LI), llvmberry::ConsGepInboundsRemove::make(INSN(*gepi1)));
                INFRULE(INSTPOS(SRC, &LI), llvmberry::ConsTransitivity::make(
                    INSN(*gepi2), INSN(*gepi1), VAR(ptr1srcname, Physical)));
                INFRULE(INSTPOS(SRC, &LI), llvmberry::ConsTransitivity::make(
                    VAR(ptr2srcname, Physical), INSN(*gepi2), INSN(*gepi1)));
              }
            }

            // (ptr1srcname >= <rhs> >= ptr2srcname) -> (ptr1srcname >= ptr2srcname)
            INFRULE(INSTPOS(SRC, v1_inst), llvmberry::ConsTransitivity::make(
                VAR(ptr1srcname, Physical), INSN(*i1), VAR(ptr2srcname, Physical)));
            INFRULE(INSTPOS(SRC, &LI), llvmberry::ConsTransitivity::make(
                VAR(ptr1srcname, Physical), INSN(*i1), VAR(ptr2srcname, Physical)));
            // (ptr2srcname >= <rhs> >= ptr1srcname) -> (ptr2srcname >= ptr1srcname)
            INFRULE(INSTPOS(SRC, v1_inst), llvmberry::ConsTransitivity::make(
                VAR(ptr2srcname, Physical), INSN(*i1), VAR(ptr1srcname, Physical)));
            INFRULE(INSTPOS(SRC, &LI), llvmberry::ConsTransitivity::make(
                VAR(ptr2srcname, Physical), INSN(*i1), VAR(ptr1srcname, Physical)));
          }
        } else {
          assert("load-load optimization : unknown case ; ptr1src and ptr2src are not equivalent, and also not Instruction" && false);
        }
      }

      auto applyGepZeroAndPropagate = [&hints, &LI]
              (GetElementPtrInst *gepinst, bool is_v1, Value *src, Instruction *pos){
        INFRULE(INSTPOS(llvmberry::Source, gepinst),
            llvmberry::ConsGepzero::make(
              VAL(gepinst, Physical),
              VAL(gepinst->getPointerOperand(), Physical), 
              INSN(*gepinst)));
        if (is_v1) {
          // src >= gepinst->getPointerOperand() >= gepinst
          llvmberry::propagateLessdef(gepinst, &LI, gepinst, gepinst->getPointerOperand(), llvmberry::Source);
          llvmberry::applyTransitivity(pos, src, gepinst->getPointerOperand(), gepinst, llvmberry::Source);
        } else {
          // gepinst >= gepinst->getPointerOperand() >= src
          llvmberry::propagateLessdef(gepinst, &LI, gepinst->getPointerOperand(), &LI, llvmberry::Source);
          llvmberry::applyTransitivity(pos, gepinst, gepinst->getPointerOperand(), src, llvmberry::Source);
        }
      };
      auto applyBitCastPtrAndPropagate = [&hints, &LI]
              (BitCastInst *bi, bool is_v1, Value *src, Instruction *pos){
        INFRULE(INSTPOS(llvmberry::Source, bi),
          llvmberry::ConsBitcastptr::make(
              VAL(bi, Physical),
              VAL(bi->getOperand(0), Physical), 
              INSN(*bi)));
        if (is_v1) {
          // src >= bi->op(0) >= bi
          llvmberry::propagateLessdef(bi, &LI, bi, bi->getOperand(0), llvmberry::Source);
          llvmberry::applyTransitivity(pos, src, bi->getOperand(0), bi, llvmberry::Source);
        } else {
          // bi>=bi->op(0)>=src
          llvmberry::propagateLessdef(bi, &LI, bi->getOperand(0), bi, llvmberry::Source);
          llvmberry::applyTransitivity(pos, bi, bi->getOperand(0), src, llvmberry::Source);
        }
      };
      auto applyAddrSpacePtrAndPropagate = [&hints, &ptr1src, &ptr2src]
              (AddrSpaceCastInst *bi, bool is_v1, Value *src, Instruction *pos){
        assert("Vellvm currently does not support addrespacecast instruction, hence there's no addrspaceptr inferene rule!" && false);
      };
      
      // step 1. prove %ptr2 >= %ptr1
      // step 1-(1). prove %ptr1src >= %ptr1
      assert(ptr1EquivalentValues->at(0) == ptr1);
      if (ptr1EquivalentValues->size() >= 2) {
        auto ptr1itr = ptr1EquivalentValues->end() - 1;
        do {
          ptr1itr--;
          Value *v = *ptr1itr;
          if (GetElementPtrInst *gepinst = dyn_cast<GetElementPtrInst>(v)) {
            applyGepZeroAndPropagate(gepinst, true, ptr1src, v1_inst);
          } else if (BitCastInst *bci = dyn_cast<BitCastInst>(v)) {
            applyBitCastPtrAndPropagate(bci, true, ptr1src, v1_inst);
          } else if (AddrSpaceCastInst *asci = dyn_cast<AddrSpaceCastInst>(v)) {
            applyAddrSpacePtrAndPropagate(asci, true, ptr1src, v1_inst);
          }
          std::string orgdesc = llvmberry::ValidationUnit::GetInstance()->getDescription();
          llvmberry::ValidationUnit::GetInstance()->setDescription(orgdesc + "ptr1equiv : " + llvmberry::getVariable(*v));
        } while (ptr1itr != ptr1EquivalentValues->begin());
        // If ptr1EquivalenceValues->size() is equivalent to 0, we cannot propagate ptr1src >= ptr1 from 
        // the definition of ptr1src to ptr1 because ptr1src == ptr1. 
        PROPAGATE(LESSDEF(
              VAR(llvmberry::getVariable(*ptr1src), Physical),
              VAR(llvmberry::getVariable(*ptr1), Physical),
              llvmberry::Source),
            BOUNDS(INSTPOS(llvmberry::Source, v1_inst), INSTPOS(llvmberry::Source, &LI)));
      }
      // step 1-(2). prove %ptr2 >= %ptr2src
      assert(ptr2EquivalentValues->at(0) == ptr2);
      if (ptr2EquivalentValues->size() >= 2) {
        auto ptr2itr = ptr2EquivalentValues->end() - 1;
        do {
          ptr2itr--;
          Value *v = *ptr2itr;
          if (GetElementPtrInst *gepinst = dyn_cast<GetElementPtrInst>(v)) {
            applyGepZeroAndPropagate(gepinst, false, ptr2src, &LI);
          } else if(BitCastInst *bci = dyn_cast<BitCastInst>(v)) {
            applyBitCastPtrAndPropagate(bci, false, ptr2src, &LI);
          } else if(AddrSpaceCastInst *asci = dyn_cast<AddrSpaceCastInst>(v)) {
            applyAddrSpacePtrAndPropagate(asci, false, ptr2src, &LI);
          }
          std::string orgdesc = llvmberry::ValidationUnit::GetInstance()->getDescription();
          llvmberry::ValidationUnit::GetInstance()->setDescription(orgdesc + "ptr2equiv : " + llvmberry::getVariable(*v));
        } while (ptr2itr != ptr2EquivalentValues->begin());
      }
      // step 1-(3). prove %ptr2 >= %ptr1 by transitivity (%ptr2 >= %ptr2src >= %ptr1src >= %ptr1)
      llvmberry::applyTransitivity(&LI, ptr2src, ptr1src, ptr1, llvmberry::Source);
      llvmberry::applyTransitivity(&LI, ptr2, ptr2src, ptr1, llvmberry::Source);

     
      // step 2. Show orthogonality
      for (auto itr = orthogonalStores->begin(); itr != orthogonalStores->end(); itr++) {
        llvmberry::StripPointerCastsArg::TyStrippedValues ptr3EquivalentValues = itr->first;
        StoreInst *noalias_si = itr->second.first;
        std::string noalias_source = itr->second.second;
        Value *ptr3src = ptr3EquivalentValues->back();
        Value *ptr3 = ptr3EquivalentValues->front();
        
        // A range of propagation of ptr1 _||_ ptr3
        Instruction *diffblock_propagate_beg = nullptr;
        Instruction *diffblock_propagate_end = nullptr;
        
        // step 2-A. make ptr1src _||_ ptr3src
        if (noalias_source == "alloca_or_global") {
          if (AllocaInst *ptr3_org_alloca = dyn_cast<AllocaInst>(ptr3src)) {
            diffblock_propagate_end = ptr3_org_alloca;
          }
          if (AllocaInst *ptr1_org_alloca = dyn_cast<AllocaInst>(ptr1src)) {
            if (diffblock_propagate_end == nullptr ||
                // Does sdiffblock_propagate_end precede ptr1_org_alloca?
                DT->dominates(diffblock_propagate_end, ptr1_org_alloca)) {
              diffblock_propagate_end = ptr1_org_alloca;
            }
          }
          // there must be noalias between ptr1src and noalias_ptrs->back().
          if (AllocaInst *ptr3_alloca = dyn_cast<AllocaInst>(ptr3src)){
            if (AllocaInst *ptr1_alloca = dyn_cast<AllocaInst>(ptr1src)) {
              // Propagate alloca.
              // Does ptr1_alloca dominate ptr3_alloca ?
              AllocaInst *alloca_ahead = nullptr;
              AllocaInst *alloca_behind = nullptr;
              if (DT->dominates(ptr1_alloca, ptr3_alloca)) {
                alloca_ahead = ptr1_alloca;
                alloca_behind = ptr3_alloca;
                diffblock_propagate_beg = ptr3_alloca;
              } else if (DT->dominates(ptr3_alloca, ptr1_alloca)) {
                alloca_ahead = ptr3_alloca;
                alloca_behind = ptr1_alloca;
                diffblock_propagate_beg = ptr1_alloca;
              } else {
                assert("ptr1_alloca or ptr3_alloca must dominate another!" && false);
              }
              PROPAGATE(llvmberry::ConsAlloca::make(
                    REGISTER(llvmberry::getVariable(*alloca_ahead), Physical), llvmberry::Source),
                  BOUNDS(INSTPOS(llvmberry::Source, alloca_ahead), INSTPOS(llvmberry::Source, alloca_behind)));
            } else if (GlobalVariable *ptr1_gv = llvm::dyn_cast<GlobalVariable>(ptr1src)) {
              INFRULE(INSTPOS(llvmberry::Source, ptr3_alloca),
                llvmberry::ConsDiffblockGlobalAlloca::make(
                    llvmberry::ConsConstGlobalVarAddr::make(*ptr1_gv),
                    REGISTER(llvmberry::getVariable(*ptr3_alloca), Physical)));
              diffblock_propagate_beg = ptr3_alloca;
            } else {
              assert("Noalias store must be (alloca OR global) _||_ (alloca OR global)" && false);
            }

          } else if (GlobalVariable *ptr3_gv = llvm::dyn_cast<GlobalVariable>(ptr3src)) {
            if (AllocaInst *ptr1_alloca = llvm::dyn_cast<AllocaInst>(ptr1src)) {
              INFRULE(INSTPOS(llvmberry::Source, ptr1_alloca),
                llvmberry::ConsDiffblockGlobalAlloca::make(
                    llvmberry::ConsConstGlobalVarAddr::make(*ptr3_gv),
                    REGISTER(llvmberry::getVariable(*ptr1_alloca), Physical)));
              diffblock_propagate_beg = ptr1_alloca;

            } else if(GlobalVariable *ptr1_gv = llvm::dyn_cast<GlobalVariable>(ptr1src)) {
              INFRULE(INSTPOS(llvmberry::Source, noalias_si),
                llvmberry::ConsDiffblockGlobalGlobal::make(
                    llvmberry::ConsConstGlobalVarAddr::make(*ptr1_gv),
                    llvmberry::ConsConstGlobalVarAddr::make(*ptr3_gv)));
              diffblock_propagate_beg = diffblock_propagate_end;
            } else {
              assert("Noalias store must be (alloca OR global) _||_ (alloca OR global)" && false);
            }
          } else {
            assert("Noalias store must be (alloca OR global) _||_ (alloca OR global)" && false);
          }
          // step 2-B. propagate ptr1src _||_ ptr3src 
          if (diffblock_propagate_beg != diffblock_propagate_end) {
            PROPAGATE(llvmberry::ConsDiffblock::make(
                  VAL(ptr1src, Physical), VAL(ptr3src, Physical), llvmberry::Source),
                BOUNDS(INSTPOS(llvmberry::Source, diffblock_propagate_beg),
                  INSTPOS(llvmberry::Source, diffblock_propagate_end)));
          }
          // ptr1src >= ptr1 is already shown.
          // We have to show ptr3src >= ptr3.
          if (diffblock_propagate_end == nullptr)
            diffblock_propagate_end = noalias_si;
          if (ptr3EquivalentValues->size() > 1) {
            for (size_t i = 1; i < ptr3EquivalentValues->size(); i++) {
              Value *v = ptr3EquivalentValues->at(i);
              if (GetElementPtrInst *gepinst = dyn_cast<GetElementPtrInst>(v)) {
                applyGepZeroAndPropagate(gepinst, true, ptr3src, diffblock_propagate_end);
              } else if(BitCastInst *bci = dyn_cast<BitCastInst>(v)) {
                applyBitCastPtrAndPropagate(bci, true, ptr3src, diffblock_propagate_end);
              } else if(AddrSpaceCastInst *asci = dyn_cast<AddrSpaceCastInst>(v)) {
                applyAddrSpacePtrAndPropagate(asci, true, ptr3src, diffblock_propagate_end);
              }
            }
          } else {
            // ptr3src >= ptr3src
            assert(ptr3src == ptr3);
            INFRULE(INSTPOS(llvmberry::Source, diffblock_propagate_end),
              llvmberry::ConsIntroEq::make(VAL(ptr3src, Physical)));
          }
          // Now, apply DiffblockLessthan. : it makes ptr1src _||_ ptr3src => ptr1 _||_ ptr3
          INFRULE(INSTPOS(llvmberry::Source, diffblock_propagate_end),
            llvmberry::ConsDiffblockLessthan::make(
                VAL(ptr1src, Physical), VAL(ptr3src, Physical), VAL(ptr1, Physical), VAL(ptr3, Physical)));
         
          // step 2-C. propagate ptr1 _||_ ptr3.
          if (diffblock_propagate_end != noalias_si) {
            PROPAGATE(llvmberry::ConsDiffblock::make(VAL(ptr1, Physical), VAL(ptr3, Physical), llvmberry::Source),
                BOUNDS(INSTPOS(llvmberry::Source, diffblock_propagate_end),
                  INSTPOS(llvmberry::Source, noalias_si)));
          }

          // step 2-D. make and propagate ptr1 _|_ ptr3
          // ptr1 _||_ ptr3 -> ptr1 _|_ ptr3
          INFRULE(INSTPOS(llvmberry::Source, diffblock_propagate_end),
            llvmberry::ConsDiffblockNoalias::make(
                VAL(ptr1, Physical), VAL(ptr3, Physical),
                POINTER(ptr1), POINTER(ptr3)));
          // Now, propagate ptr1 _|_ ptr3 to the store instruction
          if (diffblock_propagate_end != noalias_si) {
            PROPAGATE(NOALIAS(POINTER(ptr1), POINTER(ptr3), llvmberry::Source),
              BOUNDS(INSTPOS(llvmberry::Source, diffblock_propagate_end),
                  INSTPOS(llvmberry::Source, noalias_si)));
          }
        } else if(noalias_source == "aliasanalysis") {
          // TODO:Currently we cannot prove noalias based on alias analysis..
          llvmberry::ValidationUnit::GetInstance()->setDescription("This load-load/load-store optimization is done because"
                "alias analysis reported that two pointers are equivalent. "
                "However currently we cannot prove that two pointers are equivalent even if"
                " alias analysis info is given.");
        }
      }


      lls_arg->v1_inst = v1_inst;
      lls_arg->v1 = v1;
      lls_arg->ptr1 = ptr1;
      lls_arg->ptr1src = ptr1src;
      lls_arg->ptr2 = ptr2;
      lls_arg->ptr2src = ptr2src;
      lls_arg->v2_org_position = INSTPOS(llvmberry::Source, &LI);
      // step 3, 4, 5 must be done below..
    });

    Value *NewInst = Builder->CreateBitOrPointerCast(AvailableVal, LI.getType(),
                                            LI.getName() + ".cast");

    llvmberry::ValidationUnit::GetInstance()->intrude([&NewInst, &AvailableVal, &LI](
        llvmberry::Dictionary &data, llvmberry::CoreHint &hints) {
      auto falv_arg = data.get<llvmberry::ArgForFindAvailableLoadedValue>();
      auto lls_arg = data.get<llvmberry::ArgForLoadLoadStore>();
      
      Instruction *v1_inst = lls_arg->v1_inst;
      Value *v1 = lls_arg->v1;
      Value *ptr1 = lls_arg->ptr1;
      Value *ptr2 = lls_arg->ptr2;
      std::shared_ptr<llvmberry::TyPosition> LI_org_pos = lls_arg->v2_org_position;
      assert(v1 && "load-load optimization : v1 should not be null.");

      if (NewInst != AvailableVal) {
        // step 3. Prove %v2 >= \hat{k}.
        Instruction *inst = dyn_cast<Instruction>(NewInst);
        assert(inst);
        
        llvmberry::insertSrcNopAtTgtI(hints, inst);
        PROPAGATE(llvmberry::ConsMaydiff::make(llvmberry::getVariable(*inst), 
              llvmberry::Physical),
            llvmberry::ConsGlobal::make());
        // Step 3-(1). propagate *(%ptr1) >= %v1.
        PROPAGATE(LESSDEF(INSN(*v1_inst), EXPR(v1, Physical), SRC),
            BOUNDS(INSTPOS(SRC, v1_inst), LI_org_pos));
        // Step 3-(2). Create \hat{k}, make *(%ptr1) >= \hat{k}
        //   <cast %v1> >= ^k,   ^k >= <cast %v1>
        INFRULE(INSTPOS(TGT, inst),
          llvmberry::ConsIntroGhost::make(INSN(*inst), REGISTER("^k", Ghost)));
        //   Propagate "<cast %v1> >= ^k" from nop to %v2 in src
        PROPAGATE(LESSDEF(INSN(*inst), VAR("^k", Ghost), SRC),
          BOUNDS(INSTPOS(TGT, inst), LI_org_pos));
        //   Apply Transitivity to ^k >= bitcast %v1 >= %v1' from %v1' in tgt
        INFRULE(INSTPOS(TGT, inst),
          llvmberry::ConsTransitivityTgt::make(
              VAR("^k", Ghost), INSN(*inst), EXPR(inst, Physical)));
        // Propagate ^k >= %v1' from %v1' to %v2 in tgt
        PROPAGATE(LESSDEF(VAR("^k", Ghost), 
            VAR(llvmberry::getVariable(*inst), Physical), TGT),
          BOUNDS(INSTPOS(TGT, inst), INSTPOS(TGT, &LI)));
 
        if (BitCastInst *bi = dyn_cast<BitCastInst>(inst)) {
          // Make *(%ptr1) >= ^k from (*(%ptr1) >= %v1) && (bitcast %v1 >= ^k) at %v2 in src
          INFRULE(LI_org_pos,
            llvmberry::ConsBitcastLoad::make(
                VAL(ptr1, Physical),
                VALTYPE(bi->getSrcTy()),
                VAL(AvailableVal, Physical),
                VALTYPE(bi->getDestTy()),
                ID("^k", Ghost),
                BITSIZE(LI.getAlignment())));
        } else if (PtrToIntInst *ptii = dyn_cast<PtrToIntInst>(inst)) {
          INFRULE(LI_org_pos,
            llvmberry::ConsPtrtointLoad::make(
                VAL(ptr1, Physical),
                VALTYPE(ptii->getSrcTy()),
                VAL(AvailableVal, Physical),
                VALTYPE(ptii->getDestTy()),
                ID("^k", Ghost),
                BITSIZE(LI.getAlignment())));
        } else if (IntToPtrInst *itpi = dyn_cast<IntToPtrInst>(inst)) {
          INFRULE(LI_org_pos,
            llvmberry::ConsInttoptrLoad::make(
                VAL(ptr1, Physical),
                VALTYPE(itpi->getSrcTy()),
                VAL(AvailableVal, Physical),
                VALTYPE(itpi->getDestTy()),
                ID("^k", Ghost),
                BITSIZE(LI.getAlignment())));
        } else {
          assert(false && "Unknown casting instruction appeared in load-load optimization!");
        }
        // Step 3-(3). make *(ptr2) >= \hat{k} from %ptr2 >= %ptr1, *(%ptr1) >= \hat{k}
        INFRULE(LI_org_pos,
          llvmberry::ConsTransitivityPointerLhs::make(
              VAL(ptr2, Physical), // p
              VAL(ptr1, Physical), // q
              ID("^k", Ghost), // k ; load q >= \hat{k}
              VALTYPE(LI.getType()),
              BITSIZE(LI.getAlignment())));
        // Step 3-(4). make %v2 >= hat{k} from %v2 >= *(%ptr2), *(%ptr2) >= \hat{k}
        INFRULE(LI_org_pos,
          llvmberry::ConsTransitivity::make(
              VAR(llvmberry::getVariable(LI), Physical), INSN(LI), VAR("^k", Ghost)));
        
        llvmberry::generateHintForReplaceAllUsesWith(&LI, NewInst, "^k", LI_org_pos);
      } else {
        // step 3. prove %v2 >= %v1 
        // step 3-(1). propagate *(%ptr1) >= %v1.
        PROPAGATE(LESSDEF(INSN(*v1_inst), EXPR(v1, Physical), llvmberry::Source),
            BOUNDS(INSTPOS(llvmberry::Source, v1_inst),
                INSTPOS(llvmberry::Source, &LI)));
        // step 3-(2). prove %v2 >= *(ptr2) >= %v1
        INFRULE(INSTPOS(llvmberry::Source, &LI),
          llvmberry::ConsTransitivityPointerLhs::make(
              VAL(ptr2, Physical), // p
              VAL(ptr1, Physical), // q
              VAL(v1, Physical), // v ; load q >= v
              VALTYPE(LI.getType()),
              BITSIZE(LI.getAlignment())));
        // step 3-(3). prove %v2 >= %v1
        INFRULE(INSTPOS(llvmberry::Source, &LI),
          llvmberry::ConsTransitivity::make(
              VAR(llvmberry::getVariable(LI), Physical), INSN(LI), EXPR(v1, Physical)));
         llvmberry::generateHintForReplaceAllUsesWith(&LI, NewInst);
      }
      
    });

    Instruction *res = ReplaceInstUsesWith(LI, NewInst);

    llvmberry::ValidationUnit::End();

    return res;
  }
  llvmberry::ValidationUnit::GetInstance()->setIsAborted();
  llvmberry::ValidationUnit::End();

  // load(gep null, ...) -> unreachable
  if (GetElementPtrInst *GEPI = dyn_cast<GetElementPtrInst>(Op)) {
    const Value *GEPI0 = GEPI->getOperand(0);
    // TODO: Consider a target hook for valid address spaces for this xform.
    if (isa<ConstantPointerNull>(GEPI0) && GEPI->getPointerAddressSpace() == 0){
      // Insert a new store to null instruction before the load to indicate
      // that this code is not reachable.  We do this instead of inserting
      // an unreachable instruction directly because we cannot modify the
      // CFG.
      new StoreInst(UndefValue::get(LI.getType()),
                    Constant::getNullValue(Op->getType()), &LI);
      return ReplaceInstUsesWith(LI, UndefValue::get(LI.getType()));
    }
  }

  // load null/undef -> unreachable
  // TODO: Consider a target hook for valid address spaces for this xform.
  if (isa<UndefValue>(Op) ||
      (isa<ConstantPointerNull>(Op) && LI.getPointerAddressSpace() == 0)) {
    // Insert a new store to null instruction before the load to indicate that
    // this code is not reachable.  We do this instead of inserting an
    // unreachable instruction directly because we cannot modify the CFG.
    new StoreInst(UndefValue::get(LI.getType()),
                  Constant::getNullValue(Op->getType()), &LI);
    return ReplaceInstUsesWith(LI, UndefValue::get(LI.getType()));
  }

  if (Op->hasOneUse()) {
    // Change select and PHI nodes to select values instead of addresses: this
    // helps alias analysis out a lot, allows many others simplifications, and
    // exposes redundancy in the code.
    //
    // Note that we cannot do the transformation unless we know that the
    // introduced loads cannot trap!  Something like this is valid as long as
    // the condition is always false: load (select bool %C, int* null, int* %G),
    // but it would not be valid if we transformed it to load from null
    // unconditionally.
    //
    if (SelectInst *SI = dyn_cast<SelectInst>(Op)) {
      // load (select (Cond, &V1, &V2))  --> select(Cond, load &V1, load &V2).
      unsigned Align = LI.getAlignment();
      if (isSafeToLoadUnconditionally(SI->getOperand(1), SI, Align) &&
          isSafeToLoadUnconditionally(SI->getOperand(2), SI, Align)) {
        LoadInst *V1 = Builder->CreateLoad(SI->getOperand(1),
                                           SI->getOperand(1)->getName()+".val");
        LoadInst *V2 = Builder->CreateLoad(SI->getOperand(2),
                                           SI->getOperand(2)->getName()+".val");
        V1->setAlignment(Align);
        V2->setAlignment(Align);
        return SelectInst::Create(SI->getCondition(), V1, V2);
      }

      // load (select (cond, null, P)) -> load P
      if (isa<ConstantPointerNull>(SI->getOperand(1)) && 
          LI.getPointerAddressSpace() == 0) {
        LI.setOperand(0, SI->getOperand(2));
        return &LI;
      }

      // load (select (cond, P, null)) -> load P
      if (isa<ConstantPointerNull>(SI->getOperand(2)) &&
          LI.getPointerAddressSpace() == 0) {
        LI.setOperand(0, SI->getOperand(1));
        return &LI;
      }
    }
  }
  return nullptr;
}

/// \brief Combine stores to match the type of value being stored.
///
/// The core idea here is that the memory does not have any intrinsic type and
/// where we can we should match the type of a store to the type of value being
/// stored.
///
/// However, this routine must never change the width of a store or the number of
/// stores as that would introduce a semantic change. This combine is expected to
/// be a semantic no-op which just allows stores to more closely model the types
/// of their incoming values.
///
/// Currently, we also refuse to change the precise type used for an atomic or
/// volatile store. This is debatable, and might be reasonable to change later.
/// However, it is risky in case some backend or other part of LLVM is relying
/// on the exact type stored to select appropriate atomic operations.
///
/// \returns true if the store was successfully combined away. This indicates
/// the caller must erase the store instruction. We have to let the caller erase
/// the store instruction sas otherwise there is no way to signal whether it was
/// combined or not: IC.EraseInstFromFunction returns a null pointer.
static bool combineStoreToValueType(InstCombiner &IC, StoreInst &SI) {
  // FIXME: We could probably with some care handle both volatile and atomic
  // stores here but it isn't clear that this is important.
  if (!SI.isSimple())
    return false;

  Value *V = SI.getValueOperand();

  // Fold away bit casts of the stored value by storing the original type.
  if (auto *BC = dyn_cast<BitCastInst>(V)) {
    V = BC->getOperand(0);
    combineStoreToNewValue(IC, SI, V);
    return true;
  }

  // FIXME: We should also canonicalize loads of vectors when their elements are
  // cast to other types.
  return false;
}

static bool unpackStoreToAggregate(InstCombiner &IC, StoreInst &SI) {
  // FIXME: We could probably with some care handle both volatile and atomic
  // stores here but it isn't clear that this is important.
  if (!SI.isSimple())
    return false;

  Value *V = SI.getValueOperand();
  Type *T = V->getType();

  if (!T->isAggregateType())
    return false;

  if (auto *ST = dyn_cast<StructType>(T)) {
    // If the struct only have one element, we unpack.
    if (ST->getNumElements() == 1) {
      V = IC.Builder->CreateExtractValue(V, 0);
      combineStoreToNewValue(IC, SI, V);
      return true;
    }
  }

  if (auto *AT = dyn_cast<ArrayType>(T)) {
    // If the array only have one element, we unpack.
    if (AT->getNumElements() == 1) {
      V = IC.Builder->CreateExtractValue(V, 0);
      combineStoreToNewValue(IC, SI, V);
      return true;
    }
  }

  return false;
}

/// equivalentAddressValues - Test if A and B will obviously have the same
/// value. This includes recognizing that %t0 and %t1 will have the same
/// value in code like this:
///   %t0 = getelementptr \@a, 0, 3
///   store i32 0, i32* %t0
///   %t1 = getelementptr \@a, 0, 3
///   %t2 = load i32* %t1
///
static bool equivalentAddressValues(Value *A, Value *B) {
  // Test if the values are trivially equivalent.
  if (A == B) return true;

  // Test if the values come form identical arithmetic instructions.
  // This uses isIdenticalToWhenDefined instead of isIdenticalTo because
  // its only used to compare two uses within the same basic block, which
  // means that they'll always either have the same value or one of them
  // will have an undefined value.
  if (isa<BinaryOperator>(A) ||
      isa<CastInst>(A) ||
      isa<PHINode>(A) ||
      isa<GetElementPtrInst>(A))
    if (Instruction *BI = dyn_cast<Instruction>(B))
      if (cast<Instruction>(A)->isIdenticalToWhenDefined(BI))
        return true;

  // Otherwise they may not be equivalent.
  return false;
}

Instruction *InstCombiner::visitStoreInst(StoreInst &SI) {
  Value *Val = SI.getOperand(0);
  Value *Ptr = SI.getOperand(1);

  // Try to canonicalize the stored type.
  if (combineStoreToValueType(*this, SI))
    return EraseInstFromFunction(SI);

  // Attempt to improve the alignment.
  unsigned KnownAlign = getOrEnforceKnownAlignment(
      Ptr, DL.getPrefTypeAlignment(Val->getType()), DL, &SI, AC, DT);
  unsigned StoreAlign = SI.getAlignment();
  unsigned EffectiveStoreAlign =
      StoreAlign != 0 ? StoreAlign : DL.getABITypeAlignment(Val->getType());

  if (KnownAlign > EffectiveStoreAlign)
    SI.setAlignment(KnownAlign);
  else if (StoreAlign == 0)
    SI.setAlignment(EffectiveStoreAlign);

  // Try to canonicalize the stored type.
  if (unpackStoreToAggregate(*this, SI))
    return EraseInstFromFunction(SI);

  // Replace GEP indices if possible.
  if (Instruction *NewGEPI = replaceGEPIdxWithZero(*this, Ptr, SI)) {
      Worklist.Add(NewGEPI);
      return &SI;
  }

  // Don't hack volatile/atomic stores.
  // FIXME: Some bits are legal for atomic stores; needs refactoring.
  if (!SI.isSimple()) return nullptr;

  // If the RHS is an alloca with a single use, zapify the store, making the
  // alloca dead.
  if (Ptr->hasOneUse()) {
    if (isa<AllocaInst>(Ptr)) {
      llvmberry::ValidationUnit::Begin("dead_store_elim", SI.getParent()->getParent());
      llvmberry::ValidationUnit::GetInstance()->intrude([&SI, &Ptr](
          llvmberry::Dictionary &data, llvmberry::CoreHint &hints) {
        std::string regname = llvmberry::getVariable(*Ptr);
        AllocaInst *ai = dyn_cast<AllocaInst>(Ptr);
        llvmberry::insertTgtNopAtSrcI(hints, &SI);
        PROPAGATE(ALLOCA(REGISTER(regname, Physical), SRC),
            BOUNDS(INSTPOS(SRC, ai), INSTPOS(SRC, &SI)));
        PROPAGATE(PRIVATE(REGISTER(regname, Physical), SRC),
            BOUNDS(INSTPOS(SRC, ai), INSTPOS(SRC, &SI)));
      });
      return EraseInstFromFunction(SI);
    }
    if (GetElementPtrInst *GEP = dyn_cast<GetElementPtrInst>(Ptr)) {
      if (isa<AllocaInst>(GEP->getOperand(0))) {
        if (GEP->getOperand(0)->hasOneUse())
          return EraseInstFromFunction(SI);
      }
    }
  }

  // Do really simple DSE, to catch cases where there are several consecutive
  // stores to the same location, separated by a few arithmetic operations. This
  // situation often occurs with bitfield accesses.
  BasicBlock::iterator BBI = &SI;
  for (unsigned ScanInsts = 6; BBI != SI.getParent()->begin() && ScanInsts;
       --ScanInsts) {
    --BBI;
    // Don't count debug info directives, lest they affect codegen,
    // and we skip pointer-to-pointer bitcasts, which are NOPs.
    if (isa<DbgInfoIntrinsic>(BBI) ||
        (isa<BitCastInst>(BBI) && BBI->getType()->isPointerTy())) {
      ScanInsts++;
      continue;
    }

    if (StoreInst *PrevSI = dyn_cast<StoreInst>(BBI)) {
      // Prev store isn't volatile, and stores to the same location?
      if (PrevSI->isSimple() && equivalentAddressValues(PrevSI->getOperand(1),
                                                        SI.getOperand(1))) {
        ++NumDeadStore;
        ++BBI;
        EraseInstFromFunction(*PrevSI);
        continue;
      }
      break;
    }

    // If this is a load, we have to stop.  However, if the loaded value is from
    // the pointer we're loading and is producing the pointer we're storing,
    // then *this* store is dead (X = load P; store X -> P).
    if (LoadInst *LI = dyn_cast<LoadInst>(BBI)) {
      if (LI == Val && equivalentAddressValues(LI->getOperand(0), Ptr) &&
          LI->isSimple())
        return EraseInstFromFunction(SI);

      // Otherwise, this is a load from some other location.  Stores before it
      // may not be dead.
      break;
    }

    // Don't skip over loads or things that can modify memory.
    if (BBI->mayWriteToMemory() || BBI->mayReadFromMemory())
      break;
  }

  // store X, null    -> turns into 'unreachable' in SimplifyCFG
  if (isa<ConstantPointerNull>(Ptr) && SI.getPointerAddressSpace() == 0) {
    if (!isa<UndefValue>(Val)) {
      SI.setOperand(0, UndefValue::get(Val->getType()));
      if (Instruction *U = dyn_cast<Instruction>(Val))
        Worklist.Add(U);  // Dropped a use.
    }
    return nullptr;  // Do not modify these!
  }

  // store undef, Ptr -> noop
  if (isa<UndefValue>(Val))
    return EraseInstFromFunction(SI);

  // If this store is the last instruction in the basic block (possibly
  // excepting debug info instructions), and if the block ends with an
  // unconditional branch, try to move it to the successor block.
  BBI = &SI;
  do {
    ++BBI;
  } while (isa<DbgInfoIntrinsic>(BBI) ||
           (isa<BitCastInst>(BBI) && BBI->getType()->isPointerTy()));
  if (BranchInst *BI = dyn_cast<BranchInst>(BBI))
    if (BI->isUnconditional())
      if (SimplifyStoreAtEndOfBlock(SI))
        return nullptr;  // xform done!

  return nullptr;
}

/// SimplifyStoreAtEndOfBlock - Turn things like:
///   if () { *P = v1; } else { *P = v2 }
/// into a phi node with a store in the successor.
///
/// Simplify things like:
///   *P = v1; if () { *P = v2; }
/// into a phi node with a store in the successor.
///
bool InstCombiner::SimplifyStoreAtEndOfBlock(StoreInst &SI) {
  BasicBlock *StoreBB = SI.getParent();

  // Check to see if the successor block has exactly two incoming edges.  If
  // so, see if the other predecessor contains a store to the same location.
  // if so, insert a PHI node (if needed) and move the stores down.
  BasicBlock *DestBB = StoreBB->getTerminator()->getSuccessor(0);

  // Determine whether Dest has exactly two predecessors and, if so, compute
  // the other predecessor.
  pred_iterator PI = pred_begin(DestBB);
  BasicBlock *P = *PI;
  BasicBlock *OtherBB = nullptr;

  if (P != StoreBB)
    OtherBB = P;

  if (++PI == pred_end(DestBB))
    return false;

  P = *PI;
  if (P != StoreBB) {
    if (OtherBB)
      return false;
    OtherBB = P;
  }
  if (++PI != pred_end(DestBB))
    return false;

  // Bail out if all the relevant blocks aren't distinct (this can happen,
  // for example, if SI is in an infinite loop)
  if (StoreBB == DestBB || OtherBB == DestBB)
    return false;

  // Verify that the other block ends in a branch and is not otherwise empty.
  BasicBlock::iterator BBI = OtherBB->getTerminator();
  BranchInst *OtherBr = dyn_cast<BranchInst>(BBI);
  if (!OtherBr || BBI == OtherBB->begin())
    return false;

  // If the other block ends in an unconditional branch, check for the 'if then
  // else' case.  there is an instruction before the branch.
  StoreInst *OtherStore = nullptr;
  if (OtherBr->isUnconditional()) {
    --BBI;
    // Skip over debugging info.
    while (isa<DbgInfoIntrinsic>(BBI) ||
           (isa<BitCastInst>(BBI) && BBI->getType()->isPointerTy())) {
      if (BBI==OtherBB->begin())
        return false;
      --BBI;
    }
    // If this isn't a store, isn't a store to the same location, or is not the
    // right kind of store, bail out.
    OtherStore = dyn_cast<StoreInst>(BBI);
    if (!OtherStore || OtherStore->getOperand(1) != SI.getOperand(1) ||
        !SI.isSameOperationAs(OtherStore))
      return false;
  } else {
    // Otherwise, the other block ended with a conditional branch. If one of the
    // destinations is StoreBB, then we have the if/then case.
    if (OtherBr->getSuccessor(0) != StoreBB &&
        OtherBr->getSuccessor(1) != StoreBB)
      return false;

    // Okay, we know that OtherBr now goes to Dest and StoreBB, so this is an
    // if/then triangle.  See if there is a store to the same ptr as SI that
    // lives in OtherBB.
    for (;; --BBI) {
      // Check to see if we find the matching store.
      if ((OtherStore = dyn_cast<StoreInst>(BBI))) {
        if (OtherStore->getOperand(1) != SI.getOperand(1) ||
            !SI.isSameOperationAs(OtherStore))
          return false;
        break;
      }
      // If we find something that may be using or overwriting the stored
      // value, or if we run out of instructions, we can't do the xform.
      if (BBI->mayReadFromMemory() || BBI->mayWriteToMemory() ||
          BBI == OtherBB->begin())
        return false;
    }

    // In order to eliminate the store in OtherBr, we have to
    // make sure nothing reads or overwrites the stored value in
    // StoreBB.
    for (BasicBlock::iterator I = StoreBB->begin(); &*I != &SI; ++I) {
      // FIXME: This should really be AA driven.
      if (I->mayReadFromMemory() || I->mayWriteToMemory())
        return false;
    }
  }

  // Insert a PHI node now if we need it.
  Value *MergedVal = OtherStore->getOperand(0);
  if (MergedVal != SI.getOperand(0)) {
    PHINode *PN = PHINode::Create(MergedVal->getType(), 2, "storemerge");
    PN->addIncoming(SI.getOperand(0), SI.getParent());
    PN->addIncoming(OtherStore->getOperand(0), OtherBB);
    MergedVal = InsertNewInstBefore(PN, DestBB->front());
  }

  // Advance to a place where it is safe to insert the new store and
  // insert it.
  BBI = DestBB->getFirstInsertionPt();
  StoreInst *NewSI = new StoreInst(MergedVal, SI.getOperand(1),
                                   SI.isVolatile(),
                                   SI.getAlignment(),
                                   SI.getOrdering(),
                                   SI.getSynchScope());
  InsertNewInstBefore(NewSI, *BBI);
  NewSI->setDebugLoc(OtherStore->getDebugLoc());

  // If the two stores had AA tags, merge them.
  AAMDNodes AATags;
  SI.getAAMetadata(AATags);
  if (AATags) {
    OtherStore->getAAMetadata(AATags, /* Merge = */ true);
    NewSI->setAAMetadata(AATags);
  }

  // Nuke the old stores.
  EraseInstFromFunction(SI);
  EraseInstFromFunction(*OtherStore);
  return true;
}
