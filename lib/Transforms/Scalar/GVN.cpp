//===- GVN.cpp - Eliminate redundant values and loads ---------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This pass performs global value numbering to eliminate fully redundant
// instructions.  It also performs simple dead load elimination.
//
// Note that this pass does the value numbering itself; it does not use the
// ValueNumbering analysis passes.
//
//===----------------------------------------------------------------------===//

#include "llvm/Transforms/Scalar.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DepthFirstIterator.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/AssumptionCache.h"
#include "llvm/Analysis/CFG.h"
#include "llvm/Analysis/ConstantFolding.h"
#include "llvm/Analysis/InstructionSimplify.h"
#include "llvm/Analysis/Loads.h"
#include "llvm/Analysis/MemoryBuiltins.h"
#include "llvm/Analysis/MemoryDependenceAnalysis.h"
#include "llvm/Analysis/PHITransAddr.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/PatternMatch.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/Local.h"
#include "llvm/Transforms/Utils/SSAUpdater.h"

#include "llvm/LLVMBerry/ValidationUnit.h"
#include "llvm/LLVMBerry/Infrules.h"
#include "llvm/LLVMBerry/Hintgen.h"

#include <vector>
using namespace llvm;
using namespace PatternMatch;

#define DEBUG_TYPE "gvn"

STATISTIC(NumGVNInstr,  "Number of instructions deleted");
STATISTIC(NumGVNLoad,   "Number of loads deleted");
STATISTIC(NumGVNPRE,    "Number of instructions PRE'd");
STATISTIC(NumGVNBlocks, "Number of blocks merged");
STATISTIC(NumGVNSimpl,  "Number of instructions simplified");
STATISTIC(NumGVNEqProp, "Number of equalities propagated");
STATISTIC(NumPRELoad,   "Number of loads PRE'd");

static cl::opt<bool> EnablePRE("enable-pre",
                               cl::init(true), cl::Hidden);
static cl::opt<bool> EnableLoadPRE("enable-load-pre", cl::init(true));

// Maximum allowed recursion depth.
static cl::opt<uint32_t>
MaxRecurseDepth("max-recurse-depth", cl::Hidden, cl::init(1000), cl::ZeroOrMore,
                cl::desc("Max recurse depth (default = 1000)"));

//===----------------------------------------------------------------------===//
//                         ValueTable Class
//===----------------------------------------------------------------------===//

/// This class holds the mapping between values and value numbers.  It is used
/// as an efficient mechanism to determine the expression-wise equivalence of
/// two values.
namespace {
  struct Expression {
    uint32_t opcode;
    Type *type;
    SmallVector<uint32_t, 4> varargs;

    Expression(uint32_t o = ~2U) : opcode(o) { }

    bool operator==(const Expression &other) const {
      if (opcode != other.opcode)
        return false;
      if (opcode == ~0U || opcode == ~1U)
        return true;
      if (type != other.type)
        return false;
      if (varargs != other.varargs)
        return false;
      return true;
    }

    friend hash_code hash_value(const Expression &Value) {
      return hash_combine(Value.opcode, Value.type,
                          hash_combine_range(Value.varargs.begin(),
                                             Value.varargs.end()));
    }
  };

  class ValueTable {
    DenseMap<Value*, uint32_t> valueNumbering;
    DenseMap<Expression, uint32_t> expressionNumbering;
    AliasAnalysis *AA;
    MemoryDependenceAnalysis *MD;
    DominatorTree *DT;

    uint32_t nextValueNumber;

    Expression create_expression(Instruction* I);
    Expression create_cmp_expression(unsigned Opcode,
                                     CmpInst::Predicate Predicate,
                                     Value *LHS, Value *RHS);
    Expression create_extractvalue_expression(ExtractValueInst* EI);
    uint32_t lookup_or_add_call(CallInst* C);
  public:
    ValueTable() : nextValueNumber(1) { }
    uint32_t lookup_or_add(Value *V);
    uint32_t lookup(Value *V) const;
    // Added for LLVMBerry
    uint32_t lookup_safe(Value *V) const;
    uint32_t lookup_or_add_cmp(unsigned Opcode, CmpInst::Predicate Pred,
                               Value *LHS, Value *RHS);
    void add(Value *V, uint32_t num);
    void clear();
    void erase(Value *v);
    void setAliasAnalysis(AliasAnalysis* A) { AA = A; }
    AliasAnalysis *getAliasAnalysis() const { return AA; }
    void setMemDep(MemoryDependenceAnalysis* M) { MD = M; }
    void setDomTree(DominatorTree* D) { DT = D; }
    uint32_t getNextUnusedValueNumber() { return nextValueNumber; }
    void verifyRemoved(const Value *) const;
  };
}

namespace llvm {
template <> struct DenseMapInfo<Expression> {
  static inline Expression getEmptyKey() {
    return ~0U;
  }

  static inline Expression getTombstoneKey() {
    return ~1U;
  }

  static unsigned getHashValue(const Expression e) {
    using llvm::hash_value;
    return static_cast<unsigned>(hash_value(e));
  }
  static bool isEqual(const Expression &LHS, const Expression &RHS) {
    return LHS == RHS;
  }
};

}

//===----------------------------------------------------------------------===//
//                     ValueTable Internal Functions
//===----------------------------------------------------------------------===//

Expression ValueTable::create_expression(Instruction *I) {
  Expression e;
  e.type = I->getType();
  e.opcode = I->getOpcode();
  for (Instruction::op_iterator OI = I->op_begin(), OE = I->op_end();
       OI != OE; ++OI)
    e.varargs.push_back(lookup_or_add(*OI));
  if (I->isCommutative()) {
    // Ensure that commutative instructions that only differ by a permutation
    // of their operands get the same value number by sorting the operand value
    // numbers.  Since all commutative instructions have two operands it is more
    // efficient to sort by hand rather than using, say, std::sort.
    assert(I->getNumOperands() == 2 && "Unsupported commutative instruction!");
    if (e.varargs[0] > e.varargs[1])
      std::swap(e.varargs[0], e.varargs[1]);
  }

  if (CmpInst *C = dyn_cast<CmpInst>(I)) {
    // Sort the operand value numbers so x<y and y>x get the same value number.
    CmpInst::Predicate Predicate = C->getPredicate();
    if (e.varargs[0] > e.varargs[1]) {
      std::swap(e.varargs[0], e.varargs[1]);
      Predicate = CmpInst::getSwappedPredicate(Predicate);
    }
    e.opcode = (C->getOpcode() << 8) | Predicate;
  } else if (InsertValueInst *E = dyn_cast<InsertValueInst>(I)) {
    for (InsertValueInst::idx_iterator II = E->idx_begin(), IE = E->idx_end();
         II != IE; ++II)
      e.varargs.push_back(*II);
  }

  return e;
}

Expression ValueTable::create_cmp_expression(unsigned Opcode,
                                             CmpInst::Predicate Predicate,
                                             Value *LHS, Value *RHS) {
  assert((Opcode == Instruction::ICmp || Opcode == Instruction::FCmp) &&
         "Not a comparison!");
  Expression e;
  e.type = CmpInst::makeCmpResultType(LHS->getType());
  e.varargs.push_back(lookup_or_add(LHS));
  e.varargs.push_back(lookup_or_add(RHS));

  // Sort the operand value numbers so x<y and y>x get the same value number.
  if (e.varargs[0] > e.varargs[1]) {
    std::swap(e.varargs[0], e.varargs[1]);
    Predicate = CmpInst::getSwappedPredicate(Predicate);
  }
  e.opcode = (Opcode << 8) | Predicate;
  return e;
}

Expression ValueTable::create_extractvalue_expression(ExtractValueInst *EI) {
  assert(EI && "Not an ExtractValueInst?");
  Expression e;
  e.type = EI->getType();
  e.opcode = 0;

  IntrinsicInst *I = dyn_cast<IntrinsicInst>(EI->getAggregateOperand());
  if (I != nullptr && EI->getNumIndices() == 1 && *EI->idx_begin() == 0 ) {
    // EI might be an extract from one of our recognised intrinsics. If it
    // is we'll synthesize a semantically equivalent expression instead on
    // an extract value expression.
    switch (I->getIntrinsicID()) {
      case Intrinsic::sadd_with_overflow:
      case Intrinsic::uadd_with_overflow:
        e.opcode = Instruction::Add;
        break;
      case Intrinsic::ssub_with_overflow:
      case Intrinsic::usub_with_overflow:
        e.opcode = Instruction::Sub;
        break;
      case Intrinsic::smul_with_overflow:
      case Intrinsic::umul_with_overflow:
        e.opcode = Instruction::Mul;
        break;
      default:
        break;
    }

    if (e.opcode != 0) {
      // Intrinsic recognized. Grab its args to finish building the expression.
      assert(I->getNumArgOperands() == 2 &&
             "Expect two args for recognised intrinsics.");
      e.varargs.push_back(lookup_or_add(I->getArgOperand(0)));
      e.varargs.push_back(lookup_or_add(I->getArgOperand(1)));
      return e;
    }
  }

  // Not a recognised intrinsic. Fall back to producing an extract value
  // expression.
  e.opcode = EI->getOpcode();
  for (Instruction::op_iterator OI = EI->op_begin(), OE = EI->op_end();
       OI != OE; ++OI)
    e.varargs.push_back(lookup_or_add(*OI));

  for (ExtractValueInst::idx_iterator II = EI->idx_begin(), IE = EI->idx_end();
         II != IE; ++II)
    e.varargs.push_back(*II);

  return e;
}

//===----------------------------------------------------------------------===//
//                     ValueTable External Functions
//===----------------------------------------------------------------------===//

/// add - Insert a value into the table with a specified value number.
void ValueTable::add(Value *V, uint32_t num) {
  valueNumbering.insert(std::make_pair(V, num));
}

uint32_t ValueTable::lookup_or_add_call(CallInst *C) {
  if (AA->doesNotAccessMemory(C)) {
    Expression exp = create_expression(C);
    uint32_t &e = expressionNumbering[exp];
    if (!e) e = nextValueNumber++;
    valueNumbering[C] = e;
    return e;
  } else if (AA->onlyReadsMemory(C)) {
    Expression exp = create_expression(C);
    uint32_t &e = expressionNumbering[exp];
    if (!e) {
      e = nextValueNumber++;
      valueNumbering[C] = e;
      return e;
    }
    if (!MD) {
      e = nextValueNumber++;
      valueNumbering[C] = e;
      return e;
    }

    MemDepResult local_dep = MD->getDependency(C);

    if (!local_dep.isDef() && !local_dep.isNonLocal()) {
      valueNumbering[C] =  nextValueNumber;
      return nextValueNumber++;
    }

    if (local_dep.isDef()) {
      CallInst* local_cdep = cast<CallInst>(local_dep.getInst());

      if (local_cdep->getNumArgOperands() != C->getNumArgOperands()) {
        valueNumbering[C] = nextValueNumber;
        return nextValueNumber++;
      }

      for (unsigned i = 0, e = C->getNumArgOperands(); i < e; ++i) {
        uint32_t c_vn = lookup_or_add(C->getArgOperand(i));
        uint32_t cd_vn = lookup_or_add(local_cdep->getArgOperand(i));
        if (c_vn != cd_vn) {
          valueNumbering[C] = nextValueNumber;
          return nextValueNumber++;
        }
      }

      uint32_t v = lookup_or_add(local_cdep);
      valueNumbering[C] = v;
      return v;
    }

    // Non-local case.
    const MemoryDependenceAnalysis::NonLocalDepInfo &deps =
      MD->getNonLocalCallDependency(CallSite(C));
    // FIXME: Move the checking logic to MemDep!
    CallInst* cdep = nullptr;

    // Check to see if we have a single dominating call instruction that is
    // identical to C.
    for (unsigned i = 0, e = deps.size(); i != e; ++i) {
      const NonLocalDepEntry *I = &deps[i];
      if (I->getResult().isNonLocal())
        continue;

      // We don't handle non-definitions.  If we already have a call, reject
      // instruction dependencies.
      if (!I->getResult().isDef() || cdep != nullptr) {
        cdep = nullptr;
        break;
      }

      CallInst *NonLocalDepCall = dyn_cast<CallInst>(I->getResult().getInst());
      // FIXME: All duplicated with non-local case.
      if (NonLocalDepCall && DT->properlyDominates(I->getBB(), C->getParent())){
        cdep = NonLocalDepCall;
        continue;
      }

      cdep = nullptr;
      break;
    }

    if (!cdep) {
      valueNumbering[C] = nextValueNumber;
      return nextValueNumber++;
    }

    if (cdep->getNumArgOperands() != C->getNumArgOperands()) {
      valueNumbering[C] = nextValueNumber;
      return nextValueNumber++;
    }
    for (unsigned i = 0, e = C->getNumArgOperands(); i < e; ++i) {
      uint32_t c_vn = lookup_or_add(C->getArgOperand(i));
      uint32_t cd_vn = lookup_or_add(cdep->getArgOperand(i));
      if (c_vn != cd_vn) {
        valueNumbering[C] = nextValueNumber;
        return nextValueNumber++;
      }
    }

    uint32_t v = lookup_or_add(cdep);
    valueNumbering[C] = v;
    return v;

  } else {
    valueNumbering[C] = nextValueNumber;
    return nextValueNumber++;
  }
}

/// lookup_or_add - Returns the value number for the specified value, assigning
/// it a new number if it did not have one before.
uint32_t ValueTable::lookup_or_add(Value *V) {
  DenseMap<Value*, uint32_t>::iterator VI = valueNumbering.find(V);
  if (VI != valueNumbering.end())
    return VI->second;

  if (!isa<Instruction>(V)) {
    valueNumbering[V] = nextValueNumber;
    return nextValueNumber++;
  }

  Instruction* I = cast<Instruction>(V);
  Expression exp;
  switch (I->getOpcode()) {
    case Instruction::Call:
      return lookup_or_add_call(cast<CallInst>(I));
    case Instruction::Add:
    case Instruction::FAdd:
    case Instruction::Sub:
    case Instruction::FSub:
    case Instruction::Mul:
    case Instruction::FMul:
    case Instruction::UDiv:
    case Instruction::SDiv:
    case Instruction::FDiv:
    case Instruction::URem:
    case Instruction::SRem:
    case Instruction::FRem:
    case Instruction::Shl:
    case Instruction::LShr:
    case Instruction::AShr:
    case Instruction::And:
    case Instruction::Or:
    case Instruction::Xor:
    case Instruction::ICmp:
    case Instruction::FCmp:
    case Instruction::Trunc:
    case Instruction::ZExt:
    case Instruction::SExt:
    case Instruction::FPToUI:
    case Instruction::FPToSI:
    case Instruction::UIToFP:
    case Instruction::SIToFP:
    case Instruction::FPTrunc:
    case Instruction::FPExt:
    case Instruction::PtrToInt:
    case Instruction::IntToPtr:
    case Instruction::BitCast:
    case Instruction::Select:
    case Instruction::ExtractElement:
    case Instruction::InsertElement:
    case Instruction::ShuffleVector:
    case Instruction::InsertValue:
    case Instruction::GetElementPtr:
      exp = create_expression(I);
      break;
    case Instruction::ExtractValue:
      exp = create_extractvalue_expression(cast<ExtractValueInst>(I));
      break;
    default:
      valueNumbering[V] = nextValueNumber;
      return nextValueNumber++;
  }

  uint32_t& e = expressionNumbering[exp];
  if (!e) e = nextValueNumber++;
  valueNumbering[V] = e;
  return e;
}

/// Returns the value number of the specified value. Fails if
/// the value has not yet been numbered.
uint32_t ValueTable::lookup(Value *V) const {
  DenseMap<Value*, uint32_t>::const_iterator VI = valueNumbering.find(V);
  assert(VI != valueNumbering.end() && "Value not numbered?");
  return VI->second;
}

uint32_t ValueTable::lookup_safe(Value *V) const {
  DenseMap<Value*, uint32_t>::const_iterator VI = valueNumbering.find(V);
  if (VI == valueNumbering.end()) return 0;
  return VI->second;
}

/// Returns the value number of the given comparison,
/// assigning it a new number if it did not have one before.  Useful when
/// we deduced the result of a comparison, but don't immediately have an
/// instruction realizing that comparison to hand.
uint32_t ValueTable::lookup_or_add_cmp(unsigned Opcode,
                                       CmpInst::Predicate Predicate,
                                       Value *LHS, Value *RHS) {
  Expression exp = create_cmp_expression(Opcode, Predicate, LHS, RHS);
  uint32_t& e = expressionNumbering[exp];
  if (!e) e = nextValueNumber++;
  return e;
}

/// Remove all entries from the ValueTable.
void ValueTable::clear() {
  valueNumbering.clear();
  expressionNumbering.clear();
  nextValueNumber = 1;
}

/// Remove a value from the value numbering.
void ValueTable::erase(Value *V) {
  valueNumbering.erase(V);
}

/// verifyRemoved - Verify that the value is removed from all internal data
/// structures.
void ValueTable::verifyRemoved(const Value *V) const {
  for (DenseMap<Value*, uint32_t>::const_iterator
         I = valueNumbering.begin(), E = valueNumbering.end(); I != E; ++I) {
    assert(I->first != V && "Inst still occurs in value numbering map!");
  }
}

//===----------------------------------------------------------------------===//
//                                GVN Pass
//===----------------------------------------------------------------------===//

namespace {
  class GVN;
  struct AvailableValueInBlock {
    /// BB - The basic block in question.
    BasicBlock *BB;
    enum ValType {
      SimpleVal,  // A simple offsetted value that is accessed.
      LoadVal,    // A value produced by a load.
      MemIntrin,  // A memory intrinsic which is loaded from.
      UndefVal    // A UndefValue representing a value from dead block (which
                  // is not yet physically removed from the CFG). 
    };
  
    /// V - The value that is live out of the block.
    PointerIntPair<Value *, 2, ValType> Val;
  
    /// Offset - The byte offset in Val that is interesting for the load query.
    unsigned Offset;
  
    static AvailableValueInBlock get(BasicBlock *BB, Value *V,
                                     unsigned Offset = 0) {
      AvailableValueInBlock Res;
      Res.BB = BB;
      Res.Val.setPointer(V);
      Res.Val.setInt(SimpleVal);
      Res.Offset = Offset;
      return Res;
    }
  
    static AvailableValueInBlock getMI(BasicBlock *BB, MemIntrinsic *MI,
                                       unsigned Offset = 0) {
      AvailableValueInBlock Res;
      Res.BB = BB;
      Res.Val.setPointer(MI);
      Res.Val.setInt(MemIntrin);
      Res.Offset = Offset;
      return Res;
    }
  
    static AvailableValueInBlock getLoad(BasicBlock *BB, LoadInst *LI,
                                         unsigned Offset = 0) {
      AvailableValueInBlock Res;
      Res.BB = BB;
      Res.Val.setPointer(LI);
      Res.Val.setInt(LoadVal);
      Res.Offset = Offset;
      return Res;
    }

    static AvailableValueInBlock getUndef(BasicBlock *BB) {
      AvailableValueInBlock Res;
      Res.BB = BB;
      Res.Val.setPointer(nullptr);
      Res.Val.setInt(UndefVal);
      Res.Offset = 0;
      return Res;
    }

    bool isSimpleValue() const { return Val.getInt() == SimpleVal; }
    bool isCoercedLoadValue() const { return Val.getInt() == LoadVal; }
    bool isMemIntrinValue() const { return Val.getInt() == MemIntrin; }
    bool isUndefValue() const { return Val.getInt() == UndefVal; }
  
    Value *getSimpleValue() const {
      assert(isSimpleValue() && "Wrong accessor");
      return Val.getPointer();
    }
  
    LoadInst *getCoercedLoadValue() const {
      assert(isCoercedLoadValue() && "Wrong accessor");
      return cast<LoadInst>(Val.getPointer());
    }
  
    MemIntrinsic *getMemIntrinValue() const {
      assert(isMemIntrinValue() && "Wrong accessor");
      return cast<MemIntrinsic>(Val.getPointer());
    }
  
    /// Emit code into this block to adjust the value defined here to the
    /// specified type. This handles various coercion cases.
    Value *MaterializeAdjustedValue(LoadInst *LI, GVN &gvn) const;
  };

  class GVN : public FunctionPass {
    bool NoLoads;
    MemoryDependenceAnalysis *MD;
    DominatorTree *DT;
    const TargetLibraryInfo *TLI;
    AssumptionCache *AC;
    SetVector<BasicBlock *> DeadBlocks;

    ValueTable VN;

    /// A mapping from value numbers to lists of Value*'s that
    /// have that value number.  Use findLeader to query it.
    struct LeaderTableEntry {
      Value *Val;
      const BasicBlock *BB;
      LeaderTableEntry *Next;
    };
    DenseMap<uint32_t, LeaderTableEntry> LeaderTable;
    BumpPtrAllocator TableAllocator;

    SmallVector<Instruction*, 8> InstrsToErase;

    typedef SmallVector<NonLocalDepResult, 64> LoadDepVect;
    typedef SmallVector<AvailableValueInBlock, 64> AvailValInBlkVect;
    typedef SmallVector<BasicBlock*, 64> UnavailBlkVect;

  public:
    static char ID; // Pass identification, replacement for typeid
    explicit GVN(bool noloads = false)
        : FunctionPass(ID), NoLoads(noloads), MD(nullptr) {
      initializeGVNPass(*PassRegistry::getPassRegistry());
    }

    bool runOnFunction(Function &F) override;

    /// This removes the specified instruction from
    /// our various maps and marks it for deletion.
    void markInstructionForDeletion(Instruction *I) {
      VN.erase(I);
      InstrsToErase.push_back(I);
    }

    DominatorTree &getDominatorTree() const { return *DT; }
    AliasAnalysis *getAliasAnalysis() const { return VN.getAliasAnalysis(); }
    MemoryDependenceAnalysis &getMemDep() const { return *MD; }
  private:
    /// Push a new Value to the LeaderTable onto the list for its value number.
    void addToLeaderTable(uint32_t N, Value *V, const BasicBlock *BB) {
      LeaderTableEntry &Curr = LeaderTable[N];
      if (!Curr.Val) {
        Curr.Val = V;
        Curr.BB = BB;
        return;
      }

      LeaderTableEntry *Node = TableAllocator.Allocate<LeaderTableEntry>();
      Node->Val = V;
      Node->BB = BB;
      Node->Next = Curr.Next;
      Curr.Next = Node;
    }

    /// Scan the list of values corresponding to a given
    /// value number, and remove the given instruction if encountered.
    void removeFromLeaderTable(uint32_t N, Instruction *I, BasicBlock *BB) {
      LeaderTableEntry* Prev = nullptr;
      LeaderTableEntry* Curr = &LeaderTable[N];

      while (Curr && (Curr->Val != I || Curr->BB != BB)) {
        Prev = Curr;
        Curr = Curr->Next;
      }

      if (!Curr)
        return;

      if (Prev) {
        Prev->Next = Curr->Next;
      } else {
        if (!Curr->Next) {
          Curr->Val = nullptr;
          Curr->BB = nullptr;
        } else {
          LeaderTableEntry* Next = Curr->Next;
          Curr->Val = Next->Val;
          Curr->BB = Next->BB;
          Curr->Next = Next->Next;
        }
      }
    }

    // List of critical edges to be split between iterations.
    SmallVector<std::pair<TerminatorInst*, unsigned>, 4> toSplit;

    // This transformation requires dominator postdominator info
    void getAnalysisUsage(AnalysisUsage &AU) const override {
      AU.addRequired<AssumptionCacheTracker>();
      AU.addRequired<DominatorTreeWrapperPass>();
      AU.addRequired<TargetLibraryInfoWrapperPass>();
      if (!NoLoads)
        AU.addRequired<MemoryDependenceAnalysis>();
      AU.addRequired<AliasAnalysis>();

      AU.addPreserved<DominatorTreeWrapperPass>();
      AU.addPreserved<AliasAnalysis>();
    }


    // Helper fuctions of redundant load elimination 
    bool processLoad(LoadInst *L);
    bool processNonLocalLoad(LoadInst *L);
    void AnalyzeLoadAvailability(LoadInst *LI, LoadDepVect &Deps, 
                                 AvailValInBlkVect &ValuesPerBlock,
                                 UnavailBlkVect &UnavailableBlocks);
    bool PerformLoadPRE(LoadInst *LI, AvailValInBlkVect &ValuesPerBlock, 
                        UnavailBlkVect &UnavailableBlocks);

    // Other helper routines
    bool processInstruction(Instruction *I);
    bool processBlock(BasicBlock *BB);
    void dump(DenseMap<uint32_t, Value*> &d);
    bool iterateOnFunction(Function &F);
    bool performPRE(Function &F);
    bool performScalarPRE(Instruction *I);
    bool performScalarPREInsertion(Instruction *Instr, BasicBlock *Pred,
                                   unsigned int ValNo);
    Value *findLeader(const BasicBlock *BB, uint32_t num);
    void cleanupGlobalSets();
    void verifyRemoved(const Instruction *I) const;
    bool splitCriticalEdges();
    BasicBlock *splitCriticalEdges(BasicBlock *Pred, BasicBlock *Succ);
    bool propagateEquality(Value *LHS, Value *RHS, const BasicBlockEdge &Root);
    bool processFoldableCondBr(BranchInst *BI);
    void addDeadBlock(BasicBlock *BB);
    void assignValNumForDeadCode();
  };

  char GVN::ID = 0;
}

namespace {
class PREAnalysisResult {
public:
  bool PrevPRENotEnough;
  std::vector<std::pair<PHINode *, int>> PrevPRE;
  bool isFromNonLocalLoad;

  PREAnalysisResult(Instruction *CurInst, PHINode *PN) {
    llvmberry::PassDictionary &pdata = llvmberry::PassDictionary::GetInstance();
    isFromNonLocalLoad =
        pdata.get<llvmberry::ArgForGVNPRE>()->isFromNonLocalLoad;
    PrevPRENotEnough = false;
    std::vector<Value *> op_CurInst;
    unsigned numPredBlocks = 0;
    BasicBlock *PNBlock = PN->getParent();
    for (auto PI = pred_begin(PNBlock), PE = pred_end(PNBlock); PI != PE; ++PI)
      numPredBlocks++;

    for (auto OI = CurInst->op_begin(); OI != CurInst->op_end(); ++OI)
      op_CurInst.push_back(OI->get());

    for (Instruction &I : *PNBlock)
      if (PHINode *PI = dyn_cast<PHINode>(&I)) {
        int hit = 0;
        int idx = -1;
        for (unsigned i = 0, e = numPredBlocks; i != e; ++i) {
          BasicBlock *PB = PN->getIncomingBlock(i);
          Value *V = PN->getIncomingValue(i);
          dbgs() << "V is : " << *V << "\n";

          // it may be constant int... ?!
          if (Instruction *VI = dyn_cast<Instruction>(V)) {
            // if (isa<PHINode>(VI)) {
            //   hit++;
            //   continue;
            // }

            // It finds the first j that matches
            // VI may have same operand, such as VI = a + a, so there can be
            // many j
            // Anyhow, propagating only once && substituting only once may
            // sufficient
            for (int j = 0; j < VI->getNumOperands(); j++) {
              if (dyn_cast<Instruction>(VI->getOperand(j)) ==
                  PI->getIncomingValueForBlock(PB)) {
                hit++;
                // if matched, match to same idx
                if (idx == -1)
                  idx = j;
                else if (idx == j) {
                } else {
                  assert("idx is not equal to j" && false);
                }
              }
            }
          }
        }
        // if matched, all prev blocks match
        if (idx != -1) {
          if (hit == numPredBlocks) {
            PrevPRE.push_back(std::make_pair(PI, idx));
          } else {
            PrevPRENotEnough = true;
          }
        }
      }
  }
};

bool propagateInstrUntilBlockEnd(llvmberry::CoreHint &hints, Instruction *Inst,
                                 BasicBlock *PB) {
  std::string Inst_id = llvmberry::getVariable(*Inst);
  if (isa<PHINode>(Inst))
    assert("Phi should not occur here" && false);
  PROPAGATE(LESSDEF(RHS(Inst_id, Physical, SRC), VAR(Inst_id, Physical), SRC),
            BOUNDS(INSTPOS(SRC, Inst), llvmberry::TyPosition::make_end_of_block(
                                           llvmberry::Source, *PB)));
  PROPAGATE(LESSDEF(VAR(Inst_id, Physical), RHS(Inst_id, Physical, SRC), SRC),
            BOUNDS(INSTPOS(SRC, Inst), llvmberry::TyPosition::make_end_of_block(
                                           llvmberry::Source, *PB)));
  return true;
}

// [ INSN(CurInst) >= Var(Phi) ] in start_of_block(Phi->getParent())
void generateHintForPRE(Instruction *CurInst, PHINode *Phi) {
  // dbgs() << "CurInst : " << *CurInst << "\n";
  // dbgs() << "Phi : " << *Phi << "\n";
  BasicBlock *PhiBlock = Phi->getParent();
  std::string CurInst_id = llvmberry::getVariable(*CurInst);
  std::string Phi_id = llvmberry::getVariable(*Phi);
  PREAnalysisResult *PREAR = new PREAnalysisResult(CurInst, Phi);

  if (PREAR->PrevPRE.size() == 0) {
    llvmberry::ValidationUnit::GetInstance()
        ->intrude([&CurInst, &Phi, &PhiBlock, &CurInst_id, &Phi_id, &PREAR](
              llvmberry::ValidationUnit::Dictionary &data,
              llvmberry::CoreHint &hints) {
          if (isa<CallInst>(CurInst)) {
            hints.appendToDescription("CurInstIsCall");
            hints.setReturnCodeToAdmitted();
          }

          if (PREAR->isFromNonLocalLoad) {
            hints.appendToDescription("isFromNonLocalLoad");
            hints.setReturnCodeToAdmitted();
          }

          // For each pred block, propagate the chain of involved values until
          // the
          // end
          // of the pred block
          for (auto PI = pred_begin(PhiBlock), PE = pred_end(PhiBlock);
               PI != PE; ++PI) {
            BasicBlock *PB = *PI;
            Value *V = Phi->getIncomingValueForBlock(PB);

            auto PBPhiPos = llvmberry::TyPosition::make(
                SRC, PhiBlock->getName(), PB->getName());

            hints.setDescription(
                (hints.getDescription() + "\nV is: " + (*V).getName()).str());
            hints.appendToDescription("CurInst is: " +
                                      ((*CurInst).getName()).str());

            if (Instruction *VI = dyn_cast<Instruction>(V)) {
              std::string VI_id = llvmberry::getVariable(*VI);
              hints.appendToDescription("VI_id is: " + VI_id);
              hints.appendToDescription("VI's getName is: " +
                                        ((*VI).getName()).str());

              Instruction *CurInstInPB = llvmberry::getPHIResolved(CurInst, PB);

              // Somehow get [ INSN(CurInstInPB) >= Var(VI) ] in block(Phi,
              // VPHI)
              if (PHINode *VPHI = dyn_cast<PHINode>(V)) {
                // Somehow get [ INSN(CurInstInPB) >= Var(VI) ] in
                // start_of_block(VPHI)
                generateHintForPRE(CurInstInPB, VPHI);

                // Propagate [ INSN(CurInstInPB) >= VAR(VI) ]
                PROPAGATE(
                    LESSDEF(INSN(*CurInstInPB), VAR(VI_id, Physical), SRC),
                    BOUNDS(llvmberry::TyPosition::make_start_of_block(
                               llvmberry::Source, llvmberry::getBasicBlockIndex(
                                                      VPHI->getParent())),
                           PBPhiPos));
              }
              // Somehow get [ INSN(CurInstInPB) >= Var(VI) ] in block(Phi,
              // VPHI)
              else {
                // Propagate [ RHS(VI) >= VAR(VI) ]
                PROPAGATE(LESSDEF(RHS(VI_id, Physical, SRC),
                                  VAR(VI_id, Physical), SRC),
                          BOUNDS(INSTPOS(SRC, VI), PBPhiPos));

                // Somehow get [ INSN(CurInstInPBInPB) >= Var(VI) ]
                // Expect INSN(CurInstInPBInPB) is already Var(VI)

                if (false) {
                  // Propagate [ RHS(VI) <=> VAR(VI) ]
                propagateInstrUntilBlockEnd(hints, VI, PB);

                // Transitivity [ Var(VI) >= Var(VI)p >= Var(Phi) ]
                // Currently, assume Rhs(VI) = Rhs(CurInstInPB)
                // TODO: difference btw BB->getName() and
                // llvmberry::getBasicBlockIndex?
                INFRULE(PBPhiPos,
                        llvmberry::ConsTransitivity::make(
                            VAR(VI_id, Physical), VAR(VI_id, Previous),
                            VAR(Phi_id, Physical)));

                // Transitivity [ Rhs(VI) >= Var(VI) >= Var(Phi) ]
                // Currently, assume Rhs(VI) = Rhs(CurInstInPB)
                // TODO: difference btw BB->getName() and
                // llvmberry::getBasicBlockIndex?
                INFRULE(PBPhiPos,
                        llvmberry::ConsTransitivity::make(
                            RHS(VI_id, Physical, SRC), VAR(VI_id, Physical),
                            VAR(Phi_id, Physical)));
                }

                bool inboundsRemovalOccured = false;
                if (auto *CurInstInPBGEP =
                        dyn_cast<GetElementPtrInst>(CurInstInPB)) {
                  if (auto *VIGEP = dyn_cast<GetElementPtrInst>(VI)) {
                    if (!CurInstInPBGEP->isInBounds() && VIGEP->isInBounds()) {
                      hints.appendToDescription("gep removal - bug");
                      hints.setReturnCodeToAdmitted();
                      // hints.setReturnCodeToFail();
                      return;
                    }
                    if (CurInstInPBGEP->isInBounds() && !VIGEP->isInBounds())
                      inboundsRemovalOccured = true;
                  } else
                    assert(false && "This cannot occur");
                }

                if (inboundsRemovalOccured) {
                  // CurInstInPB has inbounds
                  // This VI does not have inbounds

                  // GEP Inbounds Remove [ INSN(CurInstInPB) >= INSN(VI) ]
                  INFRULE(PBPhiPos,
                          llvmberry::ConsGepInboundsRemove::make(INSN(*VI)));
                  // Transitivity [ INSN(CurInstInPB) >= INSN(VI) >= Var(Phi) ]
                  INFRULE(PBPhiPos, llvmberry::ConsTransitivity::make(
                                        INSN(*CurInstInPB), INSN(*VI),
                                        VAR(VI_id, Physical)));
                }
              }

              // Transitivity [ Var(VI) >= Var(VI)p >= Var(Phi) ]
              INFRULE(PBPhiPos, llvmberry::ConsTransitivity::make(
                                    VAR(VI_id, Physical), VAR(VI_id, Previous),
                                    VAR(Phi_id, Physical)));

              // Transitivity [ INSN(CurInstInPB) >= Var(VI) >= Var(Phi) ]
              INFRULE(PBPhiPos, llvmberry::ConsTransitivity::make(
                                    INSN(*CurInstInPB), VAR(VI_id, Physical),
                                    VAR(Phi_id, Physical)));

              // Transitivity [ INSN(CurInst) <=> INSN(CurInstInPB) ]
              llvmberry::generateHintForPHIResolved(CurInst, PB, SRC);

              // Transitivity [ INSN(CurInst) >= INSN(CurInstInPB) >= Var(Phi)
              // ]
              INFRULE(PBPhiPos, llvmberry::ConsTransitivity::make(
                                    INSN(*CurInst), INSN(*CurInstInPB),
                                    VAR(Phi_id, Physical)));

              delete CurInstInPB;
            } else {
              // If repl is not an instruction, then it's always from
              // propagateEquality.
              const BasicBlock *BBSucc =
                  llvmberry::PassDictionary::GetInstance()
                      .get<llvmberry::ArgForGVNPRE>()
                      ->prevLeaderBBs[PB];
              assert(BBSucc && "Expect BBSucc to exist");
              const BasicBlock *BBPred = BBSucc->getSinglePredecessor();
              assert(BBPred &&
                     "Expect it to be introduced from propagateEquality, and "
                     "it checks "
                     "RootDominatesEnd, meaning it has single predecessor");

              hints.appendToDescription("V Is Not Instruction");
              hints.setReturnCodeToFail();
            }
          }
          });
  } else {
    llvmberry::ValidationUnit::GetInstance()
        ->intrude([&CurInst, &Phi, &CurInst_id, &PhiBlock, &Phi_id, &PREAR](
              llvmberry::ValidationUnit::Dictionary &data,
              llvmberry::CoreHint &hints) {
      if (isa<CallInst>(CurInst)) {
        hints.appendToDescription("CurInstIsCall");
        hints.setReturnCodeToAdmitted();
      }

      if (PREAR->isFromNonLocalLoad) {
        hints.appendToDescription("isFromNonLocalLoad");
        hints.setReturnCodeToAdmitted();
      }
      dbgs() << "CurInst : " << *CurInst << "\n";
      dbgs() << "Phi : " << *Phi << "\n";
      // if is same for all, it does not involve previous PRE and just works
      // it is treated below
      if (PREAR->PrevPRENotEnough) {
        hints.appendToDescription("PrevPRENotEnough");
        hints.setReturnCodeToFail();
        return;
      }
      std::vector<std::pair<PHINode *, int>> PrevPRE = PREAR->PrevPRE;
      hints.appendToDescription("CurInst is: " + ((*CurInst).getName()).str());

      for (auto PI = pred_begin(PhiBlock), PE = pred_end(PhiBlock); PI != PE;
           ++PI) {
        BasicBlock *PB = *PI;
        Value *V = Phi->getIncomingValueForBlock(PB);

        if (!isa<Instruction>(V)) {
          // constant int occurs... How can constant int get value number???
          hints.appendToDescription("V not instruction");
          hints.setReturnCodeToFail();
          return;
        }

        // TODO cleanse Somehow get in comment
        // TODO change "Var" into "VAR" in comment
        // TODO make TyPosition::make() blah that used many time into variable

        Instruction *VI = dyn_cast<Instruction>(V);
        std::string VI_id = llvmberry::getVariable(*VI);
        hints.appendToDescription("VI_id is: " + VI_id);

        dbgs() << "CurInst: " << *CurInst << "\n";
        dbgs() << "PB: " << *PB << "\n";
        Instruction *CurInstInPB = llvmberry::getPHIResolved(CurInst, PB);

        // Somehow get [ INSN(CurInstInPB) >= Var(VI) ] in block(Phi, VPHI)
        if (PHINode *VPHI = dyn_cast<PHINode>(V)) {
          assert("VPHI case should not occur" && false);
          // // Somehow get [ INSN(CurInstInPB) >= Var(VI) ] in
          // // start_of_block(VPHI)
          // generateHintForPRE(CurInstInPB, VPHI);

          // // Propagate [ INSN(CurInstInPB) >= VAR(VI) ]
          // PROPAGATE(LESSDEF(INSN(*CurInstInPB), VAR(VI_id, Physical), SRC),
          //           BOUNDS(llvmberry::TyPosition::make_start_of_block(
          //                      llvmberry::Source,
          // llvmberry::getBasicBlockIndex(
          //                                             VPHI->getParent())),
          //                  llvmberry::TyPosition::make(SRC,
          // PhiBlock->getName(),
          //                                              PB->getName())));
        }
        // Somehow get [ INSN(CurInstInPB) >= Var(VI) ] in block(Phi, VI)
        else {
          // TODO if it is not isSameForAll, and PrevPRE is empty, what does
          // this
          // mean..?
          // if (!PrevPRE.size())
          //   break;

          // Propagate [ RHS(VI) >= VAR(VI) ]
          PROPAGATE(
              LESSDEF(RHS(VI_id, Physical, SRC), VAR(VI_id, Physical), SRC),
              BOUNDS(INSTPOS(SRC, VI), llvmberry::TyPosition::make_end_of_block(
                                           llvmberry::Source, *PB)));

          Instruction *VI_evolving = (*VI).clone();

          // Somehow get [ INSN(CurInstInPB) >= Var(VI) ]
          for (auto k : PrevPRE) {
            // dbgs() << "VI_evolving : " << *VI_evolving << "\n";
            PHINode *PrevPhi = k.first;
            dbgs() << "PrevPhi " << *PrevPhi << "\n";
            std::string PrevPhi_id = llvmberry::getVariable(*PrevPhi);
            int idx = k.second;

            Instruction *VI_op = dyn_cast<Instruction>((*VI).getOperand(idx));
            std::string VI_op_id = llvmberry::getVariable(*VI_op);

            // Transitivity [ VAR(PrevPhi) >= VAR(VI_op)p >= Var(VI_op) ]
            INFRULE(llvmberry::TyPosition::make(SRC, PhiBlock->getName(),
                                                PB->getName()),
                    llvmberry::ConsTransitivity::make(VAR(PrevPhi_id, Physical),
                                                      VAR(VI_op_id, Previous),
                                                      VAR(VI_op_id, Physical)));

            // dbgs() << "VI_op : " << *VI_op << "\n";
            // dbgs() << "VI_evolving : " << *VI_evolving << "\n";

            // SubstituteRev [ VI_evolving_next >= VI_evolving ]
            // VI_evolving_next = VI_evolving[VI_op := PrevPhi]
            INFRULE(llvmberry::TyPosition::make(SRC, (*PhiBlock).getName(),
                                                (*PB).getName()),
                    llvmberry::ConsSubstituteRev::make(
                        REGISTER(VI_op_id, Physical), VAL(PrevPhi, Physical),
                        llvmberry::ConsInsn::make(*VI_evolving)));

            Instruction *VI_evolving_next = (*VI_evolving).clone();
            (*VI_evolving_next).setOperand(idx, PrevPhi);

            // Transitivity [ INSN(VI_evolving_next) >= INSN(VI_evolving) >=
            // Var(VI) ]

            // At first, INSN(VI_evolving) = RHS(VI) >= Var(VI)
            // Next, recursively
            INFRULE(llvmberry::TyPosition::make(SRC, PhiBlock->getName(),
                                                PB->getName()),
                    llvmberry::ConsTransitivity::make(INSN(*VI_evolving_next),
                                                      INSN(*VI_evolving),
                                                      VAR(VI_id, Physical)));

            delete VI_evolving;
            VI_evolving = VI_evolving_next;
          }
          delete VI_evolving;
        }

        // Transitivity [ Var(VI) >= Var(VI)p >= Var(Phi) ]
        INFRULE(llvmberry::TyPosition::make(SRC, PhiBlock->getName(),
                                            PB->getName()),
                llvmberry::ConsTransitivity::make(VAR(VI_id, Physical),
                                                  VAR(VI_id, Previous),
                                                  VAR(Phi_id, Physical)));

        // Transitivity [ INSN(CurInstInPB) >= Var(VI) >= Var(Phi) ]
        INFRULE(llvmberry::TyPosition::make(SRC, PhiBlock->getName(),
                                            PB->getName()),
                llvmberry::ConsTransitivity::make(INSN(*CurInstInPB),
                                                  VAR(VI_id, Physical),
                                                  VAR(Phi_id, Physical)));

        llvmberry::generateHintForPHIResolved(CurInst, PB, SRC);

        // Transitivity [ INSN(CurInst) >= INSN(CurInstInPB) >= Var(Phi) ]
        INFRULE(llvmberry::TyPosition::make(SRC, PhiBlock->getName(),
                                            PB->getName()),
                llvmberry::ConsTransitivity::make(
                    INSN(*CurInst), INSN(*CurInstInPB), VAR(Phi_id, Physical)));

        delete CurInstInPB;
      }

      return;
          });
  }
}
}

// The public interface to this file...
FunctionPass *llvm::createGVNPass(bool NoLoads) {
  return new GVN(NoLoads);
}

INITIALIZE_PASS_BEGIN(GVN, "gvn", "Global Value Numbering", false, false)
INITIALIZE_PASS_DEPENDENCY(AssumptionCacheTracker)
INITIALIZE_PASS_DEPENDENCY(MemoryDependenceAnalysis)
INITIALIZE_PASS_DEPENDENCY(DominatorTreeWrapperPass)
INITIALIZE_PASS_DEPENDENCY(TargetLibraryInfoWrapperPass)
INITIALIZE_AG_DEPENDENCY(AliasAnalysis)
INITIALIZE_PASS_END(GVN, "gvn", "Global Value Numbering", false, false)

#if !defined(NDEBUG) || defined(LLVM_ENABLE_DUMP)
void GVN::dump(DenseMap<uint32_t, Value*>& d) {
  errs() << "{\n";
  for (DenseMap<uint32_t, Value*>::iterator I = d.begin(),
       E = d.end(); I != E; ++I) {
      errs() << I->first << "\n";
      I->second->dump();
  }
  errs() << "}\n";
}
#endif

// Start LLVMBerry hint generation code
bool is_rhs_available(const Instruction *I) {
  if (isa<CallInst>(I) || isa<PHINode>(I) ||
      isa<TerminatorInst>(I) || isa<AllocaInst>(I))
    // TODO: more instructions can be added
    return false;
  else
    return true;
}

// Check whether two values have the same VN.
// This is necessary for the case when we should use VN.lookup_or_add_cmp
// instead of VN.lookup not to alter the VN table, for cmp-neg-pred case.
bool is_same_vn(ValueTable &VN, Value *V1, Value *V2, bool isV2Conc) {
  uint32_t vn_V2;

  if (Instruction *I2 = dyn_cast<Instruction>(V2)) {
    if (isV2Conc)
      vn_V2 = VN.lookup_safe(I2);
    else if (CmpInst *CI2 = dyn_cast<CmpInst>(I2))
      vn_V2 = VN.lookup_or_add_cmp(CI2->getOpcode(), CI2->getPredicate(),
                                   CI2->getOperand(0), CI2->getOperand(1));
    else
      assert(false && "GVN is_same_vn: Not concrete but not CmpInst");
  } else
    vn_V2 = VN.lookup_safe(V2);

  return (VN.lookup_safe(V1) == vn_V2);
}

// // Compare Value Vectors' VN. If each pair has the identical VN, return true.
// bool same_ops_vn(ValueTable &VN, SmallVector<Value *, 4> ops1,
//                  SmallVector<Value *, 4> ops2, int numOps) {
//   for (int i = 0; i < numOps; ++i)
//     if (VN.lookup(ops1[i]) != VN.lookup(ops2[i]))
//       return false;
//   return true;
// }

// Generate [ I1 >= I2 ] at POS if I1 and I2 have the same VN.
// If I2 is not concrete (clonned during hintgen),
// then just produce [ I1 >= exp(I2) ] at POS.
bool hintgen_same_vn(llvmberry::CoreHint &hints, ValueTable &VN,
                     Instruction *I1, Instruction *I2, bool isI2Conc,
                     Instruction *POS) {
  std::string id_I1 = llvmberry::getVariable(*I1),
              id_I2 = llvmberry::getVariable(*I2);

  if (I1 == I2) {
    // TODO: Below not needed for now. Let's see if this is unnecessary.
    // Currently just returning true is OK.
    if (POS != I1)
      PROPAGATE(LESSDEF(VAR(id_I1, Physical), RHS(id_I1, Physical, SRC), SRC),
                BOUNDS(INSTPOS(SRC, I1), INSTPOS(SRC, POS)));
    return true;
  }

  if (!is_same_vn(VN, I1, I2, isI2Conc)) {
    hints.appendToDescription("GVN hintgen_same_vn: is_same_vn I1 I2 failed.");
    return false;
  }

  // Insts should have the same opcode. Otherwise it's PHI-related case, which
  // is not covered yet.

  if (I1->getOpcode() != I2->getOpcode()) {
    if (isa<PHINode>(I1) || isa<PHINode>(I2)) {
      hints.appendToDescription("GVN hintgen_same_vn: PRE & GVN combined case - not handled yet.");
      // Return true since we should do this later, but now validation will fail
      // since we don't generate hint here.
      return true;
    }
    assert(
        false &&
        "GVN hintgen_same_vn: Same VN but not same Opcode and neither is PHI?");
  }

  if (!is_rhs_available(I1) || !is_rhs_available(I2)) {
    hints.appendToDescription("GVN hintgen_same_vn: rhs of I1 or I2 not available");
    return false;
  }

  // Now we know I1 and I2 have the same VN.
  assert(
      I1->getOpcode() == I2->getOpcode() &&
      I1->getNumOperands() == I2->getNumOperands() &&
      "GVN hintgen_same_vn: Insts with same vn should have the same opcode and "
      "num of operands!");

  SmallVector<Value *, 4> ops1, ops2;
  int numOps = I1->getNumOperands();

  for (auto OI = I1->op_begin(), OE = I1->op_end(); OI != OE; ++OI)
    ops1.push_back(*OI);
  for (auto OI = I2->op_begin(), OE = I2->op_end(); OI != OE; ++OI)
    ops2.push_back(*OI);

  // Propagate [ id(I1) >= exp(I1) ] at POS.
  if (I1 != POS)
    PROPAGATE(LESSDEF(VAR(id_I1, Physical), RHS(id_I1, Physical, SRC), SRC),
              BOUNDS(INSTPOS(SRC, I1), INSTPOS(SRC, POS)));

  Instruction *Ii = I1->clone();

  // Check swapping to make [ id(I1) >= exp(Ii) ]
  if (isa<CmpInst>(I1) || I1->isCommutative()) {
    assert(numOps == 2 && "GVN same_vn: commutative but not 2 operands!");

    if ((!is_same_vn(VN, ops1[0], ops2[0], true)) &&
        is_same_vn(VN, ops1[0], ops2[1], true) &&
        is_same_vn(VN, ops1[1], ops2[0], true)) {

      if (isa<ICmpInst>(I1)) {
        INFRULE(INSTPOS(SRC, POS),
                llvmberry::ConsIcmpSwapOperands::make(*cast<ICmpInst>(I1)));
      } else if (isa<FCmpInst>(I1)) {
        assert(false && "GVN same_vn: FCmp case not handled yet.");
      } else if (BinaryOperator *BI1 = dyn_cast<BinaryOperator>(I1)) {
        llvmberry::applyCommutativity(POS, BI1, SRC);
      }
      std::swap(ops1[0], ops1[1]);
      Ii->setOperand(0, ops1[0]);
      Ii->setOperand(1, ops1[1]);
    }
  }

  // If e_I1 is gep inb and e_I2 isn't, remove inbounds
  // to get [ gep inb .. >= gep .. ] and then
  // transitivity [ id(I1) >= gep inb .. >= gep .. ]
  if (GetElementPtrInst *GEP_I1 = dyn_cast<GetElementPtrInst>(I1)) {
    GetElementPtrInst *GEP_I2 = cast<GetElementPtrInst>(I2);
    if (GEP_I1->isInBounds() && !GEP_I2->isInBounds()) {
      INFRULE(INSTPOS(SRC, POS),
              llvmberry::ConsGepInboundsRemove::make(INSN(*GEP_I1)));
      GetElementPtrInst *GEP_Ii = cast<GetElementPtrInst>(Ii);
      GEP_Ii->setIsInBounds(false);
      auto gepi_expr = INSN(*Ii);

      INFRULE(INSTPOS(SRC, POS),
              llvmberry::ConsTransitivity::make(
                  VAR(id_I1, Physical), RHS(id_I1, Physical, SRC), gepi_expr));
    }
  }

  // From [ id(I1) >= exp(Ii) ], produce [ id(I1) >= exp(I2) ].
  for (unsigned i = 0; i < I1->getNumOperands(); ++i) {
    if (ops1[i] == ops2[i]) continue;
    assert(isa<Instruction>(ops1[i]) && isa<Instruction>(ops2[i]) &&
           "GVN same_vn: I1 and I2's operands are not Instructions but "
           "also not the same.");

    Instruction *I_ops1 = cast<Instruction>(ops1[i]);
    Instruction *I_ops2 = cast<Instruction>(ops2[i]);

    std::string id_I_ops1 = llvmberry::getVariable(*I_ops1),
                id_I_ops2 = llvmberry::getVariable(*I_ops2);

    if (!is_same_vn(VN, I_ops1, I_ops2, true))
      hints.appendToDescription("GVN hintgen_same_vn: I_ops1 and I_ops2 have different VNs.");
    // assert(is_same_vn(VN, I_ops1, I_ops2, true) &&
    //        "GVN same_vn: I_ops1 and I_ops2 have different VNs.");

    // This generates [ id(I_ops1) >= exp(I_ops2) ] at POS.
    bool hintgen = hintgen_same_vn(hints, VN, I_ops1, I_ops2, true, POS);
    if (!hintgen) {
      hints.appendToDescription("GVN same_vn: Failed to generate hint for operands.");
      delete Ii;
      return false;
    }

    // Propagate [ exp(I_ops2) >= id(I_ops2) ] until POS.
    // And trasitivity [ id(I_ops1) >= exp(I_ops2) >= id(I_ops2) ] at POS.
    PROPAGATE(LESSDEF(RHS(id_I_ops2, Physical, SRC), VAR(id_I_ops2, Physical), SRC),
                BOUNDS(INSTPOS(SRC, I_ops2), INSTPOS(SRC, POS)));
    INFRULE(INSTPOS(SRC, POS),
            llvmberry::ConsTransitivity::make(VAR(id_I_ops1, Physical),
                                              RHS(id_I_ops2, Physical, SRC),
                                              VAR(id_I_ops2, Physical)));

    auto prev_expr =
        llvmberry::ConsInsn::make(llvmberry::TyInstruction::make(*Ii));
    Ii->setOperand(i, ops2[i]);
    auto next_expr =
        llvmberry::ConsInsn::make(llvmberry::TyInstruction::make(*Ii));

    // ReplaceRhs to get [ id(I1) >= exp(Ii_new) ]
    INFRULE(INSTPOS(SRC, POS),
            llvmberry::ConsReplaceRhs::make(
                REGISTER(id_I_ops1, Physical), VAL(I_ops2, Physical),
                VAR(id_I1, Physical), prev_expr, next_expr));
  }

  if (isI2Conc) {

    // PROPAGATE [ exp(I2) >= I2 ].
    PROPAGATE(LESSDEF(RHS(id_I2, Physical, SRC), VAR(id_I2, Physical), SRC),
              BOUNDS(INSTPOS(SRC, I2), INSTPOS(SRC, POS)));

    // Transitivity [ I1 >= exp(I2) >= I2 ].
    INFRULE(INSTPOS(SRC, POS),
            llvmberry::ConsTransitivity::make(VAR(id_I1, Physical),
                                              RHS(id_I2, Physical, SRC),
                                              VAR(id_I2, Physical)));
  }

  delete Ii;

  return true;
}

// Generate [ I1 >= I2 ] at POS, if I1 and I2 have the same VN.
// Return true if successful.
// (isI2Conc = true) when I2 is concrete. (I2 exists in the IR code.)
// Otherwise, I2 is a clone produced during hint generation.

// // TODO: fill this
// bool hintgen_same_vn(llvmberry::CoreHint &hints, ValueTable &VN,
//                      Instruction *I1, Instruction *I2, bool isI2Conc,
//                      Instruction *POS) {
//   return true;
// }

// new version of hintgen_prop_eq (will replace old v)
// Search space to generate [ I >= repl ] at I,
// from the assumption [ A = B ] at begin(BB_succ).
// Note that repl is never Inst in this case.
// A is always greater than B in this lexicographic order: (Inst >= Arg >= Const) * (VN value).
// A can be a clone produced during hint generation. In some of those cases it doesn't have a VN.
bool hintgen_propeq(llvmberry::CoreHint &hints, ValueTable &VN,
                    Value *A,
                        Value *B, Instruction *I, Value *repl,
                        const BasicBlock *BB, const BasicBlock *BB_succ) {
  // GVN::propagateEquality doesn't propagate <const> == <const>
  if (isa<Constant>(A) && isa<Constant>(B))
    assert(false && "GVN propeq: Both A and B are constant!");

  // Check order of A and B.
  assert((isa<Instruction>(A) || isa<Constant>(B) ||
          (isa<Argument>(A) && isa<Argument>(B))) &&
         "GVN propeq: Unexpected pattern of A and B.");

  // But useful hint is generated only when A is Inst.
  if (!isa<Instruction>(A))
    return false;

  Instruction *I_A = cast<Instruction>(A);
  std::string id_I_A = llvmberry::getVariable(*I_A);
  std::string id_I = llvmberry::getVariable(*I);

  // Base case
  if (B == repl && is_same_vn(VN, I, I_A, true)) {
    // // Propagate [ A >= B (=repl) ] from begin(BB_succ) to I.
    auto repl_expr = llvmberry::TyExpr::make(*repl);
    PROPAGATE(LESSDEF(VAR(id_I_A, Physical), repl_expr, SRC),
              BOUNDS(llvmberry::TyPosition::make_start_of_block(
                         SRC, llvmberry::getBasicBlockIndex(BB_succ)),
                     INSTPOS(SRC, I)));

    // same_vn generates [ I >= I_A ] at I.
    bool hintgen = hintgen_same_vn(hints, VN, I, I_A, true, I);
    if (!hintgen) {
      hints.appendToDescription("GVN propeq: hintgen_same_vn failed.");
      return false;
    }
    // assert(hintgen && "GVN propeq: Base case same_vn failed!");

    // Transitivity [ id(I) >= exp(I_A) >= exp(B) (=repl) ] at I.
    INFRULE(INSTPOS(SRC, I),
            llvmberry::ConsTransitivity::make(
                VAR(id_I, Physical), VAR(id_I_A, Physical), repl_expr));

    return true;
  }

  // B should be boolean. Otherwise, return false since this way is stuck.
  ConstantInt *TrueVal = ConstantInt::getTrue(BB->getContext());
  ConstantInt *FalseVal = ConstantInt::getFalse(BB->getContext());
  bool Bval;

  if (TrueVal == B)
    Bval = true;
  else if (FalseVal == B)
    Bval = false;
  else
    return false;

  std::shared_ptr<llvmberry::TyPosition> pos_BB_infr =
      llvmberry::TyPosition::make(SRC, BB_succ->getName(), BB->getName());
  std::shared_ptr<llvmberry::TyPosition> pos_BB_prop =
      llvmberry::TyPosition::make_end_of_block(SRC, *BB);
  std::shared_ptr<llvmberry::TyExpr> expr_true =
      llvmberry::TyExpr::make(*TrueVal);
  std::shared_ptr<llvmberry::TyExpr> expr_false =
      llvmberry::TyExpr::make(*FalseVal);

  // And/Or case
  if ((I_A->getOpcode() == Instruction::And) && Bval) {
    // And case
    // Assumption: [ T >= A ] at begin(BB_succ)
    // - Propagate [ A >= X && Y ] from A to begin(BB_succ)
    // - Transitivity [ T >= X && Y ] at begin(BB_succ)
    // - Infer [ X = T, Y = T ] at begin(BB_succ).
    // Then for each x in {X, Y},
    // - Call hintgen_propeq(x).

    PROPAGATE(LESSDEF(VAR(id_I_A, Physical), RHS(id_I_A, Physical, SRC), SRC),
              BOUNDS(INSTPOS(SRC, I_A), pos_BB_prop));

    INFRULE(pos_BB_infr,
            llvmberry::ConsTransitivity::make(expr_true,
                                              VAR(id_I_A, Physical),
                                              RHS(id_I_A, Physical, SRC)));

    INFRULE(pos_BB_infr, llvmberry::ConsAndTrueBool::make(
                             llvmberry::TyValue::make(*I_A->getOperand(0)),
                             llvmberry::TyValue::make(*I_A->getOperand(1))));

    for (int i = 0; i <= 1; ++i) {
      if (isa<Instruction>(I_A->getOperand(i)))
        if (hintgen_propeq(hints, VN, I_A->getOperand(i), TrueVal, I,
                           repl, BB, BB_succ))
          return true;
    }
    // If both x failed, this way is stuck.
    return false;
  } else if ((I_A->getOpcode() == Instruction::Or) && !Bval) {
    // Or case
    // Assumption: [ F >= A ] at begin(BB_succ)
    // - Propagate [ A >= X || Y ] from A to begin(BB_succ)
    // - Transitivity [ F >= X || Y ] at begin(BB_succ)
    // - Infer [ X = F, Y = F ] at begin(BB_succ).
    // Then for each x in {X, Y},
    // - Call hintgen_propeq(x).

    PROPAGATE(LESSDEF(VAR(id_I_A, Physical), RHS(id_I_A, Physical, SRC), SRC),
              BOUNDS(INSTPOS(SRC, I_A), pos_BB_prop));

    INFRULE(pos_BB_infr,
            llvmberry::ConsTransitivity::make(expr_false,
                                              VAR(id_I_A, Physical),
                                              RHS(id_I_A, Physical, SRC)));

    INFRULE(pos_BB_infr, llvmberry::ConsOrFalse::make(
                             llvmberry::TyValue::make(*I_A->getOperand(0)),
                             llvmberry::TyValue::make(*I_A->getOperand(1)),
                             llvmberry::ConsSize::make(1)));

    for (int i = 0; i <= 1; ++i) {
      if (isa<Instruction>(I_A->getOperand(i)))
        if (hintgen_propeq(hints, VN, I_A->getOperand(i), FalseVal, I,
                           repl, BB, BB_succ))
          return true;
    }
    // If both x failed, this way is stuck.
    return false;
  }
  // And/Or case finished.

  // Cmp case
  assert(!isa<FCmpInst>(I_A) && "GVN gen_propeq_hint: FCmp case not yet covered.");

  if (ICmpInst *CI_A = dyn_cast<ICmpInst>(I_A)) {
    // We additinally handle the ICmp EQ/NE case.
    if (((CI_A->getPredicate() == CmpInst::ICMP_EQ) && Bval) ||
        ((CI_A->getPredicate() == CmpInst::ICMP_NE) && !Bval)) {
      Value *X = CI_A->getOperand(0), *Y = CI_A->getOperand(1);

      if (isa<Instruction>(X) != isa<Instruction>(Y)) {
        // Let X be Instruction.
        if (isa<Instruction>(Y))
          std::swap(X, Y);

        // Assumption: [ (T/F) >= A ] at begin(BB_succ)
        // - Propagate [ A >= icmp (eq/ne) X Y ] from A to begin(BB_succ)
        // - Transitivity [ (T/F) >= icmp (eq/ne) X Y ] at begin(BB_succ)
        // - Infer [ X = Y ]
        // - If only one of them is instruction Z, say another one W,
        //   Call propeq(Z, W)

        PROPAGATE(LESSDEF(VAR(id_I_A, Physical), RHS(id_I_A, Physical, SRC), SRC),
                  BOUNDS(INSTPOS(SRC, I_A), pos_BB_prop));
        INFRULE(pos_BB_infr,
                llvmberry::ConsTransitivity::make(Bval ? expr_true : expr_false,
                                                  VAR(id_I_A, Physical),
                                                  RHS(id_I_A, Physical, SRC)));

        if (Bval)
          INFRULE(pos_BB_infr,
                  llvmberry::ConsIcmpEqSame::make(*CI_A));
        else
          INFRULE(pos_BB_infr,
                  llvmberry::ConsIcmpNeqSame::make(*CI_A));

        if (hintgen_propeq(hints, VN, X, Y, I, repl, BB, BB_succ))
          return true;
      }
    } // If this fails, do nothing. We still have to check the ~pred case.

    // From [ (cmp P X Y) = B ], generate [ (cmp ~P X Y) = ~B ]
    // and check whether (cmp ~P X Y) has the same VN with I.
    // We shouldn't recursively call hintgen_propeq since original
    // propagateEquality don't insert this into the worklist.
    // (Otherwise this leads to an infinite recursion.)

    // Continue if (repl = ~B) holds.
    // ConstantInt *negB = Bval ? FalseVal : TrueVal;
    if (repl != (Bval ? FalseVal : TrueVal))
      return false;

    CmpInst *CI_A_inv = dyn_cast<CmpInst>(CI_A->clone());
    CI_A_inv->setPredicate(CI_A->getInversePredicate());

    // 1. Generate [ cmp ~P .. = ~B ] at begin(BB_succ).
    // 2. Propagate [ cmp ~P .. >= ~B ] from begin(BB_succ) until I.

    // Assumption: [ A = T/F ] at begin(BB_succ)
    // - Check whether I and CI_A_inv has the same vn. Then do below.
    // - Propagate [ A >= icmp P X Y ] from A to begin(BB_succ)
    // - Transitivity [ T/F >= A >= icmp P X Y ] at begin(BB_succ)
    // - Infer [ F/T = icmp ~P X Y ] at begin(BB_succ)
    // - Propagate [ icmp ~P X Y >= F/T ] at I
    // - Derive [ I >= icmp ~P X Y ] from same_vn at I
    // - Transitivity [ I >= icmp ~P X Y >= F/T ] at I

    bool success = false;

    if (is_same_vn(VN, I, CI_A_inv, false)) {
      auto Bval_expr = Bval ? expr_true : expr_false;
      auto repl_expr = Bval ? expr_false : expr_true;
      PROPAGATE(LESSDEF(VAR(id_I_A, Physical), RHS(id_I_A, Physical, SRC), SRC),
                BOUNDS(INSTPOS(SRC, I_A), pos_BB_prop));
      INFRULE(pos_BB_infr,
              llvmberry::ConsTransitivity::make(Bval_expr,
                                                VAR(id_I_A, Physical),
                                                RHS(id_I_A, Physical, SRC)));

      INFRULE(pos_BB_infr,
              llvmberry::ConsIcmpInverseRhs::make(*CI_A, Bval ? 1 : 0));

      PROPAGATE(LESSDEF(INSN(*CI_A_inv), repl_expr, SRC),
                BOUNDS(pos_BB_infr, INSTPOS(SRC, I)));

      bool hintgen = hintgen_same_vn(hints, VN, I, CI_A_inv, false, I);
      if (!hintgen) {
        hints.appendToDescription("GVN propeq: cmp neg case found but hintgen_same_vn failed");
        success = false;
      } else {
        // assert(hintgen && "GVN propeq: Cmp neg case failed!");

        INFRULE(INSTPOS(SRC, I),
                llvmberry::ConsTransitivity::make(VAR(id_I, Physical),
                                                  INSN(*CI_A_inv), repl_expr));

        success = true;
      }
    }

    delete CI_A_inv;

    return success;
  }

  // Reaching here means stuck.
  return false;
}

// Create hint invariant [ L >= R ] in scope
// TODO: use GVN's expression instead of HintGen.cpp
// and remove copied code in HintGen.cpp
std::shared_ptr<llvmberry::TyPropagateObject>
make_repl_inv(llvmberry::CoreHint &hints, ValueTable &VN, Instruction *I,
              Value *repl, llvmberry::TyScope scope) {
  std::string id_I = llvmberry::getVariable(*I);

  if (Instruction *I_repl = dyn_cast<Instruction>(repl)) {
    // repl is Inst, which implies that repl and I have the same VN,
    // since an Inst cannot be a leader of other VN's.
    std::string id_I_repl = llvmberry::getVariable(*I_repl);

    // This is the simplest, but most frequent case.
    // x = e1 (= op a1 b1 ..)
    // y = e2 (= op a2 b2 ..)
    // z = f(y) -> z = f(x)

    assert(VN.lookup(I) == VN.lookup(I_repl) &&
           "GVN make_repl_inv: repl and I have different value numbers!");
    // This produces [ id(I) >= I_repl ] at I.
    bool hintgen = hintgen_same_vn(hints, VN, I, I_repl, true, I);
    if (!hintgen) {
      hints.appendToDescription("GVN repl_inv: hintgen_same_vn failed.");
    }
    // assert(hintgen && "GVN make_repl_inv: same_vn failed!");

    // // Propagate [ exp(I_repl) >= id(I_repl) ] until I.
    // // Transitivity [ id(I) >= exp(I_repl) >= id(I_repl) ] at I.
    // PROPAGATE(LESSDEF(RHS(id_I_repl, Physical, SRC), VAR(id_I_repl, Physical), SRC),
    //           BOUNDS(INSTPOS(SRC, I_repl), INSTPOS(SRC, I)));
    // INFRULE(INSTPOS(SRC, I),
    //         llvmberry::ConsTransitivity::make(VAR(id_I, Physical),
    //                                           RHS(id_I_repl, Physical, SRC),
    //                                           VAR(id_I_repl, Physical)));
  } else {
    // If repl is not an instruction, then it's always from propagateEquality.
    const BasicBlock *BB_succ = llvmberry::PassDictionary::GetInstance()
                                     .get<llvmberry::ArgForGVNReplace>()
                                     ->BB;
    const BasicBlock *BB_pred = BB_succ->getSinglePredecessor();
    assert(BB_pred &&
           "Expect it to be introduced from propagateEquality, and it checks "
           "RootDominatesEnd, meaning it has single predecessor");

    TerminatorInst *TI =
        const_cast<TerminatorInst *>(BB_pred->getTerminator());

    //from here

    Instruction *condI = nullptr;
    ConstantInt *CI_cond = nullptr;

    if (BranchInst *BI = llvm::dyn_cast<llvm::BranchInst>(TI)) {
      condI = llvm::dyn_cast<Instruction>(BI->getCondition());

      if (BI->getSuccessor(0) == BB_succ) {
        CI_cond = ConstantInt::getTrue(BB_succ->getContext());
      } else if (BI->getSuccessor(1) == BB_succ) {
        CI_cond = ConstantInt::getFalse(BB_succ->getContext());
      } else
        assert(false &&
               "GVN make_repl_inv: Leader_bb not successor of leader_bb_pred");
    } else if (SwitchInst *SI = llvm::dyn_cast<llvm::SwitchInst>(TI)) {
      condI = llvm::dyn_cast<Instruction>(SI->getCondition());

      // Set to null when case for BB_succ is not found, not unique, or default.
      // Find case from dest.
      // We cannot use SwitchInst::findCaseDest since BB_succ is const.
      for (SwitchInst::CaseIt i = SI->case_begin(), e = SI->case_end(); i != e;
           ++i)
        if (i.getCaseSuccessor() == BB_succ)
          CI_cond = i.getCaseValue();
    } else
      assert(false && "GVN make_repl_inv: Unexpected terminator.");

    assert(condI && "GVN make_repl_inv: Branch case with non-instr cond!");
    assert(CI_cond && "GVN make_repl_inv: Branch condition constant not found!");

    std::string id_condI = llvmberry::getVariable(*condI);

    // // 1. Propagate [ exp(condI) = id(condI) ] until begin(BB_succ).
    // // 2. Transitivity [ exp(condI) = CI_cond ] at begin(BB_succ).
    // PROPAGATE(
    //           LESSDEF(VAR(id_condI, Physical), RHS(id_condI, Physical, SRC), SRC),
    //           BOUNDS(INSTPOS(SRC, condI),
    //                  llvmberry::TyPosition::make_end_of_block(SRC, *BB_pred)));
    // PROPAGATE(
    //           LESSDEF(RHS(id_condI, Physical, SRC), VAR(id_condI, Physical), SRC),
    //           BOUNDS(INSTPOS(SRC, condI),
    //                  llvmberry::TyPosition::make_end_of_block(SRC, *BB_pred)));

    // INFRULE(
    //         llvmberry::TyPosition::make(SRC, BB_succ->getName(), BB_pred->getName()),
    //         llvmberry::ConsTransitivity::make(RHS(id_condI, Physical, SRC),
    //                                           VAR(id_condI, Physical),
    //                                           llvmberry::TyExpr::make(*CI_cond)));
    // INFRULE(
    //         llvmberry::TyPosition::make(SRC, BB_succ->getName(), BB_pred->getName()),
    //         llvmberry::ConsTransitivity::make(llvmberry::TyExpr::make(*CI_cond),
    //                                           VAR(id_condI, Physical),
    //                                           RHS(id_condI, Physical, SRC)));

    // hintgen_propeq generates hints to infer [ id(I) >= repl ] at I,
    // assuming [ exp(condI) = CI_cond ] at begin(BB_succ).
    // [ id(condI) = CI_cond ] is generated by the validator.
    bool hintgen = hintgen_propeq(hints, VN, condI, CI_cond, I, repl,
                                  BB_pred, BB_succ);
    // assert(hintgen && "GVN make_repl_inv: hintgen_propeq failed!");
    if (!hintgen) {
      hints.appendToDescription("GVN repl_inv: hintgen_propeq failed.");
    }
  }

  return LESSDEF(VAR(id_I, Physical), llvmberry::TyExpr::make(*repl), SRC);
}
// End LLVMBerry

/// Return true if we can prove that the value
/// we're analyzing is fully available in the specified block.  As we go, keep
/// track of which blocks we know are fully alive in FullyAvailableBlocks.  This
/// map is actually a tri-state map with the following values:
///   0) we know the block *is not* fully available.
///   1) we know the block *is* fully available.
///   2) we do not know whether the block is fully available or not, but we are
///      currently speculating that it will be.
///   3) we are speculating for this block and have used that to speculate for
///      other blocks.
static bool IsValueFullyAvailableInBlock(BasicBlock *BB,
                            DenseMap<BasicBlock*, char> &FullyAvailableBlocks,
                            uint32_t RecurseDepth) {
  if (RecurseDepth > MaxRecurseDepth)
    return false;

  // Optimistically assume that the block is fully available and check to see
  // if we already know about this block in one lookup.
  std::pair<DenseMap<BasicBlock*, char>::iterator, char> IV =
    FullyAvailableBlocks.insert(std::make_pair(BB, 2));

  // If the entry already existed for this block, return the precomputed value.
  if (!IV.second) {
    // If this is a speculative "available" value, mark it as being used for
    // speculation of other blocks.
    if (IV.first->second == 2)
      IV.first->second = 3;
    return IV.first->second != 0;
  }

  // Otherwise, see if it is fully available in all predecessors.
  pred_iterator PI = pred_begin(BB), PE = pred_end(BB);

  // If this block has no predecessors, it isn't live-in here.
  if (PI == PE)
    goto SpeculationFailure;

  for (; PI != PE; ++PI)
    // If the value isn't fully available in one of our predecessors, then it
    // isn't fully available in this block either.  Undo our previous
    // optimistic assumption and bail out.
    if (!IsValueFullyAvailableInBlock(*PI, FullyAvailableBlocks,RecurseDepth+1))
      goto SpeculationFailure;

  return true;

// If we get here, we found out that this is not, after
// all, a fully-available block.  We have a problem if we speculated on this and
// used the speculation to mark other blocks as available.
SpeculationFailure:
  char &BBVal = FullyAvailableBlocks[BB];

  // If we didn't speculate on this, just return with it set to false.
  if (BBVal == 2) {
    BBVal = 0;
    return false;
  }

  // If we did speculate on this value, we could have blocks set to 1 that are
  // incorrect.  Walk the (transitive) successors of this block and mark them as
  // 0 if set to one.
  SmallVector<BasicBlock*, 32> BBWorklist;
  BBWorklist.push_back(BB);

  do {
    BasicBlock *Entry = BBWorklist.pop_back_val();
    // Note that this sets blocks to 0 (unavailable) if they happen to not
    // already be in FullyAvailableBlocks.  This is safe.
    char &EntryVal = FullyAvailableBlocks[Entry];
    if (EntryVal == 0) continue;  // Already unavailable.

    // Mark as unavailable.
    EntryVal = 0;

    BBWorklist.append(succ_begin(Entry), succ_end(Entry));
  } while (!BBWorklist.empty());

  return false;
}


/// Return true if CoerceAvailableValueToLoadType will succeed.
static bool CanCoerceMustAliasedValueToLoad(Value *StoredVal,
                                            Type *LoadTy,
                                            const DataLayout &DL) {
  // If the loaded or stored value is an first class array or struct, don't try
  // to transform them.  We need to be able to bitcast to integer.
  if (LoadTy->isStructTy() || LoadTy->isArrayTy() ||
      StoredVal->getType()->isStructTy() ||
      StoredVal->getType()->isArrayTy())
    return false;

  // The store has to be at least as big as the load.
  if (DL.getTypeSizeInBits(StoredVal->getType()) <
        DL.getTypeSizeInBits(LoadTy))
    return false;

  return true;
}

/// If we saw a store of a value to memory, and
/// then a load from a must-aliased pointer of a different type, try to coerce
/// the stored value.  LoadedTy is the type of the load we want to replace.
/// IRB is IRBuilder used to insert new instructions.
///
/// If we can't do it, return null.
static Value *CoerceAvailableValueToLoadType(Value *StoredVal, Type *LoadedTy,
                                             IRBuilder<> &IRB,
                                             const DataLayout &DL) {
  if (!CanCoerceMustAliasedValueToLoad(StoredVal, LoadedTy, DL))
    return nullptr;

  // If this is already the right type, just return it.
  Type *StoredValTy = StoredVal->getType();

  uint64_t StoreSize = DL.getTypeSizeInBits(StoredValTy);
  uint64_t LoadSize = DL.getTypeSizeInBits(LoadedTy);

  // If the store and reload are the same size, we can always reuse it.
  if (StoreSize == LoadSize) {
    // Pointer to Pointer -> use bitcast.
    if (StoredValTy->getScalarType()->isPointerTy() &&
        LoadedTy->getScalarType()->isPointerTy())
      return IRB.CreateBitCast(StoredVal, LoadedTy);

    // Convert source pointers to integers, which can be bitcast.
    if (StoredValTy->getScalarType()->isPointerTy()) {
      StoredValTy = DL.getIntPtrType(StoredValTy);
      StoredVal = IRB.CreatePtrToInt(StoredVal, StoredValTy);
    }

    Type *TypeToCastTo = LoadedTy;
    if (TypeToCastTo->getScalarType()->isPointerTy())
      TypeToCastTo = DL.getIntPtrType(TypeToCastTo);

    if (StoredValTy != TypeToCastTo)
      StoredVal = IRB.CreateBitCast(StoredVal, TypeToCastTo);

    // Cast to pointer if the load needs a pointer type.
    if (LoadedTy->getScalarType()->isPointerTy())
      StoredVal = IRB.CreateIntToPtr(StoredVal, LoadedTy);

    return StoredVal;
  }

  // If the loaded value is smaller than the available value, then we can
  // extract out a piece from it.  If the available value is too small, then we
  // can't do anything.
  assert(StoreSize >= LoadSize && "CanCoerceMustAliasedValueToLoad fail");

  // Convert source pointers to integers, which can be manipulated.
  if (StoredValTy->getScalarType()->isPointerTy()) {
    StoredValTy = DL.getIntPtrType(StoredValTy);
    StoredVal = IRB.CreatePtrToInt(StoredVal, StoredValTy);
  }

  // Convert vectors and fp to integer, which can be manipulated.
  if (!StoredValTy->isIntegerTy()) {
    StoredValTy = IntegerType::get(StoredValTy->getContext(), StoreSize);
    StoredVal = IRB.CreateBitCast(StoredVal, StoredValTy);
  }

  // If this is a big-endian system, we need to shift the value down to the low
  // bits so that a truncate will work.
  if (DL.isBigEndian()) {
    StoredVal = IRB.CreateLShr(StoredVal, StoreSize - LoadSize, "tmp");
  }

  // Truncate the integer to the right size now.
  Type *NewIntTy = IntegerType::get(StoredValTy->getContext(), LoadSize);
  StoredVal  = IRB.CreateTrunc(StoredVal, NewIntTy, "trunc");

  if (LoadedTy == NewIntTy)
    return StoredVal;

  // If the result is a pointer, inttoptr.
  if (LoadedTy->getScalarType()->isPointerTy())
    return IRB.CreateIntToPtr(StoredVal, LoadedTy, "inttoptr");

  // Otherwise, bitcast.
  return IRB.CreateBitCast(StoredVal, LoadedTy, "bitcast");
}

/// This function is called when we have a
/// memdep query of a load that ends up being a clobbering memory write (store,
/// memset, memcpy, memmove).  This means that the write *may* provide bits used
/// by the load but we can't be sure because the pointers don't mustalias.
///
/// Check this case to see if there is anything more we can do before we give
/// up.  This returns -1 if we have to give up, or a byte number in the stored
/// value of the piece that feeds the load.
static int AnalyzeLoadFromClobberingWrite(Type *LoadTy, Value *LoadPtr,
                                          Value *WritePtr,
                                          uint64_t WriteSizeInBits,
                                          const DataLayout &DL) {
  // If the loaded or stored value is a first class array or struct, don't try
  // to transform them.  We need to be able to bitcast to integer.
  if (LoadTy->isStructTy() || LoadTy->isArrayTy())
    return -1;

  int64_t StoreOffset = 0, LoadOffset = 0;
  Value *StoreBase =
      GetPointerBaseWithConstantOffset(WritePtr, StoreOffset, DL);
  Value *LoadBase = GetPointerBaseWithConstantOffset(LoadPtr, LoadOffset, DL);
  if (StoreBase != LoadBase)
    return -1;

  // If the load and store are to the exact same address, they should have been
  // a must alias.  AA must have gotten confused.
  // FIXME: Study to see if/when this happens.  One case is forwarding a memset
  // to a load from the base of the memset.
#if 0
  if (LoadOffset == StoreOffset) {
    dbgs() << "STORE/LOAD DEP WITH COMMON POINTER MISSED:\n"
    << "Base       = " << *StoreBase << "\n"
    << "Store Ptr  = " << *WritePtr << "\n"
    << "Store Offs = " << StoreOffset << "\n"
    << "Load Ptr   = " << *LoadPtr << "\n";
    abort();
  }
#endif

  // If the load and store don't overlap at all, the store doesn't provide
  // anything to the load.  In this case, they really don't alias at all, AA
  // must have gotten confused.
  uint64_t LoadSize = DL.getTypeSizeInBits(LoadTy);

  if ((WriteSizeInBits & 7) | (LoadSize & 7))
    return -1;
  uint64_t StoreSize = WriteSizeInBits >> 3;  // Convert to bytes.
  LoadSize >>= 3;


  bool isAAFailure = false;
  if (StoreOffset < LoadOffset)
    isAAFailure = StoreOffset+int64_t(StoreSize) <= LoadOffset;
  else
    isAAFailure = LoadOffset+int64_t(LoadSize) <= StoreOffset;

  if (isAAFailure) {
#if 0
    dbgs() << "STORE LOAD DEP WITH COMMON BASE:\n"
    << "Base       = " << *StoreBase << "\n"
    << "Store Ptr  = " << *WritePtr << "\n"
    << "Store Offs = " << StoreOffset << "\n"
    << "Load Ptr   = " << *LoadPtr << "\n";
    abort();
#endif
    return -1;
  }

  // If the Load isn't completely contained within the stored bits, we don't
  // have all the bits to feed it.  We could do something crazy in the future
  // (issue a smaller load then merge the bits in) but this seems unlikely to be
  // valuable.
  if (StoreOffset > LoadOffset ||
      StoreOffset+StoreSize < LoadOffset+LoadSize)
    return -1;

  // Okay, we can do this transformation.  Return the number of bytes into the
  // store that the load is.
  return LoadOffset-StoreOffset;
}

/// This function is called when we have a
/// memdep query of a load that ends up being a clobbering store.
static int AnalyzeLoadFromClobberingStore(Type *LoadTy, Value *LoadPtr,
                                          StoreInst *DepSI) {
  // Cannot handle reading from store of first-class aggregate yet.
  if (DepSI->getValueOperand()->getType()->isStructTy() ||
      DepSI->getValueOperand()->getType()->isArrayTy())
    return -1;

  const DataLayout &DL = DepSI->getModule()->getDataLayout();
  Value *StorePtr = DepSI->getPointerOperand();
  uint64_t StoreSize =DL.getTypeSizeInBits(DepSI->getValueOperand()->getType());
  return AnalyzeLoadFromClobberingWrite(LoadTy, LoadPtr,
                                        StorePtr, StoreSize, DL);
}

/// This function is called when we have a
/// memdep query of a load that ends up being clobbered by another load.  See if
/// the other load can feed into the second load.
static int AnalyzeLoadFromClobberingLoad(Type *LoadTy, Value *LoadPtr,
                                         LoadInst *DepLI, const DataLayout &DL){
  // Cannot handle reading from store of first-class aggregate yet.
  if (DepLI->getType()->isStructTy() || DepLI->getType()->isArrayTy())
    return -1;

  Value *DepPtr = DepLI->getPointerOperand();
  uint64_t DepSize = DL.getTypeSizeInBits(DepLI->getType());
  int R = AnalyzeLoadFromClobberingWrite(LoadTy, LoadPtr, DepPtr, DepSize, DL);
  if (R != -1) return R;

  // If we have a load/load clobber an DepLI can be widened to cover this load,
  // then we should widen it!
  int64_t LoadOffs = 0;
  const Value *LoadBase =
      GetPointerBaseWithConstantOffset(LoadPtr, LoadOffs, DL);
  unsigned LoadSize = DL.getTypeStoreSize(LoadTy);

  unsigned Size = MemoryDependenceAnalysis::getLoadLoadClobberFullWidthSize(
      LoadBase, LoadOffs, LoadSize, DepLI);
  if (Size == 0) return -1;

  return AnalyzeLoadFromClobberingWrite(LoadTy, LoadPtr, DepPtr, Size*8, DL);
}



static int AnalyzeLoadFromClobberingMemInst(Type *LoadTy, Value *LoadPtr,
                                            MemIntrinsic *MI,
                                            const DataLayout &DL) {
  // If the mem operation is a non-constant size, we can't handle it.
  ConstantInt *SizeCst = dyn_cast<ConstantInt>(MI->getLength());
  if (!SizeCst) return -1;
  uint64_t MemSizeInBits = SizeCst->getZExtValue()*8;

  // If this is memset, we just need to see if the offset is valid in the size
  // of the memset..
  if (MI->getIntrinsicID() == Intrinsic::memset)
    return AnalyzeLoadFromClobberingWrite(LoadTy, LoadPtr, MI->getDest(),
                                          MemSizeInBits, DL);

  // If we have a memcpy/memmove, the only case we can handle is if this is a
  // copy from constant memory.  In that case, we can read directly from the
  // constant memory.
  MemTransferInst *MTI = cast<MemTransferInst>(MI);

  Constant *Src = dyn_cast<Constant>(MTI->getSource());
  if (!Src) return -1;

  GlobalVariable *GV = dyn_cast<GlobalVariable>(GetUnderlyingObject(Src, DL));
  if (!GV || !GV->isConstant()) return -1;

  // See if the access is within the bounds of the transfer.
  int Offset = AnalyzeLoadFromClobberingWrite(LoadTy, LoadPtr,
                                              MI->getDest(), MemSizeInBits, DL);
  if (Offset == -1)
    return Offset;

  unsigned AS = Src->getType()->getPointerAddressSpace();
  // Otherwise, see if we can constant fold a load from the constant with the
  // offset applied as appropriate.
  Src = ConstantExpr::getBitCast(Src,
                                 Type::getInt8PtrTy(Src->getContext(), AS));
  Constant *OffsetCst =
    ConstantInt::get(Type::getInt64Ty(Src->getContext()), (unsigned)Offset);
  Src = ConstantExpr::getGetElementPtr(Type::getInt8Ty(Src->getContext()), Src,
                                       OffsetCst);
  Src = ConstantExpr::getBitCast(Src, PointerType::get(LoadTy, AS));
  if (ConstantFoldLoadFromConstPtr(Src, DL))
    return Offset;
  return -1;
}


/// This function is called when we have a
/// memdep query of a load that ends up being a clobbering store.  This means
/// that the store provides bits used by the load but we the pointers don't
/// mustalias.  Check this case to see if there is anything more we can do
/// before we give up.
static Value *GetStoreValueForLoad(Value *SrcVal, unsigned Offset,
                                   Type *LoadTy,
                                   Instruction *InsertPt, const DataLayout &DL){
  LLVMContext &Ctx = SrcVal->getType()->getContext();

  uint64_t StoreSize = (DL.getTypeSizeInBits(SrcVal->getType()) + 7) / 8;
  uint64_t LoadSize = (DL.getTypeSizeInBits(LoadTy) + 7) / 8;

  IRBuilder<> Builder(InsertPt);

  // Compute which bits of the stored value are being used by the load.  Convert
  // to an integer type to start with.
  if (SrcVal->getType()->getScalarType()->isPointerTy())
    SrcVal = Builder.CreatePtrToInt(SrcVal,
        DL.getIntPtrType(SrcVal->getType()));
  if (!SrcVal->getType()->isIntegerTy())
    SrcVal = Builder.CreateBitCast(SrcVal, IntegerType::get(Ctx, StoreSize*8));

  // Shift the bits to the least significant depending on endianness.
  unsigned ShiftAmt;
  if (DL.isLittleEndian())
    ShiftAmt = Offset*8;
  else
    ShiftAmt = (StoreSize-LoadSize-Offset)*8;

  if (ShiftAmt)
    SrcVal = Builder.CreateLShr(SrcVal, ShiftAmt);

  if (LoadSize != StoreSize)
    SrcVal = Builder.CreateTrunc(SrcVal, IntegerType::get(Ctx, LoadSize*8));

  return CoerceAvailableValueToLoadType(SrcVal, LoadTy, Builder, DL);
}

/// This function is called when we have a
/// memdep query of a load that ends up being a clobbering load.  This means
/// that the load *may* provide bits used by the load but we can't be sure
/// because the pointers don't mustalias.  Check this case to see if there is
/// anything more we can do before we give up.
static Value *GetLoadValueForLoad(LoadInst *SrcVal, unsigned Offset,
                                  Type *LoadTy, Instruction *InsertPt,
                                  GVN &gvn) {
  const DataLayout &DL = SrcVal->getModule()->getDataLayout();
  // If Offset+LoadTy exceeds the size of SrcVal, then we must be wanting to
  // widen SrcVal out to a larger load.
  unsigned SrcValSize = DL.getTypeStoreSize(SrcVal->getType());
  unsigned LoadSize = DL.getTypeStoreSize(LoadTy);
  if (Offset+LoadSize > SrcValSize) {
    assert(SrcVal->isSimple() && "Cannot widen volatile/atomic load!");
    assert(SrcVal->getType()->isIntegerTy() && "Can't widen non-integer load");
    // If we have a load/load clobber an DepLI can be widened to cover this
    // load, then we should widen it to the next power of 2 size big enough!
    unsigned NewLoadSize = Offset+LoadSize;
    if (!isPowerOf2_32(NewLoadSize))
      NewLoadSize = NextPowerOf2(NewLoadSize);

    Value *PtrVal = SrcVal->getPointerOperand();

    // Insert the new load after the old load.  This ensures that subsequent
    // memdep queries will find the new load.  We can't easily remove the old
    // load completely because it is already in the value numbering table.
    IRBuilder<> Builder(SrcVal->getParent(), ++BasicBlock::iterator(SrcVal));
    Type *DestPTy =
      IntegerType::get(LoadTy->getContext(), NewLoadSize*8);
    DestPTy = PointerType::get(DestPTy,
                               PtrVal->getType()->getPointerAddressSpace());
    Builder.SetCurrentDebugLocation(SrcVal->getDebugLoc());
    PtrVal = Builder.CreateBitCast(PtrVal, DestPTy);
    LoadInst *NewLoad = Builder.CreateLoad(PtrVal);
    NewLoad->takeName(SrcVal);
    NewLoad->setAlignment(SrcVal->getAlignment());

    DEBUG(dbgs() << "GVN WIDENED LOAD: " << *SrcVal << "\n");
    DEBUG(dbgs() << "TO: " << *NewLoad << "\n");

    // Replace uses of the original load with the wider load.  On a big endian
    // system, we need to shift down to get the relevant bits.
    Value *RV = NewLoad;
    if (DL.isBigEndian())
      RV = Builder.CreateLShr(RV,
                    NewLoadSize*8-SrcVal->getType()->getPrimitiveSizeInBits());
    RV = Builder.CreateTrunc(RV, SrcVal->getType());
    SrcVal->replaceAllUsesWith(RV);

    // We would like to use gvn.markInstructionForDeletion here, but we can't
    // because the load is already memoized into the leader map table that GVN
    // tracks.  It is potentially possible to remove the load from the table,
    // but then there all of the operations based on it would need to be
    // rehashed.  Just leave the dead load around.
    gvn.getMemDep().removeInstruction(SrcVal);
    SrcVal = NewLoad;
  }

  return GetStoreValueForLoad(SrcVal, Offset, LoadTy, InsertPt, DL);
}


/// This function is called when we have a
/// memdep query of a load that ends up being a clobbering mem intrinsic.
static Value *GetMemInstValueForLoad(MemIntrinsic *SrcInst, unsigned Offset,
                                     Type *LoadTy, Instruction *InsertPt,
                                     const DataLayout &DL){
  LLVMContext &Ctx = LoadTy->getContext();
  uint64_t LoadSize = DL.getTypeSizeInBits(LoadTy)/8;

  IRBuilder<> Builder(InsertPt);

  // We know that this method is only called when the mem transfer fully
  // provides the bits for the load.
  if (MemSetInst *MSI = dyn_cast<MemSetInst>(SrcInst)) {
    // memset(P, 'x', 1234) -> splat('x'), even if x is a variable, and
    // independently of what the offset is.
    Value *Val = MSI->getValue();
    if (LoadSize != 1)
      Val = Builder.CreateZExt(Val, IntegerType::get(Ctx, LoadSize*8));

    Value *OneElt = Val;

    // Splat the value out to the right number of bits.
    for (unsigned NumBytesSet = 1; NumBytesSet != LoadSize; ) {
      // If we can double the number of bytes set, do it.
      if (NumBytesSet*2 <= LoadSize) {
        Value *ShVal = Builder.CreateShl(Val, NumBytesSet*8);
        Val = Builder.CreateOr(Val, ShVal);
        NumBytesSet <<= 1;
        continue;
      }

      // Otherwise insert one byte at a time.
      Value *ShVal = Builder.CreateShl(Val, 1*8);
      Val = Builder.CreateOr(OneElt, ShVal);
      ++NumBytesSet;
    }

    return CoerceAvailableValueToLoadType(Val, LoadTy, Builder, DL);
  }

  // Otherwise, this is a memcpy/memmove from a constant global.
  MemTransferInst *MTI = cast<MemTransferInst>(SrcInst);
  Constant *Src = cast<Constant>(MTI->getSource());
  unsigned AS = Src->getType()->getPointerAddressSpace();

  // Otherwise, see if we can constant fold a load from the constant with the
  // offset applied as appropriate.
  Src = ConstantExpr::getBitCast(Src,
                                 Type::getInt8PtrTy(Src->getContext(), AS));
  Constant *OffsetCst =
    ConstantInt::get(Type::getInt64Ty(Src->getContext()), (unsigned)Offset);
  Src = ConstantExpr::getGetElementPtr(Type::getInt8Ty(Src->getContext()), Src,
                                       OffsetCst);
  Src = ConstantExpr::getBitCast(Src, PointerType::get(LoadTy, AS));
  return ConstantFoldLoadFromConstPtr(Src, DL);
}


/// Given a set of loads specified by ValuesPerBlock,
/// construct SSA form, allowing us to eliminate LI.  This returns the value
/// that should be used at LI's definition site.
static Value *ConstructSSAForLoadSet(LoadInst *LI,
                         SmallVectorImpl<AvailableValueInBlock> &ValuesPerBlock,
                                     GVN &gvn) {
  // Check for the fully redundant, dominating load case.  In this case, we can
  // just use the dominating value directly.
  if (ValuesPerBlock.size() == 1 &&
      gvn.getDominatorTree().properlyDominates(ValuesPerBlock[0].BB,
                                               LI->getParent())) {
    assert(!ValuesPerBlock[0].isUndefValue() && "Dead BB dominate this block");
    return ValuesPerBlock[0].MaterializeAdjustedValue(LI, gvn);
  }

  // Otherwise, we have to construct SSA form.
  SmallVector<PHINode*, 8> NewPHIs;
  SSAUpdater SSAUpdate(&NewPHIs);
  SSAUpdate.Initialize(LI->getType(), LI->getName());

  for (unsigned i = 0, e = ValuesPerBlock.size(); i != e; ++i) {
    const AvailableValueInBlock &AV = ValuesPerBlock[i];
    BasicBlock *BB = AV.BB;

    if (SSAUpdate.HasValueForBlock(BB))
      continue;

    SSAUpdate.AddAvailableValue(BB, AV.MaterializeAdjustedValue(LI, gvn));
  }

  // Perform PHI construction.
  Value *V = SSAUpdate.GetValueInMiddleOfBlock(LI->getParent());

  // If new PHI nodes were created, notify alias analysis.
  if (V->getType()->getScalarType()->isPointerTy()) {
    AliasAnalysis *AA = gvn.getAliasAnalysis();

    // Scan the new PHIs and inform alias analysis that we've added potentially
    // escaping uses to any values that are operands to these PHIs.
    for (unsigned i = 0, e = NewPHIs.size(); i != e; ++i) {
      PHINode *P = NewPHIs[i];
      for (unsigned ii = 0, ee = P->getNumIncomingValues(); ii != ee; ++ii) {
        unsigned jj = PHINode::getOperandNumForIncomingValue(ii);
        AA->addEscapingUse(P->getOperandUse(jj));
      }
    }
  }

  return V;
}

Value *AvailableValueInBlock::MaterializeAdjustedValue(LoadInst *LI,
                                                       GVN &gvn) const {
  Value *Res;
  Type *LoadTy = LI->getType();
  const DataLayout &DL = LI->getModule()->getDataLayout();
  if (isSimpleValue()) {
    Res = getSimpleValue();
    if (Res->getType() != LoadTy) {
      Res = GetStoreValueForLoad(Res, Offset, LoadTy, BB->getTerminator(), DL);

      DEBUG(dbgs() << "GVN COERCED NONLOCAL VAL:\nOffset: " << Offset << "  "
                   << *getSimpleValue() << '\n'
                   << *Res << '\n' << "\n\n\n");
    }
  } else if (isCoercedLoadValue()) {
    LoadInst *Load = getCoercedLoadValue();
    if (Load->getType() == LoadTy && Offset == 0) {
      Res = Load;
    } else {
      Res = GetLoadValueForLoad(Load, Offset, LoadTy, BB->getTerminator(),
                                gvn);
  
      DEBUG(dbgs() << "GVN COERCED NONLOCAL LOAD:\nOffset: " << Offset << "  "
                   << *getCoercedLoadValue() << '\n'
                   << *Res << '\n' << "\n\n\n");
    }
  } else if (isMemIntrinValue()) {
    Res = GetMemInstValueForLoad(getMemIntrinValue(), Offset, LoadTy,
                                 BB->getTerminator(), DL);
    DEBUG(dbgs() << "GVN COERCED NONLOCAL MEM INTRIN:\nOffset: " << Offset
                 << "  " << *getMemIntrinValue() << '\n'
                 << *Res << '\n' << "\n\n\n");
  } else {
    assert(isUndefValue() && "Should be UndefVal");
    DEBUG(dbgs() << "GVN COERCED NONLOCAL Undef:\n";);
    return UndefValue::get(LoadTy);
  }
  return Res;
}

static bool isLifetimeStart(const Instruction *Inst) {
  if (const IntrinsicInst* II = dyn_cast<IntrinsicInst>(Inst))
    return II->getIntrinsicID() == Intrinsic::lifetime_start;
  return false;
}

void GVN::AnalyzeLoadAvailability(LoadInst *LI, LoadDepVect &Deps, 
                                  AvailValInBlkVect &ValuesPerBlock,
                                  UnavailBlkVect &UnavailableBlocks) {

  // Filter out useless results (non-locals, etc).  Keep track of the blocks
  // where we have a value available in repl, also keep track of whether we see
  // dependencies that produce an unknown value for the load (such as a call
  // that could potentially clobber the load).
  unsigned NumDeps = Deps.size();
  const DataLayout &DL = LI->getModule()->getDataLayout();
  for (unsigned i = 0, e = NumDeps; i != e; ++i) {
    BasicBlock *DepBB = Deps[i].getBB();
    MemDepResult DepInfo = Deps[i].getResult();

    if (DeadBlocks.count(DepBB)) {
      // Dead dependent mem-op disguise as a load evaluating the same value
      // as the load in question.
      ValuesPerBlock.push_back(AvailableValueInBlock::getUndef(DepBB));
      continue;
    }

    if (!DepInfo.isDef() && !DepInfo.isClobber()) {
      UnavailableBlocks.push_back(DepBB);
      continue;
    }

    if (DepInfo.isClobber()) {
      // The address being loaded in this non-local block may not be the same as
      // the pointer operand of the load if PHI translation occurs.  Make sure
      // to consider the right address.
      Value *Address = Deps[i].getAddress();

      // If the dependence is to a store that writes to a superset of the bits
      // read by the load, we can extract the bits we need for the load from the
      // stored value.
      if (StoreInst *DepSI = dyn_cast<StoreInst>(DepInfo.getInst())) {
        if (Address) {
          int Offset =
              AnalyzeLoadFromClobberingStore(LI->getType(), Address, DepSI);
          if (Offset != -1) {
            ValuesPerBlock.push_back(AvailableValueInBlock::get(DepBB,
                                                       DepSI->getValueOperand(),
                                                                Offset));
            continue;
          }
        }
      }

      // Check to see if we have something like this:
      //    load i32* P
      //    load i8* (P+1)
      // if we have this, replace the later with an extraction from the former.
      if (LoadInst *DepLI = dyn_cast<LoadInst>(DepInfo.getInst())) {
        // If this is a clobber and L is the first instruction in its block, then
        // we have the first instruction in the entry block.
        if (DepLI != LI && Address) {
          int Offset =
              AnalyzeLoadFromClobberingLoad(LI->getType(), Address, DepLI, DL);

          if (Offset != -1) {
            ValuesPerBlock.push_back(AvailableValueInBlock::getLoad(DepBB,DepLI,
                                                                    Offset));
            continue;
          }
        }
      }

      // If the clobbering value is a memset/memcpy/memmove, see if we can
      // forward a value on from it.
      if (MemIntrinsic *DepMI = dyn_cast<MemIntrinsic>(DepInfo.getInst())) {
        if (Address) {
          int Offset = AnalyzeLoadFromClobberingMemInst(LI->getType(), Address,
                                                        DepMI, DL);
          if (Offset != -1) {
            ValuesPerBlock.push_back(AvailableValueInBlock::getMI(DepBB, DepMI,
                                                                  Offset));
            continue;
          }
        }
      }

      UnavailableBlocks.push_back(DepBB);
      continue;
    }

    // DepInfo.isDef() here

    Instruction *DepInst = DepInfo.getInst();

    // Loading the allocation -> undef.
    if (isa<AllocaInst>(DepInst) || isMallocLikeFn(DepInst, TLI) ||
        // Loading immediately after lifetime begin -> undef.
        isLifetimeStart(DepInst)) {
      ValuesPerBlock.push_back(AvailableValueInBlock::get(DepBB,
                                             UndefValue::get(LI->getType())));
      continue;
    }

    // Loading from calloc (which zero initializes memory) -> zero
    if (isCallocLikeFn(DepInst, TLI)) {
      ValuesPerBlock.push_back(AvailableValueInBlock::get(
          DepBB, Constant::getNullValue(LI->getType())));
      continue;
    }

    if (StoreInst *S = dyn_cast<StoreInst>(DepInst)) {
      // Reject loads and stores that are to the same address but are of
      // different types if we have to.
      if (S->getValueOperand()->getType() != LI->getType()) {
        // If the stored value is larger or equal to the loaded value, we can
        // reuse it.
        if (!CanCoerceMustAliasedValueToLoad(S->getValueOperand(),
                                             LI->getType(), DL)) {
          UnavailableBlocks.push_back(DepBB);
          continue;
        }
      }

      ValuesPerBlock.push_back(AvailableValueInBlock::get(DepBB,
                                                         S->getValueOperand()));
      continue;
    }

    if (LoadInst *LD = dyn_cast<LoadInst>(DepInst)) {
      // If the types mismatch and we can't handle it, reject reuse of the load.
      if (LD->getType() != LI->getType()) {
        // If the stored value is larger or equal to the loaded value, we can
        // reuse it.
        if (!CanCoerceMustAliasedValueToLoad(LD, LI->getType(), DL)) {
          UnavailableBlocks.push_back(DepBB);
          continue;
        }
      }
      ValuesPerBlock.push_back(AvailableValueInBlock::getLoad(DepBB, LD));
      continue;
    }

    UnavailableBlocks.push_back(DepBB);
  }
}

bool GVN::PerformLoadPRE(LoadInst *LI, AvailValInBlkVect &ValuesPerBlock, 
                         UnavailBlkVect &UnavailableBlocks) {
  // Okay, we have *some* definitions of the value.  This means that the value
  // is available in some of our (transitive) predecessors.  Lets think about
  // doing PRE of this load.  This will involve inserting a new load into the
  // predecessor when it's not available.  We could do this in general, but
  // prefer to not increase code size.  As such, we only do this when we know
  // that we only have to insert *one* load (which means we're basically moving
  // the load, not inserting a new one).

  SmallPtrSet<BasicBlock *, 4> Blockers;
  for (unsigned i = 0, e = UnavailableBlocks.size(); i != e; ++i)
    Blockers.insert(UnavailableBlocks[i]);

  // Let's find the first basic block with more than one predecessor.  Walk
  // backwards through predecessors if needed.
  BasicBlock *LoadBB = LI->getParent();
  BasicBlock *TmpBB = LoadBB;

  while (TmpBB->getSinglePredecessor()) {
    TmpBB = TmpBB->getSinglePredecessor();
    if (TmpBB == LoadBB) // Infinite (unreachable) loop.
      return false;
    if (Blockers.count(TmpBB))
      return false;

    // If any of these blocks has more than one successor (i.e. if the edge we
    // just traversed was critical), then there are other paths through this
    // block along which the load may not be anticipated.  Hoisting the load
    // above this block would be adding the load to execution paths along
    // which it was not previously executed.
    if (TmpBB->getTerminator()->getNumSuccessors() != 1)
      return false;
  }

  assert(TmpBB);
  LoadBB = TmpBB;

  // Check to see how many predecessors have the loaded value fully
  // available.
  MapVector<BasicBlock *, Value *> PredLoads;
  DenseMap<BasicBlock*, char> FullyAvailableBlocks;
  for (unsigned i = 0, e = ValuesPerBlock.size(); i != e; ++i)
    FullyAvailableBlocks[ValuesPerBlock[i].BB] = true;
  for (unsigned i = 0, e = UnavailableBlocks.size(); i != e; ++i)
    FullyAvailableBlocks[UnavailableBlocks[i]] = false;

  SmallVector<BasicBlock *, 4> CriticalEdgePred;
  for (pred_iterator PI = pred_begin(LoadBB), E = pred_end(LoadBB);
       PI != E; ++PI) {
    BasicBlock *Pred = *PI;
    if (IsValueFullyAvailableInBlock(Pred, FullyAvailableBlocks, 0)) {
      continue;
    }

    if (Pred->getTerminator()->getNumSuccessors() != 1) {
      if (isa<IndirectBrInst>(Pred->getTerminator())) {
        DEBUG(dbgs() << "COULD NOT PRE LOAD BECAUSE OF INDBR CRITICAL EDGE '"
              << Pred->getName() << "': " << *LI << '\n');
        return false;
      }

      if (LoadBB->isLandingPad()) {
        DEBUG(dbgs()
              << "COULD NOT PRE LOAD BECAUSE OF LANDING PAD CRITICAL EDGE '"
              << Pred->getName() << "': " << *LI << '\n');
        return false;
      }

      CriticalEdgePred.push_back(Pred);
    } else {
      // Only add the predecessors that will not be split for now.
      PredLoads[Pred] = nullptr;
    }
  }

  // Decide whether PRE is profitable for this load.
  unsigned NumUnavailablePreds = PredLoads.size() + CriticalEdgePred.size();
  assert(NumUnavailablePreds != 0 &&
         "Fully available value should already be eliminated!");

  // If this load is unavailable in multiple predecessors, reject it.
  // FIXME: If we could restructure the CFG, we could make a common pred with
  // all the preds that don't have an available LI and insert a new load into
  // that one block.
  if (NumUnavailablePreds != 1)
      return false;

  // Split critical edges, and update the unavailable predecessors accordingly.
  for (BasicBlock *OrigPred : CriticalEdgePred) {
    BasicBlock *NewPred = splitCriticalEdges(OrigPred, LoadBB);
    assert(!PredLoads.count(OrigPred) && "Split edges shouldn't be in map!");
    PredLoads[NewPred] = nullptr;
    DEBUG(dbgs() << "Split critical edge " << OrigPred->getName() << "->"
                 << LoadBB->getName() << '\n');
  }

  // Check if the load can safely be moved to all the unavailable predecessors.
  bool CanDoPRE = true;
  const DataLayout &DL = LI->getModule()->getDataLayout();
  SmallVector<Instruction*, 8> NewInsts;
  for (auto &PredLoad : PredLoads) {
    BasicBlock *UnavailablePred = PredLoad.first;

    // Do PHI translation to get its value in the predecessor if necessary.  The
    // returned pointer (if non-null) is guaranteed to dominate UnavailablePred.

    // If all preds have a single successor, then we know it is safe to insert
    // the load on the pred (?!?), so we can insert code to materialize the
    // pointer if it is not available.
    PHITransAddr Address(LI->getPointerOperand(), DL, AC);
    Value *LoadPtr = nullptr;
    LoadPtr = Address.PHITranslateWithInsertion(LoadBB, UnavailablePred,
                                                *DT, NewInsts);

    // If we couldn't find or insert a computation of this phi translated value,
    // we fail PRE.
    if (!LoadPtr) {
      DEBUG(dbgs() << "COULDN'T INSERT PHI TRANSLATED VALUE OF: "
            << *LI->getPointerOperand() << "\n");
      CanDoPRE = false;
      break;
    }

    PredLoad.second = LoadPtr;
  }

  if (!CanDoPRE) {
    while (!NewInsts.empty()) {
      Instruction *I = NewInsts.pop_back_val();
      if (MD) MD->removeInstruction(I);
      llvmberry::name_instructions(*(I->getParent()->getParent()));
      llvmberry::ValidationUnit::Begin("GVN_dead_code_elim2",
                                       I->getParent()->getParent());
      llvmberry::generateHintForGVNDCE(*I);
      I->eraseFromParent();
      llvmberry::ValidationUnit::End();
    }
    // HINT: Don't revert the edge-splitting as following transformation may
    // also need to split these critical edges.
    return !CriticalEdgePred.empty();
  }

  // Okay, we can eliminate this load by inserting a reload in the predecessor
  // and using PHI construction to get the value in the other predecessors, do
  // it.
  DEBUG(dbgs() << "GVN REMOVING PRE LOAD: " << *LI << '\n');
  DEBUG(if (!NewInsts.empty())
          dbgs() << "INSERTED " << NewInsts.size() << " INSTS: "
                 << *NewInsts.back() << '\n');

  // Assign value numbers to the new instructions.
  for (unsigned i = 0, e = NewInsts.size(); i != e; ++i) {
    // FIXME: We really _ought_ to insert these value numbers into their
    // parent's availability map.  However, in doing so, we risk getting into
    // ordering issues.  If a block hasn't been processed yet, we would be
    // marking a value as AVAIL-IN, which isn't what we intend.
    VN.lookup_or_add(NewInsts[i]);
  }

  for (const auto &PredLoad : PredLoads) {
    BasicBlock *UnavailablePred = PredLoad.first;
    Value *LoadPtr = PredLoad.second;

    Instruction *NewLoad = new LoadInst(LoadPtr, LI->getName()+".pre", false,
                                        LI->getAlignment(),
                                        UnavailablePred->getTerminator());

    // Transfer the old load's AA tags to the new load.
    AAMDNodes Tags;
    LI->getAAMetadata(Tags);
    if (Tags)
      NewLoad->setAAMetadata(Tags);

    // Transfer DebugLoc.
    NewLoad->setDebugLoc(LI->getDebugLoc());

    // Add the newly created load.
    ValuesPerBlock.push_back(AvailableValueInBlock::get(UnavailablePred,
                                                        NewLoad));
    MD->invalidateCachedPointerInfo(LoadPtr);
    DEBUG(dbgs() << "GVN INSERTED " << *NewLoad << '\n');
  }

  // Perform PHI construction.
  Value *V = ConstructSSAForLoadSet(LI, ValuesPerBlock, *this);
  LI->replaceAllUsesWith(V);
  if (isa<PHINode>(V))
    V->takeName(LI);
  if (Instruction *I = dyn_cast<Instruction>(V))
    I->setDebugLoc(LI->getDebugLoc());
  if (V->getType()->getScalarType()->isPointerTy())
    MD->invalidateCachedPointerInfo(V);
  markInstructionForDeletion(LI);
  ++NumPRELoad;
  return true;
}

/// Attempt to eliminate a load whose dependencies are
/// non-local by performing PHI construction.
bool GVN::processNonLocalLoad(LoadInst *LI) {
  // Step 1: Find the non-local dependencies of the load.
  LoadDepVect Deps;
  MD->getNonLocalPointerDependency(LI, Deps);

  // If we had to process more than one hundred blocks to find the
  // dependencies, this load isn't worth worrying about.  Optimizing
  // it will be too expensive.
  unsigned NumDeps = Deps.size();
  if (NumDeps > 100)
    return false;

  // If we had a phi translation failure, we'll have a single entry which is a
  // clobber in the current block.  Reject this early.
  if (NumDeps == 1 &&
      !Deps[0].getResult().isDef() && !Deps[0].getResult().isClobber()) {
    DEBUG(
      dbgs() << "GVN: non-local load ";
      LI->printAsOperand(dbgs());
      dbgs() << " has unknown dependencies\n";
    );
    return false;
  }

  // If this load follows a GEP, see if we can PRE the indices before analyzing.
  if (GetElementPtrInst *GEP = dyn_cast<GetElementPtrInst>(LI->getOperand(0))) {
    for (GetElementPtrInst::op_iterator OI = GEP->idx_begin(),
                                        OE = GEP->idx_end();
         OI != OE; ++OI)
      if (Instruction *I = dyn_cast<Instruction>(OI->get())) {
        llvmberry::intrude([]() {
          llvmberry::PassDictionary &pdata =
              llvmberry::PassDictionary::GetInstance();
          pdata.get<llvmberry::ArgForGVNPRE>()->isFromNonLocalLoad = true;
        });
        performScalarPRE(I);
      }
  }

  // Step 2: Analyze the availability of the load
  AvailValInBlkVect ValuesPerBlock;
  UnavailBlkVect UnavailableBlocks;
  AnalyzeLoadAvailability(LI, Deps, ValuesPerBlock, UnavailableBlocks);

  // If we have no predecessors that produce a known value for this load, exit
  // early.
  if (ValuesPerBlock.empty())
    return false;

  // Step 3: Eliminate fully redundancy.
  //
  // If all of the instructions we depend on produce a known value for this
  // load, then it is fully redundant and we can use PHI insertion to compute
  // its value.  Insert PHIs and remove the fully redundant value now.
  if (UnavailableBlocks.empty()) {
    DEBUG(dbgs() << "GVN REMOVING NONLOCAL LOAD: " << *LI << '\n');

    // Perform PHI construction.
    Value *V = ConstructSSAForLoadSet(LI, ValuesPerBlock, *this);
    LI->replaceAllUsesWith(V);

    if (isa<PHINode>(V))
      V->takeName(LI);
    if (Instruction *I = dyn_cast<Instruction>(V))
      if (LI->getDebugLoc())
        I->setDebugLoc(LI->getDebugLoc());
    if (V->getType()->getScalarType()->isPointerTy())
      MD->invalidateCachedPointerInfo(V);
    markInstructionForDeletion(LI);
    ++NumGVNLoad;
    return true;
  }

  // Step 4: Eliminate partial redundancy.
  if (!EnablePRE || !EnableLoadPRE)
    return false;

  return PerformLoadPRE(LI, ValuesPerBlock, UnavailableBlocks);
}


static void patchReplacementInstruction(Instruction *I, Value *Repl) {
  // Patch the replacement so that it is not more restrictive than the value
  // being replaced.
  BinaryOperator *Op = dyn_cast<BinaryOperator>(I);
  BinaryOperator *ReplOp = dyn_cast<BinaryOperator>(Repl);
  if (Op && ReplOp)
    ReplOp->andIRFlags(Op);

  if (Instruction *ReplInst = dyn_cast<Instruction>(Repl)) {
    // llvmberry: patch for getelementptr inbounds
    // Mimics llvm trunk, r275532
    if (auto *SrcGEP = dyn_cast<GetElementPtrInst>(I))
      if (auto *DestGEP = dyn_cast<GetElementPtrInst>(ReplInst))
        DestGEP->setIsInBounds(SrcGEP->isInBounds() & DestGEP->isInBounds());

    // FIXME: If both the original and replacement value are part of the
    // same control-flow region (meaning that the execution of one
    // guarentees the executation of the other), then we can combine the
    // noalias scopes here and do better than the general conservative
    // answer used in combineMetadata().

    // In general, GVN unifies expressions over different control-flow
    // regions, and so we need a conservative combination of the noalias
    // scopes.
    static const unsigned KnownIDs[] = {
      LLVMContext::MD_tbaa,
      LLVMContext::MD_alias_scope,
      LLVMContext::MD_noalias,
      LLVMContext::MD_range,
      LLVMContext::MD_fpmath,
      LLVMContext::MD_invariant_load,
    };
    combineMetadata(ReplInst, I, KnownIDs);
  }
}

static void patchAndReplaceAllUsesWith(Instruction *I, Value *Repl) {
  patchReplacementInstruction(I, Repl);

  llvmberry::ValidationUnit::Begin("GVN_replace", I->getParent()->getParent());

  llvmberry::ValidationUnit::GetInstance()->intrude([&I, &Repl](
      llvmberry::ValidationUnit::Dictionary &data, llvmberry::CoreHint &hints) {
    llvmberry::PassDictionary &pdata = llvmberry::PassDictionary::GetInstance();
    if (!pdata.get<llvmberry::ArgForGVNReplace>()->isGVNReplace) {
      llvmberry::ValidationUnit::GetInstance()->setIsAborted();
      return;
    }
    pdata.get<llvmberry::ArgForGVNReplace>()->isGVNReplace = false;

    ValueTable &VN = *boost::any_cast<ValueTable*>(pdata.get<llvmberry::ArgForGVNReplace>()->VNptr);

    std::shared_ptr<llvmberry::TyPropagateObject> repl_inv
      = make_repl_inv(hints, VN, I, Repl, SRC);

    // TODO: change to assert
    if (!repl_inv)
      return;

    // Propagate repl_inv from I to each use, and replace will be done
    // automatically
    for (auto UI = I->use_begin(); UI != I->use_end(); ++UI) {
      if (!isa<Instruction>(UI->getUser())) {
        // let the validation fail when the user is not an instruction
        assert(false && "User is not an instruction");
      }

      Instruction *userI = dyn_cast<Instruction>(UI->getUser());
      std::string userI_id = llvmberry::getVariable(*userI);

      std::string prev_block_name = "";
      if (isa<PHINode>(userI)) {
        BasicBlock *bb_from = dyn_cast<PHINode>(userI)->getIncomingBlock(*UI);
        prev_block_name = llvmberry::getBasicBlockIndex(bb_from);
      }

      PROPAGATE(repl_inv, BOUNDS(INSTPOS(SRC, I), llvmberry::TyPosition::make(
                                                      llvmberry::Source, *userI,
                                                      prev_block_name)));
    }
  });

  I->replaceAllUsesWith(Repl);

  llvmberry::ValidationUnit::EndIfExists();
}

/// Attempt to eliminate a load, first by eliminating it
/// locally, and then attempting non-local elimination if that fails.
bool GVN::processLoad(LoadInst *L) {
  if (!MD)
    return false;

  if (!L->isSimple())
    return false;

  if (L->use_empty()) {
    markInstructionForDeletion(L);
    return true;
  }

  // ... to a pointer that has been loaded from before...
  MemDepResult Dep = MD->getDependency(L);
  const DataLayout &DL = L->getModule()->getDataLayout();

  // If we have a clobber and target data is around, see if this is a clobber
  // that we can fix up through code synthesis.
  if (Dep.isClobber()) {
    // Check to see if we have something like this:
    //   store i32 123, i32* %P
    //   %A = bitcast i32* %P to i8*
    //   %B = gep i8* %A, i32 1
    //   %C = load i8* %B
    //
    // We could do that by recognizing if the clobber instructions are obviously
    // a common base + constant offset, and if the previous store (or memset)
    // completely covers this load.  This sort of thing can happen in bitfield
    // access code.
    Value *AvailVal = nullptr;
    if (StoreInst *DepSI = dyn_cast<StoreInst>(Dep.getInst())) {
      int Offset = AnalyzeLoadFromClobberingStore(
          L->getType(), L->getPointerOperand(), DepSI);
      if (Offset != -1)
        AvailVal = GetStoreValueForLoad(DepSI->getValueOperand(), Offset,
                                        L->getType(), L, DL);
    }

    // Check to see if we have something like this:
    //    load i32* P
    //    load i8* (P+1)
    // if we have this, replace the later with an extraction from the former.
    if (LoadInst *DepLI = dyn_cast<LoadInst>(Dep.getInst())) {
      // If this is a clobber and L is the first instruction in its block, then
      // we have the first instruction in the entry block.
      if (DepLI == L)
        return false;

      int Offset = AnalyzeLoadFromClobberingLoad(
          L->getType(), L->getPointerOperand(), DepLI, DL);
      if (Offset != -1)
        AvailVal = GetLoadValueForLoad(DepLI, Offset, L->getType(), L, *this);
    }

    // If the clobbering value is a memset/memcpy/memmove, see if we can forward
    // a value on from it.
    if (MemIntrinsic *DepMI = dyn_cast<MemIntrinsic>(Dep.getInst())) {
      int Offset = AnalyzeLoadFromClobberingMemInst(
          L->getType(), L->getPointerOperand(), DepMI, DL);
      if (Offset != -1)
        AvailVal = GetMemInstValueForLoad(DepMI, Offset, L->getType(), L, DL);
    }

    if (AvailVal) {
      DEBUG(dbgs() << "GVN COERCED INST:\n" << *Dep.getInst() << '\n'
            << *AvailVal << '\n' << *L << "\n\n\n");

      // Replace the load!
      L->replaceAllUsesWith(AvailVal);
      if (AvailVal->getType()->getScalarType()->isPointerTy())
        MD->invalidateCachedPointerInfo(AvailVal);
      markInstructionForDeletion(L);
      ++NumGVNLoad;
      return true;
    }
  }

  // If the value isn't available, don't do anything!
  if (Dep.isClobber()) {
    DEBUG(
      // fast print dep, using operator<< on instruction is too slow.
      dbgs() << "GVN: load ";
      L->printAsOperand(dbgs());
      Instruction *I = Dep.getInst();
      dbgs() << " is clobbered by " << *I << '\n';
    );
    return false;
  }

  // If it is defined in another block, try harder.
  if (Dep.isNonLocal())
    return processNonLocalLoad(L);

  if (!Dep.isDef()) {
    DEBUG(
      // fast print dep, using operator<< on instruction is too slow.
      dbgs() << "GVN: load ";
      L->printAsOperand(dbgs());
      dbgs() << " has unknown dependence\n";
    );
    return false;
  }

  Instruction *DepInst = Dep.getInst();
  if (StoreInst *DepSI = dyn_cast<StoreInst>(DepInst)) {
    Value *StoredVal = DepSI->getValueOperand();

    // The store and load are to a must-aliased pointer, but they may not
    // actually have the same type.  See if we know how to reuse the stored
    // value (depending on its type).
    if (StoredVal->getType() != L->getType()) {
      IRBuilder<> Builder(L);
      StoredVal =
          CoerceAvailableValueToLoadType(StoredVal, L->getType(), Builder, DL);
      if (!StoredVal)
        return false;

      DEBUG(dbgs() << "GVN COERCED STORE:\n" << *DepSI << '\n' << *StoredVal
                   << '\n' << *L << "\n\n\n");
    }

    // Remove it!
    L->replaceAllUsesWith(StoredVal);
    if (StoredVal->getType()->getScalarType()->isPointerTy())
      MD->invalidateCachedPointerInfo(StoredVal);
    markInstructionForDeletion(L);
    ++NumGVNLoad;
    return true;
  }

  if (LoadInst *DepLI = dyn_cast<LoadInst>(DepInst)) {
    Value *AvailableVal = DepLI;

    // The loads are of a must-aliased pointer, but they may not actually have
    // the same type.  See if we know how to reuse the previously loaded value
    // (depending on its type).
    if (DepLI->getType() != L->getType()) {
      IRBuilder<> Builder(L);
      AvailableVal =
          CoerceAvailableValueToLoadType(DepLI, L->getType(), Builder, DL);
      if (!AvailableVal)
        return false;

      DEBUG(dbgs() << "GVN COERCED LOAD:\n" << *DepLI << "\n" << *AvailableVal
                   << "\n" << *L << "\n\n\n");
    }

    // Remove it!
    patchAndReplaceAllUsesWith(L, AvailableVal);
    if (DepLI->getType()->getScalarType()->isPointerTy())
      MD->invalidateCachedPointerInfo(DepLI);
    markInstructionForDeletion(L);
    ++NumGVNLoad;
    return true;
  }

  // If this load really doesn't depend on anything, then we must be loading an
  // undef value.  This can happen when loading for a fresh allocation with no
  // intervening stores, for example.
  if (isa<AllocaInst>(DepInst) || isMallocLikeFn(DepInst, TLI)) {
    L->replaceAllUsesWith(UndefValue::get(L->getType()));
    markInstructionForDeletion(L);
    ++NumGVNLoad;
    return true;
  }

  // If this load occurs either right after a lifetime begin,
  // then the loaded value is undefined.
  if (IntrinsicInst *II = dyn_cast<IntrinsicInst>(DepInst)) {
    if (II->getIntrinsicID() == Intrinsic::lifetime_start) {
      L->replaceAllUsesWith(UndefValue::get(L->getType()));
      markInstructionForDeletion(L);
      ++NumGVNLoad;
      return true;
    }
  }

  // If this load follows a calloc (which zero initializes memory),
  // then the loaded value is zero
  if (isCallocLikeFn(DepInst, TLI)) {
    L->replaceAllUsesWith(Constant::getNullValue(L->getType()));
    markInstructionForDeletion(L);
    ++NumGVNLoad;
    return true;
  }

  return false;
}

// In order to find a leader for a given value number at a
// specific basic block, we first obtain the list of all Values for that number,
// and then scan the list to find one whose block dominates the block in
// question.  This is fast because dominator tree queries consist of only
// a few comparisons of DFS numbers.
Value *GVN::findLeader(const BasicBlock *BB, uint32_t num) {
  LeaderTableEntry Vals = LeaderTable[num];
  if (!Vals.Val) return nullptr;

  Value *Val = nullptr;
  if (DT->dominates(Vals.BB, BB)) {
    Val = Vals.Val;

    // llvmberry::ValidationUnit::GetInstance()->intrude(
    //     [&Vals](llvmberry::Dictionary &data, llvmberry::CoreHint &hints)
    //     { data.get<llvmberry::ArgForGVNReplace>()->BB = Vals.BB; });
    llvmberry::intrude([&Vals, &BB]() {
      llvmberry::PassDictionary &pdata = llvmberry::PassDictionary::GetInstance();
      pdata.get<llvmberry::ArgForGVNReplace>()->BB = Vals.BB;
      pdata.get<llvmberry::ArgForGVNPRE>()->prevLeaderBBs[BB] = Vals.BB;
    });
    if (isa<Constant>(Val)) return Val;
  }

  LeaderTableEntry* Next = Vals.Next;
  while (Next) {
    if (DT->dominates(Next->BB, BB)) {
      if (isa<Constant>(Next->Val)) {
        llvmberry::intrude([&Next, &BB]() {
          llvmberry::PassDictionary &pdata = llvmberry::PassDictionary::GetInstance();
          pdata.get<llvmberry::ArgForGVNReplace>()->BB = Next->BB;
          pdata.get<llvmberry::ArgForGVNPRE>()->prevLeaderBBs[BB] = Next->BB;
        });
        return Next->Val;
      }
      if (!Val) {
        Val = Next->Val;
        llvmberry::intrude([&Next, &BB]() {
          llvmberry::PassDictionary &pdata = llvmberry::PassDictionary::GetInstance();
          pdata.get<llvmberry::ArgForGVNReplace>()->BB = Next->BB;
          pdata.get<llvmberry::ArgForGVNPRE>()->prevLeaderBBs[BB] = Next->BB;
        });
      }
    }

    Next = Next->Next;
  }

  return Val;
}

/// There is an edge from 'Src' to 'Dst'.  Return
/// true if every path from the entry block to 'Dst' passes via this edge.  In
/// particular 'Dst' must not be reachable via another edge from 'Src'.
static bool isOnlyReachableViaThisEdge(const BasicBlockEdge &E,
                                       DominatorTree *DT) {
  // While in theory it is interesting to consider the case in which Dst has
  // more than one predecessor, because Dst might be part of a loop which is
  // only reachable from Src, in practice it is pointless since at the time
  // GVN runs all such loops have preheaders, which means that Dst will have
  // been changed to have only one predecessor, namely Src.
  const BasicBlock *Pred = E.getEnd()->getSinglePredecessor();
  const BasicBlock *Src = E.getStart();
  assert((!Pred || Pred == Src) && "No edge between these basic blocks!");
  (void)Src;
  return Pred != nullptr;
}

/// The given values are known to be equal in every block
/// dominated by 'Root'.  Exploit this, for example by replacing 'LHS' with
/// 'RHS' everywhere in the scope.  Returns whether a change was made.
bool GVN::propagateEquality(Value *LHS, Value *RHS,
                            const BasicBlockEdge &Root) {
  SmallVector<std::pair<Value*, Value*>, 4> Worklist;
  Worklist.push_back(std::make_pair(LHS, RHS));
  bool Changed = false;
  // For speed, compute a conservative fast approximation to
  // DT->dominates(Root, Root.getEnd());
  bool RootDominatesEnd = isOnlyReachableViaThisEdge(Root, DT);

  while (!Worklist.empty()) {
    std::pair<Value*, Value*> Item = Worklist.pop_back_val();
    LHS = Item.first; RHS = Item.second;

    if (LHS == RHS) continue;
    assert(LHS->getType() == RHS->getType() && "Equality but unequal types!");

    // Don't try to propagate equalities between constants.
    if (isa<Constant>(LHS) && isa<Constant>(RHS)) continue;

    // Prefer a constant on the right-hand side, or an Argument if no constants.
    if (isa<Constant>(LHS) || (isa<Argument>(LHS) && !isa<Constant>(RHS)))
      std::swap(LHS, RHS);
    assert((isa<Argument>(LHS) || isa<Instruction>(LHS)) && "Unexpected value!");

    // If there is no obvious reason to prefer the left-hand side over the
    // right-hand side, ensure the longest lived term is on the right-hand side,
    // so the shortest lived term will be replaced by the longest lived.
    // This tends to expose more simplifications.
    uint32_t LVN = VN.lookup_or_add(LHS);
    if ((isa<Argument>(LHS) && isa<Argument>(RHS)) ||
        (isa<Instruction>(LHS) && isa<Instruction>(RHS))) {
      // Move the 'oldest' value to the right-hand side, using the value number
      // as a proxy for age.
      uint32_t RVN = VN.lookup_or_add(RHS);
      if (LVN < RVN) {
        std::swap(LHS, RHS);
        LVN = RVN;
      }
    }

    // If value numbering later sees that an instruction in the scope is equal
    // to 'LHS' then ensure it will be turned into 'RHS'.  In order to preserve
    // the invariant that instructions only occur in the leader table for their
    // own value number (this is used by removeFromLeaderTable), do not do this
    // if RHS is an instruction (if an instruction in the scope is morphed into
    // LHS then it will be turned into RHS by the next GVN iteration anyway, so
    // using the leader table is about compiling faster, not optimizing better).
    // The leader table only tracks basic blocks, not edges. Only add to if we
    // have the simple case where the edge dominates the end.
    if (RootDominatesEnd && !isa<Instruction>(RHS))
      addToLeaderTable(LVN, RHS, Root.getEnd());

    // Replace all occurrences of 'LHS' with 'RHS' everywhere in the scope.  As
    // LHS always has at least one use that is not dominated by Root, this will
    // never do anything if LHS has only one use.
    if (!LHS->hasOneUse()) {
      unsigned NumReplacements = replaceDominatedUsesWith(LHS, RHS, *DT, Root);
      Changed |= NumReplacements > 0;
      NumGVNEqProp += NumReplacements;
    }

    // Now try to deduce additional equalities from this one. For example, if
    // the known equality was "(A != B)" == "false" then it follows that A and B
    // are equal in the scope. Only boolean equalities with an explicit true or
    // false RHS are currently supported.
    if (!RHS->getType()->isIntegerTy(1))
      // Not a boolean equality - bail out.
      continue;
    ConstantInt *CI = dyn_cast<ConstantInt>(RHS);
    if (!CI)
      // RHS neither 'true' nor 'false' - bail out.
      continue;
    // Whether RHS equals 'true'.  Otherwise it equals 'false'.
    bool isKnownTrue = CI->isAllOnesValue();
    bool isKnownFalse = !isKnownTrue;

    // If "A && B" is known true then both A and B are known true.  If "A || B"
    // is known false then both A and B are known false.
    Value *A, *B;
    if ((isKnownTrue && match(LHS, m_And(m_Value(A), m_Value(B)))) ||
        (isKnownFalse && match(LHS, m_Or(m_Value(A), m_Value(B))))) {
      Worklist.push_back(std::make_pair(A, RHS));
      Worklist.push_back(std::make_pair(B, RHS));
      continue;
    }

    // If we are propagating an equality like "(A == B)" == "true" then also
    // propagate the equality A == B.  When propagating a comparison such as
    // "(A >= B)" == "true", replace all instances of "A < B" with "false".
    if (CmpInst *Cmp = dyn_cast<CmpInst>(LHS)) {
      Value *Op0 = Cmp->getOperand(0), *Op1 = Cmp->getOperand(1);

      // If "A == B" is known true, or "A != B" is known false, then replace
      // A with B everywhere in the scope.
      if ((isKnownTrue && Cmp->getPredicate() == CmpInst::ICMP_EQ) ||
          (isKnownFalse && Cmp->getPredicate() == CmpInst::ICMP_NE))
        Worklist.push_back(std::make_pair(Op0, Op1));

      // Handle the floating point versions of equality comparisons too.
      if ((isKnownTrue && Cmp->getPredicate() == CmpInst::FCMP_OEQ) ||
          (isKnownFalse && Cmp->getPredicate() == CmpInst::FCMP_UNE)) {

        // Floating point -0.0 and 0.0 compare equal, so we can only
        // propagate values if we know that we have a constant and that
        // its value is non-zero.
        
        // FIXME: We should do this optimization if 'no signed zeros' is
        // applicable via an instruction-level fast-math-flag or some other
        // indicator that relaxed FP semantics are being used.

        if (isa<ConstantFP>(Op1) && !cast<ConstantFP>(Op1)->isZero())
          Worklist.push_back(std::make_pair(Op0, Op1));
      }
 
      // If "A >= B" is known true, replace "A < B" with false everywhere.
      CmpInst::Predicate NotPred = Cmp->getInversePredicate();
      Constant *NotVal = ConstantInt::get(Cmp->getType(), isKnownFalse);
      // Since we don't have the instruction "A < B" immediately to hand, work
      // out the value number that it would have and use that to find an
      // appropriate instruction (if any).
      uint32_t NextNum = VN.getNextUnusedValueNumber();
      uint32_t Num = VN.lookup_or_add_cmp(Cmp->getOpcode(), NotPred, Op0, Op1);
      // If the number we were assigned was brand new then there is no point in
      // looking for an instruction realizing it: there cannot be one!
      if (Num < NextNum) {
        Value *NotCmp = findLeader(Root.getEnd(), Num);
        if (NotCmp && isa<Instruction>(NotCmp)) {
          unsigned NumReplacements =
            replaceDominatedUsesWith(NotCmp, NotVal, *DT, Root);
          Changed |= NumReplacements > 0;
          NumGVNEqProp += NumReplacements;
        }
      }
      // Ensure that any instruction in scope that gets the "A < B" value number
      // is replaced with false.
      // The leader table only tracks basic blocks, not edges. Only add to if we
      // have the simple case where the edge dominates the end.
      if (RootDominatesEnd)
        addToLeaderTable(Num, NotVal, Root.getEnd());

      continue;
    }
  }

  return Changed;
}

/// When calculating availability, handle an instruction
/// by inserting it into the appropriate sets
bool GVN::processInstruction(Instruction *I) {
  // Ignore dbg info intrinsics.
  if (isa<DbgInfoIntrinsic>(I))
    return false;

  // If the instruction can be easily simplified then do so now in preference
  // to value numbering it.  Value numbering often exposes redundancies, for
  // example if it determines that %y is equal to %x then the instruction
  // "%z = and i32 %x, %y" becomes "%z = and i32 %x, %x" which we now simplify.
  const DataLayout &DL = I->getModule()->getDataLayout();
  if (Value *V = SimplifyInstruction(I, DL, TLI, DT, AC)) {
    I->replaceAllUsesWith(V);
    if (MD && V->getType()->getScalarType()->isPointerTy())
      MD->invalidateCachedPointerInfo(V);
    markInstructionForDeletion(I);
    ++NumGVNSimpl;
    return true;
  }

  if (LoadInst *LI = dyn_cast<LoadInst>(I)) {
    if (processLoad(LI))
      return true;

    unsigned Num = VN.lookup_or_add(LI);
    addToLeaderTable(Num, LI, LI->getParent());
    return false;
  }

  // For conditional branches, we can perform simple conditional propagation on
  // the condition value itself.
  if (BranchInst *BI = dyn_cast<BranchInst>(I)) {
    if (!BI->isConditional())
      return false;

    if (isa<Constant>(BI->getCondition()))
      return processFoldableCondBr(BI);

    Value *BranchCond = BI->getCondition();
    BasicBlock *TrueSucc = BI->getSuccessor(0);
    BasicBlock *FalseSucc = BI->getSuccessor(1);
    // Avoid multiple edges early.
    if (TrueSucc == FalseSucc)
      return false;

    BasicBlock *Parent = BI->getParent();
    bool Changed = false;

    Value *TrueVal = ConstantInt::getTrue(TrueSucc->getContext());
    BasicBlockEdge TrueE(Parent, TrueSucc);
    Changed |= propagateEquality(BranchCond, TrueVal, TrueE);

    Value *FalseVal = ConstantInt::getFalse(FalseSucc->getContext());
    BasicBlockEdge FalseE(Parent, FalseSucc);
    Changed |= propagateEquality(BranchCond, FalseVal, FalseE);

    return Changed;
  }

  // For switches, propagate the case values into the case destinations.
  if (SwitchInst *SI = dyn_cast<SwitchInst>(I)) {
    Value *SwitchCond = SI->getCondition();
    BasicBlock *Parent = SI->getParent();
    bool Changed = false;

    // Remember how many outgoing edges there are to every successor.
    SmallDenseMap<BasicBlock *, unsigned, 16> SwitchEdges;
    for (unsigned i = 0, n = SI->getNumSuccessors(); i != n; ++i)
      ++SwitchEdges[SI->getSuccessor(i)];

    for (SwitchInst::CaseIt i = SI->case_begin(), e = SI->case_end();
         i != e; ++i) {
      BasicBlock *Dst = i.getCaseSuccessor();
      // If there is only a single edge, propagate the case value into it.
      if (SwitchEdges.lookup(Dst) == 1) {
        BasicBlockEdge E(Parent, Dst);
        Changed |= propagateEquality(SwitchCond, i.getCaseValue(), E);
      }
    }
    return Changed;
  }

  // Instructions with void type don't return a value, so there's
  // no point in trying to find redundancies in them.
  if (I->getType()->isVoidTy()) return false;

  uint32_t NextNum = VN.getNextUnusedValueNumber();
  unsigned Num = VN.lookup_or_add(I);

  // Allocations are always uniquely numbered, so we can save time and memory
  // by fast failing them.
  if (isa<AllocaInst>(I) || isa<TerminatorInst>(I) || isa<PHINode>(I)) {
    addToLeaderTable(Num, I, I->getParent());
    return false;
  }

  // If the number we were assigned was a brand new VN, then we don't
  // need to do a lookup to see if the number already exists
  // somewhere in the domtree: it can't!
  if (Num >= NextNum) {
    addToLeaderTable(Num, I, I->getParent());
    return false;
  }

  // Perform fast-path value-number based elimination of values inherited from
  // dominators.
  Value *repl = findLeader(I->getParent(), Num);
  if (!repl) {
    // Failure, just remember this instance for future use.
    addToLeaderTable(Num, I, I->getParent());
    return false;
  }

  // Remove it!
  llvmberry::intrude([this]() {
    llvmberry::PassDictionary &pdata = llvmberry::PassDictionary::GetInstance();
    pdata.get<llvmberry::ArgForGVNReplace>()->isGVNReplace = true;
    pdata.get<llvmberry::ArgForGVNReplace>()->VNptr = &VN;
  });
  patchAndReplaceAllUsesWith(I, repl);

  if (MD && repl->getType()->getScalarType()->isPointerTy())
    MD->invalidateCachedPointerInfo(repl);
  markInstructionForDeletion(I);
  return true;
}

/// runOnFunction - This is the main transformation entry point for a function.
bool GVN::runOnFunction(Function& F) {
  if (skipOptnoneFunction(F))
    return false;

  llvmberry::ValidationUnit::StartPass(llvmberry::ValidationUnit::GVN);
  llvmberry::intrude([]() {
    llvmberry::PassDictionary &pdata = llvmberry::PassDictionary::GetInstance();
    pdata.create<llvmberry::ArgForGVNReplace>();
    pdata.create<llvmberry::ArgForGVNPRE>();
  });
  if (!NoLoads)
    MD = &getAnalysis<MemoryDependenceAnalysis>();
  DT = &getAnalysis<DominatorTreeWrapperPass>().getDomTree();
  AC = &getAnalysis<AssumptionCacheTracker>().getAssumptionCache(F);
  TLI = &getAnalysis<TargetLibraryInfoWrapperPass>().getTLI();
  VN.setAliasAnalysis(&getAnalysis<AliasAnalysis>());
  VN.setMemDep(MD);
  VN.setDomTree(DT);

  bool Changed = false;
  bool ShouldContinue = true;

  // Merge unconditional branches, allowing PRE to catch more
  // optimization opportunities.
  for (Function::iterator FI = F.begin(), FE = F.end(); FI != FE; ) {
    BasicBlock *BB = FI++;

    bool removedBlock = MergeBlockIntoPredecessor(
        BB, DT, /* LoopInfo */ nullptr, VN.getAliasAnalysis(), MD);
    if (removedBlock) ++NumGVNBlocks;

    Changed |= removedBlock;
  }

  unsigned Iteration = 0;
  while (ShouldContinue) {
    DEBUG(dbgs() << "GVN iteration: " << Iteration << "\n");
    ShouldContinue = iterateOnFunction(F);
    Changed |= ShouldContinue;
    ++Iteration;
  }

  if (EnablePRE) {
    // Fabricate val-num for dead-code in order to suppress assertion in
    // performPRE().
    assignValNumForDeadCode();
    bool PREChanged = true;
    while (PREChanged) {
      PREChanged = performPRE(F);
      Changed |= PREChanged;
    }
  }

  // FIXME: Should perform GVN again after PRE does something.  PRE can move
  // computations into blocks where they become fully redundant.  Note that
  // we can't do this until PRE's critical edge splitting updates memdep.
  // Actually, when this happens, we should just fully integrate PRE into GVN.

  cleanupGlobalSets();
  // Do not cleanup DeadBlocks in cleanupGlobalSets() as it's called for each
  // iteration. 
  DeadBlocks.clear();

  llvmberry::intrude([]() {
    llvmberry::PassDictionary &pdata = llvmberry::PassDictionary::GetInstance();
    pdata.erase<llvmberry::ArgForGVNReplace>();
    pdata.erase<llvmberry::ArgForGVNPRE>();
  });

  llvmberry::ValidationUnit::EndPass();

  return Changed;
}


bool GVN::processBlock(BasicBlock *BB) {
  // FIXME: Kill off InstrsToErase by doing erasing eagerly in a helper function
  // (and incrementing BI before processing an instruction).
  assert(InstrsToErase.empty() &&
         "We expect InstrsToErase to be empty across iterations");
  if (DeadBlocks.count(BB))
    return false;

  bool ChangedFunction = false;

  for (BasicBlock::iterator BI = BB->begin(), BE = BB->end();
       BI != BE;) {
    ChangedFunction |= processInstruction(BI);
    if (InstrsToErase.empty()) {
      ++BI;
      continue;
    }

    // If we need some instructions deleted, do it now.
    NumGVNInstr += InstrsToErase.size();

    // Avoid iterator invalidation.
    bool AtStart = BI == BB->begin();
    if (!AtStart)
      --BI;

    for (SmallVectorImpl<Instruction *>::iterator I = InstrsToErase.begin(),
         E = InstrsToErase.end(); I != E; ++I) {
      DEBUG(dbgs() << "GVN removed: " << **I << '\n');
      if (MD) MD->removeInstruction(*I);
      DEBUG(verifyRemoved(*I));
      llvmberry::name_instructions(*((*I)->getParent()->getParent()));
      llvmberry::ValidationUnit::Begin("GVN_dead_code_elim1",
                                       (*I)->getParent()->getParent());
      llvmberry::generateHintForGVNDCE(**I);
      (*I)->eraseFromParent();
      llvmberry::ValidationUnit::End();
    }
    InstrsToErase.clear();

    if (AtStart)
      BI = BB->begin();
    else
      ++BI;
  }

  return ChangedFunction;
}

// Instantiate an expression in a predecessor that lacked it.
bool GVN::performScalarPREInsertion(Instruction *Instr, BasicBlock *Pred,
                                    unsigned int ValNo) {
  // Because we are going top-down through the block, all value numbers
  // will be available in the predecessor by the time we need them.  Any
  // that weren't originally present will have been instantiated earlier
  // in this loop.
  bool success = true;
  for (unsigned i = 0, e = Instr->getNumOperands(); i != e; ++i) {
    Value *Op = Instr->getOperand(i);
    if (isa<Argument>(Op) || isa<Constant>(Op) || isa<GlobalValue>(Op))
      continue;

    if (Value *V = findLeader(Pred, VN.lookup(Op))) {
      Instr->setOperand(i, V);
    } else {
      success = false;
      break;
    }
  }

  // Fail out if we encounter an operand that is not available in
  // the PRE predecessor.  This is typically because of loads which
  // are not value numbered precisely.
  if (!success)
    return false;

  Instr->insertBefore(Pred->getTerminator());
  Instr->setName(Instr->getName() + ".pre");
  Instr->setDebugLoc(Instr->getDebugLoc());
  VN.add(Instr, ValNo);

  // Update the availability map to include the new instruction.
  addToLeaderTable(ValNo, Instr, Pred);
  return true;
}

bool GVN::performScalarPRE(Instruction *CurInst) {
  SmallVector<std::pair<Value*, BasicBlock*>, 8> predMap;

  if (isa<AllocaInst>(CurInst) || isa<TerminatorInst>(CurInst) ||
      isa<PHINode>(CurInst) || CurInst->getType()->isVoidTy() ||
      CurInst->mayReadFromMemory() || CurInst->mayHaveSideEffects() ||
      isa<DbgInfoIntrinsic>(CurInst))
    return false;

  // Don't do PRE on compares. The PHI would prevent CodeGenPrepare from
  // sinking the compare again, and it would force the code generator to
  // move the i1 from processor flags or predicate registers into a general
  // purpose register.
  if (isa<CmpInst>(CurInst))
    return false;

  // We don't currently value number ANY inline asm calls.
  if (CallInst *CallI = dyn_cast<CallInst>(CurInst))
    if (CallI->isInlineAsm())
      return false;

  uint32_t ValNo = VN.lookup(CurInst);

  // Look for the predecessors for PRE opportunities.  We're
  // only trying to solve the basic diamond case, where
  // a value is computed in the successor and one predecessor,
  // but not the other.  We also explicitly disallow cases
  // where the successor is its own predecessor, because they're
  // more complicated to get right.
  unsigned NumWith = 0;
  unsigned NumWithout = 0;
  BasicBlock *PREPred = nullptr;
  BasicBlock *CurrentBlock = CurInst->getParent();
  predMap.clear();

  llvmberry::intrude([]() {
    llvmberry::PassDictionary &pdata = llvmberry::PassDictionary::GetInstance();
    pdata.get<llvmberry::ArgForGVNPRE>()->prevLeaderBBs.clear();
  });
  for (pred_iterator PI = pred_begin(CurrentBlock), PE = pred_end(CurrentBlock);
       PI != PE; ++PI) {
    BasicBlock *P = *PI;
    // We're not interested in PRE where the block is its
    // own predecessor, or in blocks with predecessors
    // that are not reachable.
    if (P == CurrentBlock) {
      NumWithout = 2;
      break;
    } else if (!DT->isReachableFromEntry(P)) {
      NumWithout = 2;
      break;
    }

    Value *predV = findLeader(P, ValNo);
    if (!predV) {
      predMap.push_back(std::make_pair(static_cast<Value *>(nullptr), P));
      PREPred = P;
      ++NumWithout;
    } else if (predV == CurInst) {
      /* CurInst dominates this predecessor. */
      NumWithout = 2;
      break;
    } else {
      predMap.push_back(std::make_pair(predV, P));
      ++NumWith;
    }
  }

  // Don't do PRE when it might increase code size, i.e. when
  // we would need to insert instructions in more than one pred.
  if (NumWithout > 1 || NumWith == 0)
    return false;

  // We may have a case where all predecessors have the instruction,
  // and we just need to insert a phi node. Otherwise, perform
  // insertion.
  Instruction *PREInstr = nullptr;

  if (NumWithout != 0) {
    // Don't do PRE across indirect branch.
    if (isa<IndirectBrInst>(PREPred->getTerminator()))
      return false;

    // We can't do PRE safely on a critical edge, so instead we schedule
    // the edge to be split and perform the PRE the next time we iterate
    // on the function.
    unsigned SuccNum = GetSuccessorNumber(PREPred, CurrentBlock);
    if (isCriticalEdge(PREPred->getTerminator(), SuccNum)) {
      toSplit.push_back(std::make_pair(PREPred->getTerminator(), SuccNum));
      return false;
    }
    // We need to insert somewhere, so let's give it a shot
    PREInstr = CurInst->clone();
    if (!performScalarPREInsertion(PREInstr, PREPred, ValNo)) {
      // If we failed insertion, make sure we remove the instruction.
      DEBUG(verifyRemoved(PREInstr));
      delete PREInstr;
      return false;
    }
  }

  // Either we should have filled in the PRE instruction, or we should
  // not have needed insertions.
  assert (PREInstr != nullptr || NumWithout == 0);

  ++NumGVNPRE;

  // Create a PHI to make the value available in this block.
  PHINode *Phi =
      PHINode::Create(CurInst->getType(), predMap.size(),
                      CurInst->getName() + ".pre-phi", CurrentBlock->begin());
  for (unsigned i = 0, e = predMap.size(); i != e; ++i) {
    if (Value *V = predMap[i].first)
      Phi->addIncoming(V, predMap[i].second);
    else
      Phi->addIncoming(PREInstr, PREPred);
  }

  VN.add(Phi, ValNo);
  addToLeaderTable(ValNo, Phi, CurrentBlock);
  Phi->setDebugLoc(CurInst->getDebugLoc());

  // Validation hint generation for PRE
  llvmberry::intrude([&CurInst, &Phi]() {
    PREAnalysisResult *PREAR = new PREAnalysisResult(CurInst, Phi);
    llvmberry::name_instructions(*(CurInst->getParent()->getParent()));
    if (PREAR->PrevPRE.size() == 0)
      llvmberry::ValidationUnit::Begin("GVN_PRE",
                                       CurInst->getParent()->getParent());
    else
      llvmberry::ValidationUnit::Begin("GVN_PRE_hard",
                                       CurInst->getParent()->getParent());

    // Somehow get [ INSN(CurInst) >= Var(Phi) ] in start_of_block(Phi)
    generateHintForPRE(CurInst, Phi);

    llvmberry::ValidationUnit::GetInstance()->intrude([&CurInst, &Phi](
        llvmberry::ValidationUnit::Dictionary &data,
        llvmberry::CoreHint &hints) {
      std::string CurInst_id = llvmberry::getVariable(*CurInst);
      std::string Phi_id = llvmberry::getVariable(*Phi);

      // Propagate [ INSN(CurInst) >= Var(Phi) ] until CurInst
      PROPAGATE(LESSDEF(INSN(*CurInst), VAR(Phi_id, Physical), SRC),
                BOUNDS(llvmberry::TyPosition::make_start_of_block(
                           llvmberry::Source,
                           llvmberry::getBasicBlockIndex(Phi->getParent())),
                       INSTPOS(SRC, CurInst)));

      // Transitivity [ Var(CurInst) >= INSN(CurInst) >= Var(Phi) ]
      INFRULE(INSTPOS(SRC, CurInst),
              llvmberry::ConsTransitivity::make(VAR(CurInst_id, Physical),
                                                INSN(*CurInst),
                                                VAR(Phi_id, Physical)));

      // TODO: for all uses of CurInst
      // replace curInst -> phi
      for (auto UI = CurInst->use_begin(); UI != CurInst->use_end(); ++UI) {
        if (Instruction *userI = dyn_cast<Instruction>(UI->getUser())) {
          std::string userI_id = llvmberry::getVariable(*userI);

          std::string prev_block_name = "";
          if (isa<PHINode>(userI)) {
            BasicBlock *bb_from =
                dyn_cast<PHINode>(userI)->getIncomingBlock(*UI);
            prev_block_name = llvmberry::getBasicBlockIndex(bb_from);
          }

          hints.appendToDescription("userI: " + ((*userI).getName()).str());
          hints.appendToDescription("userI_id: " + userI_id);
          hints.appendToDescription(
              "userI's parent's index: " +
              llvmberry::getBasicBlockIndex(userI->getParent()));
          hints.appendToDescription("userI hasName: " +
                                    std::to_string(userI->hasName()));

          PROPAGATE(
              LESSDEF(VAR(CurInst_id, Physical), VAR(Phi_id, Physical), SRC),
              BOUNDS(INSTPOS(SRC, CurInst),
                     llvmberry::TyPosition::make(llvmberry::Source, *userI,
                                                 prev_block_name)));
        } else {
          hints.appendToDescription("userNotInstruction");
          hints.setReturnCodeToFail();
          return;
        }
      }
    });
  });

  CurInst->replaceAllUsesWith(Phi);

  llvmberry::intrude([] {
    llvmberry::ValidationUnit::End();
  });

  if (Phi->getType()->getScalarType()->isPointerTy()) {
    // Because we have added a PHI-use of the pointer value, it has now
    // "escaped" from alias analysis' perspective.  We need to inform
    // AA of this.
    for (unsigned ii = 0, ee = Phi->getNumIncomingValues(); ii != ee; ++ii) {
      unsigned jj = PHINode::getOperandNumForIncomingValue(ii);
      VN.getAliasAnalysis()->addEscapingUse(Phi->getOperandUse(jj));
    }

    if (MD)
      MD->invalidateCachedPointerInfo(Phi);
  }
  VN.erase(CurInst);
  removeFromLeaderTable(ValNo, CurInst, CurrentBlock);

  DEBUG(dbgs() << "GVN PRE removed: " << *CurInst << '\n');
  if (MD)
    MD->removeInstruction(CurInst);
  DEBUG(verifyRemoved(CurInst));
  llvmberry::name_instructions(*(CurInst->getParent()->getParent()));
  llvmberry::ValidationUnit::Begin("GVN_dead_code_elim3",
                                   CurInst->getParent()->getParent());
  llvmberry::generateHintForGVNDCE(*CurInst);
  CurInst->eraseFromParent();
  llvmberry::ValidationUnit::End();
  ++NumGVNInstr;
  
  return true;
}

/// Perform a purely local form of PRE that looks for diamond
/// control flow patterns and attempts to perform simple PRE at the join point.
bool GVN::performPRE(Function &F) {
  bool Changed = false;
  for (BasicBlock *CurrentBlock : depth_first(&F.getEntryBlock())) {
    // Nothing to PRE in the entry block.
    if (CurrentBlock == &F.getEntryBlock())
      continue;

    // Don't perform PRE on a landing pad.
    if (CurrentBlock->isLandingPad())
      continue;

    for (BasicBlock::iterator BI = CurrentBlock->begin(),
                              BE = CurrentBlock->end();
         BI != BE;) {
      Instruction *CurInst = BI++;
      llvmberry::intrude([]() {
        llvmberry::PassDictionary &pdata =
            llvmberry::PassDictionary::GetInstance();
        pdata.get<llvmberry::ArgForGVNPRE>()->isFromNonLocalLoad = false;
      });
      Changed = performScalarPRE(CurInst);
    }
  }

  if (splitCriticalEdges())
    Changed = true;

  return Changed;
}

/// Split the critical edge connecting the given two blocks, and return
/// the block inserted to the critical edge.
BasicBlock *GVN::splitCriticalEdges(BasicBlock *Pred, BasicBlock *Succ) {
  BasicBlock *BB = SplitCriticalEdge(
      Pred, Succ, CriticalEdgeSplittingOptions(getAliasAnalysis(), DT));
  if (MD)
    MD->invalidateCachedPredecessors();
  return BB;
}

/// Split critical edges found during the previous
/// iteration that may enable further optimization.
bool GVN::splitCriticalEdges() {
  if (toSplit.empty())
    return false;
  do {
    std::pair<TerminatorInst*, unsigned> Edge = toSplit.pop_back_val();
    SplitCriticalEdge(Edge.first, Edge.second,
                      CriticalEdgeSplittingOptions(getAliasAnalysis(), DT));
  } while (!toSplit.empty());
  if (MD) MD->invalidateCachedPredecessors();
  return true;
}

/// Executes one iteration of GVN
bool GVN::iterateOnFunction(Function &F) {
  cleanupGlobalSets();

  // Top-down walk of the dominator tree
  bool Changed = false;
  // Save the blocks this function have before transformation begins. GVN may
  // split critical edge, and hence may invalidate the RPO/DT iterator.
  //
  std::vector<BasicBlock *> BBVect;
  BBVect.reserve(256);
  // Needed for value numbering with phi construction to work.
  ReversePostOrderTraversal<Function *> RPOT(&F);
  for (ReversePostOrderTraversal<Function *>::rpo_iterator RI = RPOT.begin(),
                                                           RE = RPOT.end();
       RI != RE; ++RI)
    BBVect.push_back(*RI);

  for (std::vector<BasicBlock *>::iterator I = BBVect.begin(), E = BBVect.end();
       I != E; I++)
    Changed |= processBlock(*I);

  return Changed;
}

void GVN::cleanupGlobalSets() {
  VN.clear();
  LeaderTable.clear();
  TableAllocator.Reset();
}

/// Verify that the specified instruction does not occur in our
/// internal data structures.
void GVN::verifyRemoved(const Instruction *Inst) const {
  VN.verifyRemoved(Inst);

  // Walk through the value number scope to make sure the instruction isn't
  // ferreted away in it.
  for (DenseMap<uint32_t, LeaderTableEntry>::const_iterator
       I = LeaderTable.begin(), E = LeaderTable.end(); I != E; ++I) {
    const LeaderTableEntry *Node = &I->second;
    assert(Node->Val != Inst && "Inst still in value numbering scope!");

    while (Node->Next) {
      Node = Node->Next;
      assert(Node->Val != Inst && "Inst still in value numbering scope!");
    }
  }
}

/// BB is declared dead, which implied other blocks become dead as well. This
/// function is to add all these blocks to "DeadBlocks". For the dead blocks'
/// live successors, update their phi nodes by replacing the operands
/// corresponding to dead blocks with UndefVal.
void GVN::addDeadBlock(BasicBlock *BB) {
  SmallVector<BasicBlock *, 4> NewDead;
  SmallSetVector<BasicBlock *, 4> DF;

  NewDead.push_back(BB);
  while (!NewDead.empty()) {
    BasicBlock *D = NewDead.pop_back_val();
    if (DeadBlocks.count(D))
      continue;

    // All blocks dominated by D are dead.
    SmallVector<BasicBlock *, 8> Dom;
    DT->getDescendants(D, Dom);
    DeadBlocks.insert(Dom.begin(), Dom.end());
    
    // Figure out the dominance-frontier(D).
    for (SmallVectorImpl<BasicBlock *>::iterator I = Dom.begin(),
           E = Dom.end(); I != E; I++) {
      BasicBlock *B = *I;
      for (succ_iterator SI = succ_begin(B), SE = succ_end(B); SI != SE; SI++) {
        BasicBlock *S = *SI;
        if (DeadBlocks.count(S))
          continue;

        bool AllPredDead = true;
        for (pred_iterator PI = pred_begin(S), PE = pred_end(S); PI != PE; PI++)
          if (!DeadBlocks.count(*PI)) {
            AllPredDead = false;
            break;
          }

        if (!AllPredDead) {
          // S could be proved dead later on. That is why we don't update phi
          // operands at this moment.
          DF.insert(S);
        } else {
          // While S is not dominated by D, it is dead by now. This could take
          // place if S already have a dead predecessor before D is declared
          // dead.
          NewDead.push_back(S);
        }
      }
    }
  }

  // For the dead blocks' live successors, update their phi nodes by replacing
  // the operands corresponding to dead blocks with UndefVal.
  for(SmallSetVector<BasicBlock *, 4>::iterator I = DF.begin(), E = DF.end();
        I != E; I++) {
    BasicBlock *B = *I;
    if (DeadBlocks.count(B))
      continue;

    SmallVector<BasicBlock *, 4> Preds(pred_begin(B), pred_end(B));
    for (SmallVectorImpl<BasicBlock *>::iterator PI = Preds.begin(),
           PE = Preds.end(); PI != PE; PI++) {
      BasicBlock *P = *PI;

      if (!DeadBlocks.count(P))
        continue;

      if (isCriticalEdge(P->getTerminator(), GetSuccessorNumber(P, B))) {
        if (BasicBlock *S = splitCriticalEdges(P, B))
          DeadBlocks.insert(P = S);
      }

      for (BasicBlock::iterator II = B->begin(); isa<PHINode>(II); ++II) {
        PHINode &Phi = cast<PHINode>(*II);
        Phi.setIncomingValue(Phi.getBasicBlockIndex(P),
                             UndefValue::get(Phi.getType()));
      }
    }
  }
}

// If the given branch is recognized as a foldable branch (i.e. conditional
// branch with constant condition), it will perform following analyses and
// transformation.
//  1) If the dead out-coming edge is a critical-edge, split it. Let 
//     R be the target of the dead out-coming edge.
//  1) Identify the set of dead blocks implied by the branch's dead outcoming
//     edge. The result of this step will be {X| X is dominated by R}
//  2) Identify those blocks which haves at least one dead prodecessor. The
//     result of this step will be dominance-frontier(R).
//  3) Update the PHIs in DF(R) by replacing the operands corresponding to 
//     dead blocks with "UndefVal" in an hope these PHIs will optimized away.
//
// Return true iff *NEW* dead code are found.
bool GVN::processFoldableCondBr(BranchInst *BI) {
  if (!BI || BI->isUnconditional())
    return false;

  // If a branch has two identical successors, we cannot declare either dead.
  if (BI->getSuccessor(0) == BI->getSuccessor(1))
    return false;

  ConstantInt *Cond = dyn_cast<ConstantInt>(BI->getCondition());
  if (!Cond)
    return false;

  BasicBlock *DeadRoot = Cond->getZExtValue() ? 
                         BI->getSuccessor(1) : BI->getSuccessor(0);
  if (DeadBlocks.count(DeadRoot))
    return false;

  if (!DeadRoot->getSinglePredecessor())
    DeadRoot = splitCriticalEdges(BI->getParent(), DeadRoot);

  addDeadBlock(DeadRoot);
  return true;
}

// performPRE() will trigger assert if it comes across an instruction without
// associated val-num. As it normally has far more live instructions than dead
// instructions, it makes more sense just to "fabricate" a val-number for the
// dead code than checking if instruction involved is dead or not.
void GVN::assignValNumForDeadCode() {
  for (SetVector<BasicBlock *>::iterator I = DeadBlocks.begin(),
        E = DeadBlocks.end(); I != E; I++) {
    BasicBlock *BB = *I;
    for (BasicBlock::iterator II = BB->begin(), EE = BB->end();
          II != EE; II++) {
      Instruction *Inst = &*II;
      unsigned ValNum = VN.lookup_or_add(Inst);
      addToLeaderTable(ValNum, Inst, BB);
    }
  }
}
