//===- InstCombineAddSub.cpp ----------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the visit functions for add, fadd, sub, and fsub.
//
//===----------------------------------------------------------------------===//

#include "InstCombineInternal.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Analysis/InstructionSimplify.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/GetElementPtrTypeIterator.h"
#include "llvm/IR/PatternMatch.h"

#include "llvm/LLVMBerry/ValidationUnit.h"
#include "llvm/LLVMBerry/Structure.h"
#include "llvm/LLVMBerry/Infrules.h"
#include "llvm/LLVMBerry/InstCombine/InfrulesAddSub.h"
#include "llvm/LLVMBerry/Hintgen.h"

using namespace llvm;
using namespace llvm::PatternMatch;

#define DEBUG_TYPE "instcombine"

namespace {

  /// Class representing coefficient of floating-point addend.
  /// This class needs to be highly efficient, which is especially true for
  /// the constructor. As of I write this comment, the cost of the default
  /// constructor is merely 4-byte-store-zero (Assuming compiler is able to
  /// perform write-merging).
  ///
  class FAddendCoef {
  public:
    // The constructor has to initialize a APFloat, which is unnecessary for
    // most addends which have coefficient either 1 or -1. So, the constructor
    // is expensive. In order to avoid the cost of the constructor, we should
    // reuse some instances whenever possible. The pre-created instances
    // FAddCombine::Add[0-5] embodies this idea.
    //
    FAddendCoef() : IsFp(false), BufHasFpVal(false), IntVal(0) {}
    ~FAddendCoef();

    void set(short C) {
      assert(!insaneIntVal(C) && "Insane coefficient");
      IsFp = false; IntVal = C;
    }

    void set(const APFloat& C);

    void negate();

    bool isZero() const { return isInt() ? !IntVal : getFpVal().isZero(); }
    Value *getValue(Type *) const;

    // If possible, don't define operator+/operator- etc because these
    // operators inevitably call FAddendCoef's constructor which is not cheap.
    void operator=(const FAddendCoef &A);
    void operator+=(const FAddendCoef &A);
    void operator-=(const FAddendCoef &A);
    void operator*=(const FAddendCoef &S);

    bool isOne() const { return isInt() && IntVal == 1; }
    bool isTwo() const { return isInt() && IntVal == 2; }
    bool isMinusOne() const { return isInt() && IntVal == -1; }
    bool isMinusTwo() const { return isInt() && IntVal == -2; }

  private:
    bool insaneIntVal(int V) { return V > 4 || V < -4; }
    APFloat *getFpValPtr(void)
      { return reinterpret_cast<APFloat*>(&FpValBuf.buffer[0]); }
    const APFloat *getFpValPtr(void) const
      { return reinterpret_cast<const APFloat*>(&FpValBuf.buffer[0]); }

    const APFloat &getFpVal(void) const {
      assert(IsFp && BufHasFpVal && "Incorret state");
      return *getFpValPtr();
    }

    APFloat &getFpVal(void) {
      assert(IsFp && BufHasFpVal && "Incorret state");
      return *getFpValPtr();
    }

    bool isInt() const { return !IsFp; }

    // If the coefficient is represented by an integer, promote it to a
    // floating point.
    void convertToFpType(const fltSemantics &Sem);

    // Construct an APFloat from a signed integer.
    // TODO: We should get rid of this function when APFloat can be constructed
    //       from an *SIGNED* integer.
    APFloat createAPFloatFromInt(const fltSemantics &Sem, int Val);
  private:

    bool IsFp;

    // True iff FpValBuf contains an instance of APFloat.
    bool BufHasFpVal;

    // The integer coefficient of an individual addend is either 1 or -1,
    // and we try to simplify at most 4 addends from neighboring at most
    // two instructions. So the range of <IntVal> falls in [-4, 4]. APInt
    // is overkill of this end.
    short IntVal;

    AlignedCharArrayUnion<APFloat> FpValBuf;
  };

  /// FAddend is used to represent floating-point addend. An addend is
  /// represented as <C, V>, where the V is a symbolic value, and C is a
  /// constant coefficient. A constant addend is represented as <C, 0>.
  ///
  class FAddend {
  public:
    FAddend() { Val = nullptr; }

    Value *getSymVal (void) const { return Val; }
    const FAddendCoef &getCoef(void) const { return Coeff; }

    bool isConstant() const { return Val == nullptr; }
    bool isZero() const { return Coeff.isZero(); }

    void set(short Coefficient, Value *V) { Coeff.set(Coefficient), Val = V; }
    void set(const APFloat& Coefficient, Value *V)
      { Coeff.set(Coefficient); Val = V; }
    void set(const ConstantFP* Coefficient, Value *V)
      { Coeff.set(Coefficient->getValueAPF()); Val = V; }

    void negate() { Coeff.negate(); }

    /// Drill down the U-D chain one step to find the definition of V, and
    /// try to break the definition into one or two addends.
    static unsigned drillValueDownOneStep(Value* V, FAddend &A0, FAddend &A1);

    /// Similar to FAddend::drillDownOneStep() except that the value being
    /// splitted is the addend itself.
    unsigned drillAddendDownOneStep(FAddend &Addend0, FAddend &Addend1) const;

    void operator+=(const FAddend &T) {
      assert((Val == T.Val) && "Symbolic-values disagree");
      Coeff += T.Coeff;
    }

  private:
    void Scale(const FAddendCoef& ScaleAmt) { Coeff *= ScaleAmt; }

    // This addend has the value of "Coeff * Val".
    Value *Val;
    FAddendCoef Coeff;
  };

  /// FAddCombine is the class for optimizing an unsafe fadd/fsub along
  /// with its neighboring at most two instructions.
  ///
  class FAddCombine {
  public:
    FAddCombine(InstCombiner::BuilderTy *B) : Builder(B), Instr(nullptr) {}
    Value *simplify(Instruction *FAdd);

  private:
    typedef SmallVector<const FAddend*, 4> AddendVect;

    Value *simplifyFAdd(AddendVect& V, unsigned InstrQuota);

    Value *performFactorization(Instruction *I);

    /// Convert given addend to a Value
    Value *createAddendVal(const FAddend &A, bool& NeedNeg);

    /// Return the number of instructions needed to emit the N-ary addition.
    unsigned calcInstrNumber(const AddendVect& Vect);
    Value *createFSub(Value *Opnd0, Value *Opnd1);
    Value *createFAdd(Value *Opnd0, Value *Opnd1);
    Value *createFMul(Value *Opnd0, Value *Opnd1);
    Value *createFDiv(Value *Opnd0, Value *Opnd1);
    Value *createFNeg(Value *V);
    Value *createNaryFAdd(const AddendVect& Opnds, unsigned InstrQuota);
    void createInstPostProc(Instruction *NewInst, bool NoNumber = false);

    InstCombiner::BuilderTy *Builder;
    Instruction *Instr;

  private:
     // Debugging stuff are clustered here.
    #ifndef NDEBUG
      unsigned CreateInstrNum;
      void initCreateInstNum() { CreateInstrNum = 0; }
      void incCreateInstNum() { CreateInstrNum++; }
    #else
      void initCreateInstNum() {}
      void incCreateInstNum() {}
    #endif
  };
}

//===----------------------------------------------------------------------===//
//
// Implementation of
//    {FAddendCoef, FAddend, FAddition, FAddCombine}.
//
//===----------------------------------------------------------------------===//
FAddendCoef::~FAddendCoef() {
  if (BufHasFpVal)
    getFpValPtr()->~APFloat();
}

void FAddendCoef::set(const APFloat& C) {
  APFloat *P = getFpValPtr();

  if (isInt()) {
    // As the buffer is meanless byte stream, we cannot call
    // APFloat::operator=().
    new(P) APFloat(C);
  } else
    *P = C;

  IsFp = BufHasFpVal = true;
}

void FAddendCoef::convertToFpType(const fltSemantics &Sem) {
  if (!isInt())
    return;

  APFloat *P = getFpValPtr();
  if (IntVal > 0)
    new(P) APFloat(Sem, IntVal);
  else {
    new(P) APFloat(Sem, 0 - IntVal);
    P->changeSign();
  }
  IsFp = BufHasFpVal = true;
}

APFloat FAddendCoef::createAPFloatFromInt(const fltSemantics &Sem, int Val) {
  if (Val >= 0)
    return APFloat(Sem, Val);

  APFloat T(Sem, 0 - Val);
  T.changeSign();

  return T;
}

void FAddendCoef::operator=(const FAddendCoef &That) {
  if (That.isInt())
    set(That.IntVal);
  else
    set(That.getFpVal());
}

void FAddendCoef::operator+=(const FAddendCoef &That) {
  enum APFloat::roundingMode RndMode = APFloat::rmNearestTiesToEven;
  if (isInt() == That.isInt()) {
    if (isInt())
      IntVal += That.IntVal;
    else
      getFpVal().add(That.getFpVal(), RndMode);
    return;
  }

  if (isInt()) {
    const APFloat &T = That.getFpVal();
    convertToFpType(T.getSemantics());
    getFpVal().add(T, RndMode);
    return;
  }

  APFloat &T = getFpVal();
  T.add(createAPFloatFromInt(T.getSemantics(), That.IntVal), RndMode);
}

void FAddendCoef::operator-=(const FAddendCoef &That) {
  enum APFloat::roundingMode RndMode = APFloat::rmNearestTiesToEven;
  if (isInt() == That.isInt()) {
    if (isInt())
      IntVal -= That.IntVal;
    else
      getFpVal().subtract(That.getFpVal(), RndMode);
    return;
  }

  if (isInt()) {
    const APFloat &T = That.getFpVal();
    convertToFpType(T.getSemantics());
    getFpVal().subtract(T, RndMode);
    return;
  }

  APFloat &T = getFpVal();
  T.subtract(createAPFloatFromInt(T.getSemantics(), IntVal), RndMode);
}

void FAddendCoef::operator*=(const FAddendCoef &That) {
  if (That.isOne())
    return;

  if (That.isMinusOne()) {
    negate();
    return;
  }

  if (isInt() && That.isInt()) {
    int Res = IntVal * (int)That.IntVal;
    assert(!insaneIntVal(Res) && "Insane int value");
    IntVal = Res;
    return;
  }

  const fltSemantics &Semantic =
    isInt() ? That.getFpVal().getSemantics() : getFpVal().getSemantics();

  if (isInt())
    convertToFpType(Semantic);
  APFloat &F0 = getFpVal();

  if (That.isInt())
    F0.multiply(createAPFloatFromInt(Semantic, That.IntVal),
                APFloat::rmNearestTiesToEven);
  else
    F0.multiply(That.getFpVal(), APFloat::rmNearestTiesToEven);

  return;
}

void FAddendCoef::negate() {
  if (isInt())
    IntVal = 0 - IntVal;
  else
    getFpVal().changeSign();
}

Value *FAddendCoef::getValue(Type *Ty) const {
  return isInt() ?
    ConstantFP::get(Ty, float(IntVal)) :
    ConstantFP::get(Ty->getContext(), getFpVal());
}

// The definition of <Val>     Addends
// =========================================
//  A + B                     <1, A>, <1,B>
//  A - B                     <1, A>, <1,B>
//  0 - B                     <-1, B>
//  C * A,                    <C, A>
//  A + C                     <1, A> <C, NULL>
//  0 +/- 0                   <0, NULL> (corner case)
//
// Legend: A and B are not constant, C is constant
//
unsigned FAddend::drillValueDownOneStep
  (Value *Val, FAddend &Addend0, FAddend &Addend1) {
  Instruction *I = nullptr;
  if (!Val || !(I = dyn_cast<Instruction>(Val)))
    return 0;

  unsigned Opcode = I->getOpcode();

  if (Opcode == Instruction::FAdd || Opcode == Instruction::FSub) {
    ConstantFP *C0, *C1;
    Value *Opnd0 = I->getOperand(0);
    Value *Opnd1 = I->getOperand(1);
    if ((C0 = dyn_cast<ConstantFP>(Opnd0)) && C0->isZero())
      Opnd0 = nullptr;

    if ((C1 = dyn_cast<ConstantFP>(Opnd1)) && C1->isZero())
      Opnd1 = nullptr;

    if (Opnd0) {
      if (!C0)
        Addend0.set(1, Opnd0);
      else
        Addend0.set(C0, nullptr);
    }

    if (Opnd1) {
      FAddend &Addend = Opnd0 ? Addend1 : Addend0;
      if (!C1)
        Addend.set(1, Opnd1);
      else
        Addend.set(C1, nullptr);
      if (Opcode == Instruction::FSub)
        Addend.negate();
    }

    if (Opnd0 || Opnd1)
      return Opnd0 && Opnd1 ? 2 : 1;

    // Both operands are zero. Weird!
    Addend0.set(APFloat(C0->getValueAPF().getSemantics()), nullptr);
    return 1;
  }

  if (I->getOpcode() == Instruction::FMul) {
    Value *V0 = I->getOperand(0);
    Value *V1 = I->getOperand(1);
    if (ConstantFP *C = dyn_cast<ConstantFP>(V0)) {
      Addend0.set(C, V1);
      return 1;
    }

    if (ConstantFP *C = dyn_cast<ConstantFP>(V1)) {
      Addend0.set(C, V0);
      return 1;
    }
  }

  return 0;
}

// Try to break *this* addend into two addends. e.g. Suppose this addend is
// <2.3, V>, and V = X + Y, by calling this function, we obtain two addends,
// i.e. <2.3, X> and <2.3, Y>.
//
unsigned FAddend::drillAddendDownOneStep
  (FAddend &Addend0, FAddend &Addend1) const {
  if (isConstant())
    return 0;

  unsigned BreakNum = FAddend::drillValueDownOneStep(Val, Addend0, Addend1);
  if (!BreakNum || Coeff.isOne())
    return BreakNum;

  Addend0.Scale(Coeff);

  if (BreakNum == 2)
    Addend1.Scale(Coeff);

  return BreakNum;
}

// Try to perform following optimization on the input instruction I. Return the
// simplified expression if was successful; otherwise, return 0.
//
//   Instruction "I" is                Simplified into
// -------------------------------------------------------
//   (x * y) +/- (x * z)               x * (y +/- z)
//   (y / x) +/- (z / x)               (y +/- z) / x
//
Value *FAddCombine::performFactorization(Instruction *I) {
  assert((I->getOpcode() == Instruction::FAdd ||
          I->getOpcode() == Instruction::FSub) && "Expect add/sub");

  Instruction *I0 = dyn_cast<Instruction>(I->getOperand(0));
  Instruction *I1 = dyn_cast<Instruction>(I->getOperand(1));

  if (!I0 || !I1 || I0->getOpcode() != I1->getOpcode())
    return nullptr;

  bool isMpy = false;
  if (I0->getOpcode() == Instruction::FMul)
    isMpy = true;
  else if (I0->getOpcode() != Instruction::FDiv)
    return nullptr;

  Value *Opnd0_0 = I0->getOperand(0);
  Value *Opnd0_1 = I0->getOperand(1);
  Value *Opnd1_0 = I1->getOperand(0);
  Value *Opnd1_1 = I1->getOperand(1);

  //  Input Instr I       Factor   AddSub0  AddSub1
  //  ----------------------------------------------
  // (x*y) +/- (x*z)        x        y         z
  // (y/x) +/- (z/x)        x        y         z
  //
  Value *Factor = nullptr;
  Value *AddSub0 = nullptr, *AddSub1 = nullptr;

  if (isMpy) {
    if (Opnd0_0 == Opnd1_0 || Opnd0_0 == Opnd1_1)
      Factor = Opnd0_0;
    else if (Opnd0_1 == Opnd1_0 || Opnd0_1 == Opnd1_1)
      Factor = Opnd0_1;

    if (Factor) {
      AddSub0 = (Factor == Opnd0_0) ? Opnd0_1 : Opnd0_0;
      AddSub1 = (Factor == Opnd1_0) ? Opnd1_1 : Opnd1_0;
    }
  } else if (Opnd0_1 == Opnd1_1) {
    Factor = Opnd0_1;
    AddSub0 = Opnd0_0;
    AddSub1 = Opnd1_0;
  }

  if (!Factor)
    return nullptr;

  FastMathFlags Flags;
  Flags.setUnsafeAlgebra();
  if (I0) Flags &= I->getFastMathFlags();
  if (I1) Flags &= I->getFastMathFlags();

  // Create expression "NewAddSub = AddSub0 +/- AddsSub1"
  Value *NewAddSub = (I->getOpcode() == Instruction::FAdd) ?
                      createFAdd(AddSub0, AddSub1) :
                      createFSub(AddSub0, AddSub1);
  if (ConstantFP *CFP = dyn_cast<ConstantFP>(NewAddSub)) {
    const APFloat &F = CFP->getValueAPF();
    if (!F.isNormal())
      return nullptr;
  } else if (Instruction *II = dyn_cast<Instruction>(NewAddSub))
    II->setFastMathFlags(Flags);

  if (isMpy) {
    Value *RI = createFMul(Factor, NewAddSub);
    if (Instruction *II = dyn_cast<Instruction>(RI))
      II->setFastMathFlags(Flags);
    return RI;
  }

  Value *RI = createFDiv(NewAddSub, Factor);
  if (Instruction *II = dyn_cast<Instruction>(RI))
    II->setFastMathFlags(Flags);
  return RI;
}

Value *FAddCombine::simplify(Instruction *I) {
  assert(I->hasUnsafeAlgebra() && "Should be in unsafe mode");

  // Currently we are not able to handle vector type.
  if (I->getType()->isVectorTy())
    return nullptr;

  assert((I->getOpcode() == Instruction::FAdd ||
          I->getOpcode() == Instruction::FSub) && "Expect add/sub");

  // Save the instruction before calling other member-functions.
  Instr = I;

  FAddend Opnd0, Opnd1, Opnd0_0, Opnd0_1, Opnd1_0, Opnd1_1;

  unsigned OpndNum = FAddend::drillValueDownOneStep(I, Opnd0, Opnd1);

  // Step 1: Expand the 1st addend into Opnd0_0 and Opnd0_1.
  unsigned Opnd0_ExpNum = 0;
  unsigned Opnd1_ExpNum = 0;

  if (!Opnd0.isConstant())
    Opnd0_ExpNum = Opnd0.drillAddendDownOneStep(Opnd0_0, Opnd0_1);

  // Step 2: Expand the 2nd addend into Opnd1_0 and Opnd1_1.
  if (OpndNum == 2 && !Opnd1.isConstant())
    Opnd1_ExpNum = Opnd1.drillAddendDownOneStep(Opnd1_0, Opnd1_1);

  // Step 3: Try to optimize Opnd0_0 + Opnd0_1 + Opnd1_0 + Opnd1_1
  if (Opnd0_ExpNum && Opnd1_ExpNum) {
    AddendVect AllOpnds;
    AllOpnds.push_back(&Opnd0_0);
    AllOpnds.push_back(&Opnd1_0);
    if (Opnd0_ExpNum == 2)
      AllOpnds.push_back(&Opnd0_1);
    if (Opnd1_ExpNum == 2)
      AllOpnds.push_back(&Opnd1_1);

    // Compute instruction quota. We should save at least one instruction.
    unsigned InstQuota = 0;

    Value *V0 = I->getOperand(0);
    Value *V1 = I->getOperand(1);
    InstQuota = ((!isa<Constant>(V0) && V0->hasOneUse()) &&
                 (!isa<Constant>(V1) && V1->hasOneUse())) ? 2 : 1;

    if (Value *R = simplifyFAdd(AllOpnds, InstQuota))
      return R;
  }

  if (OpndNum != 2) {
    // The input instruction is : "I=0.0 +/- V". If the "V" were able to be
    // splitted into two addends, say "V = X - Y", the instruction would have
    // been optimized into "I = Y - X" in the previous steps.
    //
    const FAddendCoef &CE = Opnd0.getCoef();
    return CE.isOne() ? Opnd0.getSymVal() : nullptr;
  }

  // step 4: Try to optimize Opnd0 + Opnd1_0 [+ Opnd1_1]
  if (Opnd1_ExpNum) {
    AddendVect AllOpnds;
    AllOpnds.push_back(&Opnd0);
    AllOpnds.push_back(&Opnd1_0);
    if (Opnd1_ExpNum == 2)
      AllOpnds.push_back(&Opnd1_1);

    if (Value *R = simplifyFAdd(AllOpnds, 1))
      return R;
  }

  // step 5: Try to optimize Opnd1 + Opnd0_0 [+ Opnd0_1]
  if (Opnd0_ExpNum) {
    AddendVect AllOpnds;
    AllOpnds.push_back(&Opnd1);
    AllOpnds.push_back(&Opnd0_0);
    if (Opnd0_ExpNum == 2)
      AllOpnds.push_back(&Opnd0_1);

    if (Value *R = simplifyFAdd(AllOpnds, 1))
      return R;
  }

  // step 6: Try factorization as the last resort,
  return performFactorization(I);
}

Value *FAddCombine::simplifyFAdd(AddendVect& Addends, unsigned InstrQuota) {

  unsigned AddendNum = Addends.size();
  assert(AddendNum <= 4 && "Too many addends");

  // For saving intermediate results;
  unsigned NextTmpIdx = 0;
  FAddend TmpResult[3];

  // Points to the constant addend of the resulting simplified expression.
  // If the resulting expr has constant-addend, this constant-addend is
  // desirable to reside at the top of the resulting expression tree. Placing
  // constant close to supper-expr(s) will potentially reveal some optimization
  // opportunities in super-expr(s).
  //
  const FAddend *ConstAdd = nullptr;

  // Simplified addends are placed <SimpVect>.
  AddendVect SimpVect;

  // The outer loop works on one symbolic-value at a time. Suppose the input
  // addends are : <a1, x>, <b1, y>, <a2, x>, <c1, z>, <b2, y>, ...
  // The symbolic-values will be processed in this order: x, y, z.
  //
  for (unsigned SymIdx = 0; SymIdx < AddendNum; SymIdx++) {

    const FAddend *ThisAddend = Addends[SymIdx];
    if (!ThisAddend) {
      // This addend was processed before.
      continue;
    }

    Value *Val = ThisAddend->getSymVal();
    unsigned StartIdx = SimpVect.size();
    SimpVect.push_back(ThisAddend);

    // The inner loop collects addends sharing same symbolic-value, and these
    // addends will be later on folded into a single addend. Following above
    // example, if the symbolic value "y" is being processed, the inner loop
    // will collect two addends "<b1,y>" and "<b2,Y>". These two addends will
    // be later on folded into "<b1+b2, y>".
    //
    for (unsigned SameSymIdx = SymIdx + 1;
         SameSymIdx < AddendNum; SameSymIdx++) {
      const FAddend *T = Addends[SameSymIdx];
      if (T && T->getSymVal() == Val) {
        // Set null such that next iteration of the outer loop will not process
        // this addend again.
        Addends[SameSymIdx] = nullptr;
        SimpVect.push_back(T);
      }
    }

    // If multiple addends share same symbolic value, fold them together.
    if (StartIdx + 1 != SimpVect.size()) {
      FAddend &R = TmpResult[NextTmpIdx ++];
      R = *SimpVect[StartIdx];
      for (unsigned Idx = StartIdx + 1; Idx < SimpVect.size(); Idx++)
        R += *SimpVect[Idx];

      // Pop all addends being folded and push the resulting folded addend.
      SimpVect.resize(StartIdx);
      if (Val) {
        if (!R.isZero()) {
          SimpVect.push_back(&R);
        }
      } else {
        // Don't push constant addend at this time. It will be the last element
        // of <SimpVect>.
        ConstAdd = &R;
      }
    }
  }

  assert((NextTmpIdx <= array_lengthof(TmpResult) + 1) &&
         "out-of-bound access");

  if (ConstAdd)
    SimpVect.push_back(ConstAdd);

  Value *Result;
  if (!SimpVect.empty())
    Result = createNaryFAdd(SimpVect, InstrQuota);
  else {
    // The addition is folded to 0.0.
    Result = ConstantFP::get(Instr->getType(), 0.0);
  }

  return Result;
}

Value *FAddCombine::createNaryFAdd
  (const AddendVect &Opnds, unsigned InstrQuota) {
  assert(!Opnds.empty() && "Expect at least one addend");

  // Step 1: Check if the # of instructions needed exceeds the quota.
  //
  unsigned InstrNeeded = calcInstrNumber(Opnds);
  if (InstrNeeded > InstrQuota)
    return nullptr;

  initCreateInstNum();

  // step 2: Emit the N-ary addition.
  // Note that at most three instructions are involved in Fadd-InstCombine: the
  // addition in question, and at most two neighboring instructions.
  // The resulting optimized addition should have at least one less instruction
  // than the original addition expression tree. This implies that the resulting
  // N-ary addition has at most two instructions, and we don't need to worry
  // about tree-height when constructing the N-ary addition.

  Value *LastVal = nullptr;
  bool LastValNeedNeg = false;

  // Iterate the addends, creating fadd/fsub using adjacent two addends.
  for (AddendVect::const_iterator I = Opnds.begin(), E = Opnds.end();
       I != E; I++) {
    bool NeedNeg;
    Value *V = createAddendVal(**I, NeedNeg);
    if (!LastVal) {
      LastVal = V;
      LastValNeedNeg = NeedNeg;
      continue;
    }

    if (LastValNeedNeg == NeedNeg) {
      LastVal = createFAdd(LastVal, V);
      continue;
    }

    if (LastValNeedNeg)
      LastVal = createFSub(V, LastVal);
    else
      LastVal = createFSub(LastVal, V);

    LastValNeedNeg = false;
  }

  if (LastValNeedNeg) {
    LastVal = createFNeg(LastVal);
  }

  #ifndef NDEBUG
    assert(CreateInstrNum == InstrNeeded &&
           "Inconsistent in instruction numbers");
  #endif

  return LastVal;
}

Value *FAddCombine::createFSub(Value *Opnd0, Value *Opnd1) {
  Value *V = Builder->CreateFSub(Opnd0, Opnd1);
  if (Instruction *I = dyn_cast<Instruction>(V))
    createInstPostProc(I);
  return V;
}

Value *FAddCombine::createFNeg(Value *V) {
  Value *Zero = cast<Value>(ConstantFP::getZeroValueForNegation(V->getType()));
  Value *NewV = createFSub(Zero, V);
  if (Instruction *I = dyn_cast<Instruction>(NewV))
    createInstPostProc(I, true); // fneg's don't receive instruction numbers.
  return NewV;
}

Value *FAddCombine::createFAdd(Value *Opnd0, Value *Opnd1) {
  Value *V = Builder->CreateFAdd(Opnd0, Opnd1);
  if (Instruction *I = dyn_cast<Instruction>(V))
    createInstPostProc(I);
  return V;
}

Value *FAddCombine::createFMul(Value *Opnd0, Value *Opnd1) {
  Value *V = Builder->CreateFMul(Opnd0, Opnd1);
  if (Instruction *I = dyn_cast<Instruction>(V))
    createInstPostProc(I);
  return V;
}

Value *FAddCombine::createFDiv(Value *Opnd0, Value *Opnd1) {
  Value *V = Builder->CreateFDiv(Opnd0, Opnd1);
  if (Instruction *I = dyn_cast<Instruction>(V))
    createInstPostProc(I);
  return V;
}

void FAddCombine::createInstPostProc(Instruction *NewInstr, bool NoNumber) {
  NewInstr->setDebugLoc(Instr->getDebugLoc());

  // Keep track of the number of instruction created.
  if (!NoNumber)
    incCreateInstNum();

  // Propagate fast-math flags
  NewInstr->setFastMathFlags(Instr->getFastMathFlags());
}

// Return the number of instruction needed to emit the N-ary addition.
// NOTE: Keep this function in sync with createAddendVal().
unsigned FAddCombine::calcInstrNumber(const AddendVect &Opnds) {
  unsigned OpndNum = Opnds.size();
  unsigned InstrNeeded = OpndNum - 1;

  // The number of addends in the form of "(-1)*x".
  unsigned NegOpndNum = 0;

  // Adjust the number of instructions needed to emit the N-ary add.
  for (AddendVect::const_iterator I = Opnds.begin(), E = Opnds.end();
       I != E; I++) {
    const FAddend *Opnd = *I;
    if (Opnd->isConstant())
      continue;

    const FAddendCoef &CE = Opnd->getCoef();
    if (CE.isMinusOne() || CE.isMinusTwo())
      NegOpndNum++;

    // Let the addend be "c * x". If "c == +/-1", the value of the addend
    // is immediately available; otherwise, it needs exactly one instruction
    // to evaluate the value.
    if (!CE.isMinusOne() && !CE.isOne())
      InstrNeeded++;
  }
  if (NegOpndNum == OpndNum)
    InstrNeeded++;
  return InstrNeeded;
}

// Input Addend        Value           NeedNeg(output)
// ================================================================
// Constant C          C               false
// <+/-1, V>           V               coefficient is -1
// <2/-2, V>          "fadd V, V"      coefficient is -2
// <C, V>             "fmul V, C"      false
//
// NOTE: Keep this function in sync with FAddCombine::calcInstrNumber.
Value *FAddCombine::createAddendVal(const FAddend &Opnd, bool &NeedNeg) {
  const FAddendCoef &Coeff = Opnd.getCoef();

  if (Opnd.isConstant()) {
    NeedNeg = false;
    return Coeff.getValue(Instr->getType());
  }

  Value *OpndVal = Opnd.getSymVal();

  if (Coeff.isMinusOne() || Coeff.isOne()) {
    NeedNeg = Coeff.isMinusOne();
    return OpndVal;
  }

  if (Coeff.isTwo() || Coeff.isMinusTwo()) {
    NeedNeg = Coeff.isMinusTwo();
    return createFAdd(OpndVal, OpndVal);
  }

  NeedNeg = false;
  return createFMul(OpndVal, Coeff.getValue(Instr->getType()));
}

// If one of the operands only has one non-zero bit, and if the other
// operand has a known-zero bit in a more significant place than it (not
// including the sign bit) the ripple may go up to and fill the zero, but
// won't change the sign. For example, (X & ~4) + 1.
static bool checkRippleForAdd(const APInt &Op0KnownZero,
                              const APInt &Op1KnownZero) {
  APInt Op1MaybeOne = ~Op1KnownZero;
  // Make sure that one of the operand has at most one bit set to 1.
  if (Op1MaybeOne.countPopulation() != 1)
    return false;

  // Find the most significant known 0 other than the sign bit.
  int BitWidth = Op0KnownZero.getBitWidth();
  APInt Op0KnownZeroTemp(Op0KnownZero);
  Op0KnownZeroTemp.clearBit(BitWidth - 1);
  int Op0ZeroPosition = BitWidth - Op0KnownZeroTemp.countLeadingZeros() - 1;

  int Op1OnePosition = BitWidth - Op1MaybeOne.countLeadingZeros() - 1;
  assert(Op1OnePosition >= 0);

  // This also covers the case of no known zero, since in that case
  // Op0ZeroPosition is -1.
  return Op0ZeroPosition >= Op1OnePosition;
}

/// WillNotOverflowSignedAdd - Return true if we can prove that:
///    (sext (add LHS, RHS))  === (add (sext LHS), (sext RHS))
/// This basically requires proving that the add in the original type would not
/// overflow to change the sign bit or have a carry out.
bool InstCombiner::WillNotOverflowSignedAdd(Value *LHS, Value *RHS,
                                            Instruction &CxtI) {
  // There are different heuristics we can use for this.  Here are some simple
  // ones.

  // If LHS and RHS each have at least two sign bits, the addition will look
  // like
  //
  // XX..... +
  // YY.....
  //
  // If the carry into the most significant position is 0, X and Y can't both
  // be 1 and therefore the carry out of the addition is also 0.
  //
  // If the carry into the most significant position is 1, X and Y can't both
  // be 0 and therefore the carry out of the addition is also 1.
  //
  // Since the carry into the most significant position is always equal to
  // the carry out of the addition, there is no signed overflow.
  if (ComputeNumSignBits(LHS, 0, &CxtI) > 1 &&
      ComputeNumSignBits(RHS, 0, &CxtI) > 1)
    return true;

  unsigned BitWidth = LHS->getType()->getScalarSizeInBits();
  APInt LHSKnownZero(BitWidth, 0);
  APInt LHSKnownOne(BitWidth, 0);
  computeKnownBits(LHS, LHSKnownZero, LHSKnownOne, 0, &CxtI);

  APInt RHSKnownZero(BitWidth, 0);
  APInt RHSKnownOne(BitWidth, 0);
  computeKnownBits(RHS, RHSKnownZero, RHSKnownOne, 0, &CxtI);

  // Addition of two 2's compliment numbers having opposite signs will never
  // overflow.
  if ((LHSKnownOne[BitWidth - 1] && RHSKnownZero[BitWidth - 1]) ||
      (LHSKnownZero[BitWidth - 1] && RHSKnownOne[BitWidth - 1]))
    return true;

  // Check if carry bit of addition will not cause overflow.
  if (checkRippleForAdd(LHSKnownZero, RHSKnownZero))
    return true;
  if (checkRippleForAdd(RHSKnownZero, LHSKnownZero))
    return true;

  return false;
}

/// \brief Return true if we can prove that:
///    (sub LHS, RHS)  === (sub nsw LHS, RHS)
/// This basically requires proving that the add in the original type would not
/// overflow to change the sign bit or have a carry out.
/// TODO: Handle this for Vectors.
bool InstCombiner::WillNotOverflowSignedSub(Value *LHS, Value *RHS,
                                            Instruction &CxtI) {
  // If LHS and RHS each have at least two sign bits, the subtraction
  // cannot overflow.
  if (ComputeNumSignBits(LHS, 0, &CxtI) > 1 &&
      ComputeNumSignBits(RHS, 0, &CxtI) > 1)
    return true;

  unsigned BitWidth = LHS->getType()->getScalarSizeInBits();
  APInt LHSKnownZero(BitWidth, 0);
  APInt LHSKnownOne(BitWidth, 0);
  computeKnownBits(LHS, LHSKnownZero, LHSKnownOne, 0, &CxtI);

  APInt RHSKnownZero(BitWidth, 0);
  APInt RHSKnownOne(BitWidth, 0);
  computeKnownBits(RHS, RHSKnownZero, RHSKnownOne, 0, &CxtI);

  // Subtraction of two 2's compliment numbers having identical signs will
  // never overflow.
  if ((LHSKnownOne[BitWidth - 1] && RHSKnownOne[BitWidth - 1]) ||
      (LHSKnownZero[BitWidth - 1] && RHSKnownZero[BitWidth - 1]))
    return true;

  // TODO: implement logic similar to checkRippleForAdd
  return false;
}

/// \brief Return true if we can prove that:
///    (sub LHS, RHS)  === (sub nuw LHS, RHS)
bool InstCombiner::WillNotOverflowUnsignedSub(Value *LHS, Value *RHS,
                                              Instruction &CxtI) {
  // If the LHS is negative and the RHS is non-negative, no unsigned wrap.
  bool LHSKnownNonNegative, LHSKnownNegative;
  bool RHSKnownNonNegative, RHSKnownNegative;
  ComputeSignBit(LHS, LHSKnownNonNegative, LHSKnownNegative, /*Depth=*/0,
                 &CxtI);
  ComputeSignBit(RHS, RHSKnownNonNegative, RHSKnownNegative, /*Depth=*/0,
                 &CxtI);
  if (LHSKnownNegative && RHSKnownNonNegative)
    return true;

  return false;
}

// Checks if any operand is negative and we can convert add to sub.
// This function checks for following negative patterns
//   ADD(XOR(OR(Z, NOT(C)), C)), 1) == NEG(AND(Z, C))
//   ADD(XOR(AND(Z, C), C), 1) == NEG(OR(Z, ~C))
//   XOR(AND(Z, C), (C + 1)) == NEG(OR(Z, ~C)) if C is even
static Value *checkForNegativeOperand(BinaryOperator &I,
                                      InstCombiner::BuilderTy *Builder) {
  Value *LHS = I.getOperand(0), *RHS = I.getOperand(1);

  // This function creates 2 instructions to replace ADD, we need at least one
  // of LHS or RHS to have one use to ensure benefit in transform.
  if (!LHS->hasOneUse() && !RHS->hasOneUse())
    return nullptr;

  Value *X = nullptr, *Y = nullptr, *Z = nullptr;
  const APInt *C1 = nullptr, *C2 = nullptr;

  // if ONE is on other side, swap
  if (match(RHS, m_Add(m_Value(X), m_One())))
    std::swap(LHS, RHS);

  if (match(LHS, m_Add(m_Value(X), m_One()))) {
    // if XOR on other side, swap
    if (match(RHS, m_Xor(m_Value(Y), m_APInt(C1))))
      std::swap(X, RHS);

    if (match(X, m_Xor(m_Value(Y), m_APInt(C1)))) {
      // X = XOR(Y, C1), Y = OR(Z, C2), C2 = NOT(C1) ==> X == NOT(AND(Z, C1))
      // ADD(ADD(X, 1), RHS) == ADD(X, ADD(RHS, 1)) == SUB(RHS, AND(Z, C1))
      if (match(Y, m_Or(m_Value(Z), m_APInt(C2))) && (*C2 == ~(*C1))) {
        Value *NewAnd = Builder->CreateAnd(Z, *C1);
        return Builder->CreateSub(RHS, NewAnd, "sub");
      } else if (match(Y, m_And(m_Value(Z), m_APInt(C2))) && (*C1 == *C2)) {
        // X = XOR(Y, C1), Y = AND(Z, C2), C2 == C1 ==> X == NOT(OR(Z, ~C1))
        // ADD(ADD(X, 1), RHS) == ADD(X, ADD(RHS, 1)) == SUB(RHS, OR(Z, ~C1))
        Value *NewOr = Builder->CreateOr(Z, ~(*C1));
        return Builder->CreateSub(RHS, NewOr, "sub");
      }
    }
  }

  // Restore LHS and RHS
  LHS = I.getOperand(0);
  RHS = I.getOperand(1);

  // if XOR is on other side, swap
  if (match(RHS, m_Xor(m_Value(Y), m_APInt(C1))))
    std::swap(LHS, RHS);

  // C2 is ODD
  // LHS = XOR(Y, C1), Y = AND(Z, C2), C1 == (C2 + 1) => LHS == NEG(OR(Z, ~C2))
  // ADD(LHS, RHS) == SUB(RHS, OR(Z, ~C2))
  if (match(LHS, m_Xor(m_Value(Y), m_APInt(C1))))
    if (C1->countTrailingZeros() == 0)
      if (match(Y, m_And(m_Value(Z), m_APInt(C2))) && *C1 == (*C2 + 1)) {
        Value *NewOr = Builder->CreateOr(Z, ~(*C2));
        return Builder->CreateSub(RHS, NewOr, "sub");
      }
  return nullptr;
}

Instruction *InstCombiner::visitAdd(BinaryOperator &I) {
  bool Changed = SimplifyAssociativeOrCommutative(I);
  Value *LHS = I.getOperand(0), *RHS = I.getOperand(1);

  if (Value *V = SimplifyVectorOp(I))
    return ReplaceInstUsesWith(I, V);

  if (Value *V = SimplifyAddInst(LHS, RHS, I.hasNoSignedWrap(),
                                 I.hasNoUnsignedWrap(), DL, TLI, DT, AC))
    return ReplaceInstUsesWith(I, V);

   // (A*B)+(A*C) -> A*(B+C) etc
  if (Value *V = SimplifyUsingDistributiveLaws(I))
    return ReplaceInstUsesWith(I, V);

  if (ConstantInt *CI = dyn_cast<ConstantInt>(RHS)) {
    // X + (signbit) --> X ^ signbit
    const APInt &Val = CI->getValue();
    if (Val.isSignBit()) {
      llvmberry::ValidationUnit::Begin("add_signbit", I.getParent()->getParent());

      INTRUDE(CAPTURE(&I, &LHS, &RHS), {
        // I = LHS + RHS --> I = LHS ^ 1
        INFRULE(INSTPOS(SRC, &I), llvmberry::ConsAddSignbit::make(
                REGISTER(I), VAL(LHS), VAL(RHS), BITSIZE(*LHS)));
      });

      return BinaryOperator::CreateXor(LHS, RHS);
    }

    // See if SimplifyDemandedBits can simplify this.  This handles stuff like
    // (X & 254)+1 -> (X&254)|1
    if (SimplifyDemandedInstructionBits(I))
      return &I;

    // zext(bool) + C -> bool ? C + 1 : C
    if (ZExtInst *ZI = dyn_cast<ZExtInst>(LHS))
      if (ZI->getSrcTy()->isIntegerTy(1)){
        llvmberry::ValidationUnit::Begin("add_zext_bool", I.getParent()->getParent());

        INTRUDE(CAPTURE(&I, &ZI, &CI), {
          BinaryOperator &Y = I;
          ZExtInst *X = ZI;
          ConstantInt *C = CI;
          // src : 
          // x = zext i1 b to i<sz> 
          // y = x + c          
          //
          // tgt : 
          // x = zext i1 b to i<sz> 
          // y = select i1 b, (c + 1), c
          //std::string reg_x_name = llvmberry::getVariable(*X);
          //std::string reg_y_name = llvmberry::getVariable(Y);
          //int64_t c = C->getSExtValue();
          //int64_t cprime = c + 1;
          int64_t cprime = C->getSExtValue() + 1;
          //int c_bitwidth = C->getType()->getIntegerBitWidth();
          int cprime_bitwidth = C->getType()->getIntegerBitWidth();

          llvmberry::propagateInstruction(hints, X, &Y, llvmberry::Source);

/*
          INFRULE(INSTPOS(SRC, &Y), llvmberry::ConsAddZextBool::make(
                  REGISTER(*X), REGISTER(Y),
                  VAL(X->getOperand(0)), CONSTINT(c, c_bitwidth),
                  CONSTINT(cprime, cprime_bitwidth), BITSIZE(c_bitwidth)));
*/
          INFRULE(INSTPOS(SRC, &Y), llvmberry::ConsAddZextBool::make(
                  REGISTER(*X), REGISTER(Y), VAL(X->getOperand(0)), CONSTINT(C),
                  CONSTINT(cprime, cprime_bitwidth), BITSIZE(*C)));
        });

        return SelectInst::Create(ZI->getOperand(0), AddOne(CI), CI);
      }

    Value *XorLHS = nullptr; ConstantInt *XorRHS = nullptr;
    if (match(LHS, m_Xor(m_Value(XorLHS), m_ConstantInt(XorRHS)))) {
      uint32_t TySizeBits = I.getType()->getScalarSizeInBits();
      const APInt &RHSVal = CI->getValue();
      unsigned ExtendAmt = 0;
      // If we have ADD(XOR(AND(X, 0xFF), 0x80), 0xF..F80), it's a sext.
      // If we have ADD(XOR(AND(X, 0xFF), 0xF..F80), 0x80), it's a sext.
      if (XorRHS->getValue() == -RHSVal) {
        if (RHSVal.isPowerOf2())
          ExtendAmt = TySizeBits - RHSVal.logBase2() - 1;
        else if (XorRHS->getValue().isPowerOf2())
          ExtendAmt = TySizeBits - XorRHS->getValue().logBase2() - 1;
      }

      if (ExtendAmt) {
        APInt Mask = APInt::getHighBitsSet(TySizeBits, ExtendAmt);
        if (!MaskedValueIsZero(XorLHS, Mask, 0, &I))
          ExtendAmt = 0;
      }

      if (ExtendAmt) {
        Constant *ShAmt = ConstantInt::get(I.getType(), ExtendAmt);
        Value *NewShl = Builder->CreateShl(XorLHS, ShAmt, "sext");
        return BinaryOperator::CreateAShr(NewShl, ShAmt);
      }

      // If this is a xor that was canonicalized from a sub, turn it back into
      // a sub and fuse this add with it.
      if (LHS->hasOneUse() && (XorRHS->getValue()+1).isPowerOf2()) {
        IntegerType *IT = cast<IntegerType>(I.getType());
        APInt LHSKnownOne(IT->getBitWidth(), 0);
        APInt LHSKnownZero(IT->getBitWidth(), 0);
        computeKnownBits(XorLHS, LHSKnownZero, LHSKnownOne, 0, &I);
        if ((XorRHS->getValue() | LHSKnownZero).isAllOnesValue())
          return BinaryOperator::CreateSub(ConstantExpr::getAdd(XorRHS, CI),
                                           XorLHS);
      }
      // (X + signbit) + C could have gotten canonicalized to (X ^ signbit) + C,
      // transform them into (X + (signbit ^ C))
      if (XorRHS->getValue().isSignBit())
          return BinaryOperator::CreateAdd(XorLHS,
                                           ConstantExpr::getXor(XorRHS, CI));
    }
  }

  if (isa<Constant>(RHS) && isa<PHINode>(LHS))
    if (Instruction *NV = FoldOpIntoPhi(I))
      return NV;

  if (I.getType()->getScalarType()->isIntegerTy(1)){
    llvmberry::ValidationUnit::Begin("add_onebit", I.getParent()->getParent());

    INTRUDE(CAPTURE(&I), {
      BinaryOperator &Z = I;
      // src : 
      // z = x + y (both are i1 type) 
      //
      // tgt : 
      // z = x ^ y
      // std::string reg_z_name = llvmberry::getVariable(Z);

      INFRULE(INSTPOS(SRC, &Z), llvmberry::ConsAddOnebit::make(
              REGISTER(Z), VAL(Z.getOperand(0)), VAL(Z.getOperand(1))));
    });
    
    return BinaryOperator::CreateXor(LHS, RHS);
  }

  // X + X --> X << 1
  if (LHS == RHS) {
    llvmberry::ValidationUnit::Begin("add_shift", I.getParent()->getParent());

    BinaryOperator *New =
      BinaryOperator::CreateShl(LHS, ConstantInt::get(I.getType(), 1));
    New->setHasNoSignedWrap(I.hasNoSignedWrap());
    New->setHasNoUnsignedWrap(I.hasNoUnsignedWrap());

    INTRUDE(CAPTURE(&I, &LHS, &RHS), {
      // I = LHS + RHS

      // from "y = v + v", create "y = v << 1"
      INFRULE(INSTPOS(SRC, &I), llvmberry::ConsAddShift::make(
              REGISTER(I), VAL(LHS), BITSIZE(*LHS)));
    });

    return New;
  }

  // -A + B  -->  B - A
  // -A + -B  -->  -(A + B)
  if (Value *LHSV = dyn_castNegVal(LHS)) {
    if (!isa<Constant>(RHS))
      if (Value *RHSV = dyn_castNegVal(RHS)) {
        llvmberry::ValidationUnit::Begin("add_dist_sub", I.getParent()->getParent());
        
        Value *NewAdd = Builder->CreateAdd(LHSV, RHSV, "sum");

        INTRUDE(CAPTURE(&LHS, &LHSV, &RHS, &RHSV, &I, &NewAdd), {
          //    <src>        <tgt>
          // mx = 0 - x | mx = 0 - x
          // my = 0 - y | my = 0 - y
          //            |  w = x + y
          //  z = x + y |  z = 0 - w
          BinaryOperator *MX = dyn_cast<BinaryOperator>(LHS);
          Value *MY = RHS;
          Value *X = LHSV;
          Value *Y = RHSV;
          BinaryOperator *W = dyn_cast<BinaryOperator>(NewAdd);
          BinaryOperator *Z = &I;
          assert(W != nullptr);

          // prepare variables
          std::string reg_w_name = llvmberry::getVariable(*W);

          // propagate "mx = 0 - x"
          llvmberry::generateHintForNegValue(MX, *Z, llvmberry::Target);
          
          // propagate "my = 0 - y"
          llvmberry::generateHintForNegValue(MY, *Z, llvmberry::Target);
          
          // add nop into source
          llvmberry::insertSrcNopAtTgtI(hints, W);
         
          // propagate "w = x + y"
          PROPAGATE(LESSDEF(RHS(reg_w_name, Physical, TGT), VAR(reg_w_name), TGT),
                    BOUNDS(INSTPOS(TGT, W), INSTPOS(TGT, Z)));

          // maydiff global w
          PROPAGATE(MAYDIFF(reg_w_name, Physical), llvmberry::ConsGlobal::make());
 
          // create z = x + y in tgt
          INFRULE(INSTPOS(TGT, Z), llvmberry::ConsAddDistSub::make(
                  REGISTER(*Z), REGISTER(*MX), VAL(MY), REGISTER(reg_w_name), VAL(X), VAL(Y), BITSIZE(*Z)));
        });


        return BinaryOperator::CreateNeg(NewAdd);
      }

    llvmberry::ValidationUnit::Begin("add_comm_sub",
                                     I.getParent()->getParent());
    INTRUDE(CAPTURE(&LHS, &LHSV, &RHS, &I), {
      // LHS = 0   - LHSV    my = 0  - y
      //   I = LHS + RHS      z = my + x

      // prepare variables
      //std::string reg_my_name = llvmberry::getVariable(*LHS);
      //std::string reg_z_name = llvmberry::getVariable(I);

      // propagate "my = 0 - y"
      llvmberry::propagateInstruction(hints, dyn_cast<Instruction>(LHS), &I, SRC);

      // from "z = my + x", create "z = x + my"
/*
      INFRULE(INSTPOS(SRC, &I), llvmberry::ConsBopCommutative::make(
              VAR(reg_z_name), llvmberry::TyBop::BopAdd,
              VAL(LHS), VAL(RHS), BITSIZE(*LHSV)));
*/
      INFRULE(INSTPOS(SRC, &I), llvmberry::ConsBopCommutative::make(
              VAR(I), llvmberry::TyBop::BopAdd, VAL(LHS), VAL(RHS), BITSIZE(*LHSV)));

      // from "z = x + my" and "my = 0 - y", create "z = x - y"
/*
      INFRULE(INSTPOS(SRC, &I), llvmberry::ConsAddSub::make(
              REGISTER(reg_my_name), REGISTER(reg_z_name), VAL(RHS), VAL(LHSV),
              BITSIZE(*LHSV)));
*/
      INFRULE(INSTPOS(SRC, &I), llvmberry::ConsAddSub::make(
              REGISTER(*LHS), REGISTER(I), VAL(RHS), VAL(LHSV), BITSIZE(*LHSV)));
    });

    return BinaryOperator::CreateSub(RHS, LHSV);
  }

  // A + -B  -->  A - B
  if (!isa<Constant>(RHS))
    if (Value *V = dyn_castNegVal(RHS)) {
      llvmberry::ValidationUnit::Begin("add_sub", I.getParent()->getParent());
      INTRUDE(CAPTURE(&V, &LHS, &RHS, &I), {
        // RHS = 0   - V       my = 0 - y
        //   I = LHS + RHS      z = x + my

        // prepare variables
        //std::string reg_my_name = llvmberry::getVariable(*RHS);
        //std::string reg_z_name = llvmberry::getVariable(I);

        // propagate "my = 0 - y"
        llvmberry::propagateInstruction(hints, dyn_cast<Instruction>(RHS), &I, SRC);

        // from "z = x + my" and "my = 0 - y", create "z = x - y"
/*
        INFRULE(INSTPOS(SRC, &I), llvmberry::ConsAddSub::make(
                REGISTER(reg_my_name), REGISTER(reg_z_name), VAL(LHS), VAL(V),
                BITSIZE(*V)));
*/
        INFRULE(INSTPOS(SRC, &I), llvmberry::ConsAddSub::make(
                REGISTER(*RHS), REGISTER(I), VAL(LHS), VAL(V), BITSIZE(*V)));
      });

      return BinaryOperator::CreateSub(LHS, V);
    }

  if (Value *V = checkForNegativeOperand(I, Builder))
    return ReplaceInstUsesWith(I, V);

  // A+B --> A|B iff A and B have no bits set in common.
  if (haveNoCommonBitsSet(LHS, RHS, DL, AC, &I, DT))
    return BinaryOperator::CreateOr(LHS, RHS);

  if (Constant *CRHS = dyn_cast<Constant>(RHS)) {
    Value *X;
    if (match(LHS, m_Not(m_Value(X)))){ // ~X + C --> (C-1) - X
      llvmberry::ValidationUnit::Begin("add_const_not", I.getParent()->getParent());
      INTRUDE(CAPTURE(&CRHS, &LHS, &I, &X), {
        // Y = X ^ -1
        // Z = Y + C1
        Instruction *Y = dyn_cast<Instruction>(LHS);
        ConstantInt *C1_ptr = dyn_cast<ConstantInt>(CRHS);
        int64_t C1 = C1_ptr->getSExtValue();
        Instruction *Z = &I;

        // prepare variables
        int bitwidth = Z->getType()->getIntegerBitWidth();

        // propagate "Y = X ^ -1"
        llvmberry::propagateInstruction(hints, Y, Z, SRC);

        // To Z, apply add_const_not
/*
        INFRULE(INSTPOS(SRC, Z), llvmberry::ConsAddConstNot::make(
                REGISTER(*Z), REGISTER(*Y), VAL(X),
                CONSTINT(C1, bitwidth), CONSTINT(C1 - 1, bitwidth),
                BITSIZE(bitwidth)));
*/
        INFRULE(INSTPOS(SRC, Z), llvmberry::ConsAddConstNot::make(
                REGISTER(*Z), REGISTER(*Y), VAL(X), CONSTINT(C1, bitwidth),
                CONSTINT(C1 - 1, bitwidth), BITSIZE(bitwidth)));
      });

      return BinaryOperator::CreateSub(SubOne(CRHS), X);
    }
  }

  if (ConstantInt *CRHS = dyn_cast<ConstantInt>(RHS)) {
    // (X & FF00) + xx00  -> (X+xx00) & FF00
    Value *X;
    ConstantInt *C2;
    if (LHS->hasOneUse() &&
        match(LHS, m_And(m_Value(X), m_ConstantInt(C2))) &&
        CRHS->getValue() == (CRHS->getValue() & C2->getValue())) {
      // See if all bits from the first bit set in the Add RHS up are included
      // in the mask.  First, get the rightmost bit.
      const APInt &AddRHSV = CRHS->getValue();

      // Form a mask of all bits from the lowest bit added through the top.
      APInt AddRHSHighBits(~((AddRHSV & -AddRHSV)-1));

      // See if the and mask includes all of these bits.
      APInt AddRHSHighBitsAnd(AddRHSHighBits & C2->getValue());

      if (AddRHSHighBits == AddRHSHighBitsAnd) {
        llvmberry::ValidationUnit::Begin("add_mask", I.getParent()->getParent());
        
        // Okay, the xform is safe.  Insert the new add pronto.
        Value *NewAdd = Builder->CreateAdd(X, CRHS, LHS->getName());

        INTRUDE(CAPTURE(&I, &LHS, &CRHS, &X, &C2, &NewAdd), {
          //   <src>     |     <tgt>
          // Y = X & c2  | Y  = X  & c2
          // nop         | Y' = X  + c1
          // Z = Y + c1  | Z  = Y' & c2
          
          int64_t c1 = CRHS->getSExtValue();
          int64_t c2 = C2->getSExtValue();
          Instruction *Y = dyn_cast<Instruction>(LHS);
          Instruction *Z = dyn_cast<Instruction>(&I);
          Instruction *Yprime = dyn_cast<Instruction>(NewAdd);

          // prepare variables
          std::string reg_yprime_name = llvmberry::getVariable(*Yprime);

          int bitwidth = Z->getType()->getIntegerBitWidth();

          // propagate "Y = X & c2" in target
          llvmberry::propagateInstruction(hints, Y, Z, TGT);

          // propagate "Y' = X + c1" in target
          llvmberry::propagateInstruction(hints, Yprime, Z, TGT);

          // To Z, apply add_mask
          INFRULE(INSTPOS(TGT, Z), llvmberry::ConsAddMask::make(
                  REGISTER(*Z), REGISTER(*Y), REGISTER(*Yprime), VAL(X),
                  CONSTINT(c1, bitwidth), CONSTINT(c2, bitwidth), BITSIZE(bitwidth)));
          
          // Propagate yp_var maydiff global
          llvmberry::propagateMaydiffGlobal(hints, reg_yprime_name, llvmberry::Physical);
                  
          insertSrcNopAtTgtI(hints, Yprime);
        });
        return BinaryOperator::CreateAnd(NewAdd, C2);
      }
    }

    // Try to fold constant add into select arguments.
    if (SelectInst *SI = dyn_cast<SelectInst>(LHS))
      if (Instruction *R = FoldOpIntoSelect(I, SI))
        return R;
  }

  // add (select X 0 (sub n A)) A  -->  select X A n
  {
    SelectInst *SI = dyn_cast<SelectInst>(LHS);
    Value *A = RHS;
    bool llvmberry_needsCommutativity = false;
    if (!SI) {
      SI = dyn_cast<SelectInst>(RHS);
      A = LHS;
      llvmberry_needsCommutativity = true;
    }
    if (SI && SI->hasOneUse()) {
      Value *TV = SI->getTrueValue();
      Value *FV = SI->getFalseValue();
      Value *N;

      // Can we fold the add into the argument of the select?
      // We check both true and false select arguments for a matching subtract.
      if (match(FV, m_Zero()) && match(TV, m_Sub(m_Value(N), m_Specific(A)))){
        llvmberry::ValidationUnit::Begin("add_select_zero", I.getParent()->getParent());
        llvmberry::generateHintForAddSelectZero(&I, dyn_cast<BinaryOperator>(TV), SI,
                llvmberry_needsCommutativity,
                true);
        
        // Fold the add into the true select value.
        return SelectInst::Create(SI->getCondition(), N, A);
      }

      if (match(TV, m_Zero()) && match(FV, m_Sub(m_Value(N), m_Specific(A)))){
        llvmberry::ValidationUnit::Begin("add_select_zero2", I.getParent()->getParent());
        llvmberry::generateHintForAddSelectZero(&I, dyn_cast<BinaryOperator>(FV), SI,
                llvmberry_needsCommutativity,
                false);

        // Fold the add into the false select value.
        return SelectInst::Create(SI->getCondition(), A, N);
      }
    }
  }

  // Check for (add (sext x), y), see if we can merge this into an
  // integer add followed by a sext.
  if (SExtInst *LHSConv = dyn_cast<SExtInst>(LHS)) {
    // (add (sext x), cst) --> (sext (add x, cst'))
    if (ConstantInt *RHSC = dyn_cast<ConstantInt>(RHS)) {
      Constant *CI =
        ConstantExpr::getTrunc(RHSC, LHSConv->getOperand(0)->getType());
      if (LHSConv->hasOneUse() &&
          ConstantExpr::getSExt(CI, I.getType()) == RHSC &&
          WillNotOverflowSignedAdd(LHSConv->getOperand(0), CI, I)) {
        // Insert the new, smaller add.
        Value *NewAdd = Builder->CreateNSWAdd(LHSConv->getOperand(0),
                                              CI, "addconv");
        return new SExtInst(NewAdd, I.getType());
      }
    }

    // (add (sext x), (sext y)) --> (sext (add int x, y))
    if (SExtInst *RHSConv = dyn_cast<SExtInst>(RHS)) {
      // Only do this if x/y have the same type, if at last one of them has a
      // single use (so we don't increase the number of sexts), and if the
      // integer add will not overflow.
      if (LHSConv->getOperand(0)->getType() ==
              RHSConv->getOperand(0)->getType() &&
          (LHSConv->hasOneUse() || RHSConv->hasOneUse()) &&
          WillNotOverflowSignedAdd(LHSConv->getOperand(0),
                                   RHSConv->getOperand(0), I)) {
        // Insert the new integer add.
        Value *NewAdd = Builder->CreateNSWAdd(LHSConv->getOperand(0),
                                             RHSConv->getOperand(0), "addconv");
        return new SExtInst(NewAdd, I.getType());
      }
    }
  }

  // (add (xor A, B) (and A, B)) --> (or A, B)
  {
    Value *A = nullptr, *B = nullptr;
    if (match(RHS, m_Xor(m_Value(A), m_Value(B))) &&
        (match(LHS, m_And(m_Specific(A), m_Specific(B))) ||
         match(LHS, m_And(m_Specific(B), m_Specific(A))))){
      llvmberry::ValidationUnit::Begin("add_xor_and", I.getParent()->getParent());
      llvmberry::generateHintForAddXorAnd(&I, 
          dyn_cast<BinaryOperator>(RHS), dyn_cast<BinaryOperator>(LHS), 
          A, B, match(LHS, m_And(m_Specific(A), m_Specific(B))) ? false : true, true);
      return BinaryOperator::CreateOr(A, B);
    }

    if (match(LHS, m_Xor(m_Value(A), m_Value(B))) &&
        (match(RHS, m_And(m_Specific(A), m_Specific(B))) ||
         match(RHS, m_And(m_Specific(B), m_Specific(A))))){
      llvmberry::ValidationUnit::Begin("add_xor_and", I.getParent()->getParent());
      llvmberry::generateHintForAddXorAnd(&I, 
          dyn_cast<BinaryOperator>(LHS), dyn_cast<BinaryOperator>(RHS), 
          A, B, match(LHS, m_And(m_Specific(A), m_Specific(B))) ? false : true, false);
      return BinaryOperator::CreateOr(A, B);
    }
  }

  // (add (or A, B) (and A, B)) --> (add A, B)
  {
    Value *A = nullptr, *B = nullptr;
    if (match(RHS, m_Or(m_Value(A), m_Value(B))) &&
        (match(LHS, m_And(m_Specific(A), m_Specific(B))) ||
         match(LHS, m_And(m_Specific(B), m_Specific(A))))) {
      llvmberry::ValidationUnit::Begin("add_or_and", I.getParent()->getParent());
      auto *New = BinaryOperator::CreateAdd(A, B);
      New->setHasNoSignedWrap(I.hasNoSignedWrap());
      New->setHasNoUnsignedWrap(I.hasNoUnsignedWrap());
      llvmberry::generateHintForAddOrAnd(&I, 
          dyn_cast<BinaryOperator>(RHS), dyn_cast<BinaryOperator>(LHS), 
          A, B, match(LHS, m_And(m_Specific(A), m_Specific(B))) ? false : true, true);
      return New;
    }

    if (match(LHS, m_Or(m_Value(A), m_Value(B))) &&
        (match(RHS, m_And(m_Specific(A), m_Specific(B))) ||
         match(RHS, m_And(m_Specific(B), m_Specific(A))))) {
      llvmberry::ValidationUnit::Begin("add_or_and", I.getParent()->getParent());
      auto *New = BinaryOperator::CreateAdd(A, B);
      New->setHasNoSignedWrap(I.hasNoSignedWrap());
      New->setHasNoUnsignedWrap(I.hasNoUnsignedWrap());
      llvmberry::generateHintForAddOrAnd(&I, 
          dyn_cast<BinaryOperator>(LHS), dyn_cast<BinaryOperator>(RHS), 
          A, B, match(RHS, m_And(m_Specific(A), m_Specific(B))) ? false : true, false);
      return New;
    }
  }

  // TODO(jingyue): Consider WillNotOverflowSignedAdd and
  // WillNotOverflowUnsignedAdd to reduce the number of invocations of
  // computeKnownBits.
  if (!I.hasNoSignedWrap() && WillNotOverflowSignedAdd(LHS, RHS, I)) {
    Changed = true;
    I.setHasNoSignedWrap(true);
  }
  if (!I.hasNoUnsignedWrap() &&
      computeOverflowForUnsignedAdd(LHS, RHS, &I) ==
          OverflowResult::NeverOverflows) {
    Changed = true;
    I.setHasNoUnsignedWrap(true);
  }

  return Changed ? &I : nullptr;
}

Instruction *InstCombiner::visitFAdd(BinaryOperator &I) {
  bool Changed = SimplifyAssociativeOrCommutative(I);
  Value *LHS = I.getOperand(0), *RHS = I.getOperand(1);

  if (Value *V = SimplifyVectorOp(I))
    return ReplaceInstUsesWith(I, V);

  if (Value *V =
          SimplifyFAddInst(LHS, RHS, I.getFastMathFlags(), DL, TLI, DT, AC))
    return ReplaceInstUsesWith(I, V);

  if (isa<Constant>(RHS)) {
    if (isa<PHINode>(LHS))
      if (Instruction *NV = FoldOpIntoPhi(I))
        return NV;

    if (SelectInst *SI = dyn_cast<SelectInst>(LHS))
      if (Instruction *NV = FoldOpIntoSelect(I, SI))
        return NV;
  }

  // -A + B  -->  B - A
  // -A + -B  -->  -(A + B)
  if (Value *LHSV = dyn_castFNegVal(LHS)) {
    Instruction *RI = BinaryOperator::CreateFSub(RHS, LHSV);
    RI->copyFastMathFlags(&I);
    return RI;
  }

  // A + -B  -->  A - B
  if (!isa<Constant>(RHS))
    if (Value *V = dyn_castFNegVal(RHS)) {
      Instruction *RI = BinaryOperator::CreateFSub(LHS, V);
      RI->copyFastMathFlags(&I);
      return RI;
    }

  // Check for (fadd double (sitofp x), y), see if we can merge this into an
  // integer add followed by a promotion.
  if (SIToFPInst *LHSConv = dyn_cast<SIToFPInst>(LHS)) {
    // (fadd double (sitofp x), fpcst) --> (sitofp (add int x, intcst))
    // ... if the constant fits in the integer value.  This is useful for things
    // like (double)(x & 1234) + 4.0 -> (double)((X & 1234)+4) which no longer
    // requires a constant pool load, and generally allows the add to be better
    // instcombined.
    if (ConstantFP *CFP = dyn_cast<ConstantFP>(RHS)) {
      Constant *CI =
      ConstantExpr::getFPToSI(CFP, LHSConv->getOperand(0)->getType());
      if (LHSConv->hasOneUse() &&
          ConstantExpr::getSIToFP(CI, I.getType()) == CFP &&
          WillNotOverflowSignedAdd(LHSConv->getOperand(0), CI, I)) {
        // Insert the new integer add.
        Value *NewAdd = Builder->CreateNSWAdd(LHSConv->getOperand(0),
                                              CI, "addconv");
        return new SIToFPInst(NewAdd, I.getType());
      }
    }

    // (fadd double (sitofp x), (sitofp y)) --> (sitofp (add int x, y))
    if (SIToFPInst *RHSConv = dyn_cast<SIToFPInst>(RHS)) {
      // Only do this if x/y have the same type, if at last one of them has a
      // single use (so we don't increase the number of int->fp conversions),
      // and if the integer add will not overflow.
      if (LHSConv->getOperand(0)->getType() ==
              RHSConv->getOperand(0)->getType() &&
          (LHSConv->hasOneUse() || RHSConv->hasOneUse()) &&
          WillNotOverflowSignedAdd(LHSConv->getOperand(0),
                                   RHSConv->getOperand(0), I)) {
        // Insert the new integer add.
        Value *NewAdd = Builder->CreateNSWAdd(LHSConv->getOperand(0),
                                              RHSConv->getOperand(0),"addconv");
        return new SIToFPInst(NewAdd, I.getType());
      }
    }
  }

  // select C, 0, B + select C, A, 0 -> select C, A, B
  {
    Value *A1, *B1, *C1, *A2, *B2, *C2;
    if (match(LHS, m_Select(m_Value(C1), m_Value(A1), m_Value(B1))) &&
        match(RHS, m_Select(m_Value(C2), m_Value(A2), m_Value(B2)))) {
      if (C1 == C2) {
        Constant *Z1=nullptr, *Z2=nullptr;
        Value *A, *B, *C=C1;
        if (match(A1, m_AnyZero()) && match(B2, m_AnyZero())) {
            Z1 = dyn_cast<Constant>(A1); A = A2;
            Z2 = dyn_cast<Constant>(B2); B = B1;
        } else if (match(B1, m_AnyZero()) && match(A2, m_AnyZero())) {
            Z1 = dyn_cast<Constant>(B1); B = B2;
            Z2 = dyn_cast<Constant>(A2); A = A1;
        }

        if (Z1 && Z2 &&
            (I.hasNoSignedZeros() ||
             (Z1->isNegativeZeroValue() && Z2->isNegativeZeroValue()))) {
          return SelectInst::Create(C, A, B);
        }
      }
    }
  }

  if (I.hasUnsafeAlgebra()) {
    if (Value *V = FAddCombine(Builder).simplify(&I))
      return ReplaceInstUsesWith(I, V);
  }

  return Changed ? &I : nullptr;
}


/// Optimize pointer differences into the same array into a size.  Consider:
///  &A[10] - &A[0]: we should compile this to "10".  LHS/RHS are the pointer
/// operands to the ptrtoint instructions for the LHS/RHS of the subtract.
///
Value *InstCombiner::OptimizePointerDifference(Value *LHS, Value *RHS,
                                               Type *Ty) {
  // If LHS is a gep based on RHS or RHS is a gep based on LHS, we can optimize
  // this.
  bool Swapped = false;
  GEPOperator *GEP1 = nullptr, *GEP2 = nullptr;

  // For now we require one side to be the base pointer "A" or a constant
  // GEP derived from it.
  if (GEPOperator *LHSGEP = dyn_cast<GEPOperator>(LHS)) {
    // (gep X, ...) - X
    if (LHSGEP->getOperand(0) == RHS) {
      GEP1 = LHSGEP;
      Swapped = false;
    } else if (GEPOperator *RHSGEP = dyn_cast<GEPOperator>(RHS)) {
      // (gep X, ...) - (gep X, ...)
      if (LHSGEP->getOperand(0)->stripPointerCasts() ==
            RHSGEP->getOperand(0)->stripPointerCasts()) {
        GEP2 = RHSGEP;
        GEP1 = LHSGEP;
        Swapped = false;
      }
    }
  }

  if (GEPOperator *RHSGEP = dyn_cast<GEPOperator>(RHS)) {
    // X - (gep X, ...)
    if (RHSGEP->getOperand(0) == LHS) {
      GEP1 = RHSGEP;
      Swapped = true;
    } else if (GEPOperator *LHSGEP = dyn_cast<GEPOperator>(LHS)) {
      // (gep X, ...) - (gep X, ...)
      if (RHSGEP->getOperand(0)->stripPointerCasts() ==
            LHSGEP->getOperand(0)->stripPointerCasts()) {
        GEP2 = LHSGEP;
        GEP1 = RHSGEP;
        Swapped = true;
      }
    }
  }

  // Avoid duplicating the arithmetic if GEP2 has non-constant indices and
  // multiple users.
  if (!GEP1 ||
      (GEP2 && !GEP2->hasAllConstantIndices() && !GEP2->hasOneUse()))
    return nullptr;

  // Emit the offset of the GEP and an intptr_t.
  Value *Result = EmitGEPOffset(GEP1);

  // If we had a constant expression GEP on the other side offsetting the
  // pointer, subtract it from the offset we have.
  if (GEP2) {
    Value *Offset = EmitGEPOffset(GEP2);
    Result = Builder->CreateSub(Result, Offset);
  }

  // If we have p - gep(p, ...)  then we have to negate the result.
  if (Swapped)
    Result = Builder->CreateNeg(Result, "diff.neg");

  return Builder->CreateIntCast(Result, Ty, true);
}

Instruction *InstCombiner::visitSub(BinaryOperator &I) {
  Value *Op0 = I.getOperand(0), *Op1 = I.getOperand(1);

  if (Value *V = SimplifyVectorOp(I))
    return ReplaceInstUsesWith(I, V);

  if (Value *V = SimplifySubInst(Op0, Op1, I.hasNoSignedWrap(),
                                 I.hasNoUnsignedWrap(), DL, TLI, DT, AC))
    return ReplaceInstUsesWith(I, V);

  // (A*B)-(A*C) -> A*(B-C) etc
  if (Value *V = SimplifyUsingDistributiveLaws(I))
    return ReplaceInstUsesWith(I, V);

  // If this is a 'B = x-(-A)', change to B = x+A.
  if (Value *V = dyn_castNegVal(Op1)) {
    BinaryOperator *Res = BinaryOperator::CreateAdd(Op0, V);

    if (const auto *BO = dyn_cast<BinaryOperator>(Op1)) {
      assert(BO->getOpcode() == Instruction::Sub &&
             "Expected a subtraction operator!");
      if (BO->hasNoSignedWrap() && I.hasNoSignedWrap())
        Res->setHasNoSignedWrap(true);
    } else {
      if (cast<Constant>(Op1)->isNotMinSignedValue() && I.hasNoSignedWrap())
        Res->setHasNoSignedWrap(true);
    }

    llvmberry::ValidationUnit::Begin("sub_add", I.getParent()->getParent());

    // Op1 will be propagated to Z if it is id
    // Inference rule will be applied if is a constant
    llvmberry::generateHintForNegValue(Op1, I);

    INTRUDE(CAPTURE(&Op0, &I, &Op1, &V), {
      //    <src>   |    <tgt>
      // my = 0 - y | my = 0 - y
      // z = x - my | z = x + y
      //std::string reg0_name = llvmberry::getVariable(I);

      /*
      INFRULE(INSTPOS(SRC, &I), llvmberry::ConsSubAdd::make(
              REGISTER(reg0_name), VAL(Op1), VAL(Op0), VAL(V),
              BITSIZE(I)));
      */
      INFRULE(INSTPOS(SRC, &I), llvmberry::ConsSubAdd::make(
              REGISTER(I), VAL(Op1), VAL(Op0), VAL(V), BITSIZE(I)));
    });

    return Res;
  }


  if (I.getType()->isIntegerTy(1)){
    llvmberry::ValidationUnit::Begin("sub_onebit", I.getParent()->getParent());

    INTRUDE(CAPTURE(&I), {
      //   <src>          <tgt>
      // Z = X - Y  | Z = X ^ Y
      BinaryOperator *Z = &I;

      INFRULE(INSTPOS(SRC, Z), llvmberry::ConsSubOnebit::make(
              REGISTER(*Z), VAL(Z->getOperand(0)), VAL(Z->getOperand(1))));
    });

    return BinaryOperator::CreateXor(Op0, Op1);
  }

  // Replace (-1 - A) with (~A).
  if (match(Op0, m_AllOnes())){
    llvmberry::ValidationUnit::Begin("sub_mone", I.getParent()->getParent());

    INTRUDE(CAPTURE(&I), {
      //   <src>          <tgt>
      // Z = -1 - X  | Z = -1 ^ X
      BinaryOperator *Z = &I;

      INFRULE(INSTPOS(SRC, Z), llvmberry::ConsSubMone::make(
              REGISTER(*Z), VAL(Z->getOperand(1)), BITSIZE(*Z)));
    });


    return BinaryOperator::CreateNot(Op1);
  }

  if (Constant *C = dyn_cast<Constant>(Op0)) {
    // C - ~X == X + (1+C)
    Value *X = nullptr;
    if (match(Op1, m_Not(m_Value(X)))){
      llvmberry::ValidationUnit::Begin("sub_const_not", I.getParent()->getParent());

      INTRUDE(CAPTURE(&I, &C, &X), {
        //     <src>     <tgt>
        // Y = X  ^ -1 | Y = X ^ -1
        // Z = C1 -  Y | Z = X + C2

        BinaryOperator *Z = &I;
        ConstantInt *C1 = dyn_cast<ConstantInt>(C);
        BinaryOperator *Y = dyn_cast<BinaryOperator>(Z->getOperand(1));

        //int64_t c1 = C1->getSExtValue();
        //int64_t c2 = c1 + 1;
        int64_t c2 = C1->getSExtValue() + 1;
        int bitwidth = Z->getType()->getIntegerBitWidth();

        llvmberry::propagateInstruction(hints, Y, Z, SRC);

        /*
        INFRULE(INSTPOS(SRC, Z), llvmberry::ConsSubConstNot::make(
                REGISTER(*Z), REGISTER(*Y), VAL(X),
                CONSTINT(c1, bitwidth), CONSTINT(c2, bitwidth),
                BITSIZE(bitwidth)));
        */
        INFRULE(INSTPOS(SRC, Z), llvmberry::ConsSubConstNot::make(
                REGISTER(*Z), REGISTER(*Y), VAL(X),
                CONSTINT(C1), CONSTINT(c2, bitwidth), BITSIZE(bitwidth)));
      });


      return BinaryOperator::CreateAdd(X, AddOne(C));
    }

    // Try to fold constant sub into select arguments.
    if (SelectInst *SI = dyn_cast<SelectInst>(Op1))
      if (Instruction *R = FoldOpIntoSelect(I, SI))
        return R;

    // C-(X+C2) --> (C-C2)-X
    Constant *C2;
    if (match(Op1, m_Add(m_Value(X), m_Constant(C2)))){
      llvmberry::ValidationUnit::Begin("sub_const_add", I.getParent()->getParent());
      INTRUDE(CAPTURE(&I, &C2, &X, &C), {
        //     <src>     <tgt>
        // Y = X  + C1 | Y = X  + C1
        // Z = C2 -  Y | Z = (C2 - C1) - X

        BinaryOperator *Z = &I;
        if (Z->getType()->isVectorTy()) {
          llvmberry::ValidationUnit::Abort();
          return;
        }
        ConstantInt *C1 = dyn_cast<ConstantInt>(C2);
        ConstantInt *C2 = dyn_cast<ConstantInt>(C);
        BinaryOperator *Y = dyn_cast<BinaryOperator>(Z->getOperand(1));

        int64_t c1 = C1->getSExtValue();
        int64_t c2 = C2->getSExtValue();
        //int64_t c3 = c2 - c1;
        int bitwidth = Z->getType()->getIntegerBitWidth();

        llvmberry::propagateInstruction(hints, Y, Z, SRC);

        INFRULE(INSTPOS(SRC, Z), llvmberry::ConsSubConstAdd::make(
                REGISTER(*Z), REGISTER(*Y), VAL(X),
                CONSTINT(c1, bitwidth), CONSTINT(c2, bitwidth),
                CONSTINT(c2 - c1, bitwidth), BITSIZE(bitwidth)));
      });
      return BinaryOperator::CreateSub(ConstantExpr::getSub(C, C2), X);
    }

    if (SimplifyDemandedInstructionBits(I))
      return &I;

    // Fold (sub 0, (zext bool to B)) --> (sext bool to B)
    if (C->isNullValue() && match(Op1, m_ZExt(m_Value(X))))
      if (X->getType()->getScalarType()->isIntegerTy(1))
        return CastInst::CreateSExtOrBitCast(X, Op1->getType());

    // Fold (sub 0, (sext bool to B)) --> (zext bool to B)
    if (C->isNullValue() && match(Op1, m_SExt(m_Value(X))))
      if (X->getType()->getScalarType()->isIntegerTy(1))
        return CastInst::CreateZExtOrBitCast(X, Op1->getType());
  }

  if (ConstantInt *C = dyn_cast<ConstantInt>(Op0)) {
    // -(X >>u 31) -> (X >>s 31)
    // -(X >>s 31) -> (X >>u 31)
    if (C->isZero()) {
      Value *X;
      ConstantInt *CI;
      if (match(Op1, m_LShr(m_Value(X), m_ConstantInt(CI))) &&
          // Verify we are shifting out everything but the sign bit.
          CI->getValue() == I.getType()->getPrimitiveSizeInBits() - 1)
        return BinaryOperator::CreateAShr(X, CI);

      if (match(Op1, m_AShr(m_Value(X), m_ConstantInt(CI))) &&
          // Verify we are shifting out everything but the sign bit.
          CI->getValue() == I.getType()->getPrimitiveSizeInBits() - 1)
        return BinaryOperator::CreateLShr(X, CI);
    }

    // Turn this into a xor if LHS is 2^n-1 and the remaining bits are known
    // zero.
    APInt IntVal = C->getValue();
    if ((IntVal + 1).isPowerOf2()) {
      unsigned BitWidth = I.getType()->getScalarSizeInBits();
      APInt KnownZero(BitWidth, 0);
      APInt KnownOne(BitWidth, 0);
      computeKnownBits(&I, KnownZero, KnownOne, 0, &I);
      if ((IntVal | KnownZero).isAllOnesValue()) {
        return BinaryOperator::CreateXor(Op1, C);
      }
    }
  }


  {
    Value *Y;
    // X-(X+Y) == -Y    X-(Y+X) == -Y
    if (match(Op1, m_Add(m_Specific(Op0), m_Value(Y))) ||
        match(Op1, m_Add(m_Value(Y), m_Specific(Op0)))) {
      llvmberry::ValidationUnit::Begin("sub_remove", I.getParent()->getParent());
      INTRUDE(CAPTURE(&I, &Op1, &Op0, &Y), {
        // prepare variable
        Instruction *reg_y_instr = dyn_cast<Instruction>(Op1);

        std::string reg_y_name = llvmberry::getVariable(*Op1);
        llvmberry::propagateInstruction(hints, reg_y_instr, &I, SRC);

        INFRULE(INSTPOS(SRC, reg_y_instr), llvmberry::ConsBopCommutative::make(
                VAR(reg_y_name), llvmberry::TyBop::BopAdd, VAL(Op0), VAL(Y), BITSIZE(*Op1)));
        INFRULE(INSTPOS(SRC, &I), llvmberry::ConsSubRemove::make(
                REGISTER(I), REGISTER(reg_y_name), VAL(Op0), VAL(Y), BITSIZE(*Op1)));
      });
      llvmberry::ValidationUnit::End();

      return BinaryOperator::CreateNeg(Y);
    }

    // (X-Y)-X == -Y
    if (match(Op0, m_Sub(m_Specific(Op1), m_Value(Y)))){
      llvmberry::ValidationUnit::Begin("sub_sub", I.getParent()->getParent());
      INTRUDE(CAPTURE(&I, &Op1, &Op0, &Y), {
        //    <src>      <tgt>
        // W = X - Y | W = X - Y
        // Z = W - X | Z = 0 - Y
        BinaryOperator *W = dyn_cast<BinaryOperator>(Op0);
        //assert(W);

        llvmberry::propagateInstruction(hints, W, &I, SRC);

        /*
        INFRULE(INSTPOS(SRC, &I), llvmberry::ConsSubSub::make(
                REGISTER(llvmberry::getVariable(I)), VAL(Op1), VAL(Y),
                VAL(Op0), BITSIZE(*Op1)));
        */
        INFRULE(INSTPOS(SRC, &I), llvmberry::ConsSubSub::make(
                REGISTER(I), VAL(Op1), VAL(Y), VAL(Op0), BITSIZE(*Op1)));
      });
      return BinaryOperator::CreateNeg(Y);
    }
  }

  // (sub (or A, B) (xor A, B)) --> (and A, B)
  {
    Value *A = nullptr, *B = nullptr;
    if (match(Op1, m_Xor(m_Value(A), m_Value(B))) &&
        (match(Op0, m_Or(m_Specific(A), m_Specific(B))) ||
         match(Op0, m_Or(m_Specific(B), m_Specific(A))))){
      llvmberry::ValidationUnit::Begin("sub_or_xor", I.getParent()->getParent());
      INTRUDE(CAPTURE(&I, &Op1, &Op0, &A, &B), {
        //    <src>      <tgt>
        // X = A | B | X = A | B
        // Y = A ^ B | Y = A ^ B
        // Z = X - Y | Z = A & B
        BinaryOperator *Z = &I;
        BinaryOperator *X = dyn_cast<BinaryOperator>(Op0);
        BinaryOperator *Y = dyn_cast<BinaryOperator>(Op1);
        //assert(X);
        //assert(Y);

        llvmberry::propagateInstruction(hints, X, Z, SRC);
        llvmberry::propagateInstruction(hints, Y, Z, SRC);
        if(match(X, m_Or(m_Specific(B), m_Specific(A))))
          llvmberry::applyCommutativity(hints, Z, X, SRC);

        INFRULE(INSTPOS(SRC, &I), llvmberry::ConsSubOrXor::make(
                REGISTER(*Z), VAL(A), VAL(B), REGISTER(*X), REGISTER(*Y), BITSIZE(*Z)));
      });
      return BinaryOperator::CreateAnd(A, B);
    }
  }

  // (sub (select (a, c, b)), (select (a, d, b))) -> (select (a, (sub c, d), 0))
  // (sub (select (a, b, c)), (select (a, b, d))) -> (select (a, 0, (sub c, d)))
  if (auto *SI0 = dyn_cast<SelectInst>(Op0)) {
    if (auto *SI1 = dyn_cast<SelectInst>(Op1)) {
      if (SI0->getCondition() == SI1->getCondition()) {
        if (Value *V = SimplifySubInst(
                SI0->getFalseValue(), SI1->getFalseValue(), I.hasNoSignedWrap(),
                I.hasNoUnsignedWrap(), DL, TLI, DT, AC))
          return SelectInst::Create(
              SI0->getCondition(),
              Builder->CreateSub(SI0->getTrueValue(), SI1->getTrueValue(), "",
                                 /*HasNUW=*/I.hasNoUnsignedWrap(),
                                 /*HasNSW=*/I.hasNoSignedWrap()),
              V);
        if (Value *V = SimplifySubInst(SI0->getTrueValue(), SI1->getTrueValue(),
                                       I.hasNoSignedWrap(),
                                       I.hasNoUnsignedWrap(), DL, TLI, DT, AC))
          return SelectInst::Create(
              SI0->getCondition(), V,
              Builder->CreateSub(SI0->getFalseValue(), SI1->getFalseValue(), "",
                                 /*HasNUW=*/I.hasNoUnsignedWrap(),
                                 /*HasNSW=*/I.hasNoSignedWrap()));
      }
    }
  }

  if (Op0->hasOneUse()) {
    Value *Y = nullptr;
    // ((X | Y) - X) --> (~X & Y)
    if (match(Op0, m_Or(m_Value(Y), m_Specific(Op1))) ||
        match(Op0, m_Or(m_Specific(Op1), m_Value(Y))))
      return BinaryOperator::CreateAnd(
          Y, Builder->CreateNot(Op1, Op1->getName() + ".not"));
  }

  if (Op1->hasOneUse()) {
    Value *X = nullptr, *Y = nullptr, *Z = nullptr;
    Constant *C = nullptr;
    Constant *CI = nullptr;

    // (X - (Y - Z))  -->  (X + (Z - Y)).
    if (match(Op1, m_Sub(m_Value(Y), m_Value(Z))))
      return BinaryOperator::CreateAdd(Op0,
                                      Builder->CreateSub(Z, Y, Op1->getName()));

    // (X - (X & Y))   -->   (X & ~Y)
    //
    if (match(Op1, m_And(m_Value(Y), m_Specific(Op0))) ||
        match(Op1, m_And(m_Specific(Op0), m_Value(Y))))
      return BinaryOperator::CreateAnd(Op0,
                                  Builder->CreateNot(Y, Y->getName() + ".not"));

    // 0 - (X sdiv C)  -> (X sdiv -C)  provided the negation doesn't overflow.
    if (match(Op1, m_SDiv(m_Value(X), m_Constant(C))) && match(Op0, m_Zero()) &&
        C->isNotMinSignedValue() && !C->isOneValue()){
      llvmberry::ValidationUnit::Begin("sub_sdiv", I.getParent()->getParent());
      INTRUDE(CAPTURE(&I, &Op1, &X, &C), {
        //    <src>      <tgt>
        // Y = X / C  | Y = X / C
        // Z = 0 - Y  | Z = X / C'
        BinaryOperator *Z = &I;
        BinaryOperator *Y = (BinaryOperator *)Op1;

        int bitwidth = Z->getType()->getIntegerBitWidth();
        //ConstantInt *C_ci = dyn_cast<ConstantInt>(C);
        //int64_t c_val = C_ci->getSExtValue();
        //int64_t cprime_val = -c_val;
        int64_t c_val = dyn_cast<ConstantInt>(C)->getSExtValue();

        llvmberry::propagateInstruction(hints, Y, Z, SRC);
        /*
        INFRULE(INSTPOS(SRC, Z), llvmberry::ConsSubSdiv::make(
                REGISTER(*Z), REGISTER(*Y), VAL(X),
                CONSTINT(c_val, bitwidth), CONSTINT(cprime_val, bitwidth),
                BITSIZE(bitwidth)));
        */
        INFRULE(INSTPOS(SRC, Z), llvmberry::ConsSubSdiv::make(
                REGISTER(*Z), REGISTER(*Y), VAL(X), CONSTINT(c_val, bitwidth),
                CONSTINT(-c_val, bitwidth), BITSIZE(bitwidth)));
      });

      return BinaryOperator::CreateSDiv(X, ConstantExpr::getNeg(C));
    }

    // 0 - (X << Y)  -> (-X << Y)   when X is freely negatable.
    if (match(Op1, m_Shl(m_Value(X), m_Value(Y))) && match(Op0, m_Zero())){
      if (Value *XNeg = dyn_castNegVal(X)){
        {
          //     <src>       <tgt>
          // X = 0 - mX | X =  0 - mX
          // Y = X << A | Y =  X << A
          // Z = 0 -  Y | Z = mX << A
          BinaryOperator *Z = &I;
          BinaryOperator *Y = dyn_cast<BinaryOperator>(Op1);
          Value *A = Y->getOperand(1);
          Value *mX = XNeg;
          llvmberry::ValidationUnit::Begin("sub_shl", I.getParent()->getParent());
          llvmberry::generateHintForNegValue(X, I);
          INTRUDE(CAPTURE(Z, Y, X, A, mX), {
            llvmberry::propagateInstruction(hints, Y, Z, SRC);
            INFRULE(INSTPOS(SRC, Z), llvmberry::ConsSubShl::make(
                    REGISTER(*Z), VAL(X), REGISTER(*Y), VAL(mX), VAL(A), BITSIZE(*Z)));
          });
        }
 
        return BinaryOperator::CreateShl(XNeg, Y);
      }
    }

    // X - A*-B -> X + A*B
    // X - -A*B -> X + A*B
    Value *A, *B;
    if (match(Op1, m_Mul(m_Value(A), m_Neg(m_Value(B)))) ||
        match(Op1, m_Mul(m_Neg(m_Value(A)), m_Value(B))))
      return BinaryOperator::CreateAdd(Op0, Builder->CreateMul(A, B));

    // X - A*CI -> X + A*-CI
    // X - CI*A -> X + A*-CI
    if (match(Op1, m_Mul(m_Value(A), m_Constant(CI))) ||
        match(Op1, m_Mul(m_Constant(CI), m_Value(A)))) {
      Value *NewMul = Builder->CreateMul(A, ConstantExpr::getNeg(CI));
      return BinaryOperator::CreateAdd(Op0, NewMul);
    }
  }

  // Optimize pointer differences into the same array into a size.  Consider:
  //  &A[10] - &A[0]: we should compile this to "10".
  Value *LHSOp, *RHSOp;
  if (match(Op0, m_PtrToInt(m_Value(LHSOp))) &&
      match(Op1, m_PtrToInt(m_Value(RHSOp))))
    if (Value *Res = OptimizePointerDifference(LHSOp, RHSOp, I.getType()))
      return ReplaceInstUsesWith(I, Res);

  // trunc(p)-trunc(q) -> trunc(p-q)
  if (match(Op0, m_Trunc(m_PtrToInt(m_Value(LHSOp)))) &&
      match(Op1, m_Trunc(m_PtrToInt(m_Value(RHSOp)))))
    if (Value *Res = OptimizePointerDifference(LHSOp, RHSOp, I.getType()))
      return ReplaceInstUsesWith(I, Res);

  bool Changed = false;
  if (!I.hasNoSignedWrap() && WillNotOverflowSignedSub(Op0, Op1, I)) {
    Changed = true;
    I.setHasNoSignedWrap(true);
  }
  if (!I.hasNoUnsignedWrap() && WillNotOverflowUnsignedSub(Op0, Op1, I)) {
    Changed = true;
    I.setHasNoUnsignedWrap(true);
  }

  return Changed ? &I : nullptr;
}

Instruction *InstCombiner::visitFSub(BinaryOperator &I) {
  Value *Op0 = I.getOperand(0), *Op1 = I.getOperand(1);

  if (Value *V = SimplifyVectorOp(I))
    return ReplaceInstUsesWith(I, V);

  if (Value *V =
          SimplifyFSubInst(Op0, Op1, I.getFastMathFlags(), DL, TLI, DT, AC))
    return ReplaceInstUsesWith(I, V);

  // fsub nsz 0, X ==> fsub nsz -0.0, X
  if (I.getFastMathFlags().noSignedZeros() && match(Op0, m_Zero())) {
    // Subtraction from -0.0 is the canonical form of fneg.
    Instruction *NewI = BinaryOperator::CreateFNeg(Op1);
    NewI->copyFastMathFlags(&I);
    return NewI;
  }

  if (isa<Constant>(Op0))
    if (SelectInst *SI = dyn_cast<SelectInst>(Op1))
      if (Instruction *NV = FoldOpIntoSelect(I, SI))
        return NV;

  // If this is a 'B = x-(-A)', change to B = x+A, potentially looking
  // through FP extensions/truncations along the way.
  if (Value *V = dyn_castFNegVal(Op1)) {
    Instruction *NewI = BinaryOperator::CreateFAdd(Op0, V);
    NewI->copyFastMathFlags(&I);
    return NewI;
  }
  if (FPTruncInst *FPTI = dyn_cast<FPTruncInst>(Op1)) {
    if (Value *V = dyn_castFNegVal(FPTI->getOperand(0))) {
      Value *NewTrunc = Builder->CreateFPTrunc(V, I.getType());
      Instruction *NewI = BinaryOperator::CreateFAdd(Op0, NewTrunc);
      NewI->copyFastMathFlags(&I);
      return NewI;
    }
  } else if (FPExtInst *FPEI = dyn_cast<FPExtInst>(Op1)) {
    if (Value *V = dyn_castFNegVal(FPEI->getOperand(0))) {
      Value *NewExt = Builder->CreateFPExt(V, I.getType());
      Instruction *NewI = BinaryOperator::CreateFAdd(Op0, NewExt);
      NewI->copyFastMathFlags(&I);
      return NewI;
    }
  }

  if (I.hasUnsafeAlgebra()) {
    if (Value *V = FAddCombine(Builder).simplify(&I))
      return ReplaceInstUsesWith(I, V);
  }

  return nullptr;
}
