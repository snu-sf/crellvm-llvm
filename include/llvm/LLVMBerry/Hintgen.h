#ifndef HINTGEN_H
#define HINTGEN_H

#include "llvm/LLVMBerry/Structure.h"


#define PHIPOS(SCOPE, PN, prevI) llvmberry::TyPosition::make(SCOPE, PN.getParent()->getName(), prevI->getParent()->getName()) 
#define PHIPOSJustPhi(SCOPE, PN) llvmberry::TyPosition::make(SCOPE, PN.getParent()->getName(), "")
#define INSTPOS(SCOPE, I) llvmberry::TyPosition::make(SCOPE, *(I))
#define INFRULE(pos, x) hints.addCommand(llvmberry::ConsInfrule::make(pos, x))
#define PROPAGATE(what, where) hints.addCommand(llvmberry::ConsPropagate::make(what, where))
#define VAR(name, tag) llvmberry::ConsVar::make(name, llvmberry::tag)
#define RHS(name, tag, SCOPE) llvmberry::ConsRhs::make(name, llvmberry::tag, SCOPE)
#define BOUNDS(from, to) llvmberry::ConsBounds::make(from, to)
#define LESSDEF(left, right, SCOPE) llvmberry::ConsLessdef::make(left, right, SCOPE)
#define INSN(x) llvmberry::ConsInsn::make(x)
#define VAL(I, tag) llvmberry::TyValue::make(*(I), llvmberry::tag)
#define EXPR(I, tag) llvmberry::TyExpr::make(*(I), llvmberry::tag)
#define REGISTER(name, tag) llvmberry::TyRegister::make(name, llvmberry::tag)
#define ID(name, tag) llvmberry::ConsId::make(name, llvmberry::tag)
#define BINOP(bop, type, val1, val2) llvmberry::ConsBinaryOp::make(bop, type, val1, val2)
#define TYPEOF(I) llvmberry::TyValueType::make(*((I)->getType()))

#define SRC llvmberry::Source
#define TGT llvmberry::Target


namespace llvmberry{
  /* applyCommutativity(I, (A bop B), scope) : 
   *   Applies commutativity rule ((A bop B) \in P => P += (B bop A)) to the position I
   */
  void applyCommutativity(llvm::Instruction *position, llvm::BinaryOperator *expression, TyScope scope);
  /* propagateInstruction(I1, I2, scope, propagateEquivalence) : 
   *   if propagateEquivalence == false : 
   *     Propagates I1 >= rhs(I1) from I1 to I2 if scope == Source, or
   *     Propagates rhs(I1) >= I1 from I1 to I2 if scope == Target
   *   else : 
   *     Propagate I1 >= rhs(I1) and rhs(I1) >= I1 from I1 to I2 in scope.
   */
  void propagateInstruction(llvm::Instruction *from, llvm::Instruction *to, TyScope scope, bool propagateEquivalence = false);
  void generateHintForNegValue(llvm::Value *V, llvm::BinaryOperator &I, TyScope scope = Source);
  /* generateHintForReplaceAllUsesWith(v1, v2) : 
   *   for each use U of v1, propagates hints that prove replacing v1 with v2 in U is safe
   */
  void generateHintForReplaceAllUsesWith(llvm::Instruction *source, llvm::Value *replaceTo);
  void generateHintForAddSelectZero(llvm::BinaryOperator *Z, 
          llvm::BinaryOperator *X, 
          llvm::SelectInst *Y, 
          bool needs_commutativity,
          bool is_leftform);
  void generateHintForOrXor(llvm::BinaryOperator &I, llvm::Value *op0, 
          llvm::Value *op1, bool needsCommutativity);
  void generateHintForOrXor2(llvm::BinaryOperator &I, 
          llvm::Value *X1_val, llvm::Value *X2_val,
          llvm::Value *A, llvm::Value *B,
          bool needsY1Commutativity, bool needsY2Commutativity);

  // inserting nop
  void insertTgtNopAtSrcI(CoreHint &hints, llvm::Instruction *I);
  void insertSrcNopAtTgtI(CoreHint &hints, llvm::Instruction *I);
}

#endif
