#ifndef HINTGEN_H
#define HINTGEN_H

#include "llvm/LLVMBerry/Structure.h"
#include <string>
#include <vector>
#include <memory>
#include "llvm/ADT/DenseMap.h"
#include "llvm/IR/CFG.h"
#include "llvm/Analysis/CFG.h"

#define PHIPOS(SCOPE, PN, prevI)                                               \
  llvmberry::TyPosition::make(SCOPE, PN.getParent()->getName(),                \
                              prevI->getParent()->getName())
#define PHIPOSJustPhi(SCOPE, PN)                                               \
  llvmberry::TyPosition::make(SCOPE, PN.getParent()->getName(), "")
#define INSTPOS(SCOPE, I) llvmberry::TyPosition::make(SCOPE, *(I))
#define INFRULE(pos, x) hints.addCommand(llvmberry::ConsInfrule::make(pos, x))
#define POINTER(v) llvmberry::TyPointer::make(*(v))
#define REGISTER(name, tag) llvmberry::TyRegister::make(name, llvmberry::tag)
#define BITSIZE(bitwidth) llvmberry::ConsSize::make(bitwidth)
#define PROPAGATE(what, where)                                                 \
  hints.addCommand(llvmberry::ConsPropagate::make(what, where))
#define BOUNDS(from, to) llvmberry::ConsBounds::make(from, to)
// EXPR, VAR, RHS, INSN macros make TyExpr object
#define VAR(name, tag) llvmberry::ConsVar::make(name, llvmberry::tag)
#define RHS(name, tag, SCOPE)                                                  \
  llvmberry::ConsRhs::make(name, llvmberry::tag, SCOPE)
#define INSN(x) llvmberry::ConsInsn::make((x))
#define EXPR(I, tag) llvmberry::TyExpr::make(*(I), llvmberry::tag)
// LESSDEF, NOALIAS, PRIVATE, MAYDIFF make TyPropagateObject instance
#define LESSDEF(left, right, SCOPE)                                            \
  llvmberry::ConsLessdef::make(left, right, SCOPE)
#define NOALIAS(ptr1, ptr2, SCOPE)                                             \
  llvmberry::ConsNoalias::make(ptr1, ptr2, SCOPE)
#define DIFFBLOCK(v1, v2, SCOPE)                                               \
  llvmberry::ConsDiffblock::make(v1, v2, SCOPE)
#define UNIQUE(reg, SCOPE) llvmberry::ConsUnique::make(reg, SCOPE)
#define PRIVATE(reg, SCOPE) llvmberry::ConsPrivate::make(reg, SCOPE)
#define MAYDIFF(name, tag) llvmberry::ConsMaydiff::make(name, tag)
// VAL, ID macros make TyValue object
#define VAL(I, tag) llvmberry::TyValue::make(*(I), llvmberry::tag)
#define ID(name, tag) llvmberry::ConsId::make(name, llvmberry::tag)
// VALTYPE, TYPEOF macros make TyValueType object
#define VALTYPE(ty) llvmberry::TyValueType::make(*(ty))
#define TYPEOF(I) llvmberry::TyValueType::make(*((I)->getType()))
// BINOP, FBINOP, BINARYINSN make TyInstruction object
#define BINOP(bop, type, val1, val2)                                           \
  llvmberry::ConsBinaryOp::make(bop, type, val1, val2)
#define FBINOP(fbop, type, val1, val2)                                         \
  llvmberry::ConsFloatBinaryOp::make(fbop, type, val1, val2)
#define BINARYINSN(binop, type, val1, val2)                                    \
  llvmberry::isFloatOpcode((binop).getOpcode())                                \
      ? FBINOP(llvmberry::getFbop((binop).getOpcode()), type, val1, val2)      \
      : BINOP(llvmberry::getBop((binop).getOpcode()), type, val1, val2)

#define SRC llvmberry::Source
#define TGT llvmberry::Target

namespace llvmberry {
/* applyCommutativity(I, (A bop B), scope) :
 *   Applies commutativity rule ((A bop B) \in P => P += (B bop A)) to the
 * position I
 */
void applyCommutativity(llvm::Instruction *position,
                        llvm::BinaryOperator *expression, TyScope scope);
/* applyTransitivity(I, v1, v2, v3, scope) :
 *   Applies transitivity rule (v1 >= v2 >= v3) to the position (I, scope)
 */
void applyTransitivity(llvm::Instruction *position, llvm::Value *v_greatest,
                       llvm::Value *v_mid, llvm::Value *v_smallest,
                       TyScope scope);
/* applyTransitivity(I, v1, v2, v3, scope, scopetag) :
 *   Applies transitivity rule (v1 >= v2 >= v3) to the position (I, scopetag)
 */
void applyTransitivity(llvm::Instruction *position, llvm::Value *v_greatest,
                       llvm::Value *v_mid, llvm::Value *v_smallest,
                       TyScope scope, TyScope position_scopetag);
/* propagateInstruction(I1, I2, scope, propagateEquivalence) :
 *   if propagateEquivalence == false :
 *     Propagates I1 >= rhs(I1) from I1 to I2 if scope == Source, or
 *     Propagates rhs(I1) >= I1 from I1 to I2 if scope == Target
 *   else :
 *     Propagate I1 >= rhs(I1) and rhs(I1) >= I1 from I1 to I2 in scope.
 */
void propagateInstruction(llvm::Instruction *from, llvm::Instruction *to,
                          TyScope scope, bool propagateEquivalence = false);
/* propagateLessdef(I1, I2, v1, v2, scope) :
 *   Propagates v1 >= v2 from I1 to I2 in scope
 */
void propagateLessdef(llvm::Instruction *from, llvm::Instruction *to,
                      const llvm::Value *lesserval,
                      const llvm::Value *greaterval, TyScope scope);
/* propagateMaydiffGlobal(var, tag) :
 *   Propagates variable (var, tag) globally
 */
void propagateMaydiffGlobal(std::string varname, TyTag tag);
/* generateHintForNegValue(V, I, Scope) :
 *   If V is a BinaryOperator (V = 0 - mV), propagate (V >= 0 - mV, 0 - mV >= V)
 * from V to I in Scope
 *   If V is a constant C, add invariants (C >=src 0 - (-C)), ((0 - (-C)) >=tgt
 * C). (It does nothing with scope)
 */
void generateHintForNegValue(llvm::Value *V, llvm::BinaryOperator &I,
                             TyScope scope = Source);
/* generateHintForReplaceAllUsesWith(v1, v2) :
 *   For each use U of v1, propagates hints to prove replacing v1 with v2 in U
 * is safe
 *   Invariant { v1 >=src v2 } must be given at definition of v1
 * generateHintForReplaceAllUsesWith(v1, v2, gv) :
 *   Invariant { v1 >=src gv } and { gv >=tgt v2 } must be given at definition
 * of v1
 */
void generateHintForReplaceAllUsesWith(
    llvm::Instruction *source, llvm::Value *replaceTo,
    std::string ghost_var = "", std::shared_ptr<TyPosition> source_pos =
                                    std::shared_ptr<TyPosition>(nullptr));
/* generateHintForReplaceAllUsesWithAtTgt(v1, v2) :
 *   For each use U of v1, propagates hints at target to prove that
 * replacing v1 with v2 in U is safe.
 *   Invariant { v1 >=tgt v2 } must be given at definition of v1
 */
void generateHintForReplaceAllUsesWithAtTgt(llvm::Instruction *source,
                                            llvm::Value *replaceTo);

/* Hint generation functions for instcombine/instsimplify micro-optimizations
 * that appear multiple times
 */
void generateHintForAddSelectZero(llvm::BinaryOperator *Z,
                                  llvm::BinaryOperator *X, llvm::SelectInst *Y,
                                  bool needs_commutativity, bool is_leftform);
void generateHintForOrAnd(llvm::BinaryOperator *Y, llvm::Value *X,
                          llvm::Value *A);
void generateHintForOrXor(llvm::BinaryOperator *W, llvm::Value *op0,
                          llvm::Value *op1, bool needsCommutativity);
void generateHintForOrXor2(llvm::BinaryOperator *Z, llvm::Value *X1_val,
                           llvm::Value *X2_val, llvm::Value *A, llvm::Value *B,
                           bool needsY1Commutativity,
                           bool needsY2Commutativity);
void generateHintForOrXor4(llvm::BinaryOperator *Z, llvm::Value *X,
                           llvm::BinaryOperator *Y, llvm::BinaryOperator *A,
                           llvm::Value *B, llvm::BinaryOperator *NB,
                           bool needsYCommutativity, bool needsZCommutativity);
void generateHintForAddXorAnd(llvm::BinaryOperator *Z, llvm::BinaryOperator *X,
                              llvm::BinaryOperator *Y, llvm::Value *A,
                              llvm::Value *B, bool needsYCommutativity,
                              bool needsZCommutativity);
void generateHintForAddOrAnd(llvm::BinaryOperator *Z, llvm::BinaryOperator *X,
                             llvm::BinaryOperator *Y, llvm::Value *A,
                             llvm::Value *B, bool needsYCommutativity,
                             bool needsZCommutativity);
void generateHintForAndOr(llvm::BinaryOperator *Z, llvm::Value *X,
                          llvm::BinaryOperator *Y, llvm::Value *A,
                          bool needsZCommutativity);
void generateHintForIcmpEqNeBopBop(llvm::ICmpInst *Z, llvm::BinaryOperator *W,
                                   llvm::BinaryOperator *Y);
void generateHintForTrivialDCE(llvm::Instruction &I);
void generateHintForGVNDCE(llvm::Instruction &I);

// inserting nop
void insertTgtNopAtSrcI(CoreHint &hints, llvm::Instruction *I);
void insertSrcNopAtTgtI(CoreHint &hints, llvm::Instruction *I);

extern std::pair<std::shared_ptr<TyExpr>, std::shared_ptr<TyExpr>>
    false_encoding;

/* Generating symbolic expressions from instructions
 * for GVN
 */

struct Expression {
  uint32_t opcode;
  llvm::Type *type;
  llvm::SmallVector<uint32_t, 4> varargs;

  Expression(uint32_t o = ~2U) : opcode(o) {}

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
};

Expression create_expression(llvm::Instruction *I, bool &swapped,
                             llvm::SmallVector<uint32_t, 4> va);

bool is_inverse_expression(Expression e1, Expression e2);

void makeReachableBlockMap(llvm::BasicBlock* Src,
                           llvm::BasicBlock* Tgt);

void generateHintForMem2RegPropagatePerBlock(std::shared_ptr<TyPropagateObject> lessdef_src,
                                             std::shared_ptr<TyPropagateObject> lessdef_tgt,
                                             llvm::Instruction* from,
                                             llvm::Instruction* to,
                                             std::vector<std::pair<llvm::BasicBlock*, llvm::BasicBlock*>> worklist,
                                             llvm::BasicBlock* BB);

void generateHintForMem2RegPropagateStore(llvm::BasicBlock* Pred,
                                          llvm::StoreInst* SI,
                                          llvm::Instruction* next,
                                          int nextIndex);

void generateHintForMem2RegPropagateLoad(llvm::Instruction* I,
                                         llvm::PHINode* tmp,
                                         llvm::LoadInst* LI,
                                         llvm::BasicBlock* useBB,
                                         int useIndex,
                                         llvm::Instruction *use);

void generateHintForMem2RegReplaceHint(llvm::Value* ReplVal,
                                       llvm::Instruction* I);

void generateHintForMem2RegPHI(llvm::BasicBlock* BB, llvm::BasicBlock* Pred,
                               llvm::AllocaInst* AI, llvm::StoreInst* SI,
                               llvm::BasicBlock::iterator II,
                               llvm::DenseMap<llvm::PHINode*, unsigned> PAM,
                               llvm::DenseMap<llvm::AllocaInst*, unsigned> AL,
                               std::vector<std::pair<llvm::BasicBlock*, llvm::BasicBlock*> >succs,
                               bool isSameBB);

void generateHintForMem2RegPHIdelete(llvm::BasicBlock* BB, 
                                     std::vector<std::pair<llvm::BasicBlock*, llvm::BasicBlock*>> VisitedBlock, 
                                     llvm::AllocaInst* AI, bool ignore);

llvm::Instruction* properPHI(llvm::BasicBlock* BB, std::string Target,
                             llvm::Instruction* I, bool isInit,
                             bool checkSI, Dictionary data);

int getIndexofMem2Reg(llvm::Instruction* instr, int instrIndex, int termIndex);

bool hasBitcastOrGEP(llvm::AllocaInst* AI);
}

#endif
