#ifndef HINTGEN_H
#define HINTGEN_H

#include "llvm/LLVMBerry/Structure.h"
#include <string>
#include <vector>
#include <memory>
#include "llvm/ADT/DenseMap.h"
#include "llvm/IR/CFG.h"
#include "llvm/Analysis/CFG.h"

#define INFRULE(pos, x)                                                        \
  hints.addCommand(llvmberry::ConsInfrule::make(pos, x),                       \
                   llvmberry::TyCppDebugInfo::make(__FILE__, __LINE__))
#define PROPAGATE(what, where)                                                 \
  hints.addCommand(llvmberry::ConsPropagate::make(what, where),                \
                   llvmberry::TyCppDebugInfo::make(__FILE__, __LINE__))

#define PHIPOS(SCOPE, PN, prevI)                                               \
  llvmberry::TyPosition::make(SCOPE, PN.getParent()->getName(),                \
                              prevI->getParent()->getName())
#define PHIPOSJustPhi(SCOPE, PN)                                               \
  llvmberry::TyPosition::make(SCOPE, PN.getParent()->getName(), "")
#define INSTPOS(SCOPE, I) llvmberry::TyPosition::make(SCOPE, *(I))
#define POINTER(v) llvmberry::TyPointer::make(*(v))
#define POINTER_ELEMTY(v) llvmberry::TyPointer::makeWithElementType(*(v))
// Below code snippet is tu support overloading of two macro functions : 
// REGISTER(str) := llvmberry::TyRegister::make(str, llvmberry::Physical)
// REGISTER(str, tag) := llvmberry::TyRegister::make(str, llvmberry::tag)
#define _REGISTER_PHYS(n) llvmberry::TyRegister::make(n, llvmberry::Physical)
#define _REGISTER(n, tag) llvmberry::TyRegister::make(n, llvmberry::tag)
#define _REGISTER_X(x, ARG1, ARG2, FUNC, ...) FUNC
#define REGISTER(...) _REGISTER_X(,##__VA_ARGS__,                              \
                                  _REGISTER(__VA_ARGS__),                      \
                                  _REGISTER_PHYS(__VA_ARGS__))

#define BITSIZE(bitwidth) llvmberry::ConsSize::make(bitwidth)
#define BOUNDS(from, to) llvmberry::ConsBounds::make(from, to)

// EXPR, VAR, RHS, INSN macros make TyExpr object
// Below code snippet is tu support overloading of two macro functions : 
// VAR(str) := llvmberry::ConsVar::make(str, llvmberry::Physical)
// VAR(str, tag) := llvmberry::ConsVar::make(str, llvmberry::tag)
#define _VAR_PHYS(n) llvmberry::ConsVar::make(n, llvmberry::Physical)
#define _VAR(n, tag) llvmberry::ConsVar::make(n, llvmberry::tag)
#define _VAR_X(x, ARG1, ARG2, FUNC, ...) FUNC
#define VAR(...) _VAR_X(,##__VA_ARGS__,_VAR(__VA_ARGS__),_VAR_PHYS(__VA_ARGS__))

#define RHS(name, tag, SCOPE)                                                  \
  llvmberry::ConsRhs::make(name, llvmberry::tag, SCOPE)
#define INSN(x) llvmberry::ConsInsn::make((x))
#define INSNWITHGHOST(x, y)                                                    \
  std::shared_ptr<llvmberry::TyExpr>(                                          \
      new llvmberry::ConsInsn(llvmberry::instructionWithGhostIdxs(x, y)))
#define EXPR(I, tag) llvmberry::TyExpr::make(*(I), llvmberry::tag)

// LESSDEF, NOALIAS, DIFFBLOCK, UNIQUE, PRIVATE, MAYDIFF make 
// TyPropagateObject instance
#define LESSDEF(left, right, SCOPE)                                            \
  llvmberry::ConsLessdef::make(left, right, SCOPE)
#define NOALIAS(ptr1, ptr2, SCOPE)                                             \
  llvmberry::ConsNoalias::make(ptr1, ptr2, SCOPE)
#define DIFFBLOCK(v1, v2, SCOPE)                                               \
  llvmberry::ConsDiffblock::make(v1, v2, SCOPE)
#define UNIQUE(reg, SCOPE) llvmberry::ConsUnique::make(reg, SCOPE)
#define PRIVATE(reg, SCOPE) llvmberry::ConsPrivate::make(reg, SCOPE)
#define MAYDIFF(name, tag) llvmberry::ConsMaydiff::make(name, llvmberry::tag)

// VAL, ID macros make TyValue object
#define ID(name, tag) llvmberry::ConsId::make(name, llvmberry::tag)
// Below snippet is to support overloading of two macro functions : 
// VAL(v) := llvmberry::TyValue::make(*(v), llvmberry::Physical)
// VAL(v, tag) := llvmberry::TyValue::make(*(v), llvmberry::tag)
#define _VAL_SRC(I) llvmberry::TyValue::make(*(I), llvmberry::Physical)
#define _VAL_TAG(I, tag) llvmberry::TyValue::make(*(I), llvmberry::tag)
#define _VAL_X(x,ARG1, ARG2, FUNC, ...) FUNC
#define VAL(...) _VAL_X(,##__VA_ARGS__,                                        \
                                _VAL_TAG(__VA_ARGS__),                         \
                                _VAL_SRC(__VA_ARGS__))

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

// CONSTINT make TyConstInt object
#define CONSTINT(val, bitwidth) llvmberry::TyConstInt::make(val, bitwidth)
#define CONSTGLOBALADDR(val) llvmberry::ConsConstGlobalVarAddr::make(*(val))

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

void makeReachableBlockMap(llvm::BasicBlock* Src,
                           llvm::BasicBlock* Tgt);

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
                               bool isSameBB);

void generateHintForMem2RegPHIdelete(llvm::BasicBlock* BB, 
                                     std::vector<std::pair<llvm::BasicBlock*,
                                                           llvm::BasicBlock*>> VisitedBlock, 
                                     llvm::AllocaInst* AI, bool ignore);

llvm::Instruction* properPHI(llvm::BasicBlock* BB, std::string Target,
                             llvm::Instruction* I, bool isInit,
                             bool checkSI, Dictionary data,
                             bool isLoop = false);

int getIndexofMem2Reg(llvm::Instruction* instr, int instrIndex, int termIndex);

bool hasBitcastOrGEP(llvm::AllocaInst* AI);

int getIndexofMem2Reg(llvm::Instruction *instr, int instrIndex, int termIndex);

void generateHintForPHIResolved(llvm::Instruction *I, llvm::BasicBlock *PB,
                                TyScope scope);

void generateHintForMem2RegPhiUndef(llvm::PHINode *APN, llvm::BasicBlock *Pred);

void calculateIndices(llvm::Function* F);

void saveInstrInfo(llvm::Instruction* I, unsigned key);
}

#endif
