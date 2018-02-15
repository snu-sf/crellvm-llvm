#ifndef HINTGEN_H
#define HINTGEN_H

#include "llvm/Crellvm/Structure.h"
#include "llvm/Crellvm/Dictionary.h"
#include <string>
#include <vector>
#include <memory>
#include "llvm/ADT/DenseMap.h"
#include "llvm/IR/CFG.h"
#include "llvm/Analysis/CFG.h"

// Meta-level function for macro overloading
// This is not intended to be used outside Hintgen.h.
// See VAR(), EXPR() for its usage.

#define _CHOOSE(x, ARG1, ARG2, FUNC, ...) FUNC

// Macros for shortening sentences
// Usage : 
//   INTRUDE(CAPTURE(X, Y), {
//     // Lambda function you want to execute
//     std::cout << X << Y;
//   });
// or
#define INTRUDE(A, ...) \
                      crellvm::ValidationUnit::GetInstance()->intrude        \
                      ([ A ] (crellvm::Dictionary &data,                     \
                                      crellvm::CoreHint &hints)              \
                                      { __VA_ARGS__ })
#define INTRUDE_IF(cond, A, ...) if (cond) { \
                      crellvm::ValidationUnit::GetInstance()->intrude        \
                      ([ A ] (crellvm::Dictionary &data,                     \
                                      crellvm::CoreHint &hints)              \
                                      { __VA_ARGS__ });                        \
                      }
#define CAPTURE(...) __VA_ARGS__

#define INFRULE(pos, x)                                                        \
  hints.addCommand(crellvm::ConsInfrule::make(pos, x),                       \
                   crellvm::TyCppDebugInfo::make(__FILE__, __LINE__))
#define PROPAGATE(what, where)                                                 \
  hints.addCommand(crellvm::ConsPropagate::make(what, where),                \
                   crellvm::TyCppDebugInfo::make(__FILE__, __LINE__))

#define PHIPOS(SCOPE, PHIBlock, prevBlock)                                     \
  crellvm::TyPosition::make(SCOPE, PHIBlock, prevBlock)                           
#define PHIPOSJustPhi(SCOPE, PN)                                               \
  crellvm::TyPosition::make(SCOPE, PN.getParent()->getName(), "")
#define INSTPOS(SCOPE, I) crellvm::TyPosition::make(SCOPE, *(I))
#define INDEXEDPOS(SCOPE, I, index, prev)                                      \
  crellvm::TyPosition::make(SCOPE, *(I), index, prev)
#define STARTPOS(SCOPE, BBname)                                                \
  crellvm::TyPosition::make_start_of_block(SCOPE, BBname)
#define ENDPOSINDEXED(SCOPE, BB, index)                                        \
  crellvm::TyPosition::make_end_of_block(SCOPE, *(BB), index)
#define ENDPOS(SCOPE, BB)                                                      \
  crellvm::TyPosition::make_end_of_block(SCOPE, *(BB))
                                                                    
#define POINTER(v) crellvm::TyPointer::make(*(v))
#define POINTER_ELEMTY(v) crellvm::TyPointer::makeWithElementType(*(v))

// REGISTER() supports overloading of two macro functions : 
// REGISTER(str) := crellvm::TyRegister::make(str, crellvm::Physical)
// REGISTER(str, tag) := crellvm::TyRegister::make(str, crellvm::tag)
// How it works : 
//     REGISTER(A, B) is evaluated into _CHOOSE(, A, B, _REGISTER(A, B),
//     _REGISTER_PHYS(A, B), NULL), which becomes _REGISTER(A, B).
//     Similariy, REGISTER(A) is evaluated into _CHOOSE(, A, _REGISTER(A),
//     _REGISTER_PHYS(A), NULL), which becomes _REGISTER_PHYS(A).
#define _REGISTER_PHYS(n) crellvm::TyRegister::make(n, crellvm::Physical)
#define _REGISTER(n, tag) crellvm::TyRegister::make(n, crellvm::tag)
#define REGISTER(...) _CHOOSE(,##__VA_ARGS__,                                  \
                                  _REGISTER(__VA_ARGS__),                      \
                                  _REGISTER_PHYS(__VA_ARGS__),                 \
                                  NULL)

#define BITSIZE(bitwidth) crellvm::ConsSize::make(bitwidth)
#define BOUNDS(from, to) crellvm::ConsBounds::make(from, to)
#define BOUNDSET(from, to_set) crellvm::ConsBoundSet::make(from, to_set)

// VAR supports overloading of two macro functions : 
// VAR(str) := crellvm::ConsVar::make(str, crellvm::Physical)
// VAR(str, tag) := crellvm::ConsVar::make(str, crellvm::tag)
// How it works : 
//     VAR(A, B) is evaluated into _CHOOSE(, A, B, _VAR(A, B),
//     _VAR_PHYS(A, B), NULL), which becomes _VAR(A, B).
//     Similariy, VAR(A) is evaluated into _CHOOSE(, A, _VAR(A),
//     _VAR_PHYS(A), NULL), which becomes _VAR_PHYS(A).
// NOTE : VAR macro makes TyExpr object
#define _VAR_PHYS(n) crellvm::ConsVar::make(n, crellvm::Physical)
#define _VAR(n, tag) crellvm::ConsVar::make(n, crellvm::tag)
#define VAR(...) _CHOOSE(,##__VA_ARGS__,_VAR(__VA_ARGS__),                     \
                        _VAR_PHYS(__VA_ARGS__), NULL)

// NOTE : RHS macro makes TyExpr object
#define RHS(name, tag, SCOPE)                                                  \
  crellvm::ConsRhs::make(name, crellvm::tag, SCOPE)
// NOTE : INSN macro makes TyExpr object
#define INSN(x) crellvm::ConsInsn::make((x))
#define INSNWITHGHOST(x, y)                                                    \
  std::shared_ptr<crellvm::TyExpr>(new crellvm::TyExpr(                    \
      std::shared_ptr<crellvm::TyExprImpl>(new crellvm::ConsInsn(            \
          crellvm::instructionWithGhostIdxs(x, y)))))
#define INSNALIGNONE(I)                                                        \
  crellvm::ConsInsn::make(std::shared_ptr<crellvm::TyInstruction>          \
    (new crellvm::ConsLoadInst(crellvm::TyLoadInst::makeAlignOne(I))))

#define _EXPR_PHYS(I) crellvm::TyExpr::make(*(I), crellvm::Physical)
#define _EXPR(I, tag) crellvm::TyExpr::make(*(I), crellvm::tag)
#define EXPR(...) _CHOOSE(,##__VA_ARGS__,_EXPR(__VA_ARGS__), \
                          _EXPR_PHYS(__VA_ARGS__), NULL)

// LESSDEF, NOALIAS, DIFFBLOCK, UNIQUE, PRIVATE, MAYDIFF make 
// TyPropagateObject instance
#define LESSDEF(left, right, SCOPE)                                            \
  crellvm::ConsLessdef::make(left, right, SCOPE)
#define NOALIAS(ptr1, ptr2, SCOPE)                                             \
  crellvm::ConsNoalias::make(ptr1, ptr2, SCOPE)
#define DIFFBLOCK(v1, v2, SCOPE)                                               \
  crellvm::ConsDiffblock::make(v1, v2, SCOPE)
#define UNIQUE(reg, SCOPE) crellvm::ConsUnique::make(reg, SCOPE)
#define PRIVATE(reg, SCOPE) crellvm::ConsPrivate::make(reg, SCOPE)
#define MAYDIFF(name, tag) crellvm::ConsMaydiff::make(name, crellvm::tag)

// VAL, ID macros make TyValue object
#define ID(name, tag) crellvm::ConsId::make(name, crellvm::tag)
// Below snippet is to support overloading of two macro functions : 
// VAL(v) := crellvm::TyValue::make(*(v), crellvm::Physical)
// VAL(v, tag) := crellvm::TyValue::make(*(v), crellvm::tag)
#define _VAL_SRC(I) crellvm::TyValue::make(*(I), crellvm::Physical)
#define _VAL_TAG(I, tag) crellvm::TyValue::make(*(I), crellvm::tag)
#define VAL(...) _CHOOSE(NULL, ##__VA_ARGS__,                                  \
                                _VAL_TAG(__VA_ARGS__),                         \
                                _VAL_SRC(__VA_ARGS__),                         \
                                NULL)

// VALTYPE, TYPEOF macros make TyValueType object
#define VALTYPE(ty) crellvm::TyValueType::make(*(ty))
#define TYPEOF(I) crellvm::TyValueType::make(*((I)->getType()))
#define TYPEOFAI(I) crellvm::TyValueType::make(*((AI)->getAllocatedType()))

// BINOP, FBINOP, BINARYINSN make TyInstruction object
#define BINOP(bop, type, val1, val2)                                           \
  crellvm::ConsBinaryOp::make(bop, type, val1, val2)
#define FBINOP(fbop, type, val1, val2)                                         \
  crellvm::ConsFloatBinaryOp::make(fbop, type, val1, val2)
#define BINARYINSN(binop, type, val1, val2)                                    \
  crellvm::isFloatOpcode((binop).getOpcode())                                \
      ? FBINOP(crellvm::getFbop((binop).getOpcode()), type, val1, val2)      \
      : BINOP(crellvm::getBop((binop).getOpcode()), type, val1, val2)
#define ICMPINSN(pred, type, val1, val2)                                       \
  crellvm::ConsICmpInst::make(pred, type, val1, val2)

// CONSTINT make TyConstInt object
#define _CONSTINT_BITSIZE(val, bitwidth)                                       \
  crellvm::TyConstInt::make(val, bitwidth)
#define _CONSTINT_LLVMOBJ(obj) crellvm::TyConstInt::make(*(obj))
#define CONSTINT(...) _CHOOSE(,##__VA_ARGS__,                                  \
                                _CONSTINT_BITSIZE(__VA_ARGS__),                \
                                _CONSTINT_LLVMOBJ(__VA_ARGS__),                \
                                NULL)
#define CONSTGLOBALADDR(val) crellvm::ConsConstGlobalVarAddr::make(*(val))
#define CONSTANT(val) crellvm::TyConstant::make(*(val))

#define SRC crellvm::Source
#define TGT crellvm::Target

#define DICTMAP(dict, key) dict.get()->find(key)->second
#define UNDEF(I) llvm::UndefValue::get(I->getType())
#define UNDEFAI(I) llvm::UndefValue::get(AI->getAllocatedType())

#define CRELLVMRPD crellvm::Mem2RegArg::RenamePassTuple

namespace crellvm {
/* applyCommutativity(I, (A bop B), scope) :
 *   Applies commutativity rule ((A bop B) \in P => P += (B bop A)) to the
 * position I
 */
void applyCommutativity(crellvm::CoreHint &hints,llvm::Instruction *position,
                        llvm::BinaryOperator *expression, TyScope scope);
/* applyTransitivity(I, v1, v2, v3, scope) :
 *   Applies transitivity rule (v1 >= v2 >= v3) to the position (I, scope)
 */
void applyTransitivity(crellvm::CoreHint &hints, llvm::Instruction *position,
                       llvm::Value *v_greatest, llvm::Value *v_mid,
                       llvm::Value *v_smallest, TyScope scope);
/* applyTransitivity(I, v1, v2, v3, scope, scopetag) :
 *   Applies transitivity rule (v1 >= v2 >= v3) to the position (I, scopetag)
 */
void applyTransitivity(crellvm::CoreHint &hints, llvm::Instruction *position,
                       llvm::Value *v_greatest, llvm::Value *v_mid, llvm::Value *v_smallest,
                       TyScope scope, TyScope position_scopetag);
/* propagateInstruction(I1, I2, scope, propagateEquivalence) :
 *   if propagateEquivalence == false :
 *     Propagates I1 >= rhs(I1) from I1 to I2 if scope == Source, or
 *     Propagates rhs(I1) >= I1 from I1 to I2 if scope == Target
 *   else :
 *     Propagate I1 >= rhs(I1) and rhs(I1) >= I1 from I1 to I2 in scope.
 */
void propagateInstruction(crellvm::CoreHint &hints, const llvm::Instruction *from,
                          const llvm::Instruction *to,
                          TyScope scope, bool propagateEquivalence = false);
/* propagateLessdef(I1, I2, v1, v2, scope) :
 *   Propagates v1 >= v2 from I1 to I2 in scope
 */
void propagateLessdef(crellvm::CoreHint &hints, const llvm::Instruction *from,
                      const llvm::Instruction *to,
                      const llvm::Value *lesserval,
                      const llvm::Value *greaterval, TyScope scope);
/* propagateMaydiffGlobal(var, tag) :
 *   Propagates variable (var, tag) globally
 */
void propagateMaydiffGlobal(crellvm::CoreHint &hints, std::string varname, TyTag tag);
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
    std::string ghost_var = "",
    std::shared_ptr<TyPosition> invariant_pos = std::shared_ptr<TyPosition>(nullptr),
    std::function<bool(const llvm::Value *)> filter = {});
/* generateHintForReplaceAllUsesWithAtTgt(v1, v2) :
 *   For each use U of v1, propagates hints at target to prove that
 * replacing v1 with v2 in U is safe.
 *   Invariant { v1 >=tgt v2 } must be given at definition of v1 (or invariant_pos)
 */
void generateHintForReplaceAllUsesWithAtTgt(llvm::Instruction *source,
                                            llvm::Value *replaceTo,
                                            std::shared_ptr<TyPosition> invariant_pos = 
                                              std::shared_ptr<TyPosition>(nullptr));

/* Hint generation functions for instcombine/instsimplify micro-optimizations
 * that appear multiple times
 */
void generateHintForAddSelectZero(llvm::BinaryOperator *Z,
                                  llvm::BinaryOperator *X, llvm::SelectInst *Y,
                                  bool is_leftform);
void generateHintForOrAnd(llvm::BinaryOperator *Y, llvm::Value *X, llvm::Value *A);
void generateHintForOrXor(llvm::BinaryOperator *W, llvm::Value *op0, llvm::Value *op1);
void generateHintForOrXor2(llvm::BinaryOperator *Z, llvm::Value *X1_val,
                           llvm::Value *X2_val, llvm::Value *A, llvm::Value *B);
void generateHintForOrXor4(llvm::BinaryOperator *Z, llvm::Value *X,
                           llvm::BinaryOperator *Y, llvm::BinaryOperator *A,
                           llvm::Value *B, llvm::BinaryOperator *NB);
void generateHintForAddXorAnd(llvm::BinaryOperator *Z, llvm::BinaryOperator *X,
                              llvm::BinaryOperator *Y, llvm::Value *A,
                              llvm::Value *B);
void generateHintForAddOrAnd(llvm::BinaryOperator *Z, llvm::BinaryOperator *X,
                             llvm::BinaryOperator *Y, llvm::Value *A,
                             llvm::Value *B);
void generateHintForAndOr(llvm::BinaryOperator *Z, llvm::Value *X,
                          llvm::BinaryOperator *Y, llvm::Value *A);
void generateHintForIcmpEqNeBopBop(llvm::ICmpInst *Z, llvm::BinaryOperator *W,
                                   llvm::BinaryOperator *Y);
void generateHintForTrivialDCE(llvm::Instruction &I);
void generateHintForGVNDCE(llvm::Instruction &I);

template<DictKeys DICTKEY>
void generateHintForInstructionSimplify(llvm::BinaryOperator &I, llvm::Value *V) {
  INTRUDE(CAPTURE(&I, &V), {
    auto ptr = data.get<DICTKEY>();
    if(ptr->isActivated()){
      crellvm::ValidationUnit::GetInstance()->setOptimizationName(ptr->getMicroOptName());
      ptr->generateHint(&I);
      crellvm::generateHintForReplaceAllUsesWith(&I, V);
    }else
      crellvm::ValidationUnit::GetInstance()->setIsAborted();
  });
}

// oldPN->getOperand(0) == FirstInst.
void generateHintForFoldPhiBin(llvm::PHINode &oldPN,
    llvm::PHINode *NewLHS, llvm::PHINode *NewRHS, llvm::DominatorTree *DT);

// inserting nop
void insertTgtNopAtSrcI(CoreHint &hints, llvm::Instruction *I);
void insertSrcNopAtTgtI(CoreHint &hints, llvm::Instruction *I);

extern std::pair<std::shared_ptr<TyExpr>, std::shared_ptr<TyExpr>>
    false_encoding;

int getIndexofMem2Reg(llvm::Instruction* instr, int instrIndex, int termIndex);

bool hasBitcastOrGEP(llvm::AllocaInst* AI);

void generateHintForPHIResolved(llvm::Instruction *I, llvm::BasicBlock *PB,
                                TyScope scope);

std::shared_ptr<std::vector<std::shared_ptr<TyPosition>>> saveUseSet (llvm::Instruction* I, Dictionary &data, CoreHint &hints);

void saveInstrIndices(llvm::Function* F, Dictionary &data);

void saveUseIndices(llvm::Function* F, unsigned opCode, Dictionary &data);

void eraseInstrOfUseIndices(llvm::Instruction* key, llvm::Instruction* I, Dictionary &data);

void propagateFromInsnToLoad (unsigned key, llvm::LoadInst *LI, Dictionary &data, CoreHint &hints);

void propagateFromInsnToPhi (unsigned key, llvm::PHINode *Phi, llvm::BasicBlock* prev, Dictionary &data, CoreHint &hints);

void checkSIOperand (unsigned key, llvm::StoreInst *SI, Dictionary &data, CoreHint &hints);

void propagateLoadInstToUse(llvm::LoadInst *LI, llvm::Value *V, std::string In, Dictionary &data, CoreHint &hints, bool checkReplace=false);

void propagateLoadGhostValueFromSIToLI(llvm::StoreInst* SI, llvm::LoadInst* LI, llvm::Value* value, Dictionary &data, CoreHint &hints, bool checkReplace=false);

void propagateLoadGhostValueFromAIToLI(llvm::AllocaInst* AI, llvm::LoadInst* LI, llvm::Value* value, Dictionary &data, CoreHint &hints);

void replaceExpr(llvm::Instruction *Tgt, llvm::Value *New, Dictionary &data);

void replaceTag(llvm::Instruction *Tgt, TyTag tag, Dictionary &data);

void unreachableBlockPropagateFalse(llvm::BasicBlock* bb, CoreHint &hints);

bool equalsIfConsVar(std::shared_ptr<TyExpr> e1, std::shared_ptr<TyExpr> e2); 

void propagateInvariantOnDominatedBlocks(std::shared_ptr<TyPropagateObject> invariant,
    llvm::BasicBlock *dominatingBlock, llvm::DominatorTree *DT, CoreHint &hints);

}

#endif
