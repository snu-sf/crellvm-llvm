#include "llvm/Crellvm/ValidationUnit.h"
#include "llvm/Crellvm/Dictionary.h"
#include "llvm/Crellvm/Hintgen.h"
#include "llvm/Crellvm/Infrules.h"
#include "llvm/Crellvm/InstCombine/InfrulesAddSub.h"
#include "llvm/Crellvm/InstCombine/InfrulesAndOrXor.h"
#include "llvm/Crellvm/InstCombine/InfrulesCompares.h"
#include "llvm/Crellvm/InstCombine/InfrulesSelect.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

namespace crellvm {
// insert nop at tgt where I is at src
void insertTgtNopAtSrcI(CoreHint &hints, llvm::Instruction *I) {
  std::string empty_str = "";
  if (I == I->getParent()->getFirstNonPHI()) {
    std::string nop_block_name = getBasicBlockIndex(I->getParent());
    hints.addNopPosition(TyPosition::make(TGT, nop_block_name, empty_str));
  } else if (!llvm::isa<llvm::PHINode>(I)) {
    llvm::BasicBlock::iterator prevI = I;
    prevI--;
    hints.addNopPosition(TyPosition::make(TGT, *prevI));
  }
}

// insert nop at src where I is at tgt
void insertSrcNopAtTgtI(CoreHint &hints, llvm::Instruction *I) {
  std::string empty_str = "";
  if (I == I->getParent()->getFirstNonPHI()) {
    std::string nop_block_name = getBasicBlockIndex(I->getParent());
    hints.addNopPosition(TyPosition::make(SRC, nop_block_name, empty_str));
  } else if (!llvm::isa<llvm::PHINode>(I)) {
    llvm::BasicBlock::iterator prevI = I;
    prevI--;
    hints.addNopPosition(TyPosition::make(SRC, *prevI));
  }
}
/* propagateInstruction(I1, I2, scope, propagateEquivalence) :
 *   if propagateEquivalence == false :
 *     Propagates I1 >= rhs(I1) from I1 to I2 if scope == Source, or
 *     Propagates rhs(I1) >= I1 from I1 to I2 if scope == Target
 *   else :
 *     Propagate I1 >= rhs(I1) and rhs(I1) >= I1 from I1 to I2 in scope.
 */
void propagateInstruction(crellvm::CoreHint &hints, const llvm::Instruction *from,
                          const llvm::Instruction *to,
                          TyScope scope, bool propagateEquivalence) {
  assert(ValidationUnit::Exists());
  std::string name = getVariable(*from);
  auto pos1 = INSTPOS(scope, from), pos2 = INSTPOS(scope, to);

  if (scope == Source) {
    PROPAGATE(LESSDEF(VAR(name), RHS(name, Physical, scope), scope), BOUNDS(pos1, pos2));
    if (propagateEquivalence)
      PROPAGATE(LESSDEF(RHS(name, Physical, scope), VAR(name), scope), BOUNDS(pos1, pos2));
  } else if (scope == Target) {
    PROPAGATE(LESSDEF(RHS(name, Physical, scope), VAR(name), scope), BOUNDS(pos1, pos2));
    if (propagateEquivalence)
      PROPAGATE(LESSDEF(VAR(name), RHS(name, Physical, scope), scope), BOUNDS(pos1, pos2));
  }// else {
  //  assert("propagateInstruction() : scope is neither Source nor Target" &&
  //         false);
  //}
}

void propagateLessdef(crellvm::CoreHint &hints,
                      const llvm::Instruction *from, const llvm::Instruction *to,
                      const llvm::Value *lesserval,
                      const llvm::Value *greaterval, TyScope scope) {
  assert(ValidationUnit::Exists());
  PROPAGATE(LESSDEF(EXPR(greaterval), EXPR(lesserval), scope),
      BOUNDS(INSTPOS(scope, from), INSTPOS(scope, to)));
}

void applyCommutativity(crellvm::CoreHint &hints, llvm::Instruction *position,
                        llvm::BinaryOperator *expression, TyScope scope) {
  assert(ValidationUnit::Exists());

  int bitwidth = isFloatOpcode(expression->getOpcode()) ? -1
                     : expression->getType()->getIntegerBitWidth();
  std::string regname = getVariable(*expression);
  auto val1 = VAL(expression->getOperand(0));
  auto val2 = VAL(expression->getOperand(1));
  
  if (scope == Source) {
    bool is_fbop = false;
    TyBop bop;
    TyFbop fbop;
    switch (expression->getOpcode()) {
    case llvm::Instruction::Add:
      bop = BopAdd;
      break;
    case llvm::Instruction::And:
      bop = BopAnd;
      break;
    case llvm::Instruction::Mul:
      bop = BopMul;
      break;
    case llvm::Instruction::Or:
      bop = BopOr;
      break;
    case llvm::Instruction::Xor:
      bop = BopXor;
      break;
    case llvm::Instruction::FAdd:
      fbop = BopFadd;
      is_fbop = true;
      break;
    case llvm::Instruction::FMul:
      fbop = BopFmul;
      is_fbop = true;
      break;
    default:
      // Unreachable: see lib/IR/Instruction.cpp, line 498
      assert("applyCommutativity() : Unreachable - we covered cases exhaustively.");
    }

    if (is_fbop) {
      INFRULE(INSTPOS(Source, position), ConsFbopCommutative::make(
              VAR(regname), fbop, val1, val2, getFloatType(expression->getType())));
    } else {
      INFRULE(INSTPOS(Source, position), ConsBopCommutative::make(
              VAR(regname), bop, val1, val2, BITSIZE(bitwidth)));
    }
  } else if (scope == Target) {
    switch (expression->getOpcode()) {
    case llvm::Instruction::Add:
      INFRULE(INSTPOS(Target, position), ConsAddCommutativeTgt::make(
              REGISTER(regname), val1, val2, BITSIZE(bitwidth)));
      break;
    case llvm::Instruction::Xor:
      INFRULE(INSTPOS(Target, position), ConsXorCommutativeTgt::make(
              REGISTER(regname), val1, val2, BITSIZE(bitwidth)));
      break;
    case llvm::Instruction::FAdd:
      INFRULE(INSTPOS(Target, position), ConsFaddCommutativeTgt::make(
              REGISTER(regname), val1, val2, getFloatType(expression->getType())));
      break;
    case llvm::Instruction::FMul:
      INFRULE(INSTPOS(Target, position), ConsFmulCommutativeTgt::make(
              REGISTER(regname), val1, val2, getFloatType(expression->getType())));
      break;
    case llvm::Instruction::Or:
      INFRULE(INSTPOS(Target, position), ConsOrCommutativeTgt::make(
              REGISTER(regname), val1, val2, BITSIZE(bitwidth)));
      break;
    default:
      assert("applyCommutativity() : we don't support commutativity rule for "
             "this binary operator");
    }
  } else {
    assert("applyCommutativity() : scope is neither Source nor Target" &&
           false);
  }
}

void applyTransitivity(crellvm::CoreHint &hints, llvm::Instruction *position,
                       llvm::Value *v1, llvm::Value *v2, llvm::Value *v3, TyScope scope) {
  // v1 >= v2 >= v3
  applyTransitivity(hints, position, v1, v2, v3, scope, scope);
}

void applyTransitivity(crellvm::CoreHint &hints, llvm::Instruction *position, llvm::Value *v1,
                       llvm::Value *v2, llvm::Value *v3, TyScope scope, TyScope position_scopetag) {
  assert(ValidationUnit::Exists());
  // v1 >= v2 >= v3
  INFRULE(INSTPOS(position_scopetag, position), ConsTransitivity::make(EXPR(v1), EXPR(v2), EXPR(v3)));
}

void propagateMaydiffGlobal(crellvm::CoreHint &hints, std::string varname, TyTag tag) {
  assert(ValidationUnit::Exists());
  PROPAGATE(ConsMaydiff::make(varname, tag), ConsGlobal::make());
}

void generateHintForNegValue(llvm::Value *V, llvm::BinaryOperator &I,
                             TyScope scope) {
  assert(ValidationUnit::Exists());

  if (llvm::BinaryOperator::isNeg(V)) {
    INTRUDE(CAPTURE(&V, &I, &scope), {
      std::string reg0_name = getVariable(I);  // z = x -my
      std::string reg1_name = getVariable(*V); // my

      llvm::Instruction *Vins = llvm::dyn_cast<llvm::Instruction>(V);

      PROPAGATE(LESSDEF(VAR(*Vins), RHS(reg1_name, Physical, scope), scope),
          BOUNDS(INSTPOS(scope, Vins), INSTPOS(scope, &I)));

      PROPAGATE(LESSDEF(RHS(reg1_name, Physical, scope), VAR(*Vins), scope),
                BOUNDS(INSTPOS(scope, Vins), INSTPOS(scope, &I)));
    });
  }
  // Constants can be considered to be negated values if they can be folded.
  if (llvm::ConstantInt *C = llvm::dyn_cast<llvm::ConstantInt>(V)) {
    INTRUDE(CAPTURE(&I, &V, &C, &scope), {
      unsigned sz_bw = I.getType()->getPrimitiveSizeInBits();
      int64_t c1 = C->getSExtValue(), c2 = -c1;

      INFRULE(INSTPOS(scope, &I), ConsNegVal::make(CONSTINT(c1, sz_bw),
              CONSTINT(c2, sz_bw), BITSIZE(I)));

      INFRULE(INSTPOS(scope, &I), ConsNegVal::make(CONSTINT(c1, sz_bw),
              CONSTINT(c2, sz_bw), BITSIZE(I)));
    });
  }
}

void generateHintForReplaceAllUsesWith(llvm::Instruction *source,
          llvm::Value *replaceTo, std::string ghostvar,
          std::shared_ptr<TyPosition> source_pos,
          std::function<bool(const llvm::Value *)> filter) {
  using namespace llvm;
  assert(ValidationUnit::Exists());
  if (!source_pos)
    source_pos = INSTPOS(SRC, source);

  INTRUDE(CAPTURE(&source, &replaceTo, &ghostvar, &source_pos, filter), {
    Instruction *I = source;

    std::string to_rem = getVariable(*I);

    for (auto UI = I->use_begin(); UI != I->use_end(); ++UI) {
      if (filter && !filter(UI->getUser()))
        continue;

      if (!isa<Instruction>(UI->getUser()))
        // let the validation fail when the user is not an instruction
        return;
      
      std::string u = getVariable(*UI->getUser());
      Instruction *u_I = dyn_cast<Instruction>(UI->getUser());

      std::string prev_block_name = "";
      if (isa<PHINode>(u_I)) {
        BasicBlock *bb_from = dyn_cast<PHINode>(u_I)->getIncomingBlock(*UI);
        prev_block_name = getBasicBlockIndex(bb_from);
      }

      if (ghostvar == "") {
        PROPAGATE(LESSDEF(VAR(to_rem), EXPR(replaceTo, Physical), SRC),
              BOUNDS(source_pos, TyPosition::make(SRC, *u_I, prev_block_name)));
        if (isa<PHINode>(u_I))
          INFRULE(TyPosition::make(SRC, *u_I, prev_block_name),
                  ConsTransitivity::make(VAR(u), VAR(to_rem, Previous), EXPR(replaceTo, Previous)));
        else if (!u.empty() && !isa<CallInst>(u_I)) {
          INFRULE(TyPosition::make(Source, *u_I, prev_block_name),
                  ConsSubstitute::make(REGISTER(to_rem), VAL(replaceTo), RHS(u, Physical, SRC)));
          INFRULE(TyPosition::make(Source, *u_I, prev_block_name),
                  ConsTransitivity::make(VAR(u), RHS(u, Physical, SRC), RHS(u, Physical, TGT)));
          //INFRULE(TyPosition::make(Source, *u_I, prev_block_name),
          //        ConsReplaceRhs::make(REGISTER(to_rem), VAL(replaceTo),
          //            VAR(u), RHS(u, Physical, SRC), RHS(u, Physical, TGT)));
        }
      } else {
        PROPAGATE(LESSDEF(VAR(to_rem), VAR(ghostvar, Ghost), SRC),
                  BOUNDS(source_pos, TyPosition::make(SRC, *u_I, prev_block_name)));
        PROPAGATE(LESSDEF(VAR(ghostvar, Ghost), EXPR(replaceTo), TGT),
                  BOUNDS(INSTPOS(TGT, I), TyPosition::make(TGT, *u_I, prev_block_name)));

        if (isa<PHINode>(u_I)) {
          // src : user = phi [ to_rem, prev_block_name ]
          // tgt : user = phi [replaceTo, prev_block_name ]
          // In src : Transitivity ;
          //    user >= to_rem(physical) >= to_rem(previous) >= ghostvar
          // In tgt : TransitivityTgt ;
          //    ghostva >= replaceTo(physical) >= replaceTo(previous) >= user
          INFRULE(TyPosition::make(SRC, *u_I, prev_block_name),
                  ConsTransitivity::make(VAR(u), VAR(to_rem, Previous), VAR(to_rem)));
          INFRULE(TyPosition::make(SRC, *u_I, prev_block_name),
                  ConsTransitivity::make(VAR(u), VAR(to_rem), VAR(ghostvar, Ghost)));
          INFRULE(TyPosition::make(TGT, *u_I, prev_block_name),
                  ConsTransitivityTgt::make(EXPR(replaceTo), EXPR(replaceTo, Previous), VAR(u)));
          INFRULE(TyPosition::make(TGT, *u_I, prev_block_name),
                  ConsTransitivityTgt::make(VAR(ghostvar, Ghost), EXPR(replaceTo), VAR(u)));
        } else if (!u.empty() && !isa<CallInst>(u_I)) {
          if (!TyInstruction::isSupported(*u_I)) {
            std::string str;
            raw_string_ostream rso(str);
            rso << "\ngenerateHintForReplaceAllUsesWith : Unsupported instruction";

            hints.appendToDescription(rso.str());
            if (hints.getReturnCode() == CoreHint::ACTUAL)
              hints.setReturnCodeToAdmitted();
            return;
          }
          INFRULE(TyPosition::make(SRC, *u_I, prev_block_name),
                  ConsSubstitute::make(REGISTER(to_rem), ID(ghostvar, Ghost), INSN(*u_I)));
          Instruction *u_I_copy = u_I->clone();

          for (unsigned i = 0; i < u_I_copy->getNumOperands(); i++) {
            if (u_I->getOperand(i) == source)
              u_I_copy->setOperand(i, replaceTo);
          }
          INFRULE(TyPosition::make(SRC, *u_I, prev_block_name),
                  ConsTransitivity::make(EXPR(u_I), INSN(*u_I), INSN(*u_I_copy)));
          delete u_I_copy;
        }
      }
    }
  });
}

void generateHintForReplaceAllUsesWithAtTgt(llvm::Instruction *source,
                                            llvm::Value *replaceTo,
                                            std::shared_ptr<TyPosition> invariant_pos) {
  assert(ValidationUnit::Exists());

  if (!invariant_pos)
    invariant_pos = INSTPOS(TGT, source);

  INTRUDE(CAPTURE(&source, &replaceTo, &invariant_pos), {
    llvm::Instruction *I = source;
    auto I_pos = invariant_pos;

    std::string I_var = getVariable(*I);

    for (auto UI = I->use_begin(); UI != I->use_end(); ++UI) {
      if (!llvm::isa<llvm::Instruction>(UI->getUser()))
        // let the validation fail when the user is not an instruction
        return;
      std::string user = getVariable(*UI->getUser());
      llvm::Instruction *user_I =
          llvm::dyn_cast<llvm::Instruction>(UI->getUser());

      std::string prev_block_name = "";
      if (llvm::isa<llvm::PHINode>(user_I)) {
        llvm::BasicBlock *bb_from =
            llvm::dyn_cast<llvm::PHINode>(user_I)->getIncomingBlock(*UI);
        prev_block_name = getBasicBlockIndex(bb_from);
      }

      PROPAGATE(LESSDEF(VAR(I_var), EXPR(replaceTo, Physical), TGT),
                BOUNDS(I_pos, TyPosition::make(TGT, *user_I, prev_block_name)));
      if (llvm::isa<llvm::PHINode>(user_I)) {
        INFRULE(TyPosition::make(TGT, *user_I, prev_block_name),
                ConsTransitivityTgt::make(VAR(I_var, Previous),
                                          EXPR(replaceTo, Previous),
                                          VAR(user)));
      } else if (!user.empty() && !llvm::isa<llvm::CallInst>(user_I)) {
        if (!TyInstruction::isSupported(*user_I)) {
          std::string str;
          llvm::raw_string_ostream rso(str);
          rso << "\ngenerateHintForReplaceAllUsesWithAtTgt : Unsupported instruction";

          hints.appendToDescription(rso.str());
          if (hints.getReturnCode() == CoreHint::ACTUAL)
            hints.setReturnCodeToAdmitted();
          return;
        }
        llvm::Instruction *user_I_copy = user_I->clone();
        INFRULE(TyPosition::make(TGT, *user_I, prev_block_name),
                ConsSubstituteTgt::make(REGISTER(I_var),
                                        VAL(replaceTo),
                                        INSN(*user_I)));

        for (unsigned i = 0; i < user_I_copy->getNumOperands(); i++) {
          if (user_I->getOperand(i) == source)
            user_I_copy->setOperand(i, replaceTo);
        }
        INFRULE(TyPosition::make(TGT, *user_I, prev_block_name),
                ConsTransitivityTgt::make(INSN(*user_I_copy), INSN(*user_I),
                                          EXPR(user_I, Physical)));
        delete user_I_copy;
      }
    }
  });
}

void generateHintForAddSelectZero(llvm::BinaryOperator *Z,
                                  llvm::BinaryOperator *X, llvm::SelectInst *Y,
                                  bool is_leftform) {
  assert(ValidationUnit::Exists());

  INTRUDE(CAPTURE(&Z, &X, &Y, is_leftform), {
    // is_leftform == true :
    //   <src>                          |     <tgt>
    // X = n - a                        | Z = n - a
    // Y = select c ? x : 0             | Y = select c ? x : 0
    // Z = Y + a                        | Z = select c ? n : a

    // is_leftform == false :
    //   <src>                          |     <tgt>
    // X = n - a                        | Z = n - a
    // Y = select c ? 0 : x             | Y = select c ? 0 : x
    // Z = Y + a                        | Z = select c ? a : n

    llvm::Value *c = Y->getCondition(), *n = X->getOperand(0);
    llvm::Value *a = X->getOperand(1); //, *a_Z = Z->getOperand(1);

    // prepare variables
    std::string ry = getVariable(*Y);
    std::string rz = getVariable(*Z);
    std::string rx = getVariable(*X);

    // Propagate "X = n - a"
    PROPAGATE(LESSDEF(VAR(*X), RHS(rx, Physical, SRC), SRC), BOUNDS(INSTPOS(SRC, X), INSTPOS(SRC, Z)));

    // Propagate "Y = select c ? x : 0" or "Y = select c ? 0 : x"
    PROPAGATE(LESSDEF(VAR(*Y), RHS(ry, Physical, SRC), SRC), BOUNDS(INSTPOS(SRC, Y), INSTPOS(SRC, Z)));

    // if (needs_commutativity)
    //  INFRULE(INSTPOS(SRC, Z), ConsBopCommutative::make(VAR(*Z),
    //          TyBop::BopAdd, VAL(Y), VAL(a_Z), BITSIZE(*Z)));

    if (is_leftform)
      INFRULE(INSTPOS(SRC, Z), ConsAddSelectZero::make(REGISTER(*Z),
              REGISTER(*X), REGISTER(*Y), VAL(c), VAL(n), VAL(a), BITSIZE(*Z)));
    else
      INFRULE(INSTPOS(SRC, Z), ConsAddSelectZero2::make(REGISTER(*Z),
              REGISTER(*X), REGISTER(*Y), VAL(c), VAL(n), VAL(a), BITSIZE(*Z)));
  });
}

void generateHintForOrAnd(llvm::BinaryOperator *Y, llvm::Value *X,
                          llvm::Value *A) {
  assert(ValidationUnit::Exists());

  INTRUDE(CAPTURE(Y, X, A), {
    auto ptr = data.get<ArgForSimplifyOrInst>();
    //bool isSwapped = ptr->isSwapped;
    ptr->setHintGenFunc("or_and", [Y, X, A, &hints](llvm::Instruction *I) {
      //   <src>   |   <tgt>
      // Y = X & A | Y = X & A
      // Z = Y | X | (Z equals X)
      llvm::BinaryOperator *Z = llvm::dyn_cast<llvm::BinaryOperator>(I);
      //assert(Z && "Z must be a binary operator in or_and optimization");

      //bool Zswapped = Z->getOperand(0) == X;
      propagateInstruction(hints, Y, Z, SRC);
      //if (Zswapped ^ isSwapped)
      //  applyCommutativity(hints, Z, Z, SRC);
      //if (Y->getOperand(0) == A)
      //  applyCommutativity(hints, Z, Y, SRC);
      INFRULE(INSTPOS(SRC, Z), ConsOrAnd::make(VAL(Z), VAL(Y),
              VAL(X), VAL(A), BITSIZE(*Z)));
    });
  });
}

void generateHintForOrXor(llvm::BinaryOperator *W, llvm::Value *op0, llvm::Value *op1) {
  assert(ValidationUnit::Exists());

  INTRUDE(CAPTURE(&W, &op0, &op1), {
    //    <src>    |   <tgt>
    // X = B ^ -1  | X = B ^ -1
    // Y = A & X   | Y = A & X
    // Z = A ^ B   | Z = A ^ B
    // W = Y | Z   | W = A ^ B
    llvm::BinaryOperator *Z = llvm::dyn_cast<llvm::BinaryOperator>(op1);
    llvm::BinaryOperator *Y = llvm::dyn_cast<llvm::BinaryOperator>(op0);
    llvm::BinaryOperator *X = llvm::dyn_cast<llvm::BinaryOperator>(Y->getOperand(1));
    //assert(X);
    //assert(Y);
    //assert(Z);
    //assert(W);
    llvm::Value *A = Z->getOperand(0), *B = Z->getOperand(1);

    propagateInstruction(hints, X, W, SRC);
    propagateInstruction(hints, Y, W, SRC);
    propagateInstruction(hints, Z, W, SRC);
    // auto: If X = -1 ^ B, apply commutativity
    //if (X->getOperand(1) == B)
    //  applyCommutativity(hints, W, X, SRC);

    //if (needsCommutativity)
    //  applyCommutativity(hints, W, W, SRC);

    INFRULE(INSTPOS(SRC, W), ConsOrXor::make(
        VAL(W), VAL(Z), VAL(X), VAL(Y), VAL(A), VAL(B), BITSIZE(*W)));
  });
}

void generateHintForOrXor2(llvm::BinaryOperator *Z, llvm::Value *X1_val,
                           llvm::Value *X2_val, llvm::Value *A, llvm::Value *B) {
  assert(ValidationUnit::Exists());

  INTRUDE(CAPTURE(&Z, &X1_val, &X2_val, &A, &B), {
    //     <src>           <tgt>
    // X1 = B  ^ -1  | X1 =  B ^ -1
    // Y1 = A  & X1  | Y1 =  A & X1
    // X2 = A  ^ -1  | X2 =  A ^ -1
    // Y2 = X2 & B   | Y2 = X2 & B
    // Z =  Y1 | Y2  | Z =   A ^ B
    llvm::BinaryOperator *Y1 = llvm::dyn_cast<llvm::BinaryOperator>(Z->getOperand(0));
    llvm::BinaryOperator *Y2 = llvm::dyn_cast<llvm::BinaryOperator>(Z->getOperand(1));
    llvm::BinaryOperator *X1 = llvm::dyn_cast<llvm::BinaryOperator>(X1_val);
    llvm::BinaryOperator *X2 = llvm::dyn_cast<llvm::BinaryOperator>(X2_val);
    //assert(Y1);
    //assert(Y2);
    //assert(X1);
    //assert(X2);
    //int bitwidth = Z->getType()->getIntegerBitWidth();

    propagateInstruction(hints, X1, Z, SRC);
    propagateInstruction(hints, X2, Z, SRC);
    propagateInstruction(hints, Y1, Z, SRC);
    propagateInstruction(hints, Y2, Z, SRC);
    // auto: If X1 = -1 ^ B, apply commutativity
    //if (X1->getOperand(1) == B)
    //  applyCommutativity(hints, Z, X1, SRC);
    // auto: If X2 = -1 ^ A, apply commutativity
    //if (X2->getOperand(1) == A)
    //  applyCommutativity(hints, Z, X2, SRC);
    //if (needsY1Commutativity)
    //  applyCommutativity(hints, Z, Y1, SRC);
    //if (needsY2Commutativity)
    //  applyCommutativity(hints, Z, Y2, SRC);

    INFRULE(INSTPOS(TGT, Z), ConsOrXor2::make(
        VAL(Z), VAL(X1), VAL(Y1), VAL(X2), VAL(Y2), VAL(A), VAL(B), BITSIZE(*Z)));
  });
}

void generateHintForOrXor4(llvm::BinaryOperator *Z, llvm::Value *X,
                           llvm::BinaryOperator *Y, llvm::BinaryOperator *A,
                           llvm::Value *B, llvm::BinaryOperator *NB) {
  //assert(ValidationUnit::Exists());

  INTRUDE(CAPTURE(&Z, &X, &Y, &A, &B, &NB), {
    // <src>      |  <tgt>
    // A = X ^ -1 | A = X ^ -1
    // Y = A ^ B  | Y = A ^ B
    // <nop>      | NB = B ^ -1
    // Z = X | Y  | Z = NB | X
    propagateInstruction(hints, A, Z, TGT);
    propagateInstruction(hints, Y, Z, TGT);
    propagateInstruction(hints, NB, Z, TGT);
    //if (needsYCommutativity)
    //  applyCommutativity(hints, Z, Y, TGT);
    insertSrcNopAtTgtI(hints, NB);
    propagateMaydiffGlobal(hints, getVariable(*NB), Physical);

    INFRULE(INSTPOS(TGT, Z), ConsOrXor4::make(VAL(Z), VAL(X),
            VAL(Y), VAL(A), VAL(B), VAL(NB), BITSIZE(*Z)));
    //if (needsZCommutativity)
    //  INFRULE(INSTPOS(TGT, Z), ConsOrCommutativeTgt::make(
    //          REGISTER(*Z), VAL(X), VAL(Y), BITSIZE(*Z)));
  });
}

void generateHintForAddXorAnd(llvm::BinaryOperator *Z, llvm::BinaryOperator *X,
                              llvm::BinaryOperator *Y, llvm::Value *A,
                              llvm::Value *B) {
  INTRUDE(CAPTURE(&Z, &X, &Y), {
    //    <src>       <tgt>
    // X = A ^ B  | X = A ^ B
    // Y = A & B  | Y = A & B
    // Z = X + Y  | Z = A | B
    llvm::Value *A = X->getOperand(0), *B = X->getOperand(1);

    propagateInstruction(hints, X, Z, SRC);
    propagateInstruction(hints, Y, Z, SRC);

    // auto: If Y = B & A, apply commutativity
    //if (needsYCommutativity)
    //  applyCommutativity(hints, Z, Y, SRC);
    // auto: If Z = Y + X, apply commutativity
    //if (needsZCommutativity)
    //  applyCommutativity(hints, Z, Z, SRC);

    INFRULE(INSTPOS(SRC, Z), ConsAddXorAnd::make(
        REGISTER(*Z), VAL(A), VAL(B), REGISTER(*X), REGISTER(*Y), BITSIZE(*Z)));
  });
}

void generateHintForAddOrAnd(llvm::BinaryOperator *Z, llvm::BinaryOperator *X,
                             llvm::BinaryOperator *Y, llvm::Value *A,
                             llvm::Value *B) {
  assert(ValidationUnit::Exists());

  INTRUDE(CAPTURE(&Z, &X, &Y), {
    //    <src>       <tgt>
    // X = A ^ B  | X = A ^ B
    // Y = A & B  | Y = A & B
    // Z = X + Y  | Z = A | B
    llvm::Value *A = X->getOperand(0), *B = X->getOperand(1);

    propagateInstruction(hints, X, Z, SRC);
    propagateInstruction(hints, Y, Z, SRC);

    // auto: If Y = B & A, apply commutativity
    //if (needsYCommutativity)
    //  applyCommutativity(hints, Z, Y, SRC);
    // auto: If Z = Y + X, apply commutativity
    //if (needsZCommutativity)
    //  applyCommutativity(hints, Z, Z, SRC);

    INFRULE(INSTPOS(SRC, Z), ConsAddOrAnd::make(REGISTER(*Z), VAL(A), VAL(B), REGISTER(*X), REGISTER(*Y), BITSIZE(*Z)));
  });
}

void generateHintForAndOr(llvm::BinaryOperator *Z, llvm::Value *X,
                          llvm::BinaryOperator *Y, llvm::Value *A) {
  assert(ValidationUnit::Exists());

  INTRUDE(CAPTURE(&Z, &X, &Y, &A), {
    assert(Z);

    propagateInstruction(hints, Y, Z, Source);
    // auto:
    //if (Y->getOperand(0) != X)
    //  applyCommutativity(hints, Z, Y, Source);
    //if (needsZCommutativity)
    //  applyCommutativity(hints, Z, Z, Source);
    INFRULE(INSTPOS(SRC, Z), ConsAndOr::make(VAL(Z), VAL(X), VAL(Y), VAL(A), BITSIZE(*Z)));
  });
}

void generateHintForIcmpEqNeBopBop(llvm::ICmpInst *Z, llvm::BinaryOperator *W,
                                   llvm::BinaryOperator *Y) {
  assert(ValidationUnit::Exists());

  INTRUDE(CAPTURE(&Z, &W, &Y), {
    //       <src>      |      <tgt>
    // W = A + X        | W = A + X
    // Y = B + X        | Y = B + X
    // Z = icmp eq W, Y | Z = icmp eq A, B
    //       <src>      |      <tgt>
    // W = A + X        | W = A + X
    // Y = B + X        | Y = B + X
    // Z = icmp ne W, Y | Z = icmp ne A, B
    llvm::Value *X = nullptr, *A = nullptr, *B = nullptr;
    auto pred = Z->getPredicate();
    unsigned bitsize = Y->getType()->getIntegerBitWidth();
    propagateInstruction(hints, W, Z, SRC);
    propagateInstruction(hints, Y, Z, SRC);
    if (W->getOperand(1) == Y->getOperand(1)) {
      X = W->getOperand(1);
      A = W->getOperand(0);
      B = Y->getOperand(0);
    } else if (W->getOperand(0) == Y->getOperand(0)) {
      X = W->getOperand(0);
      A = W->getOperand(1);
      B = Y->getOperand(1);
      // auto: W = X + A -> W = A + X
      // auto: Y = X + B -> Y = B + X
      //applyCommutativity(hints, Z, W, SRC);
      //applyCommutativity(hints, Z, Y, SRC);
    } else if (W->getOperand(0) == Y->getOperand(1)) {
      X = W->getOperand(0);
      A = W->getOperand(1);
      B = Y->getOperand(0);
      // auto: W = X + A -> W = A + X
      //applyCommutativity(hints, Z, W, SRC);
    } else if (W->getOperand(1) == Y->getOperand(0)) {
      X = W->getOperand(1);
      A = W->getOperand(0);
      B = Y->getOperand(1);
      // auto: Y = X + B -> Y = B + X
      //applyCommutativity(hints, Z, Y, SRC);
    }

    std::function<std::shared_ptr<TyInfrule>(
        std::shared_ptr<TyValue>, std::shared_ptr<TyValue>,
        std::shared_ptr<TyValue>, std::shared_ptr<TyValue>,
        std::shared_ptr<TyValue>, std::shared_ptr<TyValue>,
        std::shared_ptr<TySize>)> makeFunc;
    std::string optname;
    if (W->getOpcode() == llvm::Instruction::Add) {
      if (pred == llvm::ICmpInst::ICMP_EQ) {
        optname = "icmp_eq_add_add";
        makeFunc = ConsIcmpEqAddAdd::make;
      } else {
        optname = "icmp_ne_add_add";
        makeFunc = ConsIcmpNeAddAdd::make;
      }
    } else if (W->getOpcode() == llvm::Instruction::Sub) {
      if (pred == llvm::ICmpInst::ICMP_EQ) {
        optname = "icmp_eq_sub_sub";
        makeFunc = ConsIcmpEqSubSub::make;
      } else {
        optname = "icmp_ne_sub_sub";
        makeFunc = ConsIcmpNeSubSub::make;
      }
    } else if (W->getOpcode() == llvm::Instruction::Xor) {
      if (pred == llvm::ICmpInst::ICMP_EQ) {
        optname = "icmp_eq_xor_xor";
        makeFunc = ConsIcmpEqXorXor::make;
      } else {
        optname = "icmp_ne_xor_xor";
        makeFunc = ConsIcmpNeXorXor::make;
      }
    }// else {
    //  assert(false && "icmp_eq_<bop> optimization : opcode should be one of "
    //                  "ADD, SUB, or XOR");
    //}

    ValidationUnit::GetInstance()->setOptimizationName(optname);
    INFRULE(INSTPOS(SRC, Z), makeFunc(VAL(Z), VAL(W), VAL(X),
                    VAL(Y), VAL(A), VAL(B), BITSIZE(bitsize)));
  });
}

std::pair<std::shared_ptr<TyExpr>, std::shared_ptr<TyExpr> > false_encoding =
    std::make_pair(crellvm::ConsConst::make(0, 64),
                   crellvm::ConsConst::make(42, 64));

void generateHintForDCE(CoreHint &hints, llvm::Instruction &I) {
  std::string reg = getVariable(I);

  PROPAGATE(ConsMaydiff::make(reg, Physical), ConsGlobal::make());

  insertTgtNopAtSrcI(hints, &I);
}

void generateHintForTrivialDCE(llvm::Instruction &I) {
  assert(ValidationUnit::Exists());
  INTRUDE(CAPTURE(&I), {
    generateHintForDCE(hints, I);
    if (llvm::dyn_cast<llvm::CallInst>(&I)) {
      hints.setDescription("DCE on call "
                           "instruction.\n\"isInstructionTriviallyDead\" "
                           "should give enough power to validate.");
      hints.setReturnCodeToAdmitted();
    }
  });
}

void generateHintForGVNDCE(llvm::Instruction &I) {
  assert(ValidationUnit::Exists());
  INTRUDE(CAPTURE(&I), {
    generateHintForDCE(hints, I);
    if (llvm::dyn_cast<llvm::CallInst>(&I)) {
      hints.setDescription("DCE on call instruction inside GVN.\nIt might be "
                           "introduced from SimplifyInstruction or "
                           "lookup_or_add_call.");
      hints.setReturnCodeToAdmitted();
    }
  });
}

void generateHintForFoldPhiBin(llvm::PHINode &PN, llvm::PHINode *NewLHS, llvm::PHINode *NewRHS, llvm::DominatorTree *DT) {
  assert(NewLHS == nullptr || NewRHS == nullptr);

  INTRUDE(CAPTURE(&PN, NewLHS, NewRHS, DT), {
    llvm::PHINode *NewPHI = nullptr;
    if (NewLHS)
      NewPHI = NewLHS;
    else if (NewRHS)
      NewPHI = NewRHS;

    std::string oldphi = crellvm::getVariable(PN), newphi = "";
    llvm::BasicBlock::iterator InsertPos = PN.getParent()->getFirstInsertionPt();

    if (NewPHI) {
      newphi = crellvm::getVariable(*NewPHI);
      PROPAGATE(MAYDIFF(newphi, Physical), crellvm::ConsGlobal::make());
    }

    for (unsigned i = 0, e = PN.getNumIncomingValues(); i != e; ++i) {
      llvm::Instruction *InI = llvm::cast<llvm::Instruction>(PN.getIncomingValue(i));
      llvm::BasicBlock *InstB = InI->getParent();

      llvm::Value *CommonOp = InI->getOperand(NewLHS ? 1 : 0);
      llvm::Value *SpecialOp = InI->getOperand(NewLHS ? 0 : 1);

      auto PNpos = crellvm::TyPosition::make(SRC, PN.getParent()->getName(), PN.getIncomingBlock(i)->getName());

      //from I to endofblock propagate x or y depend on edge
      std::string reg = crellvm::getVariable(*InI); //reg is x or y
      PROPAGATE(LESSDEF(VAR(reg), RHS(reg, Physical, SRC), SRC), BOUNDS(INSTPOS(SRC, InI),
          crellvm::TyPosition::make_end_of_block(SRC, *InstB)));

      crellvm::propagateInvariantOnDominatedBlocks(LESSDEF(VAR(reg), RHS(reg, Physical, SRC), SRC), InstB, DT, hints);

      llvm::BinaryOperator *BinOp = llvm::dyn_cast<llvm::BinaryOperator>(InI);
      llvm::ICmpInst *CmpInst = llvm::dyn_cast<llvm::ICmpInst>(InI);
      auto Op0Phy = VAL(InI->getOperand(0)), Op1Phy = VAL(InI->getOperand(1));

      if (!NewPHI) {
        std::shared_ptr<crellvm::TyExpr> aph_bph;
        
        // (z >= x^) && (x^ >= x) -> (z >= x)
        INFRULE(PNpos, crellvm::ConsTransitivity::make(VAR(oldphi), VAR(reg, Previous), VAR(reg)));

        if (BinOp)
          aph_bph = INSN(BINARYINSN(*BinOp, TYPEOF(BinOp), Op0Phy, Op1Phy));
        else if (CmpInst)
          aph_bph = INSN(ICMPINSN(crellvm::getIcmpPred(CmpInst->getPredicate()),
                        TYPEOF(CmpInst->getOperand(0)), Op0Phy, Op1Phy));

        // (z >= x) && (x >= a + b) -> z >= a + b
        INFRULE(PNpos, crellvm::ConsTransitivity::make(VAR(oldphi), VAR(reg), aph_bph));
        // { z >= a + b } at src after phinode
        //    from I to endofblock propagate x or y depend on edge
        PROPAGATE(LESSDEF(VAR(oldphi), RHS(reg, Physical, SRC), SRC), BOUNDS(PHIPOSJustPhi(SRC, PN), INSTPOS(SRC, InsertPos)));

      } else {

        std::shared_ptr<crellvm::TyExpr> expr_twoprevs, expr_oneprev, expr_oneghost;
        auto Op0Prv = VAL(InI->getOperand(0), Previous), Op1Prv = VAL(InI->getOperand(1), Previous);
        auto K = ID("K", Ghost);
        auto CommOpTy = TYPEOF(CommonOp);

        if (BinOp) {
          expr_twoprevs = INSN(BINARYINSN(*BinOp, CommOpTy, Op0Prv, Op1Prv));
          expr_oneprev  = INSN(BINARYINSN(*BinOp, CommOpTy, NewLHS ? Op0Prv:Op0Phy, NewRHS ? Op1Prv:Op1Phy));
          expr_oneghost = INSN(BINARYINSN(*BinOp, CommOpTy, NewLHS ? K:Op0Phy,      NewRHS ? K:Op1Phy));
        } else if (CmpInst) {
          auto Pred = crellvm::getIcmpPred(CmpInst->getPredicate());
          expr_twoprevs = INSN(ICMPINSN(Pred, CommOpTy, Op0Prv, Op1Prv));
          expr_oneprev  = INSN(ICMPINSN(Pred, CommOpTy, NewLHS ? Op0Prv:Op0Phy, NewRHS ? Op1Prv:Op1Phy));
          expr_oneghost = INSN(ICMPINSN(Pred, CommOpTy, NewLHS ? K:Op0Phy,      NewRHS ? K:Op1Phy));
        }

        // x^ >= a^+b^ , z = x^ -> z >= a^+b^
        INFRULE(PNpos, crellvm::ConsTransitivity::make(VAR(oldphi), VAR(reg, Previous), expr_twoprevs));
        
        // If NewLHS: replace_rhs z >= a^ + b^ -> z >= a^ + b     //a is special b is common
        // If NewRHS: replace_rhs z >= a^ + b^ -> z >= a + b^     //a is common b is physical
        if (!llvm::isa<llvm::Constant>(CommonOp))
          INFRULE(PNpos, crellvm::ConsSubstitute::make(REGISTER(*CommonOp, Previous),
              VAL(CommonOp), expr_twoprevs));
        
        INFRULE(PNpos, crellvm::ConsTransitivity::make(VAR(oldphi), expr_twoprevs, expr_oneprev));

        // If NewLHS: introduce a^ >= k && k >= a^
        // If NewRHS: introduce b^ >= k && k >= b^
        INFRULE(PNpos, crellvm::ConsIntroGhost::make(EXPR(SpecialOp, Previous), REGISTER("K", Ghost)));

        // If NewLHS: infer (k >= a^) && (a^ >= t) -> k >= t in tgt
        // If NewRHS: infer (k >= b^) && (b^ >= t) -> k >= t in tgt
        INFRULE(PNpos, crellvm::ConsTransitivityTgt::make(VAR("K", Ghost), EXPR(SpecialOp, Previous), VAR(newphi)));

        // If NewLHS: introduce a^ + b >= k + b from a^ >= k 
        // If NewRHS: introduce a + b^ >= a + k from b^ >= k 
        INFRULE(PNpos, crellvm::ConsSubstituteRev::make(REGISTER("K", Ghost), VAL(SpecialOp, Previous), expr_oneghost));

        // If NewLHS: infer (z >= a^ + b) && (a^ + b >= K + b) -> z >= K + b in src
        // If NewRHS: infer (z >= a + b^) && (a + b^ >= a + K) -> z >= a + K in src
        INFRULE(PNpos, crellvm::ConsTransitivity::make(VAR(oldphi), expr_oneprev, expr_oneghost));

        // { z >= a + K } at src after phinode
        PROPAGATE(LESSDEF(VAR(oldphi), expr_oneghost, SRC), BOUNDS(PHIPOSJustPhi(SRC, PN), INSTPOS(SRC, InsertPos)));

        // { K  >= t } at tgt after phinode
        PROPAGATE(LESSDEF(VAR("K", Ghost), VAR(newphi), TGT), BOUNDS(PHIPOSJustPhi(TGT, PN), INSTPOS(TGT, InsertPos)));
      }
    }//end of for
  });
}

int getIndexofMem2Reg(llvm::Instruction* I,
                      int instrIndex, int termIndex) {
  if (llvm::dyn_cast_or_null<llvm::TerminatorInst>(I) != NULL)
    return termIndex;
  else
    return instrIndex;
}

bool hasBitcastOrGEP(llvm::AllocaInst* AI) {
  for (auto UI = AI->user_begin(), E = AI->user_end(); UI != E;) {
    llvm::Instruction* I = llvm::cast<llvm::Instruction>(*UI++);

    if (llvm::isa<llvm::BitCastInst>(I) ||
        llvm::isa<llvm::GetElementPtrInst>(I))
      return true;
  }
  return false;
}

void generateHintForPHIResolved(llvm::Instruction *I, llvm::BasicBlock *PB,
                                TyScope scope) {
  INTRUDE(CAPTURE(&I, &PB, scope), {
    llvm::Instruction *I_evolving = (*I).clone();
    for (unsigned i = 0, e = I->getNumOperands(); i != e; ++i) {
      llvm::Value *Op = I->getOperand(i);
      if (llvm::PHINode *OpPHI = llvm::dyn_cast<llvm::PHINode>(Op)) {
        if (I->getParent() != OpPHI->getParent())
          continue;
        llvm::Value *OpPHIResolved = OpPHI->getIncomingValueForBlock(PB);
        std::string OpPHI_id = getVariable(*OpPHI);
        std::string OpPHIResolved_id = getVariable(*OpPHIResolved);
        auto IPBPos = crellvm::TyPosition::make(
            SRC, getBasicBlockIndex(I->getParent()),
            getBasicBlockIndex(PB));

        INFRULE(IPBPos, ConsTransitivity::make(VAR(OpPHIResolved_id),
                                       VAR(OpPHIResolved_id, Previous),
                                       VAR(OpPHI_id)));

        INFRULE(IPBPos, ConsTransitivity::make(VAR(OpPHI_id),
                                       VAR(OpPHIResolved_id, Previous),
                                       VAR(OpPHIResolved_id)));

        llvm::Instruction *I_evolving_next = (*I_evolving).clone();
        (*I_evolving_next).setOperand(i, OpPHIResolved);

        // SubstituteRev [ I_evolving_next >= I_evolving ]
        // I_evolving_next = I_evolving[OpPHI := OpPHIResolved]
        INFRULE(IPBPos, crellvm::ConsSubstituteRev::make(
                    REGISTER(OpPHI_id), VAL(OpPHIResolved), INSN(*I_evolving)));
                    //        crellvm::ConsInsn::make(*I_evolving)));

        // SubstituteRev [ I_evolving >= I_evolving_next ]
        // I_evolving = I_evolving_next[OpPHIResolved := OpPHI]
        INFRULE(IPBPos, crellvm::ConsSubstituteRev::make(
                    REGISTER(OpPHIResolved_id), VAL(OpPHI), INSN(*I_evolving_next)));
                    //        crellvm::ConsInsn::make(*I_evolving_next)));

        INFRULE(IPBPos, crellvm::ConsTransitivity::make(
                    INSN(*I_evolving_next), INSN(*I_evolving), INSN(*I)));

        INFRULE(IPBPos, crellvm::ConsTransitivity::make(
                    INSN(*I), INSN(*I_evolving), INSN(*I_evolving_next)));

        delete I_evolving;
        I_evolving = I_evolving_next;
      }
    }
    delete I_evolving;
  });
}

std::shared_ptr<std::vector<std::shared_ptr<TyPosition>>> saveUseSet
                                      (llvm::Instruction* I, Dictionary &data) {
  auto &useIndices = *(data.get<ArgForIndices>()->useIndices);
  std::shared_ptr<std::vector<std::shared_ptr<TyPosition>>>
                  destSet(new std::vector<std::shared_ptr<TyPosition>>());

  for (auto UI = useIndices[I].begin(), E = useIndices[I].end(); UI != E;) {
    auto t = *(UI++);
    llvm::BasicBlock* useBB = std::get<0>(t);
    llvm::Instruction* use = std::get<1>(t);
    int useIndex = getIndexofMem2Reg(use, std::get<2>(t),
                        DICTMAP(data.get<crellvm::ArgForIndices>()->termIndices, useBB->getName()));

    if (use != nullptr && llvm::isa<llvm::PHINode>(use)) {
      llvm::PHINode *PHI = llvm::dyn_cast<llvm::PHINode>(use);

      for (unsigned i = 0; i != PHI->getNumIncomingValues(); ++i) {
        llvm::Value *v = llvm::dyn_cast<llvm::Value>(PHI->getIncomingValue(i));

        if (I == v) {
            const std::string &prev = PHI->getIncomingBlock(i)->getName();
            destSet->push_back(TyPosition::make(SRC, *use, useIndex, prev));
        }
      }
    } else
      destSet->push_back(TyPosition::make(SRC, *useBB, useIndex));
  }

  return destSet;
}

void saveInstrIndices(llvm::Function* F, Dictionary &data) {
  for (auto BS = F->begin(), BE = F->end(); BS != BE;) {
    llvm::BasicBlock* BB = BS++;

    data.get<crellvm::ArgForIndices>()->termIndices.get()->insert
      (std::pair<llvm::StringRef, unsigned>(BB->getName(), getTerminatorIndex(BB->getTerminator())));

    for (auto IS = BB->begin(), IE = BB->end(); IS != IE;) {
      llvm::Instruction* I = IS++;

      data.get<crellvm::ArgForIndices>()->instrIndices.get()->insert
        (std::pair<const llvm::Instruction*, unsigned>(I, getCommandIndex(*I)));
    }
  }
}

void saveUseIndices(llvm::Function* F, unsigned opCode, Dictionary &data) {
  auto &useIndices = *(data.get<ArgForIndices>()->useIndices);

  for (auto BS = F->begin(), BE = F->end(); BS != BE;) {
    llvm::BasicBlock* BB = BS++;

    for (auto IS = BB->begin(), IE = BB->end(); IS != IE;) {
      llvm::Instruction* I = IS++;

      if (I->getOpcode() == opCode)
        for (auto UI = I->use_begin(), E = I->use_end(); UI != E;) {
          llvm::Use &U = *(UI++);
          llvm::Instruction* use = llvm::dyn_cast<llvm::Instruction>(U.getUser());
          llvm::BasicBlock* useBB = use->getParent();

          useIndices[I].push_back(std::make_tuple(useBB, use, DICTMAP(data.get<crellvm::ArgForIndices>()->instrIndices, use)));
        }
    }
  }
}

void eraseInstrOfUseIndices(llvm::Instruction* key, llvm::Instruction* I, Dictionary &data) {
  auto &useIndices = *(data.get<ArgForIndices>()->useIndices);

  for (auto UI = useIndices[key].begin(), E = useIndices[key].end(); UI != E;) {
    auto t = *UI;

    if (std::get<1>(t) == I) {
      auto tuple = std::make_tuple(std::get<0>(t), nullptr, std::get<2>(t));
      int pos = std::distance(useIndices[key].begin(), UI);

      useIndices[key].erase(useIndices[key].begin()+pos);
      useIndices[key].push_back(tuple);
      break;
    }
    UI++;
  }
}

void propagateFromToUsingKey(unsigned key, std::shared_ptr<TyPosition> from_position, std::shared_ptr<TyPosition> to_position, Dictionary &data, CoreHint &hints) {
  auto &recentInstr = *(data.get<crellvm::ArgForMem2Reg>()->recentInstr);

  PROPAGATE(LESSDEF(recentInstr[key].instrL, VAR(recentInstr[key].op1, Ghost), SRC), BOUNDS(from_position, to_position));

  std::shared_ptr<TyExpr> val = recentInstr[key].instrR;
  
  PROPAGATE(LESSDEF(VAR(recentInstr[key].op1, Ghost), val, TGT), BOUNDS(from_position, to_position));

  std::string op0 = recentInstr[key].op0;
  if(op0 == "crellvm::PHI" || op0.substr(0,op0.rfind(".")) != op0)
    data.get<crellvm::ArgForMem2Reg>()->replaceItem.get()->push_back(std::shared_ptr<TyExpr>(val));

  // Infrule function
  // if from position is SI or AI, apply infrule  
  // if from position is PHI, no need to apply infrule  
  // if store operand changed before, then infrule already applied 
  if ((recentInstr[key].op0 != "crellvm::PHI") && (!recentInstr[key].check))
    INFRULE(recentInstr[key].instrPos, ConsIntroGhost::make(recentInstr[key].instrR, REGISTER(recentInstr[key].op1, Ghost)));
}

void propagateFromInsnToLoad(unsigned key, llvm::LoadInst *LI, Dictionary &data, CoreHint &hints) {
  auto &recentInstr = *(data.get<crellvm::ArgForMem2Reg>()->recentInstr);
  std::shared_ptr<TyPosition> from_position = NULL;

  if (recentInstr[key].op0 == "crellvm::PHI")
    from_position = STARTPOS(SRC, recentInstr[key].instrBB->getName());
  else
    from_position = recentInstr[key].instrPos;

  std::shared_ptr<TyPosition> to_position = INDEXEDPOS(SRC, LI, DICTMAP(data.get<crellvm::ArgForIndices>()->instrIndices, LI), "");

  propagateFromToUsingKey(key, from_position, to_position, data, hints);
}

void propagateFromInsnToPhi(unsigned key, llvm::PHINode *Phi, llvm::BasicBlock* prev, Dictionary &data, CoreHint &hints) {
  auto &recentInstr = *(data.get<crellvm::ArgForMem2Reg>()->recentInstr);
  std::shared_ptr<TyPosition> from_position = NULL;

  if (recentInstr[key].op0 == "crellvm::PHI")
    from_position = STARTPOS(SRC, recentInstr[key].instrBB->getName());
  else
    from_position = recentInstr[key].instrPos;

  std::shared_ptr<TyPosition> to_position = ENDPOSINDEXED(SRC, prev, DICTMAP(data.get<crellvm::ArgForIndices>()->termIndices, prev->getName()));

  propagateFromToUsingKey(key, from_position, to_position, data, hints);

  recentInstr[key].check = false; 
}

void checkSIOperand(unsigned key, llvm::StoreInst *SI, Dictionary &data, CoreHint &hints) {
  auto &recentInstr = *(data.get<crellvm::ArgForMem2Reg>()->recentInstr);
  auto &storeItem = *(data.get<crellvm::ArgForMem2Reg>()->storeItem);

  std::shared_ptr<TyPosition> to_position = NULL;
  to_position = INDEXEDPOS(SRC, SI, DICTMAP(data.get<crellvm::ArgForIndices>()->instrIndices, SI), "");

  if (!(storeItem[SI].op0 == "") && (!equalsIfConsVar(storeItem[SI].expr, EXPR(SI->getOperand(0), Physical)))) {
    //global -> constant or argument it won't change
    std::string op1 = getVariable(*(SI->getOperand(1)));
    // {tmp = tmp^ = a}
    // store %tmp, %y
    // [tmp = tmp^         tmp^ = a
    //  load %y = %tmp              ]
    //
    //  intro ghost
    //          tmp^ = y^ = tmp^
    INFRULE(to_position, ConsIntroGhost::make(VAR(storeItem[SI].op0, Ghost), REGISTER(op1, Ghost)));
    recentInstr[key].check = true;
  } else
    recentInstr[key].check = false;
}

void propagateLoadInstToUse(llvm::LoadInst *LI, llvm::Value *V, std::string In, Dictionary &data, CoreHint &hints, bool checkReplace) {
  auto &instrIndices = *(data.get<crellvm::ArgForIndices>()->instrIndices);
  std::shared_ptr<std::vector<std::shared_ptr<crellvm::TyPosition>>> useSet = saveUseSet(LI, data);
  std::string Rload = crellvm::getVariable(*LI);

  //propagate LI to use set
  PROPAGATE(LESSDEF(VAR(Rload, Physical), VAR(Rload, Ghost), SRC), BOUNDSET(INDEXEDPOS(SRC, LI, instrIndices[LI], ""), useSet));

  std::shared_ptr<TyExpr> val = TyExpr::make(*V, Physical);
  PROPAGATE(LESSDEF(VAR(Rload, Ghost), val, TGT), BOUNDSET(INDEXEDPOS(SRC, LI, instrIndices[LI], ""), useSet));

  //infrule at LI index
  INFRULE(INDEXEDPOS(SRC, LI, instrIndices[LI], ""), ConsIntroGhost::make(VAR(In, Ghost), REGISTER(Rload, Ghost)));

  if (checkReplace && !llvm::isa<llvm::Constant>(V))
    data.get<crellvm::ArgForMem2Reg>()->replaceItem.get()->push_back(std::shared_ptr<TyExpr>(val));
}

void propagateLoadGhostValueFromAIToLI(llvm::AllocaInst* AI, llvm::LoadInst* LI, llvm::Value* value, Dictionary &data, CoreHint &hints) {
  std::string Rghost = getVariable(*AI);
  auto replaceItem = data.get<crellvm::ArgForMem2Reg>()->replaceItem;
  auto instrIndices = data.get<crellvm::ArgForIndices>()->instrIndices;

  std::shared_ptr<TyPosition> from_position = INDEXEDPOS(SRC, AI, DICTMAP(instrIndices, AI), "");
  std::shared_ptr<TyPosition> to_position = INDEXEDPOS(SRC, LI, DICTMAP(instrIndices, LI), "");

  PROPAGATE(LESSDEF(INSNALIGNONE(AI), VAR(Rghost, Ghost), SRC), BOUNDS(from_position, to_position));

  std::shared_ptr<TyExpr> val = EXPR(value, Physical);
  PROPAGATE(LESSDEF(VAR(Rghost, Ghost), val, TGT), BOUNDS(from_position, to_position));

  if (value->getName() != "") { replaceItem.get()->push_back(std::shared_ptr<TyExpr>(val)); }
  INFRULE(from_position, ConsIntroGhost::make(val, REGISTER(Rghost, Ghost)));
}

void propagateLoadGhostValueFromSIToLI(llvm::StoreInst* SI, llvm::LoadInst* LI, llvm::Value* value, Dictionary &data, CoreHint &hints, bool checkReplace) {

  std::string Rghost = getVariable(*(SI->getOperand(1)));
  auto ArgForMem2Reg = data.get<crellvm::ArgForMem2Reg>();
  auto instrIndices = data.get<crellvm::ArgForIndices>()->instrIndices;
  
  std::shared_ptr<TyPosition> from_position = INDEXEDPOS(SRC, SI, DICTMAP(instrIndices, SI), "");
  std::shared_ptr<TyPosition> to_position = INDEXEDPOS(SRC, LI, DICTMAP(instrIndices, LI), "");

  PROPAGATE(LESSDEF(INSNALIGNONE(SI), VAR(Rghost, Ghost), SRC), BOUNDS(from_position, to_position));

  std::shared_ptr<TyExpr> val = EXPR(value, Physical);
  PROPAGATE(LESSDEF(VAR(Rghost, Ghost), val, TGT), BOUNDS(from_position, to_position));

  if (value->getName() != "") { ArgForMem2Reg->replaceItem.get()->push_back(std::shared_ptr<TyExpr>(val)); }
  std::shared_ptr<crellvm::TyExpr> expr = EXPR(value, Physical);
  
  if (checkReplace) {
    auto &storeItem = *(ArgForMem2Reg->storeItem);
    if (equalsIfConsVar(storeItem[SI].expr, expr)) { ArgForMem2Reg->replaceTag.get()->push_back(expr); }
    else if (storeItem[SI].op0 != "") { expr = VAR(storeItem[SI].op0, Ghost); }
  }

  INFRULE(from_position, crellvm::ConsIntroGhost::make(expr, REGISTER(Rghost, Ghost)));
}

bool equalsIfConsVar(std::shared_ptr<TyExpr> e1, std::shared_ptr<TyExpr> e2) {
  if (ConsVar *cv1 = e1->get().get()->getConsVar()) {
    if (ConsVar *cv2 = e2->get().get()->getConsVar()) {
      return TyRegister::isSame(cv1->getTyReg(), cv2->getTyReg());
    }  
  }
  return false;
}


void replaceExpr(llvm::Instruction *Tgt, llvm::Value *New, Dictionary &data) {
  auto &replaceItem = *(data.get<crellvm::ArgForMem2Reg>()->replaceItem);
  std::string str = "";

  if (llvm::isa<llvm::AllocaInst>(Tgt) || llvm::isa<llvm::LoadInst>(Tgt) || llvm::isa<llvm::PHINode>(Tgt))
    str = getVariable(*Tgt);
  
  auto var = ConsVar::make(str, Physical);
  for (unsigned i = 0; i < replaceItem.size(); i++) {
    std::shared_ptr<TyExpr> tmp = replaceItem.at(i);
    if (equalsIfConsVar(tmp, var))
     tmp->replace_expr(EXPR(New, Physical)); 
  }
}

void replaceTag(llvm::Instruction *Tgt, TyTag tag, Dictionary &data) {
  auto &replaceTag = *(data.get<crellvm::ArgForMem2Reg>()->replaceTag);
  std::string str = "";

  if (llvm::isa<llvm::AllocaInst>(Tgt) || llvm::isa<llvm::LoadInst>(Tgt) || llvm::isa<llvm::PHINode>(Tgt))
    str = getVariable(*Tgt);

  auto var = ConsVar::make(str, Physical);
  for (unsigned i = 0; i < replaceTag.size(); i++) {
    std::shared_ptr<TyExpr> tmp = replaceTag.at(i);
    if (equalsIfConsVar(replaceTag.at(i), var))
        tmp->replace_expr(ConsVar::make(str, tag));
  }
}

void unreachableBlockPropagateFalse(llvm::BasicBlock* bb, CoreHint &hints) {
  PROPAGATE(LESSDEF(false_encoding.first, false_encoding.second, SRC), BOUNDS(STARTPOS(SRC, getBasicBlockIndex(bb)), ENDPOS(SRC, bb)));
  std::vector<llvm::BasicBlock *> DeadBlockList;

  // find predecssor of pred and insert in worklist if it has one
  for (auto BI = pred_begin(bb), BE = pred_end(bb); BI != BE; BI++)
    DeadBlockList.push_back((*BI));

  while(!DeadBlockList.empty()) {
    llvm::BasicBlock * L = *DeadBlockList.rbegin();
    DeadBlockList.pop_back();

    // propagate false start to end of K.
    PROPAGATE(LESSDEF(false_encoding.first, false_encoding.second, SRC),
              BOUNDS(STARTPOS(SRC, getBasicBlockIndex(L)), ENDPOS(SRC, L)));

    // find predessor of K and insert in worklist if it has one
    for (auto BI = pred_begin(L), BE = pred_end(L); BI != BE; BI++)
      DeadBlockList.push_back((*BI));
  }
}

void propagateInvariantOnDominatedBlocks(std::shared_ptr<TyPropagateObject> invariant,
    llvm::BasicBlock *dominatingBlock, llvm::DominatorTree *DT, CoreHint &hints) {
  for (auto node = llvm::GraphTraits<llvm::DominatorTree *>::nodes_begin(DT);
       node != llvm::GraphTraits<llvm::DominatorTree *>::nodes_end(DT);
       ++node) {
    llvm::BasicBlock *BB = node->getBlock();
    if ((BB->getName() != dominatingBlock->getName()) && DT->dominates(dominatingBlock, BB)) {
      PROPAGATE(invariant,
          BOUNDS(crellvm::TyPosition::make_start_of_block(SRC, (BB->getName())),
                 crellvm::TyPosition::make_end_of_block(SRC, *(BB->begin()->getParent()))));
    }
  }
}

} // crellvm
