#include "llvm/LLVMBerry/ValidationUnit.h"
#include "llvm/LLVMBerry/Dictionary.h"
#include "llvm/LLVMBerry/Hintgen.h"
#include "llvm/LLVMBerry/Infrules.h"
#include "llvm/LLVMBerry/InstCombine/InfrulesAddSub.h"
#include "llvm/LLVMBerry/InstCombine/InfrulesAndOrXor.h"
#include "llvm/LLVMBerry/InstCombine/InfrulesCompares.h"
#include "llvm/LLVMBerry/InstCombine/InfrulesSelect.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

namespace llvmberry {
// insert nop at tgt where I is at src
void insertTgtNopAtSrcI(CoreHint &hints, llvm::Instruction *I) {
  std::string empty_str = "";
  if (I == I->getParent()->getFirstNonPHI()) {
    std::string nop_block_name = getBasicBlockIndex(I->getParent());
    hints.addNopPosition(
        TyPosition::make(TyScope::Target, nop_block_name, empty_str));
  } else if (!llvm::isa<llvm::PHINode>(I)) {
    llvm::BasicBlock::iterator prevI = I;
    prevI--;
    hints.addNopPosition(TyPosition::make(TyScope::Target, *prevI));
  }
}

// insert nop at src where I is at tgt
void insertSrcNopAtTgtI(CoreHint &hints, llvm::Instruction *I) {
  std::string empty_str = "";
  if (I == I->getParent()->getFirstNonPHI()) {
    std::string nop_block_name = getBasicBlockIndex(I->getParent());
    hints.addNopPosition(
        TyPosition::make(TyScope::Source, nop_block_name, empty_str));
  } else if (!llvm::isa<llvm::PHINode>(I)) {
    llvm::BasicBlock::iterator prevI = I;
    prevI--;
    hints.addNopPosition(TyPosition::make(TyScope::Source, *prevI));
  }
}
/* propagateInstruction(I1, I2, scope, propagateEquivalence) :
 *   if propagateEquivalence == false :
 *     Propagates I1 >= rhs(I1) from I1 to I2 if scope == Source, or
 *     Propagates rhs(I1) >= I1 from I1 to I2 if scope == Target
 *   else :
 *     Propagate I1 >= rhs(I1) and rhs(I1) >= I1 from I1 to I2 in scope.
 */
void propagateInstruction(llvmberry::CoreHint &hints, const llvm::Instruction *from,
                          const llvm::Instruction *to,
                          TyScope scope, bool propagateEquivalence) {
  assert(ValidationUnit::Exists());
  std::string reg_name = getVariable(*from);

  if (scope == Source) {
    PROPAGATE(LESSDEF(VAR(reg_name, Physical), RHS(reg_name, Physical, scope), scope),
              BOUNDS(INSTPOS(scope, from), INSTPOS(scope, to)));
    if (propagateEquivalence) {
      PROPAGATE(LESSDEF(RHS(reg_name, Physical, scope), VAR(reg_name, Physical), scope),
                BOUNDS(INSTPOS(scope, from), INSTPOS(scope, to)));
    }
  } else if (scope == Target) {
    PROPAGATE(LESSDEF(RHS(reg_name, Physical, scope), VAR(reg_name, Physical), scope),
              BOUNDS(INSTPOS(scope, from), INSTPOS(scope, to)));
    if (propagateEquivalence) {
      PROPAGATE(LESSDEF(VAR(reg_name, Physical), RHS(reg_name, Physical, scope), scope),
                BOUNDS(INSTPOS(scope, from), INSTPOS(scope, to)));
    }
  } else {
    assert("propagateInstruction() : scope is neither Source nor Target" &&
           false);
  }
}

void propagateLessdef(llvmberry::CoreHint &hints,
                      const llvm::Instruction *from, const llvm::Instruction *to,
                      const llvm::Value *lesserval,
                      const llvm::Value *greaterval, TyScope scope) {
  assert(ValidationUnit::Exists());
  PROPAGATE(ConsLessdef::make(TyExpr::make(*greaterval),
                        TyExpr::make(*lesserval), scope),
      BOUNDS(INSTPOS(scope, from), INSTPOS(scope, to)));
}

void applyCommutativity(llvmberry::CoreHint &hints, llvm::Instruction *position,
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

void applyTransitivity(llvmberry::CoreHint &hints, llvm::Instruction *position, llvm::Value *v_greatest,
                       llvm::Value *v_mid, llvm::Value *v_smallest,
                       TyScope scope) {
  applyTransitivity(hints, position, v_greatest, v_mid, v_smallest, scope, scope);
}

void applyTransitivity(llvmberry::CoreHint &hints, llvm::Instruction *position, llvm::Value *v_greatest,
                       llvm::Value *v_mid, llvm::Value *v_smallest,
                       TyScope scope, TyScope position_scopetag) {
  assert(ValidationUnit::Exists());

  INFRULE(INSTPOS(position_scopetag, position),
          ConsTransitivity::make(EXPR(v_greatest, Physical), EXPR(v_mid, Physical), EXPR(v_smallest, Physical)));
}

void propagateMaydiffGlobal(llvmberry::CoreHint &hints, std::string varname, TyTag tag) {
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

      PROPAGATE(LESSDEF(VAR(reg1_name), RHS(reg1_name, Physical, scope), scope),
          BOUNDS(INSTPOS(scope, Vins), INSTPOS(scope, &I)));
      /*
          ConsLessdef::make(ConsVar::make(reg1_name, Physical), // my = -y
                            ConsRhs::make(reg1_name, Physical, scope), scope),
          ConsBounds::make(
              TyPosition::make(scope, *Vins), // From my to z = x -my
              TyPosition::make(scope, I))));
      */

      PROPAGATE(LESSDEF(RHS(reg1_name, Physical, scope), VAR(reg1_name), scope),
                BOUNDS(INSTPOS(scope, Vins), INSTPOS(scope, &I)));
      /*
      hints.addCommand(ConsPropagate::make(
          ConsLessdef::make(ConsRhs::make(reg1_name, Physical, scope),
                            ConsVar::make(reg1_name, Physical), // my = -y
                            scope),
          ConsBounds::make(
              TyPosition::make(scope, *Vins), // From my to z = x -my
              TyPosition::make(scope, I))));
      */
    });
  }
  // Constants can be considered to be negated values if they can be folded.
  if (llvm::ConstantInt *C = llvm::dyn_cast<llvm::ConstantInt>(V)) {
    INTRUDE(CAPTURE(&I, &V, &C, &scope), {
      //std::string reg0_name = getVariable(I); // z = x -my

      unsigned sz_bw = I.getType()->getPrimitiveSizeInBits();
      int64_t c1 = C->getSExtValue();
      int64_t c2 = -c1;

      INFRULE(INSTPOS(scope, &I), ConsNegVal::make(CONSTINT(c1, sz_bw),
              CONSTINT(c2, sz_bw), BITSIZE(I)));
      /*
      hints.addCommand(
          ConsInfrule::make(TyPosition::make(scope, I),
                            ConsNegVal::make(TyConstInt::make(c1, sz_bw),
                                             TyConstInt::make(c2, sz_bw),
                                             ConsSize::make(sz_bw))));*/

      INFRULE(INSTPOS(scope, &I), ConsNegVal::make(CONSTINT(c1, sz_bw),
              CONSTINT(c2, sz_bw), BITSIZE(I)));
      /*hints.addCommand(
          ConsInfrule::make(TyPosition::make(scope, I),
                            ConsNegVal::make(TyConstInt::make(c1, sz_bw),
                                             TyConstInt::make(c2, sz_bw),
                                             ConsSize::make(sz_bw))));
      */
    });
  }
  //  if(ConstantDataVector *C = dyn_cast<ConstantDataVector>(V))
  //  {
  //  Todo
  //  }
}

void generateHintForReplaceAllUsesWith(llvm::Instruction *source,
                                       llvm::Value *replaceTo,
                                       std::string ghostvar,
                                       std::shared_ptr<TyPosition> source_pos,
                                       std::function<bool(const llvm::Value *)> filter) {
  assert(ValidationUnit::Exists());
  if (!source_pos) {
    source_pos = INSTPOS(SRC, source);
  }

  INTRUDE(CAPTURE(&source, &replaceTo, &ghostvar, &source_pos, filter), {
    llvm::Instruction *I = source;

    std::string to_rem = getVariable(*I);

    for (auto UI = I->use_begin(); UI != I->use_end(); ++UI) {
      if (filter && !filter(UI->getUser()))
        continue;

      if (!llvm::isa<llvm::Instruction>(UI->getUser())) {
        // let the validation fail when the user is not an instruction
        return;
      }
      std::string user = getVariable(*UI->getUser());
      llvm::Instruction *user_I =
          llvm::dyn_cast<llvm::Instruction>(UI->getUser());

      std::string prev_block_name = "";
      if (llvm::isa<llvm::PHINode>(user_I)) {
        llvm::BasicBlock *bb_from =
            llvm::dyn_cast<llvm::PHINode>(user_I)->getIncomingBlock(*UI);
        prev_block_name = getBasicBlockIndex(bb_from);
      }

      if (ghostvar == "") {
        PROPAGATE(LESSDEF(VAR(to_rem), EXPR(replaceTo, Physical), SRC),
                  BOUNDS(source_pos,
                    TyPosition::make(SRC, *user_I, prev_block_name)));
        if (llvm::isa<llvm::PHINode>(user_I)) {
          INFRULE(TyPosition::make(SRC, *user_I, prev_block_name),
                  ConsTransitivity::make(VAR(user), VAR(to_rem, Previous),
                                         EXPR(replaceTo, Previous)));
        } else if (!user.empty() && !llvm::isa<llvm::CallInst>(user_I)) {
          INFRULE(TyPosition::make(Source, *user_I, prev_block_name),
                  ConsReplaceRhs::make(
                      REGISTER(to_rem), VAL(replaceTo),
                      VAR(user), RHS(user, Physical, Source),
                      RHS(user, Physical, Target)));
        }
      } else {
        PROPAGATE(LESSDEF(VAR(to_rem), VAR(ghostvar, Ghost), SRC),
                  BOUNDS(source_pos,
                         TyPosition::make(SRC, *user_I, prev_block_name)));
        PROPAGATE(LESSDEF(VAR(ghostvar, Ghost), EXPR(replaceTo), TGT),
                  BOUNDS(INSTPOS(TGT, I),
                         TyPosition::make(TGT, *user_I, prev_block_name)));


        if (llvm::isa<llvm::PHINode>(user_I)) {
          // src : user = phi [ to_rem, prev_block_name ]
          // tgt : user = phi [replaceTo, prev_block_name ]
          // In src : Transitivity ;
          //    user >= to_rem(physical) >= to_rem(previous) >= ghostvar
          // In tgt : TransitivityTgt ;
          //    ghostva >= replaceTo(physical) >= replaceTo(previous) >= user
          INFRULE(TyPosition::make(SRC, *user_I, prev_block_name),
                  ConsTransitivity::make(VAR(user),
                                         VAR(to_rem, Previous),
                                         VAR(to_rem)));
          INFRULE(TyPosition::make(SRC, *user_I, prev_block_name),
                  ConsTransitivity::make(VAR(user),
                                         VAR(to_rem),
                                         VAR(ghostvar, Ghost)));
          INFRULE(TyPosition::make(TGT, *user_I, prev_block_name),
                  ConsTransitivityTgt::make(EXPR(replaceTo),
                                            EXPR(replaceTo, Previous),
                                            VAR(user)));
          INFRULE(TyPosition::make(TGT, *user_I, prev_block_name),
                  ConsTransitivityTgt::make(VAR(ghostvar, Ghost),
                                            EXPR(replaceTo),
                                            VAR(user)));
        } else if (!user.empty() && !llvm::isa<llvm::CallInst>(user_I)) {
          if (!TyInstruction::isSupported(*user_I)) {
            std::string str;
            llvm::raw_string_ostream rso(str);
            rso << "\ngenerateHintForReplaceAllUsesWith : Unsupported instruction";

            hints.appendToDescription(rso.str());
            if (hints.getReturnCode() == CoreHint::ACTUAL)
              hints.setReturnCodeToAdmitted();
            return;
          }
          INFRULE(TyPosition::make(SRC, *user_I, prev_block_name),
                  ConsSubstitute::make(REGISTER(to_rem),
                                       ID(ghostvar, Ghost), INSN(*user_I)));
          llvm::Instruction *user_I_copy = user_I->clone();

          for (unsigned i = 0; i < user_I_copy->getNumOperands(); i++) {
            if (user_I->getOperand(i) == source)
              user_I_copy->setOperand(i, replaceTo);
          }
          INFRULE(TyPosition::make(SRC, *user_I, prev_block_name),
                  ConsTransitivity::make(EXPR(user_I), INSN(*user_I),
                                         INSN(*user_I_copy)));
          delete user_I_copy;
        }
      }
    }
  });
}

void generateHintForReplaceAllUsesWithAtTgt(llvm::Instruction *source,
                                            llvm::Value *replaceTo,
                                            std::shared_ptr<TyPosition> invariant_pos) {
  assert(ValidationUnit::Exists());

  if (!invariant_pos) {
    invariant_pos = INSTPOS(TGT, source);
  }

  INTRUDE(CAPTURE(&source, &replaceTo, &invariant_pos), {
    llvm::Instruction *I = source;
    auto I_pos = invariant_pos;

    std::string I_var = getVariable(*I);

    for (auto UI = I->use_begin(); UI != I->use_end(); ++UI) {
      if (!llvm::isa<llvm::Instruction>(UI->getUser())) {
        // let the validation fail when the user is not an instruction
        return;
      }
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
                                  bool needs_commutativity, bool is_leftform) {
  assert(ValidationUnit::Exists());
  assert(Z);
  assert(X);
  assert(Y);

  INTRUDE(CAPTURE(&Z, &X, &Y, needs_commutativity, is_leftform), {
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

    llvm::Value *c = Y->getCondition();
    llvm::Value *n = X->getOperand(0);
    llvm::Value *a = X->getOperand(1);
    llvm::Value *a_Z = Z->getOperand(1);
    int bitwidth = Z->getType()->getIntegerBitWidth();

    // prepare variables
    std::string reg_y_name = getVariable(*Y);
    std::string reg_z_name = getVariable(*Z);
    std::string reg_x_name = getVariable(*X);

    // Propagate "X = n - a"
    PROPAGATE(LESSDEF(VAR(reg_x_name), RHS(reg_x_name, Physical, Source), Source),
              BOUNDS(INSTPOS(Source, X), INSTPOS(Source, Z)));
    /*
    hints.addCommand(ConsPropagate::make(
        ConsLessdef::make(ConsVar::make(reg_x_name, Physical),
                          ConsRhs::make(reg_x_name, Physical, Source), Source),
        ConsBounds::make(TyPosition::make(Source, *X),
                         TyPosition::make(Source, *Z))));
    */

    // Propagate "Y = select c ? x : 0" or "Y = select c ? 0 : x"
    PROPAGATE(LESSDEF(VAR(reg_y_name), RHS(reg_y_name, Physical, Source), Source),
              BOUNDS(INSTPOS(Source, Y), INSTPOS(Source, Z)));
    /*
    hints.addCommand(ConsPropagate::make(
        ConsLessdef::make(ConsVar::make(reg_y_name, Physical),
                          ConsRhs::make(reg_y_name, Physical, Source), Source),
        ConsBounds::make(TyPosition::make(Source, *Y),
                         TyPosition::make(Source, *Z))));
    */

    if (needs_commutativity) {
      INFRULE(INSTPOS(Source, Z), ConsBopCommutative::make(VAR(reg_z_name),
              TyBop::BopAdd, VAL(Y), VAL(a_Z), BITSIZE(bitwidth)));
      /*
      hints.addCommand(ConsInfrule::make(
          TyPosition::make(Source, *Z),
          ConsBopCommutative::make(VAR(reg_z_name, Physical), TyBop::BopAdd,
                                   TyValue::make(*Y), TyValue::make(*a_Z),
                                   ConsSize::make(bitwidth))));
      */
    }

    if (is_leftform) {
      INFRULE(INSTPOS(Source, Z), ConsAddSelectZero::make(REGISTER(reg_z_name),
              REGISTER(reg_x_name), REGISTER(reg_y_name), VAL(c), VAL(n), VAL(a),
              BITSIZE(bitwidth)));
      /*
      hints.addCommand(ConsInfrule::make(
          TyPosition::make(Source, *Z),
          ConsAddSelectZero::make(
              TyRegister::make(reg_z_name, Physical),
              TyRegister::make(reg_x_name, Physical),
              TyRegister::make(reg_y_name, Physical), TyValue::make(*c),
              TyValue::make(*n), TyValue::make(*a), ConsSize::make(bitwidth))));
      */
    } else {
      INFRULE(INSTPOS(Source, Z), ConsAddSelectZero2::make(REGISTER(reg_z_name),
              REGISTER(reg_x_name), REGISTER(reg_y_name), VAL(c), VAL(n), VAL(a),
              BITSIZE(bitwidth)));
      /*
      hints.addCommand(ConsInfrule::make(
          TyPosition::make(Source, *Z),
          ConsAddSelectZero2::make(
              TyRegister::make(reg_z_name, Physical),
              TyRegister::make(reg_x_name, Physical),
              TyRegister::make(reg_y_name, Physical), TyValue::make(*c),
              TyValue::make(*n), TyValue::make(*a), ConsSize::make(bitwidth))));
      */
    }
  });
}

void generateHintForOrAnd(llvm::BinaryOperator *Y, llvm::Value *X,
                          llvm::Value *A) {
  assert(ValidationUnit::Exists());

  INTRUDE(CAPTURE(Y, X, A), {
    auto ptr = data.get<ArgForSimplifyOrInst>();
    bool isSwapped = ptr->isSwapped;
    ptr->setHintGenFunc("or_and", [Y, X, A, isSwapped,
                                   &hints](llvm::Instruction *I) {
      //   <src>   |   <tgt>
      // Y = X & A | Y = X & A
      // Z = Y | X | (Z equals X)
      llvm::BinaryOperator *Z = llvm::dyn_cast<llvm::BinaryOperator>(I);
      assert(Z && "Z must be a binary operator in or_and optimization");

      bool Zswapped = Z->getOperand(0) == X;
      propagateInstruction(hints, Y, Z, SRC);
      if (Zswapped ^ isSwapped)
        applyCommutativity(hints, Z, Z, SRC);
      if (Y->getOperand(0) == A)
        applyCommutativity(hints, Z, Y, SRC);
      INFRULE(INSTPOS(SRC, Z), ConsOrAnd::make(VAL(Z), VAL(Y),
              VAL(X), VAL(A), BITSIZE(*Z)));
    });
  });
}

void generateHintForOrXor(llvm::BinaryOperator *W, llvm::Value *op0,
                          llvm::Value *op1, bool needsCommutativity) {
  assert(ValidationUnit::Exists());

  INTRUDE(CAPTURE(&W, &op0, &op1, &needsCommutativity), {
    //    <src>    |   <tgt>
    // X = B ^ -1  | X = B ^ -1
    // Y = A & X   | Y = A & X
    // Z = A ^ B   | Z = A ^ B
    // W = Y | Z   | W = A ^ B
    llvm::BinaryOperator *Z = llvm::dyn_cast<llvm::BinaryOperator>(op1);
    llvm::BinaryOperator *Y = llvm::dyn_cast<llvm::BinaryOperator>(op0);
    llvm::BinaryOperator *X =
        llvm::dyn_cast<llvm::BinaryOperator>(Y->getOperand(1));
    assert(X);
    assert(Y);
    assert(Z);
    assert(W);
    llvm::Value *A = Z->getOperand(0);
    llvm::Value *B = Z->getOperand(1);
    int bitwidth = W->getType()->getIntegerBitWidth();

    propagateInstruction(hints, X, W, Source);
    propagateInstruction(hints, Y, W, Source);
    propagateInstruction(hints, Z, W, Source);
    if (X->getOperand(1) == B)
      applyCommutativity(hints, W, X, Source);

    if (needsCommutativity)
      applyCommutativity(hints, W, W, Source);

    INFRULE(INSTPOS(SRC, W), ConsOrXor::make(
        VAL(W), VAL(Z), VAL(X), VAL(Y), VAL(A), VAL(B), BITSIZE(bitwidth)));
  });
}

void generateHintForOrXor2(llvm::BinaryOperator *Z, llvm::Value *X1_val,
                           llvm::Value *X2_val, llvm::Value *A, llvm::Value *B,
                           bool needsY1Commutativity,
                           bool needsY2Commutativity) {
  assert(ValidationUnit::Exists());

  INTRUDE(CAPTURE(&Z, &X1_val, &X2_val, &A, &B, needsY1Commutativity, needsY2Commutativity), {
    //     <src>           <tgt>
    // X1 = B  ^ -1  | X1 =  B ^ -1
    // Y1 = A  & X1  | Y1 =  A & X1
    // X2 = A  ^ -1  | X2 =  A ^ -1
    // Y2 = X2 & B   | Y2 = X2 & B
    // Z =  Y1 | Y2  | Z =   A ^ B
    llvm::BinaryOperator *Y1 =
        llvm::dyn_cast<llvm::BinaryOperator>(Z->getOperand(0));
    llvm::BinaryOperator *Y2 =
        llvm::dyn_cast<llvm::BinaryOperator>(Z->getOperand(1));
    llvm::BinaryOperator *X1 = llvm::dyn_cast<llvm::BinaryOperator>(X1_val);
    llvm::BinaryOperator *X2 = llvm::dyn_cast<llvm::BinaryOperator>(X2_val);
    assert(Y1);
    assert(Y2);
    assert(X1);
    assert(X2);
    int bitwidth = Z->getType()->getIntegerBitWidth();

    propagateInstruction(hints, X1, Z, Source);
    propagateInstruction(hints, X2, Z, Source);
    propagateInstruction(hints, Y1, Z, Source);
    propagateInstruction(hints, Y2, Z, Source);
    if (X1->getOperand(1) == B)
      applyCommutativity(hints, Z, X1, Source);
    if (X2->getOperand(1) == A)
      applyCommutativity(hints, Z, X2, Source);
    if (needsY1Commutativity)
      applyCommutativity(hints, Z, Y1, Source);
    if (needsY2Commutativity)
      applyCommutativity(hints, Z, Y2, Source);

    INFRULE(INSTPOS(TGT, Z), ConsOrXor2::make(
        VAL(Z), VAL(X1), VAL(Y1), VAL(X2), VAL(Y2), VAL(A), VAL(B), BITSIZE(bitwidth)));
    /*
    hints.addCommand(ConsInfrule::make(
        TyPosition::make(Target, *Z),
        ConsOrXor2::make(TyValue::make(*Z), TyValue::make(*X1),
                         TyValue::make(*Y1), TyValue::make(*X2),
                         TyValue::make(*Y2), TyValue::make(*A),
                         TyValue::make(*B), ConsSize::make(bitwidth))));
    */
  });
}

void generateHintForOrXor4(llvm::BinaryOperator *Z, llvm::Value *X,
                           llvm::BinaryOperator *Y, llvm::BinaryOperator *A,
                           llvm::Value *B, llvm::BinaryOperator *NB,
                           bool needsYCommutativity, bool needsZCommutativity) {
  assert(ValidationUnit::Exists());

  INTRUDE(CAPTURE(&Z, &X, &Y, &A, &B, &NB, needsYCommutativity, needsZCommutativity), {
    // <src>      |  <tgt>
    // A = X ^ -1 | A = X ^ -1
    // Y = A ^ B  | Y = A ^ B
    // <nop>      | NB = B ^ -1
    // Z = X | Y  | Z = NB | X
    int bitwidth = Z->getType()->getIntegerBitWidth();
    propagateInstruction(hints, A, Z, Target);
    propagateInstruction(hints, Y, Z, Target);
    propagateInstruction(hints, NB, Z, Target);
    if (needsYCommutativity)
      applyCommutativity(hints, Z, Y, Target);
    insertSrcNopAtTgtI(hints, NB);
    propagateMaydiffGlobal(hints, getVariable(*NB), Physical);

    INFRULE(INSTPOS(TGT, Z), ConsOrXor4::make(VAL(Z), VAL(X),
            VAL(Y), VAL(A), VAL(B), VAL(NB), BITSIZE(bitwidth)));
    if (needsZCommutativity)
      INFRULE(INSTPOS(TGT, Z), ConsOrCommutativeTgt::make(
              REGISTER(*Z), VAL(X), VAL(Y), BITSIZE(bitwidth)));
  });
}

void generateHintForAddXorAnd(llvm::BinaryOperator *Z, llvm::BinaryOperator *X,
                              llvm::BinaryOperator *Y, llvm::Value *A,
                              llvm::Value *B, bool needsYCommutativity,
                              bool needsZCommutativity) {
  INTRUDE(CAPTURE(&Z, &X, &Y, needsYCommutativity, needsZCommutativity), {
    //    <src>       <tgt>
    // X = A ^ B  | X = A ^ B
    // Y = A & B  | Y = A & B
    // Z = X + Y  | Z = A | B
    llvm::Value *A = X->getOperand(0);
    llvm::Value *B = X->getOperand(1);
    int bitwidth = Z->getType()->getIntegerBitWidth();

    propagateInstruction(hints, X, Z, Source);
    propagateInstruction(hints, Y, Z, Source);

    if (needsYCommutativity)
      applyCommutativity(hints, Z, Y, Source);
    if (needsZCommutativity)
      applyCommutativity(hints, Z, Z, Source);

    INFRULE(INSTPOS(SRC, Z), ConsAddXorAnd::make(
        REGISTER(*Z), VAL(A), VAL(B), REGISTER(*X),
        REGISTER(*Y), BITSIZE(bitwidth)));
    /*
        TyRegister::make(getVariable(*Z), Physical),
                            TyValue::make(*A), TyValue::make(*B),
                            TyRegister::make(getVariable(*X), Physical),
                            TyRegister::make(getVariable(*Y), Physical),
                            ConsSize::make(bitwidth))));
    */
  });
}

void generateHintForAddOrAnd(llvm::BinaryOperator *Z, llvm::BinaryOperator *X,
                             llvm::BinaryOperator *Y, llvm::Value *A,
                             llvm::Value *B, bool needsYCommutativity,
                             bool needsZCommutativity) {
  assert(ValidationUnit::Exists());

  INTRUDE(CAPTURE(&Z, &X, &Y, needsYCommutativity, needsZCommutativity), {
    //    <src>       <tgt>
    // X = A ^ B  | X = A ^ B
    // Y = A & B  | Y = A & B
    // Z = X + Y  | Z = A | B
    llvm::Value *A = X->getOperand(0);
    llvm::Value *B = X->getOperand(1);
    int bitwidth = Z->getType()->getIntegerBitWidth();

    propagateInstruction(hints, X, Z, Source);
    propagateInstruction(hints, Y, Z, Source);

    if (needsYCommutativity)
      applyCommutativity(hints, Z, Y, Source);
    if (needsZCommutativity)
      applyCommutativity(hints, Z, Z, Source);

    INFRULE(INSTPOS(Source, Z), ConsAddOrAnd::make(
        REGISTER(*Z), VAL(A), VAL(B), REGISTER(*X), REGISTER(*Y), BITSIZE(bitwidth)));
    /*
    hints.addCommand(ConsInfrule::make(
        TyPosition::make(Source, *Z),
        ConsAddOrAnd::make(TyRegister::make(getVariable(*Z), Physical),
                           TyValue::make(*A), TyValue::make(*B),
                           TyRegister::make(getVariable(*X), Physical),
                           TyRegister::make(getVariable(*Y), Physical),
                           ConsSize::make(bitwidth))));
    */
  });
}

void generateHintForAndOr(llvm::BinaryOperator *Z, llvm::Value *X,
                          llvm::BinaryOperator *Y, llvm::Value *A,
                          bool needsZCommutativity) {
  assert(ValidationUnit::Exists());

  INTRUDE(CAPTURE(&Z, &X, &Y, &A, needsZCommutativity), {
    assert(Z);

    propagateInstruction(hints, Y, Z, Source);
    if (Y->getOperand(0) != X)
      applyCommutativity(hints, Z, Y, Source);
    if (needsZCommutativity)
      applyCommutativity(hints, Z, Z, Source);
    INFRULE(INSTPOS(Source, Z), ConsAndOr::make(VAL(Z), VAL(X), VAL(Y), VAL(A), BITSIZE(*Z)));
    /*
    hints.addCommand(ConsInfrule::make(
        INSTPOS(Source, Z),
        ConsAndOr::make(VAL(Z, Physical), VAL(X, Physical), VAL(Y, Physical),
                        VAL(A, Physical),
                        ConsSize::make(Z->getType()->getIntegerBitWidth()))));
    */
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
    llvm::Value *X = nullptr;
    llvm::Value *A = nullptr;
    llvm::Value *B = nullptr;
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
      applyCommutativity(hints, Z, W, SRC);
      applyCommutativity(hints, Z, Y, SRC);
    } else if (W->getOperand(0) == Y->getOperand(1)) {
      X = W->getOperand(0);
      A = W->getOperand(1);
      B = Y->getOperand(0);
      applyCommutativity(hints, Z, W, SRC);
    } else if (W->getOperand(1) == Y->getOperand(0)) {
      X = W->getOperand(1);
      A = W->getOperand(0);
      B = Y->getOperand(1);
      applyCommutativity(hints, Z, Y, SRC);
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
    } else {
      assert(false && "icmp_eq_<bop> optimization : opcode should be one of "
                      "ADD, SUB, or XOR");
    }

    ValidationUnit::GetInstance()->setOptimizationName(optname);
    INFRULE(INSTPOS(SRC, Z), makeFunc(VAL(Z), VAL(W), VAL(X),
                    VAL(Y), VAL(A), VAL(B), BITSIZE(bitsize)));
  });
}

std::pair<std::shared_ptr<TyExpr>, std::shared_ptr<TyExpr> > false_encoding =
    std::make_pair(llvmberry::ConsConst::make(0, 64),
                   llvmberry::ConsConst::make(42, 64));

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

int getIndexofMem2Reg(llvm::Instruction* I,
                      int instrIndex, int termIndex) {
  if (I == nullptr)
    return instrIndex;

  if (llvm::dyn_cast<llvm::TerminatorInst>(I) != NULL)
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
        auto IPBPos = llvmberry::TyPosition::make(
            SRC, getBasicBlockIndex(I->getParent()),
            getBasicBlockIndex(PB));

        INFRULE(IPBPos,
                ConsTransitivity::make(VAR(OpPHIResolved_id),
                                       VAR(OpPHIResolved_id, Previous),
                                       VAR(OpPHI_id, Physical)));

        INFRULE(IPBPos,
                ConsTransitivity::make(VAR(OpPHI_id),
                                       VAR(OpPHIResolved_id, Previous),
                                       VAR(OpPHIResolved_id)));

        llvm::Instruction *I_evolving_next = (*I_evolving).clone();
        (*I_evolving_next).setOperand(i, OpPHIResolved);

        // SubstituteRev [ I_evolving_next >= I_evolving ]
        // I_evolving_next = I_evolving[OpPHI := OpPHIResolved]
        INFRULE(IPBPos, llvmberry::ConsSubstituteRev::make(
                    REGISTER(OpPHI_id), VAL(OpPHIResolved), INSN(*I_evolving)));
                    //        llvmberry::ConsInsn::make(*I_evolving)));

        // SubstituteRev [ I_evolving >= I_evolving_next ]
        // I_evolving = I_evolving_next[OpPHIResolved := OpPHI]
        INFRULE(IPBPos, llvmberry::ConsSubstituteRev::make(
                    REGISTER(OpPHIResolved_id), VAL(OpPHI), INSN(*I_evolving_next)));
                    //        llvmberry::ConsInsn::make(*I_evolving_next)));

        INFRULE(IPBPos, llvmberry::ConsTransitivity::make(
                    INSN(*I_evolving_next), INSN(*I_evolving), INSN(*I)));

        INFRULE(IPBPos, llvmberry::ConsTransitivity::make(
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
    int useIndex =
      getIndexofMem2Reg(use, std::get<2>(t),
                        DICTMAP(data.get<llvmberry::ArgForIndices>()->termIndices, useBB->getName()));

    if (use != nullptr && llvm::isa<llvm::PHINode>(use)) {
      llvm::PHINode *PHI = llvm::dyn_cast<llvm::PHINode>(use);

      for (unsigned i = 0; i != PHI->getNumIncomingValues(); ++i) {
        llvm::Value *v = llvm::dyn_cast<llvm::Value>(PHI->getIncomingValue(i));

        if (I == v) {
            const std::string &prev = PHI->getIncomingBlock(i)->getName();
            destSet->push_back(TyPosition::make(SRC, *use, useIndex, prev));
        }
      }
    } else {
      destSet->push_back(TyPosition::make(SRC, *useBB, useIndex));
    }
  }

  return destSet;
}

void saveInstrIndices(llvm::Function* F, Dictionary &data) {
  for (auto BS = F->begin(), BE = F->end(); BS != BE;) {
    llvm::BasicBlock* BB = BS++;

    data.get<llvmberry::ArgForIndices>()->termIndices.get()->insert
      (std::pair<llvm::StringRef, unsigned>(BB->getName(), getTerminatorIndex(BB->getTerminator())));

    for (auto IS = BB->begin(), IE = BB->end(); IS != IE;) {
      llvm::Instruction* I = IS++;

      data.get<llvmberry::ArgForIndices>()->instrIndices.get()->insert
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

          useIndices[I].push_back(std::make_tuple(useBB, use, DICTMAP(data.get<llvmberry::ArgForIndices>()->instrIndices, use)));
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
  auto &recentInstr = *(data.get<llvmberry::ArgForMem2Reg>()->recentInstr);

  PROPAGATE(LESSDEF(recentInstr[key].instrL, VAR(recentInstr[key].op1, Ghost), SRC), BOUNDS(from_position, to_position));

  std::shared_ptr<TyExpr> val = recentInstr[key].instrR;
  
  PROPAGATE(LESSDEF(VAR(recentInstr[key].op1, Ghost), val, TGT), BOUNDS(from_position, to_position));

  std::string op0 = recentInstr[key].op0;
  if(op0 == "llvmberry::PHI" || op0.substr(0,op0.rfind(".")) != op0)
    data.get<llvmberry::ArgForMem2Reg>()->replaceItem.get()->push_back(std::shared_ptr<TyExpr>(val));

  // Infrule function
  // if from position is SI or AI, apply infrule  
  // if from position is PHI, no need to apply infrule  
  // if store operand changed before, then infrule already applied 
  if ((recentInstr[key].op0 != "llvmberry::PHI") && (!recentInstr[key].check))
    INFRULE(recentInstr[key].instrPos, ConsIntroGhost::make(recentInstr[key].instrR, REGISTER(recentInstr[key].op1, Ghost)));
}

void propagateFromInsnToLoad(unsigned key, llvm::LoadInst *LI, Dictionary &data, CoreHint &hints) {
  auto &recentInstr = *(data.get<llvmberry::ArgForMem2Reg>()->recentInstr);
  std::shared_ptr<TyPosition> from_position = NULL;

  if (recentInstr[key].op0 == "llvmberry::PHI")
    from_position = STARTPOS(SRC, recentInstr[key].instrBB->getName());
  else
    from_position = recentInstr[key].instrPos;

  std::shared_ptr<TyPosition> to_position = INDEXEDPOS(SRC, LI, DICTMAP(data.get<llvmberry::ArgForIndices>()->instrIndices, LI), "");

  propagateFromToUsingKey(key, from_position, to_position, data, hints);
}

void propagateFromInsnToPhi(unsigned key, llvm::PHINode *Phi, llvm::BasicBlock* prev, Dictionary &data, CoreHint &hints) {
  auto &recentInstr = *(data.get<llvmberry::ArgForMem2Reg>()->recentInstr);
  std::shared_ptr<TyPosition> from_position = NULL;

  if (recentInstr[key].op0 == "llvmberry::PHI")
    from_position = STARTPOS(SRC, recentInstr[key].instrBB->getName());
  else
    from_position = recentInstr[key].instrPos;

  std::shared_ptr<TyPosition> to_position = ENDPOSINDEXED(SRC, prev, DICTMAP(data.get<llvmberry::ArgForIndices>()->termIndices, prev->getName()));

  propagateFromToUsingKey(key, from_position, to_position, data, hints);

  recentInstr[key].check = false; 
}

void checkSIOperand(unsigned key, llvm::StoreInst *SI, Dictionary &data, CoreHint &hints) {
  auto &recentInstr = *(data.get<llvmberry::ArgForMem2Reg>()->recentInstr);
  auto &storeItem = *(data.get<llvmberry::ArgForMem2Reg>()->storeItem);

  std::shared_ptr<TyPosition> to_position = NULL;
  to_position = INDEXEDPOS(SRC, SI, DICTMAP(data.get<llvmberry::ArgForIndices>()->instrIndices, SI), "");

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
  auto &instrIndices = *(data.get<llvmberry::ArgForIndices>()->instrIndices);
  std::shared_ptr<std::vector<std::shared_ptr<llvmberry::TyPosition>>> useSet = saveUseSet(LI, data);
  std::string Rload = llvmberry::getVariable(*LI);

  //propagate LI to use set
  PROPAGATE(LESSDEF(VAR(Rload, Physical), VAR(Rload, Ghost), SRC), BOUNDSET(INDEXEDPOS(SRC, LI, instrIndices[LI], ""), useSet));

  std::shared_ptr<TyExpr> val = TyExpr::make(*V, Physical);
  PROPAGATE(LESSDEF(VAR(Rload, Ghost), val, TGT), BOUNDSET(INDEXEDPOS(SRC, LI, instrIndices[LI], ""), useSet));

  //infrule at LI index
  INFRULE(INDEXEDPOS(SRC, LI, instrIndices[LI], ""), ConsIntroGhost::make(VAR(In, Ghost), REGISTER(Rload, Ghost)));

  if (checkReplace && !llvm::isa<llvm::Constant>(V))
    data.get<llvmberry::ArgForMem2Reg>()->replaceItem.get()->push_back(std::shared_ptr<TyExpr>(val));
}

void propagateLoadGhostValueFromAIToLI(llvm::AllocaInst* AI, llvm::LoadInst* LI, llvm::Value* value, Dictionary &data, CoreHint &hints) {
  std::string Rghost = getVariable(*AI);
  auto replaceItem = data.get<llvmberry::ArgForMem2Reg>()->replaceItem;
  auto instrIndices = data.get<llvmberry::ArgForIndices>()->instrIndices;

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
  auto ArgForMem2Reg = data.get<llvmberry::ArgForMem2Reg>();
  auto instrIndices = data.get<llvmberry::ArgForIndices>()->instrIndices;
  
  std::shared_ptr<TyPosition> from_position = INDEXEDPOS(SRC, SI, DICTMAP(instrIndices, SI), "");
  std::shared_ptr<TyPosition> to_position = INDEXEDPOS(SRC, LI, DICTMAP(instrIndices, LI), "");

  PROPAGATE(LESSDEF(INSNALIGNONE(SI), VAR(Rghost, Ghost), SRC), BOUNDS(from_position, to_position));

  std::shared_ptr<TyExpr> val = EXPR(value, Physical);
  PROPAGATE(LESSDEF(VAR(Rghost, Ghost), val, TGT), BOUNDS(from_position, to_position));

  if (value->getName() != "") { ArgForMem2Reg->replaceItem.get()->push_back(std::shared_ptr<TyExpr>(val)); }
  std::shared_ptr<llvmberry::TyExpr> expr = EXPR(value, Physical);
  
  if (checkReplace) {
    auto &storeItem = *(ArgForMem2Reg->storeItem);
    if (equalsIfConsVar(storeItem[SI].expr, expr)) { ArgForMem2Reg->replaceTag.get()->push_back(expr); }
    else if (storeItem[SI].op0 != "") { expr = VAR(storeItem[SI].op0, Ghost); }
  }

  INFRULE(from_position, llvmberry::ConsIntroGhost::make(expr, REGISTER(Rghost, Ghost)));
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
  auto &replaceItem = *(data.get<llvmberry::ArgForMem2Reg>()->replaceItem);
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
  auto &replaceTag = *(data.get<llvmberry::ArgForMem2Reg>()->replaceTag);
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

} // llvmberry
