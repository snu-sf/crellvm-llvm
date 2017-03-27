#include "llvm/LLVMBerry/ValidationUnit.h"
#include "llvm/LLVMBerry/Dictionary.h"
#include "llvm/LLVMBerry/Hintgen.h"
#include "llvm/LLVMBerry/Infrules.h"
#include "llvm/LLVMBerry/InstCombine/InfrulesAddSub.h"
#include "llvm/LLVMBerry/InstCombine/InfrulesAndOrXor.h"
#include "llvm/LLVMBerry/InstCombine/InfrulesCompares.h"
#include "llvm/LLVMBerry/InstCombine/InfrulesSelect.h"
#include "llvm/Support/Debug.h"

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
void propagateInstruction(llvm::Instruction *from, llvm::Instruction *to,
                          TyScope scope, bool propagateEquivalence) {
  assert(ValidationUnit::Exists());
  ValidationUnit::GetInstance()->intrude([&from, &to, &scope,
                                          &propagateEquivalence](
      ValidationUnit::Dictionary &data, CoreHint &hints) {
    std::string reg_name = getVariable(*from);

    if (scope == Source) {
      hints.addCommand(ConsPropagate::make(
          ConsLessdef::make(ConsVar::make(reg_name, Physical),
                            ConsRhs::make(reg_name, Physical, scope), scope),
          ConsBounds::make(TyPosition::make(scope, *from),
                           TyPosition::make(scope, *to))));
      if (propagateEquivalence) {
        hints.addCommand(ConsPropagate::make(
            ConsLessdef::make(ConsRhs::make(reg_name, Physical, scope),
                              ConsVar::make(reg_name, Physical), scope),
            ConsBounds::make(TyPosition::make(scope, *from),
                             TyPosition::make(scope, *to))));
      }
    } else if (scope == Target) {
      hints.addCommand(ConsPropagate::make(
          ConsLessdef::make(ConsRhs::make(reg_name, Physical, scope),
                            ConsVar::make(reg_name, Physical), scope),
          ConsBounds::make(TyPosition::make(scope, *from),
                           TyPosition::make(scope, *to))));
      if (propagateEquivalence) {
        hints.addCommand(ConsPropagate::make(
            ConsLessdef::make(ConsVar::make(reg_name, Physical),
                              ConsRhs::make(reg_name, Physical, scope), scope),
            ConsBounds::make(TyPosition::make(scope, *from),
                             TyPosition::make(scope, *to))));
      }
    } else {
      assert("propagateInstruction() : scope is neither Source nor Target" &&
             false);
    }
  });
}

void propagateLessdef(llvm::Instruction *from, llvm::Instruction *to,
                      const llvm::Value *lesserval,
                      const llvm::Value *greaterval, TyScope scope) {
  assert(ValidationUnit::Exists());
  ValidationUnit::GetInstance()->intrude(
      [&from, &to, &lesserval, &greaterval,
       &scope](ValidationUnit::Dictionary &data, CoreHint &hints) {
        hints.addCommand(ConsPropagate::make(
            ConsLessdef::make(TyExpr::make(*greaterval),
                              TyExpr::make(*lesserval), scope),
            ConsBounds::make(TyPosition::make(scope, *from),
                             TyPosition::make(scope, *to))));
      });
}

void applyCommutativity(llvm::Instruction *position,
                        llvm::BinaryOperator *expression, TyScope scope) {
  assert(ValidationUnit::Exists());

  ValidationUnit::GetInstance()->intrude([&position, &expression, &scope](
      ValidationUnit::Dictionary &data, CoreHint &hints) {
    int bitwidth = isFloatOpcode(expression->getOpcode())
                       ? -1
                       : expression->getType()->getIntegerBitWidth();
    std::string regname = getVariable(*expression);
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
        hints.addCommand(ConsInfrule::make(
            TyPosition::make(Source, *position),
            ConsFbopCommutative::make(VAR(regname, Physical), fbop,
                                      TyValue::make(*expression->getOperand(0)),
                                      TyValue::make(*expression->getOperand(1)),
                                      getFloatType(expression->getType()))));
      } else {
        hints.addCommand(ConsInfrule::make(
            TyPosition::make(Source, *position),
            ConsBopCommutative::make(VAR(regname, Physical), bop,
                                     TyValue::make(*expression->getOperand(0)),
                                     TyValue::make(*expression->getOperand(1)),
                                     ConsSize::make(bitwidth))));
      }
    } else if (scope == Target) {
      switch (expression->getOpcode()) {
      case llvm::Instruction::Add:
        hints.addCommand(
            ConsInfrule::make(TyPosition::make(Target, *position),
                              ConsAddCommutativeTgt::make(
                                  TyRegister::make(regname, Physical),
                                  TyValue::make(*expression->getOperand(0)),
                                  TyValue::make(*expression->getOperand(1)),
                                  ConsSize::make(bitwidth))));
        break;
      case llvm::Instruction::Xor:
        hints.addCommand(
            ConsInfrule::make(TyPosition::make(Target, *position),
                              ConsXorCommutativeTgt::make(
                                  TyRegister::make(regname, Physical),
                                  TyValue::make(*expression->getOperand(0)),
                                  TyValue::make(*expression->getOperand(1)),
                                  ConsSize::make(bitwidth))));
        break;
      case llvm::Instruction::FAdd:
        hints.addCommand(
            ConsInfrule::make(TyPosition::make(Target, *position),
                              ConsFaddCommutativeTgt::make(
                                  TyRegister::make(regname, Physical),
                                  TyValue::make(*expression->getOperand(0)),
                                  TyValue::make(*expression->getOperand(1)),
                                  getFloatType(expression->getType()))));
        break;
      case llvm::Instruction::FMul:
        INFRULE(INSTPOS(Target, position), ConsFmulCommutativeTgt::make(
                REGISTER(regname, Physical),
                VAL(expression->getOperand(0), Physical),
                VAL(expression->getOperand(1), Physical),
                getFloatType(expression->getType())));
        break;
      case llvm::Instruction::Or:
        INFRULE(
            INSTPOS(llvmberry::Target, position),
            ConsOrCommutativeTgt::make(REGISTER(regname, Physical),
                                    VAL(expression->getOperand(0), Physical),
                                    VAL(expression->getOperand(1), Physical),
                                    BITSIZE(bitwidth)));
        break;
      default:
        assert("applyCommutativity() : we don't support commutativity rule for "
               "this binary operator");
      }
    } else {
      assert("applyCommutativity() : scope is neither Source nor Target" &&
             false);
    }
  });
}

void applyTransitivity(llvm::Instruction *position, llvm::Value *v_greatest,
                       llvm::Value *v_mid, llvm::Value *v_smallest,
                       TyScope scope) {
  applyTransitivity(position, v_greatest, v_mid, v_smallest, scope, scope);
}

void applyTransitivity(llvm::Instruction *position, llvm::Value *v_greatest,
                       llvm::Value *v_mid, llvm::Value *v_smallest,
                       TyScope scope, TyScope position_scopetag) {
  assert(ValidationUnit::Exists());

  ValidationUnit::GetInstance()->intrude(
      [&position, &v_smallest, &v_mid, &v_greatest, &scope, &position_scopetag](
          ValidationUnit::Dictionary &data, CoreHint &hints) {
        INFRULE(INSTPOS(position_scopetag, position),
                ConsTransitivity::make(TyExpr::make(*v_greatest, Physical),
                                       TyExpr::make(*v_mid, Physical),
                                       TyExpr::make(*v_smallest, Physical)));
      });
}

void propagateMaydiffGlobal(std::string varname, TyTag tag) {
  assert(ValidationUnit::Exists());

  ValidationUnit::GetInstance()->intrude(
      [&varname, &tag](Dictionary &data, CoreHint &hints) {
        hints.addCommand(ConsPropagate::make(ConsMaydiff::make(varname, tag),
                                             ConsGlobal::make()));
      });
}

void generateHintForNegValue(llvm::Value *V, llvm::BinaryOperator &I,
                             TyScope scope) {
  assert(ValidationUnit::Exists());

  if (llvm::BinaryOperator::isNeg(V)) {
    ValidationUnit::GetInstance()->intrude([&V, &I, &scope](
        ValidationUnit::Dictionary &data, CoreHint &hints) {

      std::string reg0_name = getVariable(I);  // z = x -my
      std::string reg1_name = getVariable(*V); // my

      llvm::Instruction *Vins = llvm::dyn_cast<llvm::Instruction>(V);

      hints.addCommand(ConsPropagate::make(
          ConsLessdef::make(ConsVar::make(reg1_name, Physical), // my = -y
                            ConsRhs::make(reg1_name, Physical, scope), scope),
          ConsBounds::make(
              TyPosition::make(scope, *Vins), // From my to z = x -my
              TyPosition::make(scope, I))));

      hints.addCommand(ConsPropagate::make(
          ConsLessdef::make(ConsRhs::make(reg1_name, Physical, scope),
                            ConsVar::make(reg1_name, Physical), // my = -y
                            scope),
          ConsBounds::make(
              TyPosition::make(scope, *Vins), // From my to z = x -my
              TyPosition::make(scope, I))));
    });
  }
  // Constants can be considered to be negated values if they can be folded.
  if (llvm::ConstantInt *C = llvm::dyn_cast<llvm::ConstantInt>(V)) {
    ValidationUnit::GetInstance()->intrude([&I, &V, &C, &scope](
        ValidationUnit::Dictionary &data, CoreHint &hints) {

      std::string reg0_name = getVariable(I); // z = x -my

      unsigned sz_bw = I.getType()->getPrimitiveSizeInBits();
      int64_t c1 = C->getSExtValue();
      int64_t c2 = -c1;

      hints.addCommand(
          ConsInfrule::make(TyPosition::make(scope, I),
                            ConsNegVal::make(TyConstInt::make(c1, sz_bw),
                                             TyConstInt::make(c2, sz_bw),
                                             ConsSize::make(sz_bw))));

      hints.addCommand(
          ConsInfrule::make(TyPosition::make(scope, I),
                            ConsNegVal::make(TyConstInt::make(c1, sz_bw),
                                             TyConstInt::make(c2, sz_bw),
                                             ConsSize::make(sz_bw))));
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
                                       std::shared_ptr<TyPosition> source_pos) {
  assert(ValidationUnit::Exists());
  if (!source_pos) {
    source_pos = INSTPOS(SRC, source);
  }

  ValidationUnit::GetInstance()
      ->intrude([&source, &replaceTo, &ghostvar, &source_pos](
            ValidationUnit::Dictionary &data, CoreHint &hints) {
    llvm::Instruction *I = source;

    std::string to_rem = getVariable(*I);

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

      if (ghostvar == "") {
        PROPAGATE(
            LESSDEF(VAR(to_rem, Physical), EXPR(replaceTo, Physical), SRC),
            BOUNDS(source_pos,
                   TyPosition::make(SRC, *user_I, prev_block_name)));
        if (llvm::isa<llvm::PHINode>(user_I)) {
          INFRULE(TyPosition::make(SRC, *user_I, prev_block_name),
                  ConsTransitivity::make(VAR(user, Physical),
                                         VAR(to_rem, Previous),
                                         EXPR(replaceTo, Previous)));
        } else if (!user.empty() && !llvm::isa<llvm::CallInst>(user_I)) {
          INFRULE(TyPosition::make(Source, *user_I, prev_block_name),
                  ConsReplaceRhs::make(
                      REGISTER(to_rem, Physical), VAL(replaceTo, Physical),
                      VAR(user, Physical), RHS(user, Physical, Source),
                      RHS(user, Physical, Target)));
        }
      } else {
        PROPAGATE(LESSDEF(VAR(to_rem, Physical), VAR(ghostvar, Ghost), SRC),
                  BOUNDS(source_pos,
                         TyPosition::make(SRC, *user_I, prev_block_name)));
        PROPAGATE(LESSDEF(VAR(ghostvar, Ghost), EXPR(replaceTo, Physical), TGT),
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
                  ConsTransitivity::make(VAR(user, Physical),
                                         VAR(to_rem, Previous),
                                         VAR(to_rem, Physical)));
          INFRULE(TyPosition::make(SRC, *user_I, prev_block_name),
                  ConsTransitivity::make(VAR(user, Physical),
                                         VAR(to_rem, Physical),
                                         VAR(ghostvar, Ghost)));
          INFRULE(TyPosition::make(TGT, *user_I, prev_block_name),
                  ConsTransitivityTgt::make(EXPR(replaceTo, Physical),
                                            EXPR(replaceTo, Previous),
                                            VAR(user, Physical)));
          INFRULE(TyPosition::make(TGT, *user_I, prev_block_name),
                  ConsTransitivityTgt::make(VAR(ghostvar, Ghost),
                                            EXPR(replaceTo, Physical),
                                            VAR(user, Physical)));
        } else if (!user.empty() && !llvm::isa<llvm::CallInst>(user_I)) {
          INFRULE(TyPosition::make(SRC, *user_I, prev_block_name),
                  ConsSubstitute::make(REGISTER(to_rem, Physical),
                                       ID(ghostvar, Ghost), INSN(*user_I)));
          llvm::Instruction *user_I_copy = user_I->clone();

          for (unsigned i = 0; i < user_I_copy->getNumOperands(); i++) {
            if (user_I->getOperand(i) == source)
              user_I_copy->setOperand(i, replaceTo);
          }
          INFRULE(TyPosition::make(SRC, *user_I, prev_block_name),
                  ConsTransitivity::make(EXPR(user_I, Physical), INSN(*user_I),
                                         INSN(*user_I_copy)));
          delete user_I_copy;
        }
      }
    }
        });
}

void generateHintForReplaceAllUsesWithAtTgt(llvm::Instruction *source,
                                            llvm::Value *replaceTo) {
  assert(ValidationUnit::Exists());

  ValidationUnit::GetInstance()->intrude([&source, &replaceTo](
      Dictionary &data, CoreHint &hints) {
    llvm::Instruction *I = source;
    auto I_pos = INSTPOS(TGT, I);

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

      PROPAGATE(LESSDEF(VAR(I_var, Physical), EXPR(replaceTo, Physical), TGT),
                BOUNDS(I_pos, TyPosition::make(TGT, *user_I, prev_block_name)));
      if (llvm::isa<llvm::PHINode>(user_I)) {
        INFRULE(TyPosition::make(TGT, *user_I, prev_block_name),
                ConsTransitivityTgt::make(VAR(I_var, Previous),
                                          EXPR(replaceTo, Previous),
                                          VAR(user, Physical)));
      } else if (!user.empty() && !llvm::isa<llvm::CallInst>(user_I)) {
        llvm::Instruction *user_I_copy = user_I->clone();
        INFRULE(TyPosition::make(TGT, *user_I, prev_block_name),
                ConsSubstituteTgt::make(REGISTER(I_var, Physical),
                                        VAL(replaceTo, Physical),
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

  ValidationUnit::GetInstance()->intrude([&Z, &X, &Y, needs_commutativity,
                                          is_leftform](
      ValidationUnit::Dictionary &data, CoreHint &hints) {
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
    hints.addCommand(ConsPropagate::make(
        ConsLessdef::make(ConsVar::make(reg_x_name, Physical),
                          ConsRhs::make(reg_x_name, Physical, Source), Source),
        ConsBounds::make(TyPosition::make(Source, *X),
                         TyPosition::make(Source, *Z))));

    // Propagate "Y = select c ? x : 0" or "Y = select c ? 0 : x"
    hints.addCommand(ConsPropagate::make(
        ConsLessdef::make(ConsVar::make(reg_y_name, Physical),
                          ConsRhs::make(reg_y_name, Physical, Source), Source),
        ConsBounds::make(TyPosition::make(Source, *Y),
                         TyPosition::make(Source, *Z))));

    if (needs_commutativity) {
      hints.addCommand(ConsInfrule::make(
          TyPosition::make(Source, *Z),
          ConsBopCommutative::make(VAR(reg_z_name, Physical), TyBop::BopAdd,
                                   TyValue::make(*Y), TyValue::make(*a_Z),
                                   ConsSize::make(bitwidth))));
    }

    if (is_leftform) {
      hints.addCommand(ConsInfrule::make(
          TyPosition::make(Source, *Z),
          ConsAddSelectZero::make(
              TyRegister::make(reg_z_name, Physical),
              TyRegister::make(reg_x_name, Physical),
              TyRegister::make(reg_y_name, Physical), TyValue::make(*c),
              TyValue::make(*n), TyValue::make(*a), ConsSize::make(bitwidth))));
    } else {
      hints.addCommand(ConsInfrule::make(
          TyPosition::make(Source, *Z),
          ConsAddSelectZero2::make(
              TyRegister::make(reg_z_name, Physical),
              TyRegister::make(reg_x_name, Physical),
              TyRegister::make(reg_y_name, Physical), TyValue::make(*c),
              TyValue::make(*n), TyValue::make(*a), ConsSize::make(bitwidth))));
    }
  });
}

void generateHintForOrAnd(llvm::BinaryOperator *Y, llvm::Value *X,
                          llvm::Value *A) {
  assert(ValidationUnit::Exists());

  ValidationUnit::GetInstance()->intrude(
      [Y, X, A](Dictionary &data, CoreHint &hints) {
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
          propagateInstruction(Y, Z, SRC);
          if (Zswapped ^ isSwapped)
            applyCommutativity(Z, Z, SRC);
          if (Y->getOperand(0) == A)
            applyCommutativity(Z, Y, SRC);
          INFRULE(INSTPOS(SRC, Z),
                  ConsOrAnd::make(VAL(Z, Physical), VAL(Y, Physical),
                                  VAL(X, Physical), VAL(A, Physical),
                                  BITSIZE(Z->getType()->getIntegerBitWidth())));
        });
      });
}

void generateHintForOrXor(llvm::BinaryOperator *W, llvm::Value *op0,
                          llvm::Value *op1, bool needsCommutativity) {
  assert(ValidationUnit::Exists());

  ValidationUnit::GetInstance()->intrude([&W, &op0, &op1, &needsCommutativity](
      ValidationUnit::Dictionary &data, CoreHint &hints) {
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

    propagateInstruction(X, W, Source);
    propagateInstruction(Y, W, Source);
    propagateInstruction(Z, W, Source);
    if (X->getOperand(1) == B) {
      applyCommutativity(W, X, Source);
    }

    if (needsCommutativity) {
      applyCommutativity(W, W, Source);
    }

    hints.addCommand(ConsInfrule::make(
        TyPosition::make(Source, *W),
        ConsOrXor::make(TyValue::make(*W), TyValue::make(*Z), TyValue::make(*X),
                        TyValue::make(*Y), TyValue::make(*A), TyValue::make(*B),
                        ConsSize::make(bitwidth))));

  });
}

void generateHintForOrXor2(llvm::BinaryOperator *Z, llvm::Value *X1_val,
                           llvm::Value *X2_val, llvm::Value *A, llvm::Value *B,
                           bool needsY1Commutativity,
                           bool needsY2Commutativity) {
  assert(ValidationUnit::Exists());

  ValidationUnit::GetInstance()->intrude(
      [&Z, &X1_val, &X2_val, &A, &B, &needsY1Commutativity,
       &needsY2Commutativity](Dictionary &data, CoreHint &hints) {
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

        propagateInstruction(X1, Z, Source);
        propagateInstruction(X2, Z, Source);
        propagateInstruction(Y1, Z, Source);
        propagateInstruction(Y2, Z, Source);
        if (X1->getOperand(1) == B)
          applyCommutativity(Z, X1, Source);
        if (X2->getOperand(1) == A)
          applyCommutativity(Z, X2, Source);
        if (needsY1Commutativity)
          applyCommutativity(Z, Y1, Source);
        if (needsY2Commutativity)
          applyCommutativity(Z, Y2, Source);

        hints.addCommand(ConsInfrule::make(
            TyPosition::make(Target, *Z),
            ConsOrXor2::make(TyValue::make(*Z), TyValue::make(*X1),
                             TyValue::make(*Y1), TyValue::make(*X2),
                             TyValue::make(*Y2), TyValue::make(*A),
                             TyValue::make(*B), ConsSize::make(bitwidth))));

      });
}

void generateHintForOrXor4(llvm::BinaryOperator *Z, llvm::Value *X,
                           llvm::BinaryOperator *Y, llvm::BinaryOperator *A,
                           llvm::Value *B, llvm::BinaryOperator *NB,
                           bool needsYCommutativity, bool needsZCommutativity) {
  assert(ValidationUnit::Exists());

  ValidationUnit::GetInstance()->intrude(
      [&Z, &X, &Y, &A, &B, &NB, needsYCommutativity,
       needsZCommutativity](Dictionary &data, CoreHint &hints) {
        // <src>      |  <tgt>
        // A = X ^ -1 | A = X ^ -1
        // Y = A ^ B  | Y = A ^ B
        // <nop>      | NB = B ^ -1
        // Z = X | Y  | Z = NB | X
        int bitwidth = Z->getType()->getIntegerBitWidth();
        propagateInstruction(A, Z, Target);
        propagateInstruction(Y, Z, Target);
        propagateInstruction(NB, Z, Target);
        if (needsYCommutativity)
          applyCommutativity(Z, Y, Target);
        insertSrcNopAtTgtI(hints, NB);
        propagateMaydiffGlobal(getVariable(*NB), Physical);

        INFRULE(INSTPOS(TGT, Z),
                ConsOrXor4::make(VAL(Z, Physical), VAL(X, Physical),
                                 VAL(Y, Physical), VAL(A, Physical),
                                 VAL(B, Physical), VAL(NB, Physical),
                                 BITSIZE(bitwidth)));
        if (needsZCommutativity)
          INFRULE(INSTPOS(TGT, Z),
                  ConsOrCommutativeTgt::make(
                      REGISTER(getVariable(*Z), Physical),
                      VAL(X, Physical), VAL(Y, Physical), BITSIZE(bitwidth)));
      });
}

void generateHintForAddXorAnd(llvm::BinaryOperator *Z, llvm::BinaryOperator *X,
                              llvm::BinaryOperator *Y, llvm::Value *A,
                              llvm::Value *B, bool needsYCommutativity,
                              bool needsZCommutativity) {
  ValidationUnit::GetInstance()->intrude(
      [&Z, &X, &Y, &needsYCommutativity, &needsZCommutativity](
          ValidationUnit::Dictionary &data, CoreHint &hints) {
        //    <src>       <tgt>
        // X = A ^ B  | X = A ^ B
        // Y = A & B  | Y = A & B
        // Z = X + Y  | Z = A | B
        llvm::Value *A = X->getOperand(0);
        llvm::Value *B = X->getOperand(1);
        int bitwidth = Z->getType()->getIntegerBitWidth();

        propagateInstruction(X, Z, Source);
        propagateInstruction(Y, Z, Source);

        if (needsYCommutativity)
          applyCommutativity(Z, Y, Source);
        if (needsZCommutativity)
          applyCommutativity(Z, Z, Source);

        hints.addCommand(ConsInfrule::make(
            TyPosition::make(Source, *Z),
            ConsAddXorAnd::make(TyRegister::make(getVariable(*Z), Physical),
                                TyValue::make(*A), TyValue::make(*B),
                                TyRegister::make(getVariable(*X), Physical),
                                TyRegister::make(getVariable(*Y), Physical),
                                ConsSize::make(bitwidth))));
      });
}

void generateHintForAddOrAnd(llvm::BinaryOperator *Z, llvm::BinaryOperator *X,
                             llvm::BinaryOperator *Y, llvm::Value *A,
                             llvm::Value *B, bool needsYCommutativity,
                             bool needsZCommutativity) {
  assert(ValidationUnit::Exists());

  ValidationUnit::GetInstance()->intrude(
      [&Z, &X, &Y, &needsYCommutativity, &needsZCommutativity](
          ValidationUnit::Dictionary &data, CoreHint &hints) {
        //    <src>       <tgt>
        // X = A ^ B  | X = A ^ B
        // Y = A & B  | Y = A & B
        // Z = X + Y  | Z = A | B
        llvm::Value *A = X->getOperand(0);
        llvm::Value *B = X->getOperand(1);
        int bitwidth = Z->getType()->getIntegerBitWidth();

        propagateInstruction(X, Z, Source);
        propagateInstruction(Y, Z, Source);

        if (needsYCommutativity)
          applyCommutativity(Z, Y, Source);
        if (needsZCommutativity)
          applyCommutativity(Z, Z, Source);

        hints.addCommand(ConsInfrule::make(
            TyPosition::make(Source, *Z),
            ConsAddOrAnd::make(TyRegister::make(getVariable(*Z), Physical),
                               TyValue::make(*A), TyValue::make(*B),
                               TyRegister::make(getVariable(*X), Physical),
                               TyRegister::make(getVariable(*Y), Physical),
                               ConsSize::make(bitwidth))));
      });
}

void generateHintForAndOr(llvm::BinaryOperator *Z, llvm::Value *X,
                          llvm::BinaryOperator *Y, llvm::Value *A,
                          bool needsZCommutativity) {
  assert(ValidationUnit::Exists());

  ValidationUnit::GetInstance()->intrude([&Z, &X, &Y, &A, &needsZCommutativity](
      ValidationUnit::Dictionary &data, CoreHint &hints) {
    assert(Z);

    propagateInstruction(Y, Z, Source);
    if (Y->getOperand(0) != X)
      applyCommutativity(Z, Y, Source);
    if (needsZCommutativity)
      applyCommutativity(Z, Z, Source);
    hints.addCommand(ConsInfrule::make(
        INSTPOS(Source, Z),
        ConsAndOr::make(VAL(Z, Physical), VAL(X, Physical), VAL(Y, Physical),
                        VAL(A, Physical),
                        ConsSize::make(Z->getType()->getIntegerBitWidth()))));
  });
}

void generateHintForIcmpEqNeBopBop(llvm::ICmpInst *Z, llvm::BinaryOperator *W,
                                   llvm::BinaryOperator *Y) {
  assert(ValidationUnit::Exists());

  ValidationUnit::GetInstance()->intrude([&Z, &W, &Y](Dictionary &data,
                                                      CoreHint &hints) {
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
    propagateInstruction(W, Z, llvmberry::Source);
    propagateInstruction(Y, Z, llvmberry::Source);
    if (W->getOperand(1) == Y->getOperand(1)) {
      X = W->getOperand(1);
      A = W->getOperand(0);
      B = Y->getOperand(0);
    } else if (W->getOperand(0) == Y->getOperand(0)) {
      X = W->getOperand(0);
      A = W->getOperand(1);
      B = Y->getOperand(1);
      applyCommutativity(Z, W, llvmberry::Source);
      applyCommutativity(Z, Y, llvmberry::Source);
    } else if (W->getOperand(0) == Y->getOperand(1)) {
      X = W->getOperand(0);
      A = W->getOperand(1);
      B = Y->getOperand(0);
      applyCommutativity(Z, W, llvmberry::Source);
    } else if (W->getOperand(1) == Y->getOperand(0)) {
      X = W->getOperand(1);
      A = W->getOperand(0);
      B = Y->getOperand(1);
      applyCommutativity(Z, Y, llvmberry::Source);
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
    INFRULE(INSTPOS(SRC, Z),
            makeFunc(VAL(Z, Physical), VAL(W, Physical), VAL(X, Physical),
                     VAL(Y, Physical), VAL(A, Physical), VAL(B, Physical),
                     BITSIZE(bitsize)));
  });
}

std::pair<std::shared_ptr<TyExpr>, std::shared_ptr<TyExpr> > false_encoding =
    std::make_pair(llvmberry::ConsConst::make(0, 64),
                   llvmberry::ConsConst::make(42, 64));

void generateHintForDCE(CoreHint &hints, llvm::Instruction &I) {
  std::string reg = getVariable(I);

  hints.addCommand(ConsPropagate::make(
      ConsMaydiff::make(reg, Physical),
      ConsGlobal::make()));

  insertTgtNopAtSrcI(hints, &I);
}

void generateHintForTrivialDCE(llvm::Instruction &I) {
  assert(ValidationUnit::Exists());
  ValidationUnit::GetInstance()->intrude([&I](
      ValidationUnit::Dictionary &data, CoreHint &hints) {
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
  ValidationUnit::GetInstance()->intrude([&I](
      ValidationUnit::Dictionary &data, CoreHint &hints) {
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
  ValidationUnit::GetInstance()->intrude([&I, &PB, &scope](Dictionary &data,
                                                           CoreHint &hints) {
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
                    ConsTransitivity::make(VAR(OpPHIResolved_id, Physical),
                                           VAR(OpPHIResolved_id, Previous),
                                           VAR(OpPHI_id, Physical)));

            INFRULE(IPBPos,
                    ConsTransitivity::make(VAR(OpPHI_id, Physical),
                                           VAR(OpPHIResolved_id, Previous),
                                           VAR(OpPHIResolved_id, Physical)));

            llvm::Instruction *I_evolving_next = (*I_evolving).clone();
            (*I_evolving_next).setOperand(i, OpPHIResolved);

            // SubstituteRev [ I_evolving_next >= I_evolving ]
            // I_evolving_next = I_evolving[OpPHI := OpPHIResolved]
            INFRULE(IPBPos, llvmberry::ConsSubstituteRev::make(
                                REGISTER(OpPHI_id, Physical),
                                VAL(OpPHIResolved, Physical),
                                llvmberry::ConsInsn::make(*I_evolving)));

            // SubstituteRev [ I_evolving >= I_evolving_next ]
            // I_evolving = I_evolving_next[OpPHIResolved := OpPHI]
            INFRULE(IPBPos, llvmberry::ConsSubstituteRev::make(
                                REGISTER(OpPHIResolved_id, Physical),
                                VAL(OpPHI, Physical),
                                llvmberry::ConsInsn::make(*I_evolving_next)));

            INFRULE(IPBPos,
                    llvmberry::ConsTransitivity::make(
                        INSN(*I_evolving_next), INSN(*I_evolving), INSN(*I)));

            INFRULE(IPBPos,
                    llvmberry::ConsTransitivity::make(
                        INSN(*I), INSN(*I_evolving), INSN(*I_evolving_next)));

            delete I_evolving;
            I_evolving = I_evolving_next;
          }
        }
        delete I_evolving;
  });
}

std::shared_ptr<std::vector<std::shared_ptr<TyPosition>>> saveDestSet
                                      (llvm::Instruction* I, Dictionary &data) {
  auto &termIndices = *(data.get<ArgForIndices>()->termIndices);
  auto &useIndices = *(data.get<ArgForIndices>()->useIndices);
  std::shared_ptr<std::vector<std::shared_ptr<TyPosition>>>
                  destSet(new std::vector<std::shared_ptr<TyPosition>>());

  for (auto UI = useIndices[I].begin(), E = useIndices[I].end(); UI != E;) {
    auto t = *(UI++);
    llvm::BasicBlock* useBB = std::get<0>(t);
    llvm::Instruction* use = std::get<1>(t);
    int useIndex =
      getIndexofMem2Reg(use, std::get<2>(t),
                        termIndices[getBasicBlockIndex(useBB)]);

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
  auto &instrIndices = *(data.get<ArgForIndices>()->instrIndices);
  auto &termIndices = *(data.get<ArgForIndices>()->termIndices);

  for (auto BS = F->begin(), BE = F->end(); BS != BE;) {
    llvm::BasicBlock* BB = BS++;
    std::string blockName = getBasicBlockIndex(BB);

    termIndices[blockName] = getTerminatorIndex(BB->getTerminator());

    for (auto IS = BB->begin(), IE = BB->end(); IS != IE;) {
      llvm::Instruction* I = IS++;

      instrIndices[I] = getCommandIndex(*I);
    }
  }
}

void saveUseIndices(llvm::Function* F, unsigned opCode, Dictionary &data) {
  auto &instrIndices = *(data.get<ArgForIndices>()->instrIndices);
  auto &useIndices = *(data.get<ArgForIndices>()->useIndices);

  for (auto BS = F->begin(), BE = F->end(); BS != BE;) {
    llvm::BasicBlock* BB = BS++;

    for (auto IS = BB->begin(), IE = BB->end(); IS != IE;) {
      llvm::Instruction* I = IS++;

      if (I->getOpcode() == opCode)
        for (auto UI = I->use_begin(), E = I->use_end(); UI != E;) {
          llvm::Use &U = *(UI++);
          llvm::Instruction* use =
            llvm::dyn_cast<llvm::Instruction>(U.getUser());
          llvm::BasicBlock* useBB = use->getParent();
          unsigned index = instrIndices[use];

          useIndices[I].push_back(std::make_tuple(useBB, use, index));
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

void propagateFromAISIPhiToLoadPhiSI (unsigned key, llvm::Instruction *To, llvm::BasicBlock* prev, Dictionary &data, CoreHint &hints) {
  auto &instrIndices = *(data.get<ArgForIndices>()->instrIndices);
  auto &termIndices = *(data.get<ArgForIndices>()->termIndices);
  auto &recentInstr = *(data.get<ArgForMem2Reg>()->recentInstr);
  auto &storeItem = *(data.get<ArgForMem2Reg>()->storeItem);
  auto &replaceItem = *(data.get<ArgForMem2Reg>()->replaceItem);

  // variable which store in store inst can be replace
  // phi can be replace in prunning part
  // consider both of them
  std::shared_ptr<TyPosition> from_position = NULL;
  std::shared_ptr<TyPosition> to_position = NULL;
  llvm::PHINode* Phi = llvm::dyn_cast<llvm::PHINode>(To);

  // if from is phi then make start block
  // if to is phi then need prev block infro
  // from doesn't matter

  // propagate from to   from : AI SI Phi   to : Phi LI
  if (recentInstr[key].op0 == "llvmberry::PHI") 
    // position from
    from_position = TyPosition::make_start_of_block(SRC, recentInstr[key].instrBB->getName());
  else 
    from_position = recentInstr[key].instrPos;

  if (Phi != NULL) 
    //propagate position to need prev block infromation
    to_position = TyPosition::make_end_of_block(SRC, *prev, termIndices[prev->getName()]);
  else 
    to_position = TyPosition::make(SRC, *To, instrIndices[To], "");

  if (llvm::StoreInst *SI = llvm::dyn_cast<llvm::StoreInst>(To)) {
    if (!(storeItem[SI].op0 == "") &&
        (!data.get<ArgForMem2Reg>()->equalsIfConsVar(storeItem[SI].expr, TyExpr::make(*(SI->getOperand(0)), Physical)))) { 
    //global -> constant or argument it won't change

      //infrule
      std::string op1 =getVariable(*(SI->getOperand(1)));

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
    return;
  }

  PROPAGATE(LESSDEF(recentInstr[key].instrL, VAR(recentInstr[key].op1, Ghost), SRC),
            BOUNDS(from_position, to_position));

  std::shared_ptr<TyExpr> val = recentInstr[key].instrR;
  PROPAGATE(LESSDEF(VAR(recentInstr[key].op1, Ghost), val, TGT),
            BOUNDS(from_position, to_position));
  
  replaceItem.push_back(std::shared_ptr<TyExpr>(val));

  // Infrule function
  // if from position is SI or AI, apply infrule  if to position is PHI, apply infrule
  // if store Ai same rule else if phi different rule
  // if from is not phi apply infrule here
  if ((recentInstr[key].op0 != "llvmberry::PHI") && (!recentInstr[key].check))
    INFRULE(recentInstr[key].instrPos, ConsIntroGhost::make(recentInstr[key].instrR, REGISTER(recentInstr[key].op1, Ghost)));

  // if to is phi then apply infrule here
  if (Phi != NULL) {
    if (getVariable(*Phi) != recentInstr[key].op1) 
      INFRULE(TyPosition::make(SRC, *Phi, prev->getName()),
              ConsIntroGhost::make(VAR(recentInstr[key].op1, Ghost), REGISTER(getVariable(*Phi), Ghost))); 
    
    recentInstr[key].check = false;
  }
}

void propagateLoadInstToUse(llvm::LoadInst *LI, llvm::Value *V, std::string In, Dictionary &data, CoreHint &hints) {
  auto &instrIndices = *(data.get<ArgForIndices>()->instrIndices);
  auto &replaceItem = *(data.get<ArgForMem2Reg>()->replaceItem);
  std::shared_ptr<std::vector<std::shared_ptr<llvmberry::TyPosition>>> destSet = saveDestSet(LI, data);
  std::string Rload = llvmberry::getVariable(*LI);

  //propagate LI to use set
  PROPAGATE(LESSDEF(VAR(Rload, Physical), VAR(Rload, Ghost), SRC),
            BOUNDSET(TyPosition::make(SRC, *LI, instrIndices[LI], ""), destSet));

  std::shared_ptr<TyExpr> val = TyExpr::make(*V, Physical);
  PROPAGATE(LESSDEF(VAR(Rload, Ghost), val, TGT),
            BOUNDSET(TyPosition::make(SRC, *LI, instrIndices[LI], ""), destSet));

  std::shared_ptr<TyPosition> position = TyPosition::make(SRC, *LI, instrIndices[LI], "");   

  //infrule at LI index
  INFRULE(position, ConsIntroGhost::make(VAR(In, Ghost), REGISTER(Rload, Ghost)));

  if (!llvm::isa<llvm::Constant>(V))
    replaceItem.push_back(std::shared_ptr<TyExpr>(val));
}

void propagateLoadGhostValueForm(llvm::Instruction* From, llvm::Instruction* To, llvm::Value* value, Dictionary &data, CoreHint &hints) {
  auto &instrIndices = *(data.get<ArgForIndices>()->instrIndices);
  auto &storeItem = *(data.get<ArgForMem2Reg>()->storeItem);
  auto &replaceItem = *(data.get<ArgForMem2Reg>()->replaceItem);
  auto &replaceTag = *(data.get<ArgForMem2Reg>()->replaceTag);
  llvm::StoreInst *SI = llvm::dyn_cast<llvm::StoreInst>(From);
  llvm::AllocaInst *AI = llvm::dyn_cast<llvm::AllocaInst>(From);
  std::string Rghost;

  if (AI != NULL)
    Rghost = getVariable(*AI);
  else if (SI != NULL)
    Rghost = getVariable(*(SI->getOperand(1)));

  std::shared_ptr<TyPosition> from_position = TyPosition::make(SRC, *From, instrIndices[From], "");
  std::shared_ptr<TyPosition> to_position = TyPosition::make(SRC, *To, instrIndices[To], "");

  PROPAGATE(LESSDEF(INSN(std::shared_ptr<TyInstruction>(new ConsLoadInst(TyLoadInst::makeAlignOne(From)))),
                    VAR(Rghost, Ghost), SRC), 
            BOUNDS(from_position, to_position));
  
  std::shared_ptr<TyExpr> val = TyExpr::make(*value, Physical);
  PROPAGATE(LESSDEF(VAR(Rghost, Ghost), val, TGT), BOUNDS(from_position, to_position));

  if (value->getName() != "")
    replaceItem.push_back(std::shared_ptr<TyExpr>(val));

  if (SI != NULL) {
    if (storeItem[SI].op0 == "" ||
        data.get<ArgForMem2Reg>()->equalsIfConsVar(storeItem[SI].expr, TyExpr::make(*value, Physical))) {
      std::shared_ptr<TyExpr> val = TyExpr::make(*(SI->getOperand(0)), Physical);
      std::shared_ptr<TyIntroGhost> ghost(new TyIntroGhost(val, REGISTER(Rghost, Ghost)));
      INFRULE(from_position, std::shared_ptr<TyInfrule>(new ConsIntroGhost(ghost)));

      if (storeItem[SI].op0 != "")
        replaceTag.push_back(std::shared_ptr<TyExpr>(val));
    
    } else
      INFRULE(from_position, ConsIntroGhost::make(VAR(storeItem[SI].op0, Ghost), REGISTER(Rghost, Ghost)));
  } else if (AI != NULL)
    INFRULE(from_position, ConsIntroGhost::make(EXPR(value, Physical), REGISTER(Rghost, Ghost)));
}

void replaceExpr(llvm::Instruction *Tgt, llvm::Value *New, Dictionary &data) {
  auto &replaceItem = *(data.get<ArgForMem2Reg>()->replaceItem);
  std::string str = "";

  if (llvm::isa<llvm::AllocaInst>(Tgt) || llvm::isa<llvm::LoadInst>(Tgt) || llvm::isa<llvm::PHINode>(Tgt))
        str = getVariable(*Tgt);

  std::shared_ptr<TyExpr> tgtPhysical = ConsVar::make(str, Physical);
  std::shared_ptr<TyExpr> replPhysical = TyExpr::make(*New, Physical);

  for (unsigned i = 0; i < replaceItem.size(); i++) {
    std::shared_ptr<TyExpr> tmp = replaceItem.at(i);
    if (data.get<ArgForMem2Reg>()->equalsIfConsVar(tmp, tgtPhysical))
     tmp->replace_expr(replPhysical); 
  }
}

void replaceTag(llvm::Instruction *Tgt, TyTag tag, Dictionary &data) {
  auto &replaceTag = *(data.get<ArgForMem2Reg>()->replaceTag);
  std::string str = "";

  if (llvm::isa<llvm::AllocaInst>(Tgt) || llvm::isa<llvm::LoadInst>(Tgt) || llvm::isa<llvm::PHINode>(Tgt))
    str = getVariable(*Tgt);

  std::shared_ptr<TyExpr> tgtPhysical = ConsVar::make(str, Physical);
  std::shared_ptr<TyExpr> replTag = ConsVar::make(str, tag);

  for (unsigned i = 0; i < replaceTag.size(); i++) {
    std::shared_ptr<TyExpr> tmp = replaceTag.at(i);
    if (data.get<ArgForMem2Reg>()->equalsIfConsVar(tmp, tgtPhysical))
      tmp->replace_expr(replTag);
  }
}

void generateHintForMem2RegReplaceHint(llvm::Value *ReplVal,
                                       llvm::Instruction *ReplInst,
                                       Dictionary &data) {
  auto &mem2regCmd = *(data.get<ArgForMem2Reg>()->mem2regCmd);
  std::string ReplName = "";

  if (llvm::isa<llvm::AllocaInst>(ReplInst) ||
      llvm::isa<llvm::LoadInst>(ReplInst) ||
      llvm::isa<llvm::PHINode>(ReplInst))
    ReplName = getVariable(*ReplInst);
  
  if ((ReplName == "") ||
      (mem2regCmd.find(ReplName) == mem2regCmd.end()))
    return;

  data.get<ArgForMem2Reg>()->replaceLessthanUndef(ReplName,
                                                  TyValue::make(*ReplVal));
}

} // llvmberry
