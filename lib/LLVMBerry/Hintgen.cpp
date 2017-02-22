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

void makeReachableBlockMap(llvm::BasicBlock* Src,
                           llvm::BasicBlock* Tgt) {
  ValidationUnit::GetInstance()->intrude([&Src, &Tgt](
      Dictionary &data, CoreHint &hints) {
    auto &strVec = *(data.get<ArgForMem2Reg>()->strVec);
    auto &isReachable = *(data.get<ArgForMem2Reg>()->isReachable);

    for (auto BI = succ_begin(Tgt), BE = succ_end(Tgt); BI != BE;) {
      llvm::BasicBlock* BB = *(BI++);
      std::string BBname = BB->getName(); 
      if (std::find(strVec.begin(), strVec.end(), BBname) != strVec.end())
        continue;
      strVec.push_back(BBname);

      isReachable[Src].push_back(BB);
      makeReachableBlockMap(Src, BB);
    }
  });
}

void generateHintForMem2RegPropagateStore(llvm::BasicBlock* Pred,
                                          llvm::StoreInst* SI,
                                          llvm::Instruction* next,
                                          int nextIndex) {
  ValidationUnit::GetInstance()->intrude([&Pred, &SI, &next, &nextIndex](
      Dictionary &data, CoreHint &hints) {
    auto &instrIndices = *(data.get<ArgForIndices>()->instrIndices);
    auto &termIndices = *(data.get<ArgForIndices>()->termIndices);
    auto &storeItem = *(data.get<ArgForMem2Reg>()->storeItem);
    auto &mem2regCmd = *(data.get<ArgForMem2Reg>()->mem2regCmd);
    std::string Rstore = getVariable(*(SI->getOperand(1)));
    std::string bname = getBasicBlockIndex(SI->getParent());
    std::string predName = getBasicBlockIndex(Pred);
    // propagate instruction
    if (llvm::isa<llvm::PHINode>(next)) {
      PROPAGATE(LESSDEF(INSN(std::shared_ptr<TyInstruction>(
                          new ConsLoadInst(TyLoadInst::makeAlignOne(SI)))),
                        VAR(Rstore, Ghost), SRC),
                BOUNDS(TyPosition::make(SRC, *SI, instrIndices[SI], ""),
                       TyPosition::make_end_of_block(TGT, *Pred, termIndices[predName])));

      std::shared_ptr<TyPropagateLessdef> lessdef = TyPropagateLessdef::make
                                                  (VAR(Rstore, Ghost),
                                                   TyExpr::make(*(SI->getOperand(0)),
                                                                Physical),
                                                   TGT);

      if (SI->getOperand(0)->getName()!="")
        mem2regCmd[getVariable(*(SI->getOperand(0)))].lessdef.push_back(lessdef);

      PROPAGATE(std::shared_ptr<TyPropagateObject>(new ConsLessdef(lessdef)),
                BOUNDS(TyPosition::make(SRC, *SI, instrIndices[SI], ""),
                       TyPosition::make_end_of_block(SRC, *Pred, termIndices[predName])));
    } else {
      PROPAGATE(LESSDEF(INSN(std::shared_ptr<TyInstruction>(
                          new ConsLoadInst(TyLoadInst::makeAlignOne(SI)))),
                        VAR(Rstore, Ghost), SRC),
                BOUNDS(TyPosition::make(SRC, *SI, instrIndices[SI], ""),
                       TyPosition::make(SRC, *next, nextIndex, "")));

      std::shared_ptr<TyPropagateLessdef> lessdef = TyPropagateLessdef::make
                                                      (VAR(Rstore, Ghost),
                                                       TyExpr::make(*(SI->getOperand(0)),
                                                                    Physical),
                                                       TGT);

      if (SI->getOperand(0)->getName()!="")
        mem2regCmd[getVariable(*(SI->getOperand(0)))].lessdef.push_back(lessdef);
    
      PROPAGATE(std::shared_ptr<TyPropagateObject>(new ConsLessdef(lessdef)),
                BOUNDS(TyPosition::make(SRC, *SI, instrIndices[SI], ""),
                       TyPosition::make(SRC, *next, nextIndex, "")));
    }

    if (storeItem[SI].op0 == "%" ||
        data.get<ArgForMem2Reg>()->equalsIfConsVar(storeItem[SI].expr, 
                                                   TyExpr::make(*(SI->getOperand(0)),
                                                                Physical))) {
      // stored value will not be changed in another iteration
      std::shared_ptr<TyIntroGhost> ghost(new TyIntroGhost(storeItem[SI].expr,
                                                           REGISTER(Rstore, Ghost)));

      std::shared_ptr<TyTransitivity> transitivity(
          new TyTransitivity(
                INSN(std::shared_ptr<TyInstruction>(
                  new ConsLoadInst(TyLoadInst::makeAlignOne(SI)))),
                storeItem[SI].expr,
                VAR(Rstore, Ghost)));

      std::shared_ptr<TyPosition> position =
        TyPosition::make(SRC, *SI, instrIndices[SI], "");

      if (storeItem[SI].op0 != "%") {
        mem2regCmd[getVariable(*(SI->getOperand(0)))].ghost.push_back(ghost);
        mem2regCmd[getVariable(*(SI->getOperand(0)))].transSrc.push_back(std::make_pair(position, transitivity));
      }

      INFRULE(
          position,
          std::shared_ptr<TyInfrule>(new ConsIntroGhost(ghost)));

      INFRULE(
          position,
          std::shared_ptr<TyInfrule>(new ConsTransitivity(transitivity)));
    } else {
      std::shared_ptr<TyPosition> position =
        TyPosition::make(SRC, *SI, instrIndices[SI], "");

      // stored value will be changed in another iteration
      INFRULE(position,//TyPosition::make(SRC, *SI, instrIndices[SI], ""),
              ConsIntroGhost::make(VAR(storeItem[SI].op0, Ghost),
                                   REGISTER(Rstore, Ghost)));

      INFRULE(position,//TyPosition::make(SRC, *SI, instrIndices[SI], ""),
              ConsTransitivity::make(INSN(std::shared_ptr<TyInstruction>(
                                       new ConsLoadInst(TyLoadInst::makeAlignOne(SI)))),
                                     VAR(storeItem[SI].op0, Physical),
                                     VAR(storeItem[SI].op0, Ghost)));

      INFRULE(position,//TyPosition::make(SRC, *SI, instrIndices[SI], ""),
              ConsTransitivity::make(INSN(std::shared_ptr<TyInstruction>(
                                       new ConsLoadInst(TyLoadInst::makeAlignOne(SI)))),
                                     VAR(storeItem[SI].op0, Ghost),
                                     VAR(Rstore, Ghost)));

      std::shared_ptr<TyTransitivityTgt> transTgt(new TyTransitivityTgt(
                                                    VAR(Rstore, Ghost),
                                                    VAR(storeItem[SI].op0, Ghost),
                                                    TyExpr::make(*(SI->getOperand(0)), Physical)));

      INFRULE(position,
              std::shared_ptr<TyInfrule>(new ConsTransitivityTgt(transTgt)));

      if (SI->getOperand(0)->getName() != "")
        mem2regCmd[getVariable(*(SI->getOperand(0)))].transTgt.push_back(transTgt);
       
    }
  });
}

llvm::Instruction* properPHI(llvm::BasicBlock* BB, std::string Target,
                             llvm::Instruction* I, bool isInit,
                             bool checkSI, Dictionary data,
                             bool isLoop) {
  auto &blockPairVec = *(data.get<ArgForMem2Reg>()->blockPairVec);
  auto &isReachable = *(data.get<ArgForMem2Reg>()->isReachable);
  llvm::BasicBlock *IB = I->getParent();

  // return NULL if BB is loop
   if (!llvm::isa<llvm::PHINode>(I) && (BB == IB))
    return NULL;

  // if isInit is true, check current BB
  if (isInit) {
    if (checkSI) {
      llvm::BasicBlock::iterator II = BB->begin();

      while (II != BB->end()) {
        if (llvm::StoreInst* SI = llvm::dyn_cast<llvm::StoreInst>(II++)) {
          if ((Target == getVariable(*SI->getOperand(1)))) {
            return SI;
          }
        }
      }
    }

    if (llvm::PHINode *PHI = llvm::dyn_cast<llvm::PHINode>(BB->begin())) {
      llvm::BasicBlock::iterator PNI = BB->begin();

      while (PHI) {
        std::string Rphi = getVariable(*PHI);

        // fragile: this condition relies on llvm naming convention of PHI
        if (Target == Rphi.substr(0, Rphi.rfind(".")))
          return PHI;

        PNI++;
        PHI = llvm::dyn_cast<llvm::PHINode>(PNI);
      }
    }
  }

  for (auto BI = pred_begin(BB), BE = pred_end(BB); BI != BE;) {
    llvm::BasicBlock* BBtmp = *(BI++);

    // skip if IB block is not reachable to BBtmp block
    if (std::find(isReachable[IB].begin(), isReachable[IB].end(), BBtmp) == isReachable[IB].end())
      continue;

    if (std::find(blockPairVec.begin(), blockPairVec.end(),
        std::make_pair(getBasicBlockIndex(BB), getBasicBlockIndex(BBtmp))) != blockPairVec.end())
      continue;
    blockPairVec.push_back(std::make_pair(getBasicBlockIndex(BB), getBasicBlockIndex(BBtmp)));

    llvm::BasicBlock::iterator II = BBtmp->begin();
    while (II != BBtmp->end()) {
      if (llvm::StoreInst* SI = llvm::dyn_cast<llvm::StoreInst>(II++)) {
        if (Target == getVariable(*SI->getOperand(1)))
          return SI;
      }
    }

    if (llvm::PHINode *PHI = llvm::dyn_cast<llvm::PHINode>(BBtmp->begin())) {
      llvm::BasicBlock::iterator PNI = BBtmp->begin();

      while (PHI) {
        std::string Rphi = getVariable(*PHI);

        // fragile: this condition relies on llvm naming convention of PHI
        if (Target == Rphi.substr(0, Rphi.rfind("."))) {
          return PHI;
        }

        PNI++;
        PHI = llvm::dyn_cast<llvm::PHINode>(PNI);
      }
    }

    llvm::Instruction* ret = properPHI(BBtmp, Target, I, false, checkSI, data, true);
    if (ret != NULL)
      return ret;
  }
  return NULL;
}

void generateHintForMem2RegPropagateLoad(llvm::Instruction* I,
                                         llvm::LoadInst* LI,
                                         llvm::BasicBlock* useBB,
                                         int useIndex,
                                         llvm::Instruction* use) {
  assert(I != NULL && "Input Instruction should not be NULL");

  ValidationUnit::GetInstance()->intrude([&I, &LI, &use, &useIndex, &useBB](
      Dictionary &data, CoreHint &hints) {
    auto &instrIndices = *(data.get<ArgForIndices>()->instrIndices);
    auto &mem2regCmd = *(data.get<ArgForMem2Reg>()->mem2regCmd);
    auto &blockPairVec = *(data.get<ArgForMem2Reg>()->blockPairVec);
    std::string Rload = getVariable(*LI);
    llvm::PHINode* phiNode = NULL;
    if (use != NULL)
      phiNode = llvm::dyn_cast<llvm::PHINode>(use);

    if (llvm::StoreInst* SI = llvm::dyn_cast<llvm::StoreInst>(I)) {
      std::string Rstore = getVariable(*(SI->getOperand(1)));

      INFRULE(TyPosition::make(SRC, *LI, instrIndices[LI], ""),
              ConsIntroGhost::make(VAR(Rstore, Ghost), REGISTER(Rload, Ghost)));

      INFRULE(TyPosition::make(SRC, *LI, instrIndices[LI], ""),
              ConsTransitivity::make(VAR(Rload, Physical),
                                     INSN(std::shared_ptr<TyInstruction>(
                                       new ConsLoadInst(TyLoadInst::makeAlignOne(SI)))),
                                     VAR(Rstore, Ghost)));

      INFRULE(TyPosition::make(SRC, *LI, instrIndices[LI], ""),
              ConsTransitivity::make(VAR(Rload, Physical), VAR(Rstore, Ghost),
                                     VAR(Rload, Ghost)));

      if (phiNode != NULL) {
        for (unsigned i = 0; i != phiNode->getNumIncomingValues(); ++i) {
          llvm::Value *iPI =
              llvm::dyn_cast<llvm::Value>(phiNode->getIncomingValue(i));
          if (LI == iPI) {
            std::string prev = phiNode->getIncomingBlock(i)->getName();
            PROPAGATE(LESSDEF(VAR(Rload, Physical), VAR(Rload, Ghost), SRC),
                      BOUNDS(TyPosition::make(SRC, *LI, instrIndices[LI], ""),
                             TyPosition::make(SRC, *use, useIndex, prev)));
          }
        }
      } else {
        PROPAGATE(LESSDEF(VAR(Rload, Physical), VAR(Rload, Ghost), SRC),
                  BOUNDS(TyPosition::make(SRC, *LI, instrIndices[LI], ""),
                         TyPosition::make(SRC, *useBB, useIndex)));
      }

      blockPairVec.clear();
      llvm::PHINode* PHI = NULL; 
      if (llvm::Instruction* Itmp = properPHI(LI->getParent(), Rstore, SI, true, false, data))
        PHI = llvm::dyn_cast<llvm::PHINode>(Itmp);

      if (PHI != NULL) {
        std::shared_ptr<TyPropagateLessdef> lessdef =
          TyPropagateLessdef::make
            (VAR(Rload, Ghost),
             VAR(Rload, Physical),
             TGT);

        mem2regCmd[Rload].lessdef.push_back(lessdef);
        
        if (phiNode != NULL) {
          for (unsigned i = 0; i != phiNode->getNumIncomingValues(); ++i) {
            llvm::Value *iPI =
                llvm::dyn_cast<llvm::Value>(phiNode->getIncomingValue(i));
            if (LI == iPI) {
              std::string prev = phiNode->getIncomingBlock(i)->getName();
              PROPAGATE(
                  std::shared_ptr<TyPropagateObject>(new ConsLessdef(lessdef)),
                  BOUNDS(TyPosition::make(SRC, *LI, instrIndices[LI], ""),
                         TyPosition::make(SRC, *use, useIndex, prev)));
            }
          }
        } else {
          PROPAGATE(
              std::shared_ptr<TyPropagateObject>(new ConsLessdef(lessdef)),
              BOUNDS(TyPosition::make(SRC, *LI, instrIndices[LI], ""),
                     TyPosition::make(SRC, *useBB, useIndex)));
        }

        std::shared_ptr<TyTransitivityTgt> transTgt(new TyTransitivityTgt(
                                                      VAR(Rload, Ghost),
                                                      VAR(Rstore, Ghost),
                                                      VAR(Rload, Physical)));

        INFRULE(TyPosition::make(SRC, *LI, instrIndices[LI], ""),
                std::shared_ptr<TyInfrule>(new ConsTransitivityTgt(transTgt)));

        mem2regCmd[Rload].transTgt.push_back(transTgt);
      } else {
        std::shared_ptr<TyPropagateLessdef> lessdef =
          TyPropagateLessdef::make
            (VAR(Rload, Ghost),
             TyExpr::make(*(SI->getOperand(0)), Physical),
             TGT);

        if (phiNode != NULL) {
          for (unsigned i = 0; i != phiNode->getNumIncomingValues(); ++i) {
            llvm::Value *iPI =
                llvm::dyn_cast<llvm::Value>(phiNode->getIncomingValue(i));
            if (LI == iPI) {
              std::string prev = phiNode->getIncomingBlock(i)->getName();
              PROPAGATE(
                  std::shared_ptr<TyPropagateObject>(new ConsLessdef(lessdef)),
                  BOUNDS(TyPosition::make(SRC, *LI, instrIndices[LI], ""),
                         TyPosition::make(SRC, *use, useIndex, prev)));
            }
          }
        } else {
          PROPAGATE(
              std::shared_ptr<TyPropagateObject>(new ConsLessdef(lessdef)),
              BOUNDS(TyPosition::make(SRC, *LI, instrIndices[LI], ""),
                     TyPosition::make(SRC, *useBB, useIndex)));
        }
        std::shared_ptr<TyTransitivityTgt> transTgt(new TyTransitivityTgt(
                                                      VAR(Rload, Ghost),
                                                      VAR(Rstore, Ghost),
                                                      TyExpr::make(*(SI->getOperand(0)), Physical)));

        INFRULE(TyPosition::make(SRC, *LI, instrIndices[LI], ""),
                std::shared_ptr<TyInfrule>(new ConsTransitivityTgt(transTgt)));

        if (SI->getOperand(0)->getName()!="") {
          mem2regCmd[getVariable(*(SI->getOperand(0)))].lessdef.push_back(lessdef);
          mem2regCmd[getVariable(*(SI->getOperand(0)))].transTgt.push_back(transTgt);
        }
      }
    } else if (llvm::AllocaInst* AI = llvm::dyn_cast<llvm::AllocaInst>(I)) {
      std::string Ralloca = getVariable(*AI);

      INFRULE(TyPosition::make(SRC, *LI, instrIndices[LI], ""),
              ConsIntroGhost::make(VAR(Ralloca, Ghost), REGISTER(Rload, Ghost)));

      INFRULE(TyPosition::make(SRC, *LI, instrIndices[LI], ""),
              ConsTransitivity::make(VAR(Rload, Physical),
                                     INSN(std::shared_ptr<TyInstruction>(
                                       new ConsLoadInst(TyLoadInst::makeAlignOne(AI)))),
                                     VAR(Ralloca, Ghost)));

      INFRULE(TyPosition::make(SRC, *LI, instrIndices[LI], ""),
              ConsTransitivity::make(VAR(Rload, Physical), VAR(Ralloca, Ghost),
                                     VAR(Rload, Ghost)));

      if (phiNode != NULL) {
        for (unsigned i = 0; i != phiNode->getNumIncomingValues(); ++i) {
          llvm::Value *iPI =
              llvm::dyn_cast<llvm::Value>(phiNode->getIncomingValue(i));
          if (LI == iPI) {
            std::string prev = phiNode->getIncomingBlock(i)->getName();
            PROPAGATE(LESSDEF(VAR(Rload, Physical), VAR(Rload, Ghost), SRC),
                      BOUNDS(TyPosition::make(SRC, *LI, instrIndices[LI], ""),
                             TyPosition::make(SRC, *use, useIndex, prev)));
          }
        }
      } else {
        PROPAGATE(LESSDEF(VAR(Rload, Physical), VAR(Rload, Ghost), SRC),
                  BOUNDS(TyPosition::make(SRC, *LI, instrIndices[LI], ""),
                         TyPosition::make(SRC, *useBB, useIndex)));
      }

      std::shared_ptr<TyPropagateLessdef> lessdef =
        TyPropagateLessdef::make
          (VAR(Rload, Ghost),
           VAR(Rload, Physical),
           TGT);

      mem2regCmd[Rload].lessdef.push_back(lessdef);

      if (phiNode != NULL) {
        for (unsigned i = 0; i != phiNode->getNumIncomingValues(); ++i) {
          llvm::Value *iPI =
              llvm::dyn_cast<llvm::Value>(phiNode->getIncomingValue(i));
          if (LI == iPI) {
            std::string prev = phiNode->getIncomingBlock(i)->getName();
            PROPAGATE(
                std::shared_ptr<TyPropagateObject>(new ConsLessdef(lessdef)),
                BOUNDS(TyPosition::make(SRC, *LI, instrIndices[LI], ""),
                       TyPosition::make(SRC, *use, useIndex, prev)));
          }
        }
      } else {
        PROPAGATE(
            std::shared_ptr<TyPropagateObject>(new ConsLessdef(lessdef)),
            BOUNDS(TyPosition::make(SRC, *LI, instrIndices[LI], ""),
                   TyPosition::make(SRC, *useBB, useIndex)));
      }

      std::shared_ptr<TyTransitivityTgt> transTgt(new TyTransitivityTgt(
                                                    VAR(Rload, Ghost),
                                                    VAR(Ralloca, Ghost),
                                                    VAR(Rload, Physical)));

      INFRULE(TyPosition::make(SRC, *LI, instrIndices[LI], ""),
              std::shared_ptr<TyInfrule>(new ConsTransitivityTgt(transTgt)));

      mem2regCmd[Rload].transTgt.push_back(transTgt);
    }
  });
}

void generateHintForMem2RegReplaceHint(llvm::Value *ReplVal,
                                       llvm::Instruction *ReplInst) {
  ValidationUnit::GetInstance()->intrude
    ([&ReplVal, &ReplInst]
      (Dictionary &data, CoreHint &hints) {
    auto &mem2regCmd = *(data.get<ArgForMem2Reg>()->mem2regCmd);
    std::string ReplName = "";

    if (llvm::isa<llvm::AllocaInst>(ReplInst) ||
        llvm::isa<llvm::LoadInst>(ReplInst) ||
        llvm::isa<llvm::PHINode>(ReplInst))
      ReplName = getVariable(*ReplInst);
    
    if ((ReplName == "") ||
        (mem2regCmd.find(ReplName) == mem2regCmd.end()))
      return;

    data.get<ArgForMem2Reg>()->replaceCmdRhs("Lessdef", ReplName,
                                    TyExpr::make(*ReplVal, Physical));

    data.get<ArgForMem2Reg>()->replaceCmdRhs("IntroGhost", ReplName,
                                    VAR(ReplName, Ghost));

    data.get<ArgForMem2Reg>()->replaceCmdRhs("TransitivityTgt_e2", ReplName,
                                    TyExpr::make(*ReplVal, Physical));

    data.get<ArgForMem2Reg>()->replaceCmdRhs("TransitivityTgt_e3", ReplName,
                                    TyExpr::make(*ReplVal, Physical));

    data.get<ArgForMem2Reg>()->replaceLessthanUndef(ReplName,
                                                    TyValue::make(*ReplVal));

    data.get<ArgForMem2Reg>()->replaceLessthanUndefTgt(ReplName,
                                                       TyValue::make(*ReplVal));

    std::shared_ptr<TyExpr> keyExpr = ConsVar::make(ReplName, Physical);

    std::vector<std::pair<std::shared_ptr<TyPosition>,
                         std::shared_ptr<TyTransitivity>>> &vec =
      mem2regCmd[ReplName].transSrc;

    std::vector<std::pair<std::shared_ptr<TyPosition>,
                          std::shared_ptr<TyTransitivity>>> tmp;

    for (size_t i = 0; i < vec.size(); i++) {
      if (data.get<ArgForMem2Reg>()->equalsIfConsVar(vec[i].second->getExpr2(), 
                                                    keyExpr)) {
        if (ConsInsn* ci = dynamic_cast<ConsInsn*>(vec[i].second->getExpr1().get())) {
        if (ConsVar* cv2 = dynamic_cast<ConsVar*>(vec[i].second->getExpr2().get())) {
        if (ConsVar* cv3 = dynamic_cast<ConsVar*>(vec[i].second->getExpr3().get())) {
          std::shared_ptr<TyTransitivity> transitivity
            (new TyTransitivity(std::shared_ptr<TyExpr>(new ConsInsn(ci->getTyInsn())),
                                std::shared_ptr<TyExpr>(new ConsVar(cv2->getTyReg())),
                                std::shared_ptr<TyExpr>(new ConsVar(cv3->getTyReg()))));

          tmp.push_back(std::make_pair(vec[i].first, transitivity));

          std::shared_ptr<TyTransitivityTgt> transitivitytgt
            (new TyTransitivityTgt(std::shared_ptr<TyExpr>(new ConsVar(cv3->getTyReg())),
                                   VAR(ReplName, Ghost),
                                   TyExpr::make(*ReplVal, Physical)));

          if (ReplVal->getName() != "")
            mem2regCmd[getVariable(*ReplVal)].transTgt.push_back(transitivitytgt);

          INFRULE(vec[i].first,
                  std::shared_ptr<TyInfrule>(new ConsTransitivityTgt(transitivitytgt)));
         
          if (ConsLoadInst* cli = dynamic_cast<ConsLoadInst*>(ci->getTyInsn().get())) {
          if (ConsId* cid = dynamic_cast<ConsId*>(cli->getTyLoadInst().get()->getPtrValue().get())) {
          if (cv3->getTyReg() == cid->reg) {
            data.get<ArgForMem2Reg>()->replaceCmdRhs("Transitivity_e3", ReplName,
                                                     vec[i].second->getExpr3());
          }}}
        }}}
      }
    }

    data.get<ArgForMem2Reg>()->replaceCmdRhs("Transitivity_e1", ReplName,
                                    VAR(ReplName, Physical));

    data.get<ArgForMem2Reg>()->replaceCmdRhs("Transitivity_e2", ReplName,
                                    VAR(ReplName, Ghost));

    for (size_t i = 0; i < tmp.size(); i++) {
      INFRULE(tmp[i].first,
              std::shared_ptr<TyInfrule>(new ConsTransitivity(tmp[i].second)));

      mem2regCmd[ReplName].transSrc.push_back(tmp[i]);
    }
  });
}

void checkTag_propagate (llvm::BasicBlock *BB, llvm::AllocaInst *AI, llvm::Instruction *Inst) {
  ValidationUnit::GetInstance()->intrude([&BB, &AI, &Inst]
                                                 (Dictionary &data, CoreHint &hints) {
    auto &instrIndices = *(data.get<ArgForIndices>()->instrIndices);
    auto &termIndices = *(data.get<ArgForIndices>()->termIndices);
    auto &reachedEdgeTag = *(data.get<ArgForMem2Reg>()->reachedEdgeTag);

    
    llvm::Value *UndefVal = llvm::UndefValue::get(AI->getAllocatedType());

    for (auto BI = pred_begin(BB), BE = pred_end(BB); BI != BE; ++BI) {
      llvm::BasicBlock *pred = *BI;

      std::pair<llvm::BasicBlock *, llvm::BasicBlock *> edge = std::make_pair(pred, BB);

      std::pair<std::pair<llvm::BasicBlock *, llvm::BasicBlock *>,
             llvmberry::Mem2RegArg::Tr_Type> blockPairTag1 = std::make_pair(edge, llvmberry::Mem2RegArg::False);
      std::pair<std::pair<llvm::BasicBlock *, llvm::BasicBlock *>,
              llvmberry::Mem2RegArg::Tr_Type> blockPairTag2 = std::make_pair(edge, llvmberry::Mem2RegArg::LoadStart);
      std::vector<std::pair<std::pair<llvm::BasicBlock*,llvm::BasicBlock*>,
              llvmberry::Mem2RegArg::Tr_Type>>::iterator it1 =
              std::find(reachedEdgeTag.begin(), reachedEdgeTag.end(), blockPairTag1);
      std::vector<std::pair<std::pair<llvm::BasicBlock*,llvm::BasicBlock*>,
              llvmberry::Mem2RegArg::Tr_Type>>::iterator it2 =
              std::find(reachedEdgeTag.begin(), reachedEdgeTag.end(), blockPairTag2);

      if (it1 != reachedEdgeTag.end()) {
        int pos = std::distance(reachedEdgeTag.begin(), it1);
        reachedEdgeTag.at(pos).second = llvmberry::Mem2RegArg::True;

           //propagate pred block
        if (AI->getParent() != pred) {
          PROPAGATE(
                  LESSDEF(INSN(std::shared_ptr<TyInstruction>(
                                  new ConsLoadInst(TyLoadInst::makeAlignOne(AI)))),
                          VAR(getVariable(*AI), Ghost),
                          SRC),
                  BOUNDS(TyPosition::make_start_of_block(SRC, getBasicBlockIndex(pred)),
                         TyPosition::make_end_of_block(SRC, *pred, termIndices[getBasicBlockIndex(pred)])));


          PROPAGATE(
                  LESSDEF(VAR(getVariable(*AI), Ghost),
                          EXPR(UndefVal, Physical),
                          TGT),
                  BOUNDS(TyPosition::make_start_of_block(SRC, getBasicBlockIndex(pred)),
                         TyPosition::make_end_of_block(SRC, *pred, termIndices[getBasicBlockIndex(pred)])));
        } else {
          PROPAGATE(
                  LESSDEF(INSN(std::shared_ptr<TyInstruction>(
                                  new ConsLoadInst(TyLoadInst::makeAlignOne(AI)))),
                          VAR(getVariable(*AI), Ghost),
                          SRC),
                  BOUNDS(TyPosition::make(SRC, *AI, instrIndices[AI], ""),
                         TyPosition::make_end_of_block(SRC, *pred, termIndices[getBasicBlockIndex(pred)])));

          PROPAGATE(
                  LESSDEF(VAR(getVariable(*AI), Ghost),
                          EXPR(UndefVal, Physical),
                          TGT),
                  BOUNDS(TyPosition::make(SRC, *AI, instrIndices[AI], ""),
                         TyPosition::make_end_of_block(SRC, *pred, termIndices[getBasicBlockIndex(pred)])));

          INFRULE(llvmberry::TyPosition::make(SRC, *AI, instrIndices[AI], ""),
                  llvmberry::ConsIntroGhost::make(EXPR(UndefVal, Physical),
                                                  REGISTER(getVariable(*AI), Ghost)));

          INFRULE(llvmberry::TyPosition::make(SRC, *AI, instrIndices[AI], ""),
                  llvmberry::ConsTransitivity::make(INSN(*AI),
                                                    EXPR(UndefVal, Physical),
                                                    VAR(getVariable(*AI), Ghost)));
        }
            checkTag_propagate(pred, AI, Inst);
      }
      if (it2 != reachedEdgeTag.end()) {
        int pos = std::distance(reachedEdgeTag.begin(), it2);
        reachedEdgeTag.at(pos).second = llvmberry::Mem2RegArg::True;

       //propagate pred block
      if (AI->getParent() != pred) {
        PROPAGATE(
                LESSDEF(INSN(std::shared_ptr<TyInstruction>(
                                new ConsLoadInst(TyLoadInst::makeAlignOne(AI)))),
                        VAR(getVariable(*AI), Ghost),
                        SRC),
                BOUNDS(TyPosition::make_start_of_block(SRC, getBasicBlockIndex(pred)),
                       TyPosition::make_end_of_block(SRC, *pred, termIndices[getBasicBlockIndex(pred)])));


        PROPAGATE(
                LESSDEF(VAR(getVariable(*AI), Ghost),
                        EXPR(UndefVal, Physical),
                        TGT),
                BOUNDS(TyPosition::make_start_of_block(SRC, getBasicBlockIndex(pred)),
                       TyPosition::make_end_of_block(SRC, *pred, termIndices[getBasicBlockIndex(pred)])));
      } else {
        PROPAGATE(
                LESSDEF(INSN(std::shared_ptr<TyInstruction>(
                                new ConsLoadInst(TyLoadInst::makeAlignOne(AI)))),
                        VAR(getVariable(*AI), Ghost),
                        SRC),
                BOUNDS(TyPosition::make(SRC, *AI, instrIndices[AI], ""),
                       TyPosition::make_end_of_block(SRC, *pred, termIndices[getBasicBlockIndex(pred)])));

        PROPAGATE(
                LESSDEF(VAR(getVariable(*AI), Ghost),
                        EXPR(UndefVal, Physical),
                        TGT),
                BOUNDS(TyPosition::make(SRC, *AI, instrIndices[AI], ""),
                       TyPosition::make_end_of_block(SRC, *pred, termIndices[getBasicBlockIndex(pred)])));

        INFRULE(llvmberry::TyPosition::make(SRC, *AI, instrIndices[AI], ""),
                llvmberry::ConsIntroGhost::make(EXPR(UndefVal, Physical),
                                                REGISTER(getVariable(*AI), Ghost)));

        INFRULE(llvmberry::TyPosition::make(SRC, *AI, instrIndices[AI], ""),
                llvmberry::ConsTransitivity::make(INSN(*AI),
                                                  EXPR(UndefVal, Physical),
                                                  VAR(getVariable(*AI), Ghost)));
      }
            checkTag_propagate(pred, AI, Inst);
    }
  }
  });
}


void generateHintForMem2RegPHIdelete(llvm::BasicBlock *BB,
                                     std::vector<std::pair<llvm::BasicBlock*,
                                                           llvm::BasicBlock*>> VisitedBlock,
                                     llvm::AllocaInst *AI, bool ignore) {
  ValidationUnit::GetInstance()->intrude([&BB, &VisitedBlock, &AI, &ignore]
      (Dictionary &data, CoreHint &hints) {
    auto &instrIndices = *(data.get<ArgForIndices>()->instrIndices);
    auto &termIndices = *(data.get<ArgForIndices>()->termIndices);
    auto &usePile = *(data.get<ArgForMem2Reg>()->usePile);
    auto &reachedEdgeTag = *(data.get<ArgForMem2Reg>()->reachedEdgeTag);
    auto &isReachable = *(data.get<ArgForMem2Reg>()->isReachable);

    llvm::Value *UndefVal = llvm::UndefValue::get(AI->getAllocatedType());
    
    for(auto IN = BB->begin(), IE = BB->end(); IN != IE; ++IN) {
      llvm::Instruction *Inst = IN;

      if (llvm::PHINode* PN = llvm::dyn_cast<llvm::PHINode>(Inst)) {
        if (AI->getName() == PN->getName().substr(0, PN->getName().rfind("."))) {

          checkTag_propagate(BB, AI, PN); 
          return;  
        }
      } else if(llvm::StoreInst *SI = llvm::dyn_cast<llvm::StoreInst>(Inst)){
        if(SI->getOperand(1)->getName() == AI->getName()) {
          return;
        }
      } else if(llvm::LoadInst *LI = llvm::dyn_cast<llvm::LoadInst>(Inst)) {
        if(LI->getOperand(0)->getName() == AI->getName()) {
          llvm::AllocaInst *Src = llvm::dyn_cast<llvm::AllocaInst>(LI->getPointerOperand());
          if (!Src)
              continue;  

          if (BB == LI->getParent()->getParent()->begin()) {
            PROPAGATE(
                    LESSDEF(INSN(std::shared_ptr<TyInstruction>(
                                  new ConsLoadInst(TyLoadInst::makeAlignOne(AI)))),
                            VAR(getVariable(*AI), Ghost),
                            SRC),
                    BOUNDS(TyPosition::make(SRC, *AI, instrIndices[AI], ""),
                           TyPosition::make(SRC, *LI, instrIndices[LI], "")));
            PROPAGATE(
                    LESSDEF(VAR(getVariable(*AI), Ghost),
                            EXPR(UndefVal, Physical),
                            TGT),
                    BOUNDS(TyPosition::make(SRC, *AI, instrIndices[AI], ""),
                           TyPosition::make(SRC, *LI, instrIndices[LI], "")));
            
            INFRULE(llvmberry::TyPosition::make(SRC, *AI, instrIndices[AI], ""),
                    llvmberry::ConsIntroGhost::make(EXPR(UndefVal, Physical),
                                                    REGISTER(getVariable(*AI), Ghost)));

            INFRULE(llvmberry::TyPosition::make(SRC, *AI, instrIndices[AI], ""),
                    llvmberry::ConsTransitivity::make(INSN(std::shared_ptr<TyInstruction>(
                                                          new ConsLoadInst(TyLoadInst::makeAlignOne(AI)))),
                                                      EXPR(UndefVal, Physical),
                                                      VAR(getVariable(*AI), Ghost)));
          } else {
            PROPAGATE(
                    LESSDEF(INSN(std::shared_ptr<TyInstruction>(
                                  new ConsLoadInst(TyLoadInst::makeAlignOne(AI)))),
                            VAR(getVariable(*AI), Ghost),
                            SRC),
                    BOUNDS(TyPosition::make_start_of_block(SRC, getBasicBlockIndex(BB)),
                           TyPosition::make(SRC, *LI, instrIndices[LI], "")));
            PROPAGATE(
                    LESSDEF(VAR(getVariable(*AI), Ghost),
                            EXPR(UndefVal, Physical),
                            TGT),
                    BOUNDS(TyPosition::make_start_of_block(SRC, getBasicBlockIndex(BB)),
                           TyPosition::make(SRC, *LI, instrIndices[LI], ""))); 
          }

            checkTag_propagate(BB, AI, LI);
            ignore = true;

            // add hints per every use of LI
            for (auto UI = usePile[LI].begin(), E = usePile[LI].end(); UI != E;) {
              auto t = *(UI++);
              llvm::BasicBlock* useBB = std::get<0>(t);
              int useIndex =
                llvmberry::getIndexofMem2Reg(std::get<2>(t), std::get<1>(t),
                                             termIndices[llvmberry::getBasicBlockIndex(std::get<0>(t))]);

              // set index of use
              if ((LI->getParent() == useBB &&
                   instrIndices[LI] < std::get<1>(t)) ||
                  (std::find(isReachable[LI->getParent()].begin(),
                             isReachable[LI->getParent()].end(),
                             useBB) != isReachable[LI->getParent()].end())) {
                generateHintForMem2RegPropagateLoad(AI, LI, useBB, useIndex, std::get<2>(t));
              }
            }

        }
      }
    }
      
    int i = 0;
    for (auto BI = succ_begin(BB), BE = succ_end(BB); BI != BE; ++BI) {
      llvm::BasicBlock* succ = *BI;
      std::pair<llvm::BasicBlock*, llvm::BasicBlock*> Bpair = std::make_pair(BB, succ);
      std::pair<std::pair<llvm::BasicBlock*,
                          llvm::BasicBlock*>,
                llvmberry::Mem2RegArg::Tr_Type> BpairFalse = std::make_pair(Bpair, llvmberry::Mem2RegArg::False);
        std::pair<std::pair<llvm::BasicBlock*,
                          llvm::BasicBlock*>,
                llvmberry::Mem2RegArg::Tr_Type> BpairTrue = std::make_pair(Bpair, llvmberry::Mem2RegArg::True);
      std::pair<std::pair<llvm::BasicBlock*,
                          llvm::BasicBlock*>,
                llvmberry::Mem2RegArg::Tr_Type> BpairLoad = std::make_pair(Bpair, llvmberry::Mem2RegArg::LoadStart);

      if (ignore) {
        if (std::find(reachedEdgeTag.begin(), reachedEdgeTag.end(), BpairLoad) != reachedEdgeTag.end()) {
          continue;
        } else if (std::find(reachedEdgeTag.begin(), reachedEdgeTag.end(), BpairTrue) != reachedEdgeTag.end()) { 
          while (!VisitedBlock.empty()) {
            llvm::BasicBlock *current = (*VisitedBlock.rbegin()).first;
            std::pair<std::pair<llvm::BasicBlock*,
                              llvm::BasicBlock*>,
                    llvmberry::Mem2RegArg::Tr_Type> blockPairTag1 = std::make_pair(*VisitedBlock.rbegin(), llvmberry::Mem2RegArg::LoadStart);
            std::pair<std::pair<llvm::BasicBlock*,
                              llvm::BasicBlock*>,
                    llvmberry::Mem2RegArg::Tr_Type> blockPairTag2 = std::make_pair(*VisitedBlock.rbegin(), llvmberry::Mem2RegArg::False);
            std::vector<std::pair<std::pair<llvm::BasicBlock*,
                    llvm::BasicBlock*>,
                    llvmberry::Mem2RegArg::Tr_Type>>::iterator it1 =
                    std::find(reachedEdgeTag.begin(), reachedEdgeTag.end(), blockPairTag1);
            std::vector<std::pair<std::pair<llvm::BasicBlock*,
                    llvm::BasicBlock*>,
                    llvmberry::Mem2RegArg::Tr_Type>>::iterator it2 =
                    std::find(reachedEdgeTag.begin(), reachedEdgeTag.end(), blockPairTag2);
           
          if (it1 != reachedEdgeTag.end()) {
            int pos = std::distance(reachedEdgeTag.begin(), it1);
            reachedEdgeTag.at(pos).second = llvmberry::Mem2RegArg::True;
          } else if (it2 != reachedEdgeTag.end()) {
            int pos = std::distance(reachedEdgeTag.begin(), it2);
            reachedEdgeTag.at(pos).second = llvmberry::Mem2RegArg::True;
          }
          VisitedBlock.pop_back();

          if (AI->getParent() != current) {
            PROPAGATE(
                    LESSDEF(INSN(std::shared_ptr<TyInstruction>(
                                  new ConsLoadInst(TyLoadInst::makeAlignOne(AI)))),
                            VAR(getVariable(*AI), Ghost),
                            SRC),
                    BOUNDS(TyPosition::make_start_of_block(SRC, getBasicBlockIndex(current)),
                           TyPosition::make_end_of_block(SRC, *current, termIndices[getBasicBlockIndex(current)])));

            PROPAGATE(
                    LESSDEF(VAR(getVariable(*AI), Ghost),
                            EXPR(UndefVal, Physical),
                            TGT),
                    BOUNDS(TyPosition::make_start_of_block(SRC, getBasicBlockIndex(current)),
                           TyPosition::make_end_of_block(SRC, *current, termIndices[getBasicBlockIndex(current)])));
          } else {
            PROPAGATE(
                    LESSDEF(INSN(std::shared_ptr<TyInstruction>(
                                  new ConsLoadInst(TyLoadInst::makeAlignOne(AI)))),
                            VAR(getVariable(*AI), Ghost),
                            SRC),
                    BOUNDS(TyPosition::make(SRC, *AI, instrIndices[AI], ""),
                           TyPosition::make_end_of_block(SRC, *current, termIndices[getBasicBlockIndex(current)])));

            PROPAGATE(
                    LESSDEF(VAR(getVariable(*AI), Ghost),
                            EXPR(UndefVal, Physical),
                            TGT),
                    BOUNDS(TyPosition::make(SRC, *AI, instrIndices[AI], ""),
                           TyPosition::make_end_of_block(SRC, *current, termIndices[getBasicBlockIndex(current)])));

            INFRULE(llvmberry::TyPosition::make(SRC, *AI, instrIndices[AI], ""),
                    llvmberry::ConsIntroGhost::make(EXPR(UndefVal, Physical),
                                                    REGISTER(getVariable(*AI), Ghost)));

            INFRULE(llvmberry::TyPosition::make(SRC, *AI, instrIndices[AI], ""),
                    llvmberry::ConsTransitivity::make(INSN(*AI),
                                                      EXPR(UndefVal, Physical),
                                                      VAR(getVariable(*AI), Ghost)));
          }
         } 
          return ;
        } else if (std::find(reachedEdgeTag.begin(), reachedEdgeTag.end(), BpairFalse) != reachedEdgeTag.end() ) {

            std::vector<std::pair<std::pair<llvm::BasicBlock*, llvm::BasicBlock*>,
                                  llvmberry::Mem2RegArg::Tr_Type>>::iterator it =
            std::find(reachedEdgeTag.begin(), reachedEdgeTag.end(), BpairFalse);
            int pos = std::distance(reachedEdgeTag.begin(), it);
            reachedEdgeTag.at(pos).second = llvmberry::Mem2RegArg::True;
          } else { 
          reachedEdgeTag.push_back(BpairLoad);
          VisitedBlock.push_back(Bpair);
          generateHintForMem2RegPHIdelete(succ, VisitedBlock, AI, ignore);
          VisitedBlock.pop_back();
          i++;
        }
      } else { 
        if (std::find(reachedEdgeTag.begin(), reachedEdgeTag.end(), BpairFalse) != reachedEdgeTag.end() || 
            std::find(reachedEdgeTag.begin(), reachedEdgeTag.end(), BpairLoad) != reachedEdgeTag.end()) {
          continue;
        } else if (std::find(reachedEdgeTag.begin(), reachedEdgeTag.end(), BpairTrue) != reachedEdgeTag.end()) { 
        while (!VisitedBlock.empty()) {
          llvm::BasicBlock *current = (*VisitedBlock.rbegin()).first;
           std::pair<std::pair<llvm::BasicBlock*,
                              llvm::BasicBlock*>,
                    llvmberry::Mem2RegArg::Tr_Type> blockPairTag1 = std::make_pair(*VisitedBlock.rbegin(), llvmberry::Mem2RegArg::False);
          std::vector<std::pair<std::pair<llvm::BasicBlock*,
                  llvm::BasicBlock*>,
                  llvmberry::Mem2RegArg::Tr_Type>>::iterator it1 =
                  std::find(reachedEdgeTag.begin(), reachedEdgeTag.end(), blockPairTag1);        

 
           if (it1 != reachedEdgeTag.end()) {
            int pos = std::distance(reachedEdgeTag.begin(), it1);
            reachedEdgeTag.at(pos).second = llvmberry::Mem2RegArg::True;
          }

          VisitedBlock.pop_back();
          if (AI->getParent() != current) {
            PROPAGATE(
                    LESSDEF(INSN(std::shared_ptr<TyInstruction>(
                                  new ConsLoadInst(TyLoadInst::makeAlignOne(AI)))),
                            VAR(getVariable(*AI), Ghost),
                            SRC),
                    BOUNDS(TyPosition::make_start_of_block(SRC, getBasicBlockIndex(current)),
                           TyPosition::make_end_of_block(SRC, *current, termIndices[getBasicBlockIndex(current)])));

            PROPAGATE(
                    LESSDEF(VAR(getVariable(*AI), Ghost),
                            EXPR(UndefVal, Physical),
                            TGT),
                    BOUNDS(TyPosition::make_start_of_block(SRC, getBasicBlockIndex(current)),
                           TyPosition::make_end_of_block(SRC, *current, termIndices[getBasicBlockIndex(current)])));
          } else {
            PROPAGATE(
                    LESSDEF(INSN(std::shared_ptr<TyInstruction>(
                                  new ConsLoadInst(TyLoadInst::makeAlignOne(AI)))),
                            VAR(getVariable(*AI), Ghost),
                            SRC),
                    BOUNDS(TyPosition::make(SRC, *AI, instrIndices[AI], ""),
                           TyPosition::make_end_of_block(SRC, *current, termIndices[getBasicBlockIndex(current)])));

            PROPAGATE(
                    LESSDEF(VAR(getVariable(*AI), Ghost),
                            EXPR(UndefVal, Physical),
                            TGT),
                    BOUNDS(TyPosition::make(SRC, *AI, instrIndices[AI], ""),
                           TyPosition::make_end_of_block(SRC, *current, termIndices[getBasicBlockIndex(current)])));

            INFRULE(llvmberry::TyPosition::make(SRC, *AI, instrIndices[AI], ""),
                    llvmberry::ConsIntroGhost::make(EXPR(UndefVal, Physical),
                                                    REGISTER(getVariable(*AI), Ghost)));

            INFRULE(llvmberry::TyPosition::make(SRC, *AI, instrIndices[AI], ""),
                    llvmberry::ConsTransitivity::make(INSN(*AI),
                                                      EXPR(UndefVal, Physical),
                                                      VAR(getVariable(*AI), Ghost)));
          }
        }
      }  else {
        reachedEdgeTag.push_back(BpairFalse);
        VisitedBlock.push_back(Bpair);
        generateHintForMem2RegPHIdelete(succ, VisitedBlock, AI, ignore);
        VisitedBlock.pop_back();
        i++;
      }
    }
  }
  });
  return;
}

void generateHintForMem2RegPhiUndef(llvm::PHINode* APN, llvm::BasicBlock* Pred) {
  llvmberry::ValidationUnit::GetInstance()->intrude
          ([&APN, &Pred]
           (llvmberry::Dictionary &data, llvmberry::CoreHint &hints) {
    auto &allocas = *(data.get<llvmberry::ArgForMem2Reg>()->allocas);
    auto &instrIndices = *(data.get<llvmberry::ArgForIndices>()->instrIndices);
    auto &termIndices =  *(data.get<llvmberry::ArgForIndices>()->termIndices);
    auto &usePile = *(data.get<llvmberry::ArgForMem2Reg>()->usePile);
    auto &isReachable = *(data.get<llvmberry::ArgForMem2Reg>()->isReachable);
    auto &mem2regCmd = *(data.get<llvmberry::ArgForMem2Reg>()->mem2regCmd);
    auto &blockPairVec = *(data.get<llvmberry::ArgForMem2Reg>()->blockPairVec);
    llvm::AllocaInst* AI = NULL;
    std::string Rphi = getVariable(*APN);
    std::string AIname = Rphi.substr(0, Rphi.rfind("."));
    std::string prev = getBasicBlockIndex(Pred);

    for (auto i = allocas.begin(); i != allocas.end();) {
      llvm::AllocaInst* AItmp = *(i++);
    
      if (getVariable(*AItmp) == AIname)
        AI = AItmp;
    }

    if (AI != NULL) {
      std::string Ralloca = getVariable(*AI);
      for (auto UI = AI->user_begin(), E = AI->user_end(); UI != E;) {
        llvm::Instruction *User = llvm::dyn_cast<llvm::Instruction>(*UI++);

        if (llvm::LoadInst *LI = llvm::dyn_cast<llvm::LoadInst>(User)) {
          if (APN->getParent() == LI ->getParent() ||
              (std::find(isReachable[APN->getParent()].begin(),
                         isReachable[APN->getParent()].end(),
                         LI->getParent()) != isReachable[APN->getParent()].end())) {
            llvm::PHINode *check = NULL;
            llvm::StoreInst *ST = NULL;
            blockPairVec.clear();
            llvm::Instruction *tmp = properPHI(LI->getParent(), Ralloca, APN, true, true, data);

            if (tmp != NULL) {
              check = llvm::dyn_cast<llvm::PHINode>(tmp);
              ST = llvm::dyn_cast<llvm::StoreInst>(tmp);
            }

            if (ST != NULL) {
              if ((ST->getParent() == LI->getParent()) && (instrIndices[ST] > instrIndices[LI])) {
                blockPairVec.clear();
                llvm::Instruction *tmpI = properPHI(LI->getParent(), Ralloca, APN, true, false, data); //to check there is phi in same block as LI
                llvm::PHINode *checkPHI = NULL;

                if (tmpI != NULL) {
                  checkPHI = llvm::dyn_cast<llvm::PHINode>(tmpI);
                }

                if (checkPHI != NULL) {
                  std::string Rphi = llvmberry::getVariable(*checkPHI);

                  PROPAGATE(
                          LESSDEF(INSN(std::shared_ptr<llvmberry::TyInstruction>(
                                          new llvmberry::ConsLoadInst(llvmberry::TyLoadInst::makeAlignOne(AI)))),
                                  VAR(Ralloca, Ghost),
                                  SRC),
                          BOUNDS(llvmberry::TyPosition::make_start_of_block(SRC, llvmberry::getBasicBlockIndex(checkPHI->getParent())),
                                 llvmberry::TyPosition::make(SRC, *LI, instrIndices[LI], "")));

                  std::shared_ptr<llvmberry::TyPropagateLessdef> lessdef = llvmberry::TyPropagateLessdef::make
                          (VAR(Ralloca, Ghost),
                           VAR(Rphi, Physical), TGT);

                  PROPAGATE(std::shared_ptr<llvmberry::TyPropagateObject>(new llvmberry::ConsLessdef(lessdef)),
                            BOUNDS(llvmberry::TyPosition::make_start_of_block(SRC, llvmberry::getBasicBlockIndex(checkPHI->getParent())),
                                   llvmberry::TyPosition::make(SRC, *LI, instrIndices[LI], "")));

                  mem2regCmd[Rphi].lessdef.push_back(lessdef);
                }
              }
            }

            if (check == APN) {
              PROPAGATE(
                      LESSDEF(INSN(std::shared_ptr<llvmberry::TyInstruction>(
                                      new llvmberry::ConsLoadInst(llvmberry::TyLoadInst::makeAlignOne(AI)))),
                              VAR(Ralloca, Ghost),
                              SRC),
                      BOUNDS(llvmberry::TyPosition::make_start_of_block(SRC, llvmberry::getBasicBlockIndex(APN->getParent())),
                             llvmberry::TyPosition::make(SRC, *LI, instrIndices[LI], "")));

              std::shared_ptr<llvmberry::TyPropagateLessdef> lessdef = llvmberry::TyPropagateLessdef::make
                      (VAR(Ralloca, Ghost),
                       VAR(Rphi, Physical), TGT);

              PROPAGATE(std::shared_ptr<llvmberry::TyPropagateObject>(new llvmberry::ConsLessdef(lessdef)),
                        BOUNDS(llvmberry::TyPosition::make_start_of_block(SRC, llvmberry::getBasicBlockIndex(APN->getParent())),
                               llvmberry::TyPosition::make(SRC, *LI, instrIndices[LI], "")));
              mem2regCmd[Rphi].lessdef.push_back(lessdef);
            } else if (check != NULL) {
              std::string Rphi = llvmberry::getVariable(*check);

              PROPAGATE(
                      LESSDEF(INSN(std::shared_ptr<llvmberry::TyInstruction>(
                                      new llvmberry::ConsLoadInst(llvmberry::TyLoadInst::makeAlignOne(AI)))),
                              VAR(Ralloca, Ghost),
                              SRC),
                      BOUNDS(llvmberry::TyPosition::make_start_of_block(SRC, llvmberry::getBasicBlockIndex(
                                     check->getParent())),
                             llvmberry::TyPosition::make(SRC, *LI, instrIndices[LI], "")));

              std::shared_ptr<llvmberry::TyPropagateLessdef> lessdef = llvmberry::TyPropagateLessdef::make
                      (VAR(Ralloca, Ghost),
                       VAR(Rphi, Physical), TGT);

              PROPAGATE(std::shared_ptr<llvmberry::TyPropagateObject>(new llvmberry::ConsLessdef(lessdef)),
                        BOUNDS(llvmberry::TyPosition::make_start_of_block(SRC, llvmberry::getBasicBlockIndex(
                                       check->getParent())),
                               llvmberry::TyPosition::make(SRC, *LI, instrIndices[LI], "")));
              mem2regCmd[Rphi].lessdef.push_back(lessdef);
            }
          }

          for (auto UI2 = usePile[LI].begin(), E2 = usePile[LI].end(); UI2 != E2;) {
            auto t = *(UI2++);

            int useIndex =
                    llvmberry::getIndexofMem2Reg(std::get<2>(t), std::get<1>(t),
                                                 termIndices[llvmberry::getBasicBlockIndex(std::get<0>(t))]);

            llvmberry::generateHintForMem2RegPropagateLoad(AI, LI, std::get<0>(t),
                                                           useIndex, std::get<2>(t));
          }
        }
      }
    }
  });
}

void generateHintForMem2RegPHI(llvm::BasicBlock *BB, llvm::BasicBlock *Pred,
                               llvm::AllocaInst *AI, llvm::StoreInst *SI,
                               llvm::BasicBlock::iterator II,
                               llvm::DenseMap<llvm::PHINode *, unsigned> PAM,
                               llvm::DenseMap<llvm::AllocaInst *, unsigned> AL,
                               bool isSameBB) {
  ValidationUnit::GetInstance()->intrude([&BB, &Pred, &AI, &SI, &II, &PAM, &AL,
                                          &isSameBB](Dictionary &data,
                                                     CoreHint &hints) {
    if (BB == NULL || AI == NULL || SI == NULL)
      return;

    llvm::StoreInst *SItmp = SI;
    llvm::AllocaInst *AItmp = AI;

    // prepare variables
    auto &instrIndices = *(data.get<ArgForIndices>()->instrIndices);
    auto &termIndices = *(data.get<ArgForIndices>()->termIndices);
    auto &usePile = *(data.get<ArgForMem2Reg>()->usePile);
    auto &storeItem = *(data.get<ArgForMem2Reg>()->storeItem);
    auto &transTgt = *(data.get<ArgForMem2Reg>()->transTgt);
    auto &mem2regCmd = *(data.get<ArgForMem2Reg>()->mem2regCmd);
    auto &blockPairVec = *(data.get<ArgForMem2Reg>()->blockPairVec);
    auto &reachedEdge = *(data.get<ArgForMem2Reg>()->reachedEdge);
    auto &isReachable = *(data.get<ArgForMem2Reg>()->isReachable);
    std::string bname = getBasicBlockIndex(SItmp->getParent());
    std::string Ralloca = getVariable(*AItmp);
    std::string Rstore = getVariable(*SItmp->getOperand(1));
    llvm::BasicBlock *BBtmp = BB;
    llvm::BasicBlock *Predtmp = Pred;
    llvm::succ_iterator BI = succ_begin(BB), BE = succ_end(BB);
    llvm::BasicBlock::iterator IItmp = II;

    do {
      bool newStore = false;
      if (!isSameBB) {
        Predtmp = BB;
        BBtmp = *BI++;
        IItmp = BBtmp->begin();

        // do not check same block again
        std::pair <llvm::BasicBlock *, llvm::BasicBlock *> tmp = std::make_pair(Predtmp, BBtmp);
        if (std::find(reachedEdge[Ralloca].begin(), reachedEdge[Ralloca].end(), tmp) != reachedEdge[Ralloca].end())
          continue;
        reachedEdge[Ralloca].push_back(tmp);

        // if this successor block has PHI,
        // propagate store or another PHI to current PHI
        // according to several conditions
        if (llvm::PHINode *PHI = llvm::dyn_cast<llvm::PHINode>(BBtmp->begin())) {
          llvm::BasicBlock::iterator PNI = BBtmp->begin();

          while (PHI) {
            std::string Rphi = getVariable(*PHI);

            // <Condition 1>
            // check if one of incoming values of PHI is
            // stored value of current SI
            if (PAM.count(PHI) &&
                // fragile: this condition relies on llvm naming convention of PHI
                Rstore == Rphi.substr(0, Rphi.rfind("."))) {

              // check if there is other PHI between
              // SI block and current PHI block.
              // compiler can choose either 2-1 or 2-2 according to this
              blockPairVec.clear();
              llvm::Instruction* Itmp = properPHI(Predtmp, Rstore, SItmp, true, true, data);

              // <Condition 2-1>
              // if there is no other PHI between SI and current PHI,
              // we can propagate store to current PHI
              if (Itmp == NULL) {
                generateHintForMem2RegPropagateStore(Predtmp, SItmp, PHI, termIndices[getBasicBlockIndex(Predtmp)]);
              } else if (llvm::PHINode* PHItmp = llvm::dyn_cast<llvm::PHINode>(Itmp)) {
              // <Condition 2-2>
              // if we find another PHI(PHItmp) after SI,
              // propagate PHItmp to current PHI
                std::string Rphitmp = getVariable(*PHItmp);

                PROPAGATE(
                    LESSDEF(INSN(std::shared_ptr<TyInstruction>(
                              new ConsLoadInst(TyLoadInst::makeAlignOne(SItmp)))),
                            VAR(Rstore, Ghost), SRC),
                    BOUNDS(TyPosition::make_start_of_block(
                               SRC, getBasicBlockIndex(PHItmp->getParent())),
                           TyPosition::make_end_of_block(SRC, *Predtmp, termIndices[getBasicBlockIndex(Predtmp)])));

                std::shared_ptr<TyPropagateLessdef> lessdef =
                  TyPropagateLessdef::make(VAR(Rstore, Ghost),
                                           VAR(Rphitmp, Physical),
                                           TGT);

                mem2regCmd[Rphitmp].lessdef.push_back(lessdef);

                PROPAGATE(
                    std::shared_ptr<TyPropagateObject>(new ConsLessdef(lessdef)),
                    BOUNDS(TyPosition::make_start_of_block(
                               SRC, getBasicBlockIndex(PHItmp->getParent())),
                           TyPosition::make_end_of_block(SRC, *Predtmp, termIndices[getBasicBlockIndex(Predtmp)])));

                std::shared_ptr<TyTransitivityTgt> transitivitytgt
                  (new TyTransitivityTgt(VAR(Rstore, Ghost),
                                         VAR(Rphitmp, Previous),
                                         VAR(Rphi, Physical)));
                mem2regCmd[Rphitmp].transTgt.push_back(transitivitytgt);
                transTgt.push_back(transitivitytgt);

                INFRULE(TyPosition::make(TGT, PHI->getParent()->getName(),
                                         Predtmp->getName()),
                        std::shared_ptr<TyInfrule>(new ConsTransitivityTgt(transitivitytgt)));
              } else if (llvm::StoreInst* SISI = llvm::dyn_cast<llvm::StoreInst>(Itmp)) {
                if ((SItmp->getParent() != SISI->getParent() &&
                     (std::find(isReachable[SItmp->getParent()].begin(),
                                isReachable[SItmp->getParent()].end(),
                                SISI->getParent()) != isReachable[SItmp->getParent()].end())) ||
                    (SItmp->getParent() == SISI->getParent() && instrIndices[SItmp]<instrIndices[SISI]))
                  generateHintForMem2RegPropagateStore(Predtmp, SItmp, SISI, instrIndices[SISI]);
                else if (SItmp == SISI)
                  generateHintForMem2RegPropagateStore(Predtmp, SItmp, PHI, termIndices[getBasicBlockIndex(Predtmp)]);
              }

              std::string SIname = SItmp->getOperand(0)->getName();
              // this step apply infrules
              if (storeItem[SItmp].expr != NULL &&
                  (storeItem[SItmp].op0 == "%" ||
                   SIname == "" ||
                   SIname != "" ||
                   data.get<ArgForMem2Reg>()->equalsIfConsVar(storeItem[SItmp].expr, 
                                                              TyExpr::make(*(SItmp->getOperand(0)),
                                                                           Physical)))) {
                //check transtgt
                std::shared_ptr<TyTransitivityTgt> transitivitytgt
                  (new TyTransitivityTgt(VAR(Rstore, Ghost),
                                         EXPR(SItmp->getOperand(0), Physical),
                                         VAR(Rphi, Physical)));

                INFRULE(TyPosition::make(TGT, PHI->getParent()->getName(),
                                         Predtmp->getName()),
                        std::shared_ptr<TyInfrule>(new ConsTransitivityTgt(transitivitytgt)));

                if (storeItem[SItmp].op0 != "%") {
                  transTgt.push_back(transitivitytgt);
                }

                if (SIname != "") {
                  mem2regCmd["%"+SIname].transTgt.push_back(transitivitytgt);
                }
                //check end
              }

              // add hints per every use of PHI
              for (auto UI = PHI->use_begin(), UE = PHI->use_end(); UI != UE;) {
                llvm::Use &U = *(UI++);
                llvm::Instruction *use =
                    llvm::dyn_cast<llvm::Instruction>(U.getUser());

                int useIndex = getIndexofMem2Reg(
                    use, instrIndices[use],
                    termIndices[getBasicBlockIndex(use->getParent())]);

                // if use is other PHI
                if (llvm::isa<llvm::PHINode>(use)) {
                  llvm::BasicBlock* source = PHI->getParent();
                  llvm::BasicBlock* target = use->getParent();
                  
                  for (auto UI2 = pred_begin(target), UE2 = pred_end(target); UI2 != UE2;) {
                    llvm::BasicBlock* usePred = *(UI2++);
                    blockPairVec.clear();
                    llvm::PHINode* PHItmp = NULL;

                    // check properPHI only if there is no incoming value from usePred in PHI
                    if (PHI->getBasicBlockIndex(usePred) != -1)
                      if (PHI->getIncomingValueForBlock(usePred) == NULL) {
                        if (llvm::Instruction* Itmp = properPHI(usePred, Rstore, SItmp, true, true, data))
                          PHItmp = llvm::dyn_cast<llvm::PHINode>(Itmp);
                      }

                    if (PHItmp == PHI &&
                        (std::find(isReachable[source].begin(),
                                   isReachable[source].end(),
                                   usePred) != isReachable[source].end())) {
                      PROPAGATE(
                          LESSDEF(INSN(std::shared_ptr<TyInstruction>(
                                    new ConsLoadInst(TyLoadInst::makeAlignOne(SItmp)))),
                                  VAR(Rstore, Ghost), SRC),
                          BOUNDS(TyPosition::make_start_of_block(
                                 SRC, getBasicBlockIndex(PHI->getParent())),
                                 TyPosition::make_end_of_block(SRC, *usePred, termIndices[getBasicBlockIndex(usePred)])));

                      std::shared_ptr<TyPropagateLessdef> lessdef = TyPropagateLessdef::make
                                                                    (VAR(Rstore,Ghost),
                                                                     VAR(Rphi, Physical),
                                                                     TGT);
                      mem2regCmd[Rphi].lessdef.push_back(lessdef);

                      PROPAGATE(
                          std::shared_ptr<TyPropagateObject>(new ConsLessdef(lessdef)),
                          BOUNDS(TyPosition::make_start_of_block(
                                 SRC, getBasicBlockIndex(PHI->getParent())),
                                 TyPosition::make_end_of_block(SRC, *usePred, termIndices[getBasicBlockIndex(usePred)])));

                      std::string useName = "%"+std::string(use->getName());

                      std::shared_ptr<TyTransitivityTgt> transitivitytgt
                        (new TyTransitivityTgt(VAR(Rstore, Ghost),
                                               VAR(Rphi, Previous),
                                               VAR(useName, Physical)));
                     
                      INFRULE(TyPosition::make(TGT, target->getName(),
                                               usePred->getName()),
                              std::shared_ptr<TyInfrule>(new ConsTransitivityTgt(transitivitytgt)));
                     
                      mem2regCmd[Rphi].transTgt.push_back(transitivitytgt);

                    }
                  }
                } else if (llvm::isa<llvm::LoadInst>(use) &&
                           (use->getOperand(0) == SItmp->getOperand(1))) {
                  PROPAGATE(
                      LESSDEF(INSN(std::shared_ptr<TyInstruction>(
                                new ConsLoadInst(TyLoadInst::makeAlignOne(SItmp)))),
                              VAR(Rstore, Ghost), SRC),
                      BOUNDS(TyPosition::make_start_of_block(
                             SRC, getBasicBlockIndex(PHI->getParent())),
                             TyPosition::make(SRC, *use, useIndex, "")));

                  std::shared_ptr<TyPropagateLessdef> lessdef = TyPropagateLessdef::make
                                                                (VAR(Rstore, Ghost),
                                                                 VAR(Rphi, Physical),
                                                                 TGT);              
                  mem2regCmd[Rphi].lessdef.push_back(lessdef);
          
                  PROPAGATE(std::shared_ptr<TyPropagateObject>(new ConsLessdef(lessdef)),
                      BOUNDS(TyPosition::make_start_of_block(
                             SRC, getBasicBlockIndex(PHI->getParent())),
                             TyPosition::make(SRC, *use, useIndex, "")));
                }
              }
            }
            PNI++;
            PHI = llvm::dyn_cast<llvm::PHINode>(PNI);
          }
        }
      }

      // iterate instructions in current block
      for (llvm::BasicBlock::iterator Iiter = IItmp;
           !llvm::isa<llvm::TerminatorInst>(Iiter);) {
        llvm::Instruction *I = Iiter++;
        if (llvm::LoadInst *LI = llvm::dyn_cast<llvm::LoadInst>(I)) {
          if (getVariable(*(LI->getOperand(0))) != Ralloca)
            continue;

          llvm::AllocaInst *Src =
              llvm::dyn_cast<llvm::AllocaInst>(LI->getPointerOperand());
          if (!Src)
            continue;

          llvm::DenseMap<llvm::AllocaInst *, unsigned>::iterator AImap =
              AL.find(Src);
          if (AImap == AL.end())
            continue;

          // prepare variables
          std::string Rload = getVariable(*LI);
          blockPairVec.clear();
          llvm::PHINode* PHI = NULL;

          if (llvm::Instruction* Itmp = properPHI(LI->getParent(), Rstore, SItmp, true, false, data))
            PHI = llvm::dyn_cast<llvm::PHINode>(Itmp);

          if (PHI != NULL) {
            std::string Rphi = getVariable(*PHI);

            PROPAGATE(
                LESSDEF(INSN(std::shared_ptr<TyInstruction>(
                          new ConsLoadInst(TyLoadInst::makeAlignOne(SItmp)))),
                        VAR(Rstore, Ghost), SRC),
                BOUNDS(TyPosition::make_start_of_block(
                           SRC, getBasicBlockIndex(PHI->getParent())),
                       TyPosition::make(SRC, *LI, instrIndices[LI], "")));

            std::shared_ptr<TyPropagateLessdef> lessdef = TyPropagateLessdef::make
                  (VAR(Rstore, Ghost),
                   VAR(Rphi, Physical), TGT);

            PROPAGATE(std::shared_ptr<TyPropagateObject>(new ConsLessdef(lessdef)),
                BOUNDS(TyPosition::make_start_of_block(
                           SRC, getBasicBlockIndex(PHI->getParent())),
                       TyPosition::make(SRC, *LI, instrIndices[LI], "")));

            mem2regCmd[Rphi].lessdef.push_back(lessdef);

            if (storeItem[SItmp].expr != NULL &&
                (storeItem[SItmp].op0 == "%" ||
                 SItmp->getOperand(0)->getName() == "" ||
                 SItmp->getOperand(0)->getName() != "" ||
                 data.get<ArgForMem2Reg>()->equalsIfConsVar(storeItem[SItmp].expr, 
                                                            TyExpr::make(*(SItmp->getOperand(0)),
                                                                        Physical)))) {
              //check transtgt
              std::shared_ptr<TyTransitivityTgt> transitivitytgt
                (new TyTransitivityTgt(VAR(Rstore, Ghost),
                                       storeItem[SItmp].expr,
                                       VAR(getVariable(*PHI), Physical)));

              mem2regCmd[getVariable(*PHI)].transTgt.push_back(transitivitytgt);

              INFRULE(TyPosition::make(TGT, LI->getParent()->getName(),
                                       Predtmp->getName()),
                      std::shared_ptr<TyInfrule>(new ConsTransitivityTgt(transitivitytgt)));

              if (storeItem[SItmp].op0 != "%")
                transTgt.push_back(transitivitytgt);
              //check end
            }
          }

          PROPAGATE(PRIVATE(REGISTER(Ralloca, Physical), SRC),
                    BOUNDS(TyPosition::make(SRC, *AItmp, instrIndices[AItmp], ""),
                           TyPosition::make(SRC, *LI, instrIndices[LI], "")));

          if ((getVariable(*LI->getOperand(0)) == Rstore) &&
              ((LI->getParent() == SItmp->getParent() &&
                instrIndices[SItmp] < instrIndices[LI]) ||
               (std::find(isReachable[SItmp->getParent()].begin(),
                          isReachable[SItmp->getParent()].end(),
                          LI->getParent()) != isReachable[SItmp->getParent()].end()))) {
            blockPairVec.clear();
            llvm::PHINode* PHI = NULL;

            if (llvm::Instruction* Itmp = properPHI(LI->getParent(), Rstore, SItmp, true, false, data))
              PHI = llvm::dyn_cast<llvm::PHINode>(Itmp);

            if (PHI == NULL) {
              generateHintForMem2RegPropagateStore(Predtmp, SItmp, LI, instrIndices[LI]);
            }

            // add hints per every use of LI
            for (auto UI = usePile[LI].begin(), E = usePile[LI].end(); UI != E;) {
              auto t = *(UI++);
              llvm::BasicBlock* useBB = std::get<0>(t);
              int useIndex =
                llvmberry::getIndexofMem2Reg(std::get<2>(t), std::get<1>(t),
                                             termIndices[llvmberry::getBasicBlockIndex(std::get<0>(t))]);

              // set index of use
              if ((LI->getParent() == useBB &&
                   instrIndices[LI] < std::get<1>(t)) ||
                  (std::find(isReachable[LI->getParent()].begin(),
                             isReachable[LI->getParent()].end(),
                             useBB) != isReachable[LI->getParent()].end())) {
                generateHintForMem2RegPropagateLoad(SItmp, LI, useBB, useIndex, std::get<2>(t));
              }
            }
          }

          // propagate maydiff
          propagateMaydiffGlobal(Rload, Physical);
          propagateMaydiffGlobal(Rload, Previous);
        } else if (llvm::StoreInst *SItmp2 =
                       llvm::dyn_cast<llvm::StoreInst>(I)) {
          // if we find new store to same alloca,
          // stop searching remaining instructions
          if (getVariable(*(SItmp2->getOperand(1))) == Ralloca) {
            newStore = true;
            break;
          } else
            continue;
        }
      }

      // if we find new store to same alloca in same block,
      // stop propagating hint for this store
      if (isSameBB && newStore)
        break;
      
      if (!isSameBB) {
        BI--;
        if (BI != BE && !newStore) {
          generateHintForMem2RegPHI(BBtmp, Predtmp, AItmp, SItmp, IItmp,
                                    PAM, AL, true);
        }
        BI++;
      }
      isSameBB = false;
    } while (BI != BE);
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
  (llvm::Instruction* I) {
  std::shared_ptr<std::vector<std::shared_ptr<TyPosition>>> destSet(
          new std::vector<std::shared_ptr<TyPosition>>());
  
  ValidationUnit::GetInstance()->intrude
    ([&I, &destSet]
     (Dictionary &data, CoreHint &hints) {
    auto &termIndices = *(data.get<ArgForIndices>()->termIndices);
    auto &useIndices = *(data.get<ArgForIndices>()->useIndices);

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
  });

  return destSet;
}

void saveInstrIndices(llvm::Function* F) {
  ValidationUnit::GetInstance()->intrude
    ([&F]
     (Dictionary &data, CoreHint &hints) {
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
  });
}

void saveUseIndices(llvm::Function* F, unsigned opCode) {
  ValidationUnit::GetInstance()->intrude
    ([&F, &opCode]
     (Dictionary &data, CoreHint &hints) {
    auto &instrIndices = *(data.get<ArgForIndices>()->instrIndices);
    auto &useIndices = *(data.get<ArgForIndices>()->useIndices);

    for (auto BS = F->begin(), BE = F->end(); BS != BE;) {
      llvm::BasicBlock* BB = BS++;
      for (auto IS = BB->begin(), IE = BB->end(); IS != IE;) {
        llvm::Instruction* I = IS++;

        if (I->getOpcode() == opCode) {
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
  });
}

void saveInstrInfo(llvm::Instruction* I, unsigned key) {
  ValidationUnit::GetInstance()->intrude
    ([&I, &key]
     (Dictionary &data, CoreHint &hints) {
    auto &instrIndices = *(data.get<ArgForIndices>()->instrIndices);
    auto &recentInstr = *(data.get<ArgForMem2Reg>()->recentInstr);

    if (llvm::AllocaInst* AI = llvm::dyn_cast<llvm::AllocaInst>(I)) {
      llvm::Value* UndefVal = llvm::UndefValue::get(AI->getAllocatedType());

      recentInstr[key].instrL = INSN(std::shared_ptr<llvmberry::TyInstruction>
                                      (new llvmberry::ConsLoadInst
                                        (llvmberry::TyLoadInst::makeAlignOne(AI))));
      recentInstr[key].instrR = EXPR(UndefVal, Physical);
      recentInstr[key].instrVal = nullptr;
      recentInstr[key].instrPos = llvmberry::TyPosition::make(SRC, *AI, instrIndices[AI], ""); 
      recentInstr[key].op0 = "";
      recentInstr[key].op1 = "";
      recentInstr[key].instrBB = AI->getParent();
    } else if (llvm::StoreInst* SI = llvm::dyn_cast<llvm::StoreInst>(I)) {
      recentInstr[key].instrL = INSN(std::shared_ptr<llvmberry::TyInstruction>
                                      (new llvmberry::ConsLoadInst(llvmberry::TyLoadInst::makeAlignOne(SI))));
      recentInstr[key].instrR = llvmberry::TyExpr::make(*(SI->getOperand(0)), llvmberry::Physical);
      recentInstr[key].instrVal = llvmberry::TyValue::make(*(SI->getOperand(0)));
      recentInstr[key].instrPos = llvmberry::TyPosition::make(SRC, *SI, instrIndices[SI], "");
      recentInstr[key].op0 = llvmberry::getVariable(*(SI->getOperand(0)));
      recentInstr[key].op1 = llvmberry::getVariable(*(SI->getOperand(1)));
      recentInstr[key].instrBB = SI->getParent();
    }
  });
}

} // llvmberry
