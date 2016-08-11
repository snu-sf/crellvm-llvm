#include "llvm/LLVMBerry/ValidationUnit.h"
#include "llvm/LLVMBerry/Dictionary.h"
#include "llvm/LLVMBerry/Hintgen.h"
#include "llvm/LLVMBerry/Infrules.h"

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
      switch (expression->getOpcode()) {
      case llvm::Instruction::Add:
        hints.addCommand(ConsInfrule::make(
            TyPosition::make(Source, *position),
            ConsAddCommutative::make(TyRegister::make(regname, Physical),
                                     TyValue::make(*expression->getOperand(0)),
                                     TyValue::make(*expression->getOperand(1)),
                                     ConsSize::make(bitwidth))));
        break;
      case llvm::Instruction::And:
        hints.addCommand(ConsInfrule::make(
            TyPosition::make(Source, *position),
            ConsAndCommutative::make(TyRegister::make(regname, Physical),
                                     TyValue::make(*expression->getOperand(0)),
                                     TyValue::make(*expression->getOperand(1)),
                                     ConsSize::make(bitwidth))));
        break;
      case llvm::Instruction::Mul:
        hints.addCommand(ConsInfrule::make(
            TyPosition::make(Source, *position),
            ConsMulCommutative::make(TyRegister::make(regname, Physical),
                                     TyValue::make(*expression->getOperand(0)),
                                     TyValue::make(*expression->getOperand(1)),
                                     ConsSize::make(bitwidth))));
        break;
      case llvm::Instruction::Or:
        hints.addCommand(ConsInfrule::make(
            TyPosition::make(Source, *position),
            ConsOrCommutative::make(TyRegister::make(regname, Physical),
                                    TyValue::make(*expression->getOperand(0)),
                                    TyValue::make(*expression->getOperand(1)),
                                    ConsSize::make(bitwidth))));
        break;
      case llvm::Instruction::Xor:
        hints.addCommand(ConsInfrule::make(
            TyPosition::make(Source, *position),
            ConsXorCommutative::make(TyRegister::make(regname, Physical),
                                     TyValue::make(*expression->getOperand(0)),
                                     TyValue::make(*expression->getOperand(1)),
                                     ConsSize::make(bitwidth))));
        break;
      default:
        assert("applyCommutativity() : we don't support commutativity rule for "
               "this binary operator");
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
      case llvm::Instruction::Or:
        INFRULE(
            INSTPOS(llvmberry::Source, position),
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
  assert(ValidationUnit::Exists());

  ValidationUnit::GetInstance()->intrude(
      [&position, &v_smallest, &v_mid, &v_greatest,
       &scope](ValidationUnit::Dictionary &data, CoreHint &hints) {
        hints.addCommand(ConsInfrule::make(
            INSTPOS(scope, position),
            ConsTransitivity::make(TyExpr::make(*v_greatest, Physical),
                                   TyExpr::make(*v_mid, Physical),
                                   TyExpr::make(*v_smallest, Physical))));
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

  ValidationUnit::GetInstance()->intrude([&source, &replaceTo, &ghostvar,
                                          &source_pos](
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
          INFRULE(TyPosition::make(SRC, *user_I, prev_block_name),
                  ConsTransitivity::make(VAR(user, Physical),
                                         VAR(to_rem, Physical),
                                         VAR(ghostvar, Ghost)));
          INFRULE(TyPosition::make(TGT, *user_I, prev_block_name),
                  ConsTransitivity::make(VAR(user, Physical),
                                         VAR(ghostvar, Ghost),
                                         EXPR(replaceTo, Physical)));
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
          ConsAddCommutative::make(TyRegister::make(reg_z_name, Physical),
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
// Copied from ValueTable::create_expression() in GVN.cpp

// This function generates a symbolic expressions from an
// instruction that is used to decide the equivalence of values

// We copied this function because this is a private member
// function of the ValueTable class, so we cannot access it
// while generating hint.

// We modified this function to take the vector of value numbers
// of I's operands. The original function can obtain the value
// numbers from the ValueTable instance, but there's no way to
// see the ValueTable class here, since its definition is in an
// anonymous namespace in GVN.cpp

Expression create_expression(llvm::Instruction *I, bool &swapped,
                             llvm::SmallVector<uint32_t, 4> va) {
  swapped = false;
  Expression e;
  e.type = I->getType();
  e.opcode = I->getOpcode();
  e.varargs = va;

  if (I->isCommutative()) {
    assert(I->getNumOperands() == 2 && "Unsupported commutative instruction!");
    if (e.varargs[0] > e.varargs[1]) {
      swapped = true;
      std::swap(e.varargs[0], e.varargs[1]);
    }
  }

  if (llvm::CmpInst *C = llvm::dyn_cast<llvm::CmpInst>(I)) {
    llvm::CmpInst::Predicate Predicate = C->getPredicate();
    if (e.varargs[0] > e.varargs[1]) {
      std::swap(e.varargs[0], e.varargs[1]);
      Predicate = llvm::CmpInst::getSwappedPredicate(Predicate);
    }
    e.opcode = (C->getOpcode() << 8) | Predicate;
  } else if (llvm::InsertValueInst *E =
                 llvm::dyn_cast<llvm::InsertValueInst>(I)) {
    for (llvm::InsertValueInst::idx_iterator II = E->idx_begin(),
                                             IE = E->idx_end();
         II != IE; ++II)
      e.varargs.push_back(*II);
  }

  return e;
}

bool is_inverse_expression(Expression e1, Expression e2) {
  if (e1.varargs == e2.varargs) {
    uint32_t orig_opcode1 = e1.opcode >> 8;
    uint32_t orig_opcode2 = e2.opcode >> 8;
    llvm::CmpInst::Predicate p1 = (llvm::CmpInst::Predicate)(e1.opcode & 255U);
    llvm::CmpInst::Predicate p2 = (llvm::CmpInst::Predicate)(e2.opcode & 255U);
    if ((orig_opcode1 == orig_opcode2) &&
        (orig_opcode1 == llvm::Instruction::ICmp) &&
        (llvm::CmpInst::getInversePredicate(p1) == p2))
      return true;
  }
  return false;
}

void propagateStoreNoalias(llvm::AllocaInst* AI,
                           llvm::BasicBlock* BB,
                           llvm::Instruction *useInst,
                           int useIndex, bool isInit,
                           std::vector<llvm::BasicBlock*> succs,
                           std::vector<std::string> regs) {
  ValidationUnit::GetInstance()->intrude([&AI, &BB, &useInst, &useIndex,
                                          &isInit, &succs, &regs]
      (Dictionary &data, CoreHint &hints) {
    if (isInit) {
      for (auto II = BB->begin();
           !llvm::isa<llvm::TerminatorInst>(II);) {
        llvm::Instruction *I = II++;

        if (llvm::StoreInst *SI =
              llvm::dyn_cast<llvm::StoreInst>(I)) {
          // if we already deal with this SI, skip this SI
          if (std::find(regs.begin(), regs.end(),
                        SI->getOperand(1)->getName()) != regs.end())
            continue;

          succs.push_back(BB);
          
          PROPAGATE(NOALIAS(POINTER(AI), POINTER(SI->getOperand(1)), SRC),
                    BOUNDS(TyPosition::make(SRC, *SI, getCommandIndex(*SI), ""),
                           TyPosition::make(SRC, *SI, getCommandIndex(*SI)+1, "")));
          
          INFRULE(TyPosition::make(SRC, *SI, getCommandIndex(*SI), ""),
                  ConsDiffblockNoalias::make(VAL(AI, Physical),
                                             VAL(SI->getOperand(1), Physical),
                                             POINTER(AI),
                                             POINTER(SI->getOperand(1))));
        } else {
          continue;
        }
      }
    }

    for (auto BI = succ_begin(BB), BE = succ_end(BB); BI != BE;) {
      llvm::BasicBlock* BBtmp = *(BI++);

      if (std::find(succs.begin(), succs.end(), BBtmp) != succs.end())
        break;
      succs.push_back(BBtmp);

      for (auto II = BBtmp->begin();
           !llvm::isa<llvm::TerminatorInst>(II);) {
        llvm::Instruction *I = II++;

    std::cout<<"storeNoalias check: "+getBasicBlockIndex(BBtmp)<<std::endl;
        if (llvm::StoreInst *SI =
              llvm::dyn_cast<llvm::StoreInst>(I)) {
          std::cout<<"storeNoalias check: "<<std::string(SI->getOperand(1)->getName())<<std::endl;
          // if we already deal with this SI, skip this SI
          if (std::find(regs.begin(), regs.end(),
                        SI->getOperand(1)->getName()) != regs.end())
            continue;

          if (isPotentiallyReachable(SI->getParent(), useInst->getParent()) ||
              (SI->getParent()==useInst->getParent() &&
               getCommandIndex(*SI)<getCommandIndex(*useInst))) {
            
            PROPAGATE(NOALIAS(POINTER(AI), POINTER(SI->getOperand(1)), SRC),
                      BOUNDS(TyPosition::make(SRC, *SI, getCommandIndex(*SI), ""),
                             TyPosition::make(SRC, *SI, getCommandIndex(*SI)+1, "")));
            
            INFRULE(TyPosition::make(SRC, *SI, getCommandIndex(*SI), ""),
                    ConsDiffblockNoalias::make(VAL(AI, Physical),
                                               VAL(SI->getOperand(1), Physical),
                                               POINTER(AI),
                                               POINTER(SI->getOperand(1))));
          }
        } else {
          continue;
        }
      }
      propagateStoreNoalias(AI, BBtmp, useInst, useIndex, false, succs, regs);
    }
  });
}

void generateHintForMem2RegPropagateNoalias(llvm::AllocaInst *AI,
                                            llvm::Instruction *useInst,
                                            int useIndex) {
  ValidationUnit::GetInstance()->intrude([&AI, &useInst, &useIndex](
      Dictionary &data, CoreHint &hints) {
    auto &allocas = *(data.get<ArgForMem2Reg>()->allocas);
    auto &instrIndex = *(data.get<ArgForMem2Reg>()->instrIndex);
    std::vector<llvm::BasicBlock*> succs;
    std::vector<std::string> regs;

    regs.push_back(AI->getName());
    for (auto i = allocas.begin(); i != allocas.end(); ++i) {
      llvm::AllocaInst *AItmp = *i;

      if (AI == AItmp)
        continue;

      regs.push_back(AItmp->getName());

      if (instrIndex[AI] < instrIndex[AItmp]) {
        PROPAGATE(NOALIAS(POINTER(AI), POINTER(AItmp), SRC),
                  BOUNDS(TyPosition::make(SRC, *AItmp, instrIndex[AItmp], ""),
                         TyPosition::make(SRC, *useInst, useIndex, "")));

        INFRULE(TyPosition::make(SRC, *AItmp, instrIndex[AItmp], ""),
                ConsDiffblockNoalias::make(VAL(AI, Physical),
                                           VAL(AItmp, Physical),
                                           POINTER(AI),
                                           POINTER(AItmp)));
      } else {
        PROPAGATE(NOALIAS(POINTER(AItmp), POINTER(AI), SRC),
                  BOUNDS(TyPosition::make(SRC, *AI, instrIndex[AI], ""),
                         TyPosition::make(SRC, *useInst, useIndex, "")));

        INFRULE(TyPosition::make(SRC, *AI, instrIndex[AI], ""),
                ConsDiffblockNoalias::make(VAL(AItmp, Physical),
                                           VAL(AI, Physical),
                                           POINTER(AItmp),
                                           POINTER(AI)));
      }
    }
    propagateStoreNoalias(AI, AI->getParent(), useInst, useIndex, true, succs, regs);
  });
}

void generateHintForMem2RegPropagateStore(llvm::BasicBlock* Pred,
                                          llvm::StoreInst *SI,
                                          llvm::Instruction *next,
                                          int nextIndex) {
  std::cout<<"PropStore begin"<<std::endl;
  ValidationUnit::GetInstance()->intrude([&Pred, &SI, &next, &nextIndex](
      Dictionary &data, CoreHint &hints) {
    auto &instrIndex = *(data.get<ArgForMem2Reg>()->instrIndex);
    auto &termIndex = *(data.get<ArgForMem2Reg>()->termIndex);
    auto &storeItem = *(data.get<ArgForMem2Reg>()->storeItem);
    auto &mem2regCmd = *(data.get<ArgForMem2Reg>()->mem2regCmd);
    std::string Rstore = getVariable(*(SI->getOperand(1)));
    std::string bname = getBasicBlockIndex(SI->getParent());
    std::string predName = getBasicBlockIndex(Pred);
    std::string keySI = bname + "-" + std::to_string(instrIndex[SI]) + "-" + Rstore;

    std::cout<<"*****SI:"<<Rstore+", "+bname+"("+std::to_string(instrIndex[SI])+"), "+predName<<" | "<< getBasicBlockIndex(next->getParent())+": "+std::to_string(instrIndex[next])+", "+std::to_string(nextIndex)<<std::endl;

    // propagate instruction
    if (llvm::isa<llvm::PHINode>(next)) {
      PROPAGATE(LESSDEF(INSN(std::shared_ptr<TyInstruction>(
                          new ConsLoadInst(TyLoadInst::makeAlignOne(SI)))),
                        VAR(Rstore, Ghost), SRC),
                BOUNDS(TyPosition::make(SRC, *SI, instrIndex[SI], ""),
                       TyPosition::make_end_of_block(TGT, *Pred, termIndex[predName])));

      std::shared_ptr<TyPropagateLessdef> lessdef = TyPropagateLessdef::make
                                                  (VAR(Rstore, Ghost),
                                                   TyExpr::make(*(SI->getOperand(0)),
                                                                Physical),
                                                   TGT);

      if (SI->getOperand(0)->getName()!="")
        mem2regCmd[getVariable(*(SI->getOperand(0)))].lessdef.push_back(lessdef);

      PROPAGATE(std::shared_ptr<TyPropagateObject>(new ConsLessdef(lessdef)),
                BOUNDS(TyPosition::make(SRC, *SI, instrIndex[SI], ""),
                       TyPosition::make_end_of_block(TGT, *Pred, termIndex[predName])));
    } else {
      PROPAGATE(LESSDEF(INSN(std::shared_ptr<TyInstruction>(
                          new ConsLoadInst(TyLoadInst::makeAlignOne(SI)))),
                        VAR(Rstore, Ghost), SRC),
                BOUNDS(TyPosition::make(SRC, *SI, instrIndex[SI], ""),
                       TyPosition::make(SRC, *next, nextIndex, "")));

      std::shared_ptr<TyPropagateLessdef> lessdef = TyPropagateLessdef::make
                                                      (VAR(Rstore, Ghost),
                                                       TyExpr::make(*(SI->getOperand(0)),
                                                                    Physical),
                                                       TGT);

      if (SI->getOperand(0)->getName()!="")
        mem2regCmd[getVariable(*(SI->getOperand(0)))].lessdef.push_back(lessdef);
    
      if (SI->getOperand(0)->getName()=="") {
        if (llvm::isa<llvm::UndefValue>(SI->getOperand(0)))
          std::cout<<"prop store oper0 is undef"<<std::endl;
        else
          std::cout<<"prop store oper0 is constant"<<std::endl;
      }

      PROPAGATE(std::shared_ptr<TyPropagateObject>(new ConsLessdef(lessdef)),
                BOUNDS(TyPosition::make(SRC, *SI, instrIndex[SI], ""),
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
        TyPosition::make(SRC, *SI, instrIndex[SI], "");

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
        TyPosition::make(SRC, *SI, instrIndex[SI], "");

      // stored value will be changed in another iteration
      INFRULE(position,//TyPosition::make(SRC, *SI, instrIndex[SI], ""),
              ConsIntroGhost::make(VAR(storeItem[SI].op0, Ghost),
                                   REGISTER(Rstore, Ghost)));

      INFRULE(position,//TyPosition::make(SRC, *SI, instrIndex[SI], ""),
              ConsTransitivity::make(INSN(std::shared_ptr<TyInstruction>(
                                       new ConsLoadInst(TyLoadInst::makeAlignOne(SI)))),
                                     VAR(storeItem[SI].op0, Physical),
                                     VAR(storeItem[SI].op0, Ghost)));

      INFRULE(position,//TyPosition::make(SRC, *SI, instrIndex[SI], ""),
              ConsTransitivity::make(INSN(std::shared_ptr<TyInstruction>(
                                       new ConsLoadInst(TyLoadInst::makeAlignOne(SI)))),
                                     VAR(storeItem[SI].op0, Ghost),
                                     VAR(Rstore, Ghost)));

      std::shared_ptr<TyTransitivityTgt> transTgt(new TyTransitivityTgt(
                                                    VAR(Rstore, Ghost),
                                                    VAR(storeItem[SI].op0, Ghost),
                                                    TyExpr::make(*(SI->getOperand(0)), Physical)));

      INFRULE(position,//TyPosition::make(SRC, *SI, instrIndex[SI], ""),
              std::shared_ptr<TyInfrule>(new ConsTransitivityTgt(transTgt)));

      if (SI->getOperand(0)->getName()!="")
        mem2regCmd[getVariable(*(SI->getOperand(0)))].transTgt.push_back(transTgt);
       
    }
  });
  std::cout<<"PropStore end"<<std::endl;
}

llvm::PHINode* properPHI(llvm::BasicBlock *BB, std::string Target,
                         llvm::StoreInst *SI, bool isInit,
                         bool checkSI,
                         std::vector<llvm::BasicBlock*> preds) {
  llvm::BasicBlock *SIB = SI->getParent();

  // return NULL if SIB block is same as BB block 
  if (BB == SIB)
    return NULL;

  std::cout<<"properPHI start"<<std::endl;
  // if isInit is true, check current BB
  if (isInit) {
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

      if (checkSI) {
        llvm::BasicBlock::iterator INST = BB->begin();

        while (INST != BB->end()) {
          if (llvm::StoreInst* SItmp = llvm::dyn_cast<llvm::StoreInst>(INST)) {
            if (Target == getVariable(*SItmp->getOperand(1)))
              return NULL;
            INST++;
          } else {
            INST++;
            continue; 
          }
        }
      }
    }
  }

  for (auto BI = pred_begin(BB), BE = pred_end(BB); BI != BE; BI++) {
    llvm::BasicBlock* BBtmp = *BI;

    // skip if SIB block is not reachable to BBtmp block
    if (!isPotentiallyReachable(SIB, BBtmp))
      continue;

    std::cout<<"properPHI: "+getBasicBlockIndex(BBtmp)+"<-"+getBasicBlockIndex(BB)+", "+Target+"("+getBasicBlockIndex(SIB)+")"<<std::endl;
    if (std::find(preds.begin(), preds.end(), BBtmp) != preds.end())
      break;
    preds.push_back(BBtmp);

    std::cout<<"properPHI': "+getBasicBlockIndex(BBtmp)+"<-"+getBasicBlockIndex(BB)+", "+Target+"("+getBasicBlockIndex(SIB)+")"<<std::endl;
    if (llvm::PHINode *PHI = llvm::dyn_cast<llvm::PHINode>(BBtmp->begin())) {
      llvm::BasicBlock::iterator INST = BBtmp->begin();
      llvm::BasicBlock::iterator PNI = BBtmp->begin();

      while (INST != BBtmp->end()) {
        if (llvm::StoreInst* SItmp = llvm::dyn_cast<llvm::StoreInst>(INST)) {
          if (Target == getVariable(*SItmp->getOperand(1)))
            return NULL;
          INST++;
        } else {
          INST++;
          continue; 
        }
      }

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

    llvm::PHINode* ret = properPHI(BBtmp, Target, SI, false, checkSI, preds);
    if (ret != NULL)
      return ret;
  }

  return NULL;
}

void generateHintForMem2RegPropagateLoad(llvm::Instruction *I,
                                         llvm::LoadInst *LI,
                                         llvm::Instruction *use, int useIndex) {
  std::cout<<"PropLoad begin"<<std::endl;
  ValidationUnit::GetInstance()->intrude([&I, &LI, &use, &useIndex](
      Dictionary &data, CoreHint &hints) {
    auto &instrIndex = *(data.get<ArgForMem2Reg>()->instrIndex);
    auto &mem2regCmd = *(data.get<ArgForMem2Reg>()->mem2regCmd);
    std::string Rload = getVariable(*LI);

    if (llvm::StoreInst *SI = llvm::dyn_cast<llvm::StoreInst>(I)) {
      //if (llvm::isa<llvm::ConstantPointerNull>(SI->getOperand(0)))
      //  return;

      std::string Rstore = getVariable(*(SI->getOperand(1)));

      INFRULE(TyPosition::make(SRC, *LI, instrIndex[LI], ""),
              ConsIntroGhost::make(VAR(Rstore, Ghost), REGISTER(Rload, Ghost)));

      INFRULE(TyPosition::make(SRC, *LI, instrIndex[LI], ""),
              ConsTransitivity::make(VAR(Rload, Physical),
                                     INSN(std::shared_ptr<TyInstruction>(
                                       new ConsLoadInst(TyLoadInst::makeAlignOne(SI)))),
                                     VAR(Rstore, Ghost)));

      INFRULE(TyPosition::make(SRC, *LI, instrIndex[LI], ""),
              ConsTransitivity::make(VAR(Rload, Physical), VAR(Rstore, Ghost),
                                     VAR(Rload, Ghost)));

      PROPAGATE(LESSDEF(VAR(Rload, Physical), VAR(Rload, Ghost), SRC),
                BOUNDS(TyPosition::make(SRC, *LI, instrIndex[LI], ""),
                       TyPosition::make(SRC, *use, useIndex, "")));

      std::vector<llvm::BasicBlock*> preds;
      llvm::PHINode* PHI = properPHI(LI->getParent(), Rstore, SI, true, false, preds);
      //if (llvm::PHINode* PHI =
      //      properPHI(LI->getParent(), Rstore, SI, true, preds)) {
      if (PHI != NULL) {
        std::cout<<"asfdasdf"<<std::endl;
        std::shared_ptr<TyPropagateLessdef> lessdef =
          TyPropagateLessdef::make
            (VAR(Rload, Ghost),
             VAR(Rload, Physical),
             TGT);

        mem2regCmd[Rload].lessdef.push_back(lessdef);

        PROPAGATE(std::shared_ptr<TyPropagateObject>(new ConsLessdef(lessdef)),
                  BOUNDS(TyPosition::make(SRC, *LI, instrIndex[LI], ""),
                         TyPosition::make(SRC, *use, useIndex, "")));

        std::shared_ptr<TyTransitivityTgt> transTgt(new TyTransitivityTgt(
                                                      VAR(Rload, Ghost),
                                                      VAR(Rstore, Ghost),
                                                      VAR(Rload, Physical)));

        INFRULE(TyPosition::make(SRC, *LI, instrIndex[LI], ""),
                std::shared_ptr<TyInfrule>(new ConsTransitivityTgt(transTgt)));

        mem2regCmd[Rload].transTgt.push_back(transTgt);
      } else {
        std::shared_ptr<TyPropagateLessdef> lessdef =
          TyPropagateLessdef::make
            (VAR(Rload, Ghost),
             TyExpr::make(*(SI->getOperand(0)), Physical),
             TGT);

        PROPAGATE(std::shared_ptr<TyPropagateObject>(new ConsLessdef(lessdef)),
                  BOUNDS(TyPosition::make(SRC, *LI, instrIndex[LI], ""),
                         TyPosition::make(SRC, *use, useIndex, "")));

        std::shared_ptr<TyTransitivityTgt> transTgt(new TyTransitivityTgt(
                                                      VAR(Rload, Ghost),
                                                      VAR(Rstore, Ghost),
                                                      TyExpr::make(*(SI->getOperand(0)), Physical)));

        INFRULE(TyPosition::make(SRC, *LI, instrIndex[LI], ""),
                std::shared_ptr<TyInfrule>(new ConsTransitivityTgt(transTgt)));

        //if (!llvm::isa<llvm::ConstantInt>(SI->getOperand(0))) {
        if (SI->getOperand(0)->getName()!="") {
          mem2regCmd[getVariable(*(SI->getOperand(0)))].lessdef.push_back(lessdef);
          mem2regCmd[getVariable(*(SI->getOperand(0)))].transTgt.push_back(transTgt);
        }
      }
    }
  });
    std::cout<<"PropLoad end"<<std::endl;
}

void generateHintForMem2RegReplaceHint(llvm::Value *ReplVal,
                                       llvm::Instruction *ReplInst) {
  std::cout<<"ReplaceHint begin"<<std::endl;
  ValidationUnit::GetInstance()->intrude
    ([&ReplVal, &ReplInst]
      (Dictionary &data, CoreHint &hints) {
    auto &mem2regCmd = *(data.get<ArgForMem2Reg>()->mem2regCmd);
    std::string ReplName = "";

    if (llvm::isa<llvm::AllocaInst>(ReplInst) ||
        llvm::isa<llvm::LoadInst>(ReplInst) ||
        llvm::isa<llvm::PHINode>(ReplInst))
      ReplName = getVariable(*ReplInst);
    
    if (ReplName == "")
      return;

    if (mem2regCmd.find(ReplName) == mem2regCmd.end())
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

    std::shared_ptr<TyExpr> keyExpr = ConsVar::make(ReplName, Physical);

    std::vector<std::pair<std::shared_ptr<TyPosition>,
                         std::shared_ptr<TyTransitivity>>> &vec =
      mem2regCmd[ReplName].transSrc;

    std::vector<std::pair<std::shared_ptr<TyPosition>,
                          std::shared_ptr<TyTransitivity>>> tmp;

    for (size_t i = 0; i < vec.size(); i++) {
      if(data.get<ArgForMem2Reg>()->equalsIfConsVar(vec[i].second->getExpr2(), 
                                                    keyExpr)) {
        if (ConsInsn *cv1 = dynamic_cast<ConsInsn *>(vec[i].second->getExpr1().get())) {
        if (ConsVar *cv2 = dynamic_cast<ConsVar *>(vec[i].second->getExpr2().get())) {
        if (ConsVar *cv3 = dynamic_cast<ConsVar *>(vec[i].second->getExpr3().get())) {
          std::shared_ptr<TyTransitivity> transitivity
            (new TyTransitivity(std::shared_ptr<TyExpr>(new ConsInsn(cv1->getTyInsn())),
                                std::shared_ptr<TyExpr>(new ConsVar(cv2->getTyReg())),
                                std::shared_ptr<TyExpr>(new ConsVar(cv3->getTyReg()))));

          tmp.push_back(std::make_pair(vec[i].first, transitivity));

          std::shared_ptr<TyTransitivityTgt> transitivitytgt
            (new TyTransitivityTgt(std::shared_ptr<TyExpr>(new ConsVar(cv3->getTyReg())),
                                   VAR(ReplName, Ghost),
                                   TyExpr::make(*ReplVal, Physical)));

          INFRULE(vec[i].first,
                  std::shared_ptr<TyInfrule>(new ConsTransitivityTgt(transitivitytgt)));
          
          std::cout<<"where1"<<std::endl;
          data.get<ArgForMem2Reg>()->replaceCmdRhs("Transitivity_e3", ReplName,
                                                   vec[i].second->getExpr3());
        }}}
      }
    }

        std::cout<<"where3"<<std::endl;
    data.get<ArgForMem2Reg>()->replaceCmdRhs("Transitivity_e1", ReplName,
                                    VAR(ReplName, Physical));

        std::cout<<"where4"<<std::endl;
    data.get<ArgForMem2Reg>()->replaceCmdRhs("Transitivity_e2", ReplName,
                                    VAR(ReplName, Ghost));

    for (size_t i = 0; i < tmp.size(); i++) {
      INFRULE(tmp[i].first,
              std::shared_ptr<TyInfrule>(new ConsTransitivity(tmp[i].second)));

      mem2regCmd[ReplName].transSrc.push_back(tmp[i]);
    }
  });
  std::cout<<"ReplaceHint end"<<std::endl;
}

bool isPred(llvm::BasicBlock *Succ, llvm::BasicBlock *Target,
            std::vector<llvm::BasicBlock*> preds) {
    std::cout<<"In isPred begin"<<std::endl;
  if (Succ == Target)
    return true;

  // return false if Target block is not reachable
  // to Succ block
  if (!isPotentiallyReachable(Target, Succ))
    return false;

  preds.push_back(Succ);
    std::cout<<"In isPred beforeloop"<<std::endl;
  for (auto BI = pred_begin(Succ), BE = pred_end(Succ); BI != BE; BI++) {
    llvm::BasicBlock *nextPred = *BI;

    if (std::find(preds.begin(), preds.end(), nextPred) != preds.end())
      break;
    preds.push_back(nextPred);

    std::cout<<"In isPred iter: "+getBasicBlockIndex(nextPred)<<std::endl;
    if (isPred(nextPred, Target, preds))
      return true;
  }
    std::cout<<"In isPred endloop"<<std::endl;

  return false;
}

bool otherPHI(llvm::BasicBlock *BB, llvm::BasicBlock *Src,
              std::string Target,
              std::vector<llvm::BasicBlock*> preds) {
  if (BB == NULL || BB == Src)
    return false;

  // return false if Target block is not reachable
  // to Succ block
  if (!isPotentiallyReachable(Src, BB))
    return false;

  for (auto BI = pred_begin(BB), BE = pred_end(BB); BI != BE; BI++) {
    llvm::BasicBlock* BBtmp = *BI;

    if (std::find(preds.begin(), preds.end(), BBtmp) != preds.end())
      break;
    preds.push_back(BBtmp);

    if (llvm::PHINode *PHI = llvm::dyn_cast<llvm::PHINode>(BBtmp->begin())) {
      llvm::BasicBlock::iterator PNI = BBtmp->begin();

      while (PHI) {
        std::string Rphi = getVariable(*PHI);

        // fragile: this condition relies on llvm naming convention of PHI
        if (Target == Rphi.substr(0, Rphi.rfind("."))) {
          return true;
        }

        PNI++;
        PHI = llvm::dyn_cast<llvm::PHINode>(PNI);
      }
    }

    if (otherPHI(BBtmp, Src, Target, preds))
      return true;
  }

  return false;
}

void generateHintForMem2RegPHI(llvm::BasicBlock *BB, llvm::BasicBlock *Pred,
                               llvm::AllocaInst *AI, llvm::StoreInst *SI,
                               llvm::BasicBlock::iterator II,
                               llvm::DenseMap<llvm::PHINode *, unsigned> PAM,
                               llvm::DenseMap<llvm::AllocaInst *, unsigned> AL,
                               std::vector<llvm::BasicBlock*> succs,
                               bool isSameBB) {
  ValidationUnit::GetInstance()->intrude([&BB, &Pred, &AI, &SI, &II, &PAM, &AL,
                                          &succs, &isSameBB](Dictionary &data,
                                                     CoreHint &hints) {
    llvm::StoreInst *SItmp = SI;
    llvm::AllocaInst *AItmp = AI;

    // prepare variables
    auto &instrIndex = *(data.get<ArgForMem2Reg>()->instrIndex);
    auto &termIndex = *(data.get<ArgForMem2Reg>()->termIndex);
    auto &storeItem = *(data.get<ArgForMem2Reg>()->storeItem);
    auto &transTgt = *(data.get<ArgForMem2Reg>()->transTgt);
    auto &mem2regCmd = *(data.get<ArgForMem2Reg>()->mem2regCmd);
    std::string bname = getBasicBlockIndex(SItmp->getParent());
    std::string Ralloca = getVariable(*AItmp);
    std::string Rstore = getVariable(*SItmp->getOperand(1));
    std::string keySI = bname + "-" + std::to_string(instrIndex[SItmp]) + "-" + Rstore;
    llvm::BasicBlock *BBtmp = BB;
    llvm::BasicBlock *Predtmp = Pred;

    std::cout<<"Mem2RegPHI begin: "+bname+", "+keySI<<std::endl;
    llvm::succ_iterator BI = succ_begin(BB), BE = succ_end(BB);
    llvm::BasicBlock::iterator IItmp = II;
    do {
      bool newStore = false;
      std::cout<<"Mem2RegPHI checkstart: "+llvmberry::getBasicBlockIndex(BB)+"->"+llvmberry::getBasicBlockIndex(BBtmp)<<std::endl;
      if (!isSameBB) {
        Predtmp = BB;
        BBtmp = *BI++;
        IItmp = BBtmp->begin();

        bname = getBasicBlockIndex(SItmp->getParent());
        Rstore = getVariable(*SItmp->getOperand(1));
        keySI = bname + "-" + std::to_string(instrIndex[SItmp]) + "-" + Rstore;

        // do not check same block again
        if (std::find(succs.begin(), succs.end(), BBtmp) != succs.end())
          continue;
        succs.push_back(BBtmp);

        std::cout<<"Mem2RegPHI checkend: "+getBasicBlockIndex(Predtmp)+"->"+getBasicBlockIndex(BBtmp)<<std::endl;
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
            if (Predtmp != PHI->getParent() && PAM.count(PHI) &&
                // fragile: this condition relies on llvm naming convention of PHI
                Rstore == Rphi.substr(0, Rphi.rfind("."))) {
              std::vector<llvm::BasicBlock*> preds;

              // check if there is other PHI between
              // SI block and current PHI block.
              // compiler can choose either 2-1 or 2-2 according to this
              llvm::PHINode* PHItmp = properPHI(Predtmp, Rstore, SItmp, true, true, preds);

              // <Condition 2-1>
              // if there is no other PHI between SI and current PHI,
              // we can propagate store to current PHI
              if (PHItmp == NULL /*||
                  (PHItmp != NULL && (BBtmp == SItmp->getParent()))*/) {
                generateHintForMem2RegPropagateStore(Predtmp, SItmp, PHI, termIndex[bname]);
              } else {
              // <Condition 2-2>
              // if we find another PHI(PHItmp) after SI,
              // propagate PHItmp to current PHI
                std::string Rphitmp = getVariable(*PHItmp);

                PROPAGATE(
                    LESSDEF(INSN(std::shared_ptr<TyInstruction>(
                              new ConsLoadInst(TyLoadInst::makeAlignOne(SItmp)))),
                            VAR(Rstore, Ghost), SRC),
                    BOUNDS(TyPosition::make_start_of_block(
                               TGT, getBasicBlockIndex(PHItmp->getParent())),
                           TyPosition::make_end_of_block(TGT, *Predtmp)));

                PROPAGATE(
                    LESSDEF(VAR(Rstore, Ghost), VAR(Rphitmp, Physical), TGT),
                    BOUNDS(TyPosition::make_start_of_block(
                               TGT, getBasicBlockIndex(PHItmp->getParent())),
                           TyPosition::make_end_of_block(TGT, *Predtmp)));

                std::shared_ptr<TyTransitivityTgt> transitivitytgt
                  (new TyTransitivityTgt(VAR(Rstore, Ghost),
                                         VAR(Rphitmp, Previous),
                                         VAR(Rphi, Physical)));

                INFRULE(TyPosition::make(TGT, PHI->getParent()->getName(),
                                         Predtmp->getName()),
                        std::shared_ptr<TyInfrule>(new ConsTransitivityTgt(transitivitytgt)));
              }

              // this step apply different infrules according to stored value
              // 1. stored value is constant
              // 2. stored value exists in both src and tgt
              if (storeItem[SItmp].expr != NULL &&
                  (storeItem[SItmp].op0 == "%" ||
                   SItmp->getOperand(0)->getName() != "" ||
                   data.get<ArgForMem2Reg>()->equalsIfConsVar(storeItem[SItmp].expr, 
                                                              TyExpr::make(*(SItmp->getOperand(0)),
                                                                           Physical)))) {
                //check transtgt
                std::shared_ptr<TyTransitivityTgt> transitivitytgt
                  (new TyTransitivityTgt(VAR(Rstore, Ghost),
                                         //storeItem[SItmp].expr,
                                         EXPR(SItmp->getOperand(0), Physical),
                                         VAR(Rphi, Physical)));

                INFRULE(TyPosition::make(TGT, PHI->getParent()->getName(),
                                         Predtmp->getName()),
                        std::shared_ptr<TyInfrule>(new ConsTransitivityTgt(transitivitytgt)));

                if (storeItem[SItmp].op0 != "%") {
                  std::cout<<"transtgt check key: "+storeItem[SItmp].op0+", "+Rstore+", "+Rphi<<std::endl;
                  //mem2regCmd[storeItem[SItmp].op0].transTgt.push_back(transitivitytgt);
                  transTgt.push_back(transitivitytgt);
                }
                //check end
              } else if (storeItem[SItmp].op0 != "%") {
              // 3. stored value is register and it will be changed in other loop
                std::shared_ptr<TyTransitivityTgt> transitivitytgt
                  (new TyTransitivityTgt(VAR(Rstore, Ghost),
                                         VAR(storeItem[SItmp].op0, Ghost),
                                         VAR(Rphi, Physical)));
                mem2regCmd[Rphi].transTgt.push_back(transitivitytgt);

                INFRULE(TyPosition::make(TGT, PHI->getParent()->getName(),
                                         Predtmp->getName()),
                        std::shared_ptr<TyInfrule>(new ConsTransitivityTgt(transitivitytgt)));
              } else {
              // 4. stored value is null
                std::cout<<"check transTgt null"<<std::endl;
                std::shared_ptr<TyTransitivityTgt> transitivitytgt
                  (new TyTransitivityTgt(VAR(Rstore, Ghost),
                                         TyExpr::make(storeItem[SItmp].value),
                                         VAR(Rphi, Physical)));
                mem2regCmd[Rphi].transTgt.push_back(transitivitytgt);

                INFRULE(TyPosition::make(TGT, PHI->getParent()->getName(),
                                         Predtmp->getName()),
                        std::shared_ptr<TyInfrule>(new ConsTransitivityTgt(transitivitytgt)));
              }

              // add hints per every use of PHI
              for (auto UI = PHI->use_begin(), UE = PHI->use_end(); UI != UE;) {
                llvm::Use &U = *(UI++);
                llvm::Instruction *use =
                    llvm::dyn_cast<llvm::Instruction>(U.getUser());

                int useIndex = getIndexofMem2Reg(
                    use, instrIndex[use],
                    termIndex[getBasicBlockIndex(use->getParent())]);

                std::cout<<"use of PHI("+std::string(PHI->getParent()->getParent()->getName())+"): "+getBasicBlockIndex(use->getParent())+"("+std::to_string(instrIndex[use])+", "+std::to_string(useIndex)+")"<<std::endl;

                // if use is other PHI
                if (llvm::isa<llvm::PHINode>(use)) {
                  llvm::BasicBlock* source = PHI->getParent();
                  llvm::BasicBlock* target = use->getParent();
                  
                  for (auto UI2 = pred_begin(target), UE2 = pred_end(target); UI2 != UE2; UI2++) {
                    std::cout<<"isPred before("+std::string(target->getParent()->getName())+"): "+getBasicBlockIndex(target)+", "+getBasicBlockIndex(source)<<std::endl;
                    llvm::BasicBlock* usePred = *UI2;
                    std::vector<llvm::BasicBlock*> preds;
                    llvm::PHINode* PHItmp = properPHI(usePred, Rstore, SItmp, true, true, preds);

                    // now working: properPHI
                    if (PHItmp == PHI &&
                        isPotentiallyReachable(source, usePred)) {
                      PROPAGATE(
                          LESSDEF(INSN(std::shared_ptr<TyInstruction>(
                                    new ConsLoadInst(TyLoadInst::makeAlignOne(SItmp)))),
                                  VAR(Rstore, Ghost), SRC),
                          BOUNDS(TyPosition::make_start_of_block(
                                     TGT, getBasicBlockIndex(PHI->getParent())),
                                 TyPosition::make_end_of_block(TGT, *usePred)));

                      std::shared_ptr<TyPropagateLessdef> lessdef = TyPropagateLessdef::make
                                                                    (VAR(Rstore,Ghost),
                                                                     VAR(Rphi, Physical),
                                                                     TGT);
                      mem2regCmd[Rphi].lessdef.push_back(lessdef);

                      PROPAGATE(
                          std::shared_ptr<TyPropagateObject>(new ConsLessdef(lessdef)),
                          BOUNDS(TyPosition::make_start_of_block(
                                     TGT, getBasicBlockIndex(PHI->getParent())),
                                 TyPosition::make_end_of_block(TGT, *usePred)));

                      std::shared_ptr<TyTransitivityTgt> transitivitytgt
                        (new TyTransitivityTgt(VAR(Rstore, Ghost),
                                               VAR(Rphi, Previous),
                                               VAR(getVariable(*use), Physical)));
                     
                      INFRULE(TyPosition::make(TGT, target->getName(),
                                               usePred->getName()),
                              std::shared_ptr<TyInfrule>(new ConsTransitivityTgt(transitivitytgt)));
                     
                      mem2regCmd[Rphi].transTgt.push_back(transitivitytgt);
                    }
                    std::cout<<"isPred end"<<std::endl;
                  }
                } else if (llvm::isa<llvm::LoadInst>(use) &&
                           (use->getOperand(0) == SItmp->getOperand(1))) {
                  PROPAGATE(
                      LESSDEF(INSN(std::shared_ptr<TyInstruction>(
                                new ConsLoadInst(TyLoadInst::makeAlignOne(SItmp)))),
                              VAR(Rstore, Ghost), SRC),
                      BOUNDS(TyPosition::make_start_of_block(
                                 TGT, getBasicBlockIndex(PHI->getParent())),
                             TyPosition::make(SRC, *use, useIndex, "")));

                  std::shared_ptr<TyPropagateLessdef> lessdef = TyPropagateLessdef::make
                                                                (VAR(Rstore, Ghost),
                                                                 VAR(Rphi, Physical),
                                                                 TGT);              
                  mem2regCmd[Rphi].lessdef.push_back(lessdef);
          
                  PROPAGATE(std::shared_ptr<TyPropagateObject>(new ConsLessdef(lessdef)),
                      BOUNDS(TyPosition::make_start_of_block(
                                 TGT, getBasicBlockIndex(PHI->getParent())),
                             TyPosition::make(SRC, *use, useIndex, "")));
                } else {
                  std::cout<<"use is not load or phi("+std::string(use->getOpcodeName())+")"<<std::endl;
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
        std::cout << "Mem2RegPHI instruction iter begin: "+getBasicBlockIndex(I->getParent())+"("+std::to_string(instrIndex[I])+")" << std::endl;
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
          std::cout << "Mem2RegPHI load start:"+Ralloca+", "+Rload+", "+Rstore<< std::endl;

          std::vector<llvm::BasicBlock*> preds;
          llvm::PHINode* PHI = properPHI(LI->getParent(), Rstore, SItmp, true, false, preds);
          //if (llvm::PHINode* PHI =
          //      properPHI(LI->getParent(), Rstore, SItmp, true, preds)) {
          if (PHI != NULL) {
            std::cout << "Mem2RegPHI load phi: "+getVariable(*PHI)<< std::endl;
            std::string Rphi = getVariable(*PHI);

            std::vector<llvm::BasicBlock*> predsbefore;
            predsbefore.push_back(PHI->getParent());
            llvm::PHINode* PHIbefore = properPHI(LI->getParent(), Rstore, SItmp, false, false, predsbefore);
            //if (llvm::PHINode* PHIbefore =
            //      properPHI(LI->getParent(), Rstore, SItmp, false, predsbefore)) {
            if (PHIbefore != NULL) {
              llvm::BasicBlock* source = PHIbefore->getParent();
              llvm::BasicBlock* target = PHI->getParent();
            
              // now working2: properPHI
              if (source != target) {
                for (auto UI2 = pred_begin(target), UE2 = pred_end(target); UI2 != UE2; UI2++) {
                  llvm::BasicBlock* usePred = *UI2;
                  //std::vector<llvm::BasicBlock*> predscheck;
                  //llvm::PHINode* PHIcheck = properPHI(usePred, Rstore, SItmp, true, true, predscheck);
                  std::cout<<"Block iter isPred before: "+getBasicBlockIndex(source)+", "+getBasicBlockIndex(target)+", "+getBasicBlockIndex(usePred)<<std::endl;

                  if (isPotentiallyReachable(source, usePred)) {
                    std::vector<llvm::BasicBlock*> predscheck;

                    llvm::PHINode* PHItmp = properPHI(usePred, Rstore, SItmp, true, true, predscheck);
                    if (PHItmp != NULL) {
                      if (PHIbefore == PHItmp) {
                        std::cout<<"Block iter propagate"<<std::endl;
                        PROPAGATE(
                            LESSDEF(INSN(std::shared_ptr<TyInstruction>(
                                      new ConsLoadInst(TyLoadInst::makeAlignOne(SItmp)))),
                                    VAR(Rstore, Ghost), SRC),
                            BOUNDS(TyPosition::make_start_of_block(
                                       TGT, getBasicBlockIndex(source)),
                                   TyPosition::make_end_of_block(TGT, *usePred)));

                        std::shared_ptr<TyPropagateLessdef> lessdef = TyPropagateLessdef::make
                                                                      (VAR(Rstore,Ghost),
                                                                       VAR(getVariable(*PHIbefore), Physical),
                                                                       TGT);
                        //mem2regCmd[Rphi].lessdef.push_back(lessdef);

                        PROPAGATE(
                            std::shared_ptr<TyPropagateObject>(new ConsLessdef(lessdef)),
                            BOUNDS(TyPosition::make_start_of_block(
                                       TGT, getBasicBlockIndex(source)),
                                   TyPosition::make_end_of_block(TGT, *usePred)));

                        
                        std::shared_ptr<TyTransitivityTgt> transitivitytgt
                          (new TyTransitivityTgt(VAR(Rstore, Ghost),
                                                 VAR(getVariable(*PHIbefore), Previous),
                                                 VAR(Rphi, Physical)));
                       
                        INFRULE(TyPosition::make(TGT, target->getName(),
                                                 usePred->getName()),
                                std::shared_ptr<TyInfrule>(new ConsTransitivityTgt(transitivitytgt)));
                       
                        //mem2regCmd[Rphi].transTgt.push_back(transitivitytgt);
                      }
                    }
                  }
                  std::cout<<"Block iter isPred end"<<std::endl;
                }
              }
            }

            //if (!isSameBB && PAM.count(PHI) &&
            //    (Rstore == Rphi.substr(0, Rphi.rfind(".")))) {
            PROPAGATE(
                LESSDEF(INSN(std::shared_ptr<TyInstruction>(
                          new ConsLoadInst(TyLoadInst::makeAlignOne(SItmp)))),
                        VAR(Rstore, Ghost), SRC),
                BOUNDS(TyPosition::make_start_of_block(
                           SRC, getBasicBlockIndex(PHI->getParent())),
                       TyPosition::make(SRC, *LI, instrIndex[LI], "")));

            std::shared_ptr<TyPropagateLessdef> lessdef = TyPropagateLessdef::make
                  (VAR(Rstore,Ghost),
                   VAR(Rphi, Physical), TGT);

            PROPAGATE(std::shared_ptr<TyPropagateObject>(new ConsLessdef(lessdef)),
                BOUNDS(TyPosition::make_start_of_block(
                           SRC, getBasicBlockIndex(PHI->getParent())),
                       TyPosition::make(SRC, *LI, instrIndex[LI], "")));

            mem2regCmd[Rphi].lessdef.push_back(lessdef);

            if (storeItem[SItmp].expr != NULL &&
                (storeItem[SItmp].op0 == "%" ||
                 std::string(SItmp->getOperand(0)->getName()) != "" ||
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
            } else if (storeItem[SItmp].op0 != "%") {
              std::shared_ptr<TyTransitivityTgt> transitivitytgt
                (new TyTransitivityTgt(VAR(Rstore, Ghost),
                                       VAR(storeItem[SItmp].op0, Ghost),
                                       VAR(getVariable(*PHI), Physical)));
              mem2regCmd[getVariable(*PHI)].transTgt.push_back(transitivitytgt);

              INFRULE(TyPosition::make(TGT, LI->getParent()->getName(),
                                       Predtmp->getName()),
                      std::shared_ptr<TyInfrule>(new ConsTransitivityTgt(transitivitytgt)));
            } else {
              std::cout<<"transTgt null"<<std::endl;
              std::shared_ptr<TyTransitivityTgt> transitivitytgt
                (new TyTransitivityTgt(VAR(Rstore, Ghost),
                                       TyExpr::make(storeItem[SItmp].value),
                                       VAR(getVariable(*PHI), Physical)));
              mem2regCmd[getVariable(*PHI)].transTgt.push_back(transitivitytgt);

              INFRULE(TyPosition::make(TGT, LI->getParent()->getName(),
                                       Predtmp->getName()),
                      std::shared_ptr<TyInfrule>(new ConsTransitivityTgt(transitivitytgt)));
            }
                //}
            //    PNI++;
            //    PHI = llvm::dyn_cast<llvm::PHINode>(PNI);
            //  }
            //}
          }

          PROPAGATE(ALLOCA(REGISTER(Ralloca, Physical), SRC),
                    BOUNDS(TyPosition::make(SRC, *AItmp, instrIndex[AItmp], ""),
                           TyPosition::make(SRC, *LI, instrIndex[LI], "")));

          PROPAGATE(PRIVATE(REGISTER(Ralloca, Physical), SRC),
                    BOUNDS(TyPosition::make(SRC, *AItmp, instrIndex[AItmp], ""),
                           TyPosition::make(SRC, *LI, instrIndex[LI], "")));

          if ((getVariable(*LI->getOperand(0)) == Rstore) &&
              ((LI->getParent() == SItmp->getParent() &&
                instrIndex[SItmp] < instrIndex[LI]) ||
               (isPotentiallyReachable(SItmp->getParent(), LI->getParent())))) {
        std::cout << "Mem2RegPHI prop store check"<< std::endl;

            std::vector<llvm::BasicBlock*> preds;
            llvm::PHINode* PHI = properPHI(LI->getParent(), Rstore, SItmp, true, false, preds);
            //if (llvm::PHINode* PHI =
            //      properPHI(LI->getParent(), Rstore, SItmp, true, preds)) {
              //bool isSamePHI = true;

              //if (!isSamePHI) {
              //  generateHintForMem2RegPropagateStore(Predtmp, SItmp, LI, instrIndex[LI]);
              //}
            //} else {
            if (PHI == NULL) {
              generateHintForMem2RegPropagateStore(Predtmp, SItmp, LI, instrIndex[LI]);
            }

            // add hints per every use of LI
            for (auto UI = LI->use_begin(), E = LI->use_end(); UI != E;) {
              llvm::Use &U = *(UI++);
              llvm::Instruction *use =
                  llvm::dyn_cast<llvm::Instruction>(U.getUser());

              // set index of use
              int useIndex = getIndexofMem2Reg(
                  use, instrIndex[use],
                  termIndex[getBasicBlockIndex(use->getParent())]);
              std::cout<<"Mem2RegPHI use of load: "+getBasicBlockIndex(use->getParent())+"("+std::to_string(instrIndex[use])+", "+std::to_string(useIndex)+")"<<std::endl;
              if ((LI->getParent() == use->getParent() &&
                   instrIndex[LI] < instrIndex[use]) ||
                  (isPotentiallyReachable(LI->getParent(), use->getParent()))) {
                generateHintForMem2RegPropagateLoad(SItmp, LI, use, useIndex);
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
            //if (isSameBB)
            //  SItmp = SItmp2;
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
          std::cout<<"Mem2RegPHI next call"<<std::endl;
          generateHintForMem2RegPHI(BBtmp, Predtmp, AItmp, SItmp, IItmp,
                                    PAM, AL, succs, true);
        }
        BI++;
      }
      isSameBB = false;
    } while (BI != BE);
  });
  std::cout<<"Mem2RegPHI end"<<std::endl;
}

int getIndexofMem2Reg(llvm::Instruction *instr, int instrIndex, int termIndex) {
  if (llvm::isa<llvm::TerminatorInst>(*instr))
    return termIndex;
  else
    return instrIndex;
}

} // llvmberry
