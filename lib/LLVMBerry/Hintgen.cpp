#include "llvm/LLVMBerry/Hintgen.h"
#include "llvm/LLVMBerry/Infrules.h"
#include "llvm/LLVMBerry/ValidationUnit.h"
#include "llvm/LLVMBerry/Dictionary.h"

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
            ConsOrCommutative::make(REGISTER(regname, Physical),
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
                      REGISTER(llvmberry::getVariable(*Z), Physical),
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

std::pair<std::shared_ptr<TyExpr>, std::shared_ptr<TyExpr>> false_encoding =
    std::make_pair(llvmberry::ConsConst::make(0, 64),
                   llvmberry::ConsConst::make(42, 64));

void generateHintForDCE(llvmberry::CoreHint &hints, llvm::Instruction &I) {
  std::string reg = llvmberry::getVariable(I);

  hints.addCommand(llvmberry::ConsPropagate::make(
      llvmberry::ConsMaydiff::make(reg, llvmberry::Physical),
      llvmberry::ConsGlobal::make()));

  insertTgtNopAtSrcI(hints, &I);
}

void generateHintForTrivialDCE(llvm::Instruction &I) {
  assert(llvmberry::ValidationUnit::Exists());
  llvmberry::ValidationUnit::GetInstance()->intrude([&I](
      llvmberry::ValidationUnit::Dictionary &data, llvmberry::CoreHint &hints) {
    generateHintForDCE(hints, I);
    if (llvm::dyn_cast<llvm::CallInst>(&I)) {
      hints.setDescription("DCE on call "
                           "instruction.\n\"isInstructionTriviallyDead\" "
                           "should give enough power to validate.");
      hints.appendAdmittedToDescription();
    }
  });
}

void generateHintForGVNDCE(llvm::Instruction &I) {
  assert(llvmberry::ValidationUnit::Exists());
  llvmberry::ValidationUnit::GetInstance()->intrude([&I](
      llvmberry::ValidationUnit::Dictionary &data, llvmberry::CoreHint &hints) {
    generateHintForDCE(hints, I);
    if (llvm::dyn_cast<llvm::CallInst>(&I)) {
      hints.setDescription("DCE on call instruction inside GVN.\nIt might be "
                           "introduced from SimplifyInstruction or "
                           "lookup_or_add_call.");
      hints.appendAdmittedToDescription();
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

void generateHintForMem2RegPropagateNoalias(llvm::AllocaInst *AI,
                                            llvm::Instruction *useInst,
                                            int useIndex) {
  ValidationUnit::GetInstance()->intrude([&AI, &useInst, &useIndex](
      Dictionary &data, CoreHint &hints) {
    auto &allocas = *(data.get<ArgForMem2Reg>()->allocas);
    auto &instrIndex = *(data.get<ArgForMem2Reg>()->instrIndex);

    for (auto i = allocas.begin(); i != allocas.end(); ++i) {
      llvm::AllocaInst *AItmp = *i;

      if (AI == AItmp)
        continue;

      if (instrIndex[AI] < instrIndex[AItmp]) {
        PROPAGATE(NOALIAS(POINTER(AI), POINTER(AItmp), SRC),
                  BOUNDS(TyPosition::make(SRC, *AItmp, instrIndex[AItmp], ""),
                         TyPosition::make(SRC, *useInst, useIndex, "")));

        INFRULE(TyPosition::make(SRC, *AItmp, instrIndex[AItmp], ""),
                ConsDiffblockNoalias::make(VAL(AI, Physical),
                                           VAL(AItmp, Physical), POINTER(AI),
                                           POINTER(AItmp)));
      } else {
        PROPAGATE(NOALIAS(POINTER(AItmp), POINTER(AI), SRC),
                  BOUNDS(TyPosition::make(SRC, *AI, instrIndex[AI], ""),
                         TyPosition::make(SRC, *useInst, useIndex, "")));

        INFRULE(TyPosition::make(SRC, *AI, instrIndex[AI], ""),
                ConsDiffblockNoalias::make(VAL(AItmp, Physical),
                                           VAL(AI, Physical), POINTER(AItmp),
                                           POINTER(AI)));
      }
    }
  });
}

void generateHintForMem2RegPropagateStore(llvm::BasicBlock* Pred,
                                          llvm::StoreInst *SI,
                                          llvm::Instruction *next,
                                          int nextIndex) {
  ValidationUnit::GetInstance()->intrude([&Pred, &SI, &next, &nextIndex](
      Dictionary &data, CoreHint &hints) {
    auto &instrIndex = *(data.get<ArgForMem2Reg>()->instrIndex);
    auto &termIndex = *(data.get<ArgForMem2Reg>()->termIndex);
    auto &storeItem = *(data.get<ArgForMem2Reg>()->storeItem);
    auto &values = *(data.get<ArgForMem2Reg>()->values);
    std::string Rstore = getVariable(*(SI->getOperand(1)));
    std::string bname = getBasicBlockIndex(SI->getParent());
    std::string predName = getBasicBlockIndex(Pred);
    std::string keySI = bname + "-" + std::to_string(instrIndex[SI]) + "-" + Rstore;

    std::cout<<"*****SI:"<<Rstore+", "+bname+"("+std::to_string(instrIndex[SI])+")"<<" | "<< getBasicBlockIndex(next->getParent())+": "+std::to_string(instrIndex[next])+", "+std::to_string(nextIndex)<<std::endl;

    if (storeItem[SI].expr == NULL)
      return;

    // propagate instruction
    if (llvm::isa<llvm::PHINode>(next)) {
      PROPAGATE(LESSDEF(INSN(*SI), VAR(Rstore, Ghost), SRC),
                BOUNDS(TyPosition::make(SRC, *SI, instrIndex[SI], ""),
                       //TyPosition::make(SRC, *next, termIndex[predName], "")));
                       TyPosition::make_end_of_block(TGT, *Pred, termIndex[predName])));

      PROPAGATE(LESSDEF(VAR(Rstore, Ghost), values[keySI], TGT),
                BOUNDS(TyPosition::make(SRC, *SI, instrIndex[SI], ""),
                       //TyPosition::make(SRC, *next, termIndex[predName], "")));
                       TyPosition::make_end_of_block(TGT, *Pred, termIndex[predName])));
    } else {
      PROPAGATE(LESSDEF(INSN(*SI), VAR(Rstore, Ghost), SRC),
                BOUNDS(TyPosition::make(SRC, *SI, instrIndex[SI], ""),
                       TyPosition::make(SRC, *next, nextIndex, "")));

      PROPAGATE(LESSDEF(VAR(Rstore, Ghost), values[keySI], TGT),
                BOUNDS(TyPosition::make(SRC, *SI, instrIndex[SI], ""),
                       TyPosition::make(SRC, *next, nextIndex, "")));
    }

    if (storeItem[SI].expr == values[keySI]) {
      // stored value will not be changed in another iteration
      INFRULE(
          TyPosition::make(SRC, *SI, instrIndex[SI], ""),
          ConsIntroGhost::make(storeItem[SI].expr, REGISTER(Rstore, Ghost)));

      INFRULE(TyPosition::make(SRC, *SI, instrIndex[SI], ""),
              ConsTransitivity::make(INSN(*SI), storeItem[SI].expr,
                                     VAR(Rstore, Ghost)));
    } else {
      // stored value will be changed in another iteration
      INFRULE(TyPosition::make(SRC, *SI, instrIndex[SI], ""),
              ConsIntroGhost::make(VAR(storeItem[SI].op0, Ghost),
                                   REGISTER(Rstore, Ghost)));

      INFRULE(TyPosition::make(SRC, *SI, instrIndex[SI], ""),
              ConsTransitivity::make(INSN(*SI),
                                     VAR(storeItem[SI].op0, Physical),
                                     VAR(storeItem[SI].op0, Ghost)));

      INFRULE(TyPosition::make(SRC, *SI, instrIndex[SI], ""),
              ConsTransitivity::make(INSN(*SI), VAR(storeItem[SI].op0, Ghost),
                                     VAR(Rstore, Ghost)));

      INFRULE(TyPosition::make(SRC, *SI, instrIndex[SI], ""),
              ConsTransitivityTgt::make(VAR(Rstore, Ghost),
                                        VAR(storeItem[SI].op0, Ghost),
                                        values[keySI]));
    }
  });
}

void generateHintForMem2RegPropagateLoad(llvm::Instruction *I,
                                         llvm::LoadInst *LI,
                                         llvm::Instruction *use, int useIndex) {
  ValidationUnit::GetInstance()->intrude([&I, &LI, &use, &useIndex](
      Dictionary &data, CoreHint &hints) {
    auto &instrIndex = *(data.get<ArgForMem2Reg>()->instrIndex);
    auto &values = *(data.get<ArgForMem2Reg>()->values);
    std::string Rload = getVariable(*LI);

    if (values[Rload] == NULL)
      return;

    llvm::StoreInst *SI = llvm::dyn_cast<llvm::StoreInst>(I);
    std::string Rstore = getVariable(*(SI->getOperand(1)));

    PROPAGATE(LESSDEF(VAR(Rload, Physical), VAR(Rload, Ghost), SRC),
              BOUNDS(TyPosition::make(SRC, *LI, instrIndex[LI], ""),
                     TyPosition::make(SRC, *use, useIndex, "")));

    PROPAGATE(LESSDEF(VAR(Rload, Ghost), values[Rload], TGT),
              BOUNDS(TyPosition::make(SRC, *LI, instrIndex[LI], ""),
                     TyPosition::make(SRC, *use, useIndex, "")));

    INFRULE(TyPosition::make(SRC, *LI, instrIndex[LI], ""),
            ConsIntroGhost::make(VAR(Rstore, Ghost), REGISTER(Rload, Ghost)));

    INFRULE(TyPosition::make(SRC, *LI, instrIndex[LI], ""),
            ConsTransitivity::make(VAR(Rload, Physical), INSN(*SI),
                                   VAR(Rstore, Ghost)));

    INFRULE(TyPosition::make(SRC, *LI, instrIndex[LI], ""),
            ConsTransitivity::make(VAR(Rload, Physical), VAR(Rstore, Ghost),
                                   VAR(Rload, Ghost)));

    INFRULE(TyPosition::make(SRC, *LI, instrIndex[LI], ""),
            ConsTransitivityTgt::make(VAR(Rload, Ghost), VAR(Rstore, Ghost),
                                      values[Rload]));
  });
}

bool isPred(llvm::BasicBlock *Succ, llvm::BasicBlock *Target) {
  if (Succ == Target)
    return true;

  for (auto BI = pred_begin(Succ), BE = pred_end(Succ); BI != BE; ++BI) {
    if (isPred(*BI, Target))
      return true;
  }

  return false;
}

bool otherPHI(llvm::BasicBlock *BB, std::string Target) {
  for (auto BI = pred_begin(BB), BE = pred_end(BB); BI != BE; ++BI) {
    if (BB == *BI)
      return false;

    if (llvm::PHINode *PHI = llvm::dyn_cast<llvm::PHINode>((*BI)->begin())) {
    llvm::BasicBlock::iterator PNI = (*BI)->begin();

      while (PHI) {
        std::string Rphi = getVariable(*PHI);

        if (Target == Rphi.substr(0, Rphi.rfind("."))) {
          return true;
        }

        PNI++;
        PHI = llvm::dyn_cast<llvm::PHINode>(PNI);
      }
    }

    if (otherPHI(*BI, Target))
      return true;
  }

  return false;
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
    llvm::StoreInst *SItmp = SI;
    llvm::AllocaInst *AItmp =
        llvm::dyn_cast<llvm::AllocaInst>(SItmp->getPointerOperand());
    llvm::Instruction *keyInst =
        llvm::dyn_cast<llvm::Instruction>(SItmp->getOperand(1));

    // prepare variables
    auto &instrIndex = *(data.get<ArgForMem2Reg>()->instrIndex);
    auto &termIndex = *(data.get<ArgForMem2Reg>()->termIndex);
    auto &storeItem = *(data.get<ArgForMem2Reg>()->storeItem);
    auto &values = *(data.get<ArgForMem2Reg>()->values);
    std::string bname = getBasicBlockIndex(SItmp->getParent());
    std::string Ralloca = getVariable(*AItmp);
    std::string Rstore = getVariable(*keyInst);
    std::string keySI = bname + "-" + std::to_string(instrIndex[SItmp]) + "-" + Rstore;
    llvm::BasicBlock *BBtmp = BB;
    llvm::BasicBlock *Predtmp = Pred;
    bool newStore = false;

    llvm::succ_iterator BI = succ_begin(BB), BE = succ_end(BB);
    llvm::BasicBlock::iterator IItmp = II;
    do {
      if (!isSameBB) {
        if (BI != BE && !newStore) {
          generateHintForMem2RegPHI(*BI, Predtmp, AItmp, SItmp, *BI->begin(),
                                    PAM, AL, true);
        }
        Predtmp = BBtmp;
        BBtmp = *BI++;
        IItmp = BBtmp->begin();
      }

      if (llvm::PHINode *PHI = llvm::dyn_cast<llvm::PHINode>(BBtmp->begin())) {
        llvm::BasicBlock::iterator PNI = BBtmp->begin();

        while (PHI) {
          std::string Rphi = getVariable(*PHI);

          if (!isSameBB && Predtmp != PHI->getParent() &&
              PAM.count(PHI) && (Rstore == Rphi.substr(0, Rphi.rfind(".")))) {
            std::cout <<"bugfix3: "<<Rstore<<instrIndex[SItmp]<<" "<<getBasicBlockIndex(SItmp->getParent())<<"->"<<getBasicBlockIndex(Predtmp)<<std::endl;
            bool otherPhi = otherPHI(Predtmp, Rstore);

            if (llvm::PHINode *PHItmp = llvm::dyn_cast<llvm::PHINode>(Predtmp->begin())) {
              llvm::BasicBlock::iterator PNItmp = Predtmp->begin();

              while (PHItmp) {
                std::string Rphitmp = getVariable(*PHItmp);

                if (Rstore == Rphitmp.substr(0, Rphitmp.rfind("."))) {
                  otherPhi = true;
                }

                PNItmp++;
                PHItmp = llvm::dyn_cast<llvm::PHINode>(PNItmp);
              }
            }

            //llvm::PHINode *predPHI;
/*
            for (auto PI = pred_begin(BBtmp), PE = pred_end(BBtmp); PI != PE; ++PI) {
              if (SItmp->getParent() == *PI)
                break;

              if (SItmp->getParent() != Predtmp) {
                if (llvm::PHINode *PHItmp = llvm::dyn_cast<llvm::PHINode>(Predtmp->begin())) {
                  llvm::BasicBlock::iterator PNItmp = Predtmp->begin();

                  while (PHItmp) {
                    std::string Rphitmp = getVariable(*PHItmp);

                    if (Rstore == Rphitmp.substr(0, Rphitmp.rfind("."))) {
                      predPHI = PHItmp;
                      otherPHI = true;
                    }

                    PNItmp++;
                    PHItmp = llvm::dyn_cast<llvm::PHINode>(PNItmp);
                  }
                }
              }
            }
*/
            if (otherPhi) {
/*              
              PROPAGATE(
                  LESSDEF(INSN(*SItmp), VAR(Rstore, Ghost), SRC),
                  BOUNDS(TyPosition::make_start_of_block(
                             TGT, getBasicBlockIndex(Predtmp)),
                         TyPosition::make_end_of_block(TGT, *Predtmp)));

              PROPAGATE(
                  LESSDEF(VAR(Rstore, Ghost),
                          VAR(getVariable(*predPHI), Physical), TGT),
                  BOUNDS(TyPosition::make_start_of_block(
                             TGT, getBasicBlockIndex(Predtmp)),
                         TyPosition::make_end_of_block(TGT, *Predtmp)));

              INFRULE(TyPosition::make(TGT, PHI->getParent()->getName(),
                                       Predtmp->getName()),
                      ConsTransitivityTgt::make(
                          VAR(Rstore, Ghost), VAR(getVariable(*predPHI), Previous),
                          VAR(Rphi, Physical)));
*/                          
            } else {
              generateHintForMem2RegPropagateStore(Predtmp, SItmp, PHI, termIndex[bname]);
            }

            if (storeItem[SItmp].expr == values[keySI]) {
              INFRULE(TyPosition::make(TGT, PHI->getParent()->getName(),
                                       Predtmp->getName()),
                      ConsTransitivityTgt::make(VAR(Rstore, Ghost),
                                                storeItem[SItmp].expr,
                                                VAR(Rphi, Physical)));
            } else {
              INFRULE(TyPosition::make(TGT, PHI->getParent()->getName(),
                                       Predtmp->getName()),
                      ConsTransitivityTgt::make(
                          VAR(Rstore, Ghost), VAR(storeItem[SItmp].op0, Ghost),
                          VAR(Rphi, Physical)));
            }

            // add hints per every use of PHI
            for (auto UI = PHI->use_begin(), E = PHI->use_end(); UI != E;) {
              llvm::Use &U = *(UI++);
              llvm::Instruction *use =
                  llvm::dyn_cast<llvm::Instruction>(U.getUser());

              int useIndex = getIndexofMem2Reg(
                  use, instrIndex[use],
                  termIndex[getBasicBlockIndex(use->getParent())]);
            std::cout << "use of PHI: PHI(" <<getVariable(*PHI)<<"), use("<<getVariable(*use)<<","<<instrIndex[use]<<","<<getBasicBlockIndex(use->getParent())<<")"<<std::endl;

              if (llvm::isa<llvm::PHINode>(use)) {
                llvm::BasicBlock* init = use->getParent();
                llvm::BasicBlock* target = PHI->getParent();
                llvm::BasicBlock* usePred;

                for (auto UI = pred_begin(init), UE = pred_end(init); UI != UE; ++UI) {
                  if (isPred(*UI, target)) {
                    usePred = *UI;
                    break;
                  }
                }

                PROPAGATE(
                    LESSDEF(INSN(*SItmp), VAR(Rstore, Ghost), SRC),
                    BOUNDS(TyPosition::make_start_of_block(
                               TGT, getBasicBlockIndex(PHI->getParent())),
                           TyPosition::make_end_of_block(TGT, *usePred)));

                PROPAGATE(
                    LESSDEF(VAR(Rstore, Ghost),
                            VAR(getVariable(*PHI), Physical), TGT),
                    BOUNDS(TyPosition::make_start_of_block(
                               TGT, getBasicBlockIndex(PHI->getParent())),
                           TyPosition::make_end_of_block(TGT, *usePred)));

                INFRULE(TyPosition::make(TGT, init->getName(),
                                         usePred->getName()),
                        ConsTransitivityTgt::make(
                            VAR(Rstore, Ghost), VAR(getVariable(*PHI), Previous),
                            VAR(getVariable(*use), Physical)));
              } else {
                PROPAGATE(
                    LESSDEF(INSN(*SItmp), VAR(Rstore, Ghost), SRC),
                    BOUNDS(TyPosition::make_start_of_block(
                               TGT, getBasicBlockIndex(PHI->getParent())),
                           TyPosition::make(SRC, *use, useIndex, "")));

                PROPAGATE(
                    LESSDEF(VAR(Rstore, Ghost),
                            VAR(getVariable(*PHI), Physical), TGT),
                    BOUNDS(TyPosition::make_start_of_block(
                               TGT, getBasicBlockIndex(PHI->getParent())),
                           TyPosition::make(SRC, *use, useIndex, "")));
              }
            }
          }
          PNI++;
          PHI = llvm::dyn_cast<llvm::PHINode>(PNI);
        }
      }

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

          if (llvm::PHINode *PHI =
                  llvm::dyn_cast<llvm::PHINode>(LI->getParent()->begin())) {
            llvm::BasicBlock::iterator PNI = LI->getParent()->begin();

            while (PHI) {
              std::string Rphi = getVariable(*PHI);

              if (!isSameBB && PAM.count(PHI) &&
                  (Rstore == Rphi.substr(0, Rphi.rfind(".")))) {
                values[Rload] = ConsVar::make(Rphi, Physical);

                PROPAGATE(
                    LESSDEF(INSN(*SItmp), VAR(Rstore, Ghost), SRC),
                    BOUNDS(TyPosition::make_start_of_block(
                               SRC, getBasicBlockIndex(LI->getParent())),
                           TyPosition::make(SRC, *LI, instrIndex[LI], "")));

                PROPAGATE(
                    LESSDEF(VAR(Rstore, Ghost),
                            VAR(Rphi, Physical), TGT),
                    BOUNDS(TyPosition::make_start_of_block(
                               SRC, getBasicBlockIndex(LI->getParent())),
                           TyPosition::make(SRC, *LI, instrIndex[LI], "")));

                if (storeItem[SItmp].expr == values[keySI]) {
                  INFRULE(TyPosition::make(TGT, LI->getParent()->getName(),
                                           Predtmp->getName()),
                          ConsTransitivityTgt::make(
                              VAR(Rstore, Ghost), storeItem[SItmp].expr,
                              VAR(getVariable(*PHI), Physical)));
                } else {
                  INFRULE(TyPosition::make(TGT, LI->getParent()->getName(),
                                           Predtmp->getName()),
                          ConsTransitivityTgt::make(
                              VAR(Rstore, Ghost),
                              VAR(storeItem[SItmp].op0, Ghost),
                              VAR(getVariable(*PHI), Physical)));
                }
              }
              PNI++;
              PHI = llvm::dyn_cast<llvm::PHINode>(PNI);
            }
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
               (LI->getParent() != SItmp->getParent()))) {
            if (llvm::PHINode *PHI =
                    llvm::dyn_cast<llvm::PHINode>(LI->getParent()->begin())) {
              llvm::BasicBlock::iterator PNI = LI->getParent()->begin();
              std::cout << "bugfixblock: "
                        << getBasicBlockIndex(LI->getParent()) << std::endl;
              bool isSamePHI = false;
              while (PHI) {
                std::string Rphi = getVariable(*PHI);

                if (/*!isSameBB &&*/ SItmp->getParent() != LI->getParent() &&
                    PAM.count(PHI) &&
                    (Rstore == Rphi.substr(0, Rphi.rfind(".")))) {
                  values[Rload] = ConsVar::make(Rphi, Physical);
                  isSamePHI = true;
                }

                PNI++;
                PHI = llvm::dyn_cast<llvm::PHINode>(PNI);
              }
              std::cout << "isSamePHI: " <<isSamePHI <<std::endl;
                  std::cout
                      << "bugfix: " << getBasicBlockIndex(SItmp->getParent())
                      << instrIndex[SItmp] << ", " << getVariable(*LI) << " "
                      << instrIndex[LI] << std::endl;

              if (!isSamePHI) { std::cout<<"!!!!!!!!!!!!!!!!!!!!!!"<<std::endl;
                generateHintForMem2RegPropagateStore(Predtmp, SItmp, LI, instrIndex[LI]);
              }
            } else {
              std::cout << "bugfix2: " << getVariable(*SItmp->getOperand(1))
                        << ", " << getVariable(*LI) << " " << instrIndex[LI]
                        << std::endl;
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

              if ((LI->getParent() == use->getParent() &&
                   instrIndex[LI] < instrIndex[use]) ||
                  (LI->getParent() != use->getParent())) {
                generateHintForMem2RegPropagateLoad(SItmp, LI, use, useIndex);
              }
            }
          }

          // propagate maydiff
          propagateMaydiffGlobal(Rload, Physical);
          propagateMaydiffGlobal(Rload, Previous);
          /*
                    // check if load is already in nop
                    std::vector<std::string>::iterator it =
                      find(checkNop.begin(), checkNop.end(), Rload);
                    if (it == checkNop.end()) {
                      // add nop
                      hints.addNopPosition
                        (TyPosition::make
                          (Target, *LI, instrIndex[LI]-1, ""));
                    }
          */
        } else if (llvm::StoreInst *SItmp2 =
                       llvm::dyn_cast<llvm::StoreInst>(I)) {
          // if we find new store to same alloca,
          // stop searching
          if (getVariable(*(SItmp2->getOperand(1))) == Ralloca) {
            SItmp = SItmp2;
            newStore = true;
            break;
          } else
            continue;
        }
      }
      isSameBB = false;
    } while (BI != BE);
  });
}

int getIndexofMem2Reg(llvm::Instruction *instr, int instrIndex, int termIndex) {
  if (llvm::isa<llvm::TerminatorInst>(*instr))
    return termIndex;
  else
    return instrIndex;
}

} // llvmberry
