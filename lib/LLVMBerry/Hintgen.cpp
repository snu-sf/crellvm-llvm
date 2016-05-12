#include "llvm/LLVMBerry/Hintgen.h"
#include "llvm/LLVMBerry/Infrules.h"
#include "llvm/LLVMBerry/ValidationUnit.h"

namespace llvmberry{
// insert nop at tgt where I is at src
void insertTgtNopAtSrcI(CoreHint &hints, llvm::Instruction *I) {
  std::string empty_str = "";
  if (I == I->getParent()->getFirstNonPHI()) {
    std::string nop_block_name = getBasicBlockIndex(I->getParent());
    hints.addNopPosition(TyPosition::make(TyScope::Target, nop_block_name, empty_str));
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
    hints.addNopPosition(TyPosition::make(TyScope::Source, nop_block_name, empty_str));
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
void propagateInstruction(llvm::Instruction *from, llvm::Instruction *to, TyScope scope, bool propagateEquivalence) {
  assert(ValidationUnit::Exists());
  ValidationUnit::GetInstance()->intrude([&from, &to, &scope, &propagateEquivalence](
      ValidationUnit::Dictionary &data, CoreHint &hints) {
    std::string reg_name = getVariable(*from);

    if(scope == Source){
      hints.addCommand(ConsPropagate::make(
          ConsLessdef::make(
              ConsVar::make(reg_name, Physical),
              ConsRhs::make(reg_name, Physical, scope),
              scope),
          ConsBounds::make(
              TyPosition::make(scope, *from),
              TyPosition::make(scope, *to))));
      if(propagateEquivalence){
        hints.addCommand(ConsPropagate::make(
            ConsLessdef::make(
                ConsRhs::make(reg_name, Physical, scope),
                ConsVar::make(reg_name, Physical),
                scope),
            ConsBounds::make(
                TyPosition::make(scope, *from),
                TyPosition::make(scope, *to))));
      }
    }else if(scope == Target){
      hints.addCommand(ConsPropagate::make(
          ConsLessdef::make(
              ConsRhs::make(reg_name, Physical, scope),
              ConsVar::make(reg_name, Physical),
              scope),
          ConsBounds::make(
              TyPosition::make(scope, *from),
              TyPosition::make(scope, *to))));
      if(propagateEquivalence){
        hints.addCommand(ConsPropagate::make(
            ConsLessdef::make(
                ConsVar::make(reg_name, Physical),
                ConsRhs::make(reg_name, Physical, scope),
                scope),
            ConsBounds::make(
                TyPosition::make(scope, *from),
                TyPosition::make(scope, *to))));
      }
    }else{
      assert("propagateInstruction() : scope is neither Source nor Target" && false);
    }
  });
}

void propagateLessdef(llvm::Instruction *from, llvm::Instruction *to, const llvm::Value *lesserval,
        const llvm::Value *greaterval, TyScope scope) {
  assert(ValidationUnit::Exists());
  ValidationUnit::GetInstance()->intrude([&from, &to, &lesserval, &greaterval, &scope](
      ValidationUnit::Dictionary &data, CoreHint &hints) {
    hints.addCommand(ConsPropagate::make(
      ConsLessdef::make(
          TyExpr::make(*greaterval),
          TyExpr::make(*lesserval),
          scope),
      ConsBounds::make(
          TyPosition::make(scope, *from),
          TyPosition::make(scope, *to))));
  });
}

void applyCommutativity(llvm::Instruction *position, llvm::BinaryOperator *expression, TyScope scope){
  assert(ValidationUnit::Exists());
  
  ValidationUnit::GetInstance()->intrude([&position, &expression, &scope](
      ValidationUnit::Dictionary &data, CoreHint &hints){
    int bitwidth = isFloatOpcode(expression->getOpcode()) ? -1 : expression->getType()->getIntegerBitWidth();
    std::string regname = getVariable(*expression);
    if(scope == Source){
      switch(expression->getOpcode()){
      case llvm::Instruction::Add :
        hints.addCommand(ConsInfrule::make(
          TyPosition::make(Source, *position),
          ConsAddCommutative::make(
            TyRegister::make(regname, Physical),
            TyValue::make(*expression->getOperand(0)),
            TyValue::make(*expression->getOperand(1)),
            ConsSize::make(bitwidth))));
        break;
      case llvm::Instruction::And :
        hints.addCommand(ConsInfrule::make(
          TyPosition::make(Source, *position),
          ConsAndCommutative::make(
            TyRegister::make(regname, Physical),
            TyValue::make(*expression->getOperand(0)),
            TyValue::make(*expression->getOperand(1)),
            ConsSize::make(bitwidth))));
        break;
      case llvm::Instruction::Mul : 
        hints.addCommand(ConsInfrule::make(
          TyPosition::make(Source, *position),
          ConsMulCommutative::make(
            TyRegister::make(regname, Physical),
            TyValue::make(*expression->getOperand(0)),
            TyValue::make(*expression->getOperand(1)),
            ConsSize::make(bitwidth))));
        break;
      case llvm::Instruction::Or : 
        hints.addCommand(ConsInfrule::make(
          TyPosition::make(Source, *position),
          ConsOrCommutative::make(
            TyRegister::make(regname, Physical),
            TyValue::make(*expression->getOperand(0)),
            TyValue::make(*expression->getOperand(1)),
            ConsSize::make(bitwidth))));
        break;
      case llvm::Instruction::Xor :
        hints.addCommand(ConsInfrule::make(
          TyPosition::make(Source, *position),
          ConsXorCommutative::make(
            TyRegister::make(regname, Physical),
            TyValue::make(*expression->getOperand(0)),
            TyValue::make(*expression->getOperand(1)),
            ConsSize::make(bitwidth))));
        break;
      default:
        assert("applyCommutativity() : we don't support commutativity rule for this binary operator");
      }
    }else if(scope == Target){
      switch(expression->getOpcode()){
      case llvm::Instruction::Add :
        hints.addCommand(ConsInfrule::make(
          TyPosition::make(Target, *position),
          ConsAddCommutativeTgt::make(
            TyRegister::make(regname, Physical),
            TyValue::make(*expression->getOperand(0)),
            TyValue::make(*expression->getOperand(1)),
            ConsSize::make(bitwidth))));
        break;
       case llvm::Instruction::Xor :
        hints.addCommand(ConsInfrule::make(
          TyPosition::make(Target, *position),
          ConsXorCommutativeTgt::make(
            TyRegister::make(regname, Physical),
            TyValue::make(*expression->getOperand(0)),
            TyValue::make(*expression->getOperand(1)),
            ConsSize::make(bitwidth))));
        break;
      case llvm::Instruction::FAdd :
        hints.addCommand(ConsInfrule::make(
          TyPosition::make(Target, *position),
          ConsFaddCommutativeTgt::make(
            TyRegister::make(regname, Physical),
            TyValue::make(*expression->getOperand(0)),
            TyValue::make(*expression->getOperand(1)),
            getFloatType(expression->getType()))));
      case llvm::Instruction::Or : 
        INFRULE(INSTPOS(llvmberry::Source, position),
          ConsOrCommutative::make(REGISTER(regname, Physical),
            VAL(expression->getOperand(0), Physical),
            VAL(expression->getOperand(1), Physical),
            BITSIZE(bitwidth)));
        break;
      default:
        assert("applyCommutativity() : we don't support commutativity rule for this binary operator");
      }
    }else{
      assert("applyCommutativity() : scope is neither Source nor Target" && false);
    }
  }); 
}

void applyTransitivity(llvm::Instruction *position, llvm::Value *v_greatest, llvm::Value *v_mid, llvm::Value *v_smallest, TyScope scope) {
  assert(ValidationUnit::Exists());
  
  ValidationUnit::GetInstance()->intrude([&position, &v_smallest, &v_mid, &v_greatest, &scope](
      ValidationUnit::Dictionary &data, CoreHint &hints){
    hints.addCommand(ConsInfrule::make(
       INSTPOS(scope, position),
       ConsTransitivity::make(
         TyExpr::make(*v_greatest, Physical),
         TyExpr::make(*v_mid, Physical),
         TyExpr::make(*v_smallest, Physical))));
  });
}

void propagateMaydiffGlobal(std::string varname, TyTag tag) {
  assert(ValidationUnit::Exists());
  
  ValidationUnit::GetInstance()->intrude([&varname, &tag](
      Dictionary &data, CoreHint &hints) {
    hints.addCommand(ConsPropagate::make(
        ConsMaydiff::make(varname, tag),
        ConsGlobal::make()));
  });
}

void generateHintForNegValue(llvm::Value *V, llvm::BinaryOperator &I, TyScope scope) {
  assert(ValidationUnit::Exists());

  if (llvm::BinaryOperator::isNeg(V)) {
    ValidationUnit::GetInstance()->intrude([&V, &I, &scope](
        ValidationUnit::Dictionary &data,
        CoreHint &hints) {

      std::string reg0_name = getVariable(I); // z = x -my
      std::string reg1_name = getVariable(*V); // my

      llvm::Instruction *Vins = llvm::dyn_cast<llvm::Instruction>(V);

      hints.addCommand(ConsPropagate::make(
          ConsLessdef::make(
              ConsVar::make(reg1_name, Physical), // my = -y
              ConsRhs::make(reg1_name, Physical, scope),
              scope),
          ConsBounds::make(
              TyPosition::make(scope, *Vins), // From my to z = x -my
              TyPosition::make(scope, I))));

      hints.addCommand(ConsPropagate::make(
          ConsLessdef::make(
              ConsRhs::make(reg1_name, Physical, scope),
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
        ValidationUnit::Dictionary &data,
        CoreHint &hints) {

      std::string reg0_name = getVariable(I); // z = x -my

      unsigned sz_bw = I.getType()->getPrimitiveSizeInBits();
      int64_t c1 = C->getSExtValue();
      int64_t c2 = -c1;

      hints.addCommand(ConsInfrule::make(
          TyPosition::make(scope, I),
          ConsNegVal::make(TyConstInt::make(c1, sz_bw),
                                      TyConstInt::make(c2, sz_bw),
                                      ConsSize::make(sz_bw))));

      hints.addCommand(ConsInfrule::make(
          TyPosition::make(scope, I),
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

void generateHintForReplaceAllUsesWith(llvm::Instruction *source, llvm::Value *replaceTo){
  assert(ValidationUnit::Exists());
  
  ValidationUnit::GetInstance()->intrude([&source, &replaceTo](
      ValidationUnit::Dictionary &data, CoreHint &hints) {
    llvm::Instruction *I = source;
    llvm::Value *repl = replaceTo;

    std::string to_rem = getVariable(*I);

    for (auto UI = I->use_begin(); UI != I->use_end(); ++UI) {
      if (!llvm::isa<llvm::Instruction>(UI->getUser())) {
        // let the validation fail when the user is not an instruction
        return;
      }
      std::string user = getVariable(*UI->getUser());
      llvm::Instruction *user_I = llvm::dyn_cast<llvm::Instruction>(UI->getUser());

      std::string prev_block_name = "";
      if (llvm::isa<llvm::PHINode>(user_I)) {
        llvm::BasicBlock *bb_from =
            llvm::dyn_cast<llvm::PHINode>(user_I)->getIncomingBlock(*UI);
        prev_block_name = getBasicBlockIndex(bb_from);
      }

      hints.addCommand(ConsPropagate::make(
          ConsLessdef::make(
              ConsVar::make(to_rem, Physical),
              TyExpr::make(*repl, Physical),
              Source),
          ConsBounds::make(
              TyPosition::make(Source, *I),
              TyPosition::make(Source, *user_I, prev_block_name))));
      if (llvm::isa<llvm::PHINode>(user_I)) {
        hints.addCommand(ConsInfrule::make(
            TyPosition::make(Source, *user_I,
                                        prev_block_name),
            ConsTransitivity::make(
                ConsVar::make(user, Physical),
                ConsVar::make(to_rem, Previous),
                TyExpr::make(*repl, Previous))));
      } else if (!user.empty() && !llvm::isa<llvm::CallInst>(user_I)) {
        hints.addCommand(ConsInfrule::make(
            TyPosition::make(Source, *user_I,
                                        prev_block_name),
            ConsReplaceRhs::make(
                TyRegister::make(to_rem, Physical),
                TyValue::make(*repl, Physical),
                ConsVar::make(user, Physical),
                ConsRhs::make(user, Physical,
                                         Source),
                ConsRhs::make(user, Physical,
                                         Target))));
      }
    }
  });

}

void generateHintForAddSelectZero(llvm::BinaryOperator *Z, 
                                  llvm::BinaryOperator *X, 
                                  llvm::SelectInst *Y, 
                                  bool needs_commutativity,
                                  bool is_leftform){
  assert(ValidationUnit::Exists());
  assert(Z);
  assert(X);
  assert(Y);
  
  ValidationUnit::GetInstance()->intrude([&Z, &X, &Y, 
                needs_commutativity, is_leftform](
      ValidationUnit::Dictionary &data,
      CoreHint &hints) {
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
        ConsLessdef::make(
            ConsVar::make(reg_x_name, Physical),
            ConsRhs::make(reg_x_name, Physical, Source),
            Source),
        ConsBounds::make(
            TyPosition::make(Source, *X),
            TyPosition::make(Source, *Z))));

    // Propagate "Y = select c ? x : 0" or "Y = select c ? 0 : x"
    hints.addCommand(ConsPropagate::make(
        ConsLessdef::make(
            ConsVar::make(reg_y_name, Physical),
            ConsRhs::make(reg_y_name, Physical, Source),
            Source),
        ConsBounds::make(
            TyPosition::make(Source, *Y),
            TyPosition::make(Source, *Z))));

    if(needs_commutativity){
      hints.addCommand(ConsInfrule::make(
        TyPosition::make(Source, *Z),
        ConsAddCommutative::make(
            TyRegister::make(reg_z_name, Physical),
            TyValue::make(*Y),
            TyValue::make(*a_Z),
            ConsSize::make(bitwidth))));
    }
    
    if(is_leftform){
      hints.addCommand(ConsInfrule::make(
        TyPosition::make(Source, *Z),
        ConsAddSelectZero::make(
            TyRegister::make(reg_z_name, Physical),
            TyRegister::make(reg_x_name, Physical),
            TyRegister::make(reg_y_name, Physical),
            TyValue::make(*c),
            TyValue::make(*n),
            TyValue::make(*a),
            ConsSize::make(bitwidth))));
    }else{
      hints.addCommand(ConsInfrule::make(
        TyPosition::make(Source, *Z),
        ConsAddSelectZero2::make(
            TyRegister::make(reg_z_name, Physical),
            TyRegister::make(reg_x_name, Physical),
            TyRegister::make(reg_y_name, Physical),
            TyValue::make(*c),
            TyValue::make(*n),
            TyValue::make(*a),
            ConsSize::make(bitwidth))));
    }
  });
}

void generateHintForOrAnd(llvm::BinaryOperator *Y, llvm::Value *X, llvm::Value *A){
  assert(ValidationUnit::Exists());

  ValidationUnit::GetInstance()->intrude([Y, X, A](Dictionary &data, CoreHint &hints){
    auto ptr = data.get<ArgForSimplifyOrInst>();
    bool isSwapped = ptr->isSwapped;
    ptr->setHintGenFunc("or_and", [Y, X, A, isSwapped, &hints](llvm::Instruction *I){
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
      INFRULE(INSTPOS(SRC, Z), ConsOrAnd::make(
          VAL(Z, Physical), VAL(Y, Physical), VAL(X, Physical), VAL(A, Physical),
          BITSIZE(Z->getType()->getIntegerBitWidth())));
    });
  });
}

void generateHintForOrXor(llvm::BinaryOperator *W, llvm::Value *op0, llvm::Value *op1, bool needsCommutativity){
  assert(ValidationUnit::Exists());
  
  ValidationUnit::GetInstance()->intrude([&W, &op0, &op1, &needsCommutativity](
      ValidationUnit::Dictionary &data,
      CoreHint &hints) {
    //    <src>    |   <tgt>
    // X = B ^ -1  | X = B ^ -1
    // Y = A & X   | Y = A & X
    // Z = A ^ B   | Z = A ^ B
    // W = Y | Z   | W = A ^ B
    llvm::BinaryOperator *Z = llvm::dyn_cast<llvm::BinaryOperator>(op1);
    llvm::BinaryOperator *Y = llvm::dyn_cast<llvm::BinaryOperator>(op0);
    llvm::BinaryOperator *X = llvm::dyn_cast<llvm::BinaryOperator>(Y->getOperand(1));
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
    if(X->getOperand(1) == B){
      applyCommutativity(W, X, Source);
    }

    if(needsCommutativity){
      applyCommutativity(W, W, Source);
    }
   
    hints.addCommand(ConsInfrule::make(
        TyPosition::make(Source, *W),
        ConsOrXor::make(
            TyValue::make(*W), 
            TyValue::make(*Z), 
            TyValue::make(*X), 
            TyValue::make(*Y), 
            TyValue::make(*A), 
            TyValue::make(*B), 
            ConsSize::make(bitwidth))));

  });
}

void generateHintForOrXor2(llvm::BinaryOperator *Z, 
        llvm::Value *X1_val, llvm::Value *X2_val,
        llvm::Value *A, llvm::Value *B,
        bool needsY1Commutativity, bool needsY2Commutativity){
  assert(ValidationUnit::Exists());
  
  ValidationUnit::GetInstance()->intrude([&Z, &X1_val, &X2_val, &A, &B,
      &needsY1Commutativity,
      &needsY2Commutativity](Dictionary &data, CoreHint &hints) {
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
    assert(Y1);
    assert(Y2);
    assert(X1);
    assert(X2);
    int bitwidth = Z->getType()->getIntegerBitWidth();
  
    propagateInstruction(X1, Z, Source);
    propagateInstruction(X2, Z, Source);
    propagateInstruction(Y1, Z, Source);
    propagateInstruction(Y2, Z, Source);
    if(X1->getOperand(1) == B)
      applyCommutativity(Z, X1, Source);
    if(X2->getOperand(1) == A)
      applyCommutativity(Z, X2, Source);
    if(needsY1Commutativity)
      applyCommutativity(Z, Y1, Source);
    if(needsY2Commutativity)
      applyCommutativity(Z, Y2, Source);
   
    hints.addCommand(ConsInfrule::make(
      TyPosition::make(Target, *Z),
      ConsOrXor2::make(
          TyValue::make(*Z), 
          TyValue::make(*X1), 
          TyValue::make(*Y1), 
          TyValue::make(*X2), 
          TyValue::make(*Y2), 
          TyValue::make(*A), 
          TyValue::make(*B), 
          ConsSize::make(bitwidth))));
   
  });
}

void generateHintForOrXor4(llvm::BinaryOperator *Z,
          llvm::Value *X,
          llvm::BinaryOperator *Y,
          llvm::BinaryOperator *A,
          llvm::Value *B,
          llvm::BinaryOperator *NB,
          bool needsYCommutativity,
          bool needsZCommutativity) {
  assert(ValidationUnit::Exists());
  
  ValidationUnit::GetInstance()->intrude([&Z, &X, &Y, &A, &B, &NB, needsYCommutativity, needsZCommutativity]
      (Dictionary &data, CoreHint &hints) {
    // <src>      |  <tgt>
    // A = X ^ -1 | A = X ^ -1
    // Y = A ^ B  | Y = A ^ B
    // <nop>      | NB = B ^ -1
    // Z = X | Y  | Z = NB | X
    int bitwidth = Z->getType()->getIntegerBitWidth();
    propagateInstruction(A, Z, Target);
    propagateInstruction(Y, Z, Target);
    propagateInstruction(NB, Z, Target);
    if(needsYCommutativity)
      applyCommutativity(Z, Y, Target);
    insertSrcNopAtTgtI(hints, NB);
    propagateMaydiffGlobal(getVariable(*NB), Physical);

    INFRULE(INSTPOS(TGT, Z), ConsOrXor4::make(
        VAL(Z, Physical), VAL(X, Physical), VAL(Y, Physical),
        VAL(A, Physical), VAL(B, Physical), VAL(NB, Physical),
        BITSIZE(bitwidth)));
    if(needsZCommutativity)
      INFRULE(INSTPOS(TGT, Z), ConsOrCommutativeTgt::make(
        REGISTER(llvmberry::getVariable(*Z), Physical),
        VAL(X, Physical), VAL(Y, Physical), BITSIZE(bitwidth)));
  });

}

void generateHintForAddXorAnd(llvm::BinaryOperator *Z, 
        llvm::BinaryOperator *X,
        llvm::BinaryOperator *Y,
        llvm::Value *A, llvm::Value *B,
        bool needsYCommutativity, bool needsZCommutativity){
  ValidationUnit::GetInstance()->intrude([&Z, &X, &Y,
      &needsYCommutativity,
      &needsZCommutativity]
      (ValidationUnit::Dictionary &data,
      CoreHint &hints) {
    //    <src>       <tgt>
    // X = A ^ B  | X = A ^ B
    // Y = A & B  | Y = A & B
    // Z = X + Y  | Z = A | B
    llvm::Value *A = X->getOperand(0);
    llvm::Value *B = X->getOperand(1);
    int bitwidth = Z->getType()->getIntegerBitWidth();
  
    propagateInstruction(X, Z, Source);
    propagateInstruction(Y, Z, Source);
   
    if(needsYCommutativity)
      applyCommutativity(Z, Y, Source);
    if(needsZCommutativity)
      applyCommutativity(Z, Z, Source);

    hints.addCommand(ConsInfrule::make(
      TyPosition::make(Source, *Z),
      ConsAddXorAnd::make(
          TyRegister::make(getVariable(*Z), Physical),
          TyValue::make(*A), 
          TyValue::make(*B), 
          TyRegister::make(getVariable(*X), Physical),
          TyRegister::make(getVariable(*Y), Physical),
          ConsSize::make(bitwidth))));
  });
}

void generateHintForAddOrAnd(llvm::BinaryOperator *Z, 
        llvm::BinaryOperator *X,
        llvm::BinaryOperator *Y,
        llvm::Value *A, llvm::Value *B,
        bool needsYCommutativity, bool needsZCommutativity){
  assert(ValidationUnit::Exists());
  
  ValidationUnit::GetInstance()->intrude([&Z, &X, &Y,
      &needsYCommutativity,
      &needsZCommutativity]
      (ValidationUnit::Dictionary &data,
      CoreHint &hints) {
    //    <src>       <tgt>
    // X = A ^ B  | X = A ^ B
    // Y = A & B  | Y = A & B
    // Z = X + Y  | Z = A | B
    llvm::Value *A = X->getOperand(0);
    llvm::Value *B = X->getOperand(1);
    int bitwidth = Z->getType()->getIntegerBitWidth();
  
    propagateInstruction(X, Z, Source);
    propagateInstruction(Y, Z, Source);
   
    if(needsYCommutativity)
      applyCommutativity(Z, Y, Source);
    if(needsZCommutativity)
      applyCommutativity(Z, Z, Source);

    hints.addCommand(ConsInfrule::make(
      TyPosition::make(Source, *Z),
      ConsAddOrAnd::make(
          TyRegister::make(getVariable(*Z), Physical),
          TyValue::make(*A), 
          TyValue::make(*B), 
          TyRegister::make(getVariable(*X), Physical),
          TyRegister::make(getVariable(*Y), Physical),
          ConsSize::make(bitwidth))));
  });
}

void generateHintForAndOr(llvm::BinaryOperator *Z,
          llvm::Value *X,
          llvm::BinaryOperator *Y,
          llvm::Value *A,
          bool needsZCommutativity){
  assert(ValidationUnit::Exists());

  ValidationUnit::GetInstance()->intrude([&Z, &X, &Y, &A,
      &needsZCommutativity]
      (ValidationUnit::Dictionary &data, CoreHint &hints) {
    assert(Z);

    propagateInstruction(Y, Z, Source);
    if(Y->getOperand(0) != X)
      applyCommutativity(Z, Y, Source);
    if(needsZCommutativity)
      applyCommutativity(Z, Z, Source);
    hints.addCommand(ConsInfrule::make(
        INSTPOS(Source, Z),
        ConsAndOr::make(
            VAL(Z, Physical), VAL(X, Physical), VAL(Y, Physical), VAL(A, Physical),
            ConsSize::make(Z->getType()->getIntegerBitWidth()))));
  });
}

std::pair<std::shared_ptr<TyExpr>, std::shared_ptr<TyExpr>> false_encoding =
    std::make_pair(llvmberry::ConsConst::make(0, 64),
                   llvmberry::ConsConst::make(42, 64));
}
