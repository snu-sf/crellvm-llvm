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
      assert("propagateInstruction() : scope is neither llvmberry::Source nor llvmberry::Target" && false);
    }
  });
}

void propagateLessdef(llvm::Instruction *from, llvm::Instruction *to, const llvm::Value *lesserval,
        const llvm::Value *greaterval, TyScope scope) {
  assert(ValidationUnit::Exists());
  ValidationUnit::GetInstance()->intrude([&from, &to, &lesserval, &greaterval, &scope](
      ValidationUnit::Dictionary &data, CoreHint &hints) {
    hints.addCommand(llvmberry::ConsPropagate::make(
      llvmberry::ConsLessdef::make(
          llvmberry::TyExpr::make(*greaterval),
          llvmberry::TyExpr::make(*lesserval),
          scope),
      llvmberry::ConsBounds::make(
          llvmberry::TyPosition::make(scope, *from),
          llvmberry::TyPosition::make(scope, *to))));
  });
}

void applyCommutativity(llvm::Instruction *position, llvm::BinaryOperator *expression, TyScope scope){
  assert(llvmberry::ValidationUnit::Exists());
  
  llvmberry::ValidationUnit::GetInstance()->intrude([&position, &expression, &scope](
      llvmberry::ValidationUnit::Dictionary &data, llvmberry::CoreHint &hints){
    int bitwidth = llvmberry::isFloatOpcode(expression->getOpcode()) ? -1 : expression->getType()->getIntegerBitWidth();
    std::string regname = llvmberry::getVariable(*expression);
    if(scope == llvmberry::Source){
      switch(expression->getOpcode()){
      case llvm::Instruction::Add :
        hints.addCommand(llvmberry::ConsInfrule::make(
          llvmberry::TyPosition::make(llvmberry::Source, *position),
          llvmberry::ConsAddCommutative::make(
            llvmberry::TyRegister::make(regname, llvmberry::Physical),
            llvmberry::TyValue::make(*expression->getOperand(0)),
            llvmberry::TyValue::make(*expression->getOperand(1)),
            llvmberry::ConsSize::make(bitwidth))));
        break;
      case llvm::Instruction::And :
        hints.addCommand(llvmberry::ConsInfrule::make(
          llvmberry::TyPosition::make(llvmberry::Source, *position),
          llvmberry::ConsAndCommutative::make(
            llvmberry::TyRegister::make(regname, llvmberry::Physical),
            llvmberry::TyValue::make(*expression->getOperand(0)),
            llvmberry::TyValue::make(*expression->getOperand(1)),
            llvmberry::ConsSize::make(bitwidth))));
        break;
      case llvm::Instruction::Mul : 
        hints.addCommand(llvmberry::ConsInfrule::make(
          llvmberry::TyPosition::make(llvmberry::Source, *position),
          llvmberry::ConsMulCommutative::make(
            llvmberry::TyRegister::make(regname, llvmberry::Physical),
            llvmberry::TyValue::make(*expression->getOperand(0)),
            llvmberry::TyValue::make(*expression->getOperand(1)),
            llvmberry::ConsSize::make(bitwidth))));
        break;
      case llvm::Instruction::Or : 
        hints.addCommand(llvmberry::ConsInfrule::make(
          llvmberry::TyPosition::make(llvmberry::Source, *position),
          llvmberry::ConsOrCommutative::make(
            llvmberry::TyRegister::make(regname, llvmberry::Physical),
            llvmberry::TyValue::make(*expression->getOperand(0)),
            llvmberry::TyValue::make(*expression->getOperand(1)),
            llvmberry::ConsSize::make(bitwidth))));
        break;
      case llvm::Instruction::Xor :
        hints.addCommand(llvmberry::ConsInfrule::make(
          llvmberry::TyPosition::make(llvmberry::Source, *position),
          llvmberry::ConsXorCommutative::make(
            llvmberry::TyRegister::make(regname, llvmberry::Physical),
            llvmberry::TyValue::make(*expression->getOperand(0)),
            llvmberry::TyValue::make(*expression->getOperand(1)),
            llvmberry::ConsSize::make(bitwidth))));
        break;
      default:
        assert("applyCommutativity() : we don't support commutativity rule for this binary operator");
      }
    }else if(scope == llvmberry::Target){
      switch(expression->getOpcode()){
      case llvm::Instruction::Add :
        hints.addCommand(llvmberry::ConsInfrule::make(
          llvmberry::TyPosition::make(llvmberry::Target, *position),
          llvmberry::ConsAddCommutativeTgt::make(
            llvmberry::TyRegister::make(regname, llvmberry::Physical),
            llvmberry::TyValue::make(*expression->getOperand(0)),
            llvmberry::TyValue::make(*expression->getOperand(1)),
            llvmberry::ConsSize::make(bitwidth))));
        break;
       case llvm::Instruction::Xor :
        hints.addCommand(llvmberry::ConsInfrule::make(
          llvmberry::TyPosition::make(llvmberry::Target, *position),
          llvmberry::ConsXorCommutativeTgt::make(
            llvmberry::TyRegister::make(regname, llvmberry::Physical),
            llvmberry::TyValue::make(*expression->getOperand(0)),
            llvmberry::TyValue::make(*expression->getOperand(1)),
            llvmberry::ConsSize::make(bitwidth))));
        break;
      case llvm::Instruction::FAdd :
        hints.addCommand(llvmberry::ConsInfrule::make(
          llvmberry::TyPosition::make(llvmberry::Target, *position),
          llvmberry::ConsFaddCommutativeTgt::make(
            llvmberry::TyRegister::make(regname, llvmberry::Physical),
            llvmberry::TyValue::make(*expression->getOperand(0)),
            llvmberry::TyValue::make(*expression->getOperand(1)),
            llvmberry::getFloatType(expression->getType()))));
      default:
        assert("applyCommutativity() : we don't support commutativity rule for this binary operator");
      }
    }else{
      assert("applyCommutativity() : scope is neither llvmberry::Source nor llvmberry::Target" && false);
    }
  }); 
}

void applyTransitivity(llvm::Instruction *position, llvm::Value *v_greatest, llvm::Value *v_mid, llvm::Value *v_smallest, TyScope scope) {
  assert(llvmberry::ValidationUnit::Exists());
  
  llvmberry::ValidationUnit::GetInstance()->intrude([&position, &v_smallest, &v_mid, &v_greatest, &scope](
      llvmberry::ValidationUnit::Dictionary &data, llvmberry::CoreHint &hints){
    hints.addCommand(llvmberry::ConsInfrule::make(
       INSTPOS(scope, position),
       llvmberry::ConsTransitivity::make(
         TyExpr::make(*v_greatest, Physical),
         TyExpr::make(*v_mid, Physical),
         TyExpr::make(*v_smallest, Physical))));
  });
}

void generateHintForNegValue(llvm::Value *V, llvm::BinaryOperator &I, TyScope scope) {
  assert(llvmberry::ValidationUnit::Exists());

  if (llvm::BinaryOperator::isNeg(V)) {
    llvmberry::ValidationUnit::GetInstance()->intrude([&V, &I, &scope](
        llvmberry::ValidationUnit::Dictionary &data,
        llvmberry::CoreHint &hints) {

      std::string reg0_name = llvmberry::getVariable(I); // z = x -my
      std::string reg1_name = llvmberry::getVariable(*V); // my

      llvm::Instruction *Vins = llvm::dyn_cast<llvm::Instruction>(V);

      hints.addCommand(llvmberry::ConsPropagate::make(
          llvmberry::ConsLessdef::make(
              llvmberry::ConsVar::make(reg1_name, llvmberry::Physical), // my = -y
              llvmberry::ConsRhs::make(reg1_name, llvmberry::Physical, scope),
              scope),
          llvmberry::ConsBounds::make(
              llvmberry::TyPosition::make(scope, *Vins), // From my to z = x -my
              llvmberry::TyPosition::make(scope, I))));

      hints.addCommand(llvmberry::ConsPropagate::make(
          llvmberry::ConsLessdef::make(
              llvmberry::ConsRhs::make(reg1_name, llvmberry::Physical, scope),
              llvmberry::ConsVar::make(reg1_name, llvmberry::Physical), // my = -y
              scope),
          llvmberry::ConsBounds::make(
              llvmberry::TyPosition::make(scope, *Vins), // From my to z = x -my
              llvmberry::TyPosition::make(scope, I))));
    });
  }
  // Constants can be considered to be negated values if they can be folded.
  if (llvm::ConstantInt *C = llvm::dyn_cast<llvm::ConstantInt>(V)) {
    llvmberry::ValidationUnit::GetInstance()->intrude([&I, &V, &C, &scope](
        llvmberry::ValidationUnit::Dictionary &data,
        llvmberry::CoreHint &hints) {

      std::string reg0_name = llvmberry::getVariable(I); // z = x -my

      unsigned sz_bw = I.getType()->getPrimitiveSizeInBits();
      int64_t c1 = C->getSExtValue();
      int64_t c2 = -c1;

      hints.addCommand(llvmberry::ConsInfrule::make(
          llvmberry::TyPosition::make(scope, I),
          llvmberry::ConsNegVal::make(llvmberry::TyConstInt::make(c1, sz_bw),
                                      llvmberry::TyConstInt::make(c2, sz_bw),
                                      llvmberry::ConsSize::make(sz_bw))));

      hints.addCommand(llvmberry::ConsInfrule::make(
          llvmberry::TyPosition::make(scope, I),
          llvmberry::ConsNegVal::make(llvmberry::TyConstInt::make(c1, sz_bw),
                                      llvmberry::TyConstInt::make(c2, sz_bw),
                                      llvmberry::ConsSize::make(sz_bw))));
    });
  }
  //  if(ConstantDataVector *C = dyn_cast<ConstantDataVector>(V))
  //  {
  //  Todo
  //  }
}

void generateHintForReplaceAllUsesWith(llvm::Instruction *source, llvm::Value *replaceTo){
  assert(llvmberry::ValidationUnit::Exists());
  
  llvmberry::ValidationUnit::GetInstance()->intrude([&source, &replaceTo](
      llvmberry::ValidationUnit::Dictionary &data, llvmberry::CoreHint &hints) {
    llvm::Instruction *I = source;
    llvm::Value *repl = replaceTo;

    std::string to_rem = llvmberry::getVariable(*I);

    for (auto UI = I->use_begin(); UI != I->use_end(); ++UI) {
      if (!llvm::isa<llvm::Instruction>(UI->getUser())) {
        // let the validation fail when the user is not an instruction
        return;
      }
      std::string user = llvmberry::getVariable(*UI->getUser());
      llvm::Instruction *user_I = llvm::dyn_cast<llvm::Instruction>(UI->getUser());

      std::string prev_block_name = "";
      if (llvm::isa<llvm::PHINode>(user_I)) {
        llvm::BasicBlock *bb_from =
            llvm::dyn_cast<llvm::PHINode>(user_I)->getIncomingBlock(*UI);
        prev_block_name = llvmberry::getBasicBlockIndex(bb_from);
      }

      hints.addCommand(llvmberry::ConsPropagate::make(
          llvmberry::ConsLessdef::make(
              llvmberry::ConsVar::make(to_rem, llvmberry::Physical),
              llvmberry::TyExpr::make(*repl, llvmberry::Physical),
              llvmberry::Source),
          llvmberry::ConsBounds::make(
              llvmberry::TyPosition::make(llvmberry::Source, *I),
              llvmberry::TyPosition::make(llvmberry::Source, *user_I, prev_block_name))));
      if (llvm::isa<llvm::PHINode>(user_I)) {
        hints.addCommand(llvmberry::ConsInfrule::make(
            llvmberry::TyPosition::make(llvmberry::Source, *user_I,
                                        prev_block_name),
            llvmberry::ConsTransitivity::make(
                llvmberry::ConsVar::make(user, llvmberry::Physical),
                llvmberry::ConsVar::make(to_rem, llvmberry::Previous),
                llvmberry::TyExpr::make(*repl, llvmberry::Previous))));
      } else if (!user.empty() && !llvm::isa<llvm::CallInst>(user_I)) {
        hints.addCommand(llvmberry::ConsInfrule::make(
            llvmberry::TyPosition::make(llvmberry::Source, *user_I,
                                        prev_block_name),
            llvmberry::ConsReplaceRhs::make(
                llvmberry::TyRegister::make(to_rem, llvmberry::Physical),
                llvmberry::TyValue::make(*repl, llvmberry::Physical),
                llvmberry::ConsVar::make(user, llvmberry::Physical),
                llvmberry::ConsRhs::make(user, llvmberry::Physical,
                                         llvmberry::Source),
                llvmberry::ConsRhs::make(user, llvmberry::Physical,
                                         llvmberry::Target))));
      }
    }
  });

}

void generateHintForAddSelectZero(llvm::BinaryOperator *Z, 
                                  llvm::BinaryOperator *X, 
                                  llvm::SelectInst *Y, 
                                  bool needs_commutativity,
                                  bool is_leftform){
  assert(llvmberry::ValidationUnit::Exists());
  assert(Z);
  assert(X);
  assert(Y);
  
  llvmberry::ValidationUnit::GetInstance()->intrude([&Z, &X, &Y, 
                needs_commutativity, is_leftform](
      llvmberry::ValidationUnit::Dictionary &data,
      llvmberry::CoreHint &hints) {
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
    std::string reg_y_name = llvmberry::getVariable(*Y);
    std::string reg_z_name = llvmberry::getVariable(*Z);
    std::string reg_x_name = llvmberry::getVariable(*X);

    // Propagate "X = n - a"
    hints.addCommand(llvmberry::ConsPropagate::make(
        llvmberry::ConsLessdef::make(
            llvmberry::ConsVar::make(reg_x_name, llvmberry::Physical),
            llvmberry::ConsRhs::make(reg_x_name, llvmberry::Physical, llvmberry::Source),
            llvmberry::Source),
        llvmberry::ConsBounds::make(
            llvmberry::TyPosition::make(llvmberry::Source, *X),
            llvmberry::TyPosition::make(llvmberry::Source, *Z))));

    // Propagate "Y = select c ? x : 0" or "Y = select c ? 0 : x"
    hints.addCommand(llvmberry::ConsPropagate::make(
        llvmberry::ConsLessdef::make(
            llvmberry::ConsVar::make(reg_y_name, llvmberry::Physical),
            llvmberry::ConsRhs::make(reg_y_name, llvmberry::Physical, llvmberry::Source),
            llvmberry::Source),
        llvmberry::ConsBounds::make(
            llvmberry::TyPosition::make(llvmberry::Source, *Y),
            llvmberry::TyPosition::make(llvmberry::Source, *Z))));

    if(needs_commutativity){
      hints.addCommand(llvmberry::ConsInfrule::make(
        llvmberry::TyPosition::make(llvmberry::Source, *Z),
        llvmberry::ConsAddCommutative::make(
            llvmberry::TyRegister::make(reg_z_name, llvmberry::Physical),
            llvmberry::TyValue::make(*Y),
            llvmberry::TyValue::make(*a_Z),
            llvmberry::ConsSize::make(bitwidth))));
    }
    
    if(is_leftform){
      hints.addCommand(llvmberry::ConsInfrule::make(
        llvmberry::TyPosition::make(llvmberry::Source, *Z),
        llvmberry::ConsAddSelectZero::make(
            llvmberry::TyRegister::make(reg_z_name, llvmberry::Physical),
            llvmberry::TyRegister::make(reg_x_name, llvmberry::Physical),
            llvmberry::TyRegister::make(reg_y_name, llvmberry::Physical),
            llvmberry::TyValue::make(*c),
            llvmberry::TyValue::make(*n),
            llvmberry::TyValue::make(*a),
            llvmberry::ConsSize::make(bitwidth))));
    }else{
      hints.addCommand(llvmberry::ConsInfrule::make(
        llvmberry::TyPosition::make(llvmberry::Source, *Z),
        llvmberry::ConsAddSelectZero2::make(
            llvmberry::TyRegister::make(reg_z_name, llvmberry::Physical),
            llvmberry::TyRegister::make(reg_x_name, llvmberry::Physical),
            llvmberry::TyRegister::make(reg_y_name, llvmberry::Physical),
            llvmberry::TyValue::make(*c),
            llvmberry::TyValue::make(*n),
            llvmberry::TyValue::make(*a),
            llvmberry::ConsSize::make(bitwidth))));
    }
  });
}

void generateHintForOrXor(llvm::BinaryOperator &I, llvm::Value *op0, llvm::Value *op1, bool needsCommutativity){
  assert(llvmberry::ValidationUnit::Exists());
  
  llvmberry::ValidationUnit::GetInstance()->intrude([&I, &op0, &op1, &needsCommutativity](
      llvmberry::ValidationUnit::Dictionary &data,
      llvmberry::CoreHint &hints) {
    //    <src>    |   <tgt>
    // X = B ^ -1  | X = B ^ -1
    // Y = A & X   | Y = A & X
    // Z = A ^ B   | Z = A ^ B
    // W = Y | Z   | W = A ^ B
    llvm::BinaryOperator *W = &I;
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

    llvmberry::propagateInstruction(X, W, llvmberry::Source);
    llvmberry::propagateInstruction(Y, W, llvmberry::Source);
    llvmberry::propagateInstruction(Z, W, llvmberry::Source);
    if(X->getOperand(1) == B){
      llvmberry::applyCommutativity(W, X, llvmberry::Source);
    }

    if(needsCommutativity){
      llvmberry::applyCommutativity(W, W, llvmberry::Source);
    }
   
    hints.addCommand(llvmberry::ConsInfrule::make(
        llvmberry::TyPosition::make(llvmberry::Source, *W),
        llvmberry::ConsOrXor::make(
            llvmberry::TyValue::make(*W), 
            llvmberry::TyValue::make(*Z), 
            llvmberry::TyValue::make(*X), 
            llvmberry::TyValue::make(*Y), 
            llvmberry::TyValue::make(*A), 
            llvmberry::TyValue::make(*B), 
            llvmberry::ConsSize::make(bitwidth))));

  });
}

void generateHintForOrXor2(llvm::BinaryOperator &I, 
        llvm::Value *X1_val, llvm::Value *X2_val,
        llvm::Value *A, llvm::Value *B,
        bool needsY1Commutativity, bool needsY2Commutativity){
  llvmberry::ValidationUnit::GetInstance()->intrude([&I, &X1_val, &X2_val, &A, &B,
      &needsY1Commutativity,
      &needsY2Commutativity](llvmberry::ValidationUnit::Dictionary &data,
      llvmberry::CoreHint &hints) {
    //     <src>           <tgt>
    // X1 = B  ^ -1  | X1 =  B ^ -1
    // Y1 = A  & X1  | Y1 =  A & X1
    // X2 = A  ^ -1  | X2 =  A ^ -1
    // Y2 = X2 & B   | Y2 = X2 & B
    // Z =  Y1 | Y2  | Z =   A ^ B
    llvm::BinaryOperator *Z = &I;
    llvm::BinaryOperator *Y1 = llvm::dyn_cast<llvm::BinaryOperator>(Z->getOperand(0));
    llvm::BinaryOperator *Y2 = llvm::dyn_cast<llvm::BinaryOperator>(Z->getOperand(1));
    llvm::BinaryOperator *X1 = llvm::dyn_cast<llvm::BinaryOperator>(X1_val);
    llvm::BinaryOperator *X2 = llvm::dyn_cast<llvm::BinaryOperator>(X2_val);
    assert(Y1);
    assert(Y2);
    assert(X1);
    assert(X2);
    int bitwidth = Z->getType()->getIntegerBitWidth();
  
    llvmberry::propagateInstruction(X1, Z, llvmberry::Source);
    llvmberry::propagateInstruction(X2, Z, llvmberry::Source);
    llvmberry::propagateInstruction(Y1, Z, llvmberry::Source);
    llvmberry::propagateInstruction(Y2, Z, llvmberry::Source);
    if(X1->getOperand(1) == B)
      llvmberry::applyCommutativity(Z, X1, llvmberry::Source);
    if(X2->getOperand(1) == A)
      llvmberry::applyCommutativity(Z, X2, llvmberry::Source);
    if(needsY1Commutativity)
      llvmberry::applyCommutativity(Z, Y1, llvmberry::Source);
    if(needsY2Commutativity)
      llvmberry::applyCommutativity(Z, Y2, llvmberry::Source);
   
    hints.addCommand(llvmberry::ConsInfrule::make(
      llvmberry::TyPosition::make(llvmberry::Target, *Z),
      llvmberry::ConsOrXor2::make(
          llvmberry::TyValue::make(*Z), 
          llvmberry::TyValue::make(*X1), 
          llvmberry::TyValue::make(*Y1), 
          llvmberry::TyValue::make(*X2), 
          llvmberry::TyValue::make(*Y2), 
          llvmberry::TyValue::make(*A), 
          llvmberry::TyValue::make(*B), 
          llvmberry::ConsSize::make(bitwidth))));
   
  });
}

void generateHintForAddXorAnd(llvm::BinaryOperator &I, 
        llvm::BinaryOperator *X,
        llvm::BinaryOperator *Y,
        llvm::Value *A, llvm::Value *B,
        bool needsYCommutativity, bool needsZCommutativity){
  llvmberry::ValidationUnit::GetInstance()->intrude([&I, &X, &Y,
      &needsYCommutativity,
      &needsZCommutativity]
      (llvmberry::ValidationUnit::Dictionary &data,
      llvmberry::CoreHint &hints) {
    //    <src>       <tgt>
    // X = A ^ B  | X = A ^ B
    // Y = A & B  | Y = A & B
    // Z = X + Y  | Z = A | B
    llvm::BinaryOperator *Z = &I;
    llvm::Value *A = X->getOperand(0);
    llvm::Value *B = X->getOperand(1);
    int bitwidth = Z->getType()->getIntegerBitWidth();
  
    llvmberry::propagateInstruction(X, Z, llvmberry::Source);
    llvmberry::propagateInstruction(Y, Z, llvmberry::Source);
   
    if(needsYCommutativity)
      llvmberry::applyCommutativity(&I, Y, llvmberry::Source);
    if(needsZCommutativity)
      llvmberry::applyCommutativity(&I, &I, llvmberry::Source);

    hints.addCommand(llvmberry::ConsInfrule::make(
      llvmberry::TyPosition::make(llvmberry::Source, *Z),
      llvmberry::ConsAddXorAnd::make(
          llvmberry::TyRegister::make(llvmberry::getVariable(*Z), llvmberry::Physical),
          llvmberry::TyValue::make(*A), 
          llvmberry::TyValue::make(*B), 
          llvmberry::TyRegister::make(llvmberry::getVariable(*X), llvmberry::Physical),
          llvmberry::TyRegister::make(llvmberry::getVariable(*Y), llvmberry::Physical),
          llvmberry::ConsSize::make(bitwidth))));
  });
}

void generateHintForAddOrAnd(llvm::BinaryOperator &I, 
        llvm::BinaryOperator *X,
        llvm::BinaryOperator *Y,
        llvm::Value *A, llvm::Value *B,
        bool needsYCommutativity, bool needsZCommutativity){
  assert(llvmberry::ValidationUnit::Exists());
  
  llvmberry::ValidationUnit::GetInstance()->intrude([&I, &X, &Y,
      &needsYCommutativity,
      &needsZCommutativity]
      (llvmberry::ValidationUnit::Dictionary &data,
      llvmberry::CoreHint &hints) {
    //    <src>       <tgt>
    // X = A ^ B  | X = A ^ B
    // Y = A & B  | Y = A & B
    // Z = X + Y  | Z = A | B
    llvm::BinaryOperator *Z = &I;
    llvm::Value *A = X->getOperand(0);
    llvm::Value *B = X->getOperand(1);
    int bitwidth = Z->getType()->getIntegerBitWidth();
  
    llvmberry::propagateInstruction(X, Z, llvmberry::Source);
    llvmberry::propagateInstruction(Y, Z, llvmberry::Source);
   
    if(needsYCommutativity)
      llvmberry::applyCommutativity(&I, Y, llvmberry::Source);
    if(needsZCommutativity)
      llvmberry::applyCommutativity(&I, &I, llvmberry::Source);

    hints.addCommand(llvmberry::ConsInfrule::make(
      llvmberry::TyPosition::make(llvmberry::Source, *Z),
      llvmberry::ConsAddOrAnd::make(
          llvmberry::TyRegister::make(llvmberry::getVariable(*Z), llvmberry::Physical),
          llvmberry::TyValue::make(*A), 
          llvmberry::TyValue::make(*B), 
          llvmberry::TyRegister::make(llvmberry::getVariable(*X), llvmberry::Physical),
          llvmberry::TyRegister::make(llvmberry::getVariable(*Y), llvmberry::Physical),
          llvmberry::ConsSize::make(bitwidth))));
  });
}

void generateHintForAndOr(llvm::BinaryOperator &I,
          llvm::Value *X,
          llvm::BinaryOperator *Y,
          llvm::Value *A,
          bool needsZCommutativity){
  assert(ValidationUnit::Exists());

  ValidationUnit::GetInstance()->intrude([&I, &X, &Y, &A,
      &needsZCommutativity]
      (ValidationUnit::Dictionary &data, CoreHint &hints) {
    llvm::BinaryOperator *Z = llvm::dyn_cast<llvm::BinaryOperator>(&I);
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

}
