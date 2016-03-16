#include "llvm/LLVMBerry/InstCombine/InstructionCombiningHint.h"

namespace llvmberry {

using namespace llvm;

// class AssociativityHintBuilder

AssociativityHintBuilder *
AssociativityHintBuilder::GetBuilderInstance(Value *B, Value *C,
                                             Instruction::BinaryOps Opcode) {
  if (isa<Constant>(B) && isa<Constant>(C)) {
    switch (Opcode) {
    case Instruction::Add:
      return AssociativityAddHintBuilder::GetInstance();
    default:
      return nullptr;
    }
  }

  return nullptr;
}

AssociativityHintBuilder::AssociativityHintBuilder() {}

// class AssociativityAddHintBuilder

AssociativityAddHintBuilder *AssociativityAddHintBuilder::_Instance = nullptr;

AssociativityAddHintBuilder *AssociativityAddHintBuilder::GetInstance() {
  if (!_Instance)
    _Instance = new AssociativityAddHintBuilder();
  return _Instance;
}

AssociativityAddHintBuilder::AssociativityAddHintBuilder() {}

std::string AssociativityAddHintBuilder::getOptimizationName() const {
  return "associativity_add";
}

void AssociativityAddHintBuilder::buildCoreHint
(ValidationUnit *validation_unit,
 const BinaryOperator *Op0,
 const Instruction &I,
 ConstantInt *B,
 ConstantInt *C,
 ConstantInt *V) {
  validation_unit->intrude
    ([&Op0, &I, &B, &C, &V]
     (llvmberry::ValidationUnit::Dictionary &data, llvmberry::CoreHint &hints) {
    // Op0: A op B
    // I: Op0 op C
    // V: B op C

    // prepare variables
      std::string reg0_name = llvmberry::getVariable(*(Op0->getOperand(0)));
      std::string reg1_name = llvmberry::getVariable(*Op0);
      std::string reg2_name = llvmberry::getVariable(I);

      unsigned b_bw = B->getBitWidth();
      unsigned c_bw = C->getBitWidth();
      unsigned v_bw = V->getBitWidth();

      int b = (int)B->getSExtValue();
      int c = (int)C->getSExtValue();
      int v = (int)V->getSExtValue();

      hints.addCommand
        (llvmberry::ConsPropagate::make
         (llvmberry::ConsLessdef::make
          (llvmberry::ConsVar::make
           (reg1_name, llvmberry::Physical),
           llvmberry::ConsRhs::make
           (reg1_name, llvmberry::Physical),
           llvmberry::Source)
          ,
          llvmberry::ConsBounds::make
          (llvmberry::ConsCommand::make
           (llvmberry::Source, reg1_name),
           llvmberry::ConsCommand::make
           (llvmberry::Source, reg2_name))
          )
         );

      hints.addCommand
        (llvmberry::ConsInfrule::make
         (llvmberry::ConsCommand::make
          (llvmberry::Source, reg2_name),
          llvmberry::ConsAddAssociative::make
          (llvmberry::TyRegister::make(reg0_name, llvmberry::Physical),
           llvmberry::TyRegister::make(reg1_name, llvmberry::Physical),
           llvmberry::TyRegister::make(reg2_name, llvmberry::Physical),
           llvmberry::TyConstInt::make(b, b_bw),
           llvmberry::TyConstInt::make(c, c_bw),
           llvmberry::TyConstInt::make(v, v_bw),
           llvmberry::ConsSize::make(b_bw)
           )

          )
         );
    });
}

// class DeadCodeElimHintBuilder

DeadCodeElimHintBuilder::DeadCodeElimHintBuilder(Instruction *I) 
: reg(llvmberry::getVariable(*I)) {
  if(I == I->getParent()->getFirstNonPHI()) {
    nop_prev_isPHI = true;
    nop_block_name = getBasicBlockIndex(I->getParent());
  } else {
    BasicBlock::iterator prevI = I;
    prevI--;
    nop_prev_isPHI = false;
    nop_prev_reg = llvmberry::getVariable(*prevI);
  }
}

std::string DeadCodeElimHintBuilder::getOptimizationName() const {
  return "dead_code_elim";
}

void DeadCodeElimHintBuilder::buildCoreHint(ValidationUnit *validation_unit) {
  validation_unit->intrude([this]
     (ValidationUnit::Dictionary &data, CoreHint &hints) {
        hints.addCommand(
          ConsPropagate::make(
            ConsMaydiff::make(TyRegister::make(reg, Physical)),
            ConsGlobal::make()
          )
        );

        if(nop_prev_isPHI) {
          hints.addTgtNopPosition(ConsNopPosition::make(nop_block_name, true));
        } else {
          hints.addTgtNopPosition(ConsNopPosition::make(nop_prev_reg, false));
        }
     }
  );
}

} // llvmberry
