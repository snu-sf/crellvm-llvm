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
  validation_unit->intrude([&Op0, &I, &B, &C, &V](
      llvmberry::ValidationUnit::Dictionary &data, llvmberry::CoreHint &hints) {
    // Op0: A op B
    // I: Op0 op C
    // V: B op C

    // prepare variables
    // auto position_Op0 = llvmberry::Position::FromInstr(*Op0);
    // auto position_I = llvmberry::Position::FromInstr(I);
                             std::string reg0_name = llvmberry::getVariable(*(Op0->getOperand(0)));
    std::string reg1_name = llvmberry::getVariable(*Op0);
    std::string reg2_name = llvmberry::getVariable(I);

    unsigned b_bw = B->getBitWidth();
    unsigned c_bw = C->getBitWidth();
    unsigned v_bw = V->getBitWidth();

    int b = (int)B->getSExtValue();
    int c = (int)C->getSExtValue();
    int v = (int)V->getSExtValue();
    
    // llvmberry::Variable var1(llvmberry::getVariable(*Op0), llvmberry::New);
    // llvmberry::Variable var2(llvmberry::getVariable(I), llvmberry::New);

    auto ld_lhs = llvmberry::ConsVar::make(reg1_name, llvmberry::Physical);
    auto ld_rhs = llvmberry::ConsRhs::make(reg1_name, llvmberry::Physical);

    // prepare PropagateObject
    // auto ld = llvmberry::TyPropagateLessdef::make(ld_lhs, ld_rhs, llvmberry::Source);

    auto obj = llvmberry::ConsLessdef::make(ld_lhs, ld_rhs, llvmberry::Source);
    
    // std::unique_ptr<llvmberry::TyPropagateLessdef> ld
    //   (new TyPropagateLessdef(std::move(ld_lhs),
    //                           std::move(ld_rhs),
    //                           llvmberry::Source
    //                           ));
    
    // std::unique_ptr<llvmberry::TyPropagateObject> obj
    //   (new ConsLessdef(std::move(ld)));

    // prepare PropagateRange
    // std::unique_ptr<llvmberry::TyPosition> pos_from
    //   (new ConsCommand(llvmberry::Source, reg1_name));
    // std::unique_ptr<llvmberry::TyPosition> pos_to
    //   (new ConsCommand(llvmberry::Source, reg2_name));

    auto pos_from = llvmberry::ConsCommand::make(llvmberry::Source, reg1_name);
    auto pos_to = llvmberry::ConsCommand::make(llvmberry::Source, reg2_name);

    auto range = llvmberry::ConsBounds::make(pos_from, pos_to);
    
    std::unique_ptr<llvmberry::TyPropagateRange> range
      (new ConsBounds(std::move(pos_from), std::move(pos_to)));

    // prepare propagate
    std::unique_ptr<llvmberry::TyPropagate> prop
      (new TyPropagate(std::move(obj), std::move(range)));

    std::unique_ptr<llvmberry::TyCommand> prop_cmd
      (new ConsPropagate(std::move(prop)));

    // add commands
    hints.addCommand(std::move(prop_cmd));
    // std::unique_ptr<llvmberry::Command>(new llvmberry::Propagate(
        //     std::unique_ptr<llvmberry::PropagateObject>(
        //         new llvmberry::PropagateInstr(var1, position_Op0)),
        //     position_Op0, position_I, llvmberry::Source)));


    // for addassoc

    // 
    std::unique_ptr<llvmberry::TyConstInt> c1
      (new TyConstInt(b, true, b_bw));
    std::unique_ptr<llvmberry::TyConstInt> c2
      (new TyConstInt(c, true, c_bw));
    std::unique_ptr<llvmberry::TyConstInt> c3
      (new TyConstInt(v, true, v_bw));

    std::unique_ptr<llvmberry::TyAddAssociative> add_assoc
      (new TyAddAssociative
       (std::unique_ptr<llvmberry::TyRegister>(new TyRegister(reg0_name, llvmberry::Physical)),
        std::unique_ptr<llvmberry::TyRegister>(new TyRegister(reg1_name, llvmberry::Physical)),
        std::unique_ptr<llvmberry::TyRegister>(new TyRegister(reg2_name, llvmberry::Physical)),
        std::move(c1), std::move(c2), std::move(c3),
        std::unique_ptr<llvmberry::TySize>(new ConsSize(b_bw))
        ));

    std::unique_ptr<llvmberry::TyPosition> add_assoc_pos
      (new llvmberry::ConsCommand(llvmberry::Source,
                                  reg2_name));

    std::unique_ptr<llvmberry::TyInfrule> add_assoc_cons
      (new llvmberry::ConsAddAssociative(std::move(add_assoc)));

    std::unique_ptr<llvmberry::TyCommand> add_assoc_cmd
      (new llvmberry::ConsInfrule
       (std::move(add_assoc_pos), std::move(add_assoc_cons)));
    
    hints.addCommand(std::move(add_assoc_cmd));
  });
}

} // llvmberry
