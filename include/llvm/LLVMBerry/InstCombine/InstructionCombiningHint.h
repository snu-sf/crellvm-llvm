#ifndef INSTCOMBINE_INSTRUCTIONCOMBINING_H
#define INSTCOMBINE_INSTRUCTIONCOMBINING_H

#include <string>
#include "llvm/IR/Instructions.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Value.h"
#include "llvm/LLVMBerry/ValidationUnit.h"
#include <memory>

namespace llvmberry {

using namespace llvm;

// class AssociativityHintBuilder

class AssociativityHintBuilder {
public:
  static AssociativityHintBuilder *
  GetBuilderInstance(Value *B, Value *C, Instruction::BinaryOps Opcode);

public:
  virtual void buildCoreHint(ValidationUnit *validation_unit,
                             const BinaryOperator *Op0,
                             const Instruction &I,
                             ConstantInt *B,
                             ConstantInt *C,
                             ConstantInt *V) = 0;
  virtual std::string getOptimizationName() const = 0;

protected:
  AssociativityHintBuilder();
};

// class AssociativityAddHintBuilder

class AssociativityAddHintBuilder : public AssociativityHintBuilder {
private:
  static AssociativityAddHintBuilder *_Instance;
  static AssociativityAddHintBuilder *GetInstance();
  AssociativityAddHintBuilder();

public:
  virtual void buildCoreHint(ValidationUnit *validation_unit,
                             const BinaryOperator *Op0, const Instruction &I,
                             ConstantInt *B,
                             ConstantInt *C,
                             ConstantInt *V);
  virtual std::string getOptimizationName() const;

  friend class AssociativityHintBuilder;
};

/* // class DeadCodeElim */

/* class DeadCodeElimHintBuilder { */
/* public: */
/*   DeadCodeElimHintBuilder(Instruction *instr); */
/*   void buildCoreHint(ValidationUnit *validation_unit); */
/*   std::string getOptimizationName() const; */
/*   bool isValid() const; */

/* private: */
/*   bool isvalid; */
/*   Position instr_position; */
/*   Variable var; */
/* }; */

/* // class Distributivity */

/* class DistributivityHintBuilder { */
/* public: */
/*   static DistributivityHintBuilder * */
/*   GetBuilderInstance(Value *B, Value *D, Instruction::BinaryOps InnerOpcode, */
/*                      Instruction::BinaryOps TopLevelOpcode); */

/* public: */
/*   virtual void buildCoreHint(ValidationUnit *validation_unit, */
/*                              const Instruction &z, const Instruction *x, */
/*                              const Instruction *y, const Instruction *w) = 0; */
/*   virtual std::string getOptimizationName() const = 0; */

/* protected: */
/*   DistributivityHintBuilder(); */
/* }; */

/* // class DistributivityMullAdd */

/* class DistributivityMulAddHintBuilder : public DistributivityHintBuilder { */
/* private: */
/*   static DistributivityMulAddHintBuilder *_Instance; */
/*   static DistributivityMulAddHintBuilder *GetInstance(); */
/*   DistributivityMulAddHintBuilder(); */

/* public: */
/*   virtual void buildCoreHint(ValidationUnit *validation_unit, */
/*                              const Instruction &z, const Instruction *x, */
/*                              const Instruction *y, const Instruction *w); */
/*   virtual std::string getOptimizationName() const; */

/*   friend class DistributivityHintBuilder; */
/* }; */

} // llvmberry

#endif
