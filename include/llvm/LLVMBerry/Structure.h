#ifndef STRUCTURE_H
#define STRUCTURE_H

#include <iostream>
#include <set>
#include "llvm/IR/Instructions.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "cereal/archives/json.hpp"
#include "cereal/types/vector.hpp"
#include <cereal/types/memory.hpp>


namespace cereal {
[[noreturn]] void throw_exception(std::exception const &e);

template <class T>
void save(cereal::JSONOutputArchive &archive, std::unique_ptr<T> const &ptr) {
  ptr->serialize(archive);
}
} // cereal

namespace llvmberry {

enum TyScope { Source = 0, Target };

std::string getBasicBlockIndex(const llvm::BasicBlock *block);
std::string getVariable(const llvm::Value &value);
bool name_instructions(llvm::Function &F);
void generateHintForNegValue(llvm::Value *V, llvm::BinaryOperator &I, TyScope scope = Source);
void generateHintForAddSelectZero(llvm::BinaryOperator *Z, 
        llvm::BinaryOperator *X, 
        llvm::SelectInst *Y, 
        bool needs_commutativity,
        bool is_leftform);
/* position */

struct TyPositionPhinode {
public:
  TyPositionPhinode(std::string _prev_block_name);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::string prev_block_name;
};

struct TyPositionCommand {
public:
  TyPositionCommand(int _index, std::string _register_name);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  int index;
  std::string register_name;
};

struct TyInstrIndex{
public:
  virtual void serialize(cereal::JSONOutputArchive &archive) const = 0;
};

struct ConsPhinode : public TyInstrIndex {
public:
  ConsPhinode(std::unique_ptr<TyPositionPhinode> _position_phinode);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::unique_ptr<TyPositionPhinode> position_phinode;
};

struct ConsCommand : public TyInstrIndex {
public:
  ConsCommand(std::unique_ptr<TyPositionCommand> _position_command);
  void serialize(cereal::JSONOutputArchive &archive) const;

  /* static std::unique_ptr<TyPosition> make(const llvm::Instruction &I, */
  /*                                         enum TyScope _scope); */

private:
  std::unique_ptr<TyPositionCommand> position_command;
};

struct TyPosition {
public:
  TyPosition(enum TyScope _scope, std::string _block_name,
             std::unique_ptr<TyInstrIndex> _instr_index);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::unique_ptr<TyPosition> make(enum TyScope _scope,
                                          const llvm::Instruction &I);
  static std::unique_ptr<TyPosition>
  make_end_of_block(enum TyScope _scope, const llvm::BasicBlock &BB);
  static std::unique_ptr<TyPosition> make_start_of_block(enum TyScope _scope,
                                          std::string _block_name);
  static std::unique_ptr<TyPosition> make(enum TyScope _scope,
                                          std::string _block_name,
                                          std::string _prev_block_name);
  static std::unique_ptr<TyPosition> make(enum TyScope _scope,
                                          const llvm::Instruction &I,
                                          std::string _prev_block_name);

private:
  enum TyScope scope;
  std::string block_name;
  std::unique_ptr<TyInstrIndex> instr_index;
};

/* value */

// register

enum TyTag { Physical = 0, Previous, Ghost };

struct TyRegister {
public:
  TyRegister(std::string _name, enum TyTag _tag);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::unique_ptr<TyRegister> make(std::string _name, enum TyTag _tag);

private:
  std::string name;
  enum TyTag tag;
};


// type

struct TyIntType {
public:
  virtual void serialize(cereal::JSONOutputArchive &archive) const = 0;
};

struct ConsIntType : public TyIntType {
public:
  ConsIntType(int _value);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  int value;
};

enum TyFloatType {
  HalfType = 0,
  FloatType,
  DoubleType,
  FP128Type,
  PPC_FP128Type,
  X86_FP80Type
};

struct TyValueType{
public : 
  virtual void serialize(cereal::JSONOutputArchive& archive) const = 0;
  static std::unique_ptr<TyValueType> make(const llvm::Type &type);
};

struct ConsIntValueType : public TyValueType{
public : 
  ConsIntValueType(std::unique_ptr<TyIntType> _int_type);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyIntType> int_type;
};

struct ConsFloatValueType : public TyValueType{
public : 
  ConsFloatValueType(TyFloatType _float_type);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  TyFloatType float_type;
};

struct ConsNamedType : public TyValueType{
public : 
  ConsNamedType(std::string _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::string s;
};

struct ConsPtrType : public TyValueType{
public : 
  ConsPtrType(int _address_space, std::unique_ptr<TyValueType> _valuetype);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  int address_space;
  std::unique_ptr<TyValueType> valuetype;
};


// constants

struct TyConstInt {
public:
  TyConstInt(int64_t _int_value, std::unique_ptr<TyIntType> _int_type);
  TyConstInt(int64_t _int_value, int _bitwidth);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::unique_ptr<TyConstInt> make(int64_t _int_value, int _bitwidth);

private:
  int64_t int_value;
  std::unique_ptr<TyIntType> int_type;
};

struct TyConstFloat {
public:
  TyConstFloat(double _float_value, enum TyFloatType _float_type);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::unique_ptr<TyConstFloat> make(double _float_value,
                                            enum TyFloatType _float_type);

private:
  double float_value;
  enum TyFloatType float_type;
};

struct TyConstant {
public:
  virtual void serialize(cereal::JSONOutputArchive &archive) const = 0;
};

struct ConsConstInt : public TyConstant {
public:
  ConsConstInt(std::unique_ptr<TyConstInt> _const_int);
  ConsConstInt(int64_t _int_value, int _bitwidth);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::unique_ptr<TyConstInt> const_int;
};

struct ConsConstFloat : public TyConstant {
public:
  ConsConstFloat(std::unique_ptr<TyConstFloat> _const_float);
  ConsConstFloat(float _float_value, enum TyFloatType _float_type);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::unique_ptr<TyConstFloat> const_float;
};

// value

struct TyValue {
public:
  virtual void serialize(cereal::JSONOutputArchive &archive) const = 0;

  static std::unique_ptr<TyValue> make(const llvm::Value &value);
};

struct ConsId : public TyValue {
public:
  ConsId(std::unique_ptr<TyRegister> _register);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::unique_ptr<TyValue> make(std::string _name,
                                       enum TyTag _tag);

private:
  std::unique_ptr<TyRegister> reg;
};

struct ConsConstVal : public TyValue {
public:
  ConsConstVal(std::unique_ptr<TyConstant> _constant);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::unique_ptr<TyConstant> constant;
};


// instruction

struct TyInstruction {
public:
  virtual void serialize(cereal::JSONOutputArchive &archive) const = 0;
  static std::unique_ptr<TyInstruction> make(const llvm::Instruction &inst);
};

enum TyBop { BopAdd, BopSub, BopMul, BopUdiv, BopSdiv, BopUrem, BopSrem, BopShl, BopLshr, BopAshr,
        BopAnd, BopOr, BopXor, BopFadd, BopFsub, BopFmul, BopFdiv, BopFrem };

struct TyBinaryOperator{
public : 
  TyBinaryOperator(TyBop _opcode, std::unique_ptr<TyValueType> _operandtype, std::unique_ptr<TyValue> _operand1, std::unique_ptr<TyValue> _operand2);
  void serialize(cereal::JSONOutputArchive& archive) const;
  static std::unique_ptr<TyBinaryOperator> make(const llvm::BinaryOperator &bop);

private : 
  TyBop opcode;
  std::unique_ptr<TyValueType> operandtype;
  std::unique_ptr<TyValue> operand1;
  std::unique_ptr<TyValue> operand2;
};

struct TyLoadInst{
public : 
  TyLoadInst(std::unique_ptr<TyValueType> _pointertype, std::unique_ptr<TyValueType> _valtype, std::unique_ptr<TyValue> _ptrvalue, int _align);
  void serialize(cereal::JSONOutputArchive& archive) const;
  static std::unique_ptr<TyLoadInst> make(const llvm::LoadInst &li);

private : 
  std::unique_ptr<TyValueType> pointertype;
  std::unique_ptr<TyValueType> valtype;
  std::unique_ptr<TyValue> ptrvalue;
  int align;
};

struct ConsBinaryOp : public TyInstruction{
public : 
  ConsBinaryOp(std::unique_ptr<TyBinaryOperator> _binary_operator);
  static std::unique_ptr<TyInstruction> make(TyBop _opcode, std::unique_ptr<TyValueType> _operandtype, std::unique_ptr<TyValue> _operand1, std::unique_ptr<TyValue> _operand2);
  static std::unique_ptr<TyInstruction> make(const llvm::BinaryOperator &bop);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyBinaryOperator> binary_operator;
};

struct ConsLoadInst : public TyInstruction{
public : 
  ConsLoadInst(std::unique_ptr<TyLoadInst> _load_inst);
  static std::unique_ptr<TyInstruction> make(std::unique_ptr<TyValueType> _pointertype, std::unique_ptr<TyValueType> _valtype, std::unique_ptr<TyValue> _ptrvalue, int _align);
  static std::unique_ptr<TyInstruction> make(const llvm::LoadInst &li);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyLoadInst> load_inst;
};


// size

struct TySize {
public:
  virtual void serialize(cereal::JSONOutputArchive &archive) const = 0;
};

struct ConsSize : public TySize {
public:
  ConsSize(int _size);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::unique_ptr<TySize> make(int _size);

private:
  int size;
};

/* propagate */

// propagate expression

struct TyExpr {
public:
  virtual void serialize(cereal::JSONOutputArchive &archive) const = 0;
};

struct ConsVar : public TyExpr {
public:
  ConsVar(std::unique_ptr<TyRegister> _register_name);
  ConsVar(std::string _name, enum TyTag _tag);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::unique_ptr<TyExpr> make(std::string _name,
                                      enum TyTag _tag);

private:
  std::unique_ptr<TyRegister> register_name;
};

struct ConsRhs : public TyExpr {
public:
  ConsRhs(std::unique_ptr<TyRegister> _register_name, enum TyScope _scope);
  ConsRhs(std::string _name, enum TyTag _tag, enum TyScope _scope);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::unique_ptr<TyExpr> make(std::string _name,
                                      enum TyTag _tag,
                                      enum TyScope _scope);

private:
  std::unique_ptr<TyRegister> register_name;
  enum TyScope scope;
};

struct ConsConst : public TyExpr {
public:
  ConsConst(std::unique_ptr<TyConstant> _constant);

  ConsConst(int _int_value, int _value);
  ConsConst(float _float_value, enum TyFloatType _float_type);

  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::unique_ptr<TyConstant> constant;
};

struct ConsInsn : public TyExpr{
public : 
  ConsInsn(std::unique_ptr<TyInstruction> _instruction);
  void serialize(cereal::JSONOutputArchive& archive) const;
  
  static std::unique_ptr<TyExpr> make(const llvm::Instruction &i);

private : 
  std::unique_ptr<TyInstruction> instruction;
};

// propagate object

struct TyPropagateLessdef {
public:
  TyPropagateLessdef(std::unique_ptr<TyExpr> _lhs,
                     std::unique_ptr<TyExpr> _rhs,
                     enum TyScope _scope);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::unique_ptr<TyPropagateLessdef>
  make(std::unique_ptr<TyExpr> _lhs,
       std::unique_ptr<TyExpr> _rhs, enum TyScope _scope);

private:
  std::unique_ptr<TyExpr> lhs;
  std::unique_ptr<TyExpr> rhs;
  enum TyScope scope;
};

struct TyPropagateNoalias {
public:
  TyPropagateNoalias(std::unique_ptr<TyRegister> _lhs,
                     std::unique_ptr<TyRegister> _rhs, enum TyScope _scope);
  TyPropagateNoalias(std::string _lhs_name, enum TyTag _lhs_tag,
                     std::string _rhs_name, enum TyTag _rhs_tag,
                     enum TyScope _scope);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::unique_ptr<TyRegister> lhs;
  std::unique_ptr<TyRegister> rhs;
  enum TyScope scope;
};

struct TyPropagateObject {
public:
  virtual void serialize(cereal::JSONOutputArchive &archive) const = 0;
};

struct ConsLessdef : public TyPropagateObject {
public:
  ConsLessdef(std::unique_ptr<TyPropagateLessdef> _propagate_lessdef);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::unique_ptr<TyPropagateObject>
  make(std::unique_ptr<TyExpr> _lhs,
       std::unique_ptr<TyExpr> _rhs, enum TyScope _scope);

private:
  std::unique_ptr<TyPropagateLessdef> propagate_lessdef;
};

struct ConsNoalias : public TyPropagateObject {
public:
  ConsNoalias(std::unique_ptr<TyPropagateNoalias> _propagate_noalias);
  ConsNoalias(std::string _lhs_name, enum TyTag _lhs_tag, std::string _rhs_name,
              enum TyTag _rhs_tag, enum TyScope _scope);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::unique_ptr<TyPropagateNoalias> propagate_noalias;
};

struct ConsMaydiff : public TyPropagateObject {
public:
  ConsMaydiff(std::unique_ptr<TyRegister> _register_name);
  ConsMaydiff(std::string _name, enum TyTag _tag);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::unique_ptr<TyPropagateObject> make(std::string _name,
                                                 enum TyTag _tag);

private:
  std::unique_ptr<TyRegister> register_name;
};

// propagate range

struct TyPropagateRange {
public:
  virtual void serialize(cereal::JSONOutputArchive &archive) const = 0;
  virtual bool isGlobal(void) const { return false; }
};

struct ConsBounds : public TyPropagateRange {
public:
  ConsBounds(std::unique_ptr<TyPosition> _from,
             std::unique_ptr<TyPosition> _to);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::unique_ptr<TyPropagateRange>
  make(std::unique_ptr<TyPosition> _from, std::unique_ptr<TyPosition> _to);

private:
  std::unique_ptr<TyPosition> from;
  std::unique_ptr<TyPosition> to;
};

struct ConsGlobal : public TyPropagateRange {
public:
  ConsGlobal();
  void serialize(cereal::JSONOutputArchive &archive) const;
  virtual bool isGlobal(void) const { return true; }

  static std::unique_ptr<TyPropagateRange> make();
};

struct TyPropagate {
public:
  TyPropagate(std::unique_ptr<TyPropagateObject> _propagate,
              std::unique_ptr<TyPropagateRange> _propagate_range);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::unique_ptr<TyPropagateObject> propagate;
  std::unique_ptr<TyPropagateRange> propagate_range;
};

/* hint command */

struct TyCommand {
public:
  virtual void serialize(cereal::JSONOutputArchive &archive) const = 0;
};
 
/* hint command */

struct TyInfrule {
public:
  virtual void serialize(cereal::JSONOutputArchive &archive) const = 0;
};

struct ConsPropagate : public TyCommand {
public:
  ConsPropagate(std::unique_ptr<TyPropagate> _propagate);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::unique_ptr<TyCommand>
  make(std::unique_ptr<TyPropagate> _propagate);
  static std::unique_ptr<TyCommand>
  make(std::unique_ptr<TyPropagateObject> _obj,
       std::unique_ptr<TyPropagateRange> _range);

private:
  std::unique_ptr<TyPropagate> propagate;
};

struct ConsInfrule : public TyCommand {
public:
  ConsInfrule(std::unique_ptr<TyPosition> _position,
              std::unique_ptr<TyInfrule> _infrule);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::unique_ptr<TyCommand> make(std::unique_ptr<TyPosition> _position,
                                         std::unique_ptr<TyInfrule> _infrule);

private:
  std::unique_ptr<TyPosition> position;
  std::unique_ptr<TyInfrule> infrule;
};


/* core hint */

struct CoreHint {
public:
  CoreHint();
  CoreHint(std::string _module_id, std::string _function_id,
           std::string _opt_name);
  void addCommand(std::unique_ptr<TyCommand> c);
  void addNopPosition(std::unique_ptr<TyPosition> position);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::string module_id;
  std::string function_id;
  std::string opt_name;
  std::vector<std::unique_ptr<TyPosition>> nop_positions;
  std::vector<std::unique_ptr<TyCommand>> commands;
};

// inserting nop
void insertTgtNopAtSrcI(CoreHint &hints, llvm::Instruction *I);
void insertSrcNopAtTgtI(CoreHint &hints, llvm::Instruction *I);

} // llvmberry

#endif
