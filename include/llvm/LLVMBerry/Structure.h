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
void save(cereal::JSONOutputArchive &archive, std::shared_ptr<T> const &ptr) {
  ptr->serialize(archive);
}
} // cereal

namespace llvmberry {

enum TyScope { Source = 0, Target };

enum TyFloatType {
  HalfType = 0,
  FloatType,
  DoubleType,
  FP128Type,
  PPC_FP128Type,
  X86_FP80Type
};

enum TyTag { Physical = 0, Previous, Ghost };

enum TyBop { BopAdd, BopSub, BopMul, BopUdiv, BopSdiv, BopUrem, BopSrem, BopShl, BopLshr, BopAshr,
        BopAnd, BopOr, BopXor,  };

enum TyFbop { BopFadd, BopFsub, BopFmul, BopFdiv, BopFrem };

enum  TyCond {
          CondEq,
          CondNe, CondUgt, CondUge, CondUlt,
          CondUle, CondSgt, CondSge, CondSlt,
          CondSle, FIRST_ICMP_PREDICATE = CondEq, LAST_ICMP_PREDICATE = CondSle, BAD_ICMP_PREDICATE = CondSle + 1
};
enum TyFCond {
          FCMP_FALSE = 0, FCMP_OEQ = 1, FCMP_OGT = 2, FCMP_OGE = 3,
          FCMP_OLT = 4, FCMP_OLE = 5, FCMP_ONE = 6, FCMP_ORD = 7,
          FCMP_UNO = 8, FCMP_UEQ = 9, FCMP_UGT = 10, FCMP_UGE = 11,
          FCMP_ULT = 12, FCMP_ULE = 13, FCMP_UNE = 14, FCMP_TRUE = 15,
          FIRST_FCMP_PREDICATE = FCMP_FALSE, LAST_FCMP_PREDICATE = FCMP_TRUE, BAD_FCMP_PREDICATE = FCMP_TRUE + 1
};

class CoreHint;


std::string getBasicBlockIndex(const llvm::BasicBlock *block);
std::string getVariable(const llvm::Value &value);
int getCommandIndex(const llvm::Value &value);
bool name_instructions(llvm::Function &F);

std::string toString(llvmberry::TyBop bop);
std::string toString(llvmberry::TyFbop bop);
std::string toString(llvmberry::TyFloatType bop);
std::string toString(llvmberry::TyCond icmp);

bool isFloatOpcode(llvm::Instruction::BinaryOps ops);
TyFloatType getFloatType(llvm::Type *typ);
TyFbop getFbop(llvm::Instruction::BinaryOps ops);
TyBop getBop(llvm::Instruction::BinaryOps ops);
TyCond getIPredicate(llvm::ICmpInst::Predicate prd);
TyFCond getFPredicate(llvm::FCmpInst::Predicate prd);

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
  ConsPhinode(std::shared_ptr<TyPositionPhinode> _position_phinode);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyPositionPhinode> position_phinode;
};

struct ConsCommand : public TyInstrIndex {
public:
  ConsCommand(std::shared_ptr<TyPositionCommand> _position_command);
  void serialize(cereal::JSONOutputArchive &archive) const;

  /* static std::shared_ptr<TyPosition> make(const llvm::Instruction &I, */
  /*                                         enum TyScope _scope); */

private:
  std::shared_ptr<TyPositionCommand> position_command;
};

struct TyPosition {
public:
  TyPosition(enum TyScope _scope, std::string _block_name,
             std::shared_ptr<TyInstrIndex> _instr_index);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyPosition> make(enum TyScope _scope,
                                          const llvm::Instruction &I);
  static std::shared_ptr<TyPosition>
  make_end_of_block(enum TyScope _scope, const llvm::BasicBlock &BB);
  static std::shared_ptr<TyPosition>
  make_start_of_block(enum TyScope _scope, std::string _block_name);
  static std::shared_ptr<TyPosition> make(enum TyScope _scope,
                                          std::string _block_name,
                                          std::string _prev_block_name);
  static std::shared_ptr<TyPosition> make(enum TyScope _scope,
                                          const llvm::Instruction &I,
                                          std::string _prev_block_name);

private:
  enum TyScope scope;
  std::string block_name;
  std::shared_ptr<TyInstrIndex> instr_index;
};

/* value */

// register

struct TyRegister {
public:
  TyRegister(std::string _name, enum TyTag _tag);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyRegister> make(std::string _name, enum TyTag _tag);

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

struct TyValueType{
public : 
  virtual void serialize(cereal::JSONOutputArchive& archive) const = 0;
  static std::shared_ptr<TyValueType> make(const llvm::Type &type);
};

struct ConsIntValueType : public TyValueType{
public : 
  ConsIntValueType(std::shared_ptr<TyIntType> _int_type);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyIntType> int_type;
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
  ConsPtrType(int _address_space, std::shared_ptr<TyValueType> _valuetype);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  int address_space;
  std::shared_ptr<TyValueType> valuetype;
};


// constants

struct TyConstInt {
public:
  TyConstInt(int64_t _int_value, std::shared_ptr<TyIntType> _int_type);
  TyConstInt(int64_t _int_value, int _bitwidth);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyConstInt> make(int64_t _int_value, int _bitwidth);

private:
  int64_t int_value;
  std::shared_ptr<TyIntType> int_type;
};

struct TyConstFloat {
public:
  TyConstFloat(double _float_value, enum TyFloatType _float_type);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyConstFloat> make(double _float_value,
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
  ConsConstInt(std::shared_ptr<TyConstInt> _const_int);
  ConsConstInt(int64_t _int_value, int _bitwidth);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyConstInt> const_int;
};

struct ConsConstFloat : public TyConstant {
public:
  ConsConstFloat(std::shared_ptr<TyConstFloat> _const_float);
  ConsConstFloat(float _float_value, enum TyFloatType _float_type);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyConstFloat> const_float;
};

struct ConsConstUndef : public TyConstant {
public:
  ConsConstUndef(std::shared_ptr<TyValueType> _value_type);
  void serialize(cereal::JSONOutputArchive& archive) const;

private:
  std::shared_ptr<TyValueType> value_type;
};

// value

struct TyValue {
public:
  virtual void serialize(cereal::JSONOutputArchive &archive) const = 0;

  static std::shared_ptr<TyValue> make(const llvm::Value &value, enum TyTag _tag = llvmberry::Physical);
};

struct ConsId : public TyValue {
public:
  ConsId(std::shared_ptr<TyRegister> _register);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyValue> make(std::string _name,
                                       enum TyTag _tag);

  std::shared_ptr<TyRegister> reg;
};

struct ConsConstVal : public TyValue {
public:
  ConsConstVal(std::shared_ptr<TyConstant> _constant);
  void serialize(cereal::JSONOutputArchive &archive) const;

  std::shared_ptr<TyConstant> constant;
};

// instruction

struct TyInstruction {
public:
  virtual void serialize(cereal::JSONOutputArchive &archive) const = 0;
  static std::shared_ptr<TyInstruction> make(const llvm::Instruction &inst);
};

struct TyBinaryOperator{
public : 
  TyBinaryOperator(TyBop _opcode, std::shared_ptr<TyValueType> _operandtype, std::shared_ptr<TyValue> _operand1, std::shared_ptr<TyValue> _operand2);
  void serialize(cereal::JSONOutputArchive& archive) const;
  static std::shared_ptr<TyBinaryOperator> make(const llvm::BinaryOperator &bop);

private : 
  TyBop opcode;
  std::shared_ptr<TyValueType> operandtype;
  std::shared_ptr<TyValue> operand1;
  std::shared_ptr<TyValue> operand2;
};

struct TyFloatBinaryOperator{
public : 
  TyFloatBinaryOperator(TyFbop _opcode, std::shared_ptr<TyValueType> _operandtype, std::shared_ptr<TyValue> _operand1, std::shared_ptr<TyValue> _operand2);
  void serialize(cereal::JSONOutputArchive& archive) const;
  static std::shared_ptr<TyFloatBinaryOperator> make(const llvm::BinaryOperator &bop);

private : 
  TyFbop opcode;
  std::shared_ptr<TyValueType> operandtype;
  std::shared_ptr<TyValue> operand1;
  std::shared_ptr<TyValue> operand2;
};

struct TyICmpInst{
public :
  TyICmpInst(TyCond _predicate, std::shared_ptr<TyValueType> _operandtype, std::shared_ptr<TyValue> _operand1, std::shared_ptr<TyValue> _operand2);
  void serialize(cereal::JSONOutputArchive& archive) const;
  static std::shared_ptr<TyICmpInst> make(const llvm::ICmpInst &iCmpInst);

private :
  TyCond predicate;
  std::shared_ptr<TyValueType> operandtype;
  std::shared_ptr<TyValue> operand1;
  std::shared_ptr<TyValue> operand2;
};

struct TyFCmpInst{
public :
  TyFCmpInst(TyFCond _predicate, std::shared_ptr<TyValueType> _operandtype, std::shared_ptr<TyValue> _operand1, std::shared_ptr<TyValue> _operand2);
  void serialize(cereal::JSONOutputArchive& archive) const;
  static std::shared_ptr<TyFCmpInst> make(const llvm::FCmpInst &fCmpInst);

private :
  TyFCond predicate;
  std::shared_ptr<TyValueType> operandtype;
  std::shared_ptr<TyValue> operand1;
  std::shared_ptr<TyValue> operand2;
};

struct TyLoadInst{
public : 
  TyLoadInst(std::shared_ptr<TyValueType> _pointertype, std::shared_ptr<TyValueType> _valtype, std::shared_ptr<TyValue> _ptrvalue, int _align);
  void serialize(cereal::JSONOutputArchive& archive) const;
  static std::shared_ptr<TyLoadInst> make(const llvm::LoadInst &li);
  static std::shared_ptr<TyLoadInst> make(const llvm::StoreInst &si);

private : 
  std::shared_ptr<TyValueType> pointertype;
  std::shared_ptr<TyValueType> valtype;
  std::shared_ptr<TyValue> ptrvalue;
  int align;
};

struct ConsBinaryOp : public TyInstruction{
public : 
  ConsBinaryOp(std::shared_ptr<TyBinaryOperator> _binary_operator);
  static std::shared_ptr<TyInstruction> make(TyBop _opcode, std::shared_ptr<TyValueType> _operandtype, std::shared_ptr<TyValue> _operand1, std::shared_ptr<TyValue> _operand2);
  static std::shared_ptr<TyInstruction> make(const llvm::BinaryOperator &bop);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyBinaryOperator> binary_operator;
};

struct ConsFloatBinaryOp : public TyInstruction{
public : 
  ConsFloatBinaryOp(std::shared_ptr<TyFloatBinaryOperator> _binary_operator);
  static std::shared_ptr<TyInstruction> make(TyFbop _opcode, std::shared_ptr<TyValueType> _operandtype, std::shared_ptr<TyValue> _operand1, std::shared_ptr<TyValue> _operand2);
  static std::shared_ptr<TyInstruction> make(const llvm::BinaryOperator &bop);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyFloatBinaryOperator> binary_operator;
};

struct ConsICmpInst : public TyInstruction{
public :
  ConsICmpInst(std::shared_ptr<TyICmpInst> _icmp_inst);
  static std::shared_ptr<TyInstruction> make(TyCond _predicate, std::shared_ptr<TyValueType> _operandtype, std::shared_ptr<TyValue> _operand1, std::shared_ptr<TyValue> _operand2);
  static std::shared_ptr<TyInstruction> make(const llvm::ICmpInst &iCmpInst);
  void serialize(cereal::JSONOutputArchive& archive) const;

private :
  std::shared_ptr<TyICmpInst> icmp_inst;
};

struct ConsFCmpInst : public TyInstruction{
public :
  ConsFCmpInst(std::shared_ptr<TyFCmpInst> _fcmp_inst);
  static std::shared_ptr<TyInstruction> make(TyFCond _predicate, std::shared_ptr<TyValueType> _operandtype, std::shared_ptr<TyValue> _operand1, std::                   shared_ptr<TyValue> _operand2);
  static std::shared_ptr<TyInstruction> make(const llvm::FCmpInst &fCmpInst);
  void serialize(cereal::JSONOutputArchive& archive) const;

private :
  std::shared_ptr<TyFCmpInst> fcmp_inst;
};

struct ConsLoadInst : public TyInstruction{
public : 
  ConsLoadInst(std::shared_ptr<TyLoadInst> _load_inst);
  static std::shared_ptr<TyInstruction> make(std::shared_ptr<TyValueType> _pointertype, std::shared_ptr<TyValueType> _valtype, std::shared_ptr<TyValue> _ptrvalue, int _align);
  static std::shared_ptr<TyInstruction> make(const llvm::LoadInst &li);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyLoadInst> load_inst;
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

  static std::shared_ptr<TySize> make(int _size);

private:
  int size;
};

/* propagate */

// propagate expression

struct TyExpr {
public:
  virtual void serialize(cereal::JSONOutputArchive &archive) const = 0;

  static std::shared_ptr<TyExpr> make(const llvm::Value &value, enum TyTag _tag);
};

struct ConsVar : public TyExpr {
public:
  ConsVar(std::shared_ptr<TyRegister> _register_name);
  ConsVar(std::string _name, enum TyTag _tag);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyExpr> make(std::string _name,
                                      enum TyTag _tag);

private:
  std::shared_ptr<TyRegister> register_name;
};

struct ConsRhs : public TyExpr {
public:
  ConsRhs(std::shared_ptr<TyRegister> _register_name, enum TyScope _scope);
  ConsRhs(std::string _name, enum TyTag _tag, enum TyScope _scope);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyExpr> make(std::string _name,
                                      enum TyTag _tag,
                                      enum TyScope _scope);

private:
  std::shared_ptr<TyRegister> register_name;
  enum TyScope scope;
};

struct ConsConst : public TyExpr {
public:
  ConsConst(std::shared_ptr<TyConstant> _constant);

  ConsConst(int _int_value, int _value);
  ConsConst(float _float_value, enum TyFloatType _float_type);

  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyConstant> constant;
};

struct ConsInsn : public TyExpr{
public : 
  ConsInsn(std::shared_ptr<TyInstruction> _instruction);
  void serialize(cereal::JSONOutputArchive& archive) const;
  
  static std::shared_ptr<TyExpr> make(const llvm::Instruction &i);
  static std::shared_ptr<TyExpr> make(std::shared_ptr<TyInstruction> _instruction);

private : 
  std::shared_ptr<TyInstruction> instruction;
};

std::shared_ptr<TyExpr> makeExpr_fromStoreInst(const llvm::StoreInst* si);

// propagate object

struct TyPropagateLessdef {
public:
  TyPropagateLessdef(std::shared_ptr<TyExpr> _lhs,
                     std::shared_ptr<TyExpr> _rhs,
                     enum TyScope _scope);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyPropagateLessdef>
  make(std::shared_ptr<TyExpr> _lhs,
       std::shared_ptr<TyExpr> _rhs, enum TyScope _scope);

private:
  std::shared_ptr<TyExpr> lhs;
  std::shared_ptr<TyExpr> rhs;
  enum TyScope scope;
};

struct TyPropagateNoalias {
public:
  TyPropagateNoalias(std::shared_ptr<TyRegister> _lhs,
                     std::shared_ptr<TyRegister> _rhs, enum TyScope _scope);
  TyPropagateNoalias(std::string _lhs_name, enum TyTag _lhs_tag,
                     std::string _rhs_name, enum TyTag _rhs_tag,
                     enum TyScope _scope);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> lhs;
  std::shared_ptr<TyRegister> rhs;
  enum TyScope scope;
};

struct TyPropagateAlloca {
public :
  TyPropagateAlloca(std::shared_ptr<TyRegister> _p, 
                    enum TyScope _scope);
  void serialize(cereal::JSONOutputArchive& archive) const;

private :
  std::shared_ptr<TyRegister> p;
  enum TyScope scope;
};

struct TyPropagatePrivate {
public :
  TyPropagatePrivate(std::shared_ptr<TyRegister> _p, 
                     enum TyScope _scope);
  void serialize(cereal::JSONOutputArchive& archive) const;

private :
  std::shared_ptr<TyRegister> p;
  enum TyScope scope;
};

struct TyPropagateObject {
public:
  virtual void serialize(cereal::JSONOutputArchive &archive) const = 0;
};

struct ConsLessdef : public TyPropagateObject {
public:
  ConsLessdef(std::shared_ptr<TyPropagateLessdef> _propagate_lessdef);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyPropagateObject>
  make(std::shared_ptr<TyExpr> _lhs,
       std::shared_ptr<TyExpr> _rhs, enum TyScope _scope);

private:
  std::shared_ptr<TyPropagateLessdef> propagate_lessdef;
};

struct ConsNoalias : public TyPropagateObject {
public:
  ConsNoalias(std::shared_ptr<TyPropagateNoalias> _propagate_noalias);
  ConsNoalias(std::string _lhs_name, enum TyTag _lhs_tag, std::string _rhs_name,
              enum TyTag _rhs_tag, enum TyScope _scope);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyPropagateNoalias> propagate_noalias;
};

struct ConsAlloca : public TyPropagateObject {
public :
  ConsAlloca(std::shared_ptr<TyPropagateAlloca> _propagate_alloca);
  void serialize(cereal::JSONOutputArchive& archive) const;

  static std::shared_ptr<TyPropagateObject> make(std::shared_ptr<TyRegister> _p, 
                                                 enum TyScope _scope);

private :
  std::shared_ptr<TyPropagateAlloca> propagate_alloca;
};

struct ConsMaydiff : public TyPropagateObject {
public:
  ConsMaydiff(std::shared_ptr<TyRegister> _register_name);
  ConsMaydiff(std::string _name, enum TyTag _tag);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyPropagateObject> make(std::string _name,
                                                 enum TyTag _tag);

private:
  std::shared_ptr<TyRegister> register_name;
};

struct ConsPrivate : public TyPropagateObject {
public :
  ConsPrivate(std::shared_ptr<TyPropagatePrivate> _propagate_private);
  void serialize(cereal::JSONOutputArchive& archive) const;

  static std::shared_ptr<TyPropagateObject> make(std::shared_ptr<TyRegister> _p, 
                                                 enum TyScope _scope);

private :
  std::shared_ptr<TyPropagatePrivate> propagate_private;
};

// propagate range

struct TyPropagateRange {
public:
  virtual void serialize(cereal::JSONOutputArchive &archive) const = 0;
  virtual bool isGlobal(void) const { return false; }
};

struct ConsBounds : public TyPropagateRange {
public:
  ConsBounds(std::shared_ptr<TyPosition> _from,
             std::shared_ptr<TyPosition> _to);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyPropagateRange>
  make(std::shared_ptr<TyPosition> _from, std::shared_ptr<TyPosition> _to);

private:
  std::shared_ptr<TyPosition> from;
  std::shared_ptr<TyPosition> to;
};

struct ConsGlobal : public TyPropagateRange {
public:
  ConsGlobal();
  void serialize(cereal::JSONOutputArchive &archive) const;
  virtual bool isGlobal(void) const { return true; }

  static std::shared_ptr<TyPropagateRange> make();
};

struct TyPropagate {
public:
  TyPropagate(std::shared_ptr<TyPropagateObject> _propagate,
              std::shared_ptr<TyPropagateRange> _propagate_range);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyPropagateObject> propagate;
  std::shared_ptr<TyPropagateRange> propagate_range;
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
  ConsPropagate(std::shared_ptr<TyPropagate> _propagate);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyCommand>
  make(std::shared_ptr<TyPropagate> _propagate);
  static std::shared_ptr<TyCommand>
  make(std::shared_ptr<TyPropagateObject> _obj,
       std::shared_ptr<TyPropagateRange> _range);

private:
  std::shared_ptr<TyPropagate> propagate;
};

struct ConsInfrule : public TyCommand {
public:
  ConsInfrule(std::shared_ptr<TyPosition> _position,
              std::shared_ptr<TyInfrule> _infrule);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyCommand> make(std::shared_ptr<TyPosition> _position,
                                         std::shared_ptr<TyInfrule> _infrule);

private:
  std::shared_ptr<TyPosition> position;
  std::shared_ptr<TyInfrule> infrule;
};


/* core hint */

struct CoreHint {
public:
  CoreHint();
  CoreHint(std::string _module_id, std::string _function_id,
           std::string _opt_name);
  void addCommand(std::shared_ptr<TyCommand> c);
  const std::string &getDescription() const;
  void setDescription(const std::string &desc);
  void setOptimizationName(const std::string &name);
  void addNopPosition(std::shared_ptr<TyPosition> position);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::string module_id;
  std::string function_id;
  std::string opt_name;
  std::string description;
  std::vector<std::shared_ptr<TyPosition>> nop_positions;
  std::vector<std::shared_ptr<TyCommand>> commands;
};



} // llvmberry

#endif
