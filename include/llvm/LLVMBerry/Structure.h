#ifndef STRUCTURE_H
#define STRUCTURE_H

#include <iostream>
#include <set>
#include "llvm/IR/Instructions.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/Support/raw_ostream.h"

#include "cereal/archives/json.hpp"
#include "cereal/types/vector.hpp"
#include <cereal/types/memory.hpp>

namespace cereal {
[[noreturn]] void throw_exception(std::exception const &e);

template <class T>
void save(cereal::JSONOutputArchive &archive, std::shared_ptr<T> const &ptr) {
  ptr->serialize(archive);
}

template <class T1, class T2>
void save(cereal::JSONOutputArchive &archive, std::pair<T1, T2> const &p) {
  archive.makeArray();
  archive.writeName();

  archive(p.first);
  archive(p.second);
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

enum TyBop {
  BopAdd,
  BopSub,
  BopMul,
  BopUdiv,
  BopSdiv,
  BopUrem,
  BopSrem,
  BopShl,
  BopLshr,
  BopAshr,
  BopAnd,
  BopOr,
  BopXor,
};

enum TyFbop { BopFadd, BopFsub, BopFmul, BopFdiv, BopFrem };

enum TyIcmpPred {
  CondEq,
  CondNe,
  CondUgt,
  CondUge,
  CondUlt,
  CondUle,
  CondSgt,
  CondSge,
  CondSlt,
  CondSle
};
enum TyFcmpPred {
  CondFfalse,
  CondFoeq,
  CondFogt,
  CondFoge,
  CondFolt,
  CondFole,
  CondFone,
  CondFord,
  CondFuno,
  CondFueq,
  CondFugt,
  CondFuge,
  CondFult,
  CondFule,
  CondFune,
  CondFtrue
};

class CoreHint;

std::string getBasicBlockIndex(const llvm::BasicBlock *block);
std::string getVariable(const llvm::Value &value);
llvm::Instruction *getPHIResolved(llvm::Instruction *I, llvm::BasicBlock *PB);
int getCommandIndex(const llvm::Value &value);
int getTerminatorIndex(const llvm::TerminatorInst *instr);
bool name_instructions(llvm::Function &F);

std::string toString(llvmberry::TyBop bop);
std::string toString(llvmberry::TyFbop bop);
std::string toString(llvmberry::TyFloatType bop);
std::string toString(llvmberry::TyIcmpPred cond);
std::string toString(llvmberry::TyFcmpPred fcond);

bool isFloatOpcode(llvm::Instruction::BinaryOps ops);
TyFloatType getFloatType(llvm::Type *typ);
TyFbop getFbop(llvm::Instruction::BinaryOps ops);
TyBop getBop(llvm::Instruction::BinaryOps ops);
TyIcmpPred getIcmpPred(llvm::ICmpInst::Predicate prd);
TyFcmpPred getFcmpPred(llvm::FCmpInst::Predicate prd);

/*
 * position
 */

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

struct TyInstrIndex {
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
  make_end_of_block(enum TyScope _scope, const llvm::BasicBlock &BB,
                    int index);
  static std::shared_ptr<TyPosition>
  make_start_of_block(enum TyScope _scope, std::string _block_name);
  static std::shared_ptr<TyPosition> make(enum TyScope _scope,
                                          std::string _block_name,
                                          std::string _prev_block_name);
  static std::shared_ptr<TyPosition> make(enum TyScope _scope,
                                          const llvm::Instruction &I,
                                          std::string _prev_block_name);
  static std::shared_ptr<TyPosition> make(enum TyScope _scope,
                                          const llvm::Instruction &I, int index,
                                          std::string _prev_block_name);

private:
  enum TyScope scope;
  std::string block_name;
  std::shared_ptr<TyInstrIndex> instr_index;
};

/*
 * register
 */

struct TyRegister {
public:
  TyRegister(std::string _name, enum TyTag _tag);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyRegister> make(std::string _name, enum TyTag _tag);

  static bool isSame(std::shared_ptr<TyRegister> r1,
                     std::shared_ptr<TyRegister> r2);
  std::string getName();

private:
  std::string name;
  enum TyTag tag;
};

/*
 * type
 */

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

struct TyValueType {
public:
  virtual void serialize(cereal::JSONOutputArchive &archive) const = 0;
  static std::shared_ptr<TyValueType> make(const llvm::Type &type);
};

struct ConsVoidType : public TyValueType {
public:
  ConsVoidType();
  void serialize(cereal::JSONOutputArchive &archive) const;
};

struct ConsIntValueType : public TyValueType {
public:
  ConsIntValueType(std::shared_ptr<TyIntType> _int_type);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyIntType> int_type;
};

struct ConsFloatValueType : public TyValueType {
public:
  ConsFloatValueType(TyFloatType _float_type);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  TyFloatType float_type;
};

struct ConsNamedType : public TyValueType {
public:
  ConsNamedType(std::string _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::string s;
};

struct ConsPtrType : public TyValueType {
public:
  ConsPtrType(int _address_space, std::shared_ptr<TyValueType> _valuetype);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  int address_space;
  std::shared_ptr<TyValueType> valuetype;
};

struct ConsArrayType : public TyValueType {
public:
  ConsArrayType(uint64_t _array_size, std::shared_ptr<TyValueType> _valuetype);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  uint64_t array_size;
  std::shared_ptr<TyValueType> valuetype;
};

struct ConsVectorType : public TyValueType {
public:
  ConsVectorType(uint64_t _array_size, std::shared_ptr<TyValueType> _valuetype);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  uint64_t array_size;
  std::shared_ptr<TyValueType> valuetype;
};

struct ConsFunctionType : public TyValueType {
public:
  ConsFunctionType(std::shared_ptr<TyValueType> _ret_type,
                   std::vector<std::shared_ptr<TyValueType>> &_arg_ty_list,
                   bool _is_vararg, int _vararg_size);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValueType> ret_type;
  std::vector<std::shared_ptr<TyValueType>> arg_ty_list;
  bool is_vararg;
  int vararg_size;
};

struct ConsStructType : public TyValueType{
public : 
  ConsStructType(std::vector<std::shared_ptr<TyValueType>> &_vec_value_type);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::vector<std::shared_ptr<TyValueType>> vec_value_type;
};

/*
 * size
 */

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

/*
 * constants
 */

struct TyConstInt {
public:
  TyConstInt(int64_t _int_value, std::shared_ptr<TyIntType> _int_type);
  TyConstInt(int64_t _int_value, int _bitwidth);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyConstInt> make(int64_t _int_value, int _bitwidth);
  static std::shared_ptr<TyConstInt> make(const llvm::ConstantInt &ci);

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

struct TyConstGlobalVarAddr {
public:
  TyConstGlobalVarAddr(std::string _var_id,
                       std::shared_ptr<TyValueType> _var_type);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyConstGlobalVarAddr>
  make(const llvm::GlobalObject &gv);

private:
  std::string var_id;
  std::shared_ptr<TyValueType> var_type;
};

// constant exprs

class TyConstant;

struct TyConstantExpr {
public:
  virtual void serialize(cereal::JSONOutputArchive &archive) const = 0;
  static std::shared_ptr<TyConstantExpr> make(const llvm::ConstantExpr &ce);
};

struct TyConstExprGetElementPtr {
public:
  TyConstExprGetElementPtr(std::shared_ptr<TyValueType> _srcelemty,
                           std::shared_ptr<TyConstant> _v,
                           std::vector<std::shared_ptr<TyConstant>> _idxlist,
                           std::shared_ptr<TyValueType> _dstty,
                           bool _is_inbounds);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValueType> srcelemty;
  std::shared_ptr<TyConstant> v;
  std::vector<std::shared_ptr<TyConstant>> idxlist;
  std::shared_ptr<TyValueType> dstty;
  bool is_inbounds;
};

struct ConsConstExprGetElementPtr : public TyConstantExpr {
public:
  ConsConstExprGetElementPtr(
      std::shared_ptr<TyConstExprGetElementPtr> _const_expr_get_element_ptr);
  static std::shared_ptr<TyConstantExpr>
  make(std::shared_ptr<TyValueType> _srcelemty, std::shared_ptr<TyConstant> _v,
       std::vector<std::shared_ptr<TyConstant>> _idxlist,
       std::shared_ptr<TyValueType> _dstty, bool _is_inbounds);
  static std::shared_ptr<TyConstantExpr> make(const llvm::ConstantExpr &ce);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyConstExprGetElementPtr> const_expr_get_element_ptr;
};

struct TyConstExprBitcast {
public:
  TyConstExprBitcast(std::shared_ptr<TyConstant> _v,
                     std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyConstant> v;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsConstExprBitcast : public TyConstantExpr {
public:
  ConsConstExprBitcast(std::shared_ptr<TyConstExprBitcast> _const_expr_bitcast);
  static std::shared_ptr<TyConstantExpr>
  make(std::shared_ptr<TyConstant> _v, std::shared_ptr<TyValueType> _dstty);
  static std::shared_ptr<TyConstantExpr> make(const llvm::ConstantExpr &ce);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyConstExprBitcast> const_expr_bitcast;
};

struct TyConstExprInttoptr{
public : 
  TyConstExprInttoptr(std::shared_ptr<TyConstant> _v, std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyConstant> v;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsConstExprInttoptr : public TyConstantExpr{
public : 
  ConsConstExprInttoptr(std::shared_ptr<TyConstExprInttoptr> _const_expr_inttoptr);
  static std::shared_ptr<TyConstantExpr> make(std::shared_ptr<TyConstant> _v, std::shared_ptr<TyValueType> _dstty);
  static std::shared_ptr<TyConstantExpr> make(const llvm::ConstantExpr &ce);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyConstExprInttoptr> const_expr_inttoptr;
};

struct TyConstExprPtrtoint{
public : 
  TyConstExprPtrtoint(std::shared_ptr<TyConstant> _v, std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyConstant> v;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsConstExprPtrtoint : public TyConstantExpr{
public : 
  ConsConstExprPtrtoint(std::shared_ptr<TyConstExprPtrtoint> _const_expr_ptrtoint);
  static std::shared_ptr<TyConstantExpr> make(std::shared_ptr<TyConstant> _v, std::shared_ptr<TyValueType> _dstty);
  static std::shared_ptr<TyConstantExpr> make(const llvm::ConstantExpr &ce);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyConstExprPtrtoint> const_expr_ptrtoint;
};


// constants

struct TyConstant {
public:
  virtual void serialize(cereal::JSONOutputArchive &archive) const = 0;
  static std::shared_ptr<TyConstant> make(const llvm::Constant &c);
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
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValueType> value_type;
};

struct ConsConstNull : public TyConstant {
public:
  ConsConstNull(int _address_space, std::shared_ptr<TyValueType> _value_type);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  int address_space;
  std::shared_ptr<TyValueType> value_type;
};

struct ConsConstDataVector : public TyConstant {
public:
  ConsConstDataVector(std::shared_ptr<TyValueType> _elem_type,
                      std::vector<std::shared_ptr<TyConstant> > &_elements);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValueType> elem_type;
  std::vector<std::shared_ptr<TyConstant> > elements;
};

struct ConsConstGlobalVarAddr : public TyConstant {
public:
  ConsConstGlobalVarAddr(
      std::shared_ptr<TyConstGlobalVarAddr> _const_global_var_addr);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyConstant>
  make(std::string _var_id, std::shared_ptr<TyValueType> _var_type);
  static std::shared_ptr<TyConstant> make(const llvm::GlobalVariable &gv);

private:
  std::shared_ptr<TyConstGlobalVarAddr> const_global_var_addr;
};

struct ConsConstExpr : public TyConstant {
public:
  ConsConstExpr(std::shared_ptr<TyConstantExpr> _constant_expr);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyConstant> make(const llvm::ConstantExpr &gv);

private:
  std::shared_ptr<TyConstantExpr> constant_expr;
};

/*
 * Value
 */

struct TyValue {
public:
  virtual void serialize(cereal::JSONOutputArchive &archive) const = 0;

  static std::shared_ptr<TyValue> make(const llvm::Value &value,
                                       enum TyTag _tag = llvmberry::Physical);
};

struct ConsId : public TyValue {
public:
  ConsId(std::shared_ptr<TyRegister> _register);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyValue> make(std::string _name, enum TyTag _tag);

  std::shared_ptr<TyRegister> reg;
};

struct ConsConstVal : public TyValue {
public:
  ConsConstVal(std::shared_ptr<TyConstant> _constant);
  void serialize(cereal::JSONOutputArchive &archive) const;

  std::shared_ptr<TyConstant> constant;
};

/*
 * Pointer
 */

struct TyPointer {
public:
  TyPointer(std::shared_ptr<TyValue> _v, std::shared_ptr<TyValueType> _ty);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyPointer> make(const llvm::Value &v);

private:
  std::shared_ptr<TyValue> v;
  std::shared_ptr<TyValueType> ty;
};

/*
 * instruction
 */

struct TyInstruction {
public:
  virtual void serialize(cereal::JSONOutputArchive &archive) const = 0;
  static std::shared_ptr<TyInstruction> make(const llvm::Instruction &inst);
};

struct TyBinaryOperator {
public:
  TyBinaryOperator(TyBop _opcode, std::shared_ptr<TyValueType> _operandtype,
                   std::shared_ptr<TyValue> _operand1,
                   std::shared_ptr<TyValue> _operand2);
  void serialize(cereal::JSONOutputArchive &archive) const;
  static std::shared_ptr<TyBinaryOperator>
  make(const llvm::BinaryOperator &bop);

private:
  TyBop opcode;
  std::shared_ptr<TyValueType> operandtype;
  std::shared_ptr<TyValue> operand1;
  std::shared_ptr<TyValue> operand2;
};

struct TyFloatBinaryOperator {
public:
  TyFloatBinaryOperator(TyFbop _opcode,
                        std::shared_ptr<TyValueType> _operandtype,
                        std::shared_ptr<TyValue> _operand1,
                        std::shared_ptr<TyValue> _operand2);
  void serialize(cereal::JSONOutputArchive &archive) const;
  static std::shared_ptr<TyFloatBinaryOperator>
  make(const llvm::BinaryOperator &bop);

private:
  TyFbop opcode;
  std::shared_ptr<TyValueType> operandtype;
  std::shared_ptr<TyValue> operand1;
  std::shared_ptr<TyValue> operand2;
};

struct TyICmpInst {
public:
  TyICmpInst(TyIcmpPred _predicate, std::shared_ptr<TyValueType> _operandtype,
             std::shared_ptr<TyValue> _operand1,
             std::shared_ptr<TyValue> _operand2);
  void serialize(cereal::JSONOutputArchive &archive) const;
  static std::shared_ptr<TyICmpInst> make(const llvm::ICmpInst &iCmpInst);

private:
  TyIcmpPred predicate;
  std::shared_ptr<TyValueType> operandtype;
  std::shared_ptr<TyValue> operand1;
  std::shared_ptr<TyValue> operand2;
};

struct TyFCmpInst {
public:
  TyFCmpInst(TyFcmpPred _predicate, std::shared_ptr<TyValueType> _operandtype,
             std::shared_ptr<TyValue> _operand1,
             std::shared_ptr<TyValue> _operand2);
  void serialize(cereal::JSONOutputArchive &archive) const;
  static std::shared_ptr<TyFCmpInst> make(const llvm::FCmpInst &fCmpInst);

private:
  TyFcmpPred predicate;
  std::shared_ptr<TyValueType> operandtype;
  std::shared_ptr<TyValue> operand1;
  std::shared_ptr<TyValue> operand2;
};

struct TyLoadInst {
public:
  TyLoadInst(std::shared_ptr<TyValueType> _pointertype,
             std::shared_ptr<TyValueType> _valtype,
             std::shared_ptr<TyValue> _ptrvalue, int _align);
  void serialize(cereal::JSONOutputArchive &archive) const;
  static std::shared_ptr<TyLoadInst> make(const llvm::AllocaInst &ai);
  static std::shared_ptr<TyLoadInst> make(const llvm::LoadInst &li);
  static std::shared_ptr<TyLoadInst> make(const llvm::StoreInst &si);
  static std::shared_ptr<TyLoadInst> makeAlignOne(llvm::Instruction *i);

private:
  std::shared_ptr<TyValueType> pointertype;
  std::shared_ptr<TyValueType> valtype;
  std::shared_ptr<TyValue> ptrvalue;
  int align;
};

struct TySelectInst{
public : 
  TySelectInst(std::shared_ptr<TyValue> _cond, std::shared_ptr<TyValueType> _valty, std::shared_ptr<TyValue> _trueval, std::shared_ptr<TyValue> _falseval);
  void serialize(cereal::JSONOutputArchive& archive) const;
  static std::shared_ptr<TySelectInst> make(const llvm::SelectInst &si);

private : 
  std::shared_ptr<TyValue> cond;
  std::shared_ptr<TyValueType> valty;
  std::shared_ptr<TyValue> trueval;
  std::shared_ptr<TyValue> falseval;
};

struct TyBitCastInst {
public:
  TyBitCastInst(std::shared_ptr<TyValueType> _fromty,
                std::shared_ptr<TyValue> _v,
                std::shared_ptr<TyValueType> _toty);
  void serialize(cereal::JSONOutputArchive &archive) const;
  static std::shared_ptr<TyBitCastInst> make(const llvm::BitCastInst &li);

private:
  std::shared_ptr<TyValueType> fromty;
  std::shared_ptr<TyValue> v;
  std::shared_ptr<TyValueType> toty;
};

struct TyIntToPtrInst {
public:
  TyIntToPtrInst(std::shared_ptr<TyValueType> _fromty,
                 std::shared_ptr<TyValue> _v,
                 std::shared_ptr<TyValueType> _toty);
  void serialize(cereal::JSONOutputArchive &archive) const;
  static std::shared_ptr<TyIntToPtrInst> make(const llvm::IntToPtrInst &li);

private:
  std::shared_ptr<TyValueType> fromty;
  std::shared_ptr<TyValue> v;
  std::shared_ptr<TyValueType> toty;
};

struct TyPtrToIntInst {
public:
  TyPtrToIntInst(std::shared_ptr<TyValueType> _fromty,
                 std::shared_ptr<TyValue> _v,
                 std::shared_ptr<TyValueType> _toty);
  void serialize(cereal::JSONOutputArchive &archive) const;
  static std::shared_ptr<TyPtrToIntInst> make(const llvm::PtrToIntInst &li);

private:
  std::shared_ptr<TyValueType> fromty;
  std::shared_ptr<TyValue> v;
  std::shared_ptr<TyValueType> toty;
};

struct TyGetElementPtrInst {
public:
  TyGetElementPtrInst(
      std::shared_ptr<TyValueType> _ty, std::shared_ptr<TyValueType> _retty,
      std::shared_ptr<TyValue> _ptr,
      std::vector<std::pair<std::shared_ptr<TySize>, std::shared_ptr<TyValue>>>
          &_indexes,
      bool is_inbounds);
  void serialize(cereal::JSONOutputArchive &archive) const;
  static std::shared_ptr<TyGetElementPtrInst>
  make(const llvm::GetElementPtrInst &li);

private:
  std::shared_ptr<TyValueType> ty;
  std::shared_ptr<TyValueType> retty;
  std::shared_ptr<TyValue> ptr;
  std::vector<std::pair<std::shared_ptr<TySize>, std::shared_ptr<TyValue>>>
      indexes;
  bool is_inbounds;
};

struct TyFpextInst{
public : 
  TyFpextInst(std::shared_ptr<TyValueType> _fromty, std::shared_ptr<TyValue> _v, std::shared_ptr<TyValueType> _toty);
  void serialize(cereal::JSONOutputArchive& archive) const;
  static std::shared_ptr<TyFpextInst> make(const llvm::FPExtInst &fpti);

private : 
  std::shared_ptr<TyValueType> fromty;
  std::shared_ptr<TyValue> v;
  std::shared_ptr<TyValueType> toty;
};

struct TyFptruncInst{
public : 
  TyFptruncInst(std::shared_ptr<TyValueType> _fromty, std::shared_ptr<TyValue> _v, std::shared_ptr<TyValueType> _toty);
  void serialize(cereal::JSONOutputArchive& archive) const;
  static std::shared_ptr<TyFptruncInst> make(const llvm::FPTruncInst &fpti);

private : 
  std::shared_ptr<TyValueType> fromty;
  std::shared_ptr<TyValue> v;
  std::shared_ptr<TyValueType> toty;
};

struct TyZextInst{
public : 
  TyZextInst(std::shared_ptr<TyValueType> _fromty, std::shared_ptr<TyValue> _v, std::shared_ptr<TyValueType> _toty);
  void serialize(cereal::JSONOutputArchive& archive) const;
  static std::shared_ptr<TyZextInst> make(const llvm::ZExtInst &zei);

private : 
  std::shared_ptr<TyValueType> fromty;
  std::shared_ptr<TyValue> v;
  std::shared_ptr<TyValueType> toty;
};

struct TySextInst{
public : 
  TySextInst(std::shared_ptr<TyValueType> _fromty, std::shared_ptr<TyValue> _v, std::shared_ptr<TyValueType> _toty);
  void serialize(cereal::JSONOutputArchive& archive) const;
  static std::shared_ptr<TySextInst> make(const llvm::SExtInst &sei);

private : 
  std::shared_ptr<TyValueType> fromty;
  std::shared_ptr<TyValue> v;
  std::shared_ptr<TyValueType> toty;
};

struct TyTruncInst{
public : 
  TyTruncInst(std::shared_ptr<TyValueType> _fromty, std::shared_ptr<TyValue> _v, std::shared_ptr<TyValueType> _toty);
  void serialize(cereal::JSONOutputArchive& archive) const;
  static std::shared_ptr<TyTruncInst> make(const llvm::TruncInst &ti);

private : 
  std::shared_ptr<TyValueType> fromty;
  std::shared_ptr<TyValue> v;
  std::shared_ptr<TyValueType> toty;
};

struct TySitofpInst{
public : 
  TySitofpInst(std::shared_ptr<TyValueType> _fromty, std::shared_ptr<TyValue> _v, std::shared_ptr<TyValueType> _toty);
  void serialize(cereal::JSONOutputArchive& archive) const;
  static std::shared_ptr<TySitofpInst> make(const llvm::SIToFPInst &stfi);

private : 
  std::shared_ptr<TyValueType> fromty;
  std::shared_ptr<TyValue> v;
  std::shared_ptr<TyValueType> toty;
};

struct TyUitofpInst{
public : 
  TyUitofpInst(std::shared_ptr<TyValueType> _fromty, std::shared_ptr<TyValue> _v, std::shared_ptr<TyValueType> _toty);
  void serialize(cereal::JSONOutputArchive& archive) const;
  static std::shared_ptr<TyUitofpInst> make(const llvm::UIToFPInst &utfi);

private : 
  std::shared_ptr<TyValueType> fromty;
  std::shared_ptr<TyValue> v;
  std::shared_ptr<TyValueType> toty;
};

struct TyInsertValueInst{
public : 
  TyInsertValueInst(std::shared_ptr<TyValueType> _aggrty, std::shared_ptr<TyValue> _aggrv, std::shared_ptr<TyValueType> _argty, std::shared_ptr<TyValue> _argv, std::vector<unsigned> _idx);
  void serialize(cereal::JSONOutputArchive& archive) const;
  static std::shared_ptr<TyInsertValueInst> make(const llvm::InsertValueInst &ivi);

private : 
  std::shared_ptr<TyValueType> aggrty;
  std::shared_ptr<TyValue> aggrv;
  std::shared_ptr<TyValueType> argty;
  std::shared_ptr<TyValue> argv;
  std::vector<unsigned> idx;
};

struct TyExtractValueInst{
public : 
  TyExtractValueInst(std::shared_ptr<TyValueType> _aggrty, std::shared_ptr<TyValue> _aggrv, std::vector<unsigned> _idx, std::shared_ptr<TyValueType> _retty);
  void serialize(cereal::JSONOutputArchive& archive) const;
  static std::shared_ptr<TyExtractValueInst> make(const llvm::ExtractValueInst &evi);

private : 
  std::shared_ptr<TyValueType> aggrty;
  std::shared_ptr<TyValue> aggrv;
  std::vector<unsigned> idx;
  std::shared_ptr<TyValueType> retty;
};

struct ConsBinaryOp : public TyInstruction {
public:
  ConsBinaryOp(std::shared_ptr<TyBinaryOperator> _binary_operator);
  static std::shared_ptr<TyInstruction>
  make(TyBop _opcode, std::shared_ptr<TyValueType> _operandtype,
       std::shared_ptr<TyValue> _operand1, std::shared_ptr<TyValue> _operand2);
  static std::shared_ptr<TyInstruction> make(const llvm::BinaryOperator &bop);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyBinaryOperator> binary_operator;
};

struct ConsFloatBinaryOp : public TyInstruction {
public:
  ConsFloatBinaryOp(std::shared_ptr<TyFloatBinaryOperator> _binary_operator);
  static std::shared_ptr<TyInstruction>
  make(TyFbop _opcode, std::shared_ptr<TyValueType> _operandtype,
       std::shared_ptr<TyValue> _operand1, std::shared_ptr<TyValue> _operand2);
  static std::shared_ptr<TyInstruction> make(const llvm::BinaryOperator &bop);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyFloatBinaryOperator> binary_operator;
};

struct ConsICmpInst : public TyInstruction {
public:
  ConsICmpInst(std::shared_ptr<TyICmpInst> _icmp_inst);
  static std::shared_ptr<TyInstruction>
  make(TyIcmpPred _predicate, std::shared_ptr<TyValueType> _operandtype,
       std::shared_ptr<TyValue> _operand1, std::shared_ptr<TyValue> _operand2);
  static std::shared_ptr<TyInstruction> make(const llvm::ICmpInst &iCmpInst);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyICmpInst> icmp_inst;
};

struct ConsFCmpInst : public TyInstruction {
public:
  ConsFCmpInst(std::shared_ptr<TyFCmpInst> _fcmp_inst);
  static std::shared_ptr<TyInstruction>
  make(TyFcmpPred _predicate, std::shared_ptr<TyValueType> _operandtype,
       std::shared_ptr<TyValue> _operand1, std::shared_ptr<TyValue> _operand2);
  static std::shared_ptr<TyInstruction> make(const llvm::FCmpInst &fCmpInst);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyFCmpInst> fcmp_inst;
};

struct ConsLoadInst : public TyInstruction {
public:
  ConsLoadInst(std::shared_ptr<TyLoadInst> _load_inst);
  static std::shared_ptr<TyInstruction>
  make(std::shared_ptr<TyValueType> _pointertype,
       std::shared_ptr<TyValueType> _valtype,
       std::shared_ptr<TyValue> _ptrvalue, int _align);
  static std::shared_ptr<TyInstruction> make(const llvm::LoadInst &li);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyLoadInst> load_inst;
};

struct ConsSelectInst : public TyInstruction{
public : 
  ConsSelectInst(std::shared_ptr<TySelectInst> _select_inst);
  static std::shared_ptr<TyInstruction> make(std::shared_ptr<TyValue> _cond, std::shared_ptr<TyValueType> _valty, std::shared_ptr<TyValue> _trueval, std::shared_ptr<TyValue> _falseval);
  void serialize(cereal::JSONOutputArchive& archive) const;
  static std::shared_ptr<TyInstruction> make(const llvm::SelectInst &si);

private : 
  std::shared_ptr<TySelectInst> select_inst;
};

struct ConsBitCastInst : public TyInstruction {
public:
  ConsBitCastInst(std::shared_ptr<TyBitCastInst> _bit_cast_inst);
  static std::shared_ptr<TyInstruction>
  make(std::shared_ptr<TyValueType> _fromty, std::shared_ptr<TyValue> _v,
       std::shared_ptr<TyValueType> _toty);
  static std::shared_ptr<TyInstruction> make(const llvm::BitCastInst &bci);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyBitCastInst> bit_cast_inst;
};

struct ConsIntToPtrInst : public TyInstruction {
public:
  ConsIntToPtrInst(std::shared_ptr<TyIntToPtrInst> _bit_cast_inst);
  static std::shared_ptr<TyInstruction>
  make(std::shared_ptr<TyValueType> _fromty, std::shared_ptr<TyValue> _v,
       std::shared_ptr<TyValueType> _toty);
  static std::shared_ptr<TyInstruction> make(const llvm::IntToPtrInst &itpi);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyIntToPtrInst> int_to_ptr_inst;
};

struct ConsPtrToIntInst : public TyInstruction {
public:
  ConsPtrToIntInst(std::shared_ptr<TyPtrToIntInst> _ptr_to_int_inst);
  static std::shared_ptr<TyInstruction>
  make(std::shared_ptr<TyValueType> _fromty, std::shared_ptr<TyValue> _v,
       std::shared_ptr<TyValueType> _toty);
  static std::shared_ptr<TyInstruction> make(const llvm::PtrToIntInst &ptii);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyPtrToIntInst> ptr_to_int_inst;
};

struct ConsGetElementPtrInst : public TyInstruction {
public:
  ConsGetElementPtrInst(
      std::shared_ptr<TyGetElementPtrInst> _get_element_ptr_inst);
  static std::shared_ptr<TyInstruction>
  make(std::shared_ptr<TyValueType> _ty, std::shared_ptr<TyValueType> _ptrty,
       std::shared_ptr<TyValue> _ptr,
       std::vector<std::pair<std::shared_ptr<TySize>, std::shared_ptr<TyValue>>>
           &_indexes,
       bool is_inbounds);
  static std::shared_ptr<TyInstruction>
  make(const llvm::GetElementPtrInst &gepi);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyGetElementPtrInst> get_element_ptr_inst;
};

struct ConsFpextInst : public TyInstruction{
public : 
  ConsFpextInst(std::shared_ptr<TyFpextInst> _fpext_inst);
  static std::shared_ptr<TyInstruction> make(std::shared_ptr<TyValueType> _fromty, std::shared_ptr<TyValue> _v, std::shared_ptr<TyValueType> _toty);
  static std::shared_ptr<TyInstruction> make(const llvm::FPExtInst &fpei);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyFpextInst> fpext_inst;
};

struct ConsFptruncInst : public TyInstruction{
public : 
  ConsFptruncInst(std::shared_ptr<TyFptruncInst> _fptrunc_inst);
  static std::shared_ptr<TyInstruction> make(std::shared_ptr<TyValueType> _fromty, std::shared_ptr<TyValue> _v, std::shared_ptr<TyValueType> _toty);
  static std::shared_ptr<TyInstruction> make(const llvm::FPTruncInst &fpti);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyFptruncInst> fptrunc_inst;
};

struct ConsZextInst : public TyInstruction{
public : 
  ConsZextInst(std::shared_ptr<TyZextInst> _zext_inst);
  static std::shared_ptr<TyInstruction> make(std::shared_ptr<TyValueType> _fromty, std::shared_ptr<TyValue> _v, std::shared_ptr<TyValueType> _toty);
  static std::shared_ptr<TyInstruction> make(const llvm::ZExtInst &zi);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyZextInst> zext_inst;
};

struct ConsSextInst : public TyInstruction{
public : 
  ConsSextInst(std::shared_ptr<TySextInst> _sext_inst);
  static std::shared_ptr<TyInstruction> make(std::shared_ptr<TyValueType> _fromty, std::shared_ptr<TyValue> _v, std::shared_ptr<TyValueType> _toty);
  static std::shared_ptr<TyInstruction> make(const llvm::SExtInst &si);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TySextInst> sext_inst;
};

struct ConsTruncInst : public TyInstruction{
public : 
  ConsTruncInst(std::shared_ptr<TyTruncInst> _trunc_inst);
  static std::shared_ptr<TyInstruction> make(std::shared_ptr<TyValueType> _fromty, std::shared_ptr<TyValue> _v, std::shared_ptr<TyValueType> _toty);
  static std::shared_ptr<TyInstruction> make(const llvm::TruncInst &ti);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyTruncInst> trunc_inst;
};

struct ConsSitofpInst : public TyInstruction{
public : 
  ConsSitofpInst(std::shared_ptr<TySitofpInst> _sitofp_inst);
  static std::shared_ptr<TyInstruction> make(std::shared_ptr<TyValueType> _fromty, std::shared_ptr<TyValue> _v, std::shared_ptr<TyValueType> _toty);
  static std::shared_ptr<TyInstruction> make(const llvm::SIToFPInst &stfi);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TySitofpInst> sitofp_inst;
};

struct ConsUitofpInst : public TyInstruction{
public : 
  ConsUitofpInst(std::shared_ptr<TyUitofpInst> _uitofp_inst);
  static std::shared_ptr<TyInstruction> make(std::shared_ptr<TyValueType> _fromty, std::shared_ptr<TyValue> _v, std::shared_ptr<TyValueType> _toty);
  static std::shared_ptr<TyInstruction> make(const llvm::UIToFPInst &utfi);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyUitofpInst> uitofp_inst;
};

struct ConsExtractValueInst : public TyInstruction{
public : 
  ConsExtractValueInst(std::shared_ptr<TyExtractValueInst> _extract_value_inst);
  static std::shared_ptr<TyInstruction> make(std::shared_ptr<TyValueType> _aggrty, std::shared_ptr<TyValue> _aggrv, std::vector<unsigned> _idx, std::shared_ptr<TyValueType> _retty);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyExtractValueInst> extract_value_inst;
};

struct ConsInsertValueInst : public TyInstruction{
public : 
  ConsInsertValueInst(std::shared_ptr<TyInsertValueInst> _insert_value_inst);
  static std::shared_ptr<TyInstruction> make(std::shared_ptr<TyValueType> _aggrty, std::shared_ptr<TyValue> _aggrv, std::shared_ptr<TyValueType> _argty, std::shared_ptr<TyValue> _argv, std::vector<unsigned> _idx);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyInsertValueInst> insert_value_inst;
};


/* propagate */

// propagate expression

struct TyExpr {
public:
  virtual void serialize(cereal::JSONOutputArchive &archive) const = 0;

  static std::shared_ptr<TyExpr> make(const std::shared_ptr<TyValue> tyval);
  static std::shared_ptr<TyExpr> make(const llvm::Value &value,
                                      enum TyTag _tag = llvmberry::Physical);
};

struct ConsVar : public TyExpr {
public:
  ConsVar(std::shared_ptr<TyRegister> _register_name);
  ConsVar(std::string _name, enum TyTag _tag);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyExpr> make(std::string _name, enum TyTag _tag);

  std::shared_ptr<TyRegister> getTyReg();
  void updateTyReg(std::shared_ptr<TyRegister> newTyReg);

private:
  std::shared_ptr<TyRegister> register_name;
};

struct ConsRhs : public TyExpr {
public:
  ConsRhs(std::shared_ptr<TyRegister> _register_name, enum TyScope _scope);
  ConsRhs(std::string _name, enum TyTag _tag, enum TyScope _scope);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyExpr> make(std::string _name, enum TyTag _tag,
                                      enum TyScope _scope);

private:
  std::shared_ptr<TyRegister> register_name;
  enum TyScope scope;
};

struct ConsConst : public TyExpr {
public:
  ConsConst(std::shared_ptr<TyConstant> _constant);

  ConsConst(int _int_value, int _bitwidth);
  ConsConst(float _float_value, enum TyFloatType _float_type);

  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyExpr> make(int _int_value, int _bitwidth);

  std::shared_ptr<TyConstant> getTyConst();

private:
  std::shared_ptr<TyConstant> constant;
};

struct ConsInsn : public TyExpr {
public:
  ConsInsn(std::shared_ptr<TyInstruction> _instruction);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyExpr> make(const llvm::Instruction &i);
  static std::shared_ptr<TyExpr>
  make(std::shared_ptr<TyInstruction> _instruction);

  std::shared_ptr<TyInstruction> getTyInsn();

private:
  std::shared_ptr<TyInstruction> instruction;
};

/*
 * propagate object
 */

struct TyPropagateLessdef {
public:
  TyPropagateLessdef(std::shared_ptr<TyExpr> _lhs, std::shared_ptr<TyExpr> _rhs,
                     enum TyScope _scope);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyPropagateLessdef> make(std::shared_ptr<TyExpr> _lhs,
                                                  std::shared_ptr<TyExpr> _rhs,
                                                  enum TyScope _scope);

  std::shared_ptr<TyExpr> getLhs();
  std::shared_ptr<TyExpr> getRhs();
  void updateRhs(std::shared_ptr<TyExpr>(newExpr));

private:
  std::shared_ptr<TyExpr> lhs;
  std::shared_ptr<TyExpr> rhs;
  enum TyScope scope;
};

struct TyPropagateNoalias {
public:
  TyPropagateNoalias(std::shared_ptr<TyPointer> _lhs,
                     std::shared_ptr<TyPointer> _rhs, enum TyScope _scope);
  /*TyPropagateNoalias(std::string _lhs_name, enum TyTag _lhs_tag,
                     std::string _rhs_name, enum TyTag _rhs_tag,
                     enum TyScope _scope);*/
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyPropagateNoalias>
  make(std::shared_ptr<TyPointer> _lhs, std::shared_ptr<TyPointer> _rhs,
       enum TyScope _scope);

private:
  std::shared_ptr<TyPointer> lhs;
  std::shared_ptr<TyPointer> rhs;
  enum TyScope scope;
};

struct TyPropagateDiffblock {
public:
  TyPropagateDiffblock(std::shared_ptr<TyValue> _lhs,
                       std::shared_ptr<TyValue> _rhs, enum TyScope _scope);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyPropagateDiffblock>
  make(std::shared_ptr<TyValue> _lhs, std::shared_ptr<TyValue> _rhs,
       enum TyScope _scope);

private:
  std::shared_ptr<TyValue> lhs;
  std::shared_ptr<TyValue> rhs;
  enum TyScope scope;
};

struct TyPropagateUnique {
public:
  TyPropagateUnique(std::string _register_name, enum TyScope _scope);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::string register_name;
  enum TyScope scope;
};

struct TyPropagatePrivate {
public:
  TyPropagatePrivate(std::shared_ptr<TyRegister> _p, enum TyScope _scope);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
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

  static std::shared_ptr<TyPropagateObject> make(std::shared_ptr<TyExpr> _lhs,
                                                 std::shared_ptr<TyExpr> _rhs,
                                                 enum TyScope _scope);

private:
  std::shared_ptr<TyPropagateLessdef> propagate_lessdef;
};

struct ConsNoalias : public TyPropagateObject {
public:
  ConsNoalias(std::shared_ptr<TyPropagateNoalias> _propagate_noalias);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyPropagateObject>
  make(std::shared_ptr<TyPointer> _lhs, std::shared_ptr<TyPointer> _rhs,
       enum TyScope _scope);

private:
  std::shared_ptr<TyPropagateNoalias> propagate_noalias;
};

struct ConsDiffblock : public TyPropagateObject {
public:
  ConsDiffblock(std::shared_ptr<TyPropagateDiffblock> _propagate_diffblock);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyPropagateObject> make(std::shared_ptr<TyValue> _lhs,
                                                 std::shared_ptr<TyValue> _rhs,
                                                 enum TyScope _scope);

private:
  std::shared_ptr<TyPropagateDiffblock> propagate_diffblock;
};

struct ConsUnique : public TyPropagateObject {
public:
  ConsUnique(std::shared_ptr<TyPropagateUnique> _propagate_unique);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyPropagateObject> make(std::string _register_name,
                                                 enum TyScope _scope);

private:
  std::shared_ptr<TyPropagateUnique> propagate_unique;
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
public:
  ConsPrivate(std::shared_ptr<TyPropagatePrivate> _propagate_private);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyPropagateObject> make(std::shared_ptr<TyRegister> _p,
                                                 enum TyScope _scope);

private:
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
  enum RETURN_CODE { ACTUAL = 0, ADMITTED, FAIL };
  CoreHint();
  CoreHint(std::string _module_id, std::string _function_id,
           std::string _opt_name, std::string _description = "");
  const std::string &getDescription() const;
  void setDescription(const std::string &desc);
  void appendToDescription(const std::string &desc);
  const RETURN_CODE &getReturnCode() const;
  void setReturnCodeToAdmitted();
  void setReturnCodeToFail();
  void addCommand(std::shared_ptr<TyCommand> c);
  void setOptimizationName(const std::string &name);
  void addNopPosition(std::shared_ptr<TyPosition> position);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::string module_id;
  std::string function_id;
  std::string opt_name;
  std::string description;
  RETURN_CODE return_code;
  std::vector<std::shared_ptr<TyPosition>> nop_positions;
  std::vector<std::shared_ptr<TyCommand>> commands;
};

void intrude(std::function<void()> func);

} // llvmberry

#endif
