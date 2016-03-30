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

namespace llvmberry {

enum TyScope { Source = 0, Target };

std::string getBasicBlockIndex(const llvm::BasicBlock *block);
std::string getVariable(const llvm::Value &value);
bool name_instructions(llvm::Function &F);
void generateHintForNegValue(llvm::Value *V, llvm::BinaryOperator &I);
void generateHintForAddSelectZero(llvm::BinaryOperator *Z, 
        llvm::BinaryOperator *X, 
        llvm::SelectInst *Y, 
        bool needs_commutativity,
        bool is_leftform);

/* position */

struct TyPositionPhinode {
public:
  TyPositionPhinode(std::string _block_name, std::string _prev_block_name);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::string block_name;
  std::string prev_block_name;
};

struct TyPositionCommand {
public:
  TyPositionCommand(enum TyScope _scope, std::string _register_name);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  enum TyScope scope;
  std::string register_name;
};

struct TyPosition{
public:
  virtual void serialize(cereal::JSONOutputArchive &archive) const = 0;
};

struct ConsPhinode : public TyPosition {
public:
  ConsPhinode(std::unique_ptr<TyPositionPhinode> _position_phinode);
  ConsPhinode(std::string _block_name, std::string _prev_block_name);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::unique_ptr<TyPositionPhinode> position_phinode;
  int n;
};

struct ConsCommand : public TyPosition {
public:
  ConsCommand(std::unique_ptr<TyPositionCommand> _position_command);
  ConsCommand(enum TyScope _scope, std::string _register_name);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::unique_ptr<TyPosition> make(enum TyScope _scope,
                                          std::string _register_name);

private:
  std::unique_ptr<TyPositionCommand> position_command;
};

// abstract
struct TyNopPosition {
public:
  virtual void serialize(cereal::JSONOutputArchive &archive) const = 0;
};

struct ConsPhinodeCurrentBlockName : public TyNopPosition {
public:
  ConsPhinodeCurrentBlockName(std::string _block_name);
  void serialize(cereal::JSONOutputArchive &archive) const;
  static std::unique_ptr<TyNopPosition> make(std::string _block_name);

private:
  std::string block_name;
};

struct ConsCommandRegisterName : public TyNopPosition {
public:
  ConsCommandRegisterName(std::string _register_name);
  void serialize(cereal::JSONOutputArchive &archive) const;
  static std::unique_ptr<TyNopPosition> make(std::string _register_name);

private:
  std::string register_name;
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

// constant

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

struct TyConstInt {
public:
  TyConstInt(int _int_value, std::unique_ptr<TyIntType> _int_type);
  TyConstInt(int _int_value, int _bitwidth);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::unique_ptr<TyConstInt> make(int _int_value, int _bitwidth);

private:
  int int_value;
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
  ConsConstInt(int _int_value, int _value);
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

/* inference rule */

struct TyAddAssociative {
public:
  TyAddAssociative(std::unique_ptr<TyRegister> _x,
                   std::unique_ptr<TyRegister> _y,
                   std::unique_ptr<TyRegister> _z,
                   std::unique_ptr<TyConstInt> _c1,
                   std::unique_ptr<TyConstInt> _c2,
                   std::unique_ptr<TyConstInt> _c3,
                   std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::unique_ptr<TyRegister> x;
  std::unique_ptr<TyRegister> y;
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyConstInt> c1;
  std::unique_ptr<TyConstInt> c2;
  std::unique_ptr<TyConstInt> c3;
  std::unique_ptr<TySize> sz;
};

struct TyAddConstNot{
public : 
  TyAddConstNot(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyValue> _x, std::unique_ptr<TyConstInt> _c1, std::unique_ptr<TyConstInt> _c2, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyRegister> y;
  std::unique_ptr<TyValue> x;
  std::unique_ptr<TyConstInt> c1;
  std::unique_ptr<TyConstInt> c2;
  std::unique_ptr<TySize> sz;
};

struct TyAddMask{
public : 
  TyAddMask(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyRegister> _yprime, std::unique_ptr<TyValue> _x, std::unique_ptr<TyConstInt> _c1, std::unique_ptr<TyConstInt> _c2, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyRegister> y;
  std::unique_ptr<TyRegister> yprime;
  std::unique_ptr<TyValue> x;
  std::unique_ptr<TyConstInt> c1;
  std::unique_ptr<TyConstInt> c2;
  std::unique_ptr<TySize> sz;
};

struct TyAddSelectZero{
public : 
  TyAddSelectZero(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _x, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyValue> _c, std::unique_ptr<TyValue> _n, std::unique_ptr<TyValue> _a, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyRegister> x;
  std::unique_ptr<TyRegister> y;
  std::unique_ptr<TyValue> c;
  std::unique_ptr<TyValue> n;
  std::unique_ptr<TyValue> a;
  std::unique_ptr<TySize> sz;
};

struct TyAddSelectZero2{
public : 
  TyAddSelectZero2(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _x, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyValue> _c, std::unique_ptr<TyValue> _n, std::unique_ptr<TyValue> _a, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyRegister> x;
  std::unique_ptr<TyRegister> y;
  std::unique_ptr<TyValue> c;
  std::unique_ptr<TyValue> n;
  std::unique_ptr<TyValue> a;
  std::unique_ptr<TySize> sz;
};

struct TyAddOnebit{
public : 
  TyAddOnebit(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyValue> x;
  std::unique_ptr<TyValue> y;
};

struct TyAddSub {
public:
  TyAddSub(std::unique_ptr<TyRegister> _minusy, std::unique_ptr<TyRegister> _z,
           std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y,
           std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::unique_ptr<TyRegister> minusy;
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyValue> x;
  std::unique_ptr<TyValue> y;
  std::unique_ptr<TySize> sz;
};

struct TyAddShift {
public:
  TyAddShift(std::unique_ptr<TyRegister> _y, std::unique_ptr<TyValue> v,
             std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::unique_ptr<TyRegister> y;
  std::unique_ptr<TyValue> v;
  std::unique_ptr<TySize> sz;
};

struct TyAddSignbit {
public:
  TyAddSignbit(std::unique_ptr<TyRegister> _x, std::unique_ptr<TyValue> _e1,
               std::unique_ptr<TyValue> _e2, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::unique_ptr<TyRegister> x;
  std::unique_ptr<TyValue> e1;
  std::unique_ptr<TyValue> e2;
  std::unique_ptr<TySize> sz;
};

struct TyAddZextBool{
public : 
  TyAddZextBool(std::unique_ptr<TyRegister> _x, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyValue> _b, std::unique_ptr<TyConstInt> _c, std::unique_ptr<TyConstInt> _cprime, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyRegister> x;
  std::unique_ptr<TyRegister> y;
  std::unique_ptr<TyValue> b;
  std::unique_ptr<TyConstInt> c;
  std::unique_ptr<TyConstInt> cprime;
  std::unique_ptr<TySize> sz;
};

struct TySubAdd {
public:
  TySubAdd(std::unique_ptr<TyRegister> _z,
           std::unique_ptr<TyValue> _my,
           std::unique_ptr<TyRegister> _x,
           std::unique_ptr<TyValue> _y,
           std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyValue> my;
  std::unique_ptr<TyRegister> x;
  std::unique_ptr<TyValue> y;
  std::unique_ptr<TySize> sz;
};

struct TyNegVal {
public:
  TyNegVal(std::unique_ptr<TyConstInt> _c1,
           std::unique_ptr<TyConstInt> _c2,
           std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::unique_ptr<TyConstInt> c1;
  std::unique_ptr<TyConstInt> c2;
  std::unique_ptr<TySize> sz;
};

struct TyMulNeg {
public:
  TyMulNeg(std::unique_ptr<TyRegister> _z,
           std::unique_ptr<TyValue> _mx,
           std::unique_ptr<TyValue> _my,
           std::unique_ptr<TyValue> _x,
           std::unique_ptr<TyValue> _y,
           std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyValue> mx;
  std::unique_ptr<TyValue> my;
  std::unique_ptr<TyValue> x;
  std::unique_ptr<TyValue> y;
  std::unique_ptr<TySize> sz;
};

struct TySubRemove {
public:
  TySubRemove(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _y,
              std::unique_ptr<TyValue> _a, std::unique_ptr<TyValue> _b,
              std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyRegister> y;
  std::unique_ptr<TyValue> a;
  std::unique_ptr<TyValue> b;
  std::unique_ptr<TySize> sz;
};

struct TyMulBool {
public:
  TyMulBool(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _x,
            std::unique_ptr<TyRegister> _y);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyRegister> x;
  std::unique_ptr<TyRegister> y;
};

struct TyAddCommutative {
public:
  TyAddCommutative(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x,
                   std::unique_ptr<TyValue> _y, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyValue> x;
  std::unique_ptr<TyValue> y;
  std::unique_ptr<TySize> sz;
};

struct TyInfrule {
public:
  virtual void serialize(cereal::JSONOutputArchive &archive) const = 0;
};

struct ConsAddAssociative : TyInfrule {
public:
  ConsAddAssociative(std::unique_ptr<TyAddAssociative> _add_associative);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::unique_ptr<TyInfrule>
  make(std::unique_ptr<TyRegister> _x, std::unique_ptr<TyRegister> _y,
       std::unique_ptr<TyRegister> _z, std::unique_ptr<TyConstInt> _c1,
       std::unique_ptr<TyConstInt> _c2, std::unique_ptr<TyConstInt> _c3,
       std::unique_ptr<TySize> _sz);

private:
  std::unique_ptr<TyAddAssociative> add_associative;
};

struct ConsAddConstNot : public TyInfrule{
public : 
  ConsAddConstNot(std::unique_ptr<TyAddConstNot> _add_const_not);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyValue> _x, std::unique_ptr<TyConstInt> _c1, std::unique_ptr<TyConstInt> _c2, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyAddConstNot> add_const_not;
};

struct ConsAddOnebit : public TyInfrule{
public : 
  ConsAddOnebit(std::unique_ptr<TyAddOnebit> _add_onebit);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyAddOnebit> add_onebit;
};

struct ConsAddMask : public TyInfrule{
public : 
  ConsAddMask(std::unique_ptr<TyAddMask> _add_mask);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyRegister> _yprime, std::unique_ptr<TyValue> _x, std::unique_ptr<TyConstInt> _c1, std::unique_ptr<TyConstInt> _c2, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyAddMask> add_mask;
};

struct ConsAddSelectZero : public TyInfrule{
public : 
  ConsAddSelectZero(std::unique_ptr<TyAddSelectZero> _add_select_zero);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _x, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyValue> _c, std::unique_ptr<TyValue> _n, std::unique_ptr<TyValue> _a, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyAddSelectZero> add_select_zero;
};

struct ConsAddSelectZero2 : public TyInfrule{
public : 
  ConsAddSelectZero2(std::unique_ptr<TyAddSelectZero2> _add_select_zero2);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _x, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyValue> _c, std::unique_ptr<TyValue> _n, std::unique_ptr<TyValue> _a, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyAddSelectZero2> add_select_zero2;
};

struct ConsAddSub : TyInfrule {
public:
  ConsAddSub(std::unique_ptr<TyAddSub> _add_sub);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _minusy,
                                         std::unique_ptr<TyRegister> _z,
                                         std::unique_ptr<TyValue> _x,
                                         std::unique_ptr<TyValue> _y,
                                         std::unique_ptr<TySize> _sz);

private:
  std::unique_ptr<TyAddSub> add_sub;
};

struct ConsAddCommutative : TyInfrule {
public:
  ConsAddCommutative(std::unique_ptr<TyAddCommutative> _add_comm);
  void serialize(cereal::JSONOutputArchive &archive) const;
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z,
                                         std::unique_ptr<TyValue> _x,
                                         std::unique_ptr<TyValue> _y,
                                         std::unique_ptr<TySize> _sz);

private:
  std::unique_ptr<TyAddCommutative> add_commutative;
};

struct ConsAddShift : TyInfrule {
public:
  ConsAddShift(std::unique_ptr<TyAddShift> _add_shift);
  void serialize(cereal::JSONOutputArchive &archive) const;
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _y,
                                         std::unique_ptr<TyValue> _v,
                                         std::unique_ptr<TySize> _sz);

private:
  std::unique_ptr<TyAddShift> add_shift;
};

struct ConsNegVal : TyInfrule {
public:
  ConsNegVal(std::unique_ptr<TyNegVal> _neg_val);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::unique_ptr<TyInfrule> make
          (std::unique_ptr<TyConstInt> _c1,
           std::unique_ptr<TyConstInt> _c2,
           std::unique_ptr<TySize> _sz);

private:
  std::unique_ptr<TyNegVal> neg_val;
};

struct ConsAddSignbit : TyInfrule {
public:
  ConsAddSignbit(std::unique_ptr<TyAddSignbit> _add_signbit);
  void serialize(cereal::JSONOutputArchive &archive) const;
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _x,
                                         std::unique_ptr<TyValue> _e1,
                                         std::unique_ptr<TyValue> _e2,
                                         std::unique_ptr<TySize> _sz);

private:
  std::unique_ptr<TyAddSignbit> add_signbit;
};

struct ConsAddZextBool : public TyInfrule{
public : 
  ConsAddZextBool(std::unique_ptr<TyAddZextBool> _add_zext_bool);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _x, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyValue> _b, std::unique_ptr<TyConstInt> _c, std::unique_ptr<TyConstInt> _cprime, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyAddZextBool> add_zext_bool;
};

struct ConsSubAdd : TyInfrule {
public:
  ConsSubAdd(std::unique_ptr<TySubAdd> _sub_add);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z,
                                         std::unique_ptr<TyValue> _my,
                                         std::unique_ptr<TyRegister> _x,
                                         std::unique_ptr<TyValue> _y,
                                         std::unique_ptr<TySize> _sz);

private:
  std::unique_ptr<TySubAdd> sub_add;
};

struct ConsSubRemove : TyInfrule {
public:
  ConsSubRemove(std::unique_ptr<TySubRemove> _sub_remove);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z,
                                         std::unique_ptr<TyRegister> _y,
                                         std::unique_ptr<TyValue> _a,
                                         std::unique_ptr<TyValue> _b,
                                         std::unique_ptr<TySize> _sz);

private:
  std::unique_ptr<TySubRemove> sub_remove;
};

struct ConsMulNeg : TyInfrule {
public:
    ConsMulNeg(std::unique_ptr<TyMulNeg> _mul_neg);
    void serialize(cereal::JSONOutputArchive &archive) const;

    static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z,
                                           std::unique_ptr<TyValue> _mx,
                                           std::unique_ptr<TyValue> _my,
                                           std::unique_ptr<TyValue> _x,
                                           std::unique_ptr<TyValue> _y,
                                           std::unique_ptr<TySize> _sz);

private:
    std::unique_ptr<TyMulNeg> mul_neg;
  };

struct ConsMulBool : TyInfrule {
public:
  ConsMulBool(std::unique_ptr<TyMulBool> _mul_bool);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z,
                                         std::unique_ptr<TyRegister> _x,
                                         std::unique_ptr<TyRegister> _y);

private:
  std::unique_ptr<TyMulBool> mul_bool;
};

/* hint command */

struct TyCommand {
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
  void addSrcNopPosition(std::unique_ptr<TyNopPosition> position);
  void addTgtNopPosition(std::unique_ptr<TyNopPosition> position);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::string module_id;
  std::string function_id;
  std::string opt_name;
  std::vector<std::unique_ptr<TyNopPosition>> src_nop_positions;
  std::vector<std::unique_ptr<TyNopPosition>> tgt_nop_positions;
  std::vector<std::unique_ptr<TyCommand>> commands;
};

// inserting nop
void insertTgtNopAtSrcI(CoreHint &hints, llvm::Instruction *I);
void insertSrcNopAtTgtI(CoreHint &hints, llvm::Instruction *I);

} // llvmberry

#endif
