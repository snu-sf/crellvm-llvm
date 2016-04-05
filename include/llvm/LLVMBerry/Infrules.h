#ifndef INFRULES_H
#define INFRULES_H

#include "Structure.h"
#include "llvm/IR/Instructions.h"
#include "cereal/types/memory.hpp"
#include "cereal/archives/json.hpp"

namespace llvmberry{

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

struct TyAddDistSub{
public : 
  TyAddDistSub(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _minusx, std::unique_ptr<TyRegister> _minusy, std::unique_ptr<TyRegister> _w, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyRegister> minusx;
  std::unique_ptr<TyRegister> minusy;
  std::unique_ptr<TyRegister> w;
  std::unique_ptr<TyValue> x;
  std::unique_ptr<TyValue> y;
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

struct TySubConstAdd{
public : 
  TySubConstAdd(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyValue> _x, std::unique_ptr<TyConstInt> _c1, std::unique_ptr<TyConstInt> _c2, std::unique_ptr<TyConstInt> _c3, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyRegister> y;
  std::unique_ptr<TyValue> x;
  std::unique_ptr<TyConstInt> c1;
  std::unique_ptr<TyConstInt> c2;
  std::unique_ptr<TyConstInt> c3;
  std::unique_ptr<TySize> sz;
};

struct TySubConstNot{
public : 
  TySubConstNot(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyValue> _x, std::unique_ptr<TyConstInt> _c1, std::unique_ptr<TyConstInt> _c2, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyRegister> y;
  std::unique_ptr<TyValue> x;
  std::unique_ptr<TyConstInt> c1;
  std::unique_ptr<TyConstInt> c2;
  std::unique_ptr<TySize> sz;
};

struct TySubMone{
public : 
  TySubMone(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyValue> x;
  std::unique_ptr<TySize> sz;
};

struct TySubOnebit{
public : 
  TySubOnebit(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyValue> x;
  std::unique_ptr<TyValue> y;
};

struct TySubRemove2{
public : 
  TySubRemove2(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyValue> _a, std::unique_ptr<TyValue> _b, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyRegister> y;
  std::unique_ptr<TyValue> a;
  std::unique_ptr<TyValue> b;
  std::unique_ptr<TySize> sz;
};

struct TySubSdiv{
public : 
  TySubSdiv(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyValue> _x, std::unique_ptr<TyConstInt> _c, std::unique_ptr<TyConstInt> _cprime, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyRegister> y;
  std::unique_ptr<TyValue> x;
  std::unique_ptr<TyConstInt> c;
  std::unique_ptr<TyConstInt> cprime;
  std::unique_ptr<TySize> sz;
};

struct TySubShl{
public : 
  TySubShl(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyValue> _mx, std::unique_ptr<TyValue> _a, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyValue> x;
  std::unique_ptr<TyRegister> y;
  std::unique_ptr<TyValue> mx;
  std::unique_ptr<TyValue> a;
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

struct TyMulMone{
public : 
  TyMulMone(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyValue> x;
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

struct TyTransitivity {
public:
  TyTransitivity(std::unique_ptr<TyExpr> _e1, std::unique_ptr<TyExpr> _e2,
                 std::unique_ptr<TyExpr> _e3);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::unique_ptr<TyExpr> e1;
  std::unique_ptr<TyExpr> e2;
  std::unique_ptr<TyExpr> e3;
};

struct TyReplaceRhs {
public:
  TyReplaceRhs(std::unique_ptr<TyRegister> _x, std::unique_ptr<TyValue> _y,
               std::unique_ptr<TyExpr> _e1, std::unique_ptr<TyExpr> _e2,
               std::unique_ptr<TyExpr> _e2_p);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::unique_ptr<TyRegister> x;
  std::unique_ptr<TyValue> y;
  std::unique_ptr<TyExpr> e1;
  std::unique_ptr<TyExpr> e2;
  std::unique_ptr<TyExpr> e2_p;
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

struct ConsAddDistSub : public TyInfrule{
public : 
  ConsAddDistSub(std::unique_ptr<TyAddDistSub> _add_dist_sub);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _minusx, std::unique_ptr<TyRegister> _minusy, std::unique_ptr<TyRegister> _w, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyAddDistSub> add_dist_sub;
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

struct ConsSubMone : public TyInfrule{
public : 
  ConsSubMone(std::unique_ptr<TySubMone> _sub_mone);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TySubMone> sub_mone;
};

struct ConsSubOnebit : public TyInfrule{
public : 
  ConsSubOnebit(std::unique_ptr<TySubOnebit> _sub_onebit);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TySubOnebit> sub_onebit;
};

struct ConsSubConstAdd : public TyInfrule{
public : 
  ConsSubConstAdd(std::unique_ptr<TySubConstAdd> _sub_const_add);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyValue> _x, std::unique_ptr<TyConstInt> _c1, std::unique_ptr<TyConstInt> _c2, std::unique_ptr<TyConstInt> _c3, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TySubConstAdd> sub_const_add;
};

struct ConsSubConstNot : public TyInfrule{
public : 
  ConsSubConstNot(std::unique_ptr<TySubConstNot> _sub_const_not);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyValue> _x, std::unique_ptr<TyConstInt> _c1, std::unique_ptr<TyConstInt> _c2, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TySubConstNot> sub_const_not;
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

struct ConsSubRemove2 : public TyInfrule{
public : 
  ConsSubRemove2(std::unique_ptr<TySubRemove2> _sub_remove2);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyValue> _a, std::unique_ptr<TyValue> _b, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TySubRemove2> sub_remove2;
};

struct ConsSubSdiv : public TyInfrule{
public : 
  ConsSubSdiv(std::unique_ptr<TySubSdiv> _sub_sdiv);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyValue> _x, std::unique_ptr<TyConstInt> _c, std::unique_ptr<TyConstInt> _cprime, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TySubSdiv> sub_sdiv;
};

struct ConsSubShl : public TyInfrule{
public : 
  ConsSubShl(std::unique_ptr<TySubShl> _sub_shl);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyValue> _mx, std::unique_ptr<TyValue> _a, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TySubShl> sub_shl;
};

struct ConsMulMone : public TyInfrule{
public : 
  ConsMulMone(std::unique_ptr<TyMulMone> _mul_mone);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyMulMone> mul_mone;
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

struct ConsTransitivity : TyInfrule {
public:
  ConsTransitivity(std::unique_ptr<TyTransitivity> _transitivity);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyExpr> _e1,
                                         std::unique_ptr<TyExpr> _e2,
                                         std::unique_ptr<TyExpr> _e3);

private:
  std::unique_ptr<TyTransitivity> transitivity;
};

struct ConsReplaceRhs : TyInfrule {
public:
  ConsReplaceRhs(std::unique_ptr<TyReplaceRhs> _replace_rhs);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _x,
                                         std::unique_ptr<TyValue> _y,
                                         std::unique_ptr<TyExpr> _e1,
                                         std::unique_ptr<TyExpr> _e2,
                                         std::unique_ptr<TyExpr> _e2_p);

private:
  std::unique_ptr<TyReplaceRhs> replace_rhs;
};

} // llvmberry

#endif
