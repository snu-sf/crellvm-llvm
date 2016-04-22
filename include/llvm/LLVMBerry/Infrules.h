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

struct TyAddCommutativeTgt {
public:
  TyAddCommutativeTgt(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x,
                   std::unique_ptr<TyValue> _y, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyValue> x;
  std::unique_ptr<TyValue> y;
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
  TyAddDistSub(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _minusx, std::unique_ptr<TyValue> _minusy, std::unique_ptr<TyRegister> _w, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyRegister> minusx;
  std::unique_ptr<TyValue> minusy;
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

struct TyAddOrAnd{
public : 
  TyAddOrAnd(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _a, std::unique_ptr<TyValue> _b, std::unique_ptr<TyRegister> _x, std::unique_ptr<TyRegister> _y, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyValue> a;
  std::unique_ptr<TyValue> b;
  std::unique_ptr<TyRegister> x;
  std::unique_ptr<TyRegister> y;
  std::unique_ptr<TySize> sz;
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

struct TyAddSextSext{
public : 
  TyAddSextSext(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _xprime, std::unique_ptr<TyValue> _x, std::unique_ptr<TyRegister> _yprime, std::unique_ptr<TyValue> _y, std::unique_ptr<TyRegister> _zprime, std::unique_ptr<TySize> _from_sz, std::unique_ptr<TySize> _to_sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyRegister> xprime;
  std::unique_ptr<TyValue> x;
  std::unique_ptr<TyRegister> yprime;
  std::unique_ptr<TyValue> y;
  std::unique_ptr<TyRegister> zprime;
  std::unique_ptr<TySize> from_sz;
  std::unique_ptr<TySize> to_sz;
};

struct TyAddXorAnd{
public : 
  TyAddXorAnd(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _a, std::unique_ptr<TyValue> _b, std::unique_ptr<TyRegister> _x, std::unique_ptr<TyRegister> _y, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyValue> a;
  std::unique_ptr<TyValue> b;
  std::unique_ptr<TyRegister> x;
  std::unique_ptr<TyRegister> y;
  std::unique_ptr<TySize> sz;
};

struct TyAndCommutative{
public : 
  TyAndCommutative(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyValue> x;
  std::unique_ptr<TyValue> y;
  std::unique_ptr<TySize> sz;
};

struct TyAndDeMorgan{
public : 
  TyAndDeMorgan(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _x, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyRegister> _zprime, std::unique_ptr<TyValue> _a, std::unique_ptr<TyValue> _b, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyRegister> x;
  std::unique_ptr<TyRegister> y;
  std::unique_ptr<TyRegister> zprime;
  std::unique_ptr<TyValue> a;
  std::unique_ptr<TyValue> b;
  std::unique_ptr<TySize> sz;
};

struct TyBopDistributiveOverSelectinst{
public : 
  TyBopDistributiveOverSelectinst(TyBop _opcode, std::unique_ptr<TyRegister> _r, std::unique_ptr<TyRegister> _s, std::unique_ptr<TyRegister> _tprime, std::unique_ptr<TyRegister> _t0, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y, std::unique_ptr<TyValue> _z, std::unique_ptr<TyValue> _c, std::unique_ptr<TySize> _bopsz, std::unique_ptr<TyValueType> _selty);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  TyBop opcode;
  std::unique_ptr<TyRegister> r;
  std::unique_ptr<TyRegister> s;
  std::unique_ptr<TyRegister> tprime;
  std::unique_ptr<TyRegister> t0;
  std::unique_ptr<TyValue> x;
  std::unique_ptr<TyValue> y;
  std::unique_ptr<TyValue> z;
  std::unique_ptr<TyValue> c;
  std::unique_ptr<TySize> bopsz;
  std::unique_ptr<TyValueType> selty;
};

struct TyBopDistributiveOverSelectinst2{
public : 
  TyBopDistributiveOverSelectinst2(TyBop _opcode, std::unique_ptr<TyRegister> _r, std::unique_ptr<TyRegister> _s, std::unique_ptr<TyRegister> _tprime, std::unique_ptr<TyRegister> _t0, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y, std::unique_ptr<TyValue> _z, std::unique_ptr<TyValue> _c, std::unique_ptr<TySize> _bopsz, std::unique_ptr<TyValueType> _selty);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  TyBop opcode;
  std::unique_ptr<TyRegister> r;
  std::unique_ptr<TyRegister> s;
  std::unique_ptr<TyRegister> tprime;
  std::unique_ptr<TyRegister> t0;
  std::unique_ptr<TyValue> x;
  std::unique_ptr<TyValue> y;
  std::unique_ptr<TyValue> z;
  std::unique_ptr<TyValue> c;
  std::unique_ptr<TySize> bopsz;
  std::unique_ptr<TyValueType> selty;
};

struct TyFaddCommutativeTgt{
public : 
  TyFaddCommutativeTgt(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y, TyFloatType _fty);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyValue> x;
  std::unique_ptr<TyValue> y;
  TyFloatType fty;
};

struct TyFbopDistributiveOverSelectinst{
public : 
  TyFbopDistributiveOverSelectinst(TyFbop _fopcode, std::unique_ptr<TyRegister> _r, std::unique_ptr<TyRegister> _s, std::unique_ptr<TyRegister> _tprime, std::unique_ptr<TyRegister> _t0, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y, std::unique_ptr<TyValue> _z, std::unique_ptr<TyValue> _c, TyFloatType _fbopty, std::unique_ptr<TyValueType> _selty);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  TyFbop fopcode;
  std::unique_ptr<TyRegister> r;
  std::unique_ptr<TyRegister> s;
  std::unique_ptr<TyRegister> tprime;
  std::unique_ptr<TyRegister> t0;
  std::unique_ptr<TyValue> x;
  std::unique_ptr<TyValue> y;
  std::unique_ptr<TyValue> z;
  std::unique_ptr<TyValue> c;
  TyFloatType fbopty;
  std::unique_ptr<TyValueType> selty;
};

struct TyFbopDistributiveOverSelectinst2{
public : 
  TyFbopDistributiveOverSelectinst2(TyFbop _fopcode, std::unique_ptr<TyRegister> _r, std::unique_ptr<TyRegister> _s, std::unique_ptr<TyRegister> _tprime, std::unique_ptr<TyRegister> _t0, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y, std::unique_ptr<TyValue> _z, std::unique_ptr<TyValue> _c, TyFloatType _fbopty, std::unique_ptr<TyValueType> _selty);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  TyFbop fopcode;
  std::unique_ptr<TyRegister> r;
  std::unique_ptr<TyRegister> s;
  std::unique_ptr<TyRegister> tprime;
  std::unique_ptr<TyRegister> t0;
  std::unique_ptr<TyValue> x;
  std::unique_ptr<TyValue> y;
  std::unique_ptr<TyValue> z;
  std::unique_ptr<TyValue> c;
  TyFloatType fbopty;
  std::unique_ptr<TyValueType> selty;
};

struct TySdivMone{
public : 
  TySdivMone(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyValue> x;
  std::unique_ptr<TySize> sz;
};

struct TySdivSubSrem{
public : 
  TySdivSubSrem(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _b, std::unique_ptr<TyRegister> _a, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyRegister> b;
  std::unique_ptr<TyRegister> a;
  std::unique_ptr<TyValue> x;
  std::unique_ptr<TyValue> y;
  std::unique_ptr<TySize> sz;
};

struct TyUdivSubUrem{
public : 
  TyUdivSubUrem(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _b, std::unique_ptr<TyRegister> _a, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyRegister> b;
  std::unique_ptr<TyRegister> a;
  std::unique_ptr<TyValue> x;
  std::unique_ptr<TyValue> y;
  std::unique_ptr<TySize> sz;
};

struct TyMulShl{
public : 
  TyMulShl(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _a, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyRegister> y;
  std::unique_ptr<TyValue> x;
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

struct TyMulCommutative{
public : 
  TyMulCommutative(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyValue> x;
  std::unique_ptr<TyValue> y;
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

struct TyOrCommutative{
public : 
  TyOrCommutative(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyValue> x;
  std::unique_ptr<TyValue> y;
  std::unique_ptr<TySize> sz;
};

struct TyOrOr{
public : 
  TyOrOr(std::unique_ptr<TyValue> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y, std::unique_ptr<TyValue> _a, std::unique_ptr<TyValue> _b, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyValue> z;
  std::unique_ptr<TyValue> x;
  std::unique_ptr<TyValue> y;
  std::unique_ptr<TyValue> a;
  std::unique_ptr<TyValue> b;
  std::unique_ptr<TySize> sz;
};

struct TyOrOr2{
public : 
  TyOrOr2(std::unique_ptr<TyValue> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y, std::unique_ptr<TyValue> _yprime, std::unique_ptr<TyValue> _a, std::unique_ptr<TyValue> _b, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyValue> z;
  std::unique_ptr<TyValue> x;
  std::unique_ptr<TyValue> y;
  std::unique_ptr<TyValue> yprime;
  std::unique_ptr<TyValue> a;
  std::unique_ptr<TyValue> b;
  std::unique_ptr<TySize> sz;
};

struct TyOrXor{
public : 
  TyOrXor(std::unique_ptr<TyValue> _w, std::unique_ptr<TyValue> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y, std::unique_ptr<TyValue> _a, std::unique_ptr<TyValue> _b, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyValue> w;
  std::unique_ptr<TyValue> z;
  std::unique_ptr<TyValue> x;
  std::unique_ptr<TyValue> y;
  std::unique_ptr<TyValue> a;
  std::unique_ptr<TyValue> b;
  std::unique_ptr<TySize> sz;
};

struct TyOrXor2{
public : 
  TyOrXor2(std::unique_ptr<TyValue> _z, std::unique_ptr<TyValue> _x1, std::unique_ptr<TyValue> _y1, std::unique_ptr<TyValue> _x2, std::unique_ptr<TyValue> _y2, std::unique_ptr<TyValue> _a, std::unique_ptr<TyValue> _b, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyValue> z;
  std::unique_ptr<TyValue> x1;
  std::unique_ptr<TyValue> y1;
  std::unique_ptr<TyValue> x2;
  std::unique_ptr<TyValue> y2;
  std::unique_ptr<TyValue> a;
  std::unique_ptr<TyValue> b;
  std::unique_ptr<TySize> sz;
};

struct TyRemNeg{
public : 
  TyRemNeg(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _my, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyValue> my;
  std::unique_ptr<TyValue> x;
  std::unique_ptr<TyValue> y;
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

struct TySubOrXor{
public : 
  TySubOrXor(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _a, std::unique_ptr<TyValue> _b, std::unique_ptr<TyRegister> _x, std::unique_ptr<TyRegister> _y, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyValue> a;
  std::unique_ptr<TyValue> b;
  std::unique_ptr<TyRegister> x;
  std::unique_ptr<TyRegister> y;
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

struct TySubSub{
public : 
  TySubSub(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y, std::unique_ptr<TyValue> _w, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyValue> x;
  std::unique_ptr<TyValue> y;
  std::unique_ptr<TyValue> w;
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

struct TyTransitivityTgt {
public:
  TyTransitivityTgt(std::unique_ptr<TyExpr> _e1,
                    std::unique_ptr<TyExpr> _e2,
                    std::unique_ptr<TyExpr> _e3);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::unique_ptr<TyExpr> e1;
  std::unique_ptr<TyExpr> e2;
  std::unique_ptr<TyExpr> e3;
};

struct TyReplaceRhs {
public:
  TyReplaceRhs(std::unique_ptr<TyRegister> _x,
               std::unique_ptr<TyValue> _y,
               std::unique_ptr<TyExpr> _e1,
               std::unique_ptr<TyExpr> _e2,
               std::unique_ptr<TyExpr> _e2_p);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::unique_ptr<TyRegister> x;
  std::unique_ptr<TyValue> y;
  std::unique_ptr<TyExpr> e1;
  std::unique_ptr<TyExpr> e2;
  std::unique_ptr<TyExpr> e2_p;
};

struct TyReplaceRhsOpt {
public:
  TyReplaceRhsOpt(std::unique_ptr<TyRegister> _x,
                  std::unique_ptr<TyValue> _y,
                  std::unique_ptr<TyExpr> _e1,
                  std::unique_ptr<TyExpr> _e2,
                  std::unique_ptr<TyExpr> _e2_p);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::unique_ptr<TyRegister> x;
  std::unique_ptr<TyValue> y;
  std::unique_ptr<TyExpr> e1;
  std::unique_ptr<TyExpr> e2;
  std::unique_ptr<TyExpr> e2_p;
};

struct TyIntroGhost{
public :
  TyIntroGhost(std::unique_ptr<TyValue> _x, std::unique_ptr<TyRegister> _g);
  void serialize(cereal::JSONOutputArchive& archive) const;

private :
  std::unique_ptr<TyValue> x;
  std::unique_ptr<TyRegister> g;
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

struct TyUdivZext{
public : 
  TyUdivZext(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _x, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyRegister> _k, std::unique_ptr<TyValue> _a, std::unique_ptr<TyValue> _b, std::unique_ptr<TySize> _sz1, std::unique_ptr<TySize> _sz2);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyRegister> x;
  std::unique_ptr<TyRegister> y;
  std::unique_ptr<TyRegister> k;
  std::unique_ptr<TyValue> a;
  std::unique_ptr<TyValue> b;
  std::unique_ptr<TySize> sz1;
  std::unique_ptr<TySize> sz2;
};

struct TyUremZext{
public : 
  TyUremZext(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _x, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyRegister> _k, std::unique_ptr<TyValue> _a, std::unique_ptr<TyValue> _b, std::unique_ptr<TySize> _sz1, std::unique_ptr<TySize> _sz2);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyRegister> x;
  std::unique_ptr<TyRegister> y;
  std::unique_ptr<TyRegister> k;
  std::unique_ptr<TyValue> a;
  std::unique_ptr<TyValue> b;
  std::unique_ptr<TySize> sz1;
  std::unique_ptr<TySize> sz2;
};

struct TyXorCommutative{
public : 
  TyXorCommutative(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyValue> x;
  std::unique_ptr<TyValue> y;
  std::unique_ptr<TySize> sz;
};

struct TyXorCommutativeTgt{
public : 
  TyXorCommutativeTgt(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyRegister> z;
  std::unique_ptr<TyValue> x;
  std::unique_ptr<TyValue> y;
  std::unique_ptr<TySize> sz;
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
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _minusx, std::unique_ptr<TyValue> _minusy, std::unique_ptr<TyRegister> _w, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y, std::unique_ptr<TySize> _sz);
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

struct ConsAddSextSext : public TyInfrule{
public : 
  ConsAddSextSext(std::unique_ptr<TyAddSextSext> _add_sext_sext);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _xprime, std::unique_ptr<TyValue> _x, std::unique_ptr<TyRegister> _yprime, std::unique_ptr<TyValue> _y, std::unique_ptr<TyRegister> _zprime, std::unique_ptr<TySize> _from_sz, std::unique_ptr<TySize> _to_sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyAddSextSext> add_sext_sext;
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

struct ConsAddCommutativeTgt : TyInfrule {
public:
  ConsAddCommutativeTgt(std::unique_ptr<TyAddCommutativeTgt> _add_comm);
  void serialize(cereal::JSONOutputArchive &archive) const;
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z,
                                         std::unique_ptr<TyValue> _x,
                                         std::unique_ptr<TyValue> _y,
                                         std::unique_ptr<TySize> _sz);

private:
  std::unique_ptr<TyAddCommutativeTgt> add_commutative_tgt;
};

struct ConsAddOrAnd : public TyInfrule{
public : 
  ConsAddOrAnd(std::unique_ptr<TyAddOrAnd> _add_or_and);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _a, std::unique_ptr<TyValue> _b, std::unique_ptr<TyRegister> _x, std::unique_ptr<TyRegister> _y, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyAddOrAnd> add_or_and;
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

struct ConsAddXorAnd : public TyInfrule{
public : 
  ConsAddXorAnd(std::unique_ptr<TyAddXorAnd> _add_xor_and);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _a, std::unique_ptr<TyValue> _b, std::unique_ptr<TyRegister> _x, std::unique_ptr<TyRegister> _y, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyAddXorAnd> add_xor_and;
};

struct ConsAndCommutative : public TyInfrule{
public : 
  ConsAndCommutative(std::unique_ptr<TyAndCommutative> _and_commutative);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyAndCommutative> and_commutative;
};

struct ConsAndDeMorgan : public TyInfrule{
public : 
  ConsAndDeMorgan(std::unique_ptr<TyAndDeMorgan> _and_de_morgan);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _x, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyRegister> _zprime, std::unique_ptr<TyValue> _a, std::unique_ptr<TyValue> _b, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyAndDeMorgan> and_de_morgan;
};

struct ConsBopDistributiveOverSelectinst : public TyInfrule{
public : 
  ConsBopDistributiveOverSelectinst(std::unique_ptr<TyBopDistributiveOverSelectinst> _bop_distributive_over_selectinst);
  static std::unique_ptr<TyInfrule> make(TyBop _opcode, std::unique_ptr<TyRegister> _r, std::unique_ptr<TyRegister> _s, std::unique_ptr<TyRegister> _tprime, std::unique_ptr<TyRegister> _t0, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y, std::unique_ptr<TyValue> _z, std::unique_ptr<TyValue> _c, std::unique_ptr<TySize> _bopsz, std::unique_ptr<TyValueType> _selty);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyBopDistributiveOverSelectinst> bop_distributive_over_selectinst;
};

struct ConsBopDistributiveOverSelectinst2 : public TyInfrule{
public : 
  ConsBopDistributiveOverSelectinst2(std::unique_ptr<TyBopDistributiveOverSelectinst2> _bop_distributive_over_selectinst2);
  static std::unique_ptr<TyInfrule> make(TyBop _opcode, std::unique_ptr<TyRegister> _r, std::unique_ptr<TyRegister> _s, std::unique_ptr<TyRegister> _tprime, std::unique_ptr<TyRegister> _t0, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y, std::unique_ptr<TyValue> _z, std::unique_ptr<TyValue> _c, std::unique_ptr<TySize> _bopsz, std::unique_ptr<TyValueType> _selty);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyBopDistributiveOverSelectinst2> bop_distributive_over_selectinst2;
};

struct ConsFaddCommutativeTgt : public TyInfrule{
public : 
  ConsFaddCommutativeTgt(std::unique_ptr<TyFaddCommutativeTgt> _fadd_commutative_tgt);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y, TyFloatType _fty);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyFaddCommutativeTgt> fadd_commutative_tgt;
};

struct ConsFbopDistributiveOverSelectinst : public TyInfrule{
public : 
  ConsFbopDistributiveOverSelectinst(std::unique_ptr<TyFbopDistributiveOverSelectinst> _fbop_distributive_over_selectinst);
  static std::unique_ptr<TyInfrule> make(TyFbop _fopcode, std::unique_ptr<TyRegister> _r, std::unique_ptr<TyRegister> _s, std::unique_ptr<TyRegister> _tprime, std::unique_ptr<TyRegister> _t0, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y, std::unique_ptr<TyValue> _z, std::unique_ptr<TyValue> _c, TyFloatType _fbopty, std::unique_ptr<TyValueType> _selty);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyFbopDistributiveOverSelectinst> fbop_distributive_over_selectinst;
};

struct ConsFbopDistributiveOverSelectinst2 : public TyInfrule{
public : 
  ConsFbopDistributiveOverSelectinst2(std::unique_ptr<TyFbopDistributiveOverSelectinst2> _fbop_distributive_over_selectinst2);
  static std::unique_ptr<TyInfrule> make(TyFbop _fopcode, std::unique_ptr<TyRegister> _r, std::unique_ptr<TyRegister> _s, std::unique_ptr<TyRegister> _tprime, std::unique_ptr<TyRegister> _t0, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y, std::unique_ptr<TyValue> _z, std::unique_ptr<TyValue> _c, TyFloatType _fbopty, std::unique_ptr<TyValueType> _selty);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyFbopDistributiveOverSelectinst2> fbop_distributive_over_selectinst2;
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

struct ConsMulCommutative : public TyInfrule{
public : 
  ConsMulCommutative(std::unique_ptr<TyMulCommutative> _mul_commutative);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyMulCommutative> mul_commutative;
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

struct ConsSdivMone : public TyInfrule{
public : 
  ConsSdivMone(std::unique_ptr<TySdivMone> _div_mone);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TySdivMone> div_mone;
};

struct ConsMulShl : public TyInfrule{
public : 
  ConsMulShl(std::unique_ptr<TyMulShl> _mul_shl);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _a, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyMulShl> mul_shl;
};

struct ConsOrCommutative : public TyInfrule{
public : 
  ConsOrCommutative(std::unique_ptr<TyOrCommutative> _or_commutative);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyOrCommutative> or_commutative;
};

struct ConsOrOr : public TyInfrule{
public : 
  ConsOrOr(std::unique_ptr<TyOrOr> _or_or);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyValue> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y, std::unique_ptr<TyValue> _a, std::unique_ptr<TyValue> _b, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyOrOr> or_or;
};

struct ConsOrOr2 : public TyInfrule{
public : 
  ConsOrOr2(std::unique_ptr<TyOrOr2> _or_or2);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyValue> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y, std::unique_ptr<TyValue> _yprime, std::unique_ptr<TyValue> _a, std::unique_ptr<TyValue> _b, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyOrOr2> or_or2;
};

struct ConsOrXor : public TyInfrule{
public : 
  ConsOrXor(std::unique_ptr<TyOrXor> _or_xor);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyValue> _w, std::unique_ptr<TyValue> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y, std::unique_ptr<TyValue> _a, std::unique_ptr<TyValue> _b, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyOrXor> or_xor;
};

struct ConsOrXor2 : public TyInfrule{
public : 
  ConsOrXor2(std::unique_ptr<TyOrXor2> _or_xor2);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyValue> _z, std::unique_ptr<TyValue> _x1, std::unique_ptr<TyValue> _y1, std::unique_ptr<TyValue> _x2, std::unique_ptr<TyValue> _y2, std::unique_ptr<TyValue> _a, std::unique_ptr<TyValue> _b, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyOrXor2> or_xor2;
};

struct ConsRemNeg : public TyInfrule{
public : 
  ConsRemNeg(std::unique_ptr<TyRemNeg> _rem_neg);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _my, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyRemNeg> rem_neg;
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

struct ConsSubOrXor : public TyInfrule{
public : 
  ConsSubOrXor(std::unique_ptr<TySubOrXor> _sub_or_xor);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _a, std::unique_ptr<TyValue> _b, std::unique_ptr<TyRegister> _x, std::unique_ptr<TyRegister> _y, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TySubOrXor> sub_or_xor;
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

struct ConsSubSdiv : public TyInfrule{
public : 
  ConsSubSdiv(std::unique_ptr<TySubSdiv> _sub_sdiv);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyValue> _x, std::unique_ptr<TyConstInt> _c, std::unique_ptr<TyConstInt> _cprime, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TySubSdiv> sub_sdiv;
};

struct ConsSubSub : public TyInfrule{
public : 
  ConsSubSub(std::unique_ptr<TySubSub> _sub_sub);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y, std::unique_ptr<TyValue> _w, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TySubSub> sub_sub;
};

struct ConsSubShl : public TyInfrule{
public : 
  ConsSubShl(std::unique_ptr<TySubShl> _sub_shl);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyValue> _mx, std::unique_ptr<TyValue> _a, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TySubShl> sub_shl;
};

struct ConsSdivSubSrem : public TyInfrule{
public : 
  ConsSdivSubSrem(std::unique_ptr<TySdivSubSrem> _div_sub_srem);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _b, std::unique_ptr<TyRegister> _a, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TySdivSubSrem> div_sub_srem;
};
struct ConsUdivSubUrem : public TyInfrule{
public : 
  ConsUdivSubUrem(std::unique_ptr<TyUdivSubUrem> _div_sub_urem);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _b, std::unique_ptr<TyRegister> _a, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyUdivSubUrem> div_sub_urem;
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

struct ConsTransitivityTgt : TyInfrule {
public:
  ConsTransitivityTgt(std::unique_ptr<TyTransitivityTgt> _transitivity_tgt);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyExpr> _e1,
                                         std::unique_ptr<TyExpr> _e2,
                                         std::unique_ptr<TyExpr> _e3);

private:
  std::unique_ptr<TyTransitivityTgt> transitivity_tgt;
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


struct ConsReplaceRhsOpt : TyInfrule {
public:
  ConsReplaceRhsOpt(std::unique_ptr<TyReplaceRhsOpt> _replace_rhs_opt);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _x,
                                         std::unique_ptr<TyValue> _y,
                                         std::unique_ptr<TyExpr> _e1,
                                         std::unique_ptr<TyExpr> _e2,
                                         std::unique_ptr<TyExpr> _e2_p);

private:
  std::unique_ptr<TyReplaceRhsOpt> replace_rhs_opt;
};

struct ConsUdivZext : public TyInfrule{
public : 
  ConsUdivZext(std::unique_ptr<TyUdivZext> _udiv_zext);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _x, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyRegister> _k, std::unique_ptr<TyValue> _a, std::unique_ptr<TyValue> _b, std::unique_ptr<TySize> _sz1, std::unique_ptr<TySize> _sz2);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyUdivZext> udiv_zext;
};

struct ConsUremZext : public TyInfrule{
public : 
  ConsUremZext(std::unique_ptr<TyUremZext> _urem_zext);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _x, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyRegister> _k, std::unique_ptr<TyValue> _a, std::unique_ptr<TyValue> _b, std::unique_ptr<TySize> _sz1, std::unique_ptr<TySize> _sz2);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyUremZext> urem_zext;
};


struct ConsIntroGhost : public TyInfrule{
public :
  ConsIntroGhost(std::unique_ptr<TyIntroGhost> _intro_ghost);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyValue> _x, std::unique_ptr<TyRegister> _g);
  void serialize(cereal::JSONOutputArchive& archive) const;

private :
  std::unique_ptr<TyIntroGhost> intro_ghost;
};

struct ConsXorCommutative : public TyInfrule{
public : 
  ConsXorCommutative(std::unique_ptr<TyXorCommutative> _xor_commutative);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyXorCommutative> xor_commutative;
};

struct ConsXorCommutativeTgt : public TyInfrule{
public : 
  ConsXorCommutativeTgt(std::unique_ptr<TyXorCommutativeTgt> _xor_commutative_tgt);
  static std::unique_ptr<TyInfrule> make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y, std::unique_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::unique_ptr<TyXorCommutativeTgt> xor_commutative_tgt;
};




} // llvmberry

#endif
