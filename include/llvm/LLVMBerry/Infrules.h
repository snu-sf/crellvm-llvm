#ifndef INFRULES_H
#define INFRULES_H

#include "Structure.h"
#include "llvm/IR/Instructions.h"
#include "cereal/types/memory.hpp"
#include "cereal/archives/json.hpp"

namespace llvmberry{

struct TyAddAssociative {
public:
  TyAddAssociative(std::shared_ptr<TyRegister> _x,
                   std::shared_ptr<TyRegister> _y,
                   std::shared_ptr<TyRegister> _z,
                   std::shared_ptr<TyConstInt> _c1,
                   std::shared_ptr<TyConstInt> _c2,
                   std::shared_ptr<TyConstInt> _c3,
                   std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> x;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyConstInt> c1;
  std::shared_ptr<TyConstInt> c2;
  std::shared_ptr<TyConstInt> c3;
  std::shared_ptr<TySize> sz;
};

struct TyAddCommutative {
public:
  TyAddCommutative(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x,
                   std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> sz;
};

struct TyAddCommutativeTgt {
public:
  TyAddCommutativeTgt(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x,
                   std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> sz;
};

struct TyAddConstNot{
public : 
  TyAddConstNot(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c1, std::shared_ptr<TyConstInt> _c2, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyConstInt> c1;
  std::shared_ptr<TyConstInt> c2;
  std::shared_ptr<TySize> sz;
};

struct TyAddDistSub{
public : 
  TyAddDistSub(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _minusx, std::shared_ptr<TyValue> _minusy, std::shared_ptr<TyRegister> _w, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> minusx;
  std::shared_ptr<TyValue> minusy;
  std::shared_ptr<TyRegister> w;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> sz;
};

struct TyAddMask{
public : 
  TyAddMask(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyRegister> _yprime, std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c1, std::shared_ptr<TyConstInt> _c2, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyRegister> yprime;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyConstInt> c1;
  std::shared_ptr<TyConstInt> c2;
  std::shared_ptr<TySize> sz;
};

struct TyAddSelectZero{
public : 
  TyAddSelectZero(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _c, std::shared_ptr<TyValue> _n, std::shared_ptr<TyValue> _a, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> x;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyValue> c;
  std::shared_ptr<TyValue> n;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TySize> sz;
};

struct TyAddSelectZero2{
public : 
  TyAddSelectZero2(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _c, std::shared_ptr<TyValue> _n, std::shared_ptr<TyValue> _a, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> x;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyValue> c;
  std::shared_ptr<TyValue> n;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TySize> sz;
};

struct TyAddOnebit{
public : 
  TyAddOnebit(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
};

struct TyAddSub {
public:
  TyAddSub(std::shared_ptr<TyRegister> _minusy, std::shared_ptr<TyRegister> _z,
           std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
           std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> minusy;
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> sz;
};

struct TyAddShift {
public:
  TyAddShift(std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> v,
             std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyValue> v;
  std::shared_ptr<TySize> sz;
};

struct TyAddSignbit {
public:
  TyAddSignbit(std::shared_ptr<TyRegister> _x, std::shared_ptr<TyValue> _e1,
               std::shared_ptr<TyValue> _e2, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> x;
  std::shared_ptr<TyValue> e1;
  std::shared_ptr<TyValue> e2;
  std::shared_ptr<TySize> sz;
};

struct TyAddZextBool{
public : 
  TyAddZextBool(std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _b, std::shared_ptr<TyConstInt> _c, std::shared_ptr<TyConstInt> _cprime, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyRegister> x;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TyConstInt> c;
  std::shared_ptr<TyConstInt> cprime;
  std::shared_ptr<TySize> sz;
};

struct TyAndCommutative{
public : 
  TyAndCommutative(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> sz;
};

struct TyAndDeMorgan{
public : 
  TyAndDeMorgan(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyRegister> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> x;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyRegister> zprime;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> sz;
};

struct TyBopDistributiveOverSelectinst{
public : 
  TyBopDistributiveOverSelectinst(TyBop _opcode, std::shared_ptr<TyRegister> _r, std::shared_ptr<TyRegister> _s, std::shared_ptr<TyRegister> _tprime, std::shared_ptr<TyRegister> _t0, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _c, std::shared_ptr<TySize> _bopsz, std::shared_ptr<TyValueType> _selty);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  TyBop opcode;
  std::shared_ptr<TyRegister> r;
  std::shared_ptr<TyRegister> s;
  std::shared_ptr<TyRegister> tprime;
  std::shared_ptr<TyRegister> t0;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> c;
  std::shared_ptr<TySize> bopsz;
  std::shared_ptr<TyValueType> selty;
};

struct TyBopDistributiveOverSelectinst2{
public : 
  TyBopDistributiveOverSelectinst2(TyBop _opcode, std::shared_ptr<TyRegister> _r, std::shared_ptr<TyRegister> _s, std::shared_ptr<TyRegister> _tprime, std::shared_ptr<TyRegister> _t0, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _c, std::shared_ptr<TySize> _bopsz, std::shared_ptr<TyValueType> _selty);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  TyBop opcode;
  std::shared_ptr<TyRegister> r;
  std::shared_ptr<TyRegister> s;
  std::shared_ptr<TyRegister> tprime;
  std::shared_ptr<TyRegister> t0;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> c;
  std::shared_ptr<TySize> bopsz;
  std::shared_ptr<TyValueType> selty;
};

struct TyFaddCommutativeTgt{
public : 
  TyFaddCommutativeTgt(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, TyFloatType _fty);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  TyFloatType fty;
};

struct TyFbopDistributiveOverSelectinst{
public : 
  TyFbopDistributiveOverSelectinst(TyFbop _fopcode, std::shared_ptr<TyRegister> _r, std::shared_ptr<TyRegister> _s, std::shared_ptr<TyRegister> _tprime, std::shared_ptr<TyRegister> _t0, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _c, TyFloatType _fbopty, std::shared_ptr<TyValueType> _selty);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  TyFbop fopcode;
  std::shared_ptr<TyRegister> r;
  std::shared_ptr<TyRegister> s;
  std::shared_ptr<TyRegister> tprime;
  std::shared_ptr<TyRegister> t0;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> c;
  TyFloatType fbopty;
  std::shared_ptr<TyValueType> selty;
};

struct TyFbopDistributiveOverSelectinst2{
public : 
  TyFbopDistributiveOverSelectinst2(TyFbop _fopcode, std::shared_ptr<TyRegister> _r, std::shared_ptr<TyRegister> _s, std::shared_ptr<TyRegister> _tprime, std::shared_ptr<TyRegister> _t0, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _c, TyFloatType _fbopty, std::shared_ptr<TyValueType> _selty);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  TyFbop fopcode;
  std::shared_ptr<TyRegister> r;
  std::shared_ptr<TyRegister> s;
  std::shared_ptr<TyRegister> tprime;
  std::shared_ptr<TyRegister> t0;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> c;
  TyFloatType fbopty;
  std::shared_ptr<TyValueType> selty;
};

struct TySdivMone{
public : 
  TySdivMone(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TySize> sz;
};

struct TySdivSubSrem{
public : 
  TySdivSubSrem(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _b, std::shared_ptr<TyRegister> _a, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> b;
  std::shared_ptr<TyRegister> a;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> sz;
};

struct TyUdivSubUrem{
public : 
  TyUdivSubUrem(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _b, std::shared_ptr<TyRegister> _a, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> b;
  std::shared_ptr<TyRegister> a;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> sz;
};

struct TyMulShl{
public : 
  TyMulShl(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _a, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TySize> sz;
};

struct TyNegVal {
public:
  TyNegVal(std::shared_ptr<TyConstInt> _c1,
           std::shared_ptr<TyConstInt> _c2,
           std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyConstInt> c1;
  std::shared_ptr<TyConstInt> c2;
  std::shared_ptr<TySize> sz;
};

struct TyMulCommutative{
public : 
  TyMulCommutative(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> sz;
};

struct TyMulMone{
public : 
  TyMulMone(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TySize> sz;
};

struct TyMulNeg {
public:
  TyMulNeg(std::shared_ptr<TyRegister> _z,
           std::shared_ptr<TyValue> _mx,
           std::shared_ptr<TyValue> _my,
           std::shared_ptr<TyValue> _x,
           std::shared_ptr<TyValue> _y,
           std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> mx;
  std::shared_ptr<TyValue> my;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> sz;
};

struct TyOrCommutative{
public : 
  TyOrCommutative(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> sz;
};

struct TyOrOr{
public : 
  TyOrOr(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> sz;
};

struct TyOrOr2{
public : 
  TyOrOr2(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _yprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> yprime;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> sz;
};

struct TyOrXor{
public : 
  TyOrXor(std::shared_ptr<TyValue> _w, std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyValue> w;
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> sz;
};

struct TyOrXor2{
public : 
  TyOrXor2(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x1, std::shared_ptr<TyValue> _y1, std::shared_ptr<TyValue> _x2, std::shared_ptr<TyValue> _y2, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> x1;
  std::shared_ptr<TyValue> y1;
  std::shared_ptr<TyValue> x2;
  std::shared_ptr<TyValue> y2;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> sz;
};

struct TyRemNeg{
public : 
  TyRemNeg(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _my, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> my;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> sz;
};

struct TySubAdd {
public:
  TySubAdd(std::shared_ptr<TyRegister> _z,
           std::shared_ptr<TyValue> _my,
           std::shared_ptr<TyRegister> _x,
           std::shared_ptr<TyValue> _y,
           std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> my;
  std::shared_ptr<TyRegister> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> sz;
};

struct TySubConstAdd{
public : 
  TySubConstAdd(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c1, std::shared_ptr<TyConstInt> _c2, std::shared_ptr<TyConstInt> _c3, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyConstInt> c1;
  std::shared_ptr<TyConstInt> c2;
  std::shared_ptr<TyConstInt> c3;
  std::shared_ptr<TySize> sz;
};

struct TySubConstNot{
public : 
  TySubConstNot(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c1, std::shared_ptr<TyConstInt> _c2, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyConstInt> c1;
  std::shared_ptr<TyConstInt> c2;
  std::shared_ptr<TySize> sz;
};

struct TySubMone{
public : 
  TySubMone(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TySize> sz;
};

struct TySubOnebit{
public : 
  TySubOnebit(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
};

struct TySubSdiv{
public : 
  TySubSdiv(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c, std::shared_ptr<TyConstInt> _cprime, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyConstInt> c;
  std::shared_ptr<TyConstInt> cprime;
  std::shared_ptr<TySize> sz;
};

struct TySubShl{
public : 
  TySubShl(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _mx, std::shared_ptr<TyValue> _a, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyValue> mx;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TySize> sz;
};


struct TySubRemove {
public:
  TySubRemove(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y,
              std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
              std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> sz;
};

struct TyMulBool {
public:
  TyMulBool(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x,
            std::shared_ptr<TyRegister> _y);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> x;
  std::shared_ptr<TyRegister> y;
};

struct TyTransitivity {
public:
  TyTransitivity(std::shared_ptr<TyExpr> _e1, std::shared_ptr<TyExpr> _e2,
                 std::shared_ptr<TyExpr> _e3);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyExpr> e1;
  std::shared_ptr<TyExpr> e2;
  std::shared_ptr<TyExpr> e3;
};

struct TyTransitivityTgt {
public:
  TyTransitivityTgt(std::shared_ptr<TyExpr> _e1,
                    std::shared_ptr<TyExpr> _e2,
                    std::shared_ptr<TyExpr> _e3);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyExpr> e1;
  std::shared_ptr<TyExpr> e2;
  std::shared_ptr<TyExpr> e3;
};

struct TyReplaceRhs {
public:
  TyReplaceRhs(std::shared_ptr<TyRegister> _x,
               std::shared_ptr<TyValue> _y,
               std::shared_ptr<TyExpr> _e1,
               std::shared_ptr<TyExpr> _e2,
               std::shared_ptr<TyExpr> _e2_p);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyExpr> e1;
  std::shared_ptr<TyExpr> e2;
  std::shared_ptr<TyExpr> e2_p;
};

struct TyReplaceRhsOpt {
public:
  TyReplaceRhsOpt(std::shared_ptr<TyRegister> _x,
                  std::shared_ptr<TyValue> _y,
                  std::shared_ptr<TyExpr> _e1,
                  std::shared_ptr<TyExpr> _e2,
                  std::shared_ptr<TyExpr> _e2_p);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyExpr> e1;
  std::shared_ptr<TyExpr> e2;
  std::shared_ptr<TyExpr> e2_p;
};

struct TyIntroGhost{
public :
  TyIntroGhost(std::shared_ptr<TyValue> _x, std::shared_ptr<TyRegister> _g);
  void serialize(cereal::JSONOutputArchive& archive) const;

private :
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyRegister> g;
};

struct ConsAddAssociative : TyInfrule {
public:
  ConsAddAssociative(std::shared_ptr<TyAddAssociative> _add_associative);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y,
       std::shared_ptr<TyRegister> _z, std::shared_ptr<TyConstInt> _c1,
       std::shared_ptr<TyConstInt> _c2, std::shared_ptr<TyConstInt> _c3,
       std::shared_ptr<TySize> _sz);

private:
  std::shared_ptr<TyAddAssociative> add_associative;
};

struct TyUdivZext{
public : 
  TyUdivZext(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyRegister> _k, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz1, std::shared_ptr<TySize> _sz2);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> x;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyRegister> k;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> sz1;
  std::shared_ptr<TySize> sz2;
};

struct TyUremZext{
public : 
  TyUremZext(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyRegister> _k, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz1, std::shared_ptr<TySize> _sz2);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> x;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyRegister> k;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> sz1;
  std::shared_ptr<TySize> sz2;
};

struct TyXorCommutative{
public : 
  TyXorCommutative(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> sz;
};

struct TyXorCommutativeTgt{
public : 
  TyXorCommutativeTgt(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> sz;
};



struct ConsAddConstNot : public TyInfrule{
public : 
  ConsAddConstNot(std::shared_ptr<TyAddConstNot> _add_const_not);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c1, std::shared_ptr<TyConstInt> _c2, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyAddConstNot> add_const_not;
};

struct ConsAddDistSub : public TyInfrule{
public : 
  ConsAddDistSub(std::shared_ptr<TyAddDistSub> _add_dist_sub);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _minusx, std::shared_ptr<TyValue> _minusy, std::shared_ptr<TyRegister> _w, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyAddDistSub> add_dist_sub;
};

struct ConsAddOnebit : public TyInfrule{
public : 
  ConsAddOnebit(std::shared_ptr<TyAddOnebit> _add_onebit);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyAddOnebit> add_onebit;
};

struct ConsAddMask : public TyInfrule{
public : 
  ConsAddMask(std::shared_ptr<TyAddMask> _add_mask);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyRegister> _yprime, std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c1, std::shared_ptr<TyConstInt> _c2, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyAddMask> add_mask;
};

struct ConsAddSelectZero : public TyInfrule{
public : 
  ConsAddSelectZero(std::shared_ptr<TyAddSelectZero> _add_select_zero);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _c, std::shared_ptr<TyValue> _n, std::shared_ptr<TyValue> _a, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyAddSelectZero> add_select_zero;
};

struct ConsAddSelectZero2 : public TyInfrule{
public : 
  ConsAddSelectZero2(std::shared_ptr<TyAddSelectZero2> _add_select_zero2);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _c, std::shared_ptr<TyValue> _n, std::shared_ptr<TyValue> _a, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyAddSelectZero2> add_select_zero2;
};

struct ConsAddSub : TyInfrule {
public:
  ConsAddSub(std::shared_ptr<TyAddSub> _add_sub);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _minusy,
                                         std::shared_ptr<TyRegister> _z,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TySize> _sz);

private:
  std::shared_ptr<TyAddSub> add_sub;
};

struct ConsAddCommutative : TyInfrule {
public:
  ConsAddCommutative(std::shared_ptr<TyAddCommutative> _add_comm);
  void serialize(cereal::JSONOutputArchive &archive) const;
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TySize> _sz);

private:
  std::shared_ptr<TyAddCommutative> add_commutative;
};

struct ConsAddCommutativeTgt : TyInfrule {
public:
  ConsAddCommutativeTgt(std::shared_ptr<TyAddCommutativeTgt> _add_comm);
  void serialize(cereal::JSONOutputArchive &archive) const;
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TySize> _sz);

private:
  std::shared_ptr<TyAddCommutativeTgt> add_commutative_tgt;
};

struct ConsAddShift : TyInfrule {
public:
  ConsAddShift(std::shared_ptr<TyAddShift> _add_shift);
  void serialize(cereal::JSONOutputArchive &archive) const;
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _y,
                                         std::shared_ptr<TyValue> _v,
                                         std::shared_ptr<TySize> _sz);

private:
  std::shared_ptr<TyAddShift> add_shift;
};

struct ConsAndCommutative : public TyInfrule{
public : 
  ConsAndCommutative(std::shared_ptr<TyAndCommutative> _and_commutative);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyAndCommutative> and_commutative;
};

struct ConsAndDeMorgan : public TyInfrule{
public : 
  ConsAndDeMorgan(std::shared_ptr<TyAndDeMorgan> _and_de_morgan);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyRegister> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyAndDeMorgan> and_de_morgan;
};

struct ConsBopDistributiveOverSelectinst : public TyInfrule{
public : 
  ConsBopDistributiveOverSelectinst(std::shared_ptr<TyBopDistributiveOverSelectinst> _bop_distributive_over_selectinst);
  static std::shared_ptr<TyInfrule> make(TyBop _opcode, std::shared_ptr<TyRegister> _r, std::shared_ptr<TyRegister> _s, std::shared_ptr<TyRegister> _tprime, std::shared_ptr<TyRegister> _t0, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _c, std::shared_ptr<TySize> _bopsz, std::shared_ptr<TyValueType> _selty);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyBopDistributiveOverSelectinst> bop_distributive_over_selectinst;
};

struct ConsBopDistributiveOverSelectinst2 : public TyInfrule{
public : 
  ConsBopDistributiveOverSelectinst2(std::shared_ptr<TyBopDistributiveOverSelectinst2> _bop_distributive_over_selectinst2);
  static std::shared_ptr<TyInfrule> make(TyBop _opcode, std::shared_ptr<TyRegister> _r, std::shared_ptr<TyRegister> _s, std::shared_ptr<TyRegister> _tprime, std::shared_ptr<TyRegister> _t0, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _c, std::shared_ptr<TySize> _bopsz, std::shared_ptr<TyValueType> _selty);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyBopDistributiveOverSelectinst2> bop_distributive_over_selectinst2;
};

struct ConsFaddCommutativeTgt : public TyInfrule{
public : 
  ConsFaddCommutativeTgt(std::shared_ptr<TyFaddCommutativeTgt> _fadd_commutative_tgt);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, TyFloatType _fty);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyFaddCommutativeTgt> fadd_commutative_tgt;
};

struct ConsFbopDistributiveOverSelectinst : public TyInfrule{
public : 
  ConsFbopDistributiveOverSelectinst(std::shared_ptr<TyFbopDistributiveOverSelectinst> _fbop_distributive_over_selectinst);
  static std::shared_ptr<TyInfrule> make(TyFbop _fopcode, std::shared_ptr<TyRegister> _r, std::shared_ptr<TyRegister> _s, std::shared_ptr<TyRegister> _tprime, std::shared_ptr<TyRegister> _t0, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _c, TyFloatType _fbopty, std::shared_ptr<TyValueType> _selty);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyFbopDistributiveOverSelectinst> fbop_distributive_over_selectinst;
};

struct ConsFbopDistributiveOverSelectinst2 : public TyInfrule{
public : 
  ConsFbopDistributiveOverSelectinst2(std::shared_ptr<TyFbopDistributiveOverSelectinst2> _fbop_distributive_over_selectinst2);
  static std::shared_ptr<TyInfrule> make(TyFbop _fopcode, std::shared_ptr<TyRegister> _r, std::shared_ptr<TyRegister> _s, std::shared_ptr<TyRegister> _tprime, std::shared_ptr<TyRegister> _t0, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _c, TyFloatType _fbopty, std::shared_ptr<TyValueType> _selty);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyFbopDistributiveOverSelectinst2> fbop_distributive_over_selectinst2;
};

struct ConsMulBool : TyInfrule {
public:
  ConsMulBool(std::shared_ptr<TyMulBool> _mul_bool);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z,
                                         std::shared_ptr<TyRegister> _x,
                                         std::shared_ptr<TyRegister> _y);

private:
  std::shared_ptr<TyMulBool> mul_bool;
};

struct ConsMulCommutative : public TyInfrule{
public : 
  ConsMulCommutative(std::shared_ptr<TyMulCommutative> _mul_commutative);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyMulCommutative> mul_commutative;
};

struct ConsMulMone : public TyInfrule{
public : 
  ConsMulMone(std::shared_ptr<TyMulMone> _mul_mone);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyMulMone> mul_mone;
};

struct ConsMulNeg : TyInfrule {
public:
  ConsMulNeg(std::shared_ptr<TyMulNeg> _mul_neg);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z,
                                         std::shared_ptr<TyValue> _mx,
                                         std::shared_ptr<TyValue> _my,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TySize> _sz);

private:
    std::shared_ptr<TyMulNeg> mul_neg;
};

struct ConsNegVal : TyInfrule {
public:
  ConsNegVal(std::shared_ptr<TyNegVal> _neg_val);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyInfrule> make
          (std::shared_ptr<TyConstInt> _c1,
           std::shared_ptr<TyConstInt> _c2,
           std::shared_ptr<TySize> _sz);

private:
  std::shared_ptr<TyNegVal> neg_val;
};

struct ConsAddSignbit : TyInfrule {
public:
  ConsAddSignbit(std::shared_ptr<TyAddSignbit> _add_signbit);
  void serialize(cereal::JSONOutputArchive &archive) const;
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _x,
                                         std::shared_ptr<TyValue> _e1,
                                         std::shared_ptr<TyValue> _e2,
                                         std::shared_ptr<TySize> _sz);

private:
  std::shared_ptr<TyAddSignbit> add_signbit;
};

struct ConsAddZextBool : public TyInfrule{
public : 
  ConsAddZextBool(std::shared_ptr<TyAddZextBool> _add_zext_bool);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _b, std::shared_ptr<TyConstInt> _c, std::shared_ptr<TyConstInt> _cprime, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyAddZextBool> add_zext_bool;
};

struct ConsSdivMone : public TyInfrule{
public : 
  ConsSdivMone(std::shared_ptr<TySdivMone> _div_mone);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TySdivMone> div_mone;
};

struct ConsMulShl : public TyInfrule{
public : 
  ConsMulShl(std::shared_ptr<TyMulShl> _mul_shl);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _a, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyMulShl> mul_shl;
};

struct ConsOrCommutative : public TyInfrule{
public : 
  ConsOrCommutative(std::shared_ptr<TyOrCommutative> _or_commutative);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyOrCommutative> or_commutative;
};

struct ConsOrOr : public TyInfrule{
public : 
  ConsOrOr(std::shared_ptr<TyOrOr> _or_or);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyOrOr> or_or;
};

struct ConsOrOr2 : public TyInfrule{
public : 
  ConsOrOr2(std::shared_ptr<TyOrOr2> _or_or2);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _yprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyOrOr2> or_or2;
};

struct ConsOrXor : public TyInfrule{
public : 
  ConsOrXor(std::shared_ptr<TyOrXor> _or_xor);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _w, std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyOrXor> or_xor;
};

struct ConsOrXor2 : public TyInfrule{
public : 
  ConsOrXor2(std::shared_ptr<TyOrXor2> _or_xor2);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x1, std::shared_ptr<TyValue> _y1, std::shared_ptr<TyValue> _x2, std::shared_ptr<TyValue> _y2, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyOrXor2> or_xor2;
};

struct ConsRemNeg : public TyInfrule{
public : 
  ConsRemNeg(std::shared_ptr<TyRemNeg> _rem_neg);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _my, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyRemNeg> rem_neg;
};

struct ConsSubAdd : TyInfrule {
public:
  ConsSubAdd(std::shared_ptr<TySubAdd> _sub_add);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z,
                                         std::shared_ptr<TyValue> _my,
                                         std::shared_ptr<TyRegister> _x,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TySize> _sz);

private:
  std::shared_ptr<TySubAdd> sub_add;
};

struct ConsSubMone : public TyInfrule{
public : 
  ConsSubMone(std::shared_ptr<TySubMone> _sub_mone);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TySubMone> sub_mone;
};

struct ConsSubOnebit : public TyInfrule{
public : 
  ConsSubOnebit(std::shared_ptr<TySubOnebit> _sub_onebit);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TySubOnebit> sub_onebit;
};

struct ConsSubConstAdd : public TyInfrule{
public : 
  ConsSubConstAdd(std::shared_ptr<TySubConstAdd> _sub_const_add);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c1, std::shared_ptr<TyConstInt> _c2, std::shared_ptr<TyConstInt> _c3, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TySubConstAdd> sub_const_add;
};

struct ConsSubConstNot : public TyInfrule{
public : 
  ConsSubConstNot(std::shared_ptr<TySubConstNot> _sub_const_not);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c1, std::shared_ptr<TyConstInt> _c2, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TySubConstNot> sub_const_not;
};

struct ConsSubRemove : TyInfrule {
public:
  ConsSubRemove(std::shared_ptr<TySubRemove> _sub_remove);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z,
                                         std::shared_ptr<TyRegister> _y,
                                         std::shared_ptr<TyValue> _a,
                                         std::shared_ptr<TyValue> _b,
                                         std::shared_ptr<TySize> _sz);

private:
  std::shared_ptr<TySubRemove> sub_remove;
};

struct ConsSubSdiv : public TyInfrule{
public : 
  ConsSubSdiv(std::shared_ptr<TySubSdiv> _sub_sdiv);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c, std::shared_ptr<TyConstInt> _cprime, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TySubSdiv> sub_sdiv;
};

struct ConsSubShl : public TyInfrule{
public : 
  ConsSubShl(std::shared_ptr<TySubShl> _sub_shl);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _mx, std::shared_ptr<TyValue> _a, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TySubShl> sub_shl;
};

struct ConsSdivSubSrem : public TyInfrule{
public : 
  ConsSdivSubSrem(std::shared_ptr<TySdivSubSrem> _div_sub_srem);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _b, std::shared_ptr<TyRegister> _a, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TySdivSubSrem> div_sub_srem;
};
struct ConsUdivSubUrem : public TyInfrule{
public : 
  ConsUdivSubUrem(std::shared_ptr<TyUdivSubUrem> _div_sub_urem);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _b, std::shared_ptr<TyRegister> _a, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyUdivSubUrem> div_sub_urem;
};

struct ConsTransitivity : TyInfrule {
public:
  ConsTransitivity(std::shared_ptr<TyTransitivity> _transitivity);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyExpr> _e1,
                                         std::shared_ptr<TyExpr> _e2,
                                         std::shared_ptr<TyExpr> _e3);

private:
  std::shared_ptr<TyTransitivity> transitivity;
};

struct ConsTransitivityTgt : TyInfrule {
public:
  ConsTransitivityTgt(std::shared_ptr<TyTransitivityTgt> _transitivity_tgt);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyExpr> _e1,
                                         std::shared_ptr<TyExpr> _e2,
                                         std::shared_ptr<TyExpr> _e3);

private:
  std::shared_ptr<TyTransitivityTgt> transitivity_tgt;
};

struct ConsReplaceRhs : TyInfrule {
public:
  ConsReplaceRhs(std::shared_ptr<TyReplaceRhs> _replace_rhs);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _x,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TyExpr> _e1,
                                         std::shared_ptr<TyExpr> _e2,
                                         std::shared_ptr<TyExpr> _e2_p);

private:
  std::shared_ptr<TyReplaceRhs> replace_rhs;
};


struct ConsReplaceRhsOpt : TyInfrule {
public:
  ConsReplaceRhsOpt(std::shared_ptr<TyReplaceRhsOpt> _replace_rhs_opt);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _x,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TyExpr> _e1,
                                         std::shared_ptr<TyExpr> _e2,
                                         std::shared_ptr<TyExpr> _e2_p);

private:
  std::shared_ptr<TyReplaceRhsOpt> replace_rhs_opt;
};

struct ConsUdivZext : public TyInfrule{
public : 
  ConsUdivZext(std::shared_ptr<TyUdivZext> _udiv_zext);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyRegister> _k, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz1, std::shared_ptr<TySize> _sz2);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyUdivZext> udiv_zext;
};

struct ConsUremZext : public TyInfrule{
public : 
  ConsUremZext(std::shared_ptr<TyUremZext> _urem_zext);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyRegister> _k, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz1, std::shared_ptr<TySize> _sz2);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyUremZext> urem_zext;
};


struct ConsIntroGhost : public TyInfrule{
public :
  ConsIntroGhost(std::shared_ptr<TyIntroGhost> _intro_ghost);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _x, std::shared_ptr<TyRegister> _g);
  void serialize(cereal::JSONOutputArchive& archive) const;

private :
  std::shared_ptr<TyIntroGhost> intro_ghost;
};

struct ConsXorCommutative : public TyInfrule{
public : 
  ConsXorCommutative(std::shared_ptr<TyXorCommutative> _xor_commutative);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyXorCommutative> xor_commutative;
};

struct ConsXorCommutativeTgt : public TyInfrule{
public : 
  ConsXorCommutativeTgt(std::shared_ptr<TyXorCommutativeTgt> _xor_commutative_tgt);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyXorCommutativeTgt> xor_commutative_tgt;
};




} // llvmberry

#endif
