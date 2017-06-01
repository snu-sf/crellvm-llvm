#ifndef INFRULES_H
#define INFRULES_H

#include "Structure.h"
#include "llvm/IR/Instructions.h"
#include "cereal/types/memory.hpp"
#include "cereal/archives/json.hpp"

namespace llvmberry {

struct TyAddCommutativeTgt {
public:
  TyAddCommutativeTgt(std::shared_ptr<TyRegister> _z,
                      std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
                      std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> sz;
};

struct TyBopAssociative {
public:
  TyBopAssociative(std::shared_ptr<TyRegister> _x,
                   std::shared_ptr<TyRegister> _y,
                   std::shared_ptr<TyRegister> _z, TyBop _bop,
                   std::shared_ptr<TyConstInt> _c1,
                   std::shared_ptr<TyConstInt> _c2,
                   std::shared_ptr<TyConstInt> _c3,
                   std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> x;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyRegister> z;
  TyBop bop;
  std::shared_ptr<TyConstInt> c1;
  std::shared_ptr<TyConstInt> c2;
  std::shared_ptr<TyConstInt> c3;
  std::shared_ptr<TySize> sz;
};

struct TyBopCommutative {
public:
  TyBopCommutative(std::shared_ptr<TyExpr> _e, TyBop _bop,
                   std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
                   std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyExpr> e;
  TyBop bop;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> sz;
};

struct TyBopCommutativeRev {
public:
  TyBopCommutativeRev(std::shared_ptr<TyExpr> _e, TyBop _bop,
                   std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
                   std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyExpr> e;
  TyBop bop;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> sz;
};

struct TyFbopCommutative {
public:
  TyFbopCommutative(std::shared_ptr<TyExpr> _e, TyFbop _fbop,
                    std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
                    TyFloatType _fty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyExpr> e;
  TyFbop fbop;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  TyFloatType fty;
};

struct TyBitcastDoubleI64 {
public:
  TyBitcastDoubleI64(std::shared_ptr<TyConstant> _src, std::shared_ptr<TyConstInt> _tgt);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyConstant> src;
  std::shared_ptr<TyConstInt> tgt;
};

struct TyAndMone {
public:
  TyAndMone(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
            std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TySize> sz;
};

struct TyAndNot {
public:
  TyAndNot(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
           std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> sz;
};

struct TyAndOr {
public:
  TyAndOr(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
          std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _a,
          std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TySize> sz;
};

struct TyAndSame {
public:
  TyAndSame(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
            std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TySize> sz;
};

struct TyAndTrueBool {
public:
  TyAndTrueBool(std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
};

struct TyAndUndef {
public:
  TyAndUndef(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
             std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TySize> sz;
};

struct TyAndZero {
public:
  TyAndZero(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
            std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TySize> sz;
};

struct TyBopDistributiveOverSelectinst {
public:
  TyBopDistributiveOverSelectinst(
      TyBop _opcode, std::shared_ptr<TyRegister> _r,
      std::shared_ptr<TyRegister> _s, std::shared_ptr<TyRegister> _tprime,
      std::shared_ptr<TyRegister> _t0, std::shared_ptr<TyValue> _x,
      std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _z,
      std::shared_ptr<TyValue> _c, std::shared_ptr<TySize> _bopsz,
      std::shared_ptr<TyValueType> _selty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
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

struct TyBopDistributiveOverSelectinst2 {
public:
  TyBopDistributiveOverSelectinst2(
      TyBop _opcode, std::shared_ptr<TyRegister> _r,
      std::shared_ptr<TyRegister> _s, std::shared_ptr<TyRegister> _tprime,
      std::shared_ptr<TyRegister> _t0, std::shared_ptr<TyValue> _x,
      std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _z,
      std::shared_ptr<TyValue> _c, std::shared_ptr<TySize> _bopsz,
      std::shared_ptr<TyValueType> _selty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
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

struct TyDiffblockUnique {
public:
  TyDiffblockUnique(std::shared_ptr<TyRegister> _x,
                    std::shared_ptr<TyRegister> _y);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> x;
  std::shared_ptr<TyRegister> y;
};

struct TyDiffblockGlobalGlobal {
public:
  TyDiffblockGlobalGlobal(std::shared_ptr<TyConstant> _gx,
                          std::shared_ptr<TyConstant> _gy);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyConstant> gx;
  std::shared_ptr<TyConstant> gy;
};

struct TyDiffblockGlobalUnique {
public:
  TyDiffblockGlobalUnique(std::shared_ptr<TyConstant> _gx,
                          std::shared_ptr<TyRegister> _y);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyConstant> gx;
  std::shared_ptr<TyRegister> y;
};

struct TyDiffblockLessthan {
public:
  TyDiffblockLessthan(std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
                      std::shared_ptr<TyValue> _xprime,
                      std::shared_ptr<TyValue> _yprime);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> xprime;
  std::shared_ptr<TyValue> yprime;
};

struct TyDiffblockNoalias {
public:
  TyDiffblockNoalias(std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
                     std::shared_ptr<TyPointer> _xprime,
                     std::shared_ptr<TyPointer> _yprime);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyPointer> xprime;
  std::shared_ptr<TyPointer> yprime;
};

struct TyDiffblockLoad {
public:
  TyDiffblockLoad(std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _p,
                  std::shared_ptr<TyValueType> _ty,
                  std::shared_ptr<TySize> _align,
                  std::shared_ptr<TyValue> _y,
                  std::shared_ptr<TyValue> _a);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> p;
  std::shared_ptr<TyValueType> ty;
  std::shared_ptr<TySize> align;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> a;
};

struct TyDiffblockGep {
public:
  TyDiffblockGep(std::shared_ptr<TyValue> _x,
                 std::shared_ptr<TyExpr> _gepinst,
                 std::shared_ptr<TyValue> _a);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyExpr> gepinst;
  std::shared_ptr<TyValue> a;
};

struct TyDiffblockBitcast {
public:
  TyDiffblockBitcast(std::shared_ptr<TyValue> _x,
                     std::shared_ptr<TyExpr> _bitcastinst,
                     std::shared_ptr<TyValue> _a);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyExpr> bitcastinst;
  std::shared_ptr<TyValue> a;
};

struct TyFaddCommutativeTgt {
public:
  TyFaddCommutativeTgt(std::shared_ptr<TyRegister> _z,
                       std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
                       TyFloatType _fty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  TyFloatType fty;
};

struct TyFmulCommutativeTgt {
public:
  TyFmulCommutativeTgt(std::shared_ptr<TyRegister> _z,
                       std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
                       TyFloatType _fty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  TyFloatType fty;
};

struct TyGepInboundsAdd {
public:
  TyGepInboundsAdd(std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _ptr,
                   std::shared_ptr<TyValueType> _loadty,
                   std::shared_ptr<TySize> _al, std::shared_ptr<TyExpr> _e);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> v;
  std::shared_ptr<TyValue> ptr;
  std::shared_ptr<TyValueType> loadty;
  std::shared_ptr<TySize> al;
  std::shared_ptr<TyExpr> e;
};

struct TyGepInboundsRemove {
public:
  TyGepInboundsRemove(std::shared_ptr<TyExpr> _gepinst);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyExpr> gepinst;
};

struct TyLessthanUndef {
public:
  TyLessthanUndef(std::shared_ptr<TyValueType> _ty,
                  std::shared_ptr<TyValue> _v);
  void serialize(cereal::JSONOutputArchive &archive) const;

  void updateRhs(std::shared_ptr<TyValue>);

private:
  std::shared_ptr<TyValueType> ty;
  std::shared_ptr<TyValue> v;
};

struct TyLessthanUndefTgt {
public:
  TyLessthanUndefTgt(std::shared_ptr<TyValueType> _ty,
                     std::shared_ptr<TyValue> _v);
  void serialize(cereal::JSONOutputArchive &archive) const;

  void updateRhs(std::shared_ptr<TyValue>);

private:
  std::shared_ptr<TyValueType> ty;
  std::shared_ptr<TyValue> v;
};

struct TyLessthanUndefConst {
public:
  TyLessthanUndefConst(std::shared_ptr<TyConstant> _c);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyConstant> c;
};

struct TyLessthanUndefConstTgt {
public:
  TyLessthanUndefConstTgt(std::shared_ptr<TyConstant> _c);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyConstant> c;
};

struct TyLessthanUndefConstGEPorCast {
public:
  TyLessthanUndefConstGEPorCast(std::shared_ptr<TyValueType> _ty,
                                std::shared_ptr<TyConstant> _c);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValueType> ty;
  std::shared_ptr<TyConstant> c;
};

struct TyNegVal {
public:
  TyNegVal(std::shared_ptr<TyConstInt> _c1, std::shared_ptr<TyConstInt> _c2,
           std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyConstInt> c1;
  std::shared_ptr<TyConstInt> c2;
  std::shared_ptr<TySize> sz;
};

struct TyOrAnd {
public:
  TyOrAnd(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _y,
          std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _a,
          std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TySize> sz;
};

struct TyOrCommutativeTgt {
public:
  TyOrCommutativeTgt(std::shared_ptr<TyRegister> _z,
                     std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
                     std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> sz;
};

struct TyOrFalse {
public:
  TyOrFalse(std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
            std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> sz;
};

struct TyOrMone {
public:
  TyOrMone(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _a,
           std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TySize> sz;
};

struct TyOrNot {
public:
  TyOrNot(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _y,
          std::shared_ptr<TyValue> _x, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TySize> sz;
};

struct TyOrSame {
public:
  TyOrSame(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _a,
           std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TySize> sz;
};

struct TyOrUndef {
public:
  TyOrUndef(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _a,
            std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TySize> sz;
};

struct TyOrZero {
public:
  TyOrZero(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _a,
           std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TySize> sz;
};

/*
struct TySextAshr {
public:
  TySextAshr(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime,
             std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _x0,
             std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _w,
             std::shared_ptr<TyConstInt> _c1, std::shared_ptr<TyConstInt> _c2,
             std::shared_ptr<TySize> _sz1, std::shared_ptr<TySize> _sz2);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> zprime;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> x0;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> w;
  std::shared_ptr<TyConstInt> c1;
  std::shared_ptr<TyConstInt> c2;
  std::shared_ptr<TySize> sz1;
  std::shared_ptr<TySize> sz2;
};
*/

struct TyShiftUndef1 {
public:
  TyShiftUndef1(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _y,
                std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> s;
};

struct TyShiftUndef2 {
public:
  TyShiftUndef2(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _y,
                std::shared_ptr<TyConstInt> _c, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyConstInt> c;
  std::shared_ptr<TySize> s;
};

struct TyShiftZero1 {
public:
  TyShiftZero1(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _y,
               std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> s;
};

struct TyShiftZero2 {
public:
  TyShiftZero2(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _y,
               std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> s;
};

/*
struct TySextTrunc {
public:
  TySextTrunc(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
              std::shared_ptr<TyValue> _y, std::shared_ptr<TyConstInt> _c,
              std::shared_ptr<TySize> _sz1, std::shared_ptr<TySize> _sz2);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyConstInt> c;
  std::shared_ptr<TySize> sz1;
  std::shared_ptr<TySize> sz2;
};
*/

struct TyTransitivity {
public:
  TyTransitivity(std::shared_ptr<TyExpr> _e1, std::shared_ptr<TyExpr> _e2,
                 std::shared_ptr<TyExpr> _e3);
  void serialize(cereal::JSONOutputArchive &archive) const;

  std::shared_ptr<TyExpr> getExpr1();
  std::shared_ptr<TyExpr> getExpr2();
  std::shared_ptr<TyExpr> getExpr3();
  void updateExpr1(std::shared_ptr<TyExpr> newExpr);
  void updateExpr2(std::shared_ptr<TyExpr> newExpr);
  void updateExpr3(std::shared_ptr<TyExpr> newExpr);

private:
  std::shared_ptr<TyExpr> e1;
  std::shared_ptr<TyExpr> e2;
  std::shared_ptr<TyExpr> e3;
};

struct TyTransitivityPointerLhs {
public:
  TyTransitivityPointerLhs(std::shared_ptr<TyValue> _p,
                           std::shared_ptr<TyValue> _q,
                           std::shared_ptr<TyValue> _v,
                           std::shared_ptr<TyValueType> _typ,
                           std::shared_ptr<TySize> _align);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> p;
  std::shared_ptr<TyValue> q;
  std::shared_ptr<TyValue> v;
  std::shared_ptr<TyValueType> typ;
  std::shared_ptr<TySize> align;
};

struct TyTransitivityPointerRhs {
public:
  TyTransitivityPointerRhs(std::shared_ptr<TyValue> _p,
                           std::shared_ptr<TyValue> _q,
                           std::shared_ptr<TyValue> _v,
                           std::shared_ptr<TyValueType> _typ,
                           std::shared_ptr<TySize> _align);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> p;
  std::shared_ptr<TyValue> q;
  std::shared_ptr<TyValue> v;
  std::shared_ptr<TyValueType> typ;
  std::shared_ptr<TySize> align;
};

struct TyTransitivityTgt {
public:
  TyTransitivityTgt(std::shared_ptr<TyExpr> _e1, std::shared_ptr<TyExpr> _e2,
                    std::shared_ptr<TyExpr> _e3);
  void serialize(cereal::JSONOutputArchive &archive) const;

  std::shared_ptr<TyExpr> getExpr1();
  std::shared_ptr<TyExpr> getExpr2();
  std::shared_ptr<TyExpr> getExpr3();
  void updateExpr1(std::shared_ptr<TyExpr> newExpr);
  void updateExpr2(std::shared_ptr<TyExpr> newExpr);
  void updateExpr3(std::shared_ptr<TyExpr> newExpr);

private:
  std::shared_ptr<TyExpr> e1;
  std::shared_ptr<TyExpr> e2;
  std::shared_ptr<TyExpr> e3;
};

struct TySubstitute {
public:
  TySubstitute(std::shared_ptr<TyRegister> _x, std::shared_ptr<TyValue> _y,
               std::shared_ptr<TyExpr> _e);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyExpr> e;
};

struct TySubstituteRev {
public:
  TySubstituteRev(std::shared_ptr<TyRegister> _x, std::shared_ptr<TyValue> _y,
                  std::shared_ptr<TyExpr> _e);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyExpr> e;
};

struct TySubstituteTgt {
public:
  TySubstituteTgt(std::shared_ptr<TyRegister> _x, std::shared_ptr<TyValue> _y,
                  std::shared_ptr<TyExpr> _e);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyExpr> e;
};

struct TyReplaceRhs {
public:
  TyReplaceRhs(std::shared_ptr<TyRegister> _x, std::shared_ptr<TyValue> _y,
               std::shared_ptr<TyExpr> _e1, std::shared_ptr<TyExpr> _e2,
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
  TyReplaceRhsOpt(std::shared_ptr<TyRegister> _x, std::shared_ptr<TyValue> _y,
                  std::shared_ptr<TyExpr> _e1, std::shared_ptr<TyExpr> _e2,
                  std::shared_ptr<TyExpr> _e2_p);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyExpr> e1;
  std::shared_ptr<TyExpr> e2;
  std::shared_ptr<TyExpr> e2_p;
};

struct TyIntroGhost {
public:
  TyIntroGhost(std::shared_ptr<TyExpr> _x, std::shared_ptr<TyRegister> _g);
  void serialize(cereal::JSONOutputArchive &archive) const;

  std::shared_ptr<TyExpr> getExpr();
  std::shared_ptr<TyRegister> getReg();
  void updateExpr(std::shared_ptr<TyExpr> newExpr);

private:
  std::shared_ptr<TyExpr> x;
  std::shared_ptr<TyRegister> g;
};

struct TyIntroGhostSrc {
public:
  TyIntroGhostSrc(std::shared_ptr<TyExpr> _x, std::shared_ptr<TyRegister> _g);
  void serialize(cereal::JSONOutputArchive &archive) const;

  std::shared_ptr<TyExpr> getExpr();
  std::shared_ptr<TyRegister> getReg();
  void updateExpr(std::shared_ptr<TyExpr> newExpr);

private:
  std::shared_ptr<TyExpr> x;
  std::shared_ptr<TyRegister> g;
};

struct TyIntroEq {
public:
  TyIntroEq(std::shared_ptr<TyExpr> _x);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyExpr> x;
};

struct TyIntroEqTgt {
public:
  TyIntroEqTgt(std::shared_ptr<TyExpr> _x);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyExpr> x;
};

struct TyInttoptrPtrtoint {
public:
  TyInttoptrPtrtoint(std::shared_ptr<TyValue> _src,
                     std::shared_ptr<TyValue> _mid,
                     std::shared_ptr<TyValue> _dst,
                     std::shared_ptr<TyValueType> _srcty,
                     std::shared_ptr<TyValueType> _midty,
                     std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct TyInttoptrZext {
public:
  TyInttoptrZext(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
                 std::shared_ptr<TyValue> _dst,
                 std::shared_ptr<TyValueType> _srcty,
                 std::shared_ptr<TyValueType> _midty,
                 std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct TyZextTruncAnd {
public:
  TyZextTruncAnd(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
                 std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _w,
                 std::shared_ptr<TyConstant> _c, std::shared_ptr<TySize> _s,
                 std::shared_ptr<TySize> _sprime);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> w;
  std::shared_ptr<TyConstant> c;
  std::shared_ptr<TySize> s;
  std::shared_ptr<TySize> sprime;
};

struct TyXorCommutativeTgt {
public:
  TyXorCommutativeTgt(std::shared_ptr<TyRegister> _z,
                      std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
                      std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> sz;
};

struct TyXorNot {
public:
  TyXorNot(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _y,
           std::shared_ptr<TyValue> _x, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TySize> s;
};

struct TyXorSame {
public:
  TyXorSame(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _a,
            std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TySize> s;
};

struct TyXorUndef {
public:
  TyXorUndef(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _a,
             std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TySize> s;
};

struct TyXorZero {
public:
  TyXorZero(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _a,
            std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TySize> s;
};

struct TyImpliesFalse {
public:
  TyImpliesFalse(std::shared_ptr<TyConstant> _c1,
                 std::shared_ptr<TyConstant> _c2);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyConstant> c1;
  std::shared_ptr<TyConstant> c2;
};

struct TyIcmpInverse {
public:
  TyIcmpInverse(enum TyIcmpPred _predicate, std::shared_ptr<TyValueType> _ty,
                std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
                std::shared_ptr<TyConstInt> _boolean);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  enum TyIcmpPred predicate;
  std::shared_ptr<TyValueType> ty;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyConstInt> boolean;
};

struct TyIcmpInverseRhs {
public:
  TyIcmpInverseRhs(enum TyIcmpPred _predicate, std::shared_ptr<TyValueType> _ty,
                   std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
                   std::shared_ptr<TyConstInt> _boolean);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  enum TyIcmpPred predicate;
  std::shared_ptr<TyValueType> ty;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyConstInt> boolean;
};

struct TyIcmpSwapOperands {
public:
  TyIcmpSwapOperands(enum TyIcmpPred _predicate, std::shared_ptr<TyValueType> _ty,
                std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
                std::shared_ptr<TyExpr> _e);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  enum TyIcmpPred predicate;
  std::shared_ptr<TyValueType> ty;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyExpr> e;
};

struct TyFcmpSwapOperands {
public:
  TyFcmpSwapOperands(enum TyFcmpPred _predicate, TyFloatType _fty,
                     std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
                     std::shared_ptr<TyExpr> _e);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  enum TyFcmpPred predicate;
  TyFloatType fty;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyExpr> e;
};

struct TyIcmpEqSame {
public:
  TyIcmpEqSame(std::shared_ptr<TyValueType> _ty, std::shared_ptr<TyValue> _x,
               std::shared_ptr<TyValue> _y);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValueType> ty;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
};

struct TyIcmpNeqSame {
public:
  TyIcmpNeqSame(std::shared_ptr<TyValueType> _ty, std::shared_ptr<TyValue> _x,
                std::shared_ptr<TyValue> _y);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValueType> ty;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
};

/////// constructor classes ///////

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

struct ConsAndMone : public TyInfrule {
public:
  ConsAndMone(std::shared_ptr<TyAndMone> _and_mone);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyAndMone> and_mone;
};

struct ConsAndNot : public TyInfrule {
public:
  ConsAndNot(std::shared_ptr<TyAndNot> _and_not);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyAndNot> and_not;
};

struct ConsAndOr : public TyInfrule {
public:
  ConsAndOr(std::shared_ptr<TyAndOr> _and_or);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TyValue> _a,
                                         std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyAndOr> and_or;
};

struct ConsAndSame : public TyInfrule {
public:
  ConsAndSame(std::shared_ptr<TyAndSame> _and_same);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyAndSame> and_same;
};

struct ConsAndTrueBool : public TyInfrule {
public:
  ConsAndTrueBool(std::shared_ptr<TyAndTrueBool> _and_true_bool);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _y);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyAndTrueBool> and_true_bool;
};

struct ConsAndUndef : public TyInfrule {
public:
  ConsAndUndef(std::shared_ptr<TyAndUndef> _and_undef);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyAndUndef> and_undef;
};

struct ConsAndZero : public TyInfrule {
public:
  ConsAndZero(std::shared_ptr<TyAndZero> _and_zero);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyAndZero> and_zero;
};

struct ConsBopAssociative : TyInfrule {
public:
  ConsBopAssociative(std::shared_ptr<TyBopAssociative> _bop_associative);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y,
       std::shared_ptr<TyRegister> _z, TyBop _bop,
       std::shared_ptr<TyConstInt> _c1, std::shared_ptr<TyConstInt> _c2,
       std::shared_ptr<TyConstInt> _c3, std::shared_ptr<TySize> _sz);

private:
  std::shared_ptr<TyBopAssociative> bop_associative;
};

struct ConsBopCommutative : TyInfrule {
public:
  ConsBopCommutative(std::shared_ptr<TyBopCommutative> _bop_commutative);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyExpr> _e, TyBop _bop,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TySize> _sz);

private:
  std::shared_ptr<TyBopCommutative> bop_commutative;
};

struct ConsBopCommutativeRev : TyInfrule {
public:
  ConsBopCommutativeRev(std::shared_ptr<TyBopCommutativeRev> _bop_commutative_rev);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyExpr> _e, TyBop _bop,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TySize> _sz);

private:
  std::shared_ptr<TyBopCommutativeRev> bop_commutative_rev;
};

struct ConsFbopCommutative : TyInfrule {
public:
  ConsFbopCommutative(std::shared_ptr<TyFbopCommutative> _fbop_commutative);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyExpr> _e, TyFbop _fbop,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _y,
                                         TyFloatType _fty);

private:
  std::shared_ptr<TyFbopCommutative> fbop_commutative;
};

struct ConsBitcastDoubleI64 : public TyInfrule {
public:
  ConsBitcastDoubleI64(std::shared_ptr<TyBitcastDoubleI64> _bitcast_double_i64);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyConstant> _src,
                                         std::shared_ptr<TyConstInt> _tgt);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyBitcastDoubleI64> bitcast_double_i64;
};

struct ConsDiffblockUnique : public TyInfrule {
public:
  ConsDiffblockUnique(std::shared_ptr<TyDiffblockUnique> _diffblock_unique);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _x,
                                         std::shared_ptr<TyRegister> _y);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyDiffblockUnique> diffblock_unique;
};

struct ConsDiffblockGlobalGlobal : public TyInfrule {
public:
  ConsDiffblockGlobalGlobal(
      std::shared_ptr<TyDiffblockGlobalGlobal> _diffblock_global_global);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyConstant> _gx,
                                         std::shared_ptr<TyConstant> _gy);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyDiffblockGlobalGlobal> diffblock_global_global;
};

struct ConsDiffblockGlobalUnique : public TyInfrule {
public:
  ConsDiffblockGlobalUnique(
      std::shared_ptr<TyDiffblockGlobalUnique> _diffblock_global_unique);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyConstant> _gx,
                                         std::shared_ptr<TyRegister> _y);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyDiffblockGlobalUnique> diffblock_global_unique;
};

struct ConsDiffblockLessthan : public TyInfrule {
public:
  ConsDiffblockLessthan(
      std::shared_ptr<TyDiffblockLessthan> _diffblock_lessthan);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TyValue> _xprime,
                                         std::shared_ptr<TyValue> _yprime);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyDiffblockLessthan> diffblock_lessthan;
};

struct ConsDiffblockNoalias : public TyInfrule {
public:
  ConsDiffblockNoalias(std::shared_ptr<TyDiffblockNoalias> _diffblock_noalias);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TyPointer> _xprime,
                                         std::shared_ptr<TyPointer> _yprime);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyDiffblockNoalias> diffblock_noalias;
};

struct ConsDiffblockLoad : public TyInfrule {
public:
  ConsDiffblockLoad(std::shared_ptr<TyDiffblockLoad> _diffblock_load);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _p,
                                         std::shared_ptr<TyValueType> _ty,
                                         std::shared_ptr<TySize> _align,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TyValue> _a);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyDiffblockLoad> diffblock_load;
};

struct ConsDiffblockGep : public TyInfrule {
public:
  ConsDiffblockGep(std::shared_ptr<TyDiffblockGep> _diffblock_gep);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyExpr> _gepinst,
                                         std::shared_ptr<TyValue> _a);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyDiffblockGep> diffblock_gep;
};

struct ConsDiffblockBitcast : public TyInfrule {
public:
  ConsDiffblockBitcast(std::shared_ptr<TyDiffblockBitcast> _diffblock_bitcast);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyExpr> _bitcastinst,
                                         std::shared_ptr<TyValue> _a);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyDiffblockBitcast> diffblock_bitcast;
};

struct ConsFaddCommutativeTgt : public TyInfrule {
public:
  ConsFaddCommutativeTgt(
      std::shared_ptr<TyFaddCommutativeTgt> _fadd_commutative_tgt);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _y,
                                         TyFloatType _fty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyFaddCommutativeTgt> fadd_commutative_tgt;
};

struct ConsFmulCommutativeTgt : public TyInfrule {
public:
  ConsFmulCommutativeTgt(
      std::shared_ptr<TyFmulCommutativeTgt> _fmul_commutative_tgt);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _y,
                                         TyFloatType _fty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyFmulCommutativeTgt> fmul_commutative_tgt;
};

struct ConsGepInboundsAdd : public TyInfrule {
public:
  ConsGepInboundsAdd(std::shared_ptr<TyGepInboundsAdd> _gep_inbounds_add);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _v,
                                         std::shared_ptr<TyValue> _ptr,
                                         std::shared_ptr<TyValueType> _loadty,
                                         std::shared_ptr<TySize> _al,
                                         std::shared_ptr<TyExpr> _e);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyGepInboundsAdd> gep_inbounds_add;
};

struct ConsGepInboundsRemove : public TyInfrule {
public:
  ConsGepInboundsRemove(
      std::shared_ptr<TyGepInboundsRemove> _gep_inbounds_remove);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyExpr> _gepinst);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyGepInboundsRemove> gep_inbounds_remove;
};

struct ConsLessthanUndef : TyInfrule {
public:
  ConsLessthanUndef(std::shared_ptr<TyLessthanUndef> _lessthan_undef);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValueType> _ty,
                                         std::shared_ptr<TyValue> _v);

private:
  std::shared_ptr<TyLessthanUndef> lessthan_undef;
};

struct ConsLessthanUndefTgt : TyInfrule {
public:
  ConsLessthanUndefTgt(std::shared_ptr<TyLessthanUndefTgt> _lessthan_undef_tgt);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValueType> _ty,
                                         std::shared_ptr<TyValue> _v);

private:
  std::shared_ptr<TyLessthanUndefTgt> lessthan_undef_tgt;
};

struct ConsLessthanUndefConst : TyInfrule {
public:
  ConsLessthanUndefConst(std::shared_ptr<TyLessthanUndefConst> _lessthan_undef_const_tgt);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyConstant> _c);

private:
  std::shared_ptr<TyLessthanUndefConst> lessthan_undef_const;
};

struct ConsLessthanUndefConstTgt : TyInfrule {
public:
  ConsLessthanUndefConstTgt(std::shared_ptr<TyLessthanUndefConstTgt> _lessthan_undef_const_tgt);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyConstant> _c);

private:
  std::shared_ptr<TyLessthanUndefConstTgt> lessthan_undef_const_tgt;
};

struct ConsLessthanUndefConstGEPorCast : TyInfrule {
public:
  ConsLessthanUndefConstGEPorCast(std::shared_ptr<TyLessthanUndefConstGEPorCast> _lessthan_undef_const_gep_or_cast);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValueType> _ty,
                                         std::shared_ptr<TyConstant> _c);

private:
  std::shared_ptr<TyLessthanUndefConstGEPorCast> lessthan_undef_const_gep_or_cast;
};

struct ConsNegVal : TyInfrule {
public:
  ConsNegVal(std::shared_ptr<TyNegVal> _neg_val);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyConstInt> _c1,
                                         std::shared_ptr<TyConstInt> _c2,
                                         std::shared_ptr<TySize> _sz);

private:
  std::shared_ptr<TyNegVal> neg_val;
};

struct ConsOrAnd : public TyInfrule {
public:
  ConsOrAnd(std::shared_ptr<TyOrAnd> _or_and);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _a,
                                         std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyOrAnd> or_and;
};

struct ConsOrCommutativeTgt : public TyInfrule {
public:
  ConsOrCommutativeTgt(std::shared_ptr<TyOrCommutativeTgt> _or_commutative);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyOrCommutativeTgt> or_commutative_tgt;
};

struct ConsOrFalse : public TyInfrule {
public:
  ConsOrFalse(std::shared_ptr<TyOrFalse> _or_false);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyOrFalse> or_false;
};

struct ConsOrMone : public TyInfrule {
public:
  ConsOrMone(std::shared_ptr<TyOrMone> _or_mone);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z,
                                         std::shared_ptr<TyValue> _a,
                                         std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyOrMone> or_mone;
};

struct ConsOrNot : public TyInfrule {
public:
  ConsOrNot(std::shared_ptr<TyOrNot> _or_not);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyOrNot> or_not;
};

struct ConsOrUndef : public TyInfrule {
public:
  ConsOrUndef(std::shared_ptr<TyOrUndef> _or_undef);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z,
                                         std::shared_ptr<TyValue> _a,
                                         std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyOrUndef> or_undef;
};

struct ConsOrSame : public TyInfrule {
public:
  ConsOrSame(std::shared_ptr<TyOrSame> _or_same);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z,
                                         std::shared_ptr<TyValue> _a,
                                         std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyOrSame> or_same;
};

struct ConsOrZero : public TyInfrule {
public:
  ConsOrZero(std::shared_ptr<TyOrZero> _or_zero);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z,
                                         std::shared_ptr<TyValue> _a,
                                         std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyOrZero> or_zero;
};

struct ConsShiftUndef1 : public TyInfrule {
public:
  ConsShiftUndef1(std::shared_ptr<TyShiftUndef1> _shift_undef1);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyShiftUndef1> shift_undef1;
};

struct ConsShiftUndef2 : public TyInfrule {
public:
  ConsShiftUndef2(std::shared_ptr<TyShiftUndef2> _shift_undef2);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TyConstInt> _c,
                                         std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyShiftUndef2> shift_undef2;
};

struct ConsShiftZero1 : public TyInfrule {
public:
  ConsShiftZero1(std::shared_ptr<TyShiftZero1> _shift_zero1);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyShiftZero1> shift_zero1;
};

struct ConsShiftZero2 : public TyInfrule {
public:
  ConsShiftZero2(std::shared_ptr<TyShiftZero2> _shift_zero2);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyShiftZero2> shift_zero2;
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

struct ConsTransitivityPointerLhs : public TyInfrule {
public:
  ConsTransitivityPointerLhs(
      std::shared_ptr<TyTransitivityPointerLhs> _transitivity_pointer_lhs);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _p,
                                         std::shared_ptr<TyValue> _q,
                                         std::shared_ptr<TyValue> _v,
                                         std::shared_ptr<TyValueType> _typ,
                                         std::shared_ptr<TySize> _align);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyTransitivityPointerLhs> transitivity_pointer_lhs;
};

struct ConsTransitivityPointerRhs : public TyInfrule {
public:
  ConsTransitivityPointerRhs(
      std::shared_ptr<TyTransitivityPointerRhs> _transitivity_pointer_rhs);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _p,
                                         std::shared_ptr<TyValue> _q,
                                         std::shared_ptr<TyValue> _v,
                                         std::shared_ptr<TyValueType> _typ,
                                         std::shared_ptr<TySize> _align);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyTransitivityPointerRhs> transitivity_pointer_rhs;
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

struct ConsSubstitute : public TyInfrule {
public:
  ConsSubstitute(std::shared_ptr<TySubstitute> _substitute);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _x,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TyExpr> _e);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySubstitute> substitute;
};

struct ConsSubstituteRev : public TyInfrule {
public:
  ConsSubstituteRev(std::shared_ptr<TySubstituteRev> _substitute_rev);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _x,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TyExpr> _e);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySubstituteRev> substitute_rev;
};

struct ConsSubstituteTgt : public TyInfrule {
public:
  ConsSubstituteTgt(std::shared_ptr<TySubstituteTgt> _substitute_tgt);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _x,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TyExpr> _e);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySubstituteTgt> substitute_tgt;
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

struct ConsInttoptrPtrtoint : public TyInfrule {
public:
  ConsInttoptrPtrtoint(std::shared_ptr<TyInttoptrPtrtoint> _inttoptr_ptrtoint);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyInttoptrPtrtoint> inttoptr_ptrtoint;
};

struct ConsInttoptrZext : public TyInfrule {
public:
  ConsInttoptrZext(std::shared_ptr<TyInttoptrZext> _zext_inttoptr);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyInttoptrZext> zext_inttoptr;
};

struct ConsZextTruncAnd : public TyInfrule {
public:
  ConsZextTruncAnd(std::shared_ptr<TyZextTruncAnd> _zext_trunc_and);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
       std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _w,
       std::shared_ptr<TyConstant> _c, std::shared_ptr<TySize> _s,
       std::shared_ptr<TySize> _sprime);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyZextTruncAnd> zext_trunc_and;
};

struct ConsIntroGhost : public TyInfrule {
public:
  ConsIntroGhost(std::shared_ptr<TyIntroGhost> _intro_ghost);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyExpr> _x,
                                         std::shared_ptr<TyRegister> _g);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyIntroGhost> intro_ghost;
};

struct ConsIntroGhostSrc : public TyInfrule {
public:
  ConsIntroGhostSrc(std::shared_ptr<TyIntroGhostSrc> _intro_ghost_src);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyExpr> _x,
                                         std::shared_ptr<TyRegister> _g);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyIntroGhostSrc> intro_ghost_src;
};

struct ConsIntroEq : public TyInfrule {
public:
  ConsIntroEq(std::shared_ptr<TyIntroEq> _intro_eq);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _x);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyExpr> _x);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyIntroEq> intro_eq;
};

struct ConsIntroEqTgt : public TyInfrule {
public:
  ConsIntroEqTgt(std::shared_ptr<TyIntroEqTgt> _intro_eq_tgt);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _x);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyExpr> _x);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyIntroEqTgt> intro_eq_tgt;
};

struct ConsXorCommutativeTgt : public TyInfrule {
public:
  ConsXorCommutativeTgt(
      std::shared_ptr<TyXorCommutativeTgt> _xor_commutative_tgt);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyXorCommutativeTgt> xor_commutative_tgt;
};

struct ConsXorNot : public TyInfrule {
public:
  ConsXorNot(std::shared_ptr<TyXorNot> _xor_not);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyXorNot> xor_not;
};

struct ConsXorSame : public TyInfrule {
public:
  ConsXorSame(std::shared_ptr<TyXorSame> _xor_same);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z,
                                         std::shared_ptr<TyValue> _a,
                                         std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyXorSame> xor_same;
};

struct ConsXorUndef : public TyInfrule {
public:
  ConsXorUndef(std::shared_ptr<TyXorUndef> _xor_undef);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z,
                                         std::shared_ptr<TyValue> _a,
                                         std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyXorUndef> xor_undef;
};

struct ConsXorZero : public TyInfrule {
public:
  ConsXorZero(std::shared_ptr<TyXorZero> _xor_zero);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z,
                                         std::shared_ptr<TyValue> _a,
                                         std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyXorZero> xor_zero;
};

struct ConsImpliesFalse : public TyInfrule {
public:
  ConsImpliesFalse(std::shared_ptr<TyImpliesFalse> _implies_false);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyConstant> _c1,
                                         std::shared_ptr<TyConstant> _c2);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyImpliesFalse> implies_false;
};

struct ConsIcmpInverse : public TyInfrule {
public:
  ConsIcmpInverse(std::shared_ptr<TyIcmpInverse> _icmp_inverse);
  static std::shared_ptr<TyInfrule> make(llvm::ICmpInst &CI, int boolean);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyIcmpInverse> icmp_inverse;
};

struct ConsIcmpInverseRhs : public TyInfrule {
public:
  ConsIcmpInverseRhs(std::shared_ptr<TyIcmpInverseRhs> _icmp_inverse_rhs);
  static std::shared_ptr<TyInfrule> make(llvm::ICmpInst &CI, int boolean);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyIcmpInverseRhs> icmp_inverse_rhs;
};

struct ConsFcmpSwapOperands : public TyInfrule {
public:
  ConsFcmpSwapOperands(std::shared_ptr<TyFcmpSwapOperands> _fcmp_swap_operands);
  static std::shared_ptr<TyInfrule> make(llvm::FCmpInst &CI);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyFcmpSwapOperands> fcmp_swap_operands;
};

struct ConsIcmpEqSame : public TyInfrule {
public:
  ConsIcmpEqSame(std::shared_ptr<TyIcmpEqSame> _icmp_eq_same);
  static std::shared_ptr<TyInfrule> make(llvm::ICmpInst &CI);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyIcmpEqSame> icmp_eq_same;
};

struct ConsIcmpNeqSame : public TyInfrule {
public:
  ConsIcmpNeqSame(std::shared_ptr<TyIcmpNeqSame> _icmp_neq_same);
  static std::shared_ptr<TyInfrule> make(llvm::ICmpInst &CI);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyIcmpNeqSame> icmp_neq_same;
};

struct ConsIcmpSwapOperands : public TyInfrule {
public:
  ConsIcmpSwapOperands(std::shared_ptr<TyIcmpSwapOperands> _icmp_swap_operands);
  static std::shared_ptr<TyInfrule> make(llvm::ICmpInst &CI);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyIcmpSwapOperands> icmp_swap_operands;
};

} // llvmberry

#endif
