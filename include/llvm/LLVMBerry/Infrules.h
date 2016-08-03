#ifndef INFRULES_H
#define INFRULES_H

#include "Structure.h"
#include "llvm/IR/Instructions.h"
#include "cereal/types/memory.hpp"
#include "cereal/archives/json.hpp"

namespace llvmberry {

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

struct TyAddConstNot {
public:
  TyAddConstNot(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y,
                std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c1,
                std::shared_ptr<TyConstInt> _c2, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyConstInt> c1;
  std::shared_ptr<TyConstInt> c2;
  std::shared_ptr<TySize> sz;
};

struct TyAddDistSub {
public:
  TyAddDistSub(std::shared_ptr<TyRegister> _z,
               std::shared_ptr<TyRegister> _minusx,
               std::shared_ptr<TyValue> _minusy, std::shared_ptr<TyRegister> _w,
               std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
               std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> minusx;
  std::shared_ptr<TyValue> minusy;
  std::shared_ptr<TyRegister> w;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> sz;
};

struct TyAddMask {
public:
  TyAddMask(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y,
            std::shared_ptr<TyRegister> _yprime, std::shared_ptr<TyValue> _x,
            std::shared_ptr<TyConstInt> _c1, std::shared_ptr<TyConstInt> _c2,
            std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyRegister> yprime;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyConstInt> c1;
  std::shared_ptr<TyConstInt> c2;
  std::shared_ptr<TySize> sz;
};

struct TyAddSelectZero {
public:
  TyAddSelectZero(std::shared_ptr<TyRegister> _z,
                  std::shared_ptr<TyRegister> _x,
                  std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _c,
                  std::shared_ptr<TyValue> _n, std::shared_ptr<TyValue> _a,
                  std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> x;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyValue> c;
  std::shared_ptr<TyValue> n;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TySize> sz;
};

struct TyAddSelectZero2 {
public:
  TyAddSelectZero2(std::shared_ptr<TyRegister> _z,
                   std::shared_ptr<TyRegister> _x,
                   std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _c,
                   std::shared_ptr<TyValue> _n, std::shared_ptr<TyValue> _a,
                   std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> x;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyValue> c;
  std::shared_ptr<TyValue> n;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TySize> sz;
};

struct TyAddOnebit {
public:
  TyAddOnebit(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x,
              std::shared_ptr<TyValue> _y);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
};

struct TyAddOrAnd {
public:
  TyAddOrAnd(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _a,
             std::shared_ptr<TyValue> _b, std::shared_ptr<TyRegister> _x,
             std::shared_ptr<TyRegister> _y, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TyRegister> x;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TySize> sz;
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

struct TyAddZextBool {
public:
  TyAddZextBool(std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y,
                std::shared_ptr<TyValue> _b, std::shared_ptr<TyConstInt> _c,
                std::shared_ptr<TyConstInt> _cprime,
                std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> x;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TyConstInt> c;
  std::shared_ptr<TyConstInt> cprime;
  std::shared_ptr<TySize> sz;
};

struct TyAddSextSext {
public:
  TyAddSextSext(
      std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _xprime,
      std::shared_ptr<TyValue> _x, std::shared_ptr<TyRegister> _yprime,
      std::shared_ptr<TyValue> _y, std::shared_ptr<TyRegister> _zprime,
      std::shared_ptr<TySize> _from_sz, std::shared_ptr<TySize> _to_sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> xprime;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyRegister> yprime;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyRegister> zprime;
  std::shared_ptr<TySize> from_sz;
  std::shared_ptr<TySize> to_sz;
};

struct TyAddXorAnd {
public:
  TyAddXorAnd(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _a,
              std::shared_ptr<TyValue> _b, std::shared_ptr<TyRegister> _x,
              std::shared_ptr<TyRegister> _y, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TyRegister> x;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TySize> sz;
};

struct TyAndCommutative {
public:
  TyAndCommutative(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x,
                   std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> sz;
};

struct TyAndDeMorgan {
public:
  TyAndDeMorgan(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x,
                std::shared_ptr<TyRegister> _y,
                std::shared_ptr<TyRegister> _zprime,
                std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
                std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> x;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyRegister> zprime;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> sz;
};

struct TyBitcastBitcast {
public:
  TyBitcastBitcast(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
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

struct TyBitcastFpext {
public:
  TyBitcastFpext(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
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

struct TyBitcastFptosi {
public:
  TyBitcastFptosi(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
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

struct TyBitcastFptoui {
public:
  TyBitcastFptoui(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
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

struct TyBitcastFptrunc {
public:
  TyBitcastFptrunc(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
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

struct TyBitcastInttoptr {
public:
  TyBitcastInttoptr(std::shared_ptr<TyValue> _src,
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

struct TyBitcastPtrtoint {
public:
  TyBitcastPtrtoint(std::shared_ptr<TyValue> _src,
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

struct TyBitcastSametype {
public:
  TyBitcastSametype(std::shared_ptr<TyValue> _src,
                    std::shared_ptr<TyValue> _dst,
                    std::shared_ptr<TyValueType> _tty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> tty;
};

struct TyBitcastSext {
public:
  TyBitcastSext(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
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

struct TyBitcastSitofp {
public:
  TyBitcastSitofp(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
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

struct TyBitcastTrunc {
public:
  TyBitcastTrunc(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
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

struct TyBitcastUitofp {
public:
  TyBitcastUitofp(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
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

struct TyBitcastZext {
public:
  TyBitcastZext(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
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

struct TyBitcastLoad {
public:
  TyBitcastLoad(std::shared_ptr<TyValue> _ptr,
                std::shared_ptr<TyValueType> _ptrty,
                std::shared_ptr<TyValue> _v1,
                std::shared_ptr<TyValueType> _ptrty2,
                std::shared_ptr<TyValue> _v2, std::shared_ptr<TySize> _a);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> ptr;
  std::shared_ptr<TyValueType> ptrty;
  std::shared_ptr<TyValue> v1;
  std::shared_ptr<TyValueType> ptrty2;
  std::shared_ptr<TyValue> v2;
  std::shared_ptr<TySize> a;
};

struct TyBitcastptr {
public:
  TyBitcastptr(std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _vprime,
               std::shared_ptr<TyExpr> _bitcastinst);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> v;
  std::shared_ptr<TyValue> vprime;
  std::shared_ptr<TyExpr> bitcastinst;
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

struct TyAndOrConst2 {
public:
  TyAndOrConst2(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y,
                std::shared_ptr<TyRegister> _yprime,
                std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c1,
                std::shared_ptr<TyConstInt> _c2,
                std::shared_ptr<TyConstInt> _c3, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyRegister> yprime;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyConstInt> c1;
  std::shared_ptr<TyConstInt> c2;
  std::shared_ptr<TyConstInt> c3;
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

struct TyAndOrNot1 {
public:
  TyAndOrNot1(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x,
              std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _a,
              std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> x;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
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

struct TyDiffblockGlobalAlloca {
public:
  TyDiffblockGlobalAlloca(std::shared_ptr<TyConstant> _gx,
                          std::shared_ptr<TyRegister> _y);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyConstant> gx;
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

struct TyFbopDistributiveOverSelectinst {
public:
  TyFbopDistributiveOverSelectinst(
      TyFbop _fopcode, std::shared_ptr<TyRegister> _r,
      std::shared_ptr<TyRegister> _s, std::shared_ptr<TyRegister> _tprime,
      std::shared_ptr<TyRegister> _t0, std::shared_ptr<TyValue> _x,
      std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _z,
      std::shared_ptr<TyValue> _c, TyFloatType _fbopty,
      std::shared_ptr<TyValueType> _selty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
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

struct TyFbopDistributiveOverSelectinst2 {
public:
  TyFbopDistributiveOverSelectinst2(
      TyFbop _fopcode, std::shared_ptr<TyRegister> _r,
      std::shared_ptr<TyRegister> _s, std::shared_ptr<TyRegister> _tprime,
      std::shared_ptr<TyRegister> _t0, std::shared_ptr<TyValue> _x,
      std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _z,
      std::shared_ptr<TyValue> _c, TyFloatType _fbopty,
      std::shared_ptr<TyValueType> _selty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
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

struct TyFpextBitcast {
public:
  TyFpextBitcast(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
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

struct TyFpextFpext {
public:
  TyFpextFpext(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
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

struct TyFptosiBitcast {
public:
  TyFptosiBitcast(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
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

struct TyFptosiFpext {
public:
  TyFptosiFpext(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
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

struct TyFptouiBitcast {
public:
  TyFptouiBitcast(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
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

struct TyFptouiFpext {
public:
  TyFptouiFpext(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
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

struct TyFptruncBitcast {
public:
  TyFptruncBitcast(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
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

struct TyFptruncFpext {
public:
  TyFptruncFpext(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
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

struct TyGepzero {
public:
  TyGepzero(std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _vprime,
            std::shared_ptr<TyExpr> _gepinst);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> v;
  std::shared_ptr<TyValue> vprime;
  std::shared_ptr<TyExpr> gepinst;
};

struct TyGepInboundsRemove {
public:
  TyGepInboundsRemove(std::shared_ptr<TyExpr> _gepinst);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyExpr> gepinst;
};

struct TyPtrtointBitcast {
public:
  TyPtrtointBitcast(std::shared_ptr<TyValue> _src,
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

struct TyPtrtointLoad {
public:
  TyPtrtointLoad(std::shared_ptr<TyValue> _ptr,
                 std::shared_ptr<TyValueType> _ptrty,
                 std::shared_ptr<TyValue> _v1,
                 std::shared_ptr<TyValueType> _intty,
                 std::shared_ptr<TyValue> _v2, std::shared_ptr<TySize> _a);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> ptr;
  std::shared_ptr<TyValueType> ptrty;
  std::shared_ptr<TyValue> v1;
  std::shared_ptr<TyValueType> intty;
  std::shared_ptr<TyValue> v2;
  std::shared_ptr<TySize> a;
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

struct TyMulShl {
public:
  TyMulShl(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y,
           std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _a,
           std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TySize> sz;
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

struct TyMulCommutative {
public:
  TyMulCommutative(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x,
                   std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> sz;
};

struct TyMulMone {
public:
  TyMulMone(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x,
            std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TySize> sz;
};

struct TyMulNeg {
public:
  TyMulNeg(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _mx,
           std::shared_ptr<TyValue> _my, std::shared_ptr<TyValue> _x,
           std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> mx;
  std::shared_ptr<TyValue> my;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
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

struct TyOrAndXor {
public:
  TyOrAndXor(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
             std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _a,
             std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> sz;
};

struct TyOrCommutative {
public:
  TyOrCommutative(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x,
                  std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
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

struct TyOrOr {
public:
  TyOrOr(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
         std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _a,
         std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> sz;
};

struct TyOrOr2 {
public:
  TyOrOr2(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
          std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _yprime,
          std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
          std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> yprime;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
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

struct TyOrXor {
public:
  TyOrXor(std::shared_ptr<TyValue> _w, std::shared_ptr<TyValue> _z,
          std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
          std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
          std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> w;
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> sz;
};

struct TyOrXor2 {
public:
  TyOrXor2(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x1,
           std::shared_ptr<TyValue> _y1, std::shared_ptr<TyValue> _x2,
           std::shared_ptr<TyValue> _y2, std::shared_ptr<TyValue> _a,
           std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> x1;
  std::shared_ptr<TyValue> y1;
  std::shared_ptr<TyValue> x2;
  std::shared_ptr<TyValue> y2;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> sz;
};

struct TyOrXor3 {
public:
  TyOrXor3(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _y,
           std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
           std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> sz;
};

struct TyOrXor4 {
public:
  TyOrXor4(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
           std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _a,
           std::shared_ptr<TyValue> _b, std::shared_ptr<TyValue> _nb,
           std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TyValue> nb;
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

struct TyTruncOnebit {
public:
  TyTruncOnebit(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
                std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _orgsz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> orgsz;
};

struct TyTruncPtrtoint {
public:
  TyTruncPtrtoint(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
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

struct TyTruncSext {
public:
  TyTruncSext(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
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

struct TyTruncZext {
public:
  TyTruncZext(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
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

struct TyRemNeg {
public:
  TyRemNeg(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _my,
           std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
           std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> my;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> sz;
};

struct TySdivMone {
public:
  TySdivMone(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x,
             std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TySize> sz;
};

struct TySdivSubSrem {
public:
  TySdivSubSrem(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _b,
                std::shared_ptr<TyRegister> _a, std::shared_ptr<TyValue> _x,
                std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> b;
  std::shared_ptr<TyRegister> a;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> sz;
};

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

struct TySextBitcast {
public:
  TySextBitcast(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
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

struct TySextSext {
public:
  TySextSext(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
             std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
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

struct TySitofpBitcast {
public:
  TySitofpBitcast(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
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

struct TySitofpSext {
public:
  TySitofpSext(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
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

struct TySitofpZext {
public:
  TySitofpZext(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
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

struct TySubAdd {
public:
  TySubAdd(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _my,
           std::shared_ptr<TyRegister> _x, std::shared_ptr<TyValue> _y,
           std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> my;
  std::shared_ptr<TyRegister> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> sz;
};

struct TySubConstAdd {
public:
  TySubConstAdd(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y,
                std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c1,
                std::shared_ptr<TyConstInt> _c2,
                std::shared_ptr<TyConstInt> _c3, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyConstInt> c1;
  std::shared_ptr<TyConstInt> c2;
  std::shared_ptr<TyConstInt> c3;
  std::shared_ptr<TySize> sz;
};

struct TySubConstNot {
public:
  TySubConstNot(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y,
                std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c1,
                std::shared_ptr<TyConstInt> _c2, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyConstInt> c1;
  std::shared_ptr<TyConstInt> c2;
  std::shared_ptr<TySize> sz;
};

struct TySubMone {
public:
  TySubMone(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x,
            std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TySize> sz;
};

struct TySubOnebit {
public:
  TySubOnebit(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x,
              std::shared_ptr<TyValue> _y);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
};

struct TySubOrXor {
public:
  TySubOrXor(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _a,
             std::shared_ptr<TyValue> _b, std::shared_ptr<TyRegister> _x,
             std::shared_ptr<TyRegister> _y, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TyRegister> x;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TySize> sz;
};

struct TySubSdiv {
public:
  TySubSdiv(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y,
            std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c,
            std::shared_ptr<TyConstInt> _cprime, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyConstInt> c;
  std::shared_ptr<TyConstInt> cprime;
  std::shared_ptr<TySize> sz;
};

struct TySubShl {
public:
  TySubShl(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x,
           std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _mx,
           std::shared_ptr<TyValue> _a, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyValue> mx;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TySize> sz;
};

struct TySubSub {
public:
  TySubSub(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x,
           std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _w,
           std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> w;
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

struct TyTruncBitcast {
public:
  TyTruncBitcast(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
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

struct TyTruncTrunc {
public:
  TyTruncTrunc(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
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

struct TyUdivSubUrem {
public:
  TyUdivSubUrem(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _b,
                std::shared_ptr<TyRegister> _a, std::shared_ptr<TyValue> _x,
                std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> b;
  std::shared_ptr<TyRegister> a;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> sz;
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

struct TyIntroEq {
public:
  TyIntroEq(std::shared_ptr<TyValue> _x);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> x;
};

struct TyInttoptrLoad {
public:
  TyInttoptrLoad(std::shared_ptr<TyValue> _ptr,
                 std::shared_ptr<TyValueType> _intty,
                 std::shared_ptr<TyValue> _v1,
                 std::shared_ptr<TyValueType> _ptrty,
                 std::shared_ptr<TyValue> _v2, std::shared_ptr<TySize> _a);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> ptr;
  std::shared_ptr<TyValueType> intty;
  std::shared_ptr<TyValue> v1;
  std::shared_ptr<TyValueType> ptrty;
  std::shared_ptr<TyValue> v2;
  std::shared_ptr<TySize> a;
};

struct TyInttoptrBitcast {
public:
  TyInttoptrBitcast(std::shared_ptr<TyValue> _src,
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

struct TySextZext {
public:
  TySextZext(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
             std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
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

struct TyUitofpBitcast {
public:
  TyUitofpBitcast(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
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

struct TyUitofpZext {
public:
  TyUitofpZext(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
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

struct TyZextBitcast {
public:
  TyZextBitcast(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
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

struct TyZextTruncAndXor {
public:
  TyZextTruncAndXor(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
                    std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _w,
                    std::shared_ptr<TyValue> _y,
                    std::shared_ptr<TyValue> _yprime,
                    std::shared_ptr<TyConstant> _c, std::shared_ptr<TySize> _s,
                    std::shared_ptr<TySize> _sprime);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> v;
  std::shared_ptr<TyValue> w;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> yprime;
  std::shared_ptr<TyConstant> c;
  std::shared_ptr<TySize> s;
  std::shared_ptr<TySize> sprime;
};

struct TyZextXor {
public:
  TyZextXor(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _y,
            std::shared_ptr<TyValue> _yprime, std::shared_ptr<TyValue> _x);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> yprime;
  std::shared_ptr<TyValue> x;
};

struct TyZextZext {
public:
  TyZextZext(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
             std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
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

struct TyUdivZext {
public:
  TyUdivZext(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x,
             std::shared_ptr<TyRegister> _y, std::shared_ptr<TyRegister> _k,
             std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
             std::shared_ptr<TySize> _sz1, std::shared_ptr<TySize> _sz2);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> x;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyRegister> k;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> sz1;
  std::shared_ptr<TySize> sz2;
};

struct TyUdivZextConst{
public : 
  TyUdivZextConst(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x, std::shared_ptr<TyConstInt> _c, std::shared_ptr<TyRegister> _k, std::shared_ptr<TyValue> _a, std::shared_ptr<TySize> _sz1, std::shared_ptr<TySize> _sz2);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> x;
  std::shared_ptr<TyConstInt> c;
  std::shared_ptr<TyRegister> k;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TySize> sz1;
  std::shared_ptr<TySize> sz2;
};

struct TyUremZext {
public:
  TyUremZext(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x,
             std::shared_ptr<TyRegister> _y, std::shared_ptr<TyRegister> _k,
             std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
             std::shared_ptr<TySize> _sz1, std::shared_ptr<TySize> _sz2);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> x;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyRegister> k;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> sz1;
  std::shared_ptr<TySize> sz2;
};

struct TyUremZextConst{
public : 
  TyUremZextConst(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x, std::shared_ptr<TyConstInt> _c, std::shared_ptr<TyRegister> _k, std::shared_ptr<TyValue> _a, std::shared_ptr<TySize> _sz1, std::shared_ptr<TySize> _sz2);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> x;
  std::shared_ptr<TyConstInt> c;
  std::shared_ptr<TyRegister> k;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TySize> sz1;
  std::shared_ptr<TySize> sz2;
};

struct TyXorCommutative {
public:
  TyXorCommutative(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x,
                   std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> sz;
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

struct TyIcmpSwapOperands {
public:
  TyIcmpSwapOperands(enum TyIcmpPred _predicate, std::shared_ptr<TyValueType> _ty,
                std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
                std::shared_ptr<TyValue> _z);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  enum TyIcmpPred predicate;
  std::shared_ptr<TyValueType> ty;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> z;
};

struct TyIcmpEqXorNot{
public : 
  TyIcmpEqXorNot(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> zprime;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> s;
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

struct TyIcmpNeXor{
public : 
  TyIcmpNeXor(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> s;
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

struct TyIcmpSgeOrNot{
public : 
  TyIcmpSgeOrNot(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> zprime;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> s;
};

struct TyIcmpSgtAndNot{
public : 
  TyIcmpSgtAndNot(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> zprime;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> s;
};

struct TyIcmpSleOrNot{
public : 
  TyIcmpSleOrNot(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> zprime;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> s;
};

struct TyIcmpSltAndNot{
public : 
  TyIcmpSltAndNot(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> zprime;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> s;
};

struct TyIcmpUgeOrNot{
public : 
  TyIcmpUgeOrNot(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> zprime;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> s;
};

struct TyIcmpUgtAndNot{
public : 
  TyIcmpUgtAndNot(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> zprime;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> s;
};

struct TyIcmpUleOrNot{
public : 
  TyIcmpUleOrNot(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> zprime;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> s;
};

struct TyIcmpUltAndNot{
public : 
  TyIcmpUltAndNot(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> zprime;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> s;
};

/////// constructor classes ///////

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

struct ConsAddConstNot : public TyInfrule {
public:
  ConsAddConstNot(std::shared_ptr<TyAddConstNot> _add_const_not);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y,
       std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c1,
       std::shared_ptr<TyConstInt> _c2, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyAddConstNot> add_const_not;
};

struct ConsAddDistSub : public TyInfrule {
public:
  ConsAddDistSub(std::shared_ptr<TyAddDistSub> _add_dist_sub);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _minusx,
       std::shared_ptr<TyValue> _minusy, std::shared_ptr<TyRegister> _w,
       std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
       std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyAddDistSub> add_dist_sub;
};

struct ConsAddOnebit : public TyInfrule {
public:
  ConsAddOnebit(std::shared_ptr<TyAddOnebit> _add_onebit);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _y);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyAddOnebit> add_onebit;
};

struct ConsAddMask : public TyInfrule {
public:
  ConsAddMask(std::shared_ptr<TyAddMask> _add_mask);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y,
       std::shared_ptr<TyRegister> _yprime, std::shared_ptr<TyValue> _x,
       std::shared_ptr<TyConstInt> _c1, std::shared_ptr<TyConstInt> _c2,
       std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyAddMask> add_mask;
};

struct ConsAddSelectZero : public TyInfrule {
public:
  ConsAddSelectZero(std::shared_ptr<TyAddSelectZero> _add_select_zero);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x,
       std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _c,
       std::shared_ptr<TyValue> _n, std::shared_ptr<TyValue> _a,
       std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyAddSelectZero> add_select_zero;
};

struct ConsAddSelectZero2 : public TyInfrule {
public:
  ConsAddSelectZero2(std::shared_ptr<TyAddSelectZero2> _add_select_zero2);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x,
       std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _c,
       std::shared_ptr<TyValue> _n, std::shared_ptr<TyValue> _a,
       std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyAddSelectZero2> add_select_zero2;
};

struct ConsAddSextSext : public TyInfrule {
public:
  ConsAddSextSext(std::shared_ptr<TyAddSextSext> _add_sext_sext);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _xprime,
       std::shared_ptr<TyValue> _x, std::shared_ptr<TyRegister> _yprime,
       std::shared_ptr<TyValue> _y, std::shared_ptr<TyRegister> _zprime,
       std::shared_ptr<TySize> _from_sz, std::shared_ptr<TySize> _to_sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyAddSextSext> add_sext_sext;
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

struct ConsAddOrAnd : public TyInfrule {
public:
  ConsAddOrAnd(std::shared_ptr<TyAddOrAnd> _add_or_and);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _a,
       std::shared_ptr<TyValue> _b, std::shared_ptr<TyRegister> _x,
       std::shared_ptr<TyRegister> _y, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyAddOrAnd> add_or_and;
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

struct ConsAddXorAnd : public TyInfrule {
public:
  ConsAddXorAnd(std::shared_ptr<TyAddXorAnd> _add_xor_and);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _a,
       std::shared_ptr<TyValue> _b, std::shared_ptr<TyRegister> _x,
       std::shared_ptr<TyRegister> _y, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyAddXorAnd> add_xor_and;
};

struct ConsAndCommutative : public TyInfrule {
public:
  ConsAndCommutative(std::shared_ptr<TyAndCommutative> _and_commutative);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyAndCommutative> and_commutative;
};

struct ConsAndDeMorgan : public TyInfrule {
public:
  ConsAndDeMorgan(std::shared_ptr<TyAndDeMorgan> _and_de_morgan);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x,
       std::shared_ptr<TyRegister> _y, std::shared_ptr<TyRegister> _zprime,
       std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
       std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyAndDeMorgan> and_de_morgan;
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

struct ConsAndOrConst2 : public TyInfrule {
public:
  ConsAndOrConst2(std::shared_ptr<TyAndOrConst2> _ans_or_const2);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y,
       std::shared_ptr<TyRegister> _yprime, std::shared_ptr<TyValue> _x,
       std::shared_ptr<TyConstInt> _c1, std::shared_ptr<TyConstInt> _c2,
       std::shared_ptr<TyConstInt> _c3, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyAndOrConst2> and_or_const2;
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

struct ConsAndOrNot1 : public TyInfrule {
public:
  ConsAndOrNot1(std::shared_ptr<TyAndOrNot1> _and_or_not1);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x,
       std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _a,
       std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyAndOrNot1> and_or_not1;
};

struct ConsBopDistributiveOverSelectinst : public TyInfrule {
public:
  ConsBopDistributiveOverSelectinst(std::shared_ptr<
      TyBopDistributiveOverSelectinst> _bop_distributive_over_selectinst);
  static std::shared_ptr<TyInfrule>
  make(TyBop _opcode, std::shared_ptr<TyRegister> _r,
       std::shared_ptr<TyRegister> _s, std::shared_ptr<TyRegister> _tprime,
       std::shared_ptr<TyRegister> _t0, std::shared_ptr<TyValue> _x,
       std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _z,
       std::shared_ptr<TyValue> _c, std::shared_ptr<TySize> _bopsz,
       std::shared_ptr<TyValueType> _selty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyBopDistributiveOverSelectinst>
      bop_distributive_over_selectinst;
};

struct ConsBopDistributiveOverSelectinst2 : public TyInfrule {
public:
  ConsBopDistributiveOverSelectinst2(std::shared_ptr<
      TyBopDistributiveOverSelectinst2> _bop_distributive_over_selectinst2);
  static std::shared_ptr<TyInfrule>
  make(TyBop _opcode, std::shared_ptr<TyRegister> _r,
       std::shared_ptr<TyRegister> _s, std::shared_ptr<TyRegister> _tprime,
       std::shared_ptr<TyRegister> _t0, std::shared_ptr<TyValue> _x,
       std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _z,
       std::shared_ptr<TyValue> _c, std::shared_ptr<TySize> _bopsz,
       std::shared_ptr<TyValueType> _selty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyBopDistributiveOverSelectinst2>
      bop_distributive_over_selectinst2;
};

struct ConsBitcastBitcast : public TyInfrule {
public:
  ConsBitcastBitcast(std::shared_ptr<TyBitcastBitcast> _bitcast_bitcast);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyBitcastBitcast> bitcast_bitcast;
};

struct ConsBitcastFpext : public TyInfrule {
public:
  ConsBitcastFpext(std::shared_ptr<TyBitcastFpext> _bitcast_fpext);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyBitcastFpext> bitcast_fpext;
};

struct ConsBitcastFptosi : public TyInfrule {
public:
  ConsBitcastFptosi(std::shared_ptr<TyBitcastFptosi> _bitcast_fptosi);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyBitcastFptosi> bitcast_fptosi;
};

struct ConsBitcastFptoui : public TyInfrule {
public:
  ConsBitcastFptoui(std::shared_ptr<TyBitcastFptoui> _bitcast_fptoui);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyBitcastFptoui> bitcast_fptoui;
};

struct ConsBitcastFptrunc : public TyInfrule {
public:
  ConsBitcastFptrunc(std::shared_ptr<TyBitcastFptrunc> _bitcast_fptrunc);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyBitcastFptrunc> bitcast_fptrunc;
};

struct ConsBitcastInttoptr : public TyInfrule {
public:
  ConsBitcastInttoptr(std::shared_ptr<TyBitcastInttoptr> _bitcast_inttoptr);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyBitcastInttoptr> bitcast_inttoptr;
};

struct ConsBitcastPtrtoint : public TyInfrule {
public:
  ConsBitcastPtrtoint(std::shared_ptr<TyBitcastPtrtoint> _bitcast_ptrtoint);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyBitcastPtrtoint> bitcast_ptrtoint;
};

struct ConsBitcastSext : public TyInfrule {
public:
  ConsBitcastSext(std::shared_ptr<TyBitcastSext> _bitcast_sext);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyBitcastSext> bitcast_sext;
};

struct ConsBitcastSametype : public TyInfrule {
public:
  ConsBitcastSametype(std::shared_ptr<TyBitcastSametype> _bitcast_sametype);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _tty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyBitcastSametype> bitcast_sametype;
};

struct ConsBitcastSitofp : public TyInfrule {
public:
  ConsBitcastSitofp(std::shared_ptr<TyBitcastSitofp> _bitcast_sitofp);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyBitcastSitofp> bitcast_sitofp;
};

struct ConsBitcastTrunc : public TyInfrule {
public:
  ConsBitcastTrunc(std::shared_ptr<TyBitcastTrunc> _bitcast_trunc);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyBitcastTrunc> bitcast_trunc;
};

struct ConsBitcastUitofp : public TyInfrule {
public:
  ConsBitcastUitofp(std::shared_ptr<TyBitcastUitofp> _bitcast_uitofp);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyBitcastUitofp> bitcast_uitofp;
};

struct ConsBitcastZext : public TyInfrule {
public:
  ConsBitcastZext(std::shared_ptr<TyBitcastZext> _bitcast_zext);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyBitcastZext> bitcast_zext;
};

struct ConsBitcastLoad : public TyInfrule {
public:
  ConsBitcastLoad(std::shared_ptr<TyBitcastLoad> _bitcast_load);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyValue> _ptr, std::shared_ptr<TyValueType> _ptrty,
       std::shared_ptr<TyValue> _v1, std::shared_ptr<TyValueType> _ptrty2,
       std::shared_ptr<TyValue> _v2, std::shared_ptr<TySize> _a);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyBitcastLoad> bitcast_load;
};

struct ConsBitcastptr : public TyInfrule {
public:
  ConsBitcastptr(std::shared_ptr<TyBitcastptr> _bitcastptr);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _v,
                                         std::shared_ptr<TyValue> _vprime,
                                         std::shared_ptr<TyExpr> _bitcastinst);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyBitcastptr> bitcastptr;
};

struct ConsDiffblockGlobalAlloca : public TyInfrule {
public:
  ConsDiffblockGlobalAlloca(
      std::shared_ptr<TyDiffblockGlobalAlloca> _diffblock_global_alloca);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyConstant> _gx,
                                         std::shared_ptr<TyRegister> _y);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyDiffblockGlobalAlloca> diffblock_global_alloca;
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

struct ConsFbopDistributiveOverSelectinst : public TyInfrule {
public:
  ConsFbopDistributiveOverSelectinst(std::shared_ptr<
      TyFbopDistributiveOverSelectinst> _fbop_distributive_over_selectinst);
  static std::shared_ptr<TyInfrule>
  make(TyFbop _fopcode, std::shared_ptr<TyRegister> _r,
       std::shared_ptr<TyRegister> _s, std::shared_ptr<TyRegister> _tprime,
       std::shared_ptr<TyRegister> _t0, std::shared_ptr<TyValue> _x,
       std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _z,
       std::shared_ptr<TyValue> _c, TyFloatType _fbopty,
       std::shared_ptr<TyValueType> _selty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyFbopDistributiveOverSelectinst>
      fbop_distributive_over_selectinst;
};

struct ConsFbopDistributiveOverSelectinst2 : public TyInfrule {
public:
  ConsFbopDistributiveOverSelectinst2(std::shared_ptr<
      TyFbopDistributiveOverSelectinst2> _fbop_distributive_over_selectinst2);
  static std::shared_ptr<TyInfrule>
  make(TyFbop _fopcode, std::shared_ptr<TyRegister> _r,
       std::shared_ptr<TyRegister> _s, std::shared_ptr<TyRegister> _tprime,
       std::shared_ptr<TyRegister> _t0, std::shared_ptr<TyValue> _x,
       std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _z,
       std::shared_ptr<TyValue> _c, TyFloatType _fbopty,
       std::shared_ptr<TyValueType> _selty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyFbopDistributiveOverSelectinst2>
      fbop_distributive_over_selectinst2;
};

struct ConsFpextBitcast : public TyInfrule {
public:
  ConsFpextBitcast(std::shared_ptr<TyFpextBitcast> _fpext_bitcast);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyFpextBitcast> fpext_bitcast;
};

struct ConsFpextFpext : public TyInfrule {
public:
  ConsFpextFpext(std::shared_ptr<TyFpextFpext> _fpext_fpext);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyFpextFpext> fpext_fpext;
};

struct ConsFptosiBitcast : public TyInfrule {
public:
  ConsFptosiBitcast(std::shared_ptr<TyFptosiBitcast> _fptosi_bitcast);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyFptosiBitcast> fptosi_bitcast;
};

struct ConsFptosiFpext : public TyInfrule {
public:
  ConsFptosiFpext(std::shared_ptr<TyFptosiFpext> _fpext_fptosi);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyFptosiFpext> fpext_fptosi;
};

struct ConsFptouiBitcast : public TyInfrule {
public:
  ConsFptouiBitcast(std::shared_ptr<TyFptouiBitcast> _fptoui_bitcast);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyFptouiBitcast> fptoui_bitcast;
};

struct ConsFptouiFpext : public TyInfrule {
public:
  ConsFptouiFpext(std::shared_ptr<TyFptouiFpext> _fpext_fptoui);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyFptouiFpext> fpext_fptoui;
};

struct ConsFptruncBitcast : public TyInfrule {
public:
  ConsFptruncBitcast(std::shared_ptr<TyFptruncBitcast> _fptrunc_bitcast);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyFptruncBitcast> fptrunc_bitcast;
};

struct ConsFptruncFpext : public TyInfrule {
public:
  ConsFptruncFpext(std::shared_ptr<TyFptruncFpext> _fptrunc_fpext);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyFptruncFpext> fptrunc_fpext;
};

struct ConsGepzero : public TyInfrule {
public:
  ConsGepzero(std::shared_ptr<TyGepzero> _gepzero);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _v,
                                         std::shared_ptr<TyValue> _vprime,
                                         std::shared_ptr<TyExpr> _gepinst);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyGepzero> gepzero;
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

struct ConsPtrtointBitcast : public TyInfrule {
public:
  ConsPtrtointBitcast(std::shared_ptr<TyPtrtointBitcast> _inttoptr_bitcast);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyPtrtointBitcast> inttoptr_bitcast;
};

struct ConsPtrtointLoad : public TyInfrule {
public:
  ConsPtrtointLoad(std::shared_ptr<TyPtrtointLoad> _ptrtoint_load);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyValue> _ptr, std::shared_ptr<TyValueType> _ptrty,
       std::shared_ptr<TyValue> _v1, std::shared_ptr<TyValueType> _intty,
       std::shared_ptr<TyValue> _v2, std::shared_ptr<TySize> _a);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyPtrtointLoad> ptrtoint_load;
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

struct ConsMulCommutative : public TyInfrule {
public:
  ConsMulCommutative(std::shared_ptr<TyMulCommutative> _mul_commutative);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyMulCommutative> mul_commutative;
};

struct ConsMulMone : public TyInfrule {
public:
  ConsMulMone(std::shared_ptr<TyMulMone> _mul_mone);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyMulMone> mul_mone;
};

struct ConsMulNeg : TyInfrule {
public:
  ConsMulNeg(std::shared_ptr<TyMulNeg> _mul_neg);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _mx,
       std::shared_ptr<TyValue> _my, std::shared_ptr<TyValue> _x,
       std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz);

private:
  std::shared_ptr<TyMulNeg> mul_neg;
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

struct ConsAddZextBool : public TyInfrule {
public:
  ConsAddZextBool(std::shared_ptr<TyAddZextBool> _add_zext_bool);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y,
       std::shared_ptr<TyValue> _b, std::shared_ptr<TyConstInt> _c,
       std::shared_ptr<TyConstInt> _cprime, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyAddZextBool> add_zext_bool;
};

struct ConsMulShl : public TyInfrule {
public:
  ConsMulShl(std::shared_ptr<TyMulShl> _mul_shl);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z,
                                         std::shared_ptr<TyRegister> _y,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _a,
                                         std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyMulShl> mul_shl;
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

struct ConsOrAndXor : public TyInfrule {
public:
  ConsOrAndXor(std::shared_ptr<TyOrAndXor> _or_and_xor);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
       std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _a,
       std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyOrAndXor> or_and_xor;
};

struct ConsOrCommutative : public TyInfrule {
public:
  ConsOrCommutative(std::shared_ptr<TyOrCommutative> _or_commutative);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyOrCommutative> or_commutative;
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

struct ConsOrOr : public TyInfrule {
public:
  ConsOrOr(std::shared_ptr<TyOrOr> _or_or);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
       std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _a,
       std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyOrOr> or_or;
};

struct ConsOrOr2 : public TyInfrule {
public:
  ConsOrOr2(std::shared_ptr<TyOrOr2> _or_or2);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
       std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _yprime,
       std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
       std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyOrOr2> or_or2;
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

struct ConsOrXor : public TyInfrule {
public:
  ConsOrXor(std::shared_ptr<TyOrXor> _or_xor);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyValue> _w, std::shared_ptr<TyValue> _z,
       std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
       std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
       std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyOrXor> or_xor;
};

struct ConsOrXor2 : public TyInfrule {
public:
  ConsOrXor2(std::shared_ptr<TyOrXor2> _or_xor2);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x1,
       std::shared_ptr<TyValue> _y1, std::shared_ptr<TyValue> _x2,
       std::shared_ptr<TyValue> _y2, std::shared_ptr<TyValue> _a,
       std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyOrXor2> or_xor2;
};

struct ConsOrXor3 : public TyInfrule {
public:
  ConsOrXor3(std::shared_ptr<TyOrXor3> _or_xor3);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TyValue> _a,
                                         std::shared_ptr<TyValue> _b,
                                         std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyOrXor3> or_xor3;
};

struct ConsOrXor4 : public TyInfrule {
public:
  ConsOrXor4(std::shared_ptr<TyOrXor4> _or_xor4);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
       std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _a,
       std::shared_ptr<TyValue> _b, std::shared_ptr<TyValue> _nb,
       std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyOrXor4> or_xor4;
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

struct ConsRemNeg : public TyInfrule {
public:
  ConsRemNeg(std::shared_ptr<TyRemNeg> _rem_neg);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z,
                                         std::shared_ptr<TyValue> _my,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRemNeg> rem_neg;
};

struct ConsSdivMone : public TyInfrule {
public:
  ConsSdivMone(std::shared_ptr<TySdivMone> _div_mone);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySdivMone> div_mone;
};

struct ConsSextSext : public TyInfrule {
public:
  ConsSextSext(std::shared_ptr<TySextSext> _sext_sext);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySextSext> sext_sext;
};

struct ConsSitofpBitcast : public TyInfrule {
public:
  ConsSitofpBitcast(std::shared_ptr<TySitofpBitcast> _sitofp_bitcast);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySitofpBitcast> sitofp_bitcast;
};

struct ConsSitofpZext : public TyInfrule {
public:
  ConsSitofpZext(std::shared_ptr<TySitofpZext> _sitofp_zext);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySitofpZext> sitofp_zext;
};

struct ConsSitofpSext : public TyInfrule {
public:
  ConsSitofpSext(std::shared_ptr<TySitofpSext> _sext_sitofp);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySitofpSext> sext_sitofp;
};

struct ConsSextAshr : public TyInfrule {
public:
  ConsSextAshr(std::shared_ptr<TySextAshr> _sext_ashr);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime,
       std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _x0,
       std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _w,
       std::shared_ptr<TyConstInt> _c1, std::shared_ptr<TyConstInt> _c2,
       std::shared_ptr<TySize> _sz1, std::shared_ptr<TySize> _sz2);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySextAshr> sext_ashr;
};

struct ConsSextTrunc : public TyInfrule {
public:
  ConsSextTrunc(std::shared_ptr<TySextTrunc> _sext_trunc);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
       std::shared_ptr<TyValue> _y, std::shared_ptr<TyConstInt> _c,
       std::shared_ptr<TySize> _sz1, std::shared_ptr<TySize> _sz2);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySextTrunc> sext_trunc;
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

struct ConsSubMone : public TyInfrule {
public:
  ConsSubMone(std::shared_ptr<TySubMone> _sub_mone);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySubMone> sub_mone;
};

struct ConsSubOnebit : public TyInfrule {
public:
  ConsSubOnebit(std::shared_ptr<TySubOnebit> _sub_onebit);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _y);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySubOnebit> sub_onebit;
};

struct ConsSubConstAdd : public TyInfrule {
public:
  ConsSubConstAdd(std::shared_ptr<TySubConstAdd> _sub_const_add);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y,
       std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c1,
       std::shared_ptr<TyConstInt> _c2, std::shared_ptr<TyConstInt> _c3,
       std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySubConstAdd> sub_const_add;
};

struct ConsSubConstNot : public TyInfrule {
public:
  ConsSubConstNot(std::shared_ptr<TySubConstNot> _sub_const_not);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y,
       std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c1,
       std::shared_ptr<TyConstInt> _c2, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySubConstNot> sub_const_not;
};

struct ConsSubOrXor : public TyInfrule {
public:
  ConsSubOrXor(std::shared_ptr<TySubOrXor> _sub_or_xor);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _a,
       std::shared_ptr<TyValue> _b, std::shared_ptr<TyRegister> _x,
       std::shared_ptr<TyRegister> _y, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySubOrXor> sub_or_xor;
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

struct ConsSubSdiv : public TyInfrule {
public:
  ConsSubSdiv(std::shared_ptr<TySubSdiv> _sub_sdiv);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y,
       std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c,
       std::shared_ptr<TyConstInt> _cprime, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySubSdiv> sub_sdiv;
};

struct ConsSubSub : public TyInfrule {
public:
  ConsSubSub(std::shared_ptr<TySubSub> _sub_sub);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TyValue> _w,
                                         std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySubSub> sub_sub;
};

struct ConsSubShl : public TyInfrule {
public:
  ConsSubShl(std::shared_ptr<TySubShl> _sub_shl);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x,
       std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _mx,
       std::shared_ptr<TyValue> _a, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySubShl> sub_shl;
};

struct ConsSdivSubSrem : public TyInfrule {
public:
  ConsSdivSubSrem(std::shared_ptr<TySdivSubSrem> _div_sub_srem);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _b,
       std::shared_ptr<TyRegister> _a, std::shared_ptr<TyValue> _x,
       std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySdivSubSrem> div_sub_srem;
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

struct ConsTruncBitcast : public TyInfrule {
public:
  ConsTruncBitcast(std::shared_ptr<TyTruncBitcast> _trunc_bitcast);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyTruncBitcast> trunc_bitcast;
};

struct ConsTruncOnebit : public TyInfrule {
public:
  ConsTruncOnebit(std::shared_ptr<TyTruncOnebit> _trunc_onebit);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TySize> _orgsz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyTruncOnebit> trunc_onebit;
};

struct ConsTruncPtrtoint : public TyInfrule {
public:
  ConsTruncPtrtoint(std::shared_ptr<TyTruncPtrtoint> _ptrtoint_trunc);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyTruncPtrtoint> ptrtoint_trunc;
};

struct ConsTruncSext : public TyInfrule {
public:
  ConsTruncSext(std::shared_ptr<TyTruncSext> _trunc_sext);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyTruncSext> trunc_sext;
};

struct ConsTruncTrunc : public TyInfrule {
public:
  ConsTruncTrunc(std::shared_ptr<TyTruncTrunc> _trunc_trunc);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyTruncTrunc> trunc_trunc;
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

struct ConsTruncZext : public TyInfrule {
public:
  ConsTruncZext(std::shared_ptr<TyTruncZext> _trunc_zext);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyTruncZext> trunc_zext;
};

struct ConsUdivSubUrem : public TyInfrule {
public:
  ConsUdivSubUrem(std::shared_ptr<TyUdivSubUrem> _div_sub_urem);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _b,
       std::shared_ptr<TyRegister> _a, std::shared_ptr<TyValue> _x,
       std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyUdivSubUrem> div_sub_urem;
};

struct ConsUdivZext : public TyInfrule {
public:
  ConsUdivZext(std::shared_ptr<TyUdivZext> _udiv_zext);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x,
       std::shared_ptr<TyRegister> _y, std::shared_ptr<TyRegister> _k,
       std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
       std::shared_ptr<TySize> _sz1, std::shared_ptr<TySize> _sz2);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyUdivZext> udiv_zext;
};

struct ConsUdivZextConst : public TyInfrule{
public : 
  ConsUdivZextConst(std::shared_ptr<TyUdivZextConst> _udiv_zext_const);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x, std::shared_ptr<TyConstInt> _c, std::shared_ptr<TyRegister> _k, std::shared_ptr<TyValue> _a, std::shared_ptr<TySize> _sz1, std::shared_ptr<TySize> _sz2);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyUdivZextConst> udiv_zext_const;
};

struct ConsUremZext : public TyInfrule {
public:
  ConsUremZext(std::shared_ptr<TyUremZext> _urem_zext);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x,
       std::shared_ptr<TyRegister> _y, std::shared_ptr<TyRegister> _k,
       std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
       std::shared_ptr<TySize> _sz1, std::shared_ptr<TySize> _sz2);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyUremZext> urem_zext;
};

struct ConsUremZextConst : public TyInfrule{
public : 
  ConsUremZextConst(std::shared_ptr<TyUremZextConst> _urem_zext_const);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x, std::shared_ptr<TyConstInt> _c, std::shared_ptr<TyRegister> _k, std::shared_ptr<TyValue> _a, std::shared_ptr<TySize> _sz1, std::shared_ptr<TySize> _sz2);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyUremZextConst> urem_zext_const;
};

struct ConsInttoptrLoad : public TyInfrule {
public:
  ConsInttoptrLoad(std::shared_ptr<TyInttoptrLoad> _inttoptr_load);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyValue> _ptr, std::shared_ptr<TyValueType> _intty,
       std::shared_ptr<TyValue> _v1, std::shared_ptr<TyValueType> _ptrty,
       std::shared_ptr<TyValue> _v2, std::shared_ptr<TySize> _a);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyInttoptrLoad> inttoptr_load;
};

struct ConsInttoptrBitcast : public TyInfrule {
public:
  ConsInttoptrBitcast(std::shared_ptr<TyInttoptrBitcast> _inttoptr_bitcast);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyInttoptrBitcast> inttoptr_bitcast;
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

struct ConsSextBitcast : public TyInfrule {
public:
  ConsSextBitcast(std::shared_ptr<TySextBitcast> _sext_bitcast);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySextBitcast> sext_bitcast;
};

struct ConsSextZext : public TyInfrule {
public:
  ConsSextZext(std::shared_ptr<TySextZext> _zext_sext);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySextZext> zext_sext;
};

struct ConsUitofpBitcast : public TyInfrule {
public:
  ConsUitofpBitcast(std::shared_ptr<TyUitofpBitcast> _uitofp_bitcast);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyUitofpBitcast> uitofp_bitcast;
};

struct ConsUitofpZext : public TyInfrule {
public:
  ConsUitofpZext(std::shared_ptr<TyUitofpZext> _zext_uitofp);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyUitofpZext> zext_uitofp;
};

struct ConsZextBitcast : public TyInfrule {
public:
  ConsZextBitcast(std::shared_ptr<TyZextBitcast> _zext_bitcast);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyZextBitcast> zext_bitcast;
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

struct ConsZextTruncAndXor : public TyInfrule {
public:
  ConsZextTruncAndXor(std::shared_ptr<TyZextTruncAndXor> _zext_trunc_and_xor);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
       std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _w,
       std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _yprime,
       std::shared_ptr<TyConstant> _c, std::shared_ptr<TySize> _s,
       std::shared_ptr<TySize> _sprime);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyZextTruncAndXor> zext_trunc_and_xor;
};

struct ConsZextXor : public TyInfrule {
public:
  ConsZextXor(std::shared_ptr<TyZextXor> _zext_xor);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TyValue> _yprime,
                                         std::shared_ptr<TyValue> _x);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyZextXor> zext_xor;
};

struct ConsZextZext : public TyInfrule {
public:
  ConsZextZext(std::shared_ptr<TyZextZext> _zext_zext);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyZextZext> zext_zext;
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

struct ConsIntroEq : public TyInfrule {
public:
  ConsIntroEq(std::shared_ptr<TyIntroEq> _intro_eq);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _x);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyIntroEq> intro_eq;
};

struct ConsXorCommutative : public TyInfrule {
public:
  ConsXorCommutative(std::shared_ptr<TyXorCommutative> _xor_commutative);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyXorCommutative> xor_commutative;
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

struct ConsIcmpEqXorNot : public TyInfrule{
public : 
  ConsIcmpEqXorNot(std::shared_ptr<TyIcmpEqXorNot> _icmp_eq_xor_not);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyIcmpEqXorNot> icmp_eq_xor_not;
};

struct ConsIcmpInverse : public TyInfrule {
public:
  ConsIcmpInverse(std::shared_ptr<TyIcmpInverse> _icmp_inverse);
  static std::shared_ptr<TyInfrule> make(llvm::ICmpInst &CI, int boolean);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyIcmpInverse> icmp_inverse;
};

struct ConsIcmpNeXor : public TyInfrule{
public : 
  ConsIcmpNeXor(std::shared_ptr<TyIcmpNeXor> _icmp_ne_xor);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyIcmpNeXor> icmp_ne_xor;
};

struct ConsIcmpSgeOrNot : public TyInfrule{
public : 
  ConsIcmpSgeOrNot(std::shared_ptr<TyIcmpSgeOrNot> _icmp_sge_or_not);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyIcmpSgeOrNot> icmp_sge_or_not;
};

struct ConsIcmpSgtAndNot : public TyInfrule{
public : 
  ConsIcmpSgtAndNot(std::shared_ptr<TyIcmpSgtAndNot> _icmp_sgt_and_not);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyIcmpSgtAndNot> icmp_sgt_and_not;
};

struct ConsIcmpSleOrNot : public TyInfrule{
public : 
  ConsIcmpSleOrNot(std::shared_ptr<TyIcmpSleOrNot> _icmp_sle_or_not);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyIcmpSleOrNot> icmp_sle_or_not;
};

struct ConsIcmpSltAndNot : public TyInfrule{
public : 
  ConsIcmpSltAndNot(std::shared_ptr<TyIcmpSltAndNot> _icmp_slt_and_not);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyIcmpSltAndNot> icmp_slt_and_not;
};

struct ConsIcmpSwapOperands : public TyInfrule {
public:
  ConsIcmpSwapOperands(std::shared_ptr<TyIcmpSwapOperands> _icmp_swap_operands);
  static std::shared_ptr<TyInfrule> make(llvm::ICmpInst &CI);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyIcmpSwapOperands> icmp_swap_operands;
};

struct ConsIcmpUgeOrNot : public TyInfrule{
public : 
  ConsIcmpUgeOrNot(std::shared_ptr<TyIcmpUgeOrNot> _icmp_uge_or_not);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyIcmpUgeOrNot> icmp_uge_or_not;
};

struct ConsIcmpUgtAndNot : public TyInfrule{
public : 
  ConsIcmpUgtAndNot(std::shared_ptr<TyIcmpUgtAndNot> _icmp_ugt_and_not);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyIcmpUgtAndNot> icmp_ugt_and_not;
};

struct ConsIcmpUleOrNot : public TyInfrule{
public : 
  ConsIcmpUleOrNot(std::shared_ptr<TyIcmpUleOrNot> _icmp_sle_or_not);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyIcmpUleOrNot> icmp_ule_or_not;
};

struct ConsIcmpUltAndNot : public TyInfrule{
public : 
  ConsIcmpUltAndNot(std::shared_ptr<TyIcmpUltAndNot> _icmp_ult_and_not);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyIcmpUltAndNot> icmp_ult_and_not;
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

} // llvmberry

#endif
