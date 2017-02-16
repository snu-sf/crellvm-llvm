#ifndef INFRULES_AND_OR_XOR
#define INFRUELS_AND_OR_XOR

#include "llvm/LLVMBerry/Structure.h"
#include "llvm/IR/Instructions.h"
#include "cereal/types/memory.hpp"
#include "cereal/archives/json.hpp"

namespace llvmberry {

struct TyAndXorConst {
public:
  TyAndXorConst(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y,
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

struct ConsAndXorConst : public TyInfrule {
public:
  ConsAndXorConst(std::shared_ptr<TyAndXorConst> _and_xor_const);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y,
       std::shared_ptr<TyRegister> _yprime, std::shared_ptr<TyValue> _x,
       std::shared_ptr<TyConstInt> _c1, std::shared_ptr<TyConstInt> _c2,
       std::shared_ptr<TyConstInt> _c3, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyAndXorConst> and_xor_const;
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

} // llvmberry

#endif
