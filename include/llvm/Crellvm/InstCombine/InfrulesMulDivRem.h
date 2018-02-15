#ifndef INFRULES_MUL_DIV_REM
#define INFRUELS_MUL_DIV_REM

#include "llvm/Crellvm/Structure.h"
#include "llvm/IR/Instructions.h"
#include "cereal/types/memory.hpp"
#include "cereal/archives/json.hpp"

namespace crellvm {

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

struct ConsUdivZextConst : public TyInfrule{
public : 
  ConsUdivZextConst(std::shared_ptr<TyUdivZextConst> _udiv_zext_const);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x, std::shared_ptr<TyConstInt> _c, std::shared_ptr<TyRegister> _k, std::shared_ptr<TyValue> _a, std::shared_ptr<TySize> _sz1, std::shared_ptr<TySize> _sz2);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyUdivZextConst> udiv_zext_const;
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

struct ConsUremZextConst : public TyInfrule{
public : 
  ConsUremZextConst(std::shared_ptr<TyUremZextConst> _urem_zext_const);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x, std::shared_ptr<TyConstInt> _c, std::shared_ptr<TyRegister> _k, std::shared_ptr<TyValue> _a, std::shared_ptr<TySize> _sz1, std::shared_ptr<TySize> _sz2);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyUremZextConst> urem_zext_const;
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



} // crellvm

#endif
