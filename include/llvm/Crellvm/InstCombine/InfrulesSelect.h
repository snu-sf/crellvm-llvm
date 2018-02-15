#ifndef INFRULES_SELECT
#define INFRUELS_SELECT

#include "llvm/Crellvm/Structure.h"
#include "llvm/IR/Instructions.h"
#include "cereal/types/memory.hpp"
#include "cereal/archives/json.hpp"

namespace crellvm {

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

struct TySelectIcmpSgtConst {
public:
  TySelectIcmpSgtConst(std::shared_ptr<TyRegister> _z,
                       std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _x,
                       std::shared_ptr<TyConstInt> _c,
                       std::shared_ptr<TyConstInt> _cprime, bool _selcomm,
                       std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyConstInt> c;
  std::shared_ptr<TyConstInt> cprime;
  bool selcomm;
  std::shared_ptr<TySize> s;
};

struct ConsSelectIcmpSgtConst : public TyInfrule {
public:
  ConsSelectIcmpSgtConst(
      std::shared_ptr<TySelectIcmpSgtConst> _select_icmp_sgt_const);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _y,
       std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c,
       std::shared_ptr<TyConstInt> _cprime, bool _selcomm,
       std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySelectIcmpSgtConst> select_icmp_sgt_const;
};

struct TySelectIcmpUltConst {
public:
  TySelectIcmpUltConst(std::shared_ptr<TyRegister> _z,
                       std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _x,
                       std::shared_ptr<TyConstInt> _c,
                       std::shared_ptr<TyConstInt> _cprime, bool _selcomm,
                       std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyConstInt> c;
  std::shared_ptr<TyConstInt> cprime;
  bool selcomm;
  std::shared_ptr<TySize> s;
};

struct ConsSelectIcmpUltConst : public TyInfrule {
public:
  ConsSelectIcmpUltConst(
      std::shared_ptr<TySelectIcmpUltConst> _select_icmp_ult_const);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _y,
       std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c,
       std::shared_ptr<TyConstInt> _cprime, bool _selcomm,
       std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySelectIcmpUltConst> select_icmp_ult_const;
};

struct TySelectIcmpUgtConst {
public:
  TySelectIcmpUgtConst(std::shared_ptr<TyRegister> _z,
                       std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _x,
                       std::shared_ptr<TyConstInt> _c,
                       std::shared_ptr<TyConstInt> _cprime, bool _selcomm,
                       std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyConstInt> c;
  std::shared_ptr<TyConstInt> cprime;
  bool selcomm;
  std::shared_ptr<TySize> s;
};

struct ConsSelectIcmpUgtConst : public TyInfrule {
public:
  ConsSelectIcmpUgtConst(
      std::shared_ptr<TySelectIcmpUgtConst> _select_icmp_ugt_const);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _y,
       std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c,
       std::shared_ptr<TyConstInt> _cprime, bool _selcomm,
       std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySelectIcmpUgtConst> select_icmp_ugt_const;
};

struct TySelectIcmpSltConst {
public:
  TySelectIcmpSltConst(std::shared_ptr<TyRegister> _z,
                       std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _x,
                       std::shared_ptr<TyConstInt> _c,
                       std::shared_ptr<TyConstInt> _cprime, bool _selcomm,
                       std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyConstInt> c;
  std::shared_ptr<TyConstInt> cprime;
  bool selcomm;
  std::shared_ptr<TySize> s;
};

struct ConsSelectIcmpSltConst : public TyInfrule {
public:
  ConsSelectIcmpSltConst(
      std::shared_ptr<TySelectIcmpSltConst> _select_icmp_slt_const);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _y,
       std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c,
       std::shared_ptr<TyConstInt> _cprime, bool _selcomm,
       std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySelectIcmpSltConst> select_icmp_slt_const;
};

struct TySelectIcmpEq {
public:
  TySelectIcmpEq(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _y,
                 std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _v,
                 std::shared_ptr<TyConstant> _c,
                 std::shared_ptr<TyValueType> _cty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> v;
  std::shared_ptr<TyConstant> c;
  std::shared_ptr<TyValueType> cty;
};

struct ConsSelectIcmpEq : public TyInfrule {
public:
  ConsSelectIcmpEq(std::shared_ptr<TySelectIcmpEq> _select_icmp_eq);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _y,
       std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _v,
       std::shared_ptr<TyConstant> _c, std::shared_ptr<TyValueType> _cty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySelectIcmpEq> select_icmp_eq;
};

struct TySelectIcmpNe {
public:
  TySelectIcmpNe(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _y,
                 std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _v,
                 std::shared_ptr<TyConstant> _c,
                 std::shared_ptr<TyValueType> _cty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> v;
  std::shared_ptr<TyConstant> c;
  std::shared_ptr<TyValueType> cty;
};

struct ConsSelectIcmpNe : public TyInfrule {
public:
  ConsSelectIcmpNe(std::shared_ptr<TySelectIcmpNe> _select_icmp_ne);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _y,
       std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _v,
       std::shared_ptr<TyConstant> _c, std::shared_ptr<TyValueType> _cty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySelectIcmpNe> select_icmp_ne;
};

struct TySelectIcmpEqXor1 {
public:
  TySelectIcmpEqXor1(std::shared_ptr<TyValue> _z,
                     std::shared_ptr<TyValue> _zprime,
                     std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _x,
                     std::shared_ptr<TyValue> _u, std::shared_ptr<TyValue> _w,
                     std::shared_ptr<TyConstInt> _c,
                     std::shared_ptr<TyConstInt> _cprime,
                     std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> zprime;
  std::shared_ptr<TyValue> v;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> u;
  std::shared_ptr<TyValue> w;
  std::shared_ptr<TyConstInt> c;
  std::shared_ptr<TyConstInt> cprime;
  std::shared_ptr<TySize> s;
};

struct ConsSelectIcmpEqXor1 : public TyInfrule {
public:
  ConsSelectIcmpEqXor1(
      std::shared_ptr<TySelectIcmpEqXor1> _select_icmp_eq_xor1);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime,
       std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _x,
       std::shared_ptr<TyValue> _u, std::shared_ptr<TyValue> _w,
       std::shared_ptr<TyConstInt> _c, std::shared_ptr<TyConstInt> _cprime,
       std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySelectIcmpEqXor1> select_icmp_eq_xor1;
};

struct TySelectIcmpEqXor2 {
public:
  TySelectIcmpEqXor2(std::shared_ptr<TyValue> _z,
                     std::shared_ptr<TyValue> _zprime,
                     std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _x,
                     std::shared_ptr<TyValue> _u, std::shared_ptr<TyValue> _w,
                     std::shared_ptr<TyConstInt> _c,
                     std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> zprime;
  std::shared_ptr<TyValue> v;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> u;
  std::shared_ptr<TyValue> w;
  std::shared_ptr<TyConstInt> c;
  std::shared_ptr<TySize> s;
};

struct ConsSelectIcmpEqXor2 : public TyInfrule {
public:
  ConsSelectIcmpEqXor2(
      std::shared_ptr<TySelectIcmpEqXor2> _select_icmp_eq_xor2);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime,
       std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _x,
       std::shared_ptr<TyValue> _u, std::shared_ptr<TyValue> _w,
       std::shared_ptr<TyConstInt> _c, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySelectIcmpEqXor2> select_icmp_eq_xor2;
};

struct TySelectIcmpNeXor1 {
public:
  TySelectIcmpNeXor1(std::shared_ptr<TyValue> _z,
                     std::shared_ptr<TyValue> _zprime,
                     std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _x,
                     std::shared_ptr<TyValue> _u, std::shared_ptr<TyValue> _w,
                     std::shared_ptr<TyConstInt> _c,
                     std::shared_ptr<TyConstInt> _cprime,
                     std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> zprime;
  std::shared_ptr<TyValue> v;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> u;
  std::shared_ptr<TyValue> w;
  std::shared_ptr<TyConstInt> c;
  std::shared_ptr<TyConstInt> cprime;
  std::shared_ptr<TySize> s;
};

struct ConsSelectIcmpNeXor1 : public TyInfrule {
public:
  ConsSelectIcmpNeXor1(
      std::shared_ptr<TySelectIcmpNeXor1> _select_icmp_ne_xor1);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime,
       std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _x,
       std::shared_ptr<TyValue> _u, std::shared_ptr<TyValue> _w,
       std::shared_ptr<TyConstInt> _c, std::shared_ptr<TyConstInt> _cprime,
       std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySelectIcmpNeXor1> select_icmp_ne_xor1;
};

struct TySelectIcmpNeXor2 {
public:
  TySelectIcmpNeXor2(std::shared_ptr<TyValue> _z,
                     std::shared_ptr<TyValue> _zprime,
                     std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _x,
                     std::shared_ptr<TyValue> _u, std::shared_ptr<TyValue> _w,
                     std::shared_ptr<TyConstInt> _c,
                     std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> zprime;
  std::shared_ptr<TyValue> v;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> u;
  std::shared_ptr<TyValue> w;
  std::shared_ptr<TyConstInt> c;
  std::shared_ptr<TySize> s;
};

struct ConsSelectIcmpNeXor2 : public TyInfrule {
public:
  ConsSelectIcmpNeXor2(
      std::shared_ptr<TySelectIcmpNeXor2> _select_icmp_ne_xor2);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime,
       std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _x,
       std::shared_ptr<TyValue> _u, std::shared_ptr<TyValue> _w,
       std::shared_ptr<TyConstInt> _c, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySelectIcmpNeXor2> select_icmp_ne_xor2;
};

struct TySelectIcmpSltXor1 {
public:
  TySelectIcmpSltXor1(std::shared_ptr<TyValue> _z,
                      std::shared_ptr<TyValue> _zprime,
                      std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _x,
                      std::shared_ptr<TyValue> _u,
                      std::shared_ptr<TyConstInt> _c,
                      std::shared_ptr<TyConstInt> _cprime,
                      std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> zprime;
  std::shared_ptr<TyValue> v;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> u;
  std::shared_ptr<TyConstInt> c;
  std::shared_ptr<TyConstInt> cprime;
  std::shared_ptr<TySize> s;
};

struct ConsSelectIcmpSltXor1 : public TyInfrule {
public:
  ConsSelectIcmpSltXor1(
      std::shared_ptr<TySelectIcmpSltXor1> _select_icmp_slt_xor1);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime,
       std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _x,
       std::shared_ptr<TyValue> _u, std::shared_ptr<TyConstInt> _c,
       std::shared_ptr<TyConstInt> _cprime, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySelectIcmpSltXor1> select_icmp_slt_xor1;
};

struct TySelectIcmpSltXor2 {
public:
  TySelectIcmpSltXor2(std::shared_ptr<TyValue> _z,
                      std::shared_ptr<TyValue> _zprime,
                      std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _x,
                      std::shared_ptr<TyValue> _u,
                      std::shared_ptr<TyConstInt> _c,
                      std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> zprime;
  std::shared_ptr<TyValue> v;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> u;
  std::shared_ptr<TyConstInt> c;
  std::shared_ptr<TySize> s;
};

struct ConsSelectIcmpSltXor2 : public TyInfrule {
public:
  ConsSelectIcmpSltXor2(
      std::shared_ptr<TySelectIcmpSltXor2> _select_icmp_slt_xor2);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime,
       std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _x,
       std::shared_ptr<TyValue> _u, std::shared_ptr<TyConstInt> _c,
       std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySelectIcmpSltXor2> select_icmp_slt_xor2;
};

struct TySelectIcmpSgtXor1 {
public:
  TySelectIcmpSgtXor1(std::shared_ptr<TyValue> _z,
                      std::shared_ptr<TyValue> _zprime,
                      std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _x,
                      std::shared_ptr<TyValue> _u,
                      std::shared_ptr<TyConstInt> _c,
                      std::shared_ptr<TyConstInt> _cprime,
                      std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> zprime;
  std::shared_ptr<TyValue> v;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> u;
  std::shared_ptr<TyConstInt> c;
  std::shared_ptr<TyConstInt> cprime;
  std::shared_ptr<TySize> s;
};

struct ConsSelectIcmpSgtXor1 : public TyInfrule {
public:
  ConsSelectIcmpSgtXor1(
      std::shared_ptr<TySelectIcmpSgtXor1> _select_icmp_sgt_xor1);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime,
       std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _x,
       std::shared_ptr<TyValue> _u, std::shared_ptr<TyConstInt> _c,
       std::shared_ptr<TyConstInt> _cprime, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySelectIcmpSgtXor1> select_icmp_sgt_xor1;
};

struct TySelectIcmpSgtXor2 {
public:
  TySelectIcmpSgtXor2(std::shared_ptr<TyValue> _z,
                      std::shared_ptr<TyValue> _zprime,
                      std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _x,
                      std::shared_ptr<TyValue> _u,
                      std::shared_ptr<TyConstInt> _c,
                      std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> zprime;
  std::shared_ptr<TyValue> v;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> u;
  std::shared_ptr<TyConstInt> c;
  std::shared_ptr<TySize> s;
};

struct ConsSelectIcmpSgtXor2 : public TyInfrule {
public:
  ConsSelectIcmpSgtXor2(
      std::shared_ptr<TySelectIcmpSgtXor2> _select_icmp_sgt_xor2);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime,
       std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _x,
       std::shared_ptr<TyValue> _u, std::shared_ptr<TyConstInt> _c,
       std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySelectIcmpSgtXor2> select_icmp_sgt_xor2;
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

} // crellvm

#endif
