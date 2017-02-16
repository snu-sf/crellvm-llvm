#include <string>
#include <cassert>
#include <cereal/archives/json.hpp>
#include "llvm/LLVMBerry/Structure.h"
#include "llvm/LLVMBerry/ValidationUnit.h"
#include "llvm/LLVMBerry/InstCombine/InfrulesSelect.h"

namespace llvmberry {

TyFbopDistributiveOverSelectinst::TyFbopDistributiveOverSelectinst(
    TyFbop _fopcode, std::shared_ptr<TyRegister> _r,
    std::shared_ptr<TyRegister> _s, std::shared_ptr<TyRegister> _tprime,
    std::shared_ptr<TyRegister> _t0, std::shared_ptr<TyValue> _x,
    std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _z,
    std::shared_ptr<TyValue> _c, TyFloatType _fbopty,
    std::shared_ptr<TyValueType> _selty)
    : fopcode(std::move(_fopcode)), r(std::move(_r)), s(std::move(_s)),
      tprime(std::move(_tprime)), t0(std::move(_t0)), x(std::move(_x)),
      y(std::move(_y)), z(std::move(_z)), c(std::move(_c)),
      fbopty(std::move(_fbopty)), selty(std::move(_selty)) {}
void TyFbopDistributiveOverSelectinst::serialize(
    cereal::JSONOutputArchive &archive) const {
  archive(cereal::make_nvp("fopcode", toString(fopcode)));
  archive(CEREAL_NVP(r));
  archive(CEREAL_NVP(s));
  archive(CEREAL_NVP(tprime));
  archive(CEREAL_NVP(t0));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(c));
  archive(cereal::make_nvp("fbopty", toString(fbopty)));
  archive(CEREAL_NVP(selty));
}

ConsFbopDistributiveOverSelectinst::ConsFbopDistributiveOverSelectinst(
    std::shared_ptr<TyFbopDistributiveOverSelectinst>
        _fbop_distributive_over_selectinst)
    : fbop_distributive_over_selectinst(
          std::move(_fbop_distributive_over_selectinst)) {}
std::shared_ptr<TyInfrule> ConsFbopDistributiveOverSelectinst::make(
    TyFbop _fopcode, std::shared_ptr<TyRegister> _r,
    std::shared_ptr<TyRegister> _s, std::shared_ptr<TyRegister> _tprime,
    std::shared_ptr<TyRegister> _t0, std::shared_ptr<TyValue> _x,
    std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _z,
    std::shared_ptr<TyValue> _c, TyFloatType _fbopty,
    std::shared_ptr<TyValueType> _selty) {
  std::shared_ptr<TyFbopDistributiveOverSelectinst> _val(
      new TyFbopDistributiveOverSelectinst(
          std::move(_fopcode), std::move(_r), std::move(_s), std::move(_tprime),
          std::move(_t0), std::move(_x), std::move(_y), std::move(_z),
          std::move(_c), std::move(_fbopty), std::move(_selty)));
  return std::shared_ptr<TyInfrule>(
      new ConsFbopDistributiveOverSelectinst(std::move(_val)));
}
void ConsFbopDistributiveOverSelectinst::serialize(
    cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("FbopDistributiveOverSelectinst");
  archive(CEREAL_NVP(fbop_distributive_over_selectinst));
}

TyFbopDistributiveOverSelectinst2::TyFbopDistributiveOverSelectinst2(
    TyFbop _fopcode, std::shared_ptr<TyRegister> _r,
    std::shared_ptr<TyRegister> _s, std::shared_ptr<TyRegister> _tprime,
    std::shared_ptr<TyRegister> _t0, std::shared_ptr<TyValue> _x,
    std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _z,
    std::shared_ptr<TyValue> _c, TyFloatType _fbopty,
    std::shared_ptr<TyValueType> _selty)
    : fopcode(std::move(_fopcode)), r(std::move(_r)), s(std::move(_s)),
      tprime(std::move(_tprime)), t0(std::move(_t0)), x(std::move(_x)),
      y(std::move(_y)), z(std::move(_z)), c(std::move(_c)),
      fbopty(std::move(_fbopty)), selty(std::move(_selty)) {}
void TyFbopDistributiveOverSelectinst2::serialize(
    cereal::JSONOutputArchive &archive) const {
  archive(cereal::make_nvp("fopcode", toString(fopcode)));
  archive(CEREAL_NVP(r));
  archive(CEREAL_NVP(s));
  archive(CEREAL_NVP(tprime));
  archive(CEREAL_NVP(t0));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(c));
  archive(cereal::make_nvp("fbopty", toString(fbopty)));
  archive(CEREAL_NVP(selty));
}

ConsFbopDistributiveOverSelectinst2::ConsFbopDistributiveOverSelectinst2(
    std::shared_ptr<TyFbopDistributiveOverSelectinst2>
        _fbop_distributive_over_selectinst2)
    : fbop_distributive_over_selectinst2(
          std::move(_fbop_distributive_over_selectinst2)) {}
std::shared_ptr<TyInfrule> ConsFbopDistributiveOverSelectinst2::make(
    TyFbop _fopcode, std::shared_ptr<TyRegister> _r,
    std::shared_ptr<TyRegister> _s, std::shared_ptr<TyRegister> _tprime,
    std::shared_ptr<TyRegister> _t0, std::shared_ptr<TyValue> _x,
    std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _z,
    std::shared_ptr<TyValue> _c, TyFloatType _fbopty,
    std::shared_ptr<TyValueType> _selty) {
  std::shared_ptr<TyFbopDistributiveOverSelectinst2> _val(
      new TyFbopDistributiveOverSelectinst2(
          std::move(_fopcode), std::move(_r), std::move(_s), std::move(_tprime),
          std::move(_t0), std::move(_x), std::move(_y), std::move(_z),
          std::move(_c), std::move(_fbopty), std::move(_selty)));
  return std::shared_ptr<TyInfrule>(
      new ConsFbopDistributiveOverSelectinst2(std::move(_val)));
}
void ConsFbopDistributiveOverSelectinst2::serialize(
    cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("FbopDistributiveOverSelectinst2");
  archive(CEREAL_NVP(fbop_distributive_over_selectinst2));
}

TySelectIcmpSgtConst::TySelectIcmpSgtConst(std::shared_ptr<TyRegister> _z,
                                           std::shared_ptr<TyValue> _y,
                                           std::shared_ptr<TyValue> _x,
                                           std::shared_ptr<TyConstInt> _c,
                                           std::shared_ptr<TyConstInt> _cprime,
                                           bool _selcomm,
                                           std::shared_ptr<TySize> _s)
    : z(_z), y(_y), x(_x), c(_c), cprime(_cprime), selcomm(_selcomm), s(_s) {}
void TySelectIcmpSgtConst::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(y), CEREAL_NVP(x), CEREAL_NVP(c),
          CEREAL_NVP(cprime), CEREAL_NVP(selcomm), CEREAL_NVP(s));
}
ConsSelectIcmpSgtConst::ConsSelectIcmpSgtConst(
    std::shared_ptr<TySelectIcmpSgtConst> _select_icmp_sgt_const)
    : select_icmp_sgt_const(_select_icmp_sgt_const) {}
std::shared_ptr<TyInfrule> ConsSelectIcmpSgtConst::make(
    std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _y,
    std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c,
    std::shared_ptr<TyConstInt> _cprime, bool _selcomm,
    std::shared_ptr<TySize> _s) {
  std::shared_ptr<TySelectIcmpSgtConst> _val(
      new TySelectIcmpSgtConst(_z, _y, _x, _c, _cprime, _selcomm, _s));
  return std::shared_ptr<TyInfrule>(new ConsSelectIcmpSgtConst(_val));
}
void
ConsSelectIcmpSgtConst::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SelectIcmpSgtConst");
  archive(CEREAL_NVP(select_icmp_sgt_const));
}

ConsSelectIcmpUltConst::ConsSelectIcmpUltConst(
    std::shared_ptr<TySelectIcmpUltConst> _select_icmp_ult_const)
    : select_icmp_ult_const(_select_icmp_ult_const) {}
std::shared_ptr<TyInfrule> ConsSelectIcmpUltConst::make(
    std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _y,
    std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c,
    std::shared_ptr<TyConstInt> _cprime, bool _selcomm,
    std::shared_ptr<TySize> _s) {
  std::shared_ptr<TySelectIcmpUltConst> _val(
      new TySelectIcmpUltConst(_z, _y, _x, _c, _cprime, _selcomm, _s));
  return std::shared_ptr<TyInfrule>(new ConsSelectIcmpUltConst(_val));
}
void
ConsSelectIcmpUltConst::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SelectIcmpUltConst");
  archive(CEREAL_NVP(select_icmp_ult_const));
}

TySelectIcmpUltConst::TySelectIcmpUltConst(std::shared_ptr<TyRegister> _z,
                                           std::shared_ptr<TyValue> _y,
                                           std::shared_ptr<TyValue> _x,
                                           std::shared_ptr<TyConstInt> _c,
                                           std::shared_ptr<TyConstInt> _cprime,
                                           bool _selcomm,
                                           std::shared_ptr<TySize> _s)
    : z(_z), y(_y), x(_x), c(_c), cprime(_cprime), selcomm(_selcomm), s(_s) {}
void TySelectIcmpUltConst::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(y), CEREAL_NVP(x), CEREAL_NVP(c),
          CEREAL_NVP(cprime), CEREAL_NVP(selcomm), CEREAL_NVP(s));
}

TySelectIcmpUgtConst::TySelectIcmpUgtConst(std::shared_ptr<TyRegister> _z,
                                           std::shared_ptr<TyValue> _y,
                                           std::shared_ptr<TyValue> _x,
                                           std::shared_ptr<TyConstInt> _c,
                                           std::shared_ptr<TyConstInt> _cprime,
                                           bool _selcomm,
                                           std::shared_ptr<TySize> _s)
    : z(_z), y(_y), x(_x), c(_c), cprime(_cprime), selcomm(_selcomm), s(_s) {}
void TySelectIcmpUgtConst::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(y), CEREAL_NVP(x), CEREAL_NVP(c),
          CEREAL_NVP(cprime), CEREAL_NVP(selcomm), CEREAL_NVP(s));
}
ConsSelectIcmpUgtConst::ConsSelectIcmpUgtConst(
    std::shared_ptr<TySelectIcmpUgtConst> _select_icmp_ugt_const)
    : select_icmp_ugt_const(_select_icmp_ugt_const) {}
std::shared_ptr<TyInfrule> ConsSelectIcmpUgtConst::make(
    std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _y,
    std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c,
    std::shared_ptr<TyConstInt> _cprime, bool _selcomm,
    std::shared_ptr<TySize> _s) {
  std::shared_ptr<TySelectIcmpUgtConst> _val(
      new TySelectIcmpUgtConst(_z, _y, _x, _c, _cprime, _selcomm, _s));
  return std::shared_ptr<TyInfrule>(new ConsSelectIcmpUgtConst(_val));
}
void
ConsSelectIcmpUgtConst::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SelectIcmpUgtConst");
  archive(CEREAL_NVP(select_icmp_ugt_const));
}

TySelectIcmpSltConst::TySelectIcmpSltConst(std::shared_ptr<TyRegister> _z,
                                           std::shared_ptr<TyValue> _y,
                                           std::shared_ptr<TyValue> _x,
                                           std::shared_ptr<TyConstInt> _c,
                                           std::shared_ptr<TyConstInt> _cprime,
                                           bool _selcomm,
                                           std::shared_ptr<TySize> _s)
    : z(_z), y(_y), x(_x), c(_c), cprime(_cprime), selcomm(_selcomm), s(_s) {}
void TySelectIcmpSltConst::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(y), CEREAL_NVP(x), CEREAL_NVP(c),
          CEREAL_NVP(cprime), CEREAL_NVP(selcomm), CEREAL_NVP(s));
}
ConsSelectIcmpSltConst::ConsSelectIcmpSltConst(
    std::shared_ptr<TySelectIcmpSltConst> _select_icmp_slt_const)
    : select_icmp_slt_const(_select_icmp_slt_const) {}
std::shared_ptr<TyInfrule> ConsSelectIcmpSltConst::make(
    std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _y,
    std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c,
    std::shared_ptr<TyConstInt> _cprime, bool _selcomm,
    std::shared_ptr<TySize> _s) {
  std::shared_ptr<TySelectIcmpSltConst> _val(
      new TySelectIcmpSltConst(_z, _y, _x, _c, _cprime, _selcomm, _s));
  return std::shared_ptr<TyInfrule>(new ConsSelectIcmpSltConst(_val));
}
void
ConsSelectIcmpSltConst::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SelectIcmpSltConst");
  archive(CEREAL_NVP(select_icmp_slt_const));
}

TySelectIcmpEq::TySelectIcmpEq(std::shared_ptr<TyValue> _z,
                               std::shared_ptr<TyValue> _y,
                               std::shared_ptr<TyValue> _x,
                               std::shared_ptr<TyValue> _v,
                               std::shared_ptr<TyConstant> _c,
                               std::shared_ptr<TyValueType> _cty)
    : z(_z), y(_y), x(_x), v(_v), c(_c), cty(_cty) {}
void TySelectIcmpEq::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(y), CEREAL_NVP(x), CEREAL_NVP(v),
          CEREAL_NVP(c), CEREAL_NVP(cty));
}
ConsSelectIcmpEq::ConsSelectIcmpEq(
    std::shared_ptr<TySelectIcmpEq> _select_icmp_eq)
    : select_icmp_eq(_select_icmp_eq) {}
std::shared_ptr<TyInfrule>
ConsSelectIcmpEq::make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _y,
                       std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _v,
                       std::shared_ptr<TyConstant> _c,
                       std::shared_ptr<TyValueType> _cty) {
  std::shared_ptr<TySelectIcmpEq> _val(
      new TySelectIcmpEq(_z, _y, _x, _v, _c, _cty));
  return std::shared_ptr<TyInfrule>(new ConsSelectIcmpEq(_val));
}
void ConsSelectIcmpEq::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SelectIcmpEq");
  archive(CEREAL_NVP(select_icmp_eq));
}

TySelectIcmpNe::TySelectIcmpNe(std::shared_ptr<TyValue> _z,
                               std::shared_ptr<TyValue> _y,
                               std::shared_ptr<TyValue> _x,
                               std::shared_ptr<TyValue> _v,
                               std::shared_ptr<TyConstant> _c,
                               std::shared_ptr<TyValueType> _cty)
    : z(_z), y(_y), x(_x), v(_v), c(_c), cty(_cty) {}
void TySelectIcmpNe::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(y), CEREAL_NVP(x), CEREAL_NVP(v),
          CEREAL_NVP(c), CEREAL_NVP(cty));
}
ConsSelectIcmpNe::ConsSelectIcmpNe(
    std::shared_ptr<TySelectIcmpNe> _select_icmp_ne)
    : select_icmp_ne(_select_icmp_ne) {}
std::shared_ptr<TyInfrule>
ConsSelectIcmpNe::make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _y,
                       std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _v,
                       std::shared_ptr<TyConstant> _c,
                       std::shared_ptr<TyValueType> _cty) {
  std::shared_ptr<TySelectIcmpNe> _val(
      new TySelectIcmpNe(_z, _y, _x, _v, _c, _cty));
  return std::shared_ptr<TyInfrule>(new ConsSelectIcmpNe(_val));
}
void ConsSelectIcmpNe::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SelectIcmpNe");
  archive(CEREAL_NVP(select_icmp_ne));
}

TySelectIcmpEqXor1::TySelectIcmpEqXor1(
    std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime,
    std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _x,
    std::shared_ptr<TyValue> _u, std::shared_ptr<TyValue> _w,
    std::shared_ptr<TyConstInt> _c, std::shared_ptr<TyConstInt> _cprime,
    std::shared_ptr<TySize> _s)
    : z(_z), zprime(_zprime), v(_v), x(_x), u(_u), w(_w), c(_c),
      cprime(_cprime), s(_s) {}
void TySelectIcmpEqXor1::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(zprime), CEREAL_NVP(v), CEREAL_NVP(x),
          CEREAL_NVP(u), CEREAL_NVP(w), CEREAL_NVP(c), CEREAL_NVP(cprime),
          CEREAL_NVP(s));
}
ConsSelectIcmpEqXor1::ConsSelectIcmpEqXor1(
    std::shared_ptr<TySelectIcmpEqXor1> _select_icmp_eq_xor1)
    : select_icmp_eq_xor1(_select_icmp_eq_xor1) {}
std::shared_ptr<TyInfrule> ConsSelectIcmpEqXor1::make(
    std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime,
    std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _x,
    std::shared_ptr<TyValue> _u, std::shared_ptr<TyValue> _w,
    std::shared_ptr<TyConstInt> _c, std::shared_ptr<TyConstInt> _cprime,
    std::shared_ptr<TySize> _s) {
  std::shared_ptr<TySelectIcmpEqXor1> _val(
      new TySelectIcmpEqXor1(_z, _zprime, _v, _x, _u, _w, _c, _cprime, _s));
  return std::shared_ptr<TyInfrule>(new ConsSelectIcmpEqXor1(_val));
}
void ConsSelectIcmpEqXor1::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SelectIcmpEqXor1");
  archive(CEREAL_NVP(select_icmp_eq_xor1));
}

TySelectIcmpEqXor2::TySelectIcmpEqXor2(
    std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime,
    std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _x,
    std::shared_ptr<TyValue> _u, std::shared_ptr<TyValue> _w,
    std::shared_ptr<TyConstInt> _c, std::shared_ptr<TySize> _s)
    : z(_z), zprime(_zprime), v(_v), x(_x), u(_u), w(_w), c(_c), s(_s) {}
void TySelectIcmpEqXor2::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(zprime), CEREAL_NVP(v), CEREAL_NVP(x),
          CEREAL_NVP(u), CEREAL_NVP(w), CEREAL_NVP(c), CEREAL_NVP(s));
}
ConsSelectIcmpEqXor2::ConsSelectIcmpEqXor2(
    std::shared_ptr<TySelectIcmpEqXor2> _select_icmp_eq_xor2)
    : select_icmp_eq_xor2(_select_icmp_eq_xor2) {}
std::shared_ptr<TyInfrule> ConsSelectIcmpEqXor2::make(
    std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime,
    std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _x,
    std::shared_ptr<TyValue> _u, std::shared_ptr<TyValue> _w,
    std::shared_ptr<TyConstInt> _c, std::shared_ptr<TySize> _s) {
  std::shared_ptr<TySelectIcmpEqXor2> _val(
      new TySelectIcmpEqXor2(_z, _zprime, _v, _x, _u, _w, _c, _s));
  return std::shared_ptr<TyInfrule>(new ConsSelectIcmpEqXor2(_val));
}
void ConsSelectIcmpEqXor2::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SelectIcmpEqXor2");
  archive(CEREAL_NVP(select_icmp_eq_xor2));
}

TySelectIcmpNeXor1::TySelectIcmpNeXor1(
    std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime,
    std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _x,
    std::shared_ptr<TyValue> _u, std::shared_ptr<TyValue> _w,
    std::shared_ptr<TyConstInt> _c, std::shared_ptr<TyConstInt> _cprime,
    std::shared_ptr<TySize> _s)
    : z(_z), zprime(_zprime), v(_v), x(_x), u(_u), w(_w), c(_c),
      cprime(_cprime), s(_s) {}
void TySelectIcmpNeXor1::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(zprime), CEREAL_NVP(v), CEREAL_NVP(x),
          CEREAL_NVP(u), CEREAL_NVP(w), CEREAL_NVP(c), CEREAL_NVP(cprime),
          CEREAL_NVP(s));
}
ConsSelectIcmpNeXor1::ConsSelectIcmpNeXor1(
    std::shared_ptr<TySelectIcmpNeXor1> _select_icmp_ne_xor1)
    : select_icmp_ne_xor1(_select_icmp_ne_xor1) {}
std::shared_ptr<TyInfrule> ConsSelectIcmpNeXor1::make(
    std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime,
    std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _x,
    std::shared_ptr<TyValue> _u, std::shared_ptr<TyValue> _w,
    std::shared_ptr<TyConstInt> _c, std::shared_ptr<TyConstInt> _cprime,
    std::shared_ptr<TySize> _s) {
  std::shared_ptr<TySelectIcmpNeXor1> _val(
      new TySelectIcmpNeXor1(_z, _zprime, _v, _x, _u, _w, _c, _cprime, _s));
  return std::shared_ptr<TyInfrule>(new ConsSelectIcmpNeXor1(_val));
}
void ConsSelectIcmpNeXor1::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SelectIcmpNeXor1");
  archive(CEREAL_NVP(select_icmp_ne_xor1));
}

TySelectIcmpNeXor2::TySelectIcmpNeXor2(
    std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime,
    std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _x,
    std::shared_ptr<TyValue> _u, std::shared_ptr<TyValue> _w,
    std::shared_ptr<TyConstInt> _c, std::shared_ptr<TySize> _s)
    : z(_z), zprime(_zprime), v(_v), x(_x), u(_u), w(_w), c(_c), s(_s) {}
void TySelectIcmpNeXor2::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(zprime), CEREAL_NVP(v), CEREAL_NVP(x),
          CEREAL_NVP(u), CEREAL_NVP(w), CEREAL_NVP(c), CEREAL_NVP(s));
}
ConsSelectIcmpNeXor2::ConsSelectIcmpNeXor2(
    std::shared_ptr<TySelectIcmpNeXor2> _select_icmp_ne_xor2)
    : select_icmp_ne_xor2(_select_icmp_ne_xor2) {}
std::shared_ptr<TyInfrule> ConsSelectIcmpNeXor2::make(
    std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime,
    std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _x,
    std::shared_ptr<TyValue> _u, std::shared_ptr<TyValue> _w,
    std::shared_ptr<TyConstInt> _c, std::shared_ptr<TySize> _s) {
  std::shared_ptr<TySelectIcmpNeXor2> _val(
      new TySelectIcmpNeXor2(_z, _zprime, _v, _x, _u, _w, _c, _s));
  return std::shared_ptr<TyInfrule>(new ConsSelectIcmpNeXor2(_val));
}
void ConsSelectIcmpNeXor2::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SelectIcmpNeXor2");
  archive(CEREAL_NVP(select_icmp_ne_xor2));
}

TySelectIcmpSltXor1::TySelectIcmpSltXor1(
    std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime,
    std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _x,
    std::shared_ptr<TyValue> _u, std::shared_ptr<TyConstInt> _c,
    std::shared_ptr<TyConstInt> _cprime, std::shared_ptr<TySize> _s)
    : z(_z), zprime(_zprime), v(_v), x(_x), u(_u), c(_c), cprime(_cprime),
      s(_s) {}
void TySelectIcmpSltXor1::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(zprime), CEREAL_NVP(v), CEREAL_NVP(x),
          CEREAL_NVP(u), CEREAL_NVP(c), CEREAL_NVP(cprime), CEREAL_NVP(s));
}
ConsSelectIcmpSltXor1::ConsSelectIcmpSltXor1(
    std::shared_ptr<TySelectIcmpSltXor1> _select_icmp_slt_xor1)
    : select_icmp_slt_xor1(_select_icmp_slt_xor1) {}
std::shared_ptr<TyInfrule> ConsSelectIcmpSltXor1::make(
    std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime,
    std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _x,
    std::shared_ptr<TyValue> _u, std::shared_ptr<TyConstInt> _c,
    std::shared_ptr<TyConstInt> _cprime, std::shared_ptr<TySize> _s) {
  std::shared_ptr<TySelectIcmpSltXor1> _val(
      new TySelectIcmpSltXor1(_z, _zprime, _v, _x, _u, _c, _cprime, _s));
  return std::shared_ptr<TyInfrule>(new ConsSelectIcmpSltXor1(_val));
}
void
ConsSelectIcmpSltXor1::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SelectIcmpSltXor1");
  archive(CEREAL_NVP(select_icmp_slt_xor1));
}

TySelectIcmpSltXor2::TySelectIcmpSltXor2(std::shared_ptr<TyValue> _z,
                                         std::shared_ptr<TyValue> _zprime,
                                         std::shared_ptr<TyValue> _v,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _u,
                                         std::shared_ptr<TyConstInt> _c,
                                         std::shared_ptr<TySize> _s)
    : z(_z), zprime(_zprime), v(_v), x(_x), u(_u), c(_c), s(_s) {}
void TySelectIcmpSltXor2::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(zprime), CEREAL_NVP(v), CEREAL_NVP(x),
          CEREAL_NVP(u), CEREAL_NVP(c), CEREAL_NVP(s));
}
ConsSelectIcmpSltXor2::ConsSelectIcmpSltXor2(
    std::shared_ptr<TySelectIcmpSltXor2> _select_icmp_slt_xor2)
    : select_icmp_slt_xor2(_select_icmp_slt_xor2) {}
std::shared_ptr<TyInfrule> ConsSelectIcmpSltXor2::make(
    std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime,
    std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _x,
    std::shared_ptr<TyValue> _u, std::shared_ptr<TyConstInt> _c,
    std::shared_ptr<TySize> _s) {
  std::shared_ptr<TySelectIcmpSltXor2> _val(
      new TySelectIcmpSltXor2(_z, _zprime, _v, _x, _u, _c, _s));
  return std::shared_ptr<TyInfrule>(new ConsSelectIcmpSltXor2(_val));
}
void
ConsSelectIcmpSltXor2::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SelectIcmpSltXor2");
  archive(CEREAL_NVP(select_icmp_slt_xor2));
}

TySelectIcmpSgtXor1::TySelectIcmpSgtXor1(
    std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime,
    std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _x,
    std::shared_ptr<TyValue> _u, std::shared_ptr<TyConstInt> _c,
    std::shared_ptr<TyConstInt> _cprime, std::shared_ptr<TySize> _s)
    : z(_z), zprime(_zprime), v(_v), x(_x), u(_u), c(_c), cprime(_cprime),
      s(_s) {}
void TySelectIcmpSgtXor1::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(zprime), CEREAL_NVP(v), CEREAL_NVP(x),
          CEREAL_NVP(u), CEREAL_NVP(c), CEREAL_NVP(cprime), CEREAL_NVP(s));
}
ConsSelectIcmpSgtXor1::ConsSelectIcmpSgtXor1(
    std::shared_ptr<TySelectIcmpSgtXor1> _select_icmp_sgt_xor1)
    : select_icmp_sgt_xor1(_select_icmp_sgt_xor1) {}
std::shared_ptr<TyInfrule> ConsSelectIcmpSgtXor1::make(
    std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime,
    std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _x,
    std::shared_ptr<TyValue> _u, std::shared_ptr<TyConstInt> _c,
    std::shared_ptr<TyConstInt> _cprime, std::shared_ptr<TySize> _s) {
  std::shared_ptr<TySelectIcmpSgtXor1> _val(
      new TySelectIcmpSgtXor1(_z, _zprime, _v, _x, _u, _c, _cprime, _s));
  return std::shared_ptr<TyInfrule>(new ConsSelectIcmpSgtXor1(_val));
}
void
ConsSelectIcmpSgtXor1::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SelectIcmpSgtXor1");
  archive(CEREAL_NVP(select_icmp_sgt_xor1));
}

TySelectIcmpSgtXor2::TySelectIcmpSgtXor2(std::shared_ptr<TyValue> _z,
                                         std::shared_ptr<TyValue> _zprime,
                                         std::shared_ptr<TyValue> _v,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _u,
                                         std::shared_ptr<TyConstInt> _c,
                                         std::shared_ptr<TySize> _s)
    : z(_z), zprime(_zprime), v(_v), x(_x), u(_u), c(_c), s(_s) {}
void TySelectIcmpSgtXor2::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(zprime), CEREAL_NVP(v), CEREAL_NVP(x),
          CEREAL_NVP(u), CEREAL_NVP(c), CEREAL_NVP(s));
}
ConsSelectIcmpSgtXor2::ConsSelectIcmpSgtXor2(
    std::shared_ptr<TySelectIcmpSgtXor2> _select_icmp_sgt_xor2)
    : select_icmp_sgt_xor2(_select_icmp_sgt_xor2) {}
std::shared_ptr<TyInfrule> ConsSelectIcmpSgtXor2::make(
    std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime,
    std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _x,
    std::shared_ptr<TyValue> _u, std::shared_ptr<TyConstInt> _c,
    std::shared_ptr<TySize> _s) {
  std::shared_ptr<TySelectIcmpSgtXor2> _val(
      new TySelectIcmpSgtXor2(_z, _zprime, _v, _x, _u, _c, _s));
  return std::shared_ptr<TyInfrule>(new ConsSelectIcmpSgtXor2(_val));
}
void
ConsSelectIcmpSgtXor2::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SelectIcmpSgtXor2");
  archive(CEREAL_NVP(select_icmp_sgt_xor2));
}

TyBopDistributiveOverSelectinst::TyBopDistributiveOverSelectinst(
    TyBop _opcode, std::shared_ptr<TyRegister> _r,
    std::shared_ptr<TyRegister> _s, std::shared_ptr<TyRegister> _tprime,
    std::shared_ptr<TyRegister> _t0, std::shared_ptr<TyValue> _x,
    std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _z,
    std::shared_ptr<TyValue> _c, std::shared_ptr<TySize> _bopsz,
    std::shared_ptr<TyValueType> _selty)
    : opcode(std::move(_opcode)), r(std::move(_r)), s(std::move(_s)),
      tprime(std::move(_tprime)), t0(std::move(_t0)), x(std::move(_x)),
      y(std::move(_y)), z(std::move(_z)), c(std::move(_c)),
      bopsz(std::move(_bopsz)), selty(std::move(_selty)) {}
void TyBopDistributiveOverSelectinst::serialize(
    cereal::JSONOutputArchive &archive) const {
  archive(cereal::make_nvp("opcode", llvmberry::toString(opcode)));
  archive(CEREAL_NVP(r));
  archive(CEREAL_NVP(s));
  archive(CEREAL_NVP(tprime));
  archive(CEREAL_NVP(t0));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(c));
  archive(CEREAL_NVP(bopsz));
  archive(CEREAL_NVP(selty));
}

ConsBopDistributiveOverSelectinst::ConsBopDistributiveOverSelectinst(
    std::shared_ptr<TyBopDistributiveOverSelectinst>
        _bop_distributive_over_selectinst)
    : bop_distributive_over_selectinst(
          std::move(_bop_distributive_over_selectinst)) {}
std::shared_ptr<TyInfrule> ConsBopDistributiveOverSelectinst::make(
    TyBop _opcode, std::shared_ptr<TyRegister> _r,
    std::shared_ptr<TyRegister> _s, std::shared_ptr<TyRegister> _tprime,
    std::shared_ptr<TyRegister> _t0, std::shared_ptr<TyValue> _x,
    std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _z,
    std::shared_ptr<TyValue> _c, std::shared_ptr<TySize> _bopsz,
    std::shared_ptr<TyValueType> _selty) {
  std::shared_ptr<TyBopDistributiveOverSelectinst> _val(
      new TyBopDistributiveOverSelectinst(
          std::move(_opcode), std::move(_r), std::move(_s), std::move(_tprime),
          std::move(_t0), std::move(_x), std::move(_y), std::move(_z),
          std::move(_c), std::move(_bopsz), std::move(_selty)));
  return std::shared_ptr<TyInfrule>(
      new ConsBopDistributiveOverSelectinst(std::move(_val)));
}
void ConsBopDistributiveOverSelectinst::serialize(
    cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("BopDistributiveOverSelectinst");
  archive(CEREAL_NVP(bop_distributive_over_selectinst));
}

TyBopDistributiveOverSelectinst2::TyBopDistributiveOverSelectinst2(
    TyBop _opcode, std::shared_ptr<TyRegister> _r,
    std::shared_ptr<TyRegister> _s, std::shared_ptr<TyRegister> _tprime,
    std::shared_ptr<TyRegister> _t0, std::shared_ptr<TyValue> _x,
    std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _z,
    std::shared_ptr<TyValue> _c, std::shared_ptr<TySize> _bopsz,
    std::shared_ptr<TyValueType> _selty)
    : opcode(std::move(_opcode)), r(std::move(_r)), s(std::move(_s)),
      tprime(std::move(_tprime)), t0(std::move(_t0)), x(std::move(_x)),
      y(std::move(_y)), z(std::move(_z)), c(std::move(_c)),
      bopsz(std::move(_bopsz)), selty(std::move(_selty)) {}
void TyBopDistributiveOverSelectinst2::serialize(
    cereal::JSONOutputArchive &archive) const {
  archive(cereal::make_nvp("opcode", llvmberry::toString(opcode)));
  archive(CEREAL_NVP(r));
  archive(CEREAL_NVP(s));
  archive(CEREAL_NVP(tprime));
  archive(CEREAL_NVP(t0));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(c));
  archive(CEREAL_NVP(bopsz));
  archive(CEREAL_NVP(selty));
}

ConsBopDistributiveOverSelectinst2::ConsBopDistributiveOverSelectinst2(
    std::shared_ptr<TyBopDistributiveOverSelectinst2>
        _bop_distributive_over_selectinst2)
    : bop_distributive_over_selectinst2(
          std::move(_bop_distributive_over_selectinst2)) {}
std::shared_ptr<TyInfrule> ConsBopDistributiveOverSelectinst2::make(
    TyBop _opcode, std::shared_ptr<TyRegister> _r,
    std::shared_ptr<TyRegister> _s, std::shared_ptr<TyRegister> _tprime,
    std::shared_ptr<TyRegister> _t0, std::shared_ptr<TyValue> _x,
    std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _z,
    std::shared_ptr<TyValue> _c, std::shared_ptr<TySize> _bopsz,
    std::shared_ptr<TyValueType> _selty) {
  std::shared_ptr<TyBopDistributiveOverSelectinst2> _val(
      new TyBopDistributiveOverSelectinst2(
          std::move(_opcode), std::move(_r), std::move(_s), std::move(_tprime),
          std::move(_t0), std::move(_x), std::move(_y), std::move(_z),
          std::move(_c), std::move(_bopsz), std::move(_selty)));
  return std::shared_ptr<TyInfrule>(
      new ConsBopDistributiveOverSelectinst2(std::move(_val)));
}
void ConsBopDistributiveOverSelectinst2::serialize(
    cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("BopDistributiveOverSelectinst2");
  archive(CEREAL_NVP(bop_distributive_over_selectinst2));
}

}
