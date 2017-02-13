#include <string>
#include <cassert>
#include <cereal/archives/json.hpp>
#include "llvm/LLVMBerry/Structure.h"
#include "llvm/LLVMBerry/ValidationUnit.h"
#include "llvm/LLVMBerry/InstCombine/InfrulesMulDivRem.h"

namespace llvmberry {

TyMulMone::TyMulMone(std::shared_ptr<TyRegister> _z,
                     std::shared_ptr<TyValue> _x, std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), x(std::move(_x)), sz(std::move(_sz)) {}
void TyMulMone::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(sz));
}

ConsMulMone::ConsMulMone(std::shared_ptr<TyMulMone> _mul_mone)
    : mul_mone(std::move(_mul_mone)) {}
std::shared_ptr<TyInfrule> ConsMulMone::make(std::shared_ptr<TyRegister> _z,
                                             std::shared_ptr<TyValue> _x,
                                             std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyMulMone> _val(
      new TyMulMone(std::move(_z), std::move(_x), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsMulMone(std::move(_val)));
}
void ConsMulMone::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("MulMone");
  archive(CEREAL_NVP(mul_mone));
}

TyMulNeg::TyMulNeg(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _mx,
                   std::shared_ptr<TyValue> _my, std::shared_ptr<TyValue> _x,
                   std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), mx(std::move(_mx)), my(std::move(_my)),
      x(std::move(_x)), y(std::move(_y)), sz(std::move(_sz)) {}

void TyMulNeg::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(mx), CEREAL_NVP(my), CEREAL_NVP(x),
          CEREAL_NVP(y), CEREAL_NVP(sz));
}

ConsMulNeg::ConsMulNeg(std::shared_ptr<TyMulNeg> _mul_neg)
    : mul_neg(std::move(_mul_neg)) {}

void ConsMulNeg::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("MulNeg");
  archive(CEREAL_NVP(mul_neg));
}

std::shared_ptr<TyInfrule>
ConsMulNeg::make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _mx,
                 std::shared_ptr<TyValue> _my, std::shared_ptr<TyValue> _x,
                 std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyMulNeg> _mul_neg(
      new TyMulNeg(std::move(_z), std::move(_mx), std::move(_my), std::move(_x),
                   std::move(_y), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsMulNeg(std::move(_mul_neg)));
}

TyMulBool::TyMulBool(std::shared_ptr<TyRegister> _z,
                     std::shared_ptr<TyRegister> _x,
                     std::shared_ptr<TyRegister> _y)
    : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)) {}

void TyMulBool::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(x), CEREAL_NVP(y));
}

ConsMulBool::ConsMulBool(std::shared_ptr<TyMulBool> _mul_bool)
    : mul_bool(std::move(_mul_bool)) {}

void ConsMulBool::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("MulBool");
  archive(CEREAL_NVP(mul_bool));
}

std::shared_ptr<TyInfrule> ConsMulBool::make(std::shared_ptr<TyRegister> _z,
                                             std::shared_ptr<TyRegister> _x,
                                             std::shared_ptr<TyRegister> _y) {
  std::shared_ptr<TyMulBool> _mul_bool(
      new TyMulBool(std::move(_z), std::move(_x), std::move(_y)));

  return std::shared_ptr<TyInfrule>(new ConsMulBool(std::move(_mul_bool)));
}

TyMulShl::TyMulShl(std::shared_ptr<TyRegister> _z,
                   std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _x,
                   std::shared_ptr<TyValue> _a, std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), y(std::move(_y)), x(std::move(_x)), a(std::move(_a)),
      sz(std::move(_sz)) {}
void TyMulShl::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(sz));
}

ConsMulShl::ConsMulShl(std::shared_ptr<TyMulShl> _mul_shl)
    : mul_shl(std::move(_mul_shl)) {}
std::shared_ptr<TyInfrule> ConsMulShl::make(std::shared_ptr<TyRegister> _z,
                                            std::shared_ptr<TyRegister> _y,
                                            std::shared_ptr<TyValue> _x,
                                            std::shared_ptr<TyValue> _a,
                                            std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyMulShl> _val(new TyMulShl(std::move(_z), std::move(_y),
                                              std::move(_x), std::move(_a),
                                              std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsMulShl(std::move(_val)));
}
void ConsMulShl::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("MulShl");
  archive(CEREAL_NVP(mul_shl));
}

ConsSdivSubSrem::ConsSdivSubSrem(std::shared_ptr<TySdivSubSrem> _div_sub_srem)
    : div_sub_srem(std::move(_div_sub_srem)) {}
std::shared_ptr<TyInfrule> ConsSdivSubSrem::make(std::shared_ptr<TyRegister> _z,
                                                 std::shared_ptr<TyRegister> _b,
                                                 std::shared_ptr<TyRegister> _a,
                                                 std::shared_ptr<TyValue> _x,
                                                 std::shared_ptr<TyValue> _y,
                                                 std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TySdivSubSrem> _val(
      new TySdivSubSrem(std::move(_z), std::move(_b), std::move(_a),
                        std::move(_x), std::move(_y), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsSdivSubSrem(std::move(_val)));
}
void ConsSdivSubSrem::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SdivSubSrem");
  archive(CEREAL_NVP(div_sub_srem));
}

TySdivSubSrem::TySdivSubSrem(std::shared_ptr<TyRegister> _z,
                             std::shared_ptr<TyRegister> _b,
                             std::shared_ptr<TyRegister> _a,
                             std::shared_ptr<TyValue> _x,
                             std::shared_ptr<TyValue> _y,
                             std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), b(std::move(_b)), a(std::move(_a)), x(std::move(_x)),
      y(std::move(_y)), sz(std::move(_sz)) {}
void TySdivSubSrem::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(b));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(sz));
}

ConsUdivSubUrem::ConsUdivSubUrem(std::shared_ptr<TyUdivSubUrem> _div_sub_urem)
    : div_sub_urem(std::move(_div_sub_urem)) {}
std::shared_ptr<TyInfrule> ConsUdivSubUrem::make(std::shared_ptr<TyRegister> _z,
                                                 std::shared_ptr<TyRegister> _b,
                                                 std::shared_ptr<TyRegister> _a,
                                                 std::shared_ptr<TyValue> _x,
                                                 std::shared_ptr<TyValue> _y,
                                                 std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyUdivSubUrem> _val(
      new TyUdivSubUrem(std::move(_z), std::move(_b), std::move(_a),
                        std::move(_x), std::move(_y), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsUdivSubUrem(std::move(_val)));
}
void ConsUdivSubUrem::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("UdivSubUrem");
  archive(CEREAL_NVP(div_sub_urem));
}

TyUdivSubUrem::TyUdivSubUrem(std::shared_ptr<TyRegister> _z,
                             std::shared_ptr<TyRegister> _b,
                             std::shared_ptr<TyRegister> _a,
                             std::shared_ptr<TyValue> _x,
                             std::shared_ptr<TyValue> _y,
                             std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), b(std::move(_b)), a(std::move(_a)), x(std::move(_x)),
      y(std::move(_y)), sz(std::move(_sz)) {}
void TyUdivSubUrem::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(b));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(sz));
}

TyUdivZext::TyUdivZext(std::shared_ptr<TyRegister> _z,
                       std::shared_ptr<TyRegister> _x,
                       std::shared_ptr<TyRegister> _y,
                       std::shared_ptr<TyRegister> _k,
                       std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
                       std::shared_ptr<TySize> _sz1,
                       std::shared_ptr<TySize> _sz2)
    : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)), k(std::move(_k)),
      a(std::move(_a)), b(std::move(_b)), sz1(std::move(_sz1)),
      sz2(std::move(_sz2)) {}
void TyUdivZext::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(k));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(b));
  archive(CEREAL_NVP(sz1));
  archive(CEREAL_NVP(sz2));
}

ConsUdivZext::ConsUdivZext(std::shared_ptr<TyUdivZext> _udiv_zext)
    : udiv_zext(std::move(_udiv_zext)) {}
std::shared_ptr<TyInfrule> ConsUdivZext::make(
    std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x,
    std::shared_ptr<TyRegister> _y, std::shared_ptr<TyRegister> _k,
    std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
    std::shared_ptr<TySize> _sz1, std::shared_ptr<TySize> _sz2) {
  std::shared_ptr<TyUdivZext> _val(new TyUdivZext(
      std::move(_z), std::move(_x), std::move(_y), std::move(_k), std::move(_a),
      std::move(_b), std::move(_sz1), std::move(_sz2)));
  return std::shared_ptr<TyInfrule>(new ConsUdivZext(std::move(_val)));
}
void ConsUdivZext::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("UdivZext");
  archive(CEREAL_NVP(udiv_zext));
}

TyUdivZextConst::TyUdivZextConst(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x, std::shared_ptr<TyConstInt> _c, std::shared_ptr<TyRegister> _k, std::shared_ptr<TyValue> _a, std::shared_ptr<TySize> _sz1, std::shared_ptr<TySize> _sz2) : z(_z), x(_x), c(_c), k(_k), a(_a), sz1(_sz1), sz2(_sz2){
}
void TyUdivZextConst::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z), CEREAL_NVP(x), CEREAL_NVP(c), CEREAL_NVP(k), CEREAL_NVP(a), CEREAL_NVP(sz1), CEREAL_NVP(sz2));
}

ConsUdivZextConst::ConsUdivZextConst(std::shared_ptr<TyUdivZextConst> _udiv_zext_const) : udiv_zext_const(_udiv_zext_const){
}
std::shared_ptr<TyInfrule> ConsUdivZextConst::make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x, std::shared_ptr<TyConstInt> _c, std::shared_ptr<TyRegister> _k, std::shared_ptr<TyValue> _a, std::shared_ptr<TySize> _sz1, std::shared_ptr<TySize> _sz2){
  std::shared_ptr<TyUdivZextConst> _val(new TyUdivZextConst(_z, _x, _c, _k, _a, _sz1, _sz2));
  return std::shared_ptr<TyInfrule>(new ConsUdivZextConst(_val));
}
void ConsUdivZextConst::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("UdivZextConst");
  archive(CEREAL_NVP(udiv_zext_const));
}

TySdivMone::TySdivMone(std::shared_ptr<TyRegister> _z,
                       std::shared_ptr<TyValue> _x, std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), x(std::move(_x)), sz(std::move(_sz)) {}
void TySdivMone::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(sz));
}

ConsSdivMone::ConsSdivMone(std::shared_ptr<TySdivMone> _div_mone)
    : div_mone(std::move(_div_mone)) {}
std::shared_ptr<TyInfrule> ConsSdivMone::make(std::shared_ptr<TyRegister> _z,
                                              std::shared_ptr<TyValue> _x,
                                              std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TySdivMone> _val(
      new TySdivMone(std::move(_z), std::move(_x), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsSdivMone(std::move(_val)));
}
void ConsSdivMone::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SdivMone");
  archive(CEREAL_NVP(div_mone));
}

TyUremZext::TyUremZext(std::shared_ptr<TyRegister> _z,
                       std::shared_ptr<TyRegister> _x,
                       std::shared_ptr<TyRegister> _y,
                       std::shared_ptr<TyRegister> _k,
                       std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
                       std::shared_ptr<TySize> _sz1,
                       std::shared_ptr<TySize> _sz2)
    : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)), k(std::move(_k)),
      a(std::move(_a)), b(std::move(_b)), sz1(std::move(_sz1)),
      sz2(std::move(_sz2)) {}
void TyUremZext::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(k));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(b));
  archive(CEREAL_NVP(sz1));
  archive(CEREAL_NVP(sz2));
}

ConsUremZext::ConsUremZext(std::shared_ptr<TyUremZext> _urem_zext)
    : urem_zext(std::move(_urem_zext)) {}
std::shared_ptr<TyInfrule> ConsUremZext::make(
    std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x,
    std::shared_ptr<TyRegister> _y, std::shared_ptr<TyRegister> _k,
    std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
    std::shared_ptr<TySize> _sz1, std::shared_ptr<TySize> _sz2) {
  std::shared_ptr<TyUremZext> _val(new TyUremZext(
      std::move(_z), std::move(_x), std::move(_y), std::move(_k), std::move(_a),
      std::move(_b), std::move(_sz1), std::move(_sz2)));
  return std::shared_ptr<TyInfrule>(new ConsUremZext(std::move(_val)));
}
void ConsUremZext::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("UremZext");
  archive(CEREAL_NVP(urem_zext));
}

TyUremZextConst::TyUremZextConst(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x, std::shared_ptr<TyConstInt> _c, std::shared_ptr<TyRegister> _k, std::shared_ptr<TyValue> _a, std::shared_ptr<TySize> _sz1, std::shared_ptr<TySize> _sz2) : z(_z), x(_x), c(_c), k(_k), a(_a), sz1(_sz1), sz2(_sz2){
}
void TyUremZextConst::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z), CEREAL_NVP(x), CEREAL_NVP(c), CEREAL_NVP(k), CEREAL_NVP(a), CEREAL_NVP(sz1), CEREAL_NVP(sz2));
}

ConsUremZextConst::ConsUremZextConst(std::shared_ptr<TyUremZextConst> _urem_zext_const) : urem_zext_const(_urem_zext_const){
}
std::shared_ptr<TyInfrule> ConsUremZextConst::make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x, std::shared_ptr<TyConstInt> _c, std::shared_ptr<TyRegister> _k, std::shared_ptr<TyValue> _a, std::shared_ptr<TySize> _sz1, std::shared_ptr<TySize> _sz2){
  std::shared_ptr<TyUremZextConst> _val(new TyUremZextConst(_z, _x, _c, _k, _a, _sz1, _sz2));
  return std::shared_ptr<TyInfrule>(new ConsUremZextConst(_val));
}
void ConsUremZextConst::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("UremZextConst");
  archive(CEREAL_NVP(urem_zext_const));
}

TyRemNeg::TyRemNeg(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _my,
                   std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
                   std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), my(std::move(_my)), x(std::move(_x)), y(std::move(_y)),
      sz(std::move(_sz)) {}
void TyRemNeg::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(my));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(sz));
}

ConsRemNeg::ConsRemNeg(std::shared_ptr<TyRemNeg> _rem_neg)
    : rem_neg(std::move(_rem_neg)) {}
std::shared_ptr<TyInfrule> ConsRemNeg::make(std::shared_ptr<TyRegister> _z,
                                            std::shared_ptr<TyValue> _my,
                                            std::shared_ptr<TyValue> _x,
                                            std::shared_ptr<TyValue> _y,
                                            std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyRemNeg> _val(new TyRemNeg(std::move(_z), std::move(_my),
                                              std::move(_x), std::move(_y),
                                              std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsRemNeg(std::move(_val)));
}
void ConsRemNeg::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("RemNeg");
  archive(CEREAL_NVP(rem_neg));
}

}
