#include <string>
#include <cassert>
#include <cereal/archives/json.hpp>
#include "llvm/Crellvm/Structure.h"
#include "llvm/Crellvm/ValidationUnit.h"
#include "llvm/Crellvm/InstCombine/InfrulesAndOrXor.h"

namespace crellvm {

TyAndXorConst::TyAndXorConst(
    std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y,
    std::shared_ptr<TyRegister> _yprime, std::shared_ptr<TyValue> _x,
    std::shared_ptr<TyConstInt> _c1, std::shared_ptr<TyConstInt> _c2,
    std::shared_ptr<TyConstInt> _c3, std::shared_ptr<TySize> _sz)
    : z(_z), y(_y), yprime(_yprime), x(_x), c1(_c1), c2(_c2), c3(_c3), sz(_sz) {
}
void TyAndXorConst::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(yprime));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(c1));
  archive(CEREAL_NVP(c2));
  archive(CEREAL_NVP(c3));
  archive(CEREAL_NVP(sz));
}

ConsAndXorConst::ConsAndXorConst(std::shared_ptr<TyAndXorConst> _and_xor_const)
    : and_xor_const(_and_xor_const) {}
std::shared_ptr<TyInfrule> ConsAndXorConst::make(
    std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y,
    std::shared_ptr<TyRegister> _yprime, std::shared_ptr<TyValue> _x,
    std::shared_ptr<TyConstInt> _c1, std::shared_ptr<TyConstInt> _c2,
    std::shared_ptr<TyConstInt> _c3, std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyAndXorConst> _val(
      new TyAndXorConst(_z, _y, _yprime, _x, _c1, _c2, _c3, _sz));
  return std::shared_ptr<TyInfrule>(new ConsAndXorConst(_val));
}

void ConsAndXorConst::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("AndXorConst");
  archive(CEREAL_NVP(and_xor_const));
}

TyAndOrConst2::TyAndOrConst2(
    std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y,
    std::shared_ptr<TyRegister> _yprime, std::shared_ptr<TyValue> _x,
    std::shared_ptr<TyConstInt> _c1, std::shared_ptr<TyConstInt> _c2,
    std::shared_ptr<TyConstInt> _c3, std::shared_ptr<TySize> _sz)
    : z(_z), y(_y), yprime(_yprime), x(_x), c1(_c1), c2(_c2), c3(_c3), sz(_sz) {
}
void TyAndOrConst2::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(yprime));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(c1));
  archive(CEREAL_NVP(c2));
  archive(CEREAL_NVP(c3));
  archive(CEREAL_NVP(sz));
}

ConsAndOrConst2::ConsAndOrConst2(std::shared_ptr<TyAndOrConst2> _and_or_const2)
    : and_or_const2(_and_or_const2) {}
std::shared_ptr<TyInfrule> ConsAndOrConst2::make(
    std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y,
    std::shared_ptr<TyRegister> _yprime, std::shared_ptr<TyValue> _x,
    std::shared_ptr<TyConstInt> _c1, std::shared_ptr<TyConstInt> _c2,
    std::shared_ptr<TyConstInt> _c3, std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyAndOrConst2> _val(
      new TyAndOrConst2(_z, _y, _yprime, _x, _c1, _c2, _c3, _sz));
  return std::shared_ptr<TyInfrule>(new ConsAndOrConst2(_val));
}

void ConsAndOrConst2::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("AndOrConst2");
  archive(CEREAL_NVP(and_or_const2));
}

TyAndDeMorgan::TyAndDeMorgan(std::shared_ptr<TyRegister> _z,
                             std::shared_ptr<TyRegister> _x,
                             std::shared_ptr<TyRegister> _y,
                             std::shared_ptr<TyRegister> _zprime,
                             std::shared_ptr<TyValue> _a,
                             std::shared_ptr<TyValue> _b,
                             std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)),
      zprime(std::move(_zprime)), a(std::move(_a)), b(std::move(_b)),
      sz(std::move(_sz)) {}
void TyAndDeMorgan::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(zprime));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(b));
  archive(CEREAL_NVP(sz));
}

ConsAndDeMorgan::ConsAndDeMorgan(std::shared_ptr<TyAndDeMorgan> _and_de_morgan)
    : and_de_morgan(std::move(_and_de_morgan)) {}
std::shared_ptr<TyInfrule> ConsAndDeMorgan::make(
    std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x,
    std::shared_ptr<TyRegister> _y, std::shared_ptr<TyRegister> _zprime,
    std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
    std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyAndDeMorgan> _val(new TyAndDeMorgan(
      std::move(_z), std::move(_x), std::move(_y), std::move(_zprime),
      std::move(_a), std::move(_b), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsAndDeMorgan(std::move(_val)));
}
void ConsAndDeMorgan::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("AndDeMorgan");
  archive(CEREAL_NVP(and_de_morgan));
}

TyAndOrNot1::TyAndOrNot1(std::shared_ptr<TyRegister> _z,
                         std::shared_ptr<TyRegister> _x,
                         std::shared_ptr<TyRegister> _y,
                         std::shared_ptr<TyValue> _a,
                         std::shared_ptr<TyValue> _b,
                         std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)), a(std::move(_a)),
      b(std::move(_b)), sz(std::move(_sz)) {}
void TyAndOrNot1::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(b));
  archive(CEREAL_NVP(sz));
}

ConsAndOrNot1::ConsAndOrNot1(std::shared_ptr<TyAndOrNot1> _and_or_not1)
    : and_or_not1(std::move(_and_or_not1)) {}
std::shared_ptr<TyInfrule> ConsAndOrNot1::make(std::shared_ptr<TyRegister> _z,
                                               std::shared_ptr<TyRegister> _x,
                                               std::shared_ptr<TyRegister> _y,
                                               std::shared_ptr<TyValue> _a,
                                               std::shared_ptr<TyValue> _b,
                                               std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyAndOrNot1> _val(
      new TyAndOrNot1(std::move(_z), std::move(_x), std::move(_y),
                      std::move(_a), std::move(_b), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsAndOrNot1(std::move(_val)));
}
void ConsAndOrNot1::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("AndOrNot1");
  archive(CEREAL_NVP(and_or_not1));
}

TyOrOr::TyOrOr(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
               std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _a,
               std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)), a(std::move(_a)),
      b(std::move(_b)), sz(std::move(_sz)) {}
void TyOrOr::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(b));
  archive(CEREAL_NVP(sz));
}

ConsOrOr::ConsOrOr(std::shared_ptr<TyOrOr> _or_or) : or_or(std::move(_or_or)) {}
std::shared_ptr<TyInfrule>
ConsOrOr::make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
               std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _a,
               std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyOrOr> _val(new TyOrOr(std::move(_z), std::move(_x),
                                          std::move(_y), std::move(_a),
                                          std::move(_b), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsOrOr(std::move(_val)));
}
void ConsOrOr::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("OrOr");
  archive(CEREAL_NVP(or_or));
}

TyOrOr2::TyOrOr2(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
                 std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _yprime,
                 std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
                 std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)),
      yprime(std::move(_yprime)), a(std::move(_a)), b(std::move(_b)),
      sz(std::move(_sz)) {}
void TyOrOr2::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(yprime));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(b));
  archive(CEREAL_NVP(sz));
}

ConsOrOr2::ConsOrOr2(std::shared_ptr<TyOrOr2> _or_or2)
    : or_or2(std::move(_or_or2)) {}
std::shared_ptr<TyInfrule>
ConsOrOr2::make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
                std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _yprime,
                std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
                std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyOrOr2> _val(new TyOrOr2(
      std::move(_z), std::move(_x), std::move(_y), std::move(_yprime),
      std::move(_a), std::move(_b), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsOrOr2(std::move(_val)));
}
void ConsOrOr2::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("OrOr2");
  archive(CEREAL_NVP(or_or2));
}

TyOrXor3::TyOrXor3(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _y,
                   std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
                   std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), y(std::move(_y)), a(std::move(_a)), b(std::move(_b)),
      sz(std::move(_sz)) {}
void TyOrXor3::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(b));
  archive(CEREAL_NVP(sz));
}

ConsOrXor3::ConsOrXor3(std::shared_ptr<TyOrXor3> _or_xor3)
    : or_xor3(std::move(_or_xor3)) {}
std::shared_ptr<TyInfrule> ConsOrXor3::make(std::shared_ptr<TyValue> _z,
                                            std::shared_ptr<TyValue> _y,
                                            std::shared_ptr<TyValue> _a,
                                            std::shared_ptr<TyValue> _b,
                                            std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyOrXor3> _val(new TyOrXor3(std::move(_z), std::move(_y),
                                              std::move(_a), std::move(_b),
                                              std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsOrXor3(std::move(_val)));
}
void ConsOrXor3::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("OrXor3");
  archive(CEREAL_NVP(or_xor3));
}

TyOrAndXor::TyOrAndXor(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
                       std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _a,
                       std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)), a(std::move(_a)),
      b(std::move(_b)), sz(std::move(_sz)) {}
void TyOrAndXor::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(b));
  archive(CEREAL_NVP(sz));
}

ConsOrAndXor::ConsOrAndXor(std::shared_ptr<TyOrAndXor> _or_and_xor)
    : or_and_xor(std::move(_or_and_xor)) {}
std::shared_ptr<TyInfrule>
ConsOrAndXor::make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
                   std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _a,
                   std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyOrAndXor> _val(
      new TyOrAndXor(std::move(_z), std::move(_x), std::move(_y), std::move(_a),
                     std::move(_b), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsOrAndXor(std::move(_val)));
}
void ConsOrAndXor::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("OrAndXor");
  archive(CEREAL_NVP(or_and_xor));
}

TyOrXor::TyOrXor(std::shared_ptr<TyValue> _w, std::shared_ptr<TyValue> _z,
                 std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
                 std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
                 std::shared_ptr<TySize> _sz)
    : w(std::move(_w)), z(std::move(_z)), x(std::move(_x)), y(std::move(_y)),
      a(std::move(_a)), b(std::move(_b)), sz(std::move(_sz)) {}
void TyOrXor::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(w));
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(b));
  archive(CEREAL_NVP(sz));
}

ConsOrXor::ConsOrXor(std::shared_ptr<TyOrXor> _or_xor)
    : or_xor(std::move(_or_xor)) {}
std::shared_ptr<TyInfrule>
ConsOrXor::make(std::shared_ptr<TyValue> _w, std::shared_ptr<TyValue> _z,
                std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
                std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
                std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyOrXor> _val(
      new TyOrXor(std::move(_w), std::move(_z), std::move(_x), std::move(_y),
                  std::move(_a), std::move(_b), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsOrXor(std::move(_val)));
}
void ConsOrXor::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("OrXor");
  archive(CEREAL_NVP(or_xor));
}

TyOrXor2::TyOrXor2(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x1,
                   std::shared_ptr<TyValue> _y1, std::shared_ptr<TyValue> _x2,
                   std::shared_ptr<TyValue> _y2, std::shared_ptr<TyValue> _a,
                   std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), x1(std::move(_x1)), y1(std::move(_y1)),
      x2(std::move(_x2)), y2(std::move(_y2)), a(std::move(_a)),
      b(std::move(_b)), sz(std::move(_sz)) {}
void TyOrXor2::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x1));
  archive(CEREAL_NVP(y1));
  archive(CEREAL_NVP(x2));
  archive(CEREAL_NVP(y2));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(b));
  archive(CEREAL_NVP(sz));
}

ConsOrXor2::ConsOrXor2(std::shared_ptr<TyOrXor2> _or_xor2)
    : or_xor2(std::move(_or_xor2)) {}
std::shared_ptr<TyInfrule>
ConsOrXor2::make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x1,
                 std::shared_ptr<TyValue> _y1, std::shared_ptr<TyValue> _x2,
                 std::shared_ptr<TyValue> _y2, std::shared_ptr<TyValue> _a,
                 std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyOrXor2> _val(new TyOrXor2(
      std::move(_z), std::move(_x1), std::move(_y1), std::move(_x2),
      std::move(_y2), std::move(_a), std::move(_b), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsOrXor2(std::move(_val)));
}
void ConsOrXor2::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("OrXor2");
  archive(CEREAL_NVP(or_xor2));
}

TyOrXor4::TyOrXor4(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
                   std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _a,
                   std::shared_ptr<TyValue> _b, std::shared_ptr<TyValue> _nb,
                   std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)), a(std::move(_a)),
      b(std::move(_b)), nb(std::move(_nb)), sz(std::move(_sz)) {}
void TyOrXor4::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(b));
  archive(CEREAL_NVP(nb));
  archive(CEREAL_NVP(sz));
}

ConsOrXor4::ConsOrXor4(std::shared_ptr<TyOrXor4> _or_xor4)
    : or_xor4(std::move(_or_xor4)) {}
std::shared_ptr<TyInfrule>
ConsOrXor4::make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
                 std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _a,
                 std::shared_ptr<TyValue> _b, std::shared_ptr<TyValue> _nb,
                 std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyOrXor4> _val(
      new TyOrXor4(std::move(_z), std::move(_x), std::move(_y), std::move(_a),
                   std::move(_b), std::move(_nb), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsOrXor4(std::move(_val)));
}
void ConsOrXor4::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("OrXor4");
  archive(CEREAL_NVP(or_xor4));
}

}
