#include <sstream>
#include <fstream>
#include <string>
#include <cassert>
#include <cereal/archives/json.hpp>
#include <cereal/types/vector.hpp>
#include <cereal/types/memory.hpp>
#include <cereal/types/polymorphic.hpp>
#include "llvm/LLVMBerry/Structure.h"
#include "llvm/LLVMBerry/ValidationUnit.h"
#include "llvm/LLVMBerry/Infrules.h"

namespace llvmberry {

TyAddConstNot::TyAddConstNot(std::shared_ptr<TyRegister> _z,
                             std::shared_ptr<TyRegister> _y,
                             std::shared_ptr<TyValue> _x,
                             std::shared_ptr<TyConstInt> _c1,
                             std::shared_ptr<TyConstInt> _c2,
                             std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), y(std::move(_y)), x(std::move(_x)), c1(std::move(_c1)),
      c2(std::move(_c2)), sz(std::move(_sz)) {}
void TyAddConstNot::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(c1));
  archive(CEREAL_NVP(c2));
  archive(CEREAL_NVP(sz));
}

ConsAddConstNot::ConsAddConstNot(std::shared_ptr<TyAddConstNot> _add_const_not)
    : add_const_not(std::move(_add_const_not)) {}
std::shared_ptr<TyInfrule> ConsAddConstNot::make(
    std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y,
    std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c1,
    std::shared_ptr<TyConstInt> _c2, std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyAddConstNot> _val(
      new TyAddConstNot(std::move(_z), std::move(_y), std::move(_x),
                        std::move(_c1), std::move(_c2), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsAddConstNot(std::move(_val)));
}
void ConsAddConstNot::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("AddConstNot");
  archive(CEREAL_NVP(add_const_not));
}

TyAddMask::TyAddMask(std::shared_ptr<TyRegister> _z,
                     std::shared_ptr<TyRegister> _y,
                     std::shared_ptr<TyRegister> _yprime,
                     std::shared_ptr<TyValue> _x,
                     std::shared_ptr<TyConstInt> _c1,
                     std::shared_ptr<TyConstInt> _c2,
                     std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), y(std::move(_y)), yprime(std::move(_yprime)),
      x(std::move(_x)), c1(std::move(_c1)), c2(std::move(_c2)),
      sz(std::move(_sz)) {}
void TyAddMask::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(yprime));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(c1));
  archive(CEREAL_NVP(c2));
  archive(CEREAL_NVP(sz));
}

ConsAddMask::ConsAddMask(std::shared_ptr<TyAddMask> _add_mask)
    : add_mask(std::move(_add_mask)) {}
std::shared_ptr<TyInfrule> ConsAddMask::make(
    std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y,
    std::shared_ptr<TyRegister> _yprime, std::shared_ptr<TyValue> _x,
    std::shared_ptr<TyConstInt> _c1, std::shared_ptr<TyConstInt> _c2,
    std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyAddMask> _val(new TyAddMask(
      std::move(_z), std::move(_y), std::move(_yprime), std::move(_x),
      std::move(_c1), std::move(_c2), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsAddMask(std::move(_val)));
}
void ConsAddMask::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("AddMask");
  archive(CEREAL_NVP(add_mask));
}

TyAddSelectZero::TyAddSelectZero(std::shared_ptr<TyRegister> _z,
                                 std::shared_ptr<TyRegister> _x,
                                 std::shared_ptr<TyRegister> _y,
                                 std::shared_ptr<TyValue> _c,
                                 std::shared_ptr<TyValue> _n,
                                 std::shared_ptr<TyValue> _a,
                                 std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)), c(std::move(_c)),
      n(std::move(_n)), a(std::move(_a)), sz(std::move(_sz)) {}
void TyAddSelectZero::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(c));
  archive(CEREAL_NVP(n));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(sz));
}

ConsAddSelectZero::ConsAddSelectZero(
    std::shared_ptr<TyAddSelectZero> _add_select_zero)
    : add_select_zero(std::move(_add_select_zero)) {}
std::shared_ptr<TyInfrule> ConsAddSelectZero::make(
    std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x,
    std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _c,
    std::shared_ptr<TyValue> _n, std::shared_ptr<TyValue> _a,
    std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyAddSelectZero> _val(new TyAddSelectZero(
      std::move(_z), std::move(_x), std::move(_y), std::move(_c), std::move(_n),
      std::move(_a), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsAddSelectZero(std::move(_val)));
}
void ConsAddSelectZero::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("AddSelectZero");
  archive(CEREAL_NVP(add_select_zero));
}

TyAddSelectZero2::TyAddSelectZero2(std::shared_ptr<TyRegister> _z,
                                   std::shared_ptr<TyRegister> _x,
                                   std::shared_ptr<TyRegister> _y,
                                   std::shared_ptr<TyValue> _c,
                                   std::shared_ptr<TyValue> _n,
                                   std::shared_ptr<TyValue> _a,
                                   std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)), c(std::move(_c)),
      n(std::move(_n)), a(std::move(_a)), sz(std::move(_sz)) {}
void TyAddSelectZero2::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(c));
  archive(CEREAL_NVP(n));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(sz));
}

ConsAddSelectZero2::ConsAddSelectZero2(
    std::shared_ptr<TyAddSelectZero2> _add_select_zero2)
    : add_select_zero2(std::move(_add_select_zero2)) {}
std::shared_ptr<TyInfrule> ConsAddSelectZero2::make(
    std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x,
    std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _c,
    std::shared_ptr<TyValue> _n, std::shared_ptr<TyValue> _a,
    std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyAddSelectZero2> _val(new TyAddSelectZero2(
      std::move(_z), std::move(_x), std::move(_y), std::move(_c), std::move(_n),
      std::move(_a), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsAddSelectZero2(std::move(_val)));
}
void ConsAddSelectZero2::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("AddSelectZero2");
  archive(CEREAL_NVP(add_select_zero2));
}

TyAddSub::TyAddSub(std::shared_ptr<TyRegister> _minusy,
                   std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x,
                   std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz)
    : minusy(std::move(_minusy)), z(std::move(_z)), x(std::move(_x)),
      y(std::move(_y)), sz(std::move(_sz)) {}

void TyAddSub::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(minusy), CEREAL_NVP(z), CEREAL_NVP(x), CEREAL_NVP(y),
          CEREAL_NVP(sz));
}

TyAddCommutativeTgt::TyAddCommutativeTgt(std::shared_ptr<TyRegister> _z,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)), sz(std::move(_sz)) {
}

void TyAddCommutativeTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(x), CEREAL_NVP(y), CEREAL_NVP(sz));
}

ConsAddCommutativeTgt::ConsAddCommutativeTgt(
    std::shared_ptr<TyAddCommutativeTgt> _add_comm)
    : add_commutative_tgt(std::move(_add_comm)) {}

void
ConsAddCommutativeTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("AddCommutativeTgt");
  archive(CEREAL_NVP(add_commutative_tgt));
}

std::shared_ptr<TyInfrule> ConsAddCommutativeTgt::make(
    std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x,
    std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyAddCommutativeTgt> _add_comm(new TyAddCommutativeTgt(
      std::move(_z), std::move(_x), std::move(_y), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(
      new ConsAddCommutativeTgt(std::move(_add_comm)));
}

TyBopAssociative::TyBopAssociative(std::shared_ptr<TyRegister> _x,
                                   std::shared_ptr<TyRegister> _y,
                                   std::shared_ptr<TyRegister> _z, TyBop _bop,
                                   std::shared_ptr<TyConstInt> _c1,
                                   std::shared_ptr<TyConstInt> _c2,
                                   std::shared_ptr<TyConstInt> _c3,
                                   std::shared_ptr<TySize> _sz)
    : x(_x), y(_y), z(_z), bop(_bop), c1(_c1), c2(_c2), c3(_c3), sz(_sz) {}

void TyBopAssociative::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(x), CEREAL_NVP(y), CEREAL_NVP(z),
          cereal::make_nvp("bop", llvmberry::toString(bop)), CEREAL_NVP(c1),
          CEREAL_NVP(c2), CEREAL_NVP(c3), CEREAL_NVP(sz));
}

ConsBopAssociative::ConsBopAssociative(
    std::shared_ptr<TyBopAssociative> _bop_associative)
    : bop_associative(_bop_associative) {}

void ConsBopAssociative::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("BopAssociative");
  archive(CEREAL_NVP(bop_associative));
}

std::shared_ptr<TyInfrule> ConsBopAssociative::make(
    std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y,
    std::shared_ptr<TyRegister> _z, TyBop _bop, std::shared_ptr<TyConstInt> _c1,
    std::shared_ptr<TyConstInt> _c2, std::shared_ptr<TyConstInt> _c3,
    std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyBopAssociative> _add_assoc(
      new TyBopAssociative(_x, _y, _z, _bop, _c1, _c2, _c3, _sz));
  return std::shared_ptr<TyInfrule>(new ConsBopAssociative(_add_assoc));
}

TyBopCommutative::TyBopCommutative(std::shared_ptr<TyExpr> _e, TyBop _bop,
                                   std::shared_ptr<TyValue> _x,
                                   std::shared_ptr<TyValue> _y,
                                   std::shared_ptr<TySize> _sz)
    : e(_e), bop(_bop), x(_x), y(_y), sz(_sz) {}

void TyBopCommutative::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(e), cereal::make_nvp("bop", llvmberry::toString(bop)),
          CEREAL_NVP(x), CEREAL_NVP(y), CEREAL_NVP(sz));
}

ConsBopCommutative::ConsBopCommutative(
    std::shared_ptr<TyBopCommutative> _bop_commutative)
    : bop_commutative(_bop_commutative) {}

void ConsBopCommutative::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("BopCommutative");
  archive(CEREAL_NVP(bop_commutative));
}

std::shared_ptr<TyInfrule> ConsBopCommutative::make(
    std::shared_ptr<TyExpr> _e, TyBop _bop,
    std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
    std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyBopCommutative> _bop_comm(
      new TyBopCommutative(_e, _bop, _x, _y, _sz));
  return std::shared_ptr<TyInfrule>(new ConsBopCommutative(_bop_comm));
}

TyAddDistSub::TyAddDistSub(std::shared_ptr<TyRegister> _z,
                           std::shared_ptr<TyRegister> _minusx,
                           std::shared_ptr<TyValue> _minusy,
                           std::shared_ptr<TyRegister> _w,
                           std::shared_ptr<TyValue> _x,
                           std::shared_ptr<TyValue> _y,
                           std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), minusx(std::move(_minusx)), minusy(std::move(_minusy)),
      w(std::move(_w)), x(std::move(_x)), y(std::move(_y)), sz(std::move(_sz)) {
}
void TyAddDistSub::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(minusx));
  archive(CEREAL_NVP(minusy));
  archive(CEREAL_NVP(w));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(sz));
}

ConsAddDistSub::ConsAddDistSub(std::shared_ptr<TyAddDistSub> _add_dist_sub)
    : add_dist_sub(std::move(_add_dist_sub)) {}
std::shared_ptr<TyInfrule> ConsAddDistSub::make(
    std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _minusx,
    std::shared_ptr<TyValue> _minusy, std::shared_ptr<TyRegister> _w,
    std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
    std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyAddDistSub> _val(new TyAddDistSub(
      std::move(_z), std::move(_minusx), std::move(_minusy), std::move(_w),
      std::move(_x), std::move(_y), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsAddDistSub(std::move(_val)));
}
void ConsAddDistSub::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("AddDistSub");
  archive(CEREAL_NVP(add_dist_sub));
}

ConsAddSub::ConsAddSub(std::shared_ptr<TyAddSub> _add_sub)
    : add_sub(std::move(_add_sub)) {}

void ConsAddSub::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("AddSub");
  archive(CEREAL_NVP(add_sub));
}

std::shared_ptr<TyInfrule> ConsAddSub::make(std::shared_ptr<TyRegister> _minusy,
                                            std::shared_ptr<TyRegister> _z,
                                            std::shared_ptr<TyValue> _x,
                                            std::shared_ptr<TyValue> _y,
                                            std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyAddSub> _add_sub(
      new TyAddSub(std::move(_minusy), std::move(_z), std::move(_x),
                   std::move(_y), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsAddSub(std::move(_add_sub)));
}

TyAddOnebit::TyAddOnebit(std::shared_ptr<TyRegister> _z,
                         std::shared_ptr<TyValue> _x,
                         std::shared_ptr<TyValue> _y)
    : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)) {}
void TyAddOnebit::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
}

ConsAddOnebit::ConsAddOnebit(std::shared_ptr<TyAddOnebit> _add_onebit)
    : add_onebit(std::move(_add_onebit)) {}
std::shared_ptr<TyInfrule> ConsAddOnebit::make(std::shared_ptr<TyRegister> _z,
                                               std::shared_ptr<TyValue> _x,
                                               std::shared_ptr<TyValue> _y) {
  std::shared_ptr<TyAddOnebit> _val(
      new TyAddOnebit(std::move(_z), std::move(_x), std::move(_y)));
  return std::shared_ptr<TyInfrule>(new ConsAddOnebit(std::move(_val)));
}
void ConsAddOnebit::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("AddOnebit");
  archive(CEREAL_NVP(add_onebit));
}

TyAddOrAnd::TyAddOrAnd(std::shared_ptr<TyRegister> _z,
                       std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
                       std::shared_ptr<TyRegister> _x,
                       std::shared_ptr<TyRegister> _y,
                       std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), a(std::move(_a)), b(std::move(_b)), x(std::move(_x)),
      y(std::move(_y)), sz(std::move(_sz)) {}
void TyAddOrAnd::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(b));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(sz));
}

ConsAddOrAnd::ConsAddOrAnd(std::shared_ptr<TyAddOrAnd> _add_or_and)
    : add_or_and(std::move(_add_or_and)) {}
std::shared_ptr<TyInfrule> ConsAddOrAnd::make(std::shared_ptr<TyRegister> _z,
                                              std::shared_ptr<TyValue> _a,
                                              std::shared_ptr<TyValue> _b,
                                              std::shared_ptr<TyRegister> _x,
                                              std::shared_ptr<TyRegister> _y,
                                              std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyAddOrAnd> _val(
      new TyAddOrAnd(std::move(_z), std::move(_a), std::move(_b), std::move(_x),
                     std::move(_y), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsAddOrAnd(std::move(_val)));
}
void ConsAddOrAnd::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("AddOrAnd");
  archive(CEREAL_NVP(add_or_and));
}

TyAddSextSext::TyAddSextSext(
    std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _xprime,
    std::shared_ptr<TyValue> _x, std::shared_ptr<TyRegister> _yprime,
    std::shared_ptr<TyValue> _y, std::shared_ptr<TyRegister> _zprime,
    std::shared_ptr<TySize> _from_sz, std::shared_ptr<TySize> _to_sz)
    : z(std::move(_z)), xprime(std::move(_xprime)), x(std::move(_x)),
      yprime(std::move(_yprime)), y(std::move(_y)), zprime(std::move(_zprime)),
      from_sz(std::move(_from_sz)), to_sz(std::move(_to_sz)) {}
void TyAddSextSext::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(xprime));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(yprime));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(zprime));
  archive(CEREAL_NVP(from_sz));
  archive(CEREAL_NVP(to_sz));
}

ConsAddSextSext::ConsAddSextSext(std::shared_ptr<TyAddSextSext> _add_sext_sext)
    : add_sext_sext(std::move(_add_sext_sext)) {}
std::shared_ptr<TyInfrule> ConsAddSextSext::make(
    std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _xprime,
    std::shared_ptr<TyValue> _x, std::shared_ptr<TyRegister> _yprime,
    std::shared_ptr<TyValue> _y, std::shared_ptr<TyRegister> _zprime,
    std::shared_ptr<TySize> _from_sz, std::shared_ptr<TySize> _to_sz) {
  std::shared_ptr<TyAddSextSext> _val(
      new TyAddSextSext(std::move(_z), std::move(_xprime), std::move(_x),
                        std::move(_yprime), std::move(_y), std::move(_zprime),
                        std::move(_from_sz), std::move(_to_sz)));
  return std::shared_ptr<TyInfrule>(new ConsAddSextSext(std::move(_val)));
}
void ConsAddSextSext::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("AddSextSext");
  archive(CEREAL_NVP(add_sext_sext));
}

TyAddShift::TyAddShift(std::shared_ptr<TyRegister> _y,
                       std::shared_ptr<TyValue> _v, std::shared_ptr<TySize> _sz)
    : y(std::move(_y)), v(std::move(_v)), sz(std::move(_sz)) {}

void TyAddShift::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(y), CEREAL_NVP(v), CEREAL_NVP(sz));
}

ConsAddShift::ConsAddShift(std::shared_ptr<TyAddShift> _add_shift)
    : add_shift(std::move(_add_shift)) {}

void ConsAddShift::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("AddShift");
  archive(CEREAL_NVP(add_shift));
}

std::shared_ptr<TyInfrule> ConsAddShift::make(std::shared_ptr<TyRegister> _y,
                                              std::shared_ptr<TyValue> _v,
                                              std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyAddShift> _add_shift(
      new TyAddShift(std::move(_y), std::move(_v), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsAddShift(std::move(_add_shift)));
}

TyAddSignbit::TyAddSignbit(std::shared_ptr<TyRegister> _x,
                           std::shared_ptr<TyValue> _e1,
                           std::shared_ptr<TyValue> _e2,
                           std::shared_ptr<TySize> _sz)
    : x(std::move(_x)), e1(std::move(_e1)), e2(std::move(_e2)),
      sz(std::move(_sz)) {}

void TyAddSignbit::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(e1));
  archive(CEREAL_NVP(e2));
  archive(CEREAL_NVP(sz));
}

ConsAddSignbit::ConsAddSignbit(std::shared_ptr<TyAddSignbit> _add_signbit)
    : add_signbit(std::move(_add_signbit)) {}

void ConsAddSignbit::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("AddSignbit");
  archive(CEREAL_NVP(add_signbit));
}

std::shared_ptr<TyInfrule> ConsAddSignbit::make(std::shared_ptr<TyRegister> _x,
                                                std::shared_ptr<TyValue> _e1,
                                                std::shared_ptr<TyValue> _e2,
                                                std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyAddSignbit> _add_signbit(new TyAddSignbit(
      std::move(_x), std::move(_e1), std::move(_e2), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(
      new ConsAddSignbit(std::move(_add_signbit)));
}

TyAddXorAnd::TyAddXorAnd(std::shared_ptr<TyRegister> _z,
                         std::shared_ptr<TyValue> _a,
                         std::shared_ptr<TyValue> _b,
                         std::shared_ptr<TyRegister> _x,
                         std::shared_ptr<TyRegister> _y,
                         std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), a(std::move(_a)), b(std::move(_b)), x(std::move(_x)),
      y(std::move(_y)), sz(std::move(_sz)) {}
void TyAddXorAnd::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(b));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(sz));
}

ConsAddXorAnd::ConsAddXorAnd(std::shared_ptr<TyAddXorAnd> _add_xor_and)
    : add_xor_and(std::move(_add_xor_and)) {}
std::shared_ptr<TyInfrule> ConsAddXorAnd::make(std::shared_ptr<TyRegister> _z,
                                               std::shared_ptr<TyValue> _a,
                                               std::shared_ptr<TyValue> _b,
                                               std::shared_ptr<TyRegister> _x,
                                               std::shared_ptr<TyRegister> _y,
                                               std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyAddXorAnd> _val(
      new TyAddXorAnd(std::move(_z), std::move(_a), std::move(_b),
                      std::move(_x), std::move(_y), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsAddXorAnd(std::move(_val)));
}
void ConsAddXorAnd::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("AddXorAnd");
  archive(CEREAL_NVP(add_xor_and));
}

TyAddZextBool::TyAddZextBool(std::shared_ptr<TyRegister> _x,
                             std::shared_ptr<TyRegister> _y,
                             std::shared_ptr<TyValue> _b,
                             std::shared_ptr<TyConstInt> _c,
                             std::shared_ptr<TyConstInt> _cprime,
                             std::shared_ptr<TySize> _sz)
    : x(std::move(_x)), y(std::move(_y)), b(std::move(_b)), c(std::move(_c)),
      cprime(std::move(_cprime)), sz(std::move(_sz)) {}
void TyAddZextBool::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(b));
  archive(CEREAL_NVP(c));
  archive(CEREAL_NVP(cprime));
  archive(CEREAL_NVP(sz));
}

ConsAddZextBool::ConsAddZextBool(std::shared_ptr<TyAddZextBool> _add_zext_bool)
    : add_zext_bool(std::move(_add_zext_bool)) {}
std::shared_ptr<TyInfrule> ConsAddZextBool::make(
    std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y,
    std::shared_ptr<TyValue> _b, std::shared_ptr<TyConstInt> _c,
    std::shared_ptr<TyConstInt> _cprime, std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyAddZextBool> _val(
      new TyAddZextBool(std::move(_x), std::move(_y), std::move(_b),
                        std::move(_c), std::move(_cprime), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsAddZextBool(std::move(_val)));
}
void ConsAddZextBool::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("AddZextBool");
  archive(CEREAL_NVP(add_zext_bool));
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

TyBitcastBitcast::TyBitcastBitcast(std::shared_ptr<TyValue> _src,
                                   std::shared_ptr<TyValue> _mid,
                                   std::shared_ptr<TyValue> _dst,
                                   std::shared_ptr<TyValueType> _srcty,
                                   std::shared_ptr<TyValueType> _midty,
                                   std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TyBitcastBitcast::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src));
  archive(CEREAL_NVP(mid));
  archive(CEREAL_NVP(dst));
  archive(CEREAL_NVP(srcty));
  archive(CEREAL_NVP(midty));
  archive(CEREAL_NVP(dstty));
}
ConsBitcastBitcast::ConsBitcastBitcast(
    std::shared_ptr<TyBitcastBitcast> _bitcast_bitcast)
    : bitcast_bitcast(_bitcast_bitcast) {}
std::shared_ptr<TyInfrule> ConsBitcastBitcast::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TyBitcastBitcast> _val(
      new TyBitcastBitcast(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsBitcastBitcast(_val));
}
void ConsBitcastBitcast::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("BitcastBitcast");
  archive(CEREAL_NVP(bitcast_bitcast));
}

TyBitcastFpext::TyBitcastFpext(std::shared_ptr<TyValue> _src,
                               std::shared_ptr<TyValue> _mid,
                               std::shared_ptr<TyValue> _dst,
                               std::shared_ptr<TyValueType> _srcty,
                               std::shared_ptr<TyValueType> _midty,
                               std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TyBitcastFpext::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src));
  archive(CEREAL_NVP(mid));
  archive(CEREAL_NVP(dst));
  archive(CEREAL_NVP(srcty));
  archive(CEREAL_NVP(midty));
  archive(CEREAL_NVP(dstty));
}
ConsBitcastFpext::ConsBitcastFpext(
    std::shared_ptr<TyBitcastFpext> _bitcast_fpext)
    : bitcast_fpext(_bitcast_fpext) {}
std::shared_ptr<TyInfrule> ConsBitcastFpext::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TyBitcastFpext> _val(
      new TyBitcastFpext(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsBitcastFpext(_val));
}
void ConsBitcastFpext::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("BitcastFpext");
  archive(CEREAL_NVP(bitcast_fpext));
}

TyBitcastFptosi::TyBitcastFptosi(std::shared_ptr<TyValue> _src,
                                 std::shared_ptr<TyValue> _mid,
                                 std::shared_ptr<TyValue> _dst,
                                 std::shared_ptr<TyValueType> _srcty,
                                 std::shared_ptr<TyValueType> _midty,
                                 std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TyBitcastFptosi::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src));
  archive(CEREAL_NVP(mid));
  archive(CEREAL_NVP(dst));
  archive(CEREAL_NVP(srcty));
  archive(CEREAL_NVP(midty));
  archive(CEREAL_NVP(dstty));
}
ConsBitcastFptosi::ConsBitcastFptosi(
    std::shared_ptr<TyBitcastFptosi> _bitcast_fptosi)
    : bitcast_fptosi(_bitcast_fptosi) {}
std::shared_ptr<TyInfrule> ConsBitcastFptosi::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TyBitcastFptosi> _val(
      new TyBitcastFptosi(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsBitcastFptosi(_val));
}
void ConsBitcastFptosi::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("BitcastFptosi");
  archive(CEREAL_NVP(bitcast_fptosi));
}

TyBitcastFptoui::TyBitcastFptoui(std::shared_ptr<TyValue> _src,
                                 std::shared_ptr<TyValue> _mid,
                                 std::shared_ptr<TyValue> _dst,
                                 std::shared_ptr<TyValueType> _srcty,
                                 std::shared_ptr<TyValueType> _midty,
                                 std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TyBitcastFptoui::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src));
  archive(CEREAL_NVP(mid));
  archive(CEREAL_NVP(dst));
  archive(CEREAL_NVP(srcty));
  archive(CEREAL_NVP(midty));
  archive(CEREAL_NVP(dstty));
}
ConsBitcastFptoui::ConsBitcastFptoui(
    std::shared_ptr<TyBitcastFptoui> _bitcast_fptoui)
    : bitcast_fptoui(_bitcast_fptoui) {}
std::shared_ptr<TyInfrule> ConsBitcastFptoui::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TyBitcastFptoui> _val(
      new TyBitcastFptoui(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsBitcastFptoui(_val));
}
void ConsBitcastFptoui::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("BitcastFptoui");
  archive(CEREAL_NVP(bitcast_fptoui));
}

TyBitcastFptrunc::TyBitcastFptrunc(std::shared_ptr<TyValue> _src,
                                   std::shared_ptr<TyValue> _mid,
                                   std::shared_ptr<TyValue> _dst,
                                   std::shared_ptr<TyValueType> _srcty,
                                   std::shared_ptr<TyValueType> _midty,
                                   std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TyBitcastFptrunc::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src));
  archive(CEREAL_NVP(mid));
  archive(CEREAL_NVP(dst));
  archive(CEREAL_NVP(srcty));
  archive(CEREAL_NVP(midty));
  archive(CEREAL_NVP(dstty));
}
ConsBitcastFptrunc::ConsBitcastFptrunc(
    std::shared_ptr<TyBitcastFptrunc> _bitcast_fptrunc)
    : bitcast_fptrunc(_bitcast_fptrunc) {}
std::shared_ptr<TyInfrule> ConsBitcastFptrunc::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TyBitcastFptrunc> _val(
      new TyBitcastFptrunc(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsBitcastFptrunc(_val));
}
void ConsBitcastFptrunc::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("BitcastFptrunc");
  archive(CEREAL_NVP(bitcast_fptrunc));
}

TyBitcastInttoptr::TyBitcastInttoptr(std::shared_ptr<TyValue> _src,
                                     std::shared_ptr<TyValue> _mid,
                                     std::shared_ptr<TyValue> _dst,
                                     std::shared_ptr<TyValueType> _srcty,
                                     std::shared_ptr<TyValueType> _midty,
                                     std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TyBitcastInttoptr::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src));
  archive(CEREAL_NVP(mid));
  archive(CEREAL_NVP(dst));
  archive(CEREAL_NVP(srcty));
  archive(CEREAL_NVP(midty));
  archive(CEREAL_NVP(dstty));
}
ConsBitcastInttoptr::ConsBitcastInttoptr(
    std::shared_ptr<TyBitcastInttoptr> _bitcast_inttoptr)
    : bitcast_inttoptr(_bitcast_inttoptr) {}
std::shared_ptr<TyInfrule> ConsBitcastInttoptr::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TyBitcastInttoptr> _val(
      new TyBitcastInttoptr(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsBitcastInttoptr(_val));
}
void ConsBitcastInttoptr::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("BitcastInttoptr");
  archive(CEREAL_NVP(bitcast_inttoptr));
}

TyBitcastLoad::TyBitcastLoad(std::shared_ptr<TyValue> _ptr,
                             std::shared_ptr<TyValueType> _ptrty,
                             std::shared_ptr<TyValue> _v1,
                             std::shared_ptr<TyValueType> _ptrty2,
                             std::shared_ptr<TyValue> _v2,
                             std::shared_ptr<TySize> _a)
    : ptr(_ptr), ptrty(_ptrty), v1(_v1), ptrty2(_ptrty2), v2(_v2), a(_a) {}
void TyBitcastLoad::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(ptr), CEREAL_NVP(ptrty), CEREAL_NVP(v1),
          CEREAL_NVP(ptrty2), CEREAL_NVP(v2), CEREAL_NVP(a));
}
ConsBitcastLoad::ConsBitcastLoad(std::shared_ptr<TyBitcastLoad> _bitcast_load)
    : bitcast_load(_bitcast_load) {}
std::shared_ptr<TyInfrule> ConsBitcastLoad::make(
    std::shared_ptr<TyValue> _ptr, std::shared_ptr<TyValueType> _ptrty,
    std::shared_ptr<TyValue> _v1, std::shared_ptr<TyValueType> _ptrty2,
    std::shared_ptr<TyValue> _v2, std::shared_ptr<TySize> _a) {
  std::shared_ptr<TyBitcastLoad> _val(
      new TyBitcastLoad(_ptr, _ptrty, _v1, _ptrty2, _v2, _a));
  return std::shared_ptr<TyInfrule>(new ConsBitcastLoad(_val));
}
void ConsBitcastLoad::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("BitcastLoad");
  archive(CEREAL_NVP(bitcast_load));
}
TyBitcastPtrtoint::TyBitcastPtrtoint(std::shared_ptr<TyValue> _src,
                                     std::shared_ptr<TyValue> _mid,
                                     std::shared_ptr<TyValue> _dst,
                                     std::shared_ptr<TyValueType> _srcty,
                                     std::shared_ptr<TyValueType> _midty,
                                     std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TyBitcastPtrtoint::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src));
  archive(CEREAL_NVP(mid));
  archive(CEREAL_NVP(dst));
  archive(CEREAL_NVP(srcty));
  archive(CEREAL_NVP(midty));
  archive(CEREAL_NVP(dstty));
}
ConsBitcastPtrtoint::ConsBitcastPtrtoint(
    std::shared_ptr<TyBitcastPtrtoint> _bitcast_ptrtoint)
    : bitcast_ptrtoint(_bitcast_ptrtoint) {}
std::shared_ptr<TyInfrule> ConsBitcastPtrtoint::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TyBitcastPtrtoint> _val(
      new TyBitcastPtrtoint(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsBitcastPtrtoint(_val));
}
void ConsBitcastPtrtoint::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("BitcastPtrtoint");
  archive(CEREAL_NVP(bitcast_ptrtoint));
}

TyBitcastSext::TyBitcastSext(std::shared_ptr<TyValue> _src,
                             std::shared_ptr<TyValue> _mid,
                             std::shared_ptr<TyValue> _dst,
                             std::shared_ptr<TyValueType> _srcty,
                             std::shared_ptr<TyValueType> _midty,
                             std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TyBitcastSext::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src));
  archive(CEREAL_NVP(mid));
  archive(CEREAL_NVP(dst));
  archive(CEREAL_NVP(srcty));
  archive(CEREAL_NVP(midty));
  archive(CEREAL_NVP(dstty));
}
ConsBitcastSext::ConsBitcastSext(std::shared_ptr<TyBitcastSext> _bitcast_sext)
    : bitcast_sext(_bitcast_sext) {}
std::shared_ptr<TyInfrule> ConsBitcastSext::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TyBitcastSext> _val(
      new TyBitcastSext(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsBitcastSext(_val));
}
void ConsBitcastSext::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("BitcastSext");
  archive(CEREAL_NVP(bitcast_sext));
}

TyBitcastSametype::TyBitcastSametype(std::shared_ptr<TyValue> _src,
                                     std::shared_ptr<TyValue> _dst,
                                     std::shared_ptr<TyValueType> _tty)
    : src(_src), dst(_dst), tty(_tty) {}
void TyBitcastSametype::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src));
  archive(CEREAL_NVP(dst));
  archive(CEREAL_NVP(tty));
}
ConsBitcastSametype::ConsBitcastSametype(
    std::shared_ptr<TyBitcastSametype> _bitcast_sametype)
    : bitcast_sametype(_bitcast_sametype) {}
std::shared_ptr<TyInfrule>
ConsBitcastSametype::make(std::shared_ptr<TyValue> _src,
                          std::shared_ptr<TyValue> _dst,
                          std::shared_ptr<TyValueType> _tty) {
  std::shared_ptr<TyBitcastSametype> _val(
      new TyBitcastSametype(_src, _dst, _tty));
  return std::shared_ptr<TyInfrule>(new ConsBitcastSametype(_val));
}
void ConsBitcastSametype::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("BitcastSametype");
  archive(CEREAL_NVP(bitcast_sametype));
}

TyBitcastSitofp::TyBitcastSitofp(std::shared_ptr<TyValue> _src,
                                 std::shared_ptr<TyValue> _mid,
                                 std::shared_ptr<TyValue> _dst,
                                 std::shared_ptr<TyValueType> _srcty,
                                 std::shared_ptr<TyValueType> _midty,
                                 std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TyBitcastSitofp::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src));
  archive(CEREAL_NVP(mid));
  archive(CEREAL_NVP(dst));
  archive(CEREAL_NVP(srcty));
  archive(CEREAL_NVP(midty));
  archive(CEREAL_NVP(dstty));
}
ConsBitcastSitofp::ConsBitcastSitofp(
    std::shared_ptr<TyBitcastSitofp> _bitcast_sitofp)
    : bitcast_sitofp(_bitcast_sitofp) {}
std::shared_ptr<TyInfrule> ConsBitcastSitofp::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TyBitcastSitofp> _val(
      new TyBitcastSitofp(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsBitcastSitofp(_val));
}
void ConsBitcastSitofp::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("BitcastSitofp");
  archive(CEREAL_NVP(bitcast_sitofp));
}

TyBitcastTrunc::TyBitcastTrunc(std::shared_ptr<TyValue> _src,
                               std::shared_ptr<TyValue> _mid,
                               std::shared_ptr<TyValue> _dst,
                               std::shared_ptr<TyValueType> _srcty,
                               std::shared_ptr<TyValueType> _midty,
                               std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TyBitcastTrunc::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src));
  archive(CEREAL_NVP(mid));
  archive(CEREAL_NVP(dst));
  archive(CEREAL_NVP(srcty));
  archive(CEREAL_NVP(midty));
  archive(CEREAL_NVP(dstty));
}
ConsBitcastTrunc::ConsBitcastTrunc(
    std::shared_ptr<TyBitcastTrunc> _bitcast_trunc)
    : bitcast_trunc(_bitcast_trunc) {}
std::shared_ptr<TyInfrule> ConsBitcastTrunc::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TyBitcastTrunc> _val(
      new TyBitcastTrunc(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsBitcastTrunc(_val));
}
void ConsBitcastTrunc::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("BitcastTrunc");
  archive(CEREAL_NVP(bitcast_trunc));
}

TyBitcastUitofp::TyBitcastUitofp(std::shared_ptr<TyValue> _src,
                                 std::shared_ptr<TyValue> _mid,
                                 std::shared_ptr<TyValue> _dst,
                                 std::shared_ptr<TyValueType> _srcty,
                                 std::shared_ptr<TyValueType> _midty,
                                 std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TyBitcastUitofp::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src));
  archive(CEREAL_NVP(mid));
  archive(CEREAL_NVP(dst));
  archive(CEREAL_NVP(srcty));
  archive(CEREAL_NVP(midty));
  archive(CEREAL_NVP(dstty));
}
ConsBitcastUitofp::ConsBitcastUitofp(
    std::shared_ptr<TyBitcastUitofp> _bitcast_uitofp)
    : bitcast_uitofp(_bitcast_uitofp) {}
std::shared_ptr<TyInfrule> ConsBitcastUitofp::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TyBitcastUitofp> _val(
      new TyBitcastUitofp(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsBitcastUitofp(_val));
}
void ConsBitcastUitofp::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("BitcastUitofp");
  archive(CEREAL_NVP(bitcast_uitofp));
}

TyBitcastZext::TyBitcastZext(std::shared_ptr<TyValue> _src,
                             std::shared_ptr<TyValue> _mid,
                             std::shared_ptr<TyValue> _dst,
                             std::shared_ptr<TyValueType> _srcty,
                             std::shared_ptr<TyValueType> _midty,
                             std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TyBitcastZext::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src));
  archive(CEREAL_NVP(mid));
  archive(CEREAL_NVP(dst));
  archive(CEREAL_NVP(srcty));
  archive(CEREAL_NVP(midty));
  archive(CEREAL_NVP(dstty));
}
ConsBitcastZext::ConsBitcastZext(std::shared_ptr<TyBitcastZext> _bitcast_zext)
    : bitcast_zext(_bitcast_zext) {}
std::shared_ptr<TyInfrule> ConsBitcastZext::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TyBitcastZext> _val(
      new TyBitcastZext(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsBitcastZext(_val));
}
void ConsBitcastZext::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("BitcastZext");
  archive(CEREAL_NVP(bitcast_zext));
}

TyBitcastptr::TyBitcastptr(std::shared_ptr<TyValue> _vprime,
                           std::shared_ptr<TyExpr> _bitcastinst)
    : vprime(_vprime), bitcastinst(_bitcastinst) {}
void TyBitcastptr::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(vprime));
  archive(CEREAL_NVP(bitcastinst));
}

ConsBitcastptr::ConsBitcastptr(std::shared_ptr<TyBitcastptr> _bitcastptr)
    : bitcastptr(_bitcastptr) {}
std::shared_ptr<TyInfrule>
ConsBitcastptr::make(std::shared_ptr<TyValue> _vprime,
                     std::shared_ptr<TyExpr> _bitcastinst) {
  std::shared_ptr<TyBitcastptr> _val(new TyBitcastptr(_vprime, _bitcastinst));
  return std::shared_ptr<TyInfrule>(new ConsBitcastptr(_val));
}
void ConsBitcastptr::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("Bitcastptr");
  archive(CEREAL_NVP(bitcastptr));
}

TyBitcastptrConst::TyBitcastptrConst(
    std::shared_ptr<TyConstant> _vprime,
    std::shared_ptr<TyConstantExpr> _bitcastinst)
    : vprime(_vprime), bitcastinst(_bitcastinst) {}
void TyBitcastptrConst::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(vprime), CEREAL_NVP(bitcastinst));
}

ConsBitcastptrConst::ConsBitcastptrConst(
    std::shared_ptr<TyBitcastptrConst> _bitcastptr_const)
    : bitcastptr_const(_bitcastptr_const) {}
std::shared_ptr<TyInfrule>
ConsBitcastptrConst::make(std::shared_ptr<TyConstant> _vprime,
                          std::shared_ptr<TyConstantExpr> _bitcastinst) {
  std::shared_ptr<TyBitcastptrConst> _val(
      new TyBitcastptrConst(_vprime, _bitcastinst));
  return std::shared_ptr<TyInfrule>(new ConsBitcastptrConst(_val));
}
void ConsBitcastptrConst::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("BitcastptrConst");
  archive(CEREAL_NVP(bitcastptr_const));
}

TyAndMone::TyAndMone(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
                     std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), x(std::move(_x)), sz(std::move(_sz)) {}
void TyAndMone::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(sz));
}

ConsAndMone::ConsAndMone(std::shared_ptr<TyAndMone> _and_mone)
    : and_mone(std::move(_and_mone)) {}
std::shared_ptr<TyInfrule> ConsAndMone::make(std::shared_ptr<TyValue> _z,
                                             std::shared_ptr<TyValue> _x,
                                             std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyAndMone> _val(
      new TyAndMone(std::move(_z), std::move(_x), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsAndMone(std::move(_val)));
}
void ConsAndMone::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("AndMone");
  archive(CEREAL_NVP(and_mone));
}

TyAndNot::TyAndNot(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
                   std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)), sz(std::move(_sz)) {
}
void TyAndNot::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(sz));
}

ConsAndNot::ConsAndNot(std::shared_ptr<TyAndNot> _and_not)
    : and_not(std::move(_and_not)) {}
std::shared_ptr<TyInfrule> ConsAndNot::make(std::shared_ptr<TyValue> _z,
                                            std::shared_ptr<TyValue> _x,
                                            std::shared_ptr<TyValue> _y,
                                            std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyAndNot> _val(new TyAndNot(std::move(_z), std::move(_x),
                                              std::move(_y), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsAndNot(std::move(_val)));
}
void ConsAndNot::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("AndNot");
  archive(CEREAL_NVP(and_not));
}

TyAndOr::TyAndOr(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
                 std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _a,
                 std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)), a(std::move(_a)),
      sz(std::move(_sz)) {}
void TyAndOr::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(sz));
}

ConsAndOr::ConsAndOr(std::shared_ptr<TyAndOr> _and_or)
    : and_or(std::move(_and_or)) {}
std::shared_ptr<TyInfrule> ConsAndOr::make(std::shared_ptr<TyValue> _z,
                                           std::shared_ptr<TyValue> _x,
                                           std::shared_ptr<TyValue> _y,
                                           std::shared_ptr<TyValue> _a,
                                           std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyAndOr> _val(new TyAndOr(std::move(_z), std::move(_x),
                                            std::move(_y), std::move(_a),
                                            std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsAndOr(std::move(_val)));
}
void ConsAndOr::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("AndOr");
  archive(CEREAL_NVP(and_or));
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

TyAndSame::TyAndSame(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
                     std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), x(std::move(_x)), sz(std::move(_sz)) {}
void TyAndSame::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(sz));
}

ConsAndSame::ConsAndSame(std::shared_ptr<TyAndSame> _and_same)
    : and_same(std::move(_and_same)) {}
std::shared_ptr<TyInfrule> ConsAndSame::make(std::shared_ptr<TyValue> _z,
                                             std::shared_ptr<TyValue> _x,
                                             std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyAndSame> _val(
      new TyAndSame(std::move(_z), std::move(_x), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsAndSame(std::move(_val)));
}
void ConsAndSame::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("AndSame");
  archive(CEREAL_NVP(and_same));
}

TyAndTrueBool::TyAndTrueBool(std::shared_ptr<TyValue> _x,
                             std::shared_ptr<TyValue> _y)
    : x(std::move(_x)), y(std::move(_y)) {}

void TyAndTrueBool::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
}

ConsAndTrueBool::ConsAndTrueBool(std::shared_ptr<TyAndTrueBool> _and_true_bool)
    : and_true_bool(std::move(_and_true_bool)) {}

std::shared_ptr<TyInfrule> ConsAndTrueBool::make(std::shared_ptr<TyValue> _x,
                                                 std::shared_ptr<TyValue> _y) {
  std::shared_ptr<TyAndTrueBool> _val(
      new TyAndTrueBool(std::move(_x), std::move(_y)));
  return std::shared_ptr<TyInfrule>(new ConsAndTrueBool(std::move(_val)));
}

void ConsAndTrueBool::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("AndTrueBool");
  archive(CEREAL_NVP(and_true_bool));
}

TyAndUndef::TyAndUndef(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
                       std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), x(std::move(_x)), sz(std::move(_sz)) {}
void TyAndUndef::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(sz));
}

ConsAndUndef::ConsAndUndef(std::shared_ptr<TyAndUndef> _and_undef)
    : and_undef(std::move(_and_undef)) {}
std::shared_ptr<TyInfrule> ConsAndUndef::make(std::shared_ptr<TyValue> _z,
                                              std::shared_ptr<TyValue> _x,
                                              std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyAndUndef> _val(
      new TyAndUndef(std::move(_z), std::move(_x), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsAndUndef(std::move(_val)));
}
void ConsAndUndef::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("AndUndef");
  archive(CEREAL_NVP(and_undef));
}

TyAndZero::TyAndZero(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
                     std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), x(std::move(_x)), sz(std::move(_sz)) {}
void TyAndZero::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(sz));
}

ConsAndZero::ConsAndZero(std::shared_ptr<TyAndZero> _and_zero)
    : and_zero(std::move(_and_zero)) {}
std::shared_ptr<TyInfrule> ConsAndZero::make(std::shared_ptr<TyValue> _z,
                                             std::shared_ptr<TyValue> _x,
                                             std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyAndZero> _val(
      new TyAndZero(std::move(_z), std::move(_x), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsAndZero(std::move(_val)));
}
void ConsAndZero::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("AndZero");
  archive(CEREAL_NVP(and_zero));
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

TyDiffblockUnique::TyDiffblockUnique(std::shared_ptr<TyRegister> _x,
                                     std::shared_ptr<TyRegister> _y)
    : x(_x), y(_y) {}
void TyDiffblockUnique::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
}

ConsDiffblockUnique::ConsDiffblockUnique(
    std::shared_ptr<TyDiffblockUnique> _diffblock_unique)
    : diffblock_unique(_diffblock_unique) {}
std::shared_ptr<TyInfrule>
ConsDiffblockUnique::make(std::shared_ptr<TyRegister> _x,
                          std::shared_ptr<TyRegister> _y) {
  std::shared_ptr<TyDiffblockUnique> _val(new TyDiffblockUnique(_x, _y));
  return std::shared_ptr<TyInfrule>(new ConsDiffblockUnique(_val));
}
void ConsDiffblockUnique::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("DiffblockUnique");
  archive(CEREAL_NVP(diffblock_unique));
}

TyDiffblockGlobalGlobal::TyDiffblockGlobalGlobal(
    std::shared_ptr<TyConstant> _gx, std::shared_ptr<TyConstant> _gy)
    : gx(std::move(_gx)), gy(std::move(_gy)) {}
void
TyDiffblockGlobalGlobal::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(gx));
  archive(CEREAL_NVP(gy));
}

ConsDiffblockGlobalGlobal::ConsDiffblockGlobalGlobal(
    std::shared_ptr<TyDiffblockGlobalGlobal> _diffblock_global_global)
    : diffblock_global_global(std::move(_diffblock_global_global)) {}
std::shared_ptr<TyInfrule>
ConsDiffblockGlobalGlobal::make(std::shared_ptr<TyConstant> _gx,
                                std::shared_ptr<TyConstant> _gy) {
  std::shared_ptr<TyDiffblockGlobalGlobal> _val(
      new TyDiffblockGlobalGlobal(std::move(_gx), std::move(_gy)));
  return std::shared_ptr<TyInfrule>(
      new ConsDiffblockGlobalGlobal(std::move(_val)));
}
void
ConsDiffblockGlobalGlobal::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("DiffblockGlobalGlobal");
  archive(CEREAL_NVP(diffblock_global_global));
}

TyDiffblockGlobalUnique::TyDiffblockGlobalUnique(
    std::shared_ptr<TyConstant> _gx, std::shared_ptr<TyRegister> _y)
    : gx(_gx), y(_y) {}
void
TyDiffblockGlobalUnique::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(gx), CEREAL_NVP(y));
}

ConsDiffblockGlobalUnique::ConsDiffblockGlobalUnique(
    std::shared_ptr<TyDiffblockGlobalUnique> _diffblock_global_unique)
    : diffblock_global_unique(_diffblock_global_unique) {}
std::shared_ptr<TyInfrule>
ConsDiffblockGlobalUnique::make(std::shared_ptr<TyConstant> _gx,
                                std::shared_ptr<TyRegister> _y) {
  std::shared_ptr<TyDiffblockGlobalUnique> _val(
      new TyDiffblockGlobalUnique(_gx, _y));
  return std::shared_ptr<TyInfrule>(new ConsDiffblockGlobalUnique(_val));
}
void
ConsDiffblockGlobalUnique::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("DiffblockGlobalUnique");
  archive(CEREAL_NVP(diffblock_global_unique));
}

ConsDiffblockLessthan::ConsDiffblockLessthan(
    std::shared_ptr<TyDiffblockLessthan> _diffblock_lessthan)
    : diffblock_lessthan(std::move(_diffblock_lessthan)) {}
std::shared_ptr<TyInfrule> ConsDiffblockLessthan::make(
    std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
    std::shared_ptr<TyValue> _xprime, std::shared_ptr<TyValue> _yprime) {
  std::shared_ptr<TyDiffblockLessthan> _val(new TyDiffblockLessthan(
      std::move(_x), std::move(_y), std::move(_xprime), std::move(_yprime)));
  return std::shared_ptr<TyInfrule>(new ConsDiffblockLessthan(std::move(_val)));
}
void
ConsDiffblockLessthan::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("DiffblockLessthan");
  archive(CEREAL_NVP(diffblock_lessthan));
}

TyDiffblockLessthan::TyDiffblockLessthan(std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TyValue> _xprime,
                                         std::shared_ptr<TyValue> _yprime)
    : x(std::move(_x)), y(std::move(_y)), xprime(std::move(_xprime)),
      yprime(std::move(_yprime)) {}
void TyDiffblockLessthan::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(xprime));
  archive(CEREAL_NVP(yprime));
}

ConsDiffblockNoalias::ConsDiffblockNoalias(
    std::shared_ptr<TyDiffblockNoalias> _diffblock_noalias)
    : diffblock_noalias(std::move(_diffblock_noalias)) {}
std::shared_ptr<TyInfrule> ConsDiffblockNoalias::make(
    std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
    std::shared_ptr<TyPointer> _xprime, std::shared_ptr<TyPointer> _yprime) {
  std::shared_ptr<TyDiffblockNoalias> _val(new TyDiffblockNoalias(
      std::move(_x), std::move(_y), std::move(_xprime), std::move(_yprime)));
  return std::shared_ptr<TyInfrule>(new ConsDiffblockNoalias(std::move(_val)));
}
void ConsDiffblockNoalias::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("DiffblockNoalias");
  archive(CEREAL_NVP(diffblock_noalias));
}

TyDiffblockNoalias::TyDiffblockNoalias(std::shared_ptr<TyValue> _x,
                                       std::shared_ptr<TyValue> _y,
                                       std::shared_ptr<TyPointer> _xprime,
                                       std::shared_ptr<TyPointer> _yprime)
    : x(std::move(_x)), y(std::move(_y)), xprime(std::move(_xprime)),
      yprime(std::move(_yprime)) {}
void TyDiffblockNoalias::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(xprime));
  archive(CEREAL_NVP(yprime));
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

TySubAdd::TySubAdd(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _my,
                   std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
                   std::shared_ptr<TySize> _sz)
    : z(_z), my(_my), x(_x), y(_y), sz(_sz) {}

void TySubAdd::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(my), CEREAL_NVP(x), CEREAL_NVP(y),
          CEREAL_NVP(sz));
}

ConsSubAdd::ConsSubAdd(std::shared_ptr<TySubAdd> _sub_add)
    : sub_add(_sub_add) {}

void ConsSubAdd::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("SubAdd");
  archive(CEREAL_NVP(sub_add));
}

std::shared_ptr<TyInfrule> ConsSubAdd::make(std::shared_ptr<TyRegister> _z,
                                            std::shared_ptr<TyValue> _my,
                                            std::shared_ptr<TyValue> _x,
                                            std::shared_ptr<TyValue> _y,
                                            std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TySubAdd> _sub_add(new TySubAdd(_z, _my, _x, _y, _sz));
  return std::shared_ptr<TyInfrule>(new ConsSubAdd(_sub_add));
}

TySubMone::TySubMone(std::shared_ptr<TyRegister> _z,
                     std::shared_ptr<TyValue> _x, std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), x(std::move(_x)), sz(std::move(_sz)) {}
void TySubMone::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(sz));
}

ConsSubMone::ConsSubMone(std::shared_ptr<TySubMone> _sub_mone)
    : sub_mone(std::move(_sub_mone)) {}
std::shared_ptr<TyInfrule> ConsSubMone::make(std::shared_ptr<TyRegister> _z,
                                             std::shared_ptr<TyValue> _x,
                                             std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TySubMone> _val(
      new TySubMone(std::move(_z), std::move(_x), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsSubMone(std::move(_val)));
}
void ConsSubMone::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SubMone");
  archive(CEREAL_NVP(sub_mone));
}

TySubOnebit::TySubOnebit(std::shared_ptr<TyRegister> _z,
                         std::shared_ptr<TyValue> _x,
                         std::shared_ptr<TyValue> _y)
    : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)) {}
void TySubOnebit::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
}

ConsSubOnebit::ConsSubOnebit(std::shared_ptr<TySubOnebit> _sub_onebit)
    : sub_onebit(std::move(_sub_onebit)) {}
std::shared_ptr<TyInfrule> ConsSubOnebit::make(std::shared_ptr<TyRegister> _z,
                                               std::shared_ptr<TyValue> _x,
                                               std::shared_ptr<TyValue> _y) {
  std::shared_ptr<TySubOnebit> _val(
      new TySubOnebit(std::move(_z), std::move(_x), std::move(_y)));
  return std::shared_ptr<TyInfrule>(new ConsSubOnebit(std::move(_val)));
}
void ConsSubOnebit::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SubOnebit");
  archive(CEREAL_NVP(sub_onebit));
}

TySubConstNot::TySubConstNot(std::shared_ptr<TyRegister> _z,
                             std::shared_ptr<TyRegister> _y,
                             std::shared_ptr<TyValue> _x,
                             std::shared_ptr<TyConstInt> _c1,
                             std::shared_ptr<TyConstInt> _c2,
                             std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), y(std::move(_y)), x(std::move(_x)), c1(std::move(_c1)),
      c2(std::move(_c2)), sz(std::move(_sz)) {}
void TySubConstNot::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(c1));
  archive(CEREAL_NVP(c2));
  archive(CEREAL_NVP(sz));
}

ConsSubConstNot::ConsSubConstNot(std::shared_ptr<TySubConstNot> _sub_const_not)
    : sub_const_not(std::move(_sub_const_not)) {}
std::shared_ptr<TyInfrule> ConsSubConstNot::make(
    std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y,
    std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c1,
    std::shared_ptr<TyConstInt> _c2, std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TySubConstNot> _val(
      new TySubConstNot(std::move(_z), std::move(_y), std::move(_x),
                        std::move(_c1), std::move(_c2), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsSubConstNot(std::move(_val)));
}
void ConsSubConstNot::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SubConstNot");
  archive(CEREAL_NVP(sub_const_not));
}

TySubConstAdd::TySubConstAdd(std::shared_ptr<TyRegister> _z,
                             std::shared_ptr<TyRegister> _y,
                             std::shared_ptr<TyValue> _x,
                             std::shared_ptr<TyConstInt> _c1,
                             std::shared_ptr<TyConstInt> _c2,
                             std::shared_ptr<TyConstInt> _c3,
                             std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), y(std::move(_y)), x(std::move(_x)), c1(std::move(_c1)),
      c2(std::move(_c2)), c3(std::move(_c3)), sz(std::move(_sz)) {}
void TySubConstAdd::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(c1));
  archive(CEREAL_NVP(c2));
  archive(CEREAL_NVP(c3));
  archive(CEREAL_NVP(sz));
}

ConsSubConstAdd::ConsSubConstAdd(std::shared_ptr<TySubConstAdd> _sub_const_add)
    : sub_const_add(std::move(_sub_const_add)) {}
std::shared_ptr<TyInfrule> ConsSubConstAdd::make(
    std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y,
    std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c1,
    std::shared_ptr<TyConstInt> _c2, std::shared_ptr<TyConstInt> _c3,
    std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TySubConstAdd> _val(new TySubConstAdd(
      std::move(_z), std::move(_y), std::move(_x), std::move(_c1),
      std::move(_c2), std::move(_c3), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsSubConstAdd(std::move(_val)));
}
void ConsSubConstAdd::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SubConstAdd");
  archive(CEREAL_NVP(sub_const_add));
}

TySubOrXor::TySubOrXor(std::shared_ptr<TyRegister> _z,
                       std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
                       std::shared_ptr<TyRegister> _x,
                       std::shared_ptr<TyRegister> _y,
                       std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), a(std::move(_a)), b(std::move(_b)), x(std::move(_x)),
      y(std::move(_y)), sz(std::move(_sz)) {}
void TySubOrXor::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(b));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(sz));
}

ConsSubOrXor::ConsSubOrXor(std::shared_ptr<TySubOrXor> _sub_or_xor)
    : sub_or_xor(std::move(_sub_or_xor)) {}
std::shared_ptr<TyInfrule> ConsSubOrXor::make(std::shared_ptr<TyRegister> _z,
                                              std::shared_ptr<TyValue> _a,
                                              std::shared_ptr<TyValue> _b,
                                              std::shared_ptr<TyRegister> _x,
                                              std::shared_ptr<TyRegister> _y,
                                              std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TySubOrXor> _val(
      new TySubOrXor(std::move(_z), std::move(_a), std::move(_b), std::move(_x),
                     std::move(_y), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsSubOrXor(std::move(_val)));
}
void ConsSubOrXor::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SubOrXor");
  archive(CEREAL_NVP(sub_or_xor));
}

TySubSdiv::TySubSdiv(std::shared_ptr<TyRegister> _z,
                     std::shared_ptr<TyRegister> _y,
                     std::shared_ptr<TyValue> _x,
                     std::shared_ptr<TyConstInt> _c,
                     std::shared_ptr<TyConstInt> _cprime,
                     std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), y(std::move(_y)), x(std::move(_x)), c(std::move(_c)),
      cprime(std::move(_cprime)), sz(std::move(_sz)) {}
void TySubSdiv::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(c));
  archive(CEREAL_NVP(cprime));
  archive(CEREAL_NVP(sz));
}

ConsSubSdiv::ConsSubSdiv(std::shared_ptr<TySubSdiv> _sub_sdiv)
    : sub_sdiv(std::move(_sub_sdiv)) {}
std::shared_ptr<TyInfrule> ConsSubSdiv::make(
    std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y,
    std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c,
    std::shared_ptr<TyConstInt> _cprime, std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TySubSdiv> _val(
      new TySubSdiv(std::move(_z), std::move(_y), std::move(_x), std::move(_c),
                    std::move(_cprime), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsSubSdiv(std::move(_val)));
}
void ConsSubSdiv::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SubSdiv");
  archive(CEREAL_NVP(sub_sdiv));
}

TySubShl::TySubShl(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x,
                   std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _mx,
                   std::shared_ptr<TyValue> _a, std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)), mx(std::move(_mx)),
      a(std::move(_a)), sz(std::move(_sz)) {}
void TySubShl::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(mx));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(sz));
}

ConsSubShl::ConsSubShl(std::shared_ptr<TySubShl> _sub_shl)
    : sub_shl(std::move(_sub_shl)) {}
std::shared_ptr<TyInfrule>
ConsSubShl::make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x,
                 std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _mx,
                 std::shared_ptr<TyValue> _a, std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TySubShl> _val(new TySubShl(std::move(_z), std::move(_x),
                                              std::move(_y), std::move(_mx),
                                              std::move(_a), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsSubShl(std::move(_val)));
}
void ConsSubShl::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SubShl");
  archive(CEREAL_NVP(sub_shl));
}

TyFptruncBitcast::TyFptruncBitcast(std::shared_ptr<TyValue> _src,
                                   std::shared_ptr<TyValue> _mid,
                                   std::shared_ptr<TyValue> _dst,
                                   std::shared_ptr<TyValueType> _srcty,
                                   std::shared_ptr<TyValueType> _midty,
                                   std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TyFptruncBitcast::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src));
  archive(CEREAL_NVP(mid));
  archive(CEREAL_NVP(dst));
  archive(CEREAL_NVP(srcty));
  archive(CEREAL_NVP(midty));
  archive(CEREAL_NVP(dstty));
}
ConsFptruncBitcast::ConsFptruncBitcast(
    std::shared_ptr<TyFptruncBitcast> _fptrunc_bitcast)
    : fptrunc_bitcast(_fptrunc_bitcast) {}
std::shared_ptr<TyInfrule> ConsFptruncBitcast::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TyFptruncBitcast> _val(
      new TyFptruncBitcast(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsFptruncBitcast(_val));
}
void ConsFptruncBitcast::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("FptruncBitcast");
  archive(CEREAL_NVP(fptrunc_bitcast));
}

TyFptruncFpext::TyFptruncFpext(std::shared_ptr<TyValue> _src,
                               std::shared_ptr<TyValue> _mid,
                               std::shared_ptr<TyValue> _dst,
                               std::shared_ptr<TyValueType> _srcty,
                               std::shared_ptr<TyValueType> _midty,
                               std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TyFptruncFpext::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src), CEREAL_NVP(mid), CEREAL_NVP(dst), CEREAL_NVP(srcty),
          CEREAL_NVP(midty), CEREAL_NVP(dstty));
}

ConsFptruncFpext::ConsFptruncFpext(
    std::shared_ptr<TyFptruncFpext> _fptrunc_fpext)
    : fptrunc_fpext(_fptrunc_fpext) {}
std::shared_ptr<TyInfrule> ConsFptruncFpext::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TyFptruncFpext> _val(
      new TyFptruncFpext(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsFptruncFpext(_val));
}
void ConsFptruncFpext::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("FptruncFpext");
  archive(CEREAL_NVP(fptrunc_fpext));
}

TyGepzero::TyGepzero(std::shared_ptr<TyValue> _vprime,
                     std::shared_ptr<TyExpr> _gepinst)
    : vprime(_vprime), gepinst(_gepinst) {}
void TyGepzero::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(vprime));
  archive(CEREAL_NVP(gepinst));
}

ConsGepzero::ConsGepzero(std::shared_ptr<TyGepzero> _gepzero)
    : gepzero(_gepzero) {}
std::shared_ptr<TyInfrule> ConsGepzero::make(std::shared_ptr<TyValue> _vprime,
                                             std::shared_ptr<TyExpr> _gepinst) {
  std::shared_ptr<TyGepzero> _val(new TyGepzero(_vprime, _gepinst));
  return std::shared_ptr<TyInfrule>(new ConsGepzero(_val));
}
void ConsGepzero::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("Gepzero");
  archive(CEREAL_NVP(gepzero));
}

TyGepzeroConst::TyGepzeroConst(std::shared_ptr<TyConstant> _vprime,
                               std::shared_ptr<TyConstantExpr> _gepinst)
    : vprime(_vprime), gepinst(_gepinst) {}
void TyGepzeroConst::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(vprime), CEREAL_NVP(gepinst));
}

ConsGepzeroConst::ConsGepzeroConst(
    std::shared_ptr<TyGepzeroConst> _gepzero_const)
    : gepzero_const(_gepzero_const) {}
std::shared_ptr<TyInfrule>
ConsGepzeroConst::make(std::shared_ptr<TyConstant> _vprime,
                       std::shared_ptr<TyConstantExpr> _gepinst) {
  std::shared_ptr<TyGepzeroConst> _val(new TyGepzeroConst(_vprime, _gepinst));
  return std::shared_ptr<TyInfrule>(new ConsGepzeroConst(_val));
}
void ConsGepzeroConst::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("GepzeroConst");
  archive(CEREAL_NVP(gepzero_const));
}

TyGepInboundsAdd::TyGepInboundsAdd(std::shared_ptr<TyValue> _v,
                                   std::shared_ptr<TyValue> _ptr,
                                   std::shared_ptr<TyValueType> _loadty,
                                   std::shared_ptr<TySize> _al,
                                   std::shared_ptr<TyExpr> _e)
    : v(_v), ptr(_ptr), loadty(_loadty), al(_al), e(_e) {}
void TyGepInboundsAdd::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(v), CEREAL_NVP(ptr), CEREAL_NVP(loadty), CEREAL_NVP(al),
          CEREAL_NVP(e));
}

ConsGepInboundsAdd::ConsGepInboundsAdd(
    std::shared_ptr<TyGepInboundsAdd> _gep_inbounds_add)
    : gep_inbounds_add(_gep_inbounds_add) {}
std::shared_ptr<TyInfrule> ConsGepInboundsAdd::make(
    std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _ptr,
    std::shared_ptr<TyValueType> _loadty, std::shared_ptr<TySize> _al,
    std::shared_ptr<TyExpr> _e) {
  std::shared_ptr<TyGepInboundsAdd> _val(
      new TyGepInboundsAdd(_v, _ptr, _loadty, _al, _e));
  return std::shared_ptr<TyInfrule>(new ConsGepInboundsAdd(_val));
}
void ConsGepInboundsAdd::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("GepInboundsAdd");
  archive(CEREAL_NVP(gep_inbounds_add));
}

TyGepInboundsRemove::TyGepInboundsRemove(std::shared_ptr<TyExpr> _gepinst)
    : gepinst(_gepinst) {}
void TyGepInboundsRemove::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(gepinst));
}

ConsGepInboundsRemove::ConsGepInboundsRemove(
    std::shared_ptr<TyGepInboundsRemove> _gep_inbounds_remove)
    : gep_inbounds_remove(_gep_inbounds_remove) {}
std::shared_ptr<TyInfrule>
ConsGepInboundsRemove::make(std::shared_ptr<TyExpr> _gepinst) {
  std::shared_ptr<TyGepInboundsRemove> _val(new TyGepInboundsRemove(_gepinst));
  return std::shared_ptr<TyInfrule>(new ConsGepInboundsRemove(_val));
}
void
ConsGepInboundsRemove::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("GepInboundsRemove");
  archive(CEREAL_NVP(gep_inbounds_remove));
}

TySubSub::TySubSub(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x,
                   std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _w,
                   std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)), w(std::move(_w)),
      sz(std::move(_sz)) {}
void TySubSub::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(w));
  archive(CEREAL_NVP(sz));
}

ConsSubSub::ConsSubSub(std::shared_ptr<TySubSub> _sub_sub)
    : sub_sub(std::move(_sub_sub)) {}
std::shared_ptr<TyInfrule> ConsSubSub::make(std::shared_ptr<TyRegister> _z,
                                            std::shared_ptr<TyValue> _x,
                                            std::shared_ptr<TyValue> _y,
                                            std::shared_ptr<TyValue> _w,
                                            std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TySubSub> _val(new TySubSub(std::move(_z), std::move(_x),
                                              std::move(_y), std::move(_w),
                                              std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsSubSub(std::move(_val)));
}
void ConsSubSub::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SubSub");
  archive(CEREAL_NVP(sub_sub));
}

TyFaddCommutativeTgt::TyFaddCommutativeTgt(std::shared_ptr<TyRegister> _z,
                                           std::shared_ptr<TyValue> _x,
                                           std::shared_ptr<TyValue> _y,
                                           TyFloatType _fty)
    : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)), fty(_fty) {}
void TyFaddCommutativeTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(cereal::make_nvp("fty", toString(fty)));
}

ConsFaddCommutativeTgt::ConsFaddCommutativeTgt(
    std::shared_ptr<TyFaddCommutativeTgt> _fadd_commutative_tgt)
    : fadd_commutative_tgt(std::move(_fadd_commutative_tgt)) {}
std::shared_ptr<TyInfrule>
ConsFaddCommutativeTgt::make(std::shared_ptr<TyRegister> _z,
                             std::shared_ptr<TyValue> _x,
                             std::shared_ptr<TyValue> _y, TyFloatType _fty) {
  std::shared_ptr<TyFaddCommutativeTgt> _val(new TyFaddCommutativeTgt(
      std::move(_z), std::move(_x), std::move(_y), std::move(_fty)));
  return std::shared_ptr<TyInfrule>(
      new ConsFaddCommutativeTgt(std::move(_val)));
}
void
ConsFaddCommutativeTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("FaddCommutativeTgt");
  archive(CEREAL_NVP(fadd_commutative_tgt));
}

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

TyFpextBitcast::TyFpextBitcast(std::shared_ptr<TyValue> _src,
                               std::shared_ptr<TyValue> _mid,
                               std::shared_ptr<TyValue> _dst,
                               std::shared_ptr<TyValueType> _srcty,
                               std::shared_ptr<TyValueType> _midty,
                               std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TyFpextBitcast::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src));
  archive(CEREAL_NVP(mid));
  archive(CEREAL_NVP(dst));
  archive(CEREAL_NVP(srcty));
  archive(CEREAL_NVP(midty));
  archive(CEREAL_NVP(dstty));
}
ConsFpextBitcast::ConsFpextBitcast(
    std::shared_ptr<TyFpextBitcast> _fpext_bitcast)
    : fpext_bitcast(_fpext_bitcast) {}
std::shared_ptr<TyInfrule> ConsFpextBitcast::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TyFpextBitcast> _val(
      new TyFpextBitcast(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsFpextBitcast(_val));
}
void ConsFpextBitcast::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("FpextBitcast");
  archive(CEREAL_NVP(fpext_bitcast));
}

TyFpextFpext::TyFpextFpext(std::shared_ptr<TyValue> _src,
                           std::shared_ptr<TyValue> _mid,
                           std::shared_ptr<TyValue> _dst,
                           std::shared_ptr<TyValueType> _srcty,
                           std::shared_ptr<TyValueType> _midty,
                           std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TyFpextFpext::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src));
  archive(CEREAL_NVP(mid));
  archive(CEREAL_NVP(dst));
  archive(CEREAL_NVP(srcty));
  archive(CEREAL_NVP(midty));
  archive(CEREAL_NVP(dstty));
}
ConsFpextFpext::ConsFpextFpext(std::shared_ptr<TyFpextFpext> _fpext_fpext)
    : fpext_fpext(_fpext_fpext) {}
std::shared_ptr<TyInfrule> ConsFpextFpext::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TyFpextFpext> _val(
      new TyFpextFpext(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsFpextFpext(_val));
}
void ConsFpextFpext::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("FpextFpext");
  archive(CEREAL_NVP(fpext_fpext));
}

TyFptosiBitcast::TyFptosiBitcast(std::shared_ptr<TyValue> _src,
                                 std::shared_ptr<TyValue> _mid,
                                 std::shared_ptr<TyValue> _dst,
                                 std::shared_ptr<TyValueType> _srcty,
                                 std::shared_ptr<TyValueType> _midty,
                                 std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TyFptosiBitcast::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src));
  archive(CEREAL_NVP(mid));
  archive(CEREAL_NVP(dst));
  archive(CEREAL_NVP(srcty));
  archive(CEREAL_NVP(midty));
  archive(CEREAL_NVP(dstty));
}
ConsFptosiBitcast::ConsFptosiBitcast(
    std::shared_ptr<TyFptosiBitcast> _fptosi_bitcast)
    : fptosi_bitcast(_fptosi_bitcast) {}
std::shared_ptr<TyInfrule> ConsFptosiBitcast::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TyFptosiBitcast> _val(
      new TyFptosiBitcast(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsFptosiBitcast(_val));
}
void ConsFptosiBitcast::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("FptosiBitcast");
  archive(CEREAL_NVP(fptosi_bitcast));
}

TyFptosiFpext::TyFptosiFpext(std::shared_ptr<TyValue> _src,
                             std::shared_ptr<TyValue> _mid,
                             std::shared_ptr<TyValue> _dst,
                             std::shared_ptr<TyValueType> _srcty,
                             std::shared_ptr<TyValueType> _midty,
                             std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TyFptosiFpext::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src));
  archive(CEREAL_NVP(mid));
  archive(CEREAL_NVP(dst));
  archive(CEREAL_NVP(srcty));
  archive(CEREAL_NVP(midty));
  archive(CEREAL_NVP(dstty));
}
ConsFptosiFpext::ConsFptosiFpext(std::shared_ptr<TyFptosiFpext> _fpext_fptosi)
    : fpext_fptosi(_fpext_fptosi) {}
std::shared_ptr<TyInfrule> ConsFptosiFpext::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TyFptosiFpext> _val(
      new TyFptosiFpext(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsFptosiFpext(_val));
}
void ConsFptosiFpext::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("FptosiFpext");
  archive(CEREAL_NVP(fpext_fptosi));
}

TyFptouiBitcast::TyFptouiBitcast(std::shared_ptr<TyValue> _src,
                                 std::shared_ptr<TyValue> _mid,
                                 std::shared_ptr<TyValue> _dst,
                                 std::shared_ptr<TyValueType> _srcty,
                                 std::shared_ptr<TyValueType> _midty,
                                 std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TyFptouiBitcast::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src));
  archive(CEREAL_NVP(mid));
  archive(CEREAL_NVP(dst));
  archive(CEREAL_NVP(srcty));
  archive(CEREAL_NVP(midty));
  archive(CEREAL_NVP(dstty));
}
ConsFptouiBitcast::ConsFptouiBitcast(
    std::shared_ptr<TyFptouiBitcast> _fptoui_bitcast)
    : fptoui_bitcast(_fptoui_bitcast) {}
std::shared_ptr<TyInfrule> ConsFptouiBitcast::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TyFptouiBitcast> _val(
      new TyFptouiBitcast(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsFptouiBitcast(_val));
}
void ConsFptouiBitcast::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("FptouiBitcast");
  archive(CEREAL_NVP(fptoui_bitcast));
}

TyFptouiFpext::TyFptouiFpext(std::shared_ptr<TyValue> _src,
                             std::shared_ptr<TyValue> _mid,
                             std::shared_ptr<TyValue> _dst,
                             std::shared_ptr<TyValueType> _srcty,
                             std::shared_ptr<TyValueType> _midty,
                             std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TyFptouiFpext::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src));
  archive(CEREAL_NVP(mid));
  archive(CEREAL_NVP(dst));
  archive(CEREAL_NVP(srcty));
  archive(CEREAL_NVP(midty));
  archive(CEREAL_NVP(dstty));
}
ConsFptouiFpext::ConsFptouiFpext(std::shared_ptr<TyFptouiFpext> _fpext_fptoui)
    : fpext_fptoui(_fpext_fptoui) {}
std::shared_ptr<TyInfrule> ConsFptouiFpext::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TyFptouiFpext> _val(
      new TyFptouiFpext(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsFptouiFpext(_val));
}
void ConsFptouiFpext::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("FptouiFpext");
  archive(CEREAL_NVP(fpext_fptoui));
}

TyPtrtointBitcast::TyPtrtointBitcast(std::shared_ptr<TyValue> _src,
                                     std::shared_ptr<TyValue> _mid,
                                     std::shared_ptr<TyValue> _dst,
                                     std::shared_ptr<TyValueType> _srcty,
                                     std::shared_ptr<TyValueType> _midty,
                                     std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TyPtrtointBitcast::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src));
  archive(CEREAL_NVP(mid));
  archive(CEREAL_NVP(dst));
  archive(CEREAL_NVP(srcty));
  archive(CEREAL_NVP(midty));
  archive(CEREAL_NVP(dstty));
}
ConsPtrtointBitcast::ConsPtrtointBitcast(
    std::shared_ptr<TyPtrtointBitcast> _inttoptr_bitcast)
    : inttoptr_bitcast(_inttoptr_bitcast) {}
std::shared_ptr<TyInfrule> ConsPtrtointBitcast::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TyPtrtointBitcast> _val(
      new TyPtrtointBitcast(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsPtrtointBitcast(_val));
}
void ConsPtrtointBitcast::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("PtrtointBitcast");
  archive(CEREAL_NVP(inttoptr_bitcast));
}

TyPtrtointLoad::TyPtrtointLoad(std::shared_ptr<TyValue> _ptr,
                               std::shared_ptr<TyValueType> _ptrty,
                               std::shared_ptr<TyValue> _v1,
                               std::shared_ptr<TyValueType> _intty,
                               std::shared_ptr<TyValue> _v2,
                               std::shared_ptr<TySize> _a)
    : ptr(_ptr), ptrty(_ptrty), v1(_v1), intty(_intty), v2(_v2), a(_a) {}
void TyPtrtointLoad::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(ptr), CEREAL_NVP(ptrty), CEREAL_NVP(v1), CEREAL_NVP(intty),
          CEREAL_NVP(v2), CEREAL_NVP(a));
}

ConsPtrtointLoad::ConsPtrtointLoad(
    std::shared_ptr<TyPtrtointLoad> _ptrtoint_load)
    : ptrtoint_load(_ptrtoint_load) {}
std::shared_ptr<TyInfrule> ConsPtrtointLoad::make(
    std::shared_ptr<TyValue> _ptr, std::shared_ptr<TyValueType> _ptrty,
    std::shared_ptr<TyValue> _v1, std::shared_ptr<TyValueType> _intty,
    std::shared_ptr<TyValue> _v2, std::shared_ptr<TySize> _a) {
  std::shared_ptr<TyPtrtointLoad> _val(
      new TyPtrtointLoad(_ptr, _ptrty, _v1, _intty, _v2, _a));
  return std::shared_ptr<TyInfrule>(new ConsPtrtointLoad(_val));
}
void ConsPtrtointLoad::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("PtrtointLoad");
  archive(CEREAL_NVP(ptrtoint_load));
}

TyLessthanUndef::TyLessthanUndef(std::shared_ptr<TyValueType> _ty,
                                 std::shared_ptr<TyValue> _v)
    : ty(std::move(_ty)), v(std::move(_v)) {}
void TyLessthanUndef::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(ty));
  archive(CEREAL_NVP(v));
}

void TyLessthanUndef::updateRhs(std::shared_ptr<TyValue> newVal) {
  v = newVal;
}

ConsLessthanUndef::ConsLessthanUndef(
    std::shared_ptr<TyLessthanUndef> _lessthan_undef)
    : lessthan_undef(std::move(_lessthan_undef)) {}
std::shared_ptr<TyInfrule>
ConsLessthanUndef::make(std::shared_ptr<TyValueType> _ty,
                        std::shared_ptr<TyValue> _v) {
  std::shared_ptr<TyLessthanUndef> _val(
      new TyLessthanUndef(std::move(_ty), std::move(_v)));
  return std::shared_ptr<TyInfrule>(new ConsLessthanUndef(std::move(_val)));
}
void ConsLessthanUndef::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("LessthanUndef");
  archive(CEREAL_NVP(lessthan_undef));
}

TyLessthanUndefTgt::TyLessthanUndefTgt(std::shared_ptr<TyValueType> _ty,
                                       std::shared_ptr<TyValue> _v)
    : ty(std::move(_ty)), v(std::move(_v)) {}
void TyLessthanUndefTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(ty));
  archive(CEREAL_NVP(v));
}

ConsLessthanUndefTgt::ConsLessthanUndefTgt(
    std::shared_ptr<TyLessthanUndefTgt> _lessthan_undef_tgt)
    : lessthan_undef_tgt(std::move(_lessthan_undef_tgt)) {}
std::shared_ptr<TyInfrule>
ConsLessthanUndefTgt::make(std::shared_ptr<TyValueType> _ty,
                           std::shared_ptr<TyValue> _v) {
  std::shared_ptr<TyLessthanUndefTgt> _val(
      new TyLessthanUndefTgt(std::move(_ty), std::move(_v)));
  return std::shared_ptr<TyInfrule>(new ConsLessthanUndefTgt(std::move(_val)));
}
void ConsLessthanUndefTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("LessthanUndefTgt");
  archive(CEREAL_NVP(lessthan_undef_tgt));
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

TyNegVal::TyNegVal(std::shared_ptr<TyConstInt> _c1,
                   std::shared_ptr<TyConstInt> _c2, std::shared_ptr<TySize> _sz)
    : c1(std::move(_c1)), c2(std::move(_c2)), sz(std::move(_sz)) {}

void TyNegVal::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(c1), CEREAL_NVP(c2), CEREAL_NVP(sz));
}

ConsNegVal::ConsNegVal(std::shared_ptr<TyNegVal> _neg_val)
    : neg_val(std::move(_neg_val)) {}

void ConsNegVal::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("NegVal");
  archive(CEREAL_NVP(neg_val));
}

std::shared_ptr<TyInfrule> ConsNegVal::make(std::shared_ptr<TyConstInt> _c1,
                                            std::shared_ptr<TyConstInt> _c2,
                                            std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyNegVal> _neg_val(
      new TyNegVal(std::move(_c1), std::move(_c2), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsNegVal(std::move(_neg_val)));
}

TyOrAnd::TyOrAnd(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _y,
                 std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _a,
                 std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), y(std::move(_y)), x(std::move(_x)), a(std::move(_a)),
      sz(std::move(_sz)) {}
void TyOrAnd::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(sz));
}

ConsOrAnd::ConsOrAnd(std::shared_ptr<TyOrAnd> _or_and)
    : or_and(std::move(_or_and)) {}
std::shared_ptr<TyInfrule> ConsOrAnd::make(std::shared_ptr<TyValue> _z,
                                           std::shared_ptr<TyValue> _y,
                                           std::shared_ptr<TyValue> _x,
                                           std::shared_ptr<TyValue> _a,
                                           std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyOrAnd> _val(new TyOrAnd(std::move(_z), std::move(_y),
                                            std::move(_x), std::move(_a),
                                            std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsOrAnd(std::move(_val)));
}
void ConsOrAnd::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("OrAnd");
  archive(CEREAL_NVP(or_and));
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

TyOrCommutativeTgt::TyOrCommutativeTgt(std::shared_ptr<TyRegister> _z,
                                       std::shared_ptr<TyValue> _x,
                                       std::shared_ptr<TyValue> _y,
                                       std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)), sz(std::move(_sz)) {
}
void TyOrCommutativeTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(sz));
}

ConsOrCommutativeTgt::ConsOrCommutativeTgt(
    std::shared_ptr<TyOrCommutativeTgt> _or_commutative_tgt)
    : or_commutative_tgt(std::move(_or_commutative_tgt)) {}
std::shared_ptr<TyInfrule> ConsOrCommutativeTgt::make(
    std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x,
    std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyOrCommutativeTgt> _val(new TyOrCommutativeTgt(
      std::move(_z), std::move(_x), std::move(_y), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsOrCommutativeTgt(std::move(_val)));
}
void ConsOrCommutativeTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("OrCommutativeTgt");
  archive(CEREAL_NVP(or_commutative_tgt));
}

TyOrFalse::TyOrFalse(std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
                     std::shared_ptr<TySize> _sz)
    : x(std::move(_x)), y(std::move(_y)), sz(std::move(_sz)) {}

void TyOrFalse::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(sz));
}

ConsOrFalse::ConsOrFalse(std::shared_ptr<TyOrFalse> _or_false)
    : or_false(std::move(_or_false)) {}

std::shared_ptr<TyInfrule> ConsOrFalse::make(std::shared_ptr<TyValue> _x,
                                             std::shared_ptr<TyValue> _y,
                                             std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyOrFalse> _val(
      new TyOrFalse(std::move(_x), std::move(_y), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsOrFalse(std::move(_val)));
}

void ConsOrFalse::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("OrFalse");
  archive(CEREAL_NVP(or_false));
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

TyOrMone::TyOrMone(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _a,
                   std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), a(std::move(_a)), sz(std::move(_sz)) {}
void TyOrMone::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(sz));
}

ConsOrMone::ConsOrMone(std::shared_ptr<TyOrMone> _or_mone)
    : or_mone(std::move(_or_mone)) {}
std::shared_ptr<TyInfrule> ConsOrMone::make(std::shared_ptr<TyValue> _z,
                                            std::shared_ptr<TyValue> _a,
                                            std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyOrMone> _val(
      new TyOrMone(std::move(_z), std::move(_a), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsOrMone(std::move(_val)));
}
void ConsOrMone::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("OrMone");
  archive(CEREAL_NVP(or_mone));
}

TyOrNot::TyOrNot(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _y,
                 std::shared_ptr<TyValue> _x, std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), y(std::move(_y)), x(std::move(_x)), sz(std::move(_sz)) {
}
void TyOrNot::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(sz));
}

ConsOrNot::ConsOrNot(std::shared_ptr<TyOrNot> _or_not)
    : or_not(std::move(_or_not)) {}
std::shared_ptr<TyInfrule> ConsOrNot::make(std::shared_ptr<TyValue> _z,
                                           std::shared_ptr<TyValue> _y,
                                           std::shared_ptr<TyValue> _x,
                                           std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyOrNot> _val(
      new TyOrNot(std::move(_z), std::move(_y), std::move(_x), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsOrNot(std::move(_val)));
}
void ConsOrNot::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("OrNot");
  archive(CEREAL_NVP(or_not));
}

TyOrUndef::TyOrUndef(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _a,
                     std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), a(std::move(_a)), sz(std::move(_sz)) {}
void TyOrUndef::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(sz));
}

ConsOrUndef::ConsOrUndef(std::shared_ptr<TyOrUndef> _or_undef)
    : or_undef(std::move(_or_undef)) {}
std::shared_ptr<TyInfrule> ConsOrUndef::make(std::shared_ptr<TyValue> _z,
                                             std::shared_ptr<TyValue> _a,
                                             std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyOrUndef> _val(
      new TyOrUndef(std::move(_z), std::move(_a), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsOrUndef(std::move(_val)));
}
void ConsOrUndef::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("OrUndef");
  archive(CEREAL_NVP(or_undef));
}

TyOrSame::TyOrSame(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _a,
                   std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), a(std::move(_a)), sz(std::move(_sz)) {}
void TyOrSame::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(sz));
}

ConsOrSame::ConsOrSame(std::shared_ptr<TyOrSame> _or_same)
    : or_same(std::move(_or_same)) {}
std::shared_ptr<TyInfrule> ConsOrSame::make(std::shared_ptr<TyValue> _z,
                                            std::shared_ptr<TyValue> _a,
                                            std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyOrSame> _val(
      new TyOrSame(std::move(_z), std::move(_a), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsOrSame(std::move(_val)));
}
void ConsOrSame::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("OrSame");
  archive(CEREAL_NVP(or_same));
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

TyOrZero::TyOrZero(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _a,
                   std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), a(std::move(_a)), sz(std::move(_sz)) {}
void TyOrZero::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(sz));
}

ConsOrZero::ConsOrZero(std::shared_ptr<TyOrZero> _or_zero)
    : or_zero(std::move(_or_zero)) {}
std::shared_ptr<TyInfrule> ConsOrZero::make(std::shared_ptr<TyValue> _z,
                                            std::shared_ptr<TyValue> _a,
                                            std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyOrZero> _val(
      new TyOrZero(std::move(_z), std::move(_a), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsOrZero(std::move(_val)));
}
void ConsOrZero::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("OrZero");
  archive(CEREAL_NVP(or_zero));
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

TyTruncBitcast::TyTruncBitcast(std::shared_ptr<TyValue> _src,
                               std::shared_ptr<TyValue> _mid,
                               std::shared_ptr<TyValue> _dst,
                               std::shared_ptr<TyValueType> _srcty,
                               std::shared_ptr<TyValueType> _midty,
                               std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TyTruncBitcast::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src));
  archive(CEREAL_NVP(mid));
  archive(CEREAL_NVP(dst));
  archive(CEREAL_NVP(srcty));
  archive(CEREAL_NVP(midty));
  archive(CEREAL_NVP(dstty));
}
ConsTruncBitcast::ConsTruncBitcast(
    std::shared_ptr<TyTruncBitcast> _trunc_bitcast)
    : trunc_bitcast(_trunc_bitcast) {}
std::shared_ptr<TyInfrule> ConsTruncBitcast::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TyTruncBitcast> _val(
      new TyTruncBitcast(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsTruncBitcast(_val));
}
void ConsTruncBitcast::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("TruncBitcast");
  archive(CEREAL_NVP(trunc_bitcast));
}

TyTruncOnebit::TyTruncOnebit(std::shared_ptr<TyValue> _z,
                             std::shared_ptr<TyValue> _x,
                             std::shared_ptr<TyValue> _y,
                             std::shared_ptr<TySize> _orgsz)
    : z(_z), x(_x), y(_y), orgsz(_orgsz) {}
void TyTruncOnebit::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(x), CEREAL_NVP(y), CEREAL_NVP(orgsz));
}

ConsTruncOnebit::ConsTruncOnebit(std::shared_ptr<TyTruncOnebit> _trunc_onebit)
    : trunc_onebit(_trunc_onebit) {}
std::shared_ptr<TyInfrule>
ConsTruncOnebit::make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
                      std::shared_ptr<TyValue> _y,
                      std::shared_ptr<TySize> _orgsz) {
  std::shared_ptr<TyTruncOnebit> _val(new TyTruncOnebit(_z, _x, _y, _orgsz));
  return std::shared_ptr<TyInfrule>(new ConsTruncOnebit(_val));
}
void ConsTruncOnebit::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("TruncOnebit");
  archive(CEREAL_NVP(trunc_onebit));
}

TyTruncPtrtoint::TyTruncPtrtoint(std::shared_ptr<TyValue> _src,
                                 std::shared_ptr<TyValue> _mid,
                                 std::shared_ptr<TyValue> _dst,
                                 std::shared_ptr<TyValueType> _srcty,
                                 std::shared_ptr<TyValueType> _midty,
                                 std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TyTruncPtrtoint::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src));
  archive(CEREAL_NVP(mid));
  archive(CEREAL_NVP(dst));
  archive(CEREAL_NVP(srcty));
  archive(CEREAL_NVP(midty));
  archive(CEREAL_NVP(dstty));
}
ConsTruncPtrtoint::ConsTruncPtrtoint(
    std::shared_ptr<TyTruncPtrtoint> _ptrtoint_trunc)
    : ptrtoint_trunc(_ptrtoint_trunc) {}
std::shared_ptr<TyInfrule> ConsTruncPtrtoint::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TyTruncPtrtoint> _val(
      new TyTruncPtrtoint(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsTruncPtrtoint(_val));
}
void ConsTruncPtrtoint::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("TruncPtrtoint");
  archive(CEREAL_NVP(ptrtoint_trunc));
}
ConsSextBitcast::ConsSextBitcast(std::shared_ptr<TySextBitcast> _sext_bitcast)
    : sext_bitcast(_sext_bitcast) {}
std::shared_ptr<TyInfrule> ConsSextBitcast::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TySextBitcast> _val(
      new TySextBitcast(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsSextBitcast(_val));
}
void ConsSextBitcast::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SextBitcast");
  archive(CEREAL_NVP(sext_bitcast));
}

TySextSext::TySextSext(std::shared_ptr<TyValue> _src,
                       std::shared_ptr<TyValue> _mid,
                       std::shared_ptr<TyValue> _dst,
                       std::shared_ptr<TyValueType> _srcty,
                       std::shared_ptr<TyValueType> _midty,
                       std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TySextSext::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src));
  archive(CEREAL_NVP(mid));
  archive(CEREAL_NVP(dst));
  archive(CEREAL_NVP(srcty));
  archive(CEREAL_NVP(midty));
  archive(CEREAL_NVP(dstty));
}
ConsSextSext::ConsSextSext(std::shared_ptr<TySextSext> _sext_sext)
    : sext_sext(_sext_sext) {}
std::shared_ptr<TyInfrule> ConsSextSext::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TySextSext> _val(
      new TySextSext(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsSextSext(_val));
}
void ConsSextSext::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SextSext");
  archive(CEREAL_NVP(sext_sext));
}

TyShiftUndef1::TyShiftUndef1(std::shared_ptr<TyValue> _z,
                             std::shared_ptr<TyValue> _y,
                             std::shared_ptr<TySize> _s)
    : z(_z), y(_y), s(_s) {}
void TyShiftUndef1::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(y), CEREAL_NVP(s));
}

ConsShiftUndef1::ConsShiftUndef1(std::shared_ptr<TyShiftUndef1> _shift_undef1)
    : shift_undef1(_shift_undef1) {}
std::shared_ptr<TyInfrule> ConsShiftUndef1::make(std::shared_ptr<TyValue> _z,
                                                 std::shared_ptr<TyValue> _y,
                                                 std::shared_ptr<TySize> _s) {
  std::shared_ptr<TyShiftUndef1> _val(new TyShiftUndef1(_z, _y, _s));
  return std::shared_ptr<TyInfrule>(new ConsShiftUndef1(_val));
}
void ConsShiftUndef1::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("ShiftUndef1");
  archive(CEREAL_NVP(shift_undef1));
}

TyShiftUndef2::TyShiftUndef2(std::shared_ptr<TyValue> _z,
                             std::shared_ptr<TyValue> _y,
                             std::shared_ptr<TyConstInt> _c,
                             std::shared_ptr<TySize> _s)
    : z(_z), y(_y), c(_c), s(_s) {}
void TyShiftUndef2::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(y), CEREAL_NVP(c), CEREAL_NVP(s));
}

ConsShiftUndef2::ConsShiftUndef2(std::shared_ptr<TyShiftUndef2> _shift_undef2)
    : shift_undef2(_shift_undef2) {}
std::shared_ptr<TyInfrule> ConsShiftUndef2::make(std::shared_ptr<TyValue> _z,
                                                 std::shared_ptr<TyValue> _y,
                                                 std::shared_ptr<TyConstInt> _c,
                                                 std::shared_ptr<TySize> _s) {
  std::shared_ptr<TyShiftUndef2> _val(new TyShiftUndef2(_z, _y, _c, _s));
  return std::shared_ptr<TyInfrule>(new ConsShiftUndef2(_val));
}
void ConsShiftUndef2::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("ShiftUndef2");
  archive(CEREAL_NVP(shift_undef2));
}

TyShiftZero1::TyShiftZero1(std::shared_ptr<TyValue> _z,
                           std::shared_ptr<TyValue> _y,
                           std::shared_ptr<TySize> _s)
    : z(_z), y(_y), s(_s) {}
void TyShiftZero1::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(y), CEREAL_NVP(s));
}

ConsShiftZero1::ConsShiftZero1(std::shared_ptr<TyShiftZero1> _shift_zero1)
    : shift_zero1(_shift_zero1) {}
std::shared_ptr<TyInfrule> ConsShiftZero1::make(std::shared_ptr<TyValue> _z,
                                                std::shared_ptr<TyValue> _y,
                                                std::shared_ptr<TySize> _s) {
  std::shared_ptr<TyShiftZero1> _val(new TyShiftZero1(_z, _y, _s));
  return std::shared_ptr<TyInfrule>(new ConsShiftZero1(_val));
}
void ConsShiftZero1::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("ShiftZero1");
  archive(CEREAL_NVP(shift_zero1));
}

TyShiftZero2::TyShiftZero2(std::shared_ptr<TyValue> _z,
                           std::shared_ptr<TyValue> _y,
                           std::shared_ptr<TySize> _s)
    : z(_z), y(_y), s(_s) {}
void TyShiftZero2::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(y), CEREAL_NVP(s));
}

ConsShiftZero2::ConsShiftZero2(std::shared_ptr<TyShiftZero2> _shift_zero2)
    : shift_zero2(_shift_zero2) {}
std::shared_ptr<TyInfrule> ConsShiftZero2::make(std::shared_ptr<TyValue> _z,
                                                std::shared_ptr<TyValue> _y,
                                                std::shared_ptr<TySize> _s) {
  std::shared_ptr<TyShiftZero2> _val(new TyShiftZero2(_z, _y, _s));
  return std::shared_ptr<TyInfrule>(new ConsShiftZero2(_val));
}
void ConsShiftZero2::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("ShiftZero2");
  archive(CEREAL_NVP(shift_zero2));
}

TySitofpBitcast::TySitofpBitcast(std::shared_ptr<TyValue> _src,
                                 std::shared_ptr<TyValue> _mid,
                                 std::shared_ptr<TyValue> _dst,
                                 std::shared_ptr<TyValueType> _srcty,
                                 std::shared_ptr<TyValueType> _midty,
                                 std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TySitofpBitcast::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src));
  archive(CEREAL_NVP(mid));
  archive(CEREAL_NVP(dst));
  archive(CEREAL_NVP(srcty));
  archive(CEREAL_NVP(midty));
  archive(CEREAL_NVP(dstty));
}
ConsSitofpBitcast::ConsSitofpBitcast(
    std::shared_ptr<TySitofpBitcast> _sitofp_bitcast)
    : sitofp_bitcast(_sitofp_bitcast) {}
std::shared_ptr<TyInfrule> ConsSitofpBitcast::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TySitofpBitcast> _val(
      new TySitofpBitcast(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsSitofpBitcast(_val));
}
void ConsSitofpBitcast::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SitofpBitcast");
  archive(CEREAL_NVP(sitofp_bitcast));
}

TySitofpSext::TySitofpSext(std::shared_ptr<TyValue> _src,
                           std::shared_ptr<TyValue> _mid,
                           std::shared_ptr<TyValue> _dst,
                           std::shared_ptr<TyValueType> _srcty,
                           std::shared_ptr<TyValueType> _midty,
                           std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TySitofpSext::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src));
  archive(CEREAL_NVP(mid));
  archive(CEREAL_NVP(dst));
  archive(CEREAL_NVP(srcty));
  archive(CEREAL_NVP(midty));
  archive(CEREAL_NVP(dstty));
}
ConsSitofpSext::ConsSitofpSext(std::shared_ptr<TySitofpSext> _sext_sitofp)
    : sext_sitofp(_sext_sitofp) {}
std::shared_ptr<TyInfrule> ConsSitofpSext::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TySitofpSext> _val(
      new TySitofpSext(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsSitofpSext(_val));
}
void ConsSitofpSext::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SitofpSext");
  archive(CEREAL_NVP(sext_sitofp));
}

TySitofpZext::TySitofpZext(std::shared_ptr<TyValue> _src,
                           std::shared_ptr<TyValue> _mid,
                           std::shared_ptr<TyValue> _dst,
                           std::shared_ptr<TyValueType> _srcty,
                           std::shared_ptr<TyValueType> _midty,
                           std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TySitofpZext::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src), CEREAL_NVP(mid), CEREAL_NVP(dst), CEREAL_NVP(srcty),
          CEREAL_NVP(midty), CEREAL_NVP(dstty));
}

ConsSitofpZext::ConsSitofpZext(std::shared_ptr<TySitofpZext> _sitofp_zext)
    : sitofp_zext(_sitofp_zext) {}
std::shared_ptr<TyInfrule> ConsSitofpZext::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TySitofpZext> _val(
      new TySitofpZext(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsSitofpZext(_val));
}
void ConsSitofpZext::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SitofpZext");
  archive(CEREAL_NVP(sitofp_zext));
}

TySubRemove::TySubRemove(std::shared_ptr<TyRegister> _z,
                         std::shared_ptr<TyRegister> _y,
                         std::shared_ptr<TyValue> _a,
                         std::shared_ptr<TyValue> _b,
                         std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), y(std::move(_y)), a(std::move(_a)), b(std::move(_b)),
      sz(std::move(_sz)) {}

void TySubRemove::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(y), CEREAL_NVP(a), CEREAL_NVP(b),
          CEREAL_NVP(sz));
}

ConsSubRemove::ConsSubRemove(std::shared_ptr<TySubRemove> _sub_remove)
    : sub_remove(std::move(_sub_remove)) {}

void ConsSubRemove::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("SubRemove");
  archive(CEREAL_NVP(sub_remove));
}

std::shared_ptr<TyInfrule> ConsSubRemove::make(std::shared_ptr<TyRegister> _z,
                                               std::shared_ptr<TyRegister> _y,
                                               std::shared_ptr<TyValue> _a,
                                               std::shared_ptr<TyValue> _b,
                                               std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TySubRemove> _val(
      new TySubRemove(std::move(_z), std::move(_y), std::move(_a),
                      std::move(_b), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsSubRemove(std::move(_val)));
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

TyTransitivity::TyTransitivity(std::shared_ptr<TyExpr> _e1,
                               std::shared_ptr<TyExpr> _e2,
                               std::shared_ptr<TyExpr> _e3)
    : e1(std::move(_e1)), e2(std::move(_e2)), e3(std::move(_e3)) {}

void TyTransitivity::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(e1), CEREAL_NVP(e2), CEREAL_NVP(e3));
}

std::shared_ptr<TyExpr> TyTransitivity::getExpr1() {
  return e1;
}

std::shared_ptr<TyExpr> TyTransitivity::getExpr2() {
  return e2;
}

std::shared_ptr<TyExpr> TyTransitivity::getExpr3() {
  return e3;
}

void TyTransitivity::updateExpr1(std::shared_ptr<TyExpr> newExpr) {
  e1 = newExpr;
}

void TyTransitivity::updateExpr2(std::shared_ptr<TyExpr> newExpr) {
  e2 = newExpr;
}

void TyTransitivity::updateExpr3(std::shared_ptr<TyExpr> newExpr) {
  e3 = newExpr;
}

ConsTransitivity::ConsTransitivity(
    std::shared_ptr<TyTransitivity> _transitivity)
    : transitivity(std::move(_transitivity)) {}

void ConsTransitivity::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("Transitivity");
  archive(CEREAL_NVP(transitivity));
}

std::shared_ptr<TyInfrule> ConsTransitivity::make(std::shared_ptr<TyExpr> _e1,
                                                  std::shared_ptr<TyExpr> _e2,
                                                  std::shared_ptr<TyExpr> _e3) {
  std::shared_ptr<TyTransitivity> _transitivity(
      new TyTransitivity(std::move(_e1), std::move(_e2), std::move(_e3)));
  return std::shared_ptr<TyInfrule>(
      new ConsTransitivity(std::move(_transitivity)));
}

TyTransitivityPointerLhs::TyTransitivityPointerLhs(
    std::shared_ptr<TyValue> _p, std::shared_ptr<TyValue> _q,
    std::shared_ptr<TyValue> _v, std::shared_ptr<TyValueType> _typ,
    std::shared_ptr<TySize> _align)
    : p(_p), q(_q), v(_v), typ(_typ), align(_align) {}
void
TyTransitivityPointerLhs::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(p), CEREAL_NVP(q), CEREAL_NVP(v), CEREAL_NVP(typ),
          CEREAL_NVP(align));
}
ConsTransitivityPointerLhs::ConsTransitivityPointerLhs(
    std::shared_ptr<TyTransitivityPointerLhs> _transitivity_pointer_lhs)
    : transitivity_pointer_lhs(_transitivity_pointer_lhs) {}
std::shared_ptr<TyInfrule> ConsTransitivityPointerLhs::make(
    std::shared_ptr<TyValue> _p, std::shared_ptr<TyValue> _q,
    std::shared_ptr<TyValue> _v, std::shared_ptr<TyValueType> _typ,
    std::shared_ptr<TySize> _align) {
  std::shared_ptr<TyTransitivityPointerLhs> _val(
      new TyTransitivityPointerLhs(_p, _q, _v, _typ, _align));
  return std::shared_ptr<TyInfrule>(new ConsTransitivityPointerLhs(_val));
}
void ConsTransitivityPointerLhs::serialize(
    cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("TransitivityPointerLhs");
  archive(CEREAL_NVP(transitivity_pointer_lhs));
}

TyTransitivityPointerRhs::TyTransitivityPointerRhs(
    std::shared_ptr<TyValue> _p, std::shared_ptr<TyValue> _q,
    std::shared_ptr<TyValue> _v, std::shared_ptr<TyValueType> _typ,
    std::shared_ptr<TySize> _align)
    : p(_p), q(_q), v(_v), typ(_typ), align(_align) {}
void
TyTransitivityPointerRhs::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(p), CEREAL_NVP(q), CEREAL_NVP(v), CEREAL_NVP(typ),
          CEREAL_NVP(align));
}
ConsTransitivityPointerRhs::ConsTransitivityPointerRhs(
    std::shared_ptr<TyTransitivityPointerRhs> _transitivity_pointer_rhs)
    : transitivity_pointer_rhs(_transitivity_pointer_rhs) {}
std::shared_ptr<TyInfrule> ConsTransitivityPointerRhs::make(
    std::shared_ptr<TyValue> _p, std::shared_ptr<TyValue> _q,
    std::shared_ptr<TyValue> _v, std::shared_ptr<TyValueType> _typ,
    std::shared_ptr<TySize> _align) {
  std::shared_ptr<TyTransitivityPointerRhs> _val(
      new TyTransitivityPointerRhs(_p, _q, _v, _typ, _align));
  return std::shared_ptr<TyInfrule>(new ConsTransitivityPointerRhs(_val));
}
void ConsTransitivityPointerRhs::serialize(
    cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("TransitivityPointerRhs");
  archive(CEREAL_NVP(transitivity_pointer_rhs));
}

TyTransitivityTgt::TyTransitivityTgt(std::shared_ptr<TyExpr> _e1,
                                     std::shared_ptr<TyExpr> _e2,
                                     std::shared_ptr<TyExpr> _e3)
    : e1(std::move(_e1)), e2(std::move(_e2)), e3(std::move(_e3)) {}

void TyTransitivityTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(e1), CEREAL_NVP(e2), CEREAL_NVP(e3));
}

std::shared_ptr<TyExpr> TyTransitivityTgt::getExpr1() {
  return e2;
}

std::shared_ptr<TyExpr> TyTransitivityTgt::getExpr2() {
  return e2;
}

std::shared_ptr<TyExpr> TyTransitivityTgt::getExpr3() {
  return e3;
}

void TyTransitivityTgt::updateExpr1(std::shared_ptr<TyExpr> newExpr) {
  e1 = newExpr;
}

void TyTransitivityTgt::updateExpr2(std::shared_ptr<TyExpr> newExpr) {
  e2 = newExpr;
}

void TyTransitivityTgt::updateExpr3(std::shared_ptr<TyExpr> newExpr) {
  e3 = newExpr;
}

ConsTransitivityTgt::ConsTransitivityTgt(
    std::shared_ptr<TyTransitivityTgt> _transitivity_tgt)
    : transitivity_tgt(std::move(_transitivity_tgt)) {}

void ConsTransitivityTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("TransitivityTgt");
  archive(CEREAL_NVP(transitivity_tgt));
}

std::shared_ptr<TyInfrule>
ConsTransitivityTgt::make(std::shared_ptr<TyExpr> _e1,
                          std::shared_ptr<TyExpr> _e2,
                          std::shared_ptr<TyExpr> _e3) {
  std::shared_ptr<TyTransitivityTgt> _transitivity_tgt(
      new TyTransitivityTgt(std::move(_e1), std::move(_e2), std::move(_e3)));
  return std::shared_ptr<TyInfrule>(
      new ConsTransitivityTgt(std::move(_transitivity_tgt)));
}

TyTruncSext::TyTruncSext(std::shared_ptr<TyValue> _src,
                         std::shared_ptr<TyValue> _mid,
                         std::shared_ptr<TyValue> _dst,
                         std::shared_ptr<TyValueType> _srcty,
                         std::shared_ptr<TyValueType> _midty,
                         std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TyTruncSext::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src), CEREAL_NVP(mid), CEREAL_NVP(dst), CEREAL_NVP(srcty),
          CEREAL_NVP(midty), CEREAL_NVP(dstty));
}

ConsTruncSext::ConsTruncSext(std::shared_ptr<TyTruncSext> _trunc_sext)
    : trunc_sext(_trunc_sext) {}
std::shared_ptr<TyInfrule> ConsTruncSext::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TyTruncSext> _val(
      new TyTruncSext(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsTruncSext(_val));
}
void ConsTruncSext::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("TruncSext");
  archive(CEREAL_NVP(trunc_sext));
}

TyTruncTrunc::TyTruncTrunc(std::shared_ptr<TyValue> _src,
                           std::shared_ptr<TyValue> _mid,
                           std::shared_ptr<TyValue> _dst,
                           std::shared_ptr<TyValueType> _srcty,
                           std::shared_ptr<TyValueType> _midty,
                           std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TyTruncTrunc::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src), CEREAL_NVP(mid), CEREAL_NVP(dst), CEREAL_NVP(srcty),
          CEREAL_NVP(midty), CEREAL_NVP(dstty));
}
ConsTruncTrunc::ConsTruncTrunc(std::shared_ptr<TyTruncTrunc> _trunc_trunc)
    : trunc_trunc(_trunc_trunc) {}
std::shared_ptr<TyInfrule> ConsTruncTrunc::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TyTruncTrunc> _val(
      new TyTruncTrunc(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsTruncTrunc(_val));
}
void ConsTruncTrunc::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("TruncTrunc");
  archive(CEREAL_NVP(trunc_trunc));
}

TySubstitute::TySubstitute(std::shared_ptr<TyRegister> _x,
                           std::shared_ptr<TyValue> _y,
                           std::shared_ptr<TyExpr> _e)
    : x(_x), y(_y), e(_e) {}
void TySubstitute::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(e));
}

ConsSubstitute::ConsSubstitute(std::shared_ptr<TySubstitute> _substitute)
    : substitute(_substitute) {}
std::shared_ptr<TyInfrule> ConsSubstitute::make(std::shared_ptr<TyRegister> _x,
                                                std::shared_ptr<TyValue> _y,
                                                std::shared_ptr<TyExpr> _e) {
  std::shared_ptr<TySubstitute> _val(new TySubstitute(_x, _y, _e));
  return std::shared_ptr<TyInfrule>(new ConsSubstitute(_val));
}
void ConsSubstitute::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("Substitute");
  archive(CEREAL_NVP(substitute));
}

TySubstituteRev::TySubstituteRev(std::shared_ptr<TyRegister> _x,
                                 std::shared_ptr<TyValue> _y,
                                 std::shared_ptr<TyExpr> _e)
    : x(_x), y(_y), e(_e) {}
void TySubstituteRev::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(e));
}

ConsSubstituteRev::ConsSubstituteRev(
    std::shared_ptr<TySubstituteRev> _substitute_rev)
    : substitute_rev(_substitute_rev) {}
std::shared_ptr<TyInfrule>
ConsSubstituteRev::make(std::shared_ptr<TyRegister> _x,
                        std::shared_ptr<TyValue> _y,
                        std::shared_ptr<TyExpr> _e) {
  std::shared_ptr<TySubstituteRev> _val(new TySubstituteRev(_x, _y, _e));
  return std::shared_ptr<TyInfrule>(new ConsSubstituteRev(_val));
}
void ConsSubstituteRev::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SubstituteRev");
  archive(CEREAL_NVP(substitute_rev));
}

TySubstituteTgt::TySubstituteTgt(std::shared_ptr<TyRegister> _x,
                                 std::shared_ptr<TyValue> _y,
                                 std::shared_ptr<TyExpr> _e)
    : x(_x), y(_y), e(_e) {}
void TySubstituteTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(x), CEREAL_NVP(y), CEREAL_NVP(e));
}

ConsSubstituteTgt::ConsSubstituteTgt(
    std::shared_ptr<TySubstituteTgt> _substitute_tgt)
    : substitute_tgt(_substitute_tgt) {}
std::shared_ptr<TyInfrule>
ConsSubstituteTgt::make(std::shared_ptr<TyRegister> _x,
                        std::shared_ptr<TyValue> _y,
                        std::shared_ptr<TyExpr> _e) {
  std::shared_ptr<TySubstituteTgt> _val(new TySubstituteTgt(_x, _y, _e));
  return std::shared_ptr<TyInfrule>(new ConsSubstituteTgt(_val));
}

void ConsSubstituteTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SubstituteTgt");
  archive(CEREAL_NVP(substitute_tgt));
}

TyTruncZext::TyTruncZext(std::shared_ptr<TyValue> _src,
                         std::shared_ptr<TyValue> _mid,
                         std::shared_ptr<TyValue> _dst,
                         std::shared_ptr<TyValueType> _srcty,
                         std::shared_ptr<TyValueType> _midty,
                         std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TyTruncZext::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src), CEREAL_NVP(mid), CEREAL_NVP(dst), CEREAL_NVP(srcty),
          CEREAL_NVP(midty), CEREAL_NVP(dstty));
}

ConsTruncZext::ConsTruncZext(std::shared_ptr<TyTruncZext> _trunc_zext)
    : trunc_zext(_trunc_zext) {}
std::shared_ptr<TyInfrule> ConsTruncZext::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TyTruncZext> _val(
      new TyTruncZext(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsTruncZext(_val));
}
void ConsTruncZext::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("TruncZext");
  archive(CEREAL_NVP(trunc_zext));
}

TyReplaceRhs::TyReplaceRhs(std::shared_ptr<TyRegister> _x,
                           std::shared_ptr<TyValue> _y,
                           std::shared_ptr<TyExpr> _e1,
                           std::shared_ptr<TyExpr> _e2,
                           std::shared_ptr<TyExpr> _e2_p)
    : x(std::move(_x)), y(std::move(_y)), e1(std::move(_e1)),
      e2(std::move(_e2)), e2_p(std::move(_e2_p)) {}

void TyReplaceRhs::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(x), CEREAL_NVP(y), CEREAL_NVP(e1), CEREAL_NVP(e2),
          cereal::make_nvp("e2\'", e2_p));
}

ConsReplaceRhs::ConsReplaceRhs(std::shared_ptr<TyReplaceRhs> _replace_rhs)
    : replace_rhs(std::move(_replace_rhs)) {}

void ConsReplaceRhs::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("ReplaceRhs");
  archive(CEREAL_NVP(replace_rhs));
}

std::shared_ptr<TyInfrule> ConsReplaceRhs::make(std::shared_ptr<TyRegister> _x,
                                                std::shared_ptr<TyValue> _y,
                                                std::shared_ptr<TyExpr> _e1,
                                                std::shared_ptr<TyExpr> _e2,
                                                std::shared_ptr<TyExpr> _e2_p) {
  std::shared_ptr<TyReplaceRhs> _replace_rhs(
      new TyReplaceRhs(std::move(_x), std::move(_y), std::move(_e1),
                       std::move(_e2), std::move(_e2_p)));
  return std::shared_ptr<TyInfrule>(
      new ConsReplaceRhs(std::move(_replace_rhs)));
}

TyReplaceRhsOpt::TyReplaceRhsOpt(std::shared_ptr<TyRegister> _x,
                                 std::shared_ptr<TyValue> _y,
                                 std::shared_ptr<TyExpr> _e1,
                                 std::shared_ptr<TyExpr> _e2,
                                 std::shared_ptr<TyExpr> _e2_p)
    : x(std::move(_x)), y(std::move(_y)), e1(std::move(_e1)),
      e2(std::move(_e2)), e2_p(std::move(_e2_p)) {}

void TyReplaceRhsOpt::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(x), CEREAL_NVP(y), CEREAL_NVP(e1), CEREAL_NVP(e2),
          cereal::make_nvp("e2\'", e2_p));
}

ConsReplaceRhsOpt::ConsReplaceRhsOpt(
    std::shared_ptr<TyReplaceRhsOpt> _replace_rhs_opt)
    : replace_rhs_opt(std::move(_replace_rhs_opt)) {}

void ConsReplaceRhsOpt::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("ReplaceRhsOpt");
  archive(CEREAL_NVP(replace_rhs_opt));
}
std::shared_ptr<TyInfrule> ConsReplaceRhsOpt::make(
    std::shared_ptr<TyRegister> _x, std::shared_ptr<TyValue> _y,
    std::shared_ptr<TyExpr> _e1, std::shared_ptr<TyExpr> _e2,
    std::shared_ptr<TyExpr> _e2_p) {
  std::shared_ptr<TyReplaceRhsOpt> _replace_rhs_opt(
      new TyReplaceRhsOpt(std::move(_x), std::move(_y), std::move(_e1),
                          std::move(_e2), std::move(_e2_p)));
  return std::shared_ptr<TyInfrule>(
      new ConsReplaceRhsOpt(std::move(_replace_rhs_opt)));
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

TyIntroGhost::TyIntroGhost(std::shared_ptr<TyExpr> _x,
                           std::shared_ptr<TyRegister> _g)
    : x(std::move(_x)), g(std::move(_g)) {}

void TyIntroGhost::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(g));
}

std::shared_ptr<TyExpr> TyIntroGhost::getExpr() {
  return x;
}

std::shared_ptr<TyRegister> TyIntroGhost::getReg() {
  return g;
}

void TyIntroGhost::updateExpr(std::shared_ptr<TyExpr> newExpr) {
  x = newExpr;
}

ConsIntroGhost::ConsIntroGhost(std::shared_ptr<TyIntroGhost> _intro_ghost)
    : intro_ghost(std::move(_intro_ghost)) {}

std::shared_ptr<TyInfrule>
ConsIntroGhost::make(std::shared_ptr<TyExpr> _x,
                     std::shared_ptr<TyRegister> _g) {
  std::shared_ptr<TyIntroGhost> _val(
      new TyIntroGhost(std::move(_x), std::move(_g)));
  return std::shared_ptr<TyInfrule>(new ConsIntroGhost(std::move(_val)));
}

void ConsIntroGhost::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IntroGhost");
  archive(CEREAL_NVP(intro_ghost));
}

TyIntroEq::TyIntroEq(std::shared_ptr<TyExpr> _x) : x(_x) {}

void TyIntroEq::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(x));
}

ConsIntroEq::ConsIntroEq(std::shared_ptr<TyIntroEq> _intro_eq)
    : intro_eq(_intro_eq) {}

std::shared_ptr<TyInfrule> ConsIntroEq::make(std::shared_ptr<TyValue> _x) {
  return ConsIntroEq::make(TyExpr::make(_x));
}

std::shared_ptr<TyInfrule> ConsIntroEq::make(std::shared_ptr<TyExpr> _x) {
  std::shared_ptr<TyIntroEq> _val(new TyIntroEq(_x));
  return std::shared_ptr<TyInfrule>(new ConsIntroEq(_val));
}

void ConsIntroEq::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IntroEq");
  archive(CEREAL_NVP(intro_eq));
}

TyXorCommutativeTgt::TyXorCommutativeTgt(std::shared_ptr<TyRegister> _z,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)), sz(std::move(_sz)) {
}
void TyXorCommutativeTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(sz));
}

ConsXorCommutativeTgt::ConsXorCommutativeTgt(
    std::shared_ptr<TyXorCommutativeTgt> _xor_commutative_tgt)
    : xor_commutative_tgt(std::move(_xor_commutative_tgt)) {}
std::shared_ptr<TyInfrule> ConsXorCommutativeTgt::make(
    std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x,
    std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyXorCommutativeTgt> _val(new TyXorCommutativeTgt(
      std::move(_z), std::move(_x), std::move(_y), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsXorCommutativeTgt(std::move(_val)));
}
void
ConsXorCommutativeTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("XorCommutativeTgt");
  archive(CEREAL_NVP(xor_commutative_tgt));
}

TyXorNot::TyXorNot(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _y,
                   std::shared_ptr<TyValue> _x, std::shared_ptr<TySize> _s)
    : z(_z), y(_y), x(_x), s(_s) {}
void TyXorNot::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(s));
}
ConsXorNot::ConsXorNot(std::shared_ptr<TyXorNot> _xor_not)
    : xor_not(_xor_not) {}
std::shared_ptr<TyInfrule> ConsXorNot::make(std::shared_ptr<TyValue> _z,
                                            std::shared_ptr<TyValue> _y,
                                            std::shared_ptr<TyValue> _x,
                                            std::shared_ptr<TySize> _s) {
  std::shared_ptr<TyXorNot> _val(new TyXorNot(_z, _y, _x, _s));
  return std::shared_ptr<TyInfrule>(new ConsXorNot(_val));
}
void ConsXorNot::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("XorNot");
  archive(CEREAL_NVP(xor_not));
}

TyXorSame::TyXorSame(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _a,
                     std::shared_ptr<TySize> _s)
    : z(_z), a(_a), s(_s) {}
void TyXorSame::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(s));
}
ConsXorSame::ConsXorSame(std::shared_ptr<TyXorSame> _xor_same)
    : xor_same(_xor_same) {}
std::shared_ptr<TyInfrule> ConsXorSame::make(std::shared_ptr<TyValue> _z,
                                             std::shared_ptr<TyValue> _a,
                                             std::shared_ptr<TySize> _s) {
  std::shared_ptr<TyXorSame> _val(new TyXorSame(_z, _a, _s));
  return std::shared_ptr<TyInfrule>(new ConsXorSame(_val));
}
void ConsXorSame::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("XorSame");
  archive(CEREAL_NVP(xor_same));
}

TyXorUndef::TyXorUndef(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _a,
                       std::shared_ptr<TySize> _s)
    : z(_z), a(_a), s(_s) {}
void TyXorUndef::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(s));
}
ConsXorUndef::ConsXorUndef(std::shared_ptr<TyXorUndef> _xor_undef)
    : xor_undef(_xor_undef) {}
std::shared_ptr<TyInfrule> ConsXorUndef::make(std::shared_ptr<TyValue> _z,
                                              std::shared_ptr<TyValue> _a,
                                              std::shared_ptr<TySize> _s) {
  std::shared_ptr<TyXorUndef> _val(new TyXorUndef(_z, _a, _s));
  return std::shared_ptr<TyInfrule>(new ConsXorUndef(_val));
}
void ConsXorUndef::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("XorUndef");
  archive(CEREAL_NVP(xor_undef));
}

TyXorZero::TyXorZero(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _a,
                     std::shared_ptr<TySize> _s)
    : z(_z), a(_a), s(_s) {}
void TyXorZero::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(s));
}
ConsXorZero::ConsXorZero(std::shared_ptr<TyXorZero> _xor_zero)
    : xor_zero(_xor_zero) {}
std::shared_ptr<TyInfrule> ConsXorZero::make(std::shared_ptr<TyValue> _z,
                                             std::shared_ptr<TyValue> _a,
                                             std::shared_ptr<TySize> _s) {
  std::shared_ptr<TyXorZero> _val(new TyXorZero(_z, _a, _s));
  return std::shared_ptr<TyInfrule>(new ConsXorZero(_val));
}
void ConsXorZero::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("XorZero");
  archive(CEREAL_NVP(xor_zero));
}

TyInttoptrLoad::TyInttoptrLoad(std::shared_ptr<TyValue> _ptr,
                               std::shared_ptr<TyValueType> _intty,
                               std::shared_ptr<TyValue> _v1,
                               std::shared_ptr<TyValueType> _ptrty,
                               std::shared_ptr<TyValue> _v2,
                               std::shared_ptr<TySize> _a)
    : ptr(_ptr), intty(_intty), v1(_v1), ptrty(_ptrty), v2(_v2), a(_a) {}
void TyInttoptrLoad::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(ptr), CEREAL_NVP(intty), CEREAL_NVP(v1), CEREAL_NVP(ptrty),
          CEREAL_NVP(v2), CEREAL_NVP(a));
}

ConsInttoptrLoad::ConsInttoptrLoad(
    std::shared_ptr<TyInttoptrLoad> _inttoptr_load)
    : inttoptr_load(_inttoptr_load) {}
std::shared_ptr<TyInfrule> ConsInttoptrLoad::make(
    std::shared_ptr<TyValue> _ptr, std::shared_ptr<TyValueType> _intty,
    std::shared_ptr<TyValue> _v1, std::shared_ptr<TyValueType> _ptrty,
    std::shared_ptr<TyValue> _v2, std::shared_ptr<TySize> _a) {
  std::shared_ptr<TyInttoptrLoad> _val(
      new TyInttoptrLoad(_ptr, _intty, _v1, _ptrty, _v2, _a));
  return std::shared_ptr<TyInfrule>(new ConsInttoptrLoad(_val));
}
void ConsInttoptrLoad::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("InttoptrLoad");
  archive(CEREAL_NVP(inttoptr_load));
}

TyInttoptrBitcast::TyInttoptrBitcast(std::shared_ptr<TyValue> _src,
                                     std::shared_ptr<TyValue> _mid,
                                     std::shared_ptr<TyValue> _dst,
                                     std::shared_ptr<TyValueType> _srcty,
                                     std::shared_ptr<TyValueType> _midty,
                                     std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TyInttoptrBitcast::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src));
  archive(CEREAL_NVP(mid));
  archive(CEREAL_NVP(dst));
  archive(CEREAL_NVP(srcty));
  archive(CEREAL_NVP(midty));
  archive(CEREAL_NVP(dstty));
}
ConsInttoptrBitcast::ConsInttoptrBitcast(
    std::shared_ptr<TyInttoptrBitcast> _inttoptr_bitcast)
    : inttoptr_bitcast(_inttoptr_bitcast) {}
std::shared_ptr<TyInfrule> ConsInttoptrBitcast::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TyInttoptrBitcast> _val(
      new TyInttoptrBitcast(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsInttoptrBitcast(_val));
}
void ConsInttoptrBitcast::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("InttoptrBitcast");
  archive(CEREAL_NVP(inttoptr_bitcast));
}

TyInttoptrPtrtoint::TyInttoptrPtrtoint(std::shared_ptr<TyValue> _src,
                                       std::shared_ptr<TyValue> _mid,
                                       std::shared_ptr<TyValue> _dst,
                                       std::shared_ptr<TyValueType> _srcty,
                                       std::shared_ptr<TyValueType> _midty,
                                       std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TyInttoptrPtrtoint::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src), CEREAL_NVP(mid), CEREAL_NVP(dst), CEREAL_NVP(srcty),
          CEREAL_NVP(midty), CEREAL_NVP(dstty));
}

ConsInttoptrPtrtoint::ConsInttoptrPtrtoint(
    std::shared_ptr<TyInttoptrPtrtoint> _inttoptr_ptrtoint)
    : inttoptr_ptrtoint(_inttoptr_ptrtoint) {}
std::shared_ptr<TyInfrule> ConsInttoptrPtrtoint::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TyInttoptrPtrtoint> _val(
      new TyInttoptrPtrtoint(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsInttoptrPtrtoint(_val));
}
void ConsInttoptrPtrtoint::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("InttoptrPtrtoint");
  archive(CEREAL_NVP(inttoptr_ptrtoint));
}

TyInttoptrZext::TyInttoptrZext(std::shared_ptr<TyValue> _src,
                               std::shared_ptr<TyValue> _mid,
                               std::shared_ptr<TyValue> _dst,
                               std::shared_ptr<TyValueType> _srcty,
                               std::shared_ptr<TyValueType> _midty,
                               std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TyInttoptrZext::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src));
  archive(CEREAL_NVP(mid));
  archive(CEREAL_NVP(dst));
  archive(CEREAL_NVP(srcty));
  archive(CEREAL_NVP(midty));
  archive(CEREAL_NVP(dstty));
}
ConsInttoptrZext::ConsInttoptrZext(
    std::shared_ptr<TyInttoptrZext> _zext_inttoptr)
    : zext_inttoptr(_zext_inttoptr) {}
std::shared_ptr<TyInfrule> ConsInttoptrZext::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TyInttoptrZext> _val(
      new TyInttoptrZext(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsInttoptrZext(_val));
}
void ConsInttoptrZext::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("InttoptrZext");
  archive(CEREAL_NVP(zext_inttoptr));
}

TySextBitcast::TySextBitcast(std::shared_ptr<TyValue> _src,
                             std::shared_ptr<TyValue> _mid,
                             std::shared_ptr<TyValue> _dst,
                             std::shared_ptr<TyValueType> _srcty,
                             std::shared_ptr<TyValueType> _midty,
                             std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TySextBitcast::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src));
  archive(CEREAL_NVP(mid));
  archive(CEREAL_NVP(dst));
  archive(CEREAL_NVP(srcty));
  archive(CEREAL_NVP(midty));
  archive(CEREAL_NVP(dstty));
}

TySextZext::TySextZext(std::shared_ptr<TyValue> _src,
                       std::shared_ptr<TyValue> _mid,
                       std::shared_ptr<TyValue> _dst,
                       std::shared_ptr<TyValueType> _srcty,
                       std::shared_ptr<TyValueType> _midty,
                       std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TySextZext::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src));
  archive(CEREAL_NVP(mid));
  archive(CEREAL_NVP(dst));
  archive(CEREAL_NVP(srcty));
  archive(CEREAL_NVP(midty));
  archive(CEREAL_NVP(dstty));
}
ConsSextZext::ConsSextZext(std::shared_ptr<TySextZext> _zext_sext)
    : zext_sext(_zext_sext) {}
std::shared_ptr<TyInfrule> ConsSextZext::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TySextZext> _val(
      new TySextZext(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsSextZext(_val));
}
void ConsSextZext::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SextZext");
  archive(CEREAL_NVP(zext_sext));
}

TyUitofpBitcast::TyUitofpBitcast(std::shared_ptr<TyValue> _src,
                                 std::shared_ptr<TyValue> _mid,
                                 std::shared_ptr<TyValue> _dst,
                                 std::shared_ptr<TyValueType> _srcty,
                                 std::shared_ptr<TyValueType> _midty,
                                 std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TyUitofpBitcast::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src));
  archive(CEREAL_NVP(mid));
  archive(CEREAL_NVP(dst));
  archive(CEREAL_NVP(srcty));
  archive(CEREAL_NVP(midty));
  archive(CEREAL_NVP(dstty));
}
ConsUitofpBitcast::ConsUitofpBitcast(
    std::shared_ptr<TyUitofpBitcast> _uitofp_bitcast)
    : uitofp_bitcast(_uitofp_bitcast) {}
std::shared_ptr<TyInfrule> ConsUitofpBitcast::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TyUitofpBitcast> _val(
      new TyUitofpBitcast(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsUitofpBitcast(_val));
}
void ConsUitofpBitcast::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("UitofpBitcast");
  archive(CEREAL_NVP(uitofp_bitcast));
}

TyUitofpZext::TyUitofpZext(std::shared_ptr<TyValue> _src,
                           std::shared_ptr<TyValue> _mid,
                           std::shared_ptr<TyValue> _dst,
                           std::shared_ptr<TyValueType> _srcty,
                           std::shared_ptr<TyValueType> _midty,
                           std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TyUitofpZext::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src));
  archive(CEREAL_NVP(mid));
  archive(CEREAL_NVP(dst));
  archive(CEREAL_NVP(srcty));
  archive(CEREAL_NVP(midty));
  archive(CEREAL_NVP(dstty));
}
ConsUitofpZext::ConsUitofpZext(std::shared_ptr<TyUitofpZext> _zext_uitofp)
    : zext_uitofp(_zext_uitofp) {}
std::shared_ptr<TyInfrule> ConsUitofpZext::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TyUitofpZext> _val(
      new TyUitofpZext(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsUitofpZext(_val));
}
void ConsUitofpZext::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("UitofpZext");
  archive(CEREAL_NVP(zext_uitofp));
}

TyZextBitcast::TyZextBitcast(std::shared_ptr<TyValue> _src,
                             std::shared_ptr<TyValue> _mid,
                             std::shared_ptr<TyValue> _dst,
                             std::shared_ptr<TyValueType> _srcty,
                             std::shared_ptr<TyValueType> _midty,
                             std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TyZextBitcast::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src), CEREAL_NVP(mid), CEREAL_NVP(dst), CEREAL_NVP(srcty),
          CEREAL_NVP(midty), CEREAL_NVP(dstty));
}
ConsZextBitcast::ConsZextBitcast(std::shared_ptr<TyZextBitcast> _zext_bitcast)
    : zext_bitcast(_zext_bitcast) {}
std::shared_ptr<TyInfrule> ConsZextBitcast::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TyZextBitcast> _val(
      new TyZextBitcast(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsZextBitcast(_val));
}
void ConsZextBitcast::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("ZextBitcast");
  archive(CEREAL_NVP(zext_bitcast));
}

TyZextTruncAnd::TyZextTruncAnd(std::shared_ptr<TyValue> _z,
                               std::shared_ptr<TyValue> _x,
                               std::shared_ptr<TyValue> _y,
                               std::shared_ptr<TyValue> _w,
                               std::shared_ptr<TyConstant> _c,
                               std::shared_ptr<TySize> _s,
                               std::shared_ptr<TySize> _sprime)
    : z(_z), x(_x), y(_y), w(_w), c(_c), s(_s), sprime(_sprime) {}
void TyZextTruncAnd::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(x), CEREAL_NVP(y), CEREAL_NVP(w),
          CEREAL_NVP(c), CEREAL_NVP(s), CEREAL_NVP(sprime));
}

ConsZextTruncAnd::ConsZextTruncAnd(
    std::shared_ptr<TyZextTruncAnd> _zext_trunc_and)
    : zext_trunc_and(_zext_trunc_and) {}
std::shared_ptr<TyInfrule>
ConsZextTruncAnd::make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
                       std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _w,
                       std::shared_ptr<TyConstant> _c,
                       std::shared_ptr<TySize> _s,
                       std::shared_ptr<TySize> _sprime) {
  std::shared_ptr<TyZextTruncAnd> _val(
      new TyZextTruncAnd(_z, _x, _y, _w, _c, _s, _sprime));
  return std::shared_ptr<TyInfrule>(new ConsZextTruncAnd(_val));
}
void ConsZextTruncAnd::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("ZextTruncAnd");
  archive(CEREAL_NVP(zext_trunc_and));
}

TyZextTruncAndXor::TyZextTruncAndXor(
    std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
    std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _w,
    std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _yprime,
    std::shared_ptr<TyConstant> _c, std::shared_ptr<TySize> _s,
    std::shared_ptr<TySize> _sprime)
    : z(_z), x(_x), v(_v), w(_w), y(_y), yprime(_yprime), c(_c), s(_s),
      sprime(_sprime) {}
void TyZextTruncAndXor::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(x), CEREAL_NVP(v), CEREAL_NVP(w),
          CEREAL_NVP(y), CEREAL_NVP(yprime), CEREAL_NVP(c), CEREAL_NVP(s),
          CEREAL_NVP(sprime));
}

ConsZextTruncAndXor::ConsZextTruncAndXor(
    std::shared_ptr<TyZextTruncAndXor> _zext_trunc_and_xor)
    : zext_trunc_and_xor(_zext_trunc_and_xor) {}
std::shared_ptr<TyInfrule> ConsZextTruncAndXor::make(
    std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
    std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _w,
    std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _yprime,
    std::shared_ptr<TyConstant> _c, std::shared_ptr<TySize> _s,
    std::shared_ptr<TySize> _sprime) {
  std::shared_ptr<TyZextTruncAndXor> _val(
      new TyZextTruncAndXor(_z, _x, _v, _w, _y, _yprime, _c, _s, _sprime));
  return std::shared_ptr<TyInfrule>(new ConsZextTruncAndXor(_val));
}
void ConsZextTruncAndXor::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("ZextTruncAndXor");
  archive(CEREAL_NVP(zext_trunc_and_xor));
}

TyZextXor::TyZextXor(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _y,
                     std::shared_ptr<TyValue> _yprime,
                     std::shared_ptr<TyValue> _x,
                     std::shared_ptr<TySize> _s)
    : z(_z), y(_y), yprime(_yprime), x(_x), s(_s) {}
void TyZextXor::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(y), CEREAL_NVP(yprime), CEREAL_NVP(x), 
          CEREAL_NVP(s));
}

ConsZextXor::ConsZextXor(std::shared_ptr<TyZextXor> _zext_xor)
    : zext_xor(_zext_xor) {}
std::shared_ptr<TyInfrule> ConsZextXor::make(std::shared_ptr<TyValue> _z,
                                             std::shared_ptr<TyValue> _y,
                                             std::shared_ptr<TyValue> _yprime,
                                             std::shared_ptr<TyValue> _x,
                                             std::shared_ptr<TySize> _s) {
  std::shared_ptr<TyZextXor> _val(new TyZextXor(_z, _y, _yprime, _x, _s));
  return std::shared_ptr<TyInfrule>(new ConsZextXor(_val));
}
void ConsZextXor::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("ZextXor");
  archive(CEREAL_NVP(zext_xor));
}

TyZextZext::TyZextZext(std::shared_ptr<TyValue> _src,
                       std::shared_ptr<TyValue> _mid,
                       std::shared_ptr<TyValue> _dst,
                       std::shared_ptr<TyValueType> _srcty,
                       std::shared_ptr<TyValueType> _midty,
                       std::shared_ptr<TyValueType> _dstty)
    : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty),
      dstty(_dstty) {}
void TyZextZext::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src));
  archive(CEREAL_NVP(mid));
  archive(CEREAL_NVP(dst));
  archive(CEREAL_NVP(srcty));
  archive(CEREAL_NVP(midty));
  archive(CEREAL_NVP(dstty));
}
ConsZextZext::ConsZextZext(std::shared_ptr<TyZextZext> _zext_zext)
    : zext_zext(_zext_zext) {}
std::shared_ptr<TyInfrule> ConsZextZext::make(
    std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
    std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
    std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TyZextZext> _val(
      new TyZextZext(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsZextZext(_val));
}
void ConsZextZext::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("ZextZext");
  archive(CEREAL_NVP(zext_zext));
}
TyImpliesFalse::TyImpliesFalse(std::shared_ptr<TyConstant> _c1,
                               std::shared_ptr<TyConstant> _c2)
    : c1(_c1), c2(_c2) {}
void TyImpliesFalse::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(c1));
  archive(CEREAL_NVP(c2));
}

ConsImpliesFalse::ConsImpliesFalse(
    std::shared_ptr<TyImpliesFalse> _implies_false)
    : implies_false(_implies_false) {}
std::shared_ptr<TyInfrule>
ConsImpliesFalse::make(std::shared_ptr<TyConstant> _c1,
                       std::shared_ptr<TyConstant> _c2) {
  std::shared_ptr<TyImpliesFalse> _val(new TyImpliesFalse(_c1, _c2));
  return std::shared_ptr<TyInfrule>(new ConsImpliesFalse(_val));
}
void ConsImpliesFalse::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("ImpliesFalse");
  archive(CEREAL_NVP(implies_false));
}

TyIcmpInverse::TyIcmpInverse(enum TyIcmpPred _predicate,
                             std::shared_ptr<TyValueType> _ty,
                             std::shared_ptr<TyValue> _x,
                             std::shared_ptr<TyValue> _y,
                             std::shared_ptr<TyConstInt> _boolean)
    : predicate(_predicate), ty(std::move(_ty)), x(std::move(_x)),
      y(std::move(_y)), boolean(std::move(_boolean)) {}

void TyIcmpInverse::serialize(cereal::JSONOutputArchive &archive) const {
  archive(cereal::make_nvp("predicate", toString(predicate)));
  archive(CEREAL_NVP(ty));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(boolean));
}

ConsIcmpInverse::ConsIcmpInverse(std::shared_ptr<TyIcmpInverse> _icmp_inverse)
    : icmp_inverse(std::move(_icmp_inverse)) {}

std::shared_ptr<TyInfrule> ConsIcmpInverse::make(llvm::ICmpInst &CI,
                                                 int bool_val) {
  enum TyIcmpPred pred = getIcmpPred(CI.getPredicate());
  std::shared_ptr<TyValueType> ty =
      TyValueType::make(*CI.getOperand(0)->getType());
  std::shared_ptr<TyValue> x = TyValue::make(*CI.getOperand(0));
  std::shared_ptr<TyValue> y = TyValue::make(*CI.getOperand(1));
  std::shared_ptr<TyConstInt> boolean = TyConstInt::make(bool_val, 1);

  std::shared_ptr<TyIcmpInverse> _icmp_inv =
      std::make_shared<TyIcmpInverse>(pred, ty, x, y, boolean);

  return std::make_shared<ConsIcmpInverse>(_icmp_inv);
}

void ConsIcmpInverse::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IcmpInverse");
  archive(CEREAL_NVP(icmp_inverse));
}

TyIcmpInverseRhs::TyIcmpInverseRhs(enum TyIcmpPred _predicate,
                                   std::shared_ptr<TyValueType> _ty,
                                   std::shared_ptr<TyValue> _x,
                                   std::shared_ptr<TyValue> _y,
                                   std::shared_ptr<TyConstInt> _boolean)
    : predicate(_predicate), ty(std::move(_ty)), x(std::move(_x)),
      y(std::move(_y)), boolean(std::move(_boolean)) {}

void TyIcmpInverseRhs::serialize(cereal::JSONOutputArchive &archive) const {
  archive(cereal::make_nvp("predicate", toString(predicate)));
  archive(CEREAL_NVP(ty));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(boolean));
}

ConsIcmpInverseRhs::ConsIcmpInverseRhs(std::shared_ptr<TyIcmpInverseRhs> _icmp_inverse_rhs)
    : icmp_inverse_rhs(std::move(_icmp_inverse_rhs)) {}

std::shared_ptr<TyInfrule> ConsIcmpInverseRhs::make(llvm::ICmpInst &CI,
                                                    int bool_val) {
  enum TyIcmpPred pred = getIcmpPred(CI.getPredicate());
  std::shared_ptr<TyValueType> ty =
      TyValueType::make(*CI.getOperand(0)->getType());
  std::shared_ptr<TyValue> x = TyValue::make(*CI.getOperand(0));
  std::shared_ptr<TyValue> y = TyValue::make(*CI.getOperand(1));
  std::shared_ptr<TyConstInt> boolean = TyConstInt::make(bool_val, 1);

  std::shared_ptr<TyIcmpInverseRhs> _icmp_inv_rhs =
      std::make_shared<TyIcmpInverseRhs>(pred, ty, x, y, boolean);

  return std::make_shared<ConsIcmpInverseRhs>(_icmp_inv_rhs);
}

void ConsIcmpInverseRhs::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IcmpInverseRhs");
  archive(CEREAL_NVP(icmp_inverse_rhs));
}

TyIcmpSwapOperands::TyIcmpSwapOperands(enum TyIcmpPred _predicate,
                             std::shared_ptr<TyValueType> _ty,
                             std::shared_ptr<TyValue> _x,
                             std::shared_ptr<TyValue> _y,
                             std::shared_ptr<TyValue> _z)
    : predicate(_predicate), ty(_ty), x(_x), y(_y), z(_z) {}

void TyIcmpSwapOperands::serialize(cereal::JSONOutputArchive &archive) const {
  archive(cereal::make_nvp("predicate", toString(predicate)));
  archive(CEREAL_NVP(ty));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(z));
}

ConsIcmpSwapOperands::ConsIcmpSwapOperands(std::shared_ptr<TyIcmpSwapOperands> _icmp_swap_operands)
    : icmp_swap_operands(_icmp_swap_operands) {}

std::shared_ptr<TyInfrule> ConsIcmpSwapOperands::make(llvm::ICmpInst &CI) {
  enum TyIcmpPred pred = getIcmpPred(CI.getPredicate());
  std::shared_ptr<TyValueType> ty =
      TyValueType::make(*CI.getOperand(0)->getType());
  std::shared_ptr<TyValue> x = TyValue::make(*CI.getOperand(0));
  std::shared_ptr<TyValue> y = TyValue::make(*CI.getOperand(1));
  std::shared_ptr<TyValue> z = TyValue::make(CI);

  std::shared_ptr<TyIcmpSwapOperands> _icmp_so =
      std::make_shared<TyIcmpSwapOperands>(pred, ty, x, y, z);

  return std::make_shared<ConsIcmpSwapOperands>(_icmp_so);
}

void ConsIcmpSwapOperands::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IcmpSwapOperands");
  archive(CEREAL_NVP(icmp_swap_operands));
}

TyIcmpEqSame::TyIcmpEqSame(std::shared_ptr<TyValueType> _ty,
                           std::shared_ptr<TyValue> _x,
                           std::shared_ptr<TyValue> _y)
    : ty(std::move(_ty)), x(std::move(_x)), y(std::move(_y)) {}

void TyIcmpEqSame::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(ty));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
}

TyIcmpEqAddAdd::TyIcmpEqAddAdd(std::shared_ptr<TyValue> _z,
                               std::shared_ptr<TyValue> _w,
                               std::shared_ptr<TyValue> _x,
                               std::shared_ptr<TyValue> _y,
                               std::shared_ptr<TyValue> _a,
                               std::shared_ptr<TyValue> _b,
                               std::shared_ptr<TySize> _s)
    : z(_z), w(_w), x(_x), y(_y), a(_a), b(_b), s(_s) {}
void TyIcmpEqAddAdd::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(w), CEREAL_NVP(x), CEREAL_NVP(y),
          CEREAL_NVP(a), CEREAL_NVP(b), CEREAL_NVP(s));
}

ConsIcmpEqAddAdd::ConsIcmpEqAddAdd(
    std::shared_ptr<TyIcmpEqAddAdd> _icmp_eq_add_add)
    : icmp_eq_add_add(_icmp_eq_add_add) {}
std::shared_ptr<TyInfrule>
ConsIcmpEqAddAdd::make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _w,
                       std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
                       std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
                       std::shared_ptr<TySize> _s) {
  std::shared_ptr<TyIcmpEqAddAdd> _val(
      new TyIcmpEqAddAdd(_z, _w, _x, _y, _a, _b, _s));
  return std::shared_ptr<TyInfrule>(new ConsIcmpEqAddAdd(_val));
}
void ConsIcmpEqAddAdd::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IcmpEqAddAdd");
  archive(CEREAL_NVP(icmp_eq_add_add));
}

ConsIcmpEqSame::ConsIcmpEqSame(std::shared_ptr<TyIcmpEqSame> _icmp_eq_same)
    : icmp_eq_same(_icmp_eq_same) {}

std::shared_ptr<TyInfrule> ConsIcmpEqSame::make(llvm::ICmpInst &CI) {
  std::shared_ptr<TyValueType> ty =
      TyValueType::make(*CI.getOperand(0)->getType());
  std::shared_ptr<TyValue> x = TyValue::make(*CI.getOperand(0));
  std::shared_ptr<TyValue> y = TyValue::make(*CI.getOperand(1));
  std::shared_ptr<TyIcmpEqSame> _icmp_eq_same =
      std::make_shared<TyIcmpEqSame>(ty, x, y);

  return std::make_shared<ConsIcmpEqSame>(_icmp_eq_same);
}

TyIcmpEqXorNot::TyIcmpEqXorNot(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s) : z(_z), zprime(_zprime), a(_a), b(_b), s(_s){
}
void TyIcmpEqXorNot::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z), CEREAL_NVP(zprime), CEREAL_NVP(a), CEREAL_NVP(b), CEREAL_NVP(s));
}
ConsIcmpEqXorNot::ConsIcmpEqXorNot(std::shared_ptr<TyIcmpEqXorNot> _icmp_eq_xor_not) : icmp_eq_xor_not(_icmp_eq_xor_not){
}
std::shared_ptr<TyInfrule> ConsIcmpEqXorNot::make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s){
  std::shared_ptr<TyIcmpEqXorNot> _val(new TyIcmpEqXorNot(_z, _zprime, _a, _b, _s));
  return std::shared_ptr<TyInfrule>(new ConsIcmpEqXorNot(_val));
}
void ConsIcmpEqXorNot::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IcmpEqXorNot");
  archive(CEREAL_NVP(icmp_eq_xor_not));
}

void ConsIcmpEqSame::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IcmpEqSame");
  archive(CEREAL_NVP(icmp_eq_same));
}

TyIcmpEqSrem::TyIcmpEqSrem(std::shared_ptr<TyValue> _z,
                           std::shared_ptr<TyValue> _w,
                           std::shared_ptr<TyValue> _x,
                           std::shared_ptr<TyValue> _y,
                           std::shared_ptr<TySize> _s)
    : z(_z), w(_w), x(_x), y(_y), s(_s) {}
void TyIcmpEqSrem::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(w), CEREAL_NVP(x), CEREAL_NVP(y),
          CEREAL_NVP(s));
}

ConsIcmpEqSrem::ConsIcmpEqSrem(std::shared_ptr<TyIcmpEqSrem> _icmp_eq_srem)
    : icmp_eq_srem(_icmp_eq_srem) {}
std::shared_ptr<TyInfrule> ConsIcmpEqSrem::make(std::shared_ptr<TyValue> _z,
                                                std::shared_ptr<TyValue> _w,
                                                std::shared_ptr<TyValue> _x,
                                                std::shared_ptr<TyValue> _y,
                                                std::shared_ptr<TySize> _s) {
  std::shared_ptr<TyIcmpEqSrem> _val(new TyIcmpEqSrem(_z, _w, _x, _y, _s));
  return std::shared_ptr<TyInfrule>(new ConsIcmpEqSrem(_val));
}
void ConsIcmpEqSrem::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IcmpEqSrem");
  archive(CEREAL_NVP(icmp_eq_srem));
}

TyIcmpEqSub::TyIcmpEqSub(std::shared_ptr<TyValue> _z,
                         std::shared_ptr<TyValue> _x,
                         std::shared_ptr<TyValue> _a,
                         std::shared_ptr<TyValue> _b,
                         std::shared_ptr<TySize> _s)
    : z(_z), x(_x), a(_a), b(_b), s(_s) {}
void TyIcmpEqSub::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(x), CEREAL_NVP(a), CEREAL_NVP(b),
          CEREAL_NVP(s));
}

ConsIcmpEqSub::ConsIcmpEqSub(std::shared_ptr<TyIcmpEqSub> _icmp_eq_sub)
    : icmp_eq_sub(_icmp_eq_sub) {}
std::shared_ptr<TyInfrule> ConsIcmpEqSub::make(std::shared_ptr<TyValue> _z,
                                               std::shared_ptr<TyValue> _x,
                                               std::shared_ptr<TyValue> _a,
                                               std::shared_ptr<TyValue> _b,
                                               std::shared_ptr<TySize> _s) {
  std::shared_ptr<TyIcmpEqSub> _val(new TyIcmpEqSub(_z, _x, _a, _b, _s));
  return std::shared_ptr<TyInfrule>(new ConsIcmpEqSub(_val));
}

void ConsIcmpEqSub::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IcmpEqSub");
  archive(CEREAL_NVP(icmp_eq_sub));
}
TyIcmpEqSubSub::TyIcmpEqSubSub(std::shared_ptr<TyValue> _z,
                               std::shared_ptr<TyValue> _w,
                               std::shared_ptr<TyValue> _x,
                               std::shared_ptr<TyValue> _y,
                               std::shared_ptr<TyValue> _a,
                               std::shared_ptr<TyValue> _b,
                               std::shared_ptr<TySize> _s)
    : z(_z), w(_w), x(_x), y(_y), a(_a), b(_b), s(_s) {}
void TyIcmpEqSubSub::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(w), CEREAL_NVP(x), CEREAL_NVP(y),
          CEREAL_NVP(a), CEREAL_NVP(b), CEREAL_NVP(s));
}

ConsIcmpEqSubSub::ConsIcmpEqSubSub(
    std::shared_ptr<TyIcmpEqSubSub> _icmp_eq_sub_sub)
    : icmp_eq_sub_sub(_icmp_eq_sub_sub) {}
std::shared_ptr<TyInfrule>
ConsIcmpEqSubSub::make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _w,
                       std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
                       std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
                       std::shared_ptr<TySize> _s) {
  std::shared_ptr<TyIcmpEqSubSub> _val(
      new TyIcmpEqSubSub(_z, _w, _x, _y, _a, _b, _s));
  return std::shared_ptr<TyInfrule>(new ConsIcmpEqSubSub(_val));
}
void ConsIcmpEqSubSub::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IcmpEqSubSub");
  archive(CEREAL_NVP(icmp_eq_sub_sub));
}

TyIcmpEqXorXor::TyIcmpEqXorXor(std::shared_ptr<TyValue> _z,
                               std::shared_ptr<TyValue> _w,
                               std::shared_ptr<TyValue> _x,
                               std::shared_ptr<TyValue> _y,
                               std::shared_ptr<TyValue> _a,
                               std::shared_ptr<TyValue> _b,
                               std::shared_ptr<TySize> _s)
    : z(_z), w(_w), x(_x), y(_y), a(_a), b(_b), s(_s) {}
void TyIcmpEqXorXor::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(w), CEREAL_NVP(x), CEREAL_NVP(y),
          CEREAL_NVP(a), CEREAL_NVP(b), CEREAL_NVP(s));
}

ConsIcmpEqXorXor::ConsIcmpEqXorXor(
    std::shared_ptr<TyIcmpEqXorXor> _icmp_eq_xor_xor)
    : icmp_eq_xor_xor(_icmp_eq_xor_xor) {}
std::shared_ptr<TyInfrule>
ConsIcmpEqXorXor::make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _w,
                       std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
                       std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
                       std::shared_ptr<TySize> _s) {
  std::shared_ptr<TyIcmpEqXorXor> _val(
      new TyIcmpEqXorXor(_z, _w, _x, _y, _a, _b, _s));
  return std::shared_ptr<TyInfrule>(new ConsIcmpEqXorXor(_val));
}
void ConsIcmpEqXorXor::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IcmpEqXorXor");
  archive(CEREAL_NVP(icmp_eq_xor_xor));
}

TyIcmpNeAddAdd::TyIcmpNeAddAdd(std::shared_ptr<TyValue> _z,
                               std::shared_ptr<TyValue> _w,
                               std::shared_ptr<TyValue> _x,
                               std::shared_ptr<TyValue> _y,
                               std::shared_ptr<TyValue> _a,
                               std::shared_ptr<TyValue> _b,
                               std::shared_ptr<TySize> _s)
    : z(_z), w(_w), x(_x), y(_y), a(_a), b(_b), s(_s) {}
void TyIcmpNeAddAdd::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(w), CEREAL_NVP(x), CEREAL_NVP(y),
          CEREAL_NVP(a), CEREAL_NVP(b), CEREAL_NVP(s));
}

ConsIcmpNeAddAdd::ConsIcmpNeAddAdd(
    std::shared_ptr<TyIcmpNeAddAdd> _icmp_ne_add_add)
    : icmp_ne_add_add(_icmp_ne_add_add) {}
std::shared_ptr<TyInfrule>
ConsIcmpNeAddAdd::make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _w,
                       std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
                       std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
                       std::shared_ptr<TySize> _s) {
  std::shared_ptr<TyIcmpNeAddAdd> _val(
      new TyIcmpNeAddAdd(_z, _w, _x, _y, _a, _b, _s));
  return std::shared_ptr<TyInfrule>(new ConsIcmpNeAddAdd(_val));
}
void ConsIcmpNeAddAdd::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IcmpNeAddAdd");
  archive(CEREAL_NVP(icmp_ne_add_add));
}

TyIcmpNeSrem::TyIcmpNeSrem(std::shared_ptr<TyValue> _z,
                           std::shared_ptr<TyValue> _w,
                           std::shared_ptr<TyValue> _x,
                           std::shared_ptr<TyValue> _y,
                           std::shared_ptr<TySize> _s)
    : z(_z), w(_w), x(_x), y(_y), s(_s) {}
void TyIcmpNeSrem::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(w), CEREAL_NVP(x), CEREAL_NVP(y),
          CEREAL_NVP(s));
}

ConsIcmpNeSrem::ConsIcmpNeSrem(std::shared_ptr<TyIcmpNeSrem> _icmp_ne_srem)
    : icmp_ne_srem(_icmp_ne_srem) {}
std::shared_ptr<TyInfrule> ConsIcmpNeSrem::make(std::shared_ptr<TyValue> _z,
                                                std::shared_ptr<TyValue> _w,
                                                std::shared_ptr<TyValue> _x,
                                                std::shared_ptr<TyValue> _y,
                                                std::shared_ptr<TySize> _s) {
  std::shared_ptr<TyIcmpNeSrem> _val(new TyIcmpNeSrem(_z, _w, _x, _y, _s));
  return std::shared_ptr<TyInfrule>(new ConsIcmpNeSrem(_val));
}
void ConsIcmpNeSrem::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IcmpNeSrem");
  archive(CEREAL_NVP(icmp_ne_srem));
}

TyIcmpNeSub::TyIcmpNeSub(std::shared_ptr<TyValue> _z,
                         std::shared_ptr<TyValue> _x,
                         std::shared_ptr<TyValue> _a,
                         std::shared_ptr<TyValue> _b,
                         std::shared_ptr<TySize> _s)
    : z(_z), x(_x), a(_a), b(_b), s(_s) {}
void TyIcmpNeSub::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(x), CEREAL_NVP(a), CEREAL_NVP(b),
          CEREAL_NVP(s));
}

ConsIcmpNeSub::ConsIcmpNeSub(std::shared_ptr<TyIcmpNeSub> _icmp_ne_sub)
    : icmp_ne_sub(_icmp_ne_sub) {}
std::shared_ptr<TyInfrule> ConsIcmpNeSub::make(std::shared_ptr<TyValue> _z,
                                               std::shared_ptr<TyValue> _x,
                                               std::shared_ptr<TyValue> _a,
                                               std::shared_ptr<TyValue> _b,
                                               std::shared_ptr<TySize> _s) {
  std::shared_ptr<TyIcmpNeSub> _val(new TyIcmpNeSub(_z, _x, _a, _b, _s));
  return std::shared_ptr<TyInfrule>(new ConsIcmpNeSub(_val));
}
void ConsIcmpNeSub::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IcmpNeSub");
  archive(CEREAL_NVP(icmp_ne_sub));
}

TyIcmpNeSubSub::TyIcmpNeSubSub(std::shared_ptr<TyValue> _z,
                               std::shared_ptr<TyValue> _w,
                               std::shared_ptr<TyValue> _x,
                               std::shared_ptr<TyValue> _y,
                               std::shared_ptr<TyValue> _a,
                               std::shared_ptr<TyValue> _b,
                               std::shared_ptr<TySize> _s)
    : z(_z), w(_w), x(_x), y(_y), a(_a), b(_b), s(_s) {}
void TyIcmpNeSubSub::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(w), CEREAL_NVP(x), CEREAL_NVP(y),
          CEREAL_NVP(a), CEREAL_NVP(b), CEREAL_NVP(s));
}

ConsIcmpNeSubSub::ConsIcmpNeSubSub(
    std::shared_ptr<TyIcmpNeSubSub> _icmp_ne_sub_sub)
    : icmp_ne_sub_sub(_icmp_ne_sub_sub) {}
std::shared_ptr<TyInfrule>
ConsIcmpNeSubSub::make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _w,
                       std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
                       std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
                       std::shared_ptr<TySize> _s) {
  std::shared_ptr<TyIcmpNeSubSub> _val(
      new TyIcmpNeSubSub(_z, _w, _x, _y, _a, _b, _s));
  return std::shared_ptr<TyInfrule>(new ConsIcmpNeSubSub(_val));
}
void ConsIcmpNeSubSub::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IcmpNeSubSub");
  archive(CEREAL_NVP(icmp_ne_sub_sub));
}

TyIcmpNeXor::TyIcmpNeXor(std::shared_ptr<TyValue> _z,
                         std::shared_ptr<TyValue> _a,
                         std::shared_ptr<TyValue> _b,
                         std::shared_ptr<TySize> _s)
    : z(_z), a(_a), b(_b), s(_s) {}
void TyIcmpNeXor::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z), CEREAL_NVP(a), CEREAL_NVP(b), CEREAL_NVP(s));
}
ConsIcmpNeXor::ConsIcmpNeXor(std::shared_ptr<TyIcmpNeXor> _icmp_ne_xor) : icmp_ne_xor(_icmp_ne_xor){
}
std::shared_ptr<TyInfrule> ConsIcmpNeXor::make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s){
  std::shared_ptr<TyIcmpNeXor> _val(new TyIcmpNeXor(_z, _a, _b, _s));
  return std::shared_ptr<TyInfrule>(new ConsIcmpNeXor(_val));
}
void ConsIcmpNeXor::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IcmpNeXor");
  archive(CEREAL_NVP(icmp_ne_xor));
}

TyIcmpNeXorXor::TyIcmpNeXorXor(std::shared_ptr<TyValue> _z,
                               std::shared_ptr<TyValue> _w,
                               std::shared_ptr<TyValue> _x,
                               std::shared_ptr<TyValue> _y,
                               std::shared_ptr<TyValue> _a,
                               std::shared_ptr<TyValue> _b,
                               std::shared_ptr<TySize> _s)
    : z(_z), w(_w), x(_x), y(_y), a(_a), b(_b), s(_s) {}
void TyIcmpNeXorXor::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(w), CEREAL_NVP(x), CEREAL_NVP(y),
          CEREAL_NVP(a), CEREAL_NVP(b), CEREAL_NVP(s));
}

ConsIcmpNeXorXor::ConsIcmpNeXorXor(
    std::shared_ptr<TyIcmpNeXorXor> _icmp_ne_xor_xor)
    : icmp_ne_xor_xor(_icmp_ne_xor_xor) {}
std::shared_ptr<TyInfrule>
ConsIcmpNeXorXor::make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _w,
                       std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
                       std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
                       std::shared_ptr<TySize> _s) {
  std::shared_ptr<TyIcmpNeXorXor> _val(
      new TyIcmpNeXorXor(_z, _w, _x, _y, _a, _b, _s));
  return std::shared_ptr<TyInfrule>(new ConsIcmpNeXorXor(_val));
}
void ConsIcmpNeXorXor::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IcmpNeXorXor");
  archive(CEREAL_NVP(icmp_ne_xor_xor));
}

TyIcmpNeqSame::TyIcmpNeqSame(std::shared_ptr<TyValueType> _ty,
                             std::shared_ptr<TyValue> _x,
                             std::shared_ptr<TyValue> _y)
    : ty(std::move(_ty)), x(std::move(_x)), y(std::move(_y)) {}

void TyIcmpNeqSame::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(ty));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
}

ConsIcmpNeqSame::ConsIcmpNeqSame(std::shared_ptr<TyIcmpNeqSame> _icmp_neq_same)
    : icmp_neq_same(_icmp_neq_same) {}

std::shared_ptr<TyInfrule> ConsIcmpNeqSame::make(llvm::ICmpInst &CI) {
  std::shared_ptr<TyValueType> ty =
      TyValueType::make(*CI.getOperand(0)->getType());
  std::shared_ptr<TyValue> x = TyValue::make(*CI.getOperand(0));
  std::shared_ptr<TyValue> y = TyValue::make(*CI.getOperand(1));

  std::shared_ptr<TyIcmpNeqSame> _icmp_neq_same =
      std::make_shared<TyIcmpNeqSame>(ty, x, y);

  return std::make_shared<ConsIcmpNeqSame>(_icmp_neq_same);
}

void ConsIcmpNeqSame::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IcmpNeqSame");
  archive(CEREAL_NVP(icmp_neq_same));
}

TyIcmpSgeOrNot::TyIcmpSgeOrNot(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s) : z(_z), zprime(_zprime), a(_a), b(_b), s(_s){
}
void TyIcmpSgeOrNot::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z), CEREAL_NVP(zprime), CEREAL_NVP(a), CEREAL_NVP(b), CEREAL_NVP(s));
}
ConsIcmpSgeOrNot::ConsIcmpSgeOrNot(std::shared_ptr<TyIcmpSgeOrNot> _icmp_sge_or_not) : icmp_sge_or_not(_icmp_sge_or_not){
}
std::shared_ptr<TyInfrule> ConsIcmpSgeOrNot::make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s){
  std::shared_ptr<TyIcmpSgeOrNot> _val(new TyIcmpSgeOrNot(_z, _zprime, _a, _b, _s));
  return std::shared_ptr<TyInfrule>(new ConsIcmpSgeOrNot(_val));
}
void ConsIcmpSgeOrNot::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IcmpSgeOrNot");
  archive(CEREAL_NVP(icmp_sge_or_not));
}
TyIcmpSgtAndNot::TyIcmpSgtAndNot(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s) : z(_z), zprime(_zprime), a(_a), b(_b), s(_s){
}
void TyIcmpSgtAndNot::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z), CEREAL_NVP(zprime), CEREAL_NVP(a), CEREAL_NVP(b), CEREAL_NVP(s));
}
ConsIcmpSgtAndNot::ConsIcmpSgtAndNot(std::shared_ptr<TyIcmpSgtAndNot> _icmp_sgt_and_not) : icmp_sgt_and_not(_icmp_sgt_and_not){
}
std::shared_ptr<TyInfrule> ConsIcmpSgtAndNot::make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s){
  std::shared_ptr<TyIcmpSgtAndNot> _val(new TyIcmpSgtAndNot(_z, _zprime, _a, _b, _s));
  return std::shared_ptr<TyInfrule>(new ConsIcmpSgtAndNot(_val));
}
void ConsIcmpSgtAndNot::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IcmpSgtAndNot");
  archive(CEREAL_NVP(icmp_sgt_and_not));
}

TyIcmpSleOrNot::TyIcmpSleOrNot(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s) : z(_z), zprime(_zprime), a(_a), b(_b), s(_s){
}
void TyIcmpSleOrNot::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z), CEREAL_NVP(zprime), CEREAL_NVP(a), CEREAL_NVP(b), CEREAL_NVP(s));
}
ConsIcmpSleOrNot::ConsIcmpSleOrNot(std::shared_ptr<TyIcmpSleOrNot> _icmp_sle_or_not) : icmp_sle_or_not(_icmp_sle_or_not){
}
std::shared_ptr<TyInfrule> ConsIcmpSleOrNot::make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s){
  std::shared_ptr<TyIcmpSleOrNot> _val(new TyIcmpSleOrNot(_z, _zprime, _a, _b, _s));
  return std::shared_ptr<TyInfrule>(new ConsIcmpSleOrNot(_val));
}
void ConsIcmpSleOrNot::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IcmpSleOrNot");
  archive(CEREAL_NVP(icmp_sle_or_not));
}

TyIcmpSltAndNot::TyIcmpSltAndNot(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s) : z(_z), zprime(_zprime), a(_a), b(_b), s(_s){
}
void TyIcmpSltAndNot::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z), CEREAL_NVP(zprime), CEREAL_NVP(a), CEREAL_NVP(b), CEREAL_NVP(s));
}
ConsIcmpSltAndNot::ConsIcmpSltAndNot(std::shared_ptr<TyIcmpSltAndNot> _icmp_slt_and_not) : icmp_slt_and_not(_icmp_slt_and_not){
}
std::shared_ptr<TyInfrule> ConsIcmpSltAndNot::make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s){
  std::shared_ptr<TyIcmpSltAndNot> _val(new TyIcmpSltAndNot(_z, _zprime, _a, _b, _s));
  return std::shared_ptr<TyInfrule>(new ConsIcmpSltAndNot(_val));
}
void ConsIcmpSltAndNot::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IcmpSltAndNot");
  archive(CEREAL_NVP(icmp_slt_and_not));
}

TyIcmpUgeOrNot::TyIcmpUgeOrNot(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s) : z(_z), zprime(_zprime), a(_a), b(_b), s(_s){
}
void TyIcmpUgeOrNot::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z), CEREAL_NVP(zprime), CEREAL_NVP(a), CEREAL_NVP(b), CEREAL_NVP(s));
}
ConsIcmpUgeOrNot::ConsIcmpUgeOrNot(std::shared_ptr<TyIcmpUgeOrNot> _icmp_uge_or_not) : icmp_uge_or_not(_icmp_uge_or_not){
}
std::shared_ptr<TyInfrule> ConsIcmpUgeOrNot::make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s){
  std::shared_ptr<TyIcmpUgeOrNot> _val(new TyIcmpUgeOrNot(_z, _zprime, _a, _b, _s));
  return std::shared_ptr<TyInfrule>(new ConsIcmpUgeOrNot(_val));
}
void ConsIcmpUgeOrNot::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IcmpUgeOrNot");
  archive(CEREAL_NVP(icmp_uge_or_not));
}

TyIcmpUgtAndNot::TyIcmpUgtAndNot(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s) : z(_z), zprime(_zprime), a(_a), b(_b), s(_s){
}
void TyIcmpUgtAndNot::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z), CEREAL_NVP(zprime), CEREAL_NVP(a), CEREAL_NVP(b), CEREAL_NVP(s));
}
ConsIcmpUgtAndNot::ConsIcmpUgtAndNot(std::shared_ptr<TyIcmpUgtAndNot> _icmp_ugt_and_not) : icmp_ugt_and_not(_icmp_ugt_and_not){
}
std::shared_ptr<TyInfrule> ConsIcmpUgtAndNot::make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s){
  std::shared_ptr<TyIcmpUgtAndNot> _val(new TyIcmpUgtAndNot(_z, _zprime, _a, _b, _s));
  return std::shared_ptr<TyInfrule>(new ConsIcmpUgtAndNot(_val));
}
void ConsIcmpUgtAndNot::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IcmpUgtAndNot");
  archive(CEREAL_NVP(icmp_ugt_and_not));
}

TyIcmpUleOrNot::TyIcmpUleOrNot(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s) : z(_z), zprime(_zprime), a(_a), b(_b), s(_s){
}
void TyIcmpUleOrNot::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z), CEREAL_NVP(zprime), CEREAL_NVP(a), CEREAL_NVP(b), CEREAL_NVP(s));
}
ConsIcmpUleOrNot::ConsIcmpUleOrNot(std::shared_ptr<TyIcmpUleOrNot> _icmp_ule_or_not) : icmp_ule_or_not(_icmp_ule_or_not){
}
std::shared_ptr<TyInfrule> ConsIcmpUleOrNot::make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s){
  std::shared_ptr<TyIcmpUleOrNot> _val(new TyIcmpUleOrNot(_z, _zprime, _a, _b, _s));
  return std::shared_ptr<TyInfrule>(new ConsIcmpUleOrNot(_val));
}
void ConsIcmpUleOrNot::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IcmpUleOrNot");
  archive(CEREAL_NVP(icmp_ule_or_not));
}

TyIcmpUltAndNot::TyIcmpUltAndNot(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s) : z(_z), zprime(_zprime), a(_a), b(_b), s(_s){
}
void TyIcmpUltAndNot::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z), CEREAL_NVP(zprime), CEREAL_NVP(a), CEREAL_NVP(b), CEREAL_NVP(s));
}
ConsIcmpUltAndNot::ConsIcmpUltAndNot(std::shared_ptr<TyIcmpUltAndNot> _icmp_ult_and_not) : icmp_ult_and_not(_icmp_ult_and_not){
}
std::shared_ptr<TyInfrule> ConsIcmpUltAndNot::make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s){
  std::shared_ptr<TyIcmpUltAndNot> _val(new TyIcmpUltAndNot(_z, _zprime, _a, _b, _s));
  return std::shared_ptr<TyInfrule>(new ConsIcmpUltAndNot(_val));
}
void ConsIcmpUltAndNot::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IcmpUltAndNot");
  archive(CEREAL_NVP(icmp_ult_and_not));
}

} // llvmberry
