#include <string>
#include <cassert>
#include <cereal/archives/json.hpp>
#include "llvm/Crellvm/Structure.h"
#include "llvm/Crellvm/ValidationUnit.h"
#include "llvm/Crellvm/InstCombine/InfrulesAddSub.h"

namespace crellvm {

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

TyAddSub::TyAddSub(std::shared_ptr<TyRegister> _minusy,
                   std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x,
                   std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz)
    : minusy(std::move(_minusy)), z(std::move(_z)), x(std::move(_x)),
      y(std::move(_y)), sz(std::move(_sz)) {}

void TyAddSub::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(minusy), CEREAL_NVP(z), CEREAL_NVP(x), CEREAL_NVP(y),
          CEREAL_NVP(sz));
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

}
