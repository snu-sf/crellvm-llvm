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

TyBopCommutativeRev::TyBopCommutativeRev(std::shared_ptr<TyExpr> _e, TyBop _bop,
                                   std::shared_ptr<TyValue> _x,
                                   std::shared_ptr<TyValue> _y,
                                   std::shared_ptr<TySize> _sz)
    : e(_e), bop(_bop), x(_x), y(_y), sz(_sz) {}

void TyBopCommutativeRev::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(e), cereal::make_nvp("bop", llvmberry::toString(bop)),
          CEREAL_NVP(x), CEREAL_NVP(y), CEREAL_NVP(sz));
}

ConsBopCommutativeRev::ConsBopCommutativeRev(
    std::shared_ptr<TyBopCommutativeRev> _bop_commutative_rev)
    : bop_commutative_rev(_bop_commutative_rev) {}

void ConsBopCommutativeRev::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("BopCommutativeRev");
  archive(CEREAL_NVP(bop_commutative_rev));
}

std::shared_ptr<TyInfrule> ConsBopCommutativeRev::make(
    std::shared_ptr<TyExpr> _e, TyBop _bop,
    std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
    std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyBopCommutativeRev> _bop_comm(
      new TyBopCommutativeRev(_e, _bop, _x, _y, _sz));
  return std::shared_ptr<TyInfrule>(new ConsBopCommutativeRev(_bop_comm));
}

TyBopCommutativeTgt::TyBopCommutativeTgt(std::shared_ptr<TyExpr> _e, TyBop _bop,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TySize> _sz)
    : e(_e), bop(_bop), x(_x), y(_y), sz(_sz) {}

void TyBopCommutativeTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(e), cereal::make_nvp("bop", llvmberry::toString(bop)),
          CEREAL_NVP(x), CEREAL_NVP(y), CEREAL_NVP(sz));
}

ConsBopCommutativeTgt::ConsBopCommutativeTgt(
    std::shared_ptr<TyBopCommutativeTgt> _bop_commutative_tgt)
    : bop_commutative_tgt(_bop_commutative_tgt) {}

void ConsBopCommutativeTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("BopCommutativeTgt");
  archive(CEREAL_NVP(bop_commutative_tgt));
}

std::shared_ptr<TyInfrule> ConsBopCommutativeTgt::make(
    std::shared_ptr<TyExpr> _e, TyBop _bop,
    std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
    std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyBopCommutativeTgt> _bop_comm_tgt(
      new TyBopCommutativeTgt(_e, _bop, _x, _y, _sz));
  return std::shared_ptr<TyInfrule>(new ConsBopCommutativeTgt(_bop_comm_tgt));
}

TyBopCommutativeRevTgt::TyBopCommutativeRevTgt(std::shared_ptr<TyExpr> _e, TyBop _bop,
                                               std::shared_ptr<TyValue> _x,
                                               std::shared_ptr<TyValue> _y,
                                               std::shared_ptr<TySize> _sz)
    : e(_e), bop(_bop), x(_x), y(_y), sz(_sz) {}

void TyBopCommutativeRevTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(e), cereal::make_nvp("bop", llvmberry::toString(bop)),
          CEREAL_NVP(x), CEREAL_NVP(y), CEREAL_NVP(sz));
}

ConsBopCommutativeRevTgt::ConsBopCommutativeRevTgt(
    std::shared_ptr<TyBopCommutativeRevTgt> _bop_commutative_rev_tgt)
    : bop_commutative_rev_tgt(_bop_commutative_rev_tgt) {}

void ConsBopCommutativeRevTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("BopCommutativeRevTgt");
  archive(CEREAL_NVP(bop_commutative_rev_tgt));
}

std::shared_ptr<TyInfrule> ConsBopCommutativeRevTgt::make(
    std::shared_ptr<TyExpr> _e, TyBop _bop,
    std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
    std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyBopCommutativeRevTgt> _bop_comm_rev_tgt(
      new TyBopCommutativeRevTgt(_e, _bop, _x, _y, _sz));
  return std::shared_ptr<TyInfrule>(new ConsBopCommutativeRevTgt(_bop_comm_rev_tgt));
}

TyFbopCommutative::TyFbopCommutative(std::shared_ptr<TyExpr> _e, TyFbop _fbop,
                                     std::shared_ptr<TyValue> _x,
                                     std::shared_ptr<TyValue> _y,
                                     TyFloatType _fty)
    : e(_e), fbop(_fbop), x(_x), y(_y), fty(_fty) {}

void TyFbopCommutative::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(e), cereal::make_nvp("fbop", llvmberry::toString(fbop)),
          CEREAL_NVP(x), CEREAL_NVP(y), cereal::make_nvp("fty", toString(fty)));
}

ConsFbopCommutative::ConsFbopCommutative(
    std::shared_ptr<TyFbopCommutative> _fbop_commutative)
    : fbop_commutative(_fbop_commutative) {}

void ConsFbopCommutative::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("FbopCommutative");
  archive(CEREAL_NVP(fbop_commutative));
}

std::shared_ptr<TyInfrule> ConsFbopCommutative::make(
    std::shared_ptr<TyExpr> _e, TyFbop _fbop,
    std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
    TyFloatType _fty) {
  std::shared_ptr<TyFbopCommutative> _fbop_comm(
      new TyFbopCommutative(_e, _fbop, _x, _y, _fty));
  return std::shared_ptr<TyInfrule>(new ConsFbopCommutative(_fbop_comm));
}

TyFbopCommutativeRev::TyFbopCommutativeRev(std::shared_ptr<TyExpr> _e, TyFbop _fbop,
                                           std::shared_ptr<TyValue> _x,
                                           std::shared_ptr<TyValue> _y,
                                           TyFloatType _fty)
    : e(_e), fbop(_fbop), x(_x), y(_y), fty(_fty) {}

void TyFbopCommutativeRev::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(e), cereal::make_nvp("fbop", llvmberry::toString(fbop)),
          CEREAL_NVP(x), CEREAL_NVP(y), cereal::make_nvp("fty", toString(fty)));
}

ConsFbopCommutativeRev::ConsFbopCommutativeRev(
    std::shared_ptr<TyFbopCommutativeRev> _fbop_commutative_rev)
    : fbop_commutative_rev(_fbop_commutative_rev) {}

void ConsFbopCommutativeRev::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("FbopCommutativeRev");
  archive(CEREAL_NVP(fbop_commutative_rev));
}

std::shared_ptr<TyInfrule> ConsFbopCommutativeRev::make(
    std::shared_ptr<TyExpr> _e, TyFbop _fbop,
    std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
    TyFloatType _fty) {
  std::shared_ptr<TyFbopCommutativeRev> _fbop_comm_rev(
      new TyFbopCommutativeRev(_e, _fbop, _x, _y, _fty));
  return std::shared_ptr<TyInfrule>(new ConsFbopCommutativeRev(_fbop_comm_rev));
}

TyFbopCommutativeTgt::TyFbopCommutativeTgt(std::shared_ptr<TyExpr> _e, TyFbop _fbop,
                                           std::shared_ptr<TyValue> _x,
                                           std::shared_ptr<TyValue> _y,
                                           TyFloatType _fty)
    : e(_e), fbop(_fbop), x(_x), y(_y), fty(_fty) {}

void TyFbopCommutativeTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(e), cereal::make_nvp("fbop", llvmberry::toString(fbop)),
          CEREAL_NVP(x), CEREAL_NVP(y), cereal::make_nvp("fty", toString(fty)));
}

ConsFbopCommutativeTgt::ConsFbopCommutativeTgt(
    std::shared_ptr<TyFbopCommutativeTgt> _fbop_commutative_tgt)
    : fbop_commutative_tgt(_fbop_commutative_tgt) {}

void ConsFbopCommutativeTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("FbopCommutativeTgt");
  archive(CEREAL_NVP(fbop_commutative_tgt));
}

std::shared_ptr<TyInfrule> ConsFbopCommutativeTgt::make(
    std::shared_ptr<TyExpr> _e, TyFbop _fbop,
    std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
    TyFloatType _fty) {
  std::shared_ptr<TyFbopCommutativeTgt> _fbop_comm_tgt(
      new TyFbopCommutativeTgt(_e, _fbop, _x, _y, _fty));
  return std::shared_ptr<TyInfrule>(new ConsFbopCommutativeTgt(_fbop_comm_tgt));
}

TyFbopCommutativeRevTgt::TyFbopCommutativeRevTgt(std::shared_ptr<TyExpr> _e, TyFbop _fbop,
                                                 std::shared_ptr<TyValue> _x,
                                                 std::shared_ptr<TyValue> _y,
                                                 TyFloatType _fty)
    : e(_e), fbop(_fbop), x(_x), y(_y), fty(_fty) {}

void TyFbopCommutativeRevTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(e), cereal::make_nvp("fbop", llvmberry::toString(fbop)),
          CEREAL_NVP(x), CEREAL_NVP(y), cereal::make_nvp("fty", toString(fty)));
}

ConsFbopCommutativeRevTgt::ConsFbopCommutativeRevTgt(
    std::shared_ptr<TyFbopCommutativeRevTgt> _fbop_commutative_rev_tgt)
    : fbop_commutative_rev_tgt(_fbop_commutative_rev_tgt) {}

void ConsFbopCommutativeRevTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("FbopCommutativeRevTgt");
  archive(CEREAL_NVP(fbop_commutative_rev_tgt));
}

std::shared_ptr<TyInfrule> ConsFbopCommutativeRevTgt::make(
    std::shared_ptr<TyExpr> _e, TyFbop _fbop,
    std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
    TyFloatType _fty) {
  std::shared_ptr<TyFbopCommutativeRevTgt> _fbop_comm_rev_tgt(
      new TyFbopCommutativeRevTgt(_e, _fbop, _x, _y, _fty));
  return std::shared_ptr<TyInfrule>(new ConsFbopCommutativeRevTgt(_fbop_comm_rev_tgt));
}

TyBitcastDoubleI64::TyBitcastDoubleI64(std::shared_ptr<TyConstant> _src,
                                       std::shared_ptr<TyConstInt> _tgt)
    : src(_src), tgt(_tgt) {}
void TyBitcastDoubleI64::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(src));
  archive(CEREAL_NVP(tgt));
}
ConsBitcastDoubleI64::ConsBitcastDoubleI64(
    std::shared_ptr<TyBitcastDoubleI64> _bitcast_double_i64)
    : bitcast_double_i64(_bitcast_double_i64) {}
std::shared_ptr<TyInfrule> ConsBitcastDoubleI64::make(
    std::shared_ptr<TyConstant> _src, std::shared_ptr<TyConstInt> _tgt) {
  std::shared_ptr<TyBitcastDoubleI64> _val(
      new TyBitcastDoubleI64(_src, _tgt));
  return std::shared_ptr<TyInfrule>(new ConsBitcastDoubleI64(_val));
}
void ConsBitcastDoubleI64::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("BitcastDoubleI64");
  archive(CEREAL_NVP(bitcast_double_i64));
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

TyAndTrueBoolTgt::TyAndTrueBoolTgt(std::shared_ptr<TyValue> _x,
                                   std::shared_ptr<TyValue> _y)
    : x(std::move(_x)), y(std::move(_y)) {}

void TyAndTrueBoolTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
}

ConsAndTrueBoolTgt::ConsAndTrueBoolTgt(std::shared_ptr<TyAndTrueBoolTgt> _and_true_bool_tgt)
  : and_true_bool_tgt(std::move(_and_true_bool_tgt)) {}

std::shared_ptr<TyInfrule> ConsAndTrueBoolTgt::make(std::shared_ptr<TyValue> _x,
                                                    std::shared_ptr<TyValue> _y) {
  std::shared_ptr<TyAndTrueBoolTgt> _val(
      new TyAndTrueBoolTgt(std::move(_x), std::move(_y)));
  return std::shared_ptr<TyInfrule>(new ConsAndTrueBoolTgt(std::move(_val)));
}

void ConsAndTrueBoolTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("AndTrueBoolTgt");
  archive(CEREAL_NVP(and_true_bool_tgt));
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

ConsDiffblockLoad::ConsDiffblockLoad(
    std::shared_ptr<TyDiffblockLoad> _diffblock_load)
    : diffblock_load(std::move(_diffblock_load)) {}
std::shared_ptr<TyInfrule> ConsDiffblockLoad::make(
    std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _p,
    std::shared_ptr<TyValueType> _ty, std::shared_ptr<TySize> _align,
    std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _a) {
  std::shared_ptr<TyDiffblockLoad> _val(new TyDiffblockLoad(
      std::move(_x), std::move(_p), std::move(_ty), std::move(_align),
      std::move(_y), std::move(_a)));
  return std::shared_ptr<TyInfrule>(new ConsDiffblockLoad(std::move(_val)));
}
void ConsDiffblockLoad::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("DiffblockLoad");
  archive(CEREAL_NVP(diffblock_load));
}

TyDiffblockLoad::TyDiffblockLoad(std::shared_ptr<TyValue> _x,
                                 std::shared_ptr<TyValue> _p,
                                 std::shared_ptr<TyValueType> _ty,
                                 std::shared_ptr<TySize> _align,
                                 std::shared_ptr<TyValue> _y,
                                 std::shared_ptr<TyValue> _a)
  : x(std::move(_x)), p(std::move(_p)), ty(std::move(_ty)),
    align(std::move(_align)), y(std::move(_y)), a(std::move(_a)) {}
void TyDiffblockLoad::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(p));
  archive(CEREAL_NVP(ty));
  archive(CEREAL_NVP(align));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(a));
}

ConsDiffblockGep::ConsDiffblockGep(
    std::shared_ptr<TyDiffblockGep> _diffblock_gep)
    : diffblock_gep(std::move(_diffblock_gep)) {}
std::shared_ptr<TyInfrule> ConsDiffblockGep::make(
    std::shared_ptr<TyValue> _x,
    std::shared_ptr<TyExpr> _gepinst,
    std::shared_ptr<TyValue> _a) {
  std::shared_ptr<TyDiffblockGep> _val(new TyDiffblockGep(
      std::move(_x), std::move(_gepinst), std::move(_a)));
  return std::shared_ptr<TyInfrule>(new ConsDiffblockGep(std::move(_val)));
}
void ConsDiffblockGep::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("DiffblockGep");
  archive(CEREAL_NVP(diffblock_gep));
}

TyDiffblockGep::TyDiffblockGep(std::shared_ptr<TyValue> _x,
                               std::shared_ptr<TyExpr> _gepinst,
                               std::shared_ptr<TyValue> _a)
  : x(std::move(_x)), gepinst(std::move(_gepinst)), a(std::move(_a)) {}
void TyDiffblockGep::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(gepinst));
  archive(CEREAL_NVP(a));
}

ConsDiffblockBitcast::ConsDiffblockBitcast(
    std::shared_ptr<TyDiffblockBitcast> _diffblock_bitcast)
    : diffblock_bitcast(std::move(_diffblock_bitcast)) {}
std::shared_ptr<TyInfrule> ConsDiffblockBitcast::make(
    std::shared_ptr<TyValue> _x,
    std::shared_ptr<TyExpr> _bitcastinst,
    std::shared_ptr<TyValue> _a) {
  std::shared_ptr<TyDiffblockBitcast> _val(new TyDiffblockBitcast(
      std::move(_x), std::move(_bitcastinst), std::move(_a)));
  return std::shared_ptr<TyInfrule>(new ConsDiffblockBitcast(std::move(_val)));
}
void ConsDiffblockBitcast::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("DiffblockBitcast");
  archive(CEREAL_NVP(diffblock_bitcast));
}

TyDiffblockBitcast::TyDiffblockBitcast(std::shared_ptr<TyValue> _x,
                               std::shared_ptr<TyExpr> _bitcastinst,
                               std::shared_ptr<TyValue> _a)
  : x(std::move(_x)), bitcastinst(std::move(_bitcastinst)), a(std::move(_a)) {}
void TyDiffblockBitcast::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(bitcastinst));
  archive(CEREAL_NVP(a));
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

TyFmulCommutativeTgt::TyFmulCommutativeTgt(std::shared_ptr<TyRegister> _z,
                                           std::shared_ptr<TyValue> _x,
                                           std::shared_ptr<TyValue> _y,
                                           TyFloatType _fty)
    : z(_z), x(_x), y(_y), fty(_fty) {}
void TyFmulCommutativeTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(cereal::make_nvp("fty", toString(fty)));
}

ConsFmulCommutativeTgt::ConsFmulCommutativeTgt(
    std::shared_ptr<TyFmulCommutativeTgt> _fmul_commutative_tgt)
    : fmul_commutative_tgt(_fmul_commutative_tgt) {}
std::shared_ptr<TyInfrule>
ConsFmulCommutativeTgt::make(std::shared_ptr<TyRegister> _z,
                             std::shared_ptr<TyValue> _x,
                             std::shared_ptr<TyValue> _y, TyFloatType _fty) {
  std::shared_ptr<TyFmulCommutativeTgt> _val(new TyFmulCommutativeTgt(
      _z, _x, _y, _fty));
  return std::shared_ptr<TyInfrule>(new ConsFmulCommutativeTgt(_val));
}
void
ConsFmulCommutativeTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("FmulCommutativeTgt");
  archive(CEREAL_NVP(fmul_commutative_tgt));
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

void TyLessthanUndefTgt::updateRhs(std::shared_ptr<TyValue> newVal) {
  v = newVal;
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

TyLessthanUndefConstTgt::TyLessthanUndefConstTgt(std::shared_ptr<TyConstant> _c)
    : c(_c) {}

void TyLessthanUndefConstTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(c));
}

ConsLessthanUndefConstTgt::ConsLessthanUndefConstTgt(
    std::shared_ptr<TyLessthanUndefConstTgt> _lessthan_undef_const_tgt)
    : lessthan_undef_const_tgt(_lessthan_undef_const_tgt) {}

std::shared_ptr<TyInfrule>
ConsLessthanUndefConstTgt::make(std::shared_ptr<TyConstant> _c) {
  std::shared_ptr<TyLessthanUndefConstTgt> _val(
      new TyLessthanUndefConstTgt(_c));

  return std::shared_ptr<TyInfrule>(new ConsLessthanUndefConstTgt(_val));
}

void ConsLessthanUndefConstTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("LessthanUndefConstTgt");
  archive(CEREAL_NVP(lessthan_undef_const_tgt));
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

TyOrFalseTgt::TyOrFalseTgt(std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
                           std::shared_ptr<TySize> _sz)
    : x(std::move(_x)), y(std::move(_y)), sz(std::move(_sz)) {}

void TyOrFalseTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(sz));
}

ConsOrFalseTgt::ConsOrFalseTgt(std::shared_ptr<TyOrFalseTgt> _or_false_tgt)
    : or_false_tgt(std::move(_or_false_tgt)) {}

std::shared_ptr<TyInfrule> ConsOrFalseTgt::make(std::shared_ptr<TyValue> _x,
                                                std::shared_ptr<TyValue> _y,
                                                std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyOrFalseTgt> _val(
      new TyOrFalseTgt(std::move(_x), std::move(_y), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsOrFalseTgt(std::move(_val)));
}

void ConsOrFalseTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("OrFalseTgt");
  archive(CEREAL_NVP(or_false_tgt));
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

TyTransitivity::TyTransitivity(std::shared_ptr<TyExpr> _e1,
                               std::shared_ptr<TyExpr> _e2,
                               std::shared_ptr<TyExpr> _e3)
    : e1(_e1), e2(_e2), e3(_e3) {}

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
    : transitivity(_transitivity) {}

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
      new TyTransitivity(_e1, _e2, _e3));
  return std::shared_ptr<TyInfrule>(
      new ConsTransitivity(_transitivity));
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
    : e1(_e1), e2(_e2), e3(_e3) {}

void TyTransitivityTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(e1), CEREAL_NVP(e2), CEREAL_NVP(e3));
}

std::shared_ptr<TyExpr> TyTransitivityTgt::getExpr1() {
  return e1;
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
    : transitivity_tgt(_transitivity_tgt) {}

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
      new TyTransitivityTgt(_e1, _e2, _e3));
  return std::shared_ptr<TyInfrule>(
      new ConsTransitivityTgt(_transitivity_tgt));
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

TyIntroGhost::TyIntroGhost(std::shared_ptr<TyExpr> _x,
                           std::shared_ptr<TyRegister> _g)
    : x(_x), g(_g) {}

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
    : intro_ghost(_intro_ghost) {}

std::shared_ptr<TyInfrule>
ConsIntroGhost::make(std::shared_ptr<TyExpr> _x,
                     std::shared_ptr<TyRegister> _g) {
  std::shared_ptr<TyIntroGhost> _val(
      new TyIntroGhost(_x, _g));
  return std::shared_ptr<TyInfrule>(new ConsIntroGhost(_val));
}

void ConsIntroGhost::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IntroGhost");
  archive(CEREAL_NVP(intro_ghost));
}

TyIntroGhostSrc::TyIntroGhostSrc(std::shared_ptr<TyExpr> _x,
                                 std::shared_ptr<TyRegister> _g)
    : x(_x), g(_g) {}

void TyIntroGhostSrc::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(g));
}

std::shared_ptr<TyExpr> TyIntroGhostSrc::getExpr() { return x; }

std::shared_ptr<TyRegister> TyIntroGhostSrc::getReg() { return g; }

void TyIntroGhostSrc::updateExpr(std::shared_ptr<TyExpr> newExpr) {
  x = newExpr;
}

ConsIntroGhostSrc::ConsIntroGhostSrc(
    std::shared_ptr<TyIntroGhostSrc> _intro_ghost_src)
    : intro_ghost_src(_intro_ghost_src) {}

std::shared_ptr<TyInfrule>
ConsIntroGhostSrc::make(std::shared_ptr<TyExpr> _x,
                        std::shared_ptr<TyRegister> _g) {
  std::shared_ptr<TyIntroGhostSrc> _val(new TyIntroGhostSrc(_x, _g));
  return std::shared_ptr<TyInfrule>(new ConsIntroGhostSrc(_val));
}

void ConsIntroGhostSrc::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IntroGhostSrc");
  archive(CEREAL_NVP(intro_ghost_src));
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
                             std::shared_ptr<TyExpr> _e)
    : predicate(_predicate), ty(_ty), x(_x), y(_y), e(_e) {}

void TyIcmpSwapOperands::serialize(cereal::JSONOutputArchive &archive) const {
  archive(cereal::make_nvp("predicate", toString(predicate)));
  archive(CEREAL_NVP(ty));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(e));
}

ConsIcmpSwapOperands::ConsIcmpSwapOperands(std::shared_ptr<TyIcmpSwapOperands> _icmp_swap_operands)
    : icmp_swap_operands(_icmp_swap_operands) {}

std::shared_ptr<TyInfrule> ConsIcmpSwapOperands::make(llvm::ICmpInst &CI) {
  enum TyIcmpPred pred = getIcmpPred(CI.getPredicate());
  std::shared_ptr<TyValueType> ty =
      TyValueType::make(*CI.getOperand(0)->getType());
  std::shared_ptr<TyValue> x = TyValue::make(*CI.getOperand(0));
  std::shared_ptr<TyValue> y = TyValue::make(*CI.getOperand(1));
  std::shared_ptr<TyExpr> e = TyExpr::make(CI);

  std::shared_ptr<TyIcmpSwapOperands> _icmp_so =
      std::make_shared<TyIcmpSwapOperands>(pred, ty, x, y, e);

  return std::make_shared<ConsIcmpSwapOperands>(_icmp_so);
}

void ConsIcmpSwapOperands::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IcmpSwapOperands");
  archive(CEREAL_NVP(icmp_swap_operands));
}

TyIcmpSwapOperandsRev::TyIcmpSwapOperandsRev(enum TyIcmpPred _predicate,
                                             std::shared_ptr<TyValueType> _ty,
                                             std::shared_ptr<TyValue> _x,
                                             std::shared_ptr<TyValue> _y,
                                             std::shared_ptr<TyExpr> _e)
    : predicate(_predicate), ty(_ty), x(_x), y(_y), e(_e) {}

void TyIcmpSwapOperandsRev::serialize(cereal::JSONOutputArchive &archive) const {
  archive(cereal::make_nvp("predicate", toString(predicate)));
  archive(CEREAL_NVP(ty));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(e));
}

ConsIcmpSwapOperandsRev::ConsIcmpSwapOperandsRev(std::shared_ptr<TyIcmpSwapOperandsRev> _icmp_swap_operands_rev)
    : icmp_swap_operands_rev(_icmp_swap_operands_rev) {}

std::shared_ptr<TyInfrule> ConsIcmpSwapOperandsRev::make(llvm::ICmpInst &CI) {
  enum TyIcmpPred pred = getIcmpPred(CI.getPredicate());
  std::shared_ptr<TyValueType> ty =
      TyValueType::make(*CI.getOperand(0)->getType());
  std::shared_ptr<TyValue> x = TyValue::make(*CI.getOperand(0));
  std::shared_ptr<TyValue> y = TyValue::make(*CI.getOperand(1));
  std::shared_ptr<TyExpr> e = TyExpr::make(CI);

  std::shared_ptr<TyIcmpSwapOperandsRev> _icmp_so =
      std::make_shared<TyIcmpSwapOperandsRev>(pred, ty, x, y, e);

  return std::make_shared<ConsIcmpSwapOperandsRev>(_icmp_so);
}

void ConsIcmpSwapOperandsRev::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IcmpSwapOperandsRev");
  archive(CEREAL_NVP(icmp_swap_operands_rev));
}

TyIcmpSwapOperandsTgt::TyIcmpSwapOperandsTgt(enum TyIcmpPred _predicate,
                                             std::shared_ptr<TyValueType> _ty,
                                             std::shared_ptr<TyValue> _x,
                                             std::shared_ptr<TyValue> _y,
                                             std::shared_ptr<TyExpr> _e)
    : predicate(_predicate), ty(_ty), x(_x), y(_y), e(_e) {}

void TyIcmpSwapOperandsTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive(cereal::make_nvp("predicate", toString(predicate)));
  archive(CEREAL_NVP(ty));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(e));
}

ConsIcmpSwapOperandsTgt::ConsIcmpSwapOperandsTgt(std::shared_ptr<TyIcmpSwapOperandsTgt> _icmp_swap_operands_tgt)
    : icmp_swap_operands_tgt(_icmp_swap_operands_tgt) {}

std::shared_ptr<TyInfrule> ConsIcmpSwapOperandsTgt::make(llvm::ICmpInst &CI) {
  enum TyIcmpPred pred = getIcmpPred(CI.getPredicate());
  std::shared_ptr<TyValueType> ty =
      TyValueType::make(*CI.getOperand(0)->getType());
  std::shared_ptr<TyValue> x = TyValue::make(*CI.getOperand(0));
  std::shared_ptr<TyValue> y = TyValue::make(*CI.getOperand(1));
  std::shared_ptr<TyExpr> e = TyExpr::make(CI);

  std::shared_ptr<TyIcmpSwapOperandsTgt> _icmp_so =
      std::make_shared<TyIcmpSwapOperandsTgt>(pred, ty, x, y, e);

  return std::make_shared<ConsIcmpSwapOperandsTgt>(_icmp_so);
}

void ConsIcmpSwapOperandsTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IcmpSwapOperandsTgt");
  archive(CEREAL_NVP(icmp_swap_operands_tgt));
}

TyIcmpSwapOperandsRevTgt::TyIcmpSwapOperandsRevTgt(enum TyIcmpPred _predicate,
                                                   std::shared_ptr<TyValueType> _ty,
                                                   std::shared_ptr<TyValue> _x,
                                                   std::shared_ptr<TyValue> _y,
                                                   std::shared_ptr<TyExpr> _e)
    : predicate(_predicate), ty(_ty), x(_x), y(_y), e(_e) {}

void TyIcmpSwapOperandsRevTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive(cereal::make_nvp("predicate", toString(predicate)));
  archive(CEREAL_NVP(ty));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(e));
}

ConsIcmpSwapOperandsRevTgt::ConsIcmpSwapOperandsRevTgt(std::shared_ptr<TyIcmpSwapOperandsRevTgt> _icmp_swap_operands_rev_tgt)
    : icmp_swap_operands_rev_tgt(_icmp_swap_operands_rev_tgt) {}

std::shared_ptr<TyInfrule> ConsIcmpSwapOperandsRevTgt::make(llvm::ICmpInst &CI) {
  enum TyIcmpPred pred = getIcmpPred(CI.getPredicate());
  std::shared_ptr<TyValueType> ty =
      TyValueType::make(*CI.getOperand(0)->getType());
  std::shared_ptr<TyValue> x = TyValue::make(*CI.getOperand(0));
  std::shared_ptr<TyValue> y = TyValue::make(*CI.getOperand(1));
  std::shared_ptr<TyExpr> e = TyExpr::make(CI);

  std::shared_ptr<TyIcmpSwapOperandsRevTgt> _icmp_so =
      std::make_shared<TyIcmpSwapOperandsRevTgt>(pred, ty, x, y, e);

  return std::make_shared<ConsIcmpSwapOperandsRevTgt>(_icmp_so);
}

void ConsIcmpSwapOperandsRevTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IcmpSwapOperandsRevTgt");
  archive(CEREAL_NVP(icmp_swap_operands_rev_tgt));
}

TyFcmpSwapOperands::TyFcmpSwapOperands(enum TyFcmpPred _predicate,
                                       TyFloatType _fty,
                                       std::shared_ptr<TyValue> _x,
                                       std::shared_ptr<TyValue> _y,
                                       std::shared_ptr<TyExpr> _e)
    : predicate(_predicate), fty(_fty), x(_x), y(_y), e(_e) {}

void TyFcmpSwapOperands::serialize(cereal::JSONOutputArchive &archive) const {
  archive(cereal::make_nvp("predicate", toString(predicate)));
  archive(cereal::make_nvp("fty", toString(fty)));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(e));
}

ConsFcmpSwapOperands::ConsFcmpSwapOperands(std::shared_ptr<TyFcmpSwapOperands> _fcmp_swap_operands)
    : fcmp_swap_operands(_fcmp_swap_operands) {}

std::shared_ptr<TyInfrule> ConsFcmpSwapOperands::make(llvm::FCmpInst &CI) {
  enum TyFcmpPred pred = getFcmpPred(CI.getPredicate());
  TyFloatType fty = getFloatType(CI.getOperand(0)->getType());
  std::shared_ptr<TyValue> x = TyValue::make(*CI.getOperand(0));
  std::shared_ptr<TyValue> y = TyValue::make(*CI.getOperand(1));
  std::shared_ptr<TyExpr> e = TyExpr::make(CI);

  std::shared_ptr<TyFcmpSwapOperands> _fcmp_so =
      std::make_shared<TyFcmpSwapOperands>(pred, fty, x, y, e);

  return std::make_shared<ConsFcmpSwapOperands>(_fcmp_so);
}

void ConsFcmpSwapOperands::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("FcmpSwapOperands");
  archive(CEREAL_NVP(fcmp_swap_operands));
}

TyFcmpSwapOperandsRev::TyFcmpSwapOperandsRev(enum TyFcmpPred _predicate,
                                             TyFloatType _fty,
                                             std::shared_ptr<TyValue> _x,
                                             std::shared_ptr<TyValue> _y,
                                             std::shared_ptr<TyExpr> _e)
    : predicate(_predicate), fty(_fty), x(_x), y(_y), e(_e) {}

void TyFcmpSwapOperandsRev::serialize(cereal::JSONOutputArchive &archive) const {
  archive(cereal::make_nvp("predicate", toString(predicate)));
  archive(cereal::make_nvp("fty", toString(fty)));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(e));
}

ConsFcmpSwapOperandsRev::ConsFcmpSwapOperandsRev(std::shared_ptr<TyFcmpSwapOperandsRev> _fcmp_swap_operands_rev)
    : fcmp_swap_operands_rev(_fcmp_swap_operands_rev) {}

std::shared_ptr<TyInfrule> ConsFcmpSwapOperandsRev::make(llvm::FCmpInst &CI) {
  enum TyFcmpPred pred = getFcmpPred(CI.getPredicate());
  TyFloatType fty = getFloatType(CI.getOperand(0)->getType());
  std::shared_ptr<TyValue> x = TyValue::make(*CI.getOperand(0));
  std::shared_ptr<TyValue> y = TyValue::make(*CI.getOperand(1));
  std::shared_ptr<TyExpr> e = TyExpr::make(CI);

  std::shared_ptr<TyFcmpSwapOperandsRev> _fcmp_so =
      std::make_shared<TyFcmpSwapOperandsRev>(pred, fty, x, y, e);

  return std::make_shared<ConsFcmpSwapOperandsRev>(_fcmp_so);
}

void ConsFcmpSwapOperandsRev::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("FcmpSwapOperandsRev");
  archive(CEREAL_NVP(fcmp_swap_operands_rev));
}

TyFcmpSwapOperandsTgt::TyFcmpSwapOperandsTgt(enum TyFcmpPred _predicate,
                                             TyFloatType _fty,
                                             std::shared_ptr<TyValue> _x,
                                             std::shared_ptr<TyValue> _y,
                                             std::shared_ptr<TyExpr> _e)
    : predicate(_predicate), fty(_fty), x(_x), y(_y), e(_e) {}

void TyFcmpSwapOperandsTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive(cereal::make_nvp("predicate", toString(predicate)));
  archive(cereal::make_nvp("fty", toString(fty)));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(e));
}

ConsFcmpSwapOperandsTgt::ConsFcmpSwapOperandsTgt(std::shared_ptr<TyFcmpSwapOperandsTgt> _fcmp_swap_operands_tgt)
    : fcmp_swap_operands_tgt(_fcmp_swap_operands_tgt) {}

std::shared_ptr<TyInfrule> ConsFcmpSwapOperandsTgt::make(llvm::FCmpInst &CI) {
  enum TyFcmpPred pred = getFcmpPred(CI.getPredicate());
  TyFloatType fty = getFloatType(CI.getOperand(0)->getType());
  std::shared_ptr<TyValue> x = TyValue::make(*CI.getOperand(0));
  std::shared_ptr<TyValue> y = TyValue::make(*CI.getOperand(1));
  std::shared_ptr<TyExpr> e = TyExpr::make(CI);

  std::shared_ptr<TyFcmpSwapOperandsTgt> _fcmp_so =
      std::make_shared<TyFcmpSwapOperandsTgt>(pred, fty, x, y, e);

  return std::make_shared<ConsFcmpSwapOperandsTgt>(_fcmp_so);
}

void ConsFcmpSwapOperandsTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("FcmpSwapOperandsTgt");
  archive(CEREAL_NVP(fcmp_swap_operands_tgt));
}

TyFcmpSwapOperandsRevTgt::TyFcmpSwapOperandsRevTgt(enum TyFcmpPred _predicate,
                                                   TyFloatType _fty,
                                                   std::shared_ptr<TyValue> _x,
                                                   std::shared_ptr<TyValue> _y,
                                                   std::shared_ptr<TyExpr> _e)
    : predicate(_predicate), fty(_fty), x(_x), y(_y), e(_e) {}

void TyFcmpSwapOperandsRevTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive(cereal::make_nvp("predicate", toString(predicate)));
  archive(cereal::make_nvp("fty", toString(fty)));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(e));
}

ConsFcmpSwapOperandsRevTgt::ConsFcmpSwapOperandsRevTgt(std::shared_ptr<TyFcmpSwapOperandsRevTgt> _fcmp_swap_operands_rev_tgt)
    : fcmp_swap_operands_rev_tgt(_fcmp_swap_operands_rev_tgt) {}

std::shared_ptr<TyInfrule> ConsFcmpSwapOperandsRevTgt::make(llvm::FCmpInst &CI) {
  enum TyFcmpPred pred = getFcmpPred(CI.getPredicate());
  TyFloatType fty = getFloatType(CI.getOperand(0)->getType());
  std::shared_ptr<TyValue> x = TyValue::make(*CI.getOperand(0));
  std::shared_ptr<TyValue> y = TyValue::make(*CI.getOperand(1));
  std::shared_ptr<TyExpr> e = TyExpr::make(CI);

  std::shared_ptr<TyFcmpSwapOperandsRevTgt> _fcmp_so =
      std::make_shared<TyFcmpSwapOperandsRevTgt>(pred, fty, x, y, e);

  return std::make_shared<ConsFcmpSwapOperandsRevTgt>(_fcmp_so);
}

void ConsFcmpSwapOperandsRevTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("FcmpSwapOperandsRevTgt");
  archive(CEREAL_NVP(fcmp_swap_operands_rev_tgt));
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

void ConsIcmpEqSame::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IcmpEqSame");
  archive(CEREAL_NVP(icmp_eq_same));
}

TyIcmpEqSameTgt::TyIcmpEqSameTgt(std::shared_ptr<TyValueType> _ty,
                                 std::shared_ptr<TyValue> _x,
                                 std::shared_ptr<TyValue> _y)
    : ty(std::move(_ty)), x(std::move(_x)), y(std::move(_y)) {}

void TyIcmpEqSameTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(ty));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
}

ConsIcmpEqSameTgt::ConsIcmpEqSameTgt(std::shared_ptr<TyIcmpEqSameTgt> _icmp_eq_same_tgt)
    : icmp_eq_same_tgt(_icmp_eq_same_tgt) {}

std::shared_ptr<TyInfrule> ConsIcmpEqSameTgt::make(llvm::ICmpInst &CI) {
  std::shared_ptr<TyValueType> ty =
      TyValueType::make(*CI.getOperand(0)->getType());
  std::shared_ptr<TyValue> x = TyValue::make(*CI.getOperand(0));
  std::shared_ptr<TyValue> y = TyValue::make(*CI.getOperand(1));
  std::shared_ptr<TyIcmpEqSameTgt> _icmp_eq_same_tgt =
      std::make_shared<TyIcmpEqSameTgt>(ty, x, y);

  return std::make_shared<ConsIcmpEqSameTgt>(_icmp_eq_same_tgt);
}

void ConsIcmpEqSameTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IcmpEqSameTgt");
  archive(CEREAL_NVP(icmp_eq_same_tgt));
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

TyIcmpNeqSameTgt::TyIcmpNeqSameTgt(std::shared_ptr<TyValueType> _ty,
                                   std::shared_ptr<TyValue> _x,
                                   std::shared_ptr<TyValue> _y)
    : ty(std::move(_ty)), x(std::move(_x)), y(std::move(_y)) {}

void TyIcmpNeqSameTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(ty));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
}

ConsIcmpNeqSameTgt::ConsIcmpNeqSameTgt(std::shared_ptr<TyIcmpNeqSameTgt> _icmp_neq_same_tgt)
    : icmp_neq_same_tgt(_icmp_neq_same_tgt) {}

std::shared_ptr<TyInfrule> ConsIcmpNeqSameTgt::make(llvm::ICmpInst &CI) {
  std::shared_ptr<TyValueType> ty =
      TyValueType::make(*CI.getOperand(0)->getType());
  std::shared_ptr<TyValue> x = TyValue::make(*CI.getOperand(0));
  std::shared_ptr<TyValue> y = TyValue::make(*CI.getOperand(1));

  std::shared_ptr<TyIcmpNeqSameTgt> _icmp_neq_same_tgt =
      std::make_shared<TyIcmpNeqSameTgt>(ty, x, y);

  return std::make_shared<ConsIcmpNeqSameTgt>(_icmp_neq_same_tgt);
}

void ConsIcmpNeqSameTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IcmpNeqSameTgt");
  archive(CEREAL_NVP(icmp_neq_same_tgt));
}

} // llvmberry
