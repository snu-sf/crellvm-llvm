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

namespace llvmberry{

TyAddAssociative::TyAddAssociative(std::shared_ptr<TyRegister> _x,
                                   std::shared_ptr<TyRegister> _y,
                                   std::shared_ptr<TyRegister> _z,
                                   std::shared_ptr<TyConstInt> _c1,
                                   std::shared_ptr<TyConstInt> _c2,
                                   std::shared_ptr<TyConstInt> _c3,
                                   std::shared_ptr<TySize> _sz)
    : x(std::move(_x)), y(std::move(_y)), z(std::move(_z)), c1(std::move(_c1)),
      c2(std::move(_c2)), c3(std::move(_c3)), sz(std::move(_sz)) {}

void TyAddAssociative::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(x), CEREAL_NVP(y), CEREAL_NVP(z), CEREAL_NVP(c1),
          CEREAL_NVP(c2), CEREAL_NVP(c3), CEREAL_NVP(sz));
}

TyAddConstNot::TyAddConstNot(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c1, std::shared_ptr<TyConstInt> _c2, std::shared_ptr<TySize> _sz) : z(std::move(_z)), y(std::move(_y)), x(std::move(_x)), c1(std::move(_c1)), c2(std::move(_c2)), sz(std::move(_sz)){
}
void TyAddConstNot::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(c1));
  archive(CEREAL_NVP(c2));
  archive(CEREAL_NVP(sz));
}

ConsAddConstNot::ConsAddConstNot(std::shared_ptr<TyAddConstNot> _add_const_not) : add_const_not(std::move(_add_const_not)){
}
std::shared_ptr<TyInfrule> ConsAddConstNot::make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c1, std::shared_ptr<TyConstInt> _c2, std::shared_ptr<TySize> _sz){
  std::shared_ptr<TyAddConstNot> _val(new TyAddConstNot(std::move(_z), std::move(_y), std::move(_x), std::move(_c1), std::move(_c2), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsAddConstNot(std::move(_val)));
}
void ConsAddConstNot::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("AddConstNot");
  archive(CEREAL_NVP(add_const_not));
}

TyAddMask::TyAddMask(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyRegister> _yprime, std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c1, std::shared_ptr<TyConstInt> _c2, std::shared_ptr<TySize> _sz) : z(std::move(_z)), y(std::move(_y)), yprime(std::move(_yprime)), x(std::move(_x)), c1(std::move(_c1)), c2(std::move(_c2)), sz(std::move(_sz)){
}
void TyAddMask::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(yprime));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(c1));
  archive(CEREAL_NVP(c2));
  archive(CEREAL_NVP(sz));
}

ConsAddMask::ConsAddMask(std::shared_ptr<TyAddMask> _add_mask) : add_mask(std::move(_add_mask)){
}
std::shared_ptr<TyInfrule> ConsAddMask::make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyRegister> _yprime, std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c1, std::shared_ptr<TyConstInt> _c2, std::shared_ptr<TySize> _sz){
  std::shared_ptr<TyAddMask> _val(new TyAddMask(std::move(_z), std::move(_y), std::move(_yprime), std::move(_x), std::move(_c1), std::move(_c2), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsAddMask(std::move(_val)));
}
void ConsAddMask::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("AddMask");
  archive(CEREAL_NVP(add_mask));
}

TyAddSelectZero::TyAddSelectZero(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _c, std::shared_ptr<TyValue> _n, std::shared_ptr<TyValue> _a, std::shared_ptr<TySize> _sz) : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)), c(std::move(_c)), n(std::move(_n)), a(std::move(_a)), sz(std::move(_sz)){
}
void TyAddSelectZero::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(c));
  archive(CEREAL_NVP(n));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(sz));
}

ConsAddSelectZero::ConsAddSelectZero(std::shared_ptr<TyAddSelectZero> _add_select_zero) : add_select_zero(std::move(_add_select_zero)){
}
std::shared_ptr<TyInfrule> ConsAddSelectZero::make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _c, std::shared_ptr<TyValue> _n, std::shared_ptr<TyValue> _a, std::shared_ptr<TySize> _sz){
  std::shared_ptr<TyAddSelectZero> _val(new TyAddSelectZero(std::move(_z), std::move(_x), std::move(_y), std::move(_c), std::move(_n), std::move(_a), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsAddSelectZero(std::move(_val)));
}
void ConsAddSelectZero::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("AddSelectZero");
  archive(CEREAL_NVP(add_select_zero));
}

TyAddSelectZero2::TyAddSelectZero2(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _c, std::shared_ptr<TyValue> _n, std::shared_ptr<TyValue> _a, std::shared_ptr<TySize> _sz) : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)), c(std::move(_c)), n(std::move(_n)), a(std::move(_a)), sz(std::move(_sz)){
}
void TyAddSelectZero2::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(c));
  archive(CEREAL_NVP(n));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(sz));
}

ConsAddSelectZero2::ConsAddSelectZero2(std::shared_ptr<TyAddSelectZero2> _add_select_zero2) : add_select_zero2(std::move(_add_select_zero2)){
}
std::shared_ptr<TyInfrule> ConsAddSelectZero2::make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _c, std::shared_ptr<TyValue> _n, std::shared_ptr<TyValue> _a, std::shared_ptr<TySize> _sz){
  std::shared_ptr<TyAddSelectZero2> _val(new TyAddSelectZero2(std::move(_z), std::move(_x), std::move(_y), std::move(_c), std::move(_n), std::move(_a), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsAddSelectZero2(std::move(_val)));
}
void ConsAddSelectZero2::serialize(cereal::JSONOutputArchive& archive) const{
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

TyAddCommutative::TyAddCommutative(std::shared_ptr<TyRegister> _z,
                                   std::shared_ptr<TyValue> _x,
                                   std::shared_ptr<TyValue> _y,
                                   std::shared_ptr<TySize> _sz)
    : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)), sz(std::move(_sz)) {
}

void TyAddCommutative::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(x), CEREAL_NVP(y), CEREAL_NVP(sz));
}

ConsAddCommutative::ConsAddCommutative(
    std::shared_ptr<TyAddCommutative> _add_comm)
    : add_commutative(std::move(_add_comm)) {}

void ConsAddCommutative::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("AddCommutative");
  archive(CEREAL_NVP(add_commutative));
}

std::shared_ptr<TyInfrule> ConsAddCommutative::make(
    std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x,
    std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyAddCommutative> _add_comm(new TyAddCommutative(
      std::move(_z), std::move(_x), std::move(_y), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(
      new ConsAddCommutative(std::move(_add_comm)));
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

void ConsAddCommutativeTgt::serialize(cereal::JSONOutputArchive &archive) const {
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


ConsAddAssociative::ConsAddAssociative(
    std::shared_ptr<TyAddAssociative> _add_associative)
    : add_associative(std::move(_add_associative)) {}

void ConsAddAssociative::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("AddAssociative");
  archive(CEREAL_NVP(add_associative));
}

std::shared_ptr<TyInfrule> ConsAddAssociative::make(
    std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y,
    std::shared_ptr<TyRegister> _z, std::shared_ptr<TyConstInt> _c1,
    std::shared_ptr<TyConstInt> _c2, std::shared_ptr<TyConstInt> _c3,
    std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyAddAssociative> _add_assoc(new TyAddAssociative(
      std::move(_x), std::move(_y), std::move(_z), std::move(_c1),
      std::move(_c2), std::move(_c3), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(
      new ConsAddAssociative(std::move(_add_assoc)));
}

TyAddDistSub::TyAddDistSub(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _minusx, std::shared_ptr<TyValue> _minusy, std::shared_ptr<TyRegister> _w, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz) : z(std::move(_z)), minusx(std::move(_minusx)), minusy(std::move(_minusy)), w(std::move(_w)), x(std::move(_x)), y(std::move(_y)), sz(std::move(_sz)){
}
void TyAddDistSub::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(minusx));
  archive(CEREAL_NVP(minusy));
  archive(CEREAL_NVP(w));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(sz));
}

ConsAddDistSub::ConsAddDistSub(std::shared_ptr<TyAddDistSub> _add_dist_sub) : add_dist_sub(std::move(_add_dist_sub)){
}
std::shared_ptr<TyInfrule> ConsAddDistSub::make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _minusx, std::shared_ptr<TyValue> _minusy, std::shared_ptr<TyRegister> _w, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz){
  std::shared_ptr<TyAddDistSub> _val(new TyAddDistSub(std::move(_z), std::move(_minusx), std::move(_minusy), std::move(_w), std::move(_x), std::move(_y), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsAddDistSub(std::move(_val)));
}
void ConsAddDistSub::serialize(cereal::JSONOutputArchive& archive) const{
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

TyAddOnebit::TyAddOnebit(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y) : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)){
}
void TyAddOnebit::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
}

ConsAddOnebit::ConsAddOnebit(std::shared_ptr<TyAddOnebit> _add_onebit) : add_onebit(std::move(_add_onebit)){
}
std::shared_ptr<TyInfrule> ConsAddOnebit::make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y){
  std::shared_ptr<TyAddOnebit> _val(new TyAddOnebit(std::move(_z), std::move(_x), std::move(_y)));
  return std::shared_ptr<TyInfrule>(new ConsAddOnebit(std::move(_val)));
}
void ConsAddOnebit::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("AddOnebit");
  archive(CEREAL_NVP(add_onebit));
}

TyAddOrAnd::TyAddOrAnd(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y, std::shared_ptr<TySize> _sz) : z(std::move(_z)), a(std::move(_a)), b(std::move(_b)), x(std::move(_x)), y(std::move(_y)), sz(std::move(_sz)){
}
void TyAddOrAnd::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(b));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(sz));
}

ConsAddOrAnd::ConsAddOrAnd(std::shared_ptr<TyAddOrAnd> _add_or_and) : add_or_and(std::move(_add_or_and)){
}
std::shared_ptr<TyInfrule> ConsAddOrAnd::make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y, std::shared_ptr<TySize> _sz){
  std::shared_ptr<TyAddOrAnd> _val(new TyAddOrAnd(std::move(_z), std::move(_a), std::move(_b), std::move(_x), std::move(_y), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsAddOrAnd(std::move(_val)));
}
void ConsAddOrAnd::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("AddOrAnd");
  archive(CEREAL_NVP(add_or_and));
}

TyAddSextSext::TyAddSextSext(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _xprime, std::shared_ptr<TyValue> _x, std::shared_ptr<TyRegister> _yprime, std::shared_ptr<TyValue> _y, std::shared_ptr<TyRegister> _zprime, std::shared_ptr<TySize> _from_sz, std::shared_ptr<TySize> _to_sz) : z(std::move(_z)), xprime(std::move(_xprime)), x(std::move(_x)), yprime(std::move(_yprime)), y(std::move(_y)), zprime(std::move(_zprime)), from_sz(std::move(_from_sz)), to_sz(std::move(_to_sz)){
}
void TyAddSextSext::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(xprime));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(yprime));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(zprime));
  archive(CEREAL_NVP(from_sz));
  archive(CEREAL_NVP(to_sz));
}

ConsAddSextSext::ConsAddSextSext(std::shared_ptr<TyAddSextSext> _add_sext_sext) : add_sext_sext(std::move(_add_sext_sext)){
}
std::shared_ptr<TyInfrule> ConsAddSextSext::make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _xprime, std::shared_ptr<TyValue> _x, std::shared_ptr<TyRegister> _yprime, std::shared_ptr<TyValue> _y, std::shared_ptr<TyRegister> _zprime, std::shared_ptr<TySize> _from_sz, std::shared_ptr<TySize> _to_sz){
  std::shared_ptr<TyAddSextSext> _val(new TyAddSextSext(std::move(_z), std::move(_xprime), std::move(_x), std::move(_yprime), std::move(_y), std::move(_zprime), std::move(_from_sz), std::move(_to_sz)));
  return std::shared_ptr<TyInfrule>(new ConsAddSextSext(std::move(_val)));
}
void ConsAddSextSext::serialize(cereal::JSONOutputArchive& archive) const{
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

TyAddXorAnd::TyAddXorAnd(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y, std::shared_ptr<TySize> _sz) : z(std::move(_z)), a(std::move(_a)), b(std::move(_b)), x(std::move(_x)), y(std::move(_y)), sz(std::move(_sz)){
}
void TyAddXorAnd::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(b));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(sz));
}

ConsAddXorAnd::ConsAddXorAnd(std::shared_ptr<TyAddXorAnd> _add_xor_and) : add_xor_and(std::move(_add_xor_and)){
}
std::shared_ptr<TyInfrule> ConsAddXorAnd::make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y, std::shared_ptr<TySize> _sz){
  std::shared_ptr<TyAddXorAnd> _val(new TyAddXorAnd(std::move(_z), std::move(_a), std::move(_b), std::move(_x), std::move(_y), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsAddXorAnd(std::move(_val)));
}
void ConsAddXorAnd::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("AddXorAnd");
  archive(CEREAL_NVP(add_xor_and));
}

TyAddZextBool::TyAddZextBool(std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _b, std::shared_ptr<TyConstInt> _c, std::shared_ptr<TyConstInt> _cprime, std::shared_ptr<TySize> _sz) : x(std::move(_x)), y(std::move(_y)), b(std::move(_b)), c(std::move(_c)), cprime(std::move(_cprime)), sz(std::move(_sz)){
}
void TyAddZextBool::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(b));
  archive(CEREAL_NVP(c));
  archive(CEREAL_NVP(cprime));
  archive(CEREAL_NVP(sz));
}

ConsAddZextBool::ConsAddZextBool(std::shared_ptr<TyAddZextBool> _add_zext_bool) : add_zext_bool(std::move(_add_zext_bool)){
}
std::shared_ptr<TyInfrule> ConsAddZextBool::make(std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _b, std::shared_ptr<TyConstInt> _c, std::shared_ptr<TyConstInt> _cprime, std::shared_ptr<TySize> _sz){
  std::shared_ptr<TyAddZextBool> _val(new TyAddZextBool(std::move(_x), std::move(_y), std::move(_b), std::move(_c), std::move(_cprime), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsAddZextBool(std::move(_val)));
}
void ConsAddZextBool::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("AddZextBool");
  archive(CEREAL_NVP(add_zext_bool));
}

TyAndCommutative::TyAndCommutative(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz) : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)), sz(std::move(_sz)){
}
void TyAndCommutative::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(sz));
}

ConsAndCommutative::ConsAndCommutative(std::shared_ptr<TyAndCommutative> _and_commutative) : and_commutative(std::move(_and_commutative)){
}
std::shared_ptr<TyInfrule> ConsAndCommutative::make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz){
  std::shared_ptr<TyAndCommutative> _val(new TyAndCommutative(std::move(_z), std::move(_x), std::move(_y), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsAndCommutative(std::move(_val)));
}
void ConsAndCommutative::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("AndCommutative");
  archive(CEREAL_NVP(and_commutative));
}

TyAndDeMorgan::TyAndDeMorgan(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyRegister> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz) : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)), zprime(std::move(_zprime)), a(std::move(_a)), b(std::move(_b)), sz(std::move(_sz)){
}
void TyAndDeMorgan::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(zprime));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(b));
  archive(CEREAL_NVP(sz));
}

ConsAndDeMorgan::ConsAndDeMorgan(std::shared_ptr<TyAndDeMorgan> _and_de_morgan) : and_de_morgan(std::move(_and_de_morgan)){
}
std::shared_ptr<TyInfrule> ConsAndDeMorgan::make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyRegister> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz){
  std::shared_ptr<TyAndDeMorgan> _val(new TyAndDeMorgan(std::move(_z), std::move(_x), std::move(_y), std::move(_zprime), std::move(_a), std::move(_b), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsAndDeMorgan(std::move(_val)));
}
void ConsAndDeMorgan::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("AndDeMorgan");
  archive(CEREAL_NVP(and_de_morgan));
}

TyBitcastptr::TyBitcastptr(std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _vprime, std::shared_ptr<TyExpr> _bitcastinst) : v(std::move(_v)), vprime(std::move(_vprime)), bitcastinst(std::move(_bitcastinst)){
}
void TyBitcastptr::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(v));
  archive(CEREAL_NVP(vprime));
  archive(CEREAL_NVP(bitcastinst));
}

ConsBitcastptr::ConsBitcastptr(std::shared_ptr<TyBitcastptr> _bitcastptr) : bitcastptr(std::move(_bitcastptr)){
}
std::shared_ptr<TyInfrule> ConsBitcastptr::make(std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _vprime, std::shared_ptr<TyExpr> _bitcastinst){
  std::shared_ptr<TyBitcastptr> _val(new TyBitcastptr(std::move(_v), std::move(_vprime), std::move(_bitcastinst)));
  return std::shared_ptr<TyInfrule>(new ConsBitcastptr(std::move(_val)));
}
void ConsBitcastptr::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("Bitcastptr");
  archive(CEREAL_NVP(bitcastptr));
}

TyBopDistributiveOverSelectinst::TyBopDistributiveOverSelectinst(TyBop _opcode, std::shared_ptr<TyRegister> _r, std::shared_ptr<TyRegister> _s, std::shared_ptr<TyRegister> _tprime, std::shared_ptr<TyRegister> _t0, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _c, std::shared_ptr<TySize> _bopsz, std::shared_ptr<TyValueType> _selty) : opcode(std::move(_opcode)), r(std::move(_r)), s(std::move(_s)), tprime(std::move(_tprime)), t0(std::move(_t0)), x(std::move(_x)), y(std::move(_y)), z(std::move(_z)), c(std::move(_c)), bopsz(std::move(_bopsz)), selty(std::move(_selty)){
}
void TyBopDistributiveOverSelectinst::serialize(cereal::JSONOutputArchive& archive) const{
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

ConsBopDistributiveOverSelectinst::ConsBopDistributiveOverSelectinst(std::shared_ptr<TyBopDistributiveOverSelectinst> _bop_distributive_over_selectinst) : bop_distributive_over_selectinst(std::move(_bop_distributive_over_selectinst)){
}
std::shared_ptr<TyInfrule> ConsBopDistributiveOverSelectinst::make(TyBop _opcode, std::shared_ptr<TyRegister> _r, std::shared_ptr<TyRegister> _s, std::shared_ptr<TyRegister> _tprime, std::shared_ptr<TyRegister> _t0, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _c, std::shared_ptr<TySize> _bopsz, std::shared_ptr<TyValueType> _selty){
  std::shared_ptr<TyBopDistributiveOverSelectinst> _val(new TyBopDistributiveOverSelectinst(std::move(_opcode), std::move(_r), std::move(_s), std::move(_tprime), std::move(_t0), std::move(_x), std::move(_y), std::move(_z), std::move(_c), std::move(_bopsz), std::move(_selty)));
  return std::shared_ptr<TyInfrule>(new ConsBopDistributiveOverSelectinst(std::move(_val)));
}
void ConsBopDistributiveOverSelectinst::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("BopDistributiveOverSelectinst");
  archive(CEREAL_NVP(bop_distributive_over_selectinst));
}

TyBopDistributiveOverSelectinst2::TyBopDistributiveOverSelectinst2(TyBop _opcode, std::shared_ptr<TyRegister> _r, std::shared_ptr<TyRegister> _s, std::shared_ptr<TyRegister> _tprime, std::shared_ptr<TyRegister> _t0, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _c, std::shared_ptr<TySize> _bopsz, std::shared_ptr<TyValueType> _selty) : opcode(std::move(_opcode)), r(std::move(_r)), s(std::move(_s)), tprime(std::move(_tprime)), t0(std::move(_t0)), x(std::move(_x)), y(std::move(_y)), z(std::move(_z)), c(std::move(_c)), bopsz(std::move(_bopsz)), selty(std::move(_selty)){
}
void TyBopDistributiveOverSelectinst2::serialize(cereal::JSONOutputArchive& archive) const{
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

ConsBopDistributiveOverSelectinst2::ConsBopDistributiveOverSelectinst2(std::shared_ptr<TyBopDistributiveOverSelectinst2> _bop_distributive_over_selectinst2) : bop_distributive_over_selectinst2(std::move(_bop_distributive_over_selectinst2)){
}
std::shared_ptr<TyInfrule> ConsBopDistributiveOverSelectinst2::make(TyBop _opcode, std::shared_ptr<TyRegister> _r, std::shared_ptr<TyRegister> _s, std::shared_ptr<TyRegister> _tprime, std::shared_ptr<TyRegister> _t0, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _c, std::shared_ptr<TySize> _bopsz, std::shared_ptr<TyValueType> _selty){
  std::shared_ptr<TyBopDistributiveOverSelectinst2> _val(new TyBopDistributiveOverSelectinst2(std::move(_opcode), std::move(_r), std::move(_s), std::move(_tprime), std::move(_t0), std::move(_x), std::move(_y), std::move(_z), std::move(_c), std::move(_bopsz), std::move(_selty)));
  return std::shared_ptr<TyInfrule>(new ConsBopDistributiveOverSelectinst2(std::move(_val)));
}
void ConsBopDistributiveOverSelectinst2::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("BopDistributiveOverSelectinst2");
  archive(CEREAL_NVP(bop_distributive_over_selectinst2));
}


TySdivMone::TySdivMone(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TySize> _sz) : z(std::move(_z)), x(std::move(_x)), sz(std::move(_sz)){
}
void TySdivMone::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(sz));
}

ConsSdivMone::ConsSdivMone(std::shared_ptr<TySdivMone> _div_mone) : div_mone(std::move(_div_mone)){
}
std::shared_ptr<TyInfrule> ConsSdivMone::make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TySize> _sz){
  std::shared_ptr<TySdivMone> _val(new TySdivMone(std::move(_z), std::move(_x), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsSdivMone(std::move(_val)));
}
void ConsSdivMone::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SdivMone");
  archive(CEREAL_NVP(div_mone));
}

TyRemNeg::TyRemNeg(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _my, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz) : z(std::move(_z)), my(std::move(_my)), x(std::move(_x)), y(std::move(_y)), sz(std::move(_sz)){
}
void TyRemNeg::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(my));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(sz));
}

ConsRemNeg::ConsRemNeg(std::shared_ptr<TyRemNeg> _rem_neg) : rem_neg(std::move(_rem_neg)){
}
std::shared_ptr<TyInfrule> ConsRemNeg::make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _my, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz){
  std::shared_ptr<TyRemNeg> _val(new TyRemNeg(std::move(_z), std::move(_my), std::move(_x), std::move(_y), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsRemNeg(std::move(_val)));
}
void ConsRemNeg::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("RemNeg");
  archive(CEREAL_NVP(rem_neg));
}

ConsSdivSubSrem::ConsSdivSubSrem(std::shared_ptr<TySdivSubSrem> _div_sub_srem) : div_sub_srem(std::move(_div_sub_srem)){
}
std::shared_ptr<TyInfrule> ConsSdivSubSrem::make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _b, std::shared_ptr<TyRegister> _a, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz){
  std::shared_ptr<TySdivSubSrem> _val(new TySdivSubSrem(std::move(_z), std::move(_b), std::move(_a), std::move(_x), std::move(_y), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsSdivSubSrem(std::move(_val)));
}
void ConsSdivSubSrem::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SdivSubSrem");
  archive(CEREAL_NVP(div_sub_srem));
}

TySdivSubSrem::TySdivSubSrem(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _b, std::shared_ptr<TyRegister> _a, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz) : z(std::move(_z)), b(std::move(_b)), a(std::move(_a)), x(std::move(_x)), y(std::move(_y)), sz(std::move(_sz)){
}
void TySdivSubSrem::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(b));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(sz));
}

ConsUdivSubUrem::ConsUdivSubUrem(std::shared_ptr<TyUdivSubUrem> _div_sub_urem) : div_sub_urem(std::move(_div_sub_urem)){
}
std::shared_ptr<TyInfrule> ConsUdivSubUrem::make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _b, std::shared_ptr<TyRegister> _a, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz){
  std::shared_ptr<TyUdivSubUrem> _val(new TyUdivSubUrem(std::move(_z), std::move(_b), std::move(_a), std::move(_x), std::move(_y), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsUdivSubUrem(std::move(_val)));
}
void ConsUdivSubUrem::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("UdivSubUrem");
  archive(CEREAL_NVP(div_sub_urem));
}

TyUdivSubUrem::TyUdivSubUrem(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _b, std::shared_ptr<TyRegister> _a, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz) : z(std::move(_z)), b(std::move(_b)), a(std::move(_a)), x(std::move(_x)), y(std::move(_y)), sz(std::move(_sz)){
}
void TyUdivSubUrem::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(b));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(sz));
}

TySubAdd::TySubAdd
        (std::shared_ptr<TyRegister> _z,
         std::shared_ptr<TyValue> _my,
         std::shared_ptr<TyRegister> _x,
         std::shared_ptr<TyValue> _y,
         std::shared_ptr<TySize> _sz)
        : z(std::move(_z)), my(std::move(_my)), x(std::move(_x)),
          y(std::move(_y)), sz(std::move(_sz)) {}

void TySubAdd::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(my), CEREAL_NVP(x),
          CEREAL_NVP(y), CEREAL_NVP(sz));
}

ConsSubAdd::ConsSubAdd(std::shared_ptr<TySubAdd> _sub_add)
        : sub_add(std::move(_sub_add)) {}

void ConsSubAdd::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("SubAdd");
  archive(CEREAL_NVP(sub_add));
}

std::shared_ptr<TyInfrule> ConsSubAdd::make
        (std::shared_ptr<TyRegister> _z,
         std::shared_ptr<TyValue> _my,
         std::shared_ptr<TyRegister> _x,
         std::shared_ptr<TyValue> _y,
         std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TySubAdd> _sub_add
          (new TySubAdd
                   (std::move(_z), std::move(_my), std::move(_x),
                    std::move(_y), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsSubAdd(std::move(_sub_add)));
}

TySubMone::TySubMone(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TySize> _sz) : z(std::move(_z)), x(std::move(_x)), sz(std::move(_sz)){
}
void TySubMone::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(sz));
}

ConsSubMone::ConsSubMone(std::shared_ptr<TySubMone> _sub_mone) : sub_mone(std::move(_sub_mone)){
}
std::shared_ptr<TyInfrule> ConsSubMone::make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TySize> _sz){
  std::shared_ptr<TySubMone> _val(new TySubMone(std::move(_z), std::move(_x), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsSubMone(std::move(_val)));
}
void ConsSubMone::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SubMone");
  archive(CEREAL_NVP(sub_mone));
}

TySubOnebit::TySubOnebit(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y) : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)){
}
void TySubOnebit::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
}

ConsSubOnebit::ConsSubOnebit(std::shared_ptr<TySubOnebit> _sub_onebit) : sub_onebit(std::move(_sub_onebit)){
}
std::shared_ptr<TyInfrule> ConsSubOnebit::make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y){
  std::shared_ptr<TySubOnebit> _val(new TySubOnebit(std::move(_z), std::move(_x), std::move(_y)));
  return std::shared_ptr<TyInfrule>(new ConsSubOnebit(std::move(_val)));
}
void ConsSubOnebit::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SubOnebit");
  archive(CEREAL_NVP(sub_onebit));
}

TySubConstNot::TySubConstNot(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c1, std::shared_ptr<TyConstInt> _c2, std::shared_ptr<TySize> _sz) : z(std::move(_z)), y(std::move(_y)), x(std::move(_x)), c1(std::move(_c1)), c2(std::move(_c2)), sz(std::move(_sz)){
}
void TySubConstNot::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(c1));
  archive(CEREAL_NVP(c2));
  archive(CEREAL_NVP(sz));
}

ConsSubConstNot::ConsSubConstNot(std::shared_ptr<TySubConstNot> _sub_const_not) : sub_const_not(std::move(_sub_const_not)){
}
std::shared_ptr<TyInfrule> ConsSubConstNot::make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c1, std::shared_ptr<TyConstInt> _c2, std::shared_ptr<TySize> _sz){
  std::shared_ptr<TySubConstNot> _val(new TySubConstNot(std::move(_z), std::move(_y), std::move(_x), std::move(_c1), std::move(_c2), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsSubConstNot(std::move(_val)));
}
void ConsSubConstNot::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SubConstNot");
  archive(CEREAL_NVP(sub_const_not));
}

TySubConstAdd::TySubConstAdd(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c1, std::shared_ptr<TyConstInt> _c2, std::shared_ptr<TyConstInt> _c3, std::shared_ptr<TySize> _sz) : z(std::move(_z)), y(std::move(_y)), x(std::move(_x)), c1(std::move(_c1)), c2(std::move(_c2)), c3(std::move(_c3)), sz(std::move(_sz)){
}
void TySubConstAdd::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(c1));
  archive(CEREAL_NVP(c2));
  archive(CEREAL_NVP(c3));
  archive(CEREAL_NVP(sz));
}

ConsSubConstAdd::ConsSubConstAdd(std::shared_ptr<TySubConstAdd> _sub_const_add) : sub_const_add(std::move(_sub_const_add)){
}
std::shared_ptr<TyInfrule> ConsSubConstAdd::make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c1, std::shared_ptr<TyConstInt> _c2, std::shared_ptr<TyConstInt> _c3, std::shared_ptr<TySize> _sz){
  std::shared_ptr<TySubConstAdd> _val(new TySubConstAdd(std::move(_z), std::move(_y), std::move(_x), std::move(_c1), std::move(_c2), std::move(_c3), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsSubConstAdd(std::move(_val)));
}
void ConsSubConstAdd::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SubConstAdd");
  archive(CEREAL_NVP(sub_const_add));
}

TySubOrXor::TySubOrXor(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y, std::shared_ptr<TySize> _sz) : z(std::move(_z)), a(std::move(_a)), b(std::move(_b)), x(std::move(_x)), y(std::move(_y)), sz(std::move(_sz)){
}
void TySubOrXor::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(b));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(sz));
}

ConsSubOrXor::ConsSubOrXor(std::shared_ptr<TySubOrXor> _sub_or_xor) : sub_or_xor(std::move(_sub_or_xor)){
}
std::shared_ptr<TyInfrule> ConsSubOrXor::make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y, std::shared_ptr<TySize> _sz){
  std::shared_ptr<TySubOrXor> _val(new TySubOrXor(std::move(_z), std::move(_a), std::move(_b), std::move(_x), std::move(_y), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsSubOrXor(std::move(_val)));
}
void ConsSubOrXor::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SubOrXor");
  archive(CEREAL_NVP(sub_or_xor));
}

TySubSdiv::TySubSdiv(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c, std::shared_ptr<TyConstInt> _cprime, std::shared_ptr<TySize> _sz) : z(std::move(_z)), y(std::move(_y)), x(std::move(_x)), c(std::move(_c)), cprime(std::move(_cprime)), sz(std::move(_sz)){
}
void TySubSdiv::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(c));
  archive(CEREAL_NVP(cprime));
  archive(CEREAL_NVP(sz));
}

ConsSubSdiv::ConsSubSdiv(std::shared_ptr<TySubSdiv> _sub_sdiv) : sub_sdiv(std::move(_sub_sdiv)){
}
std::shared_ptr<TyInfrule> ConsSubSdiv::make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c, std::shared_ptr<TyConstInt> _cprime, std::shared_ptr<TySize> _sz){
  std::shared_ptr<TySubSdiv> _val(new TySubSdiv(std::move(_z), std::move(_y), std::move(_x), std::move(_c), std::move(_cprime), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsSubSdiv(std::move(_val)));
}
void ConsSubSdiv::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SubSdiv");
  archive(CEREAL_NVP(sub_sdiv));
}

TySubShl::TySubShl(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _mx, std::shared_ptr<TyValue> _a, std::shared_ptr<TySize> _sz) : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)), mx(std::move(_mx)), a(std::move(_a)), sz(std::move(_sz)){
}
void TySubShl::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(mx));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(sz));
}

ConsSubShl::ConsSubShl(std::shared_ptr<TySubShl> _sub_shl) : sub_shl(std::move(_sub_shl)){
}
std::shared_ptr<TyInfrule> ConsSubShl::make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _mx, std::shared_ptr<TyValue> _a, std::shared_ptr<TySize> _sz){
  std::shared_ptr<TySubShl> _val(new TySubShl(std::move(_z), std::move(_x), std::move(_y), std::move(_mx), std::move(_a), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsSubShl(std::move(_val)));
}
void ConsSubShl::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SubShl");
  archive(CEREAL_NVP(sub_shl));
}

TyGepzero::TyGepzero(std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _vprime, std::shared_ptr<TyExpr> _gepinst) : v(std::move(_v)), vprime(std::move(_vprime)), gepinst(std::move(_gepinst)){
}
void TyGepzero::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(v));
  archive(CEREAL_NVP(vprime));
  archive(CEREAL_NVP(gepinst));
}

ConsGepzero::ConsGepzero(std::shared_ptr<TyGepzero> _gepzero) : gepzero(std::move(_gepzero)){
}
std::shared_ptr<TyInfrule> ConsGepzero::make(std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _vprime, std::shared_ptr<TyExpr> _gepinst){
  std::shared_ptr<TyGepzero> _val(new TyGepzero(std::move(_v), std::move(_vprime), std::move(_gepinst)));
  return std::shared_ptr<TyInfrule>(new ConsGepzero(std::move(_val)));
}
void ConsGepzero::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("Gepzero");
  archive(CEREAL_NVP(gepzero));
}


TySubSub::TySubSub(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _w, std::shared_ptr<TySize> _sz) : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)), w(std::move(_w)), sz(std::move(_sz)){
}
void TySubSub::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(w));
  archive(CEREAL_NVP(sz));
}

ConsSubSub::ConsSubSub(std::shared_ptr<TySubSub> _sub_sub) : sub_sub(std::move(_sub_sub)){
}
std::shared_ptr<TyInfrule> ConsSubSub::make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _w, std::shared_ptr<TySize> _sz){
  std::shared_ptr<TySubSub> _val(new TySubSub(std::move(_z), std::move(_x), std::move(_y), std::move(_w), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsSubSub(std::move(_val)));
}
void ConsSubSub::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SubSub");
  archive(CEREAL_NVP(sub_sub));
}

TyFaddCommutativeTgt::TyFaddCommutativeTgt(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, TyFloatType _fty) : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)), fty(_fty){
}
void TyFaddCommutativeTgt::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(cereal::make_nvp("fty", toString(fty)));
}

ConsFaddCommutativeTgt::ConsFaddCommutativeTgt(std::shared_ptr<TyFaddCommutativeTgt> _fadd_commutative_tgt) : fadd_commutative_tgt(std::move(_fadd_commutative_tgt)){
}
std::shared_ptr<TyInfrule> ConsFaddCommutativeTgt::make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, TyFloatType _fty){
  std::shared_ptr<TyFaddCommutativeTgt> _val(new TyFaddCommutativeTgt(std::move(_z), std::move(_x), std::move(_y), std::move(_fty)));
  return std::shared_ptr<TyInfrule>(new ConsFaddCommutativeTgt(std::move(_val)));
}
void ConsFaddCommutativeTgt::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("FaddCommutativeTgt");
  archive(CEREAL_NVP(fadd_commutative_tgt));
}

TyFbopDistributiveOverSelectinst::TyFbopDistributiveOverSelectinst(TyFbop _fopcode, std::shared_ptr<TyRegister> _r, std::shared_ptr<TyRegister> _s, std::shared_ptr<TyRegister> _tprime, std::shared_ptr<TyRegister> _t0, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _c, TyFloatType _fbopty, std::shared_ptr<TyValueType> _selty) : fopcode(std::move(_fopcode)), r(std::move(_r)), s(std::move(_s)), tprime(std::move(_tprime)), t0(std::move(_t0)), x(std::move(_x)), y(std::move(_y)), z(std::move(_z)), c(std::move(_c)), fbopty(std::move(_fbopty)), selty(std::move(_selty)){
}
void TyFbopDistributiveOverSelectinst::serialize(cereal::JSONOutputArchive& archive) const{
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

ConsFbopDistributiveOverSelectinst::ConsFbopDistributiveOverSelectinst(std::shared_ptr<TyFbopDistributiveOverSelectinst> _fbop_distributive_over_selectinst) : fbop_distributive_over_selectinst(std::move(_fbop_distributive_over_selectinst)){
}
std::shared_ptr<TyInfrule> ConsFbopDistributiveOverSelectinst::make(TyFbop _fopcode, std::shared_ptr<TyRegister> _r, std::shared_ptr<TyRegister> _s, std::shared_ptr<TyRegister> _tprime, std::shared_ptr<TyRegister> _t0, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _c, TyFloatType _fbopty, std::shared_ptr<TyValueType> _selty){
  std::shared_ptr<TyFbopDistributiveOverSelectinst> _val(new TyFbopDistributiveOverSelectinst(std::move(_fopcode), std::move(_r), std::move(_s), std::move(_tprime), std::move(_t0), std::move(_x), std::move(_y), std::move(_z), std::move(_c), std::move(_fbopty), std::move(_selty)));
  return std::shared_ptr<TyInfrule>(new ConsFbopDistributiveOverSelectinst(std::move(_val)));
}
void ConsFbopDistributiveOverSelectinst::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("FbopDistributiveOverSelectinst");
  archive(CEREAL_NVP(fbop_distributive_over_selectinst));
}

TyFbopDistributiveOverSelectinst2::TyFbopDistributiveOverSelectinst2(TyFbop _fopcode, std::shared_ptr<TyRegister> _r, std::shared_ptr<TyRegister> _s, std::shared_ptr<TyRegister> _tprime, std::shared_ptr<TyRegister> _t0, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _c, TyFloatType _fbopty, std::shared_ptr<TyValueType> _selty) : fopcode(std::move(_fopcode)), r(std::move(_r)), s(std::move(_s)), tprime(std::move(_tprime)), t0(std::move(_t0)), x(std::move(_x)), y(std::move(_y)), z(std::move(_z)), c(std::move(_c)), fbopty(std::move(_fbopty)), selty(std::move(_selty)){
}
void TyFbopDistributiveOverSelectinst2::serialize(cereal::JSONOutputArchive& archive) const{
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

ConsFbopDistributiveOverSelectinst2::ConsFbopDistributiveOverSelectinst2(std::shared_ptr<TyFbopDistributiveOverSelectinst2> _fbop_distributive_over_selectinst2) : fbop_distributive_over_selectinst2(std::move(_fbop_distributive_over_selectinst2)){
}
std::shared_ptr<TyInfrule> ConsFbopDistributiveOverSelectinst2::make(TyFbop _fopcode, std::shared_ptr<TyRegister> _r, std::shared_ptr<TyRegister> _s, std::shared_ptr<TyRegister> _tprime, std::shared_ptr<TyRegister> _t0, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _c, TyFloatType _fbopty, std::shared_ptr<TyValueType> _selty){
  std::shared_ptr<TyFbopDistributiveOverSelectinst2> _val(new TyFbopDistributiveOverSelectinst2(std::move(_fopcode), std::move(_r), std::move(_s), std::move(_tprime), std::move(_t0), std::move(_x), std::move(_y), std::move(_z), std::move(_c), std::move(_fbopty), std::move(_selty)));
  return std::shared_ptr<TyInfrule>(new ConsFbopDistributiveOverSelectinst2(std::move(_val)));
}
void ConsFbopDistributiveOverSelectinst2::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("FbopDistributiveOverSelectinst2");
  archive(CEREAL_NVP(fbop_distributive_over_selectinst2));
}


TyMulCommutative::TyMulCommutative(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz) : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)), sz(std::move(_sz)){
}
void TyMulCommutative::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(sz));
}

ConsMulCommutative::ConsMulCommutative(std::shared_ptr<TyMulCommutative> _mul_commutative) : mul_commutative(std::move(_mul_commutative)){
}
std::shared_ptr<TyInfrule> ConsMulCommutative::make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz){
  std::shared_ptr<TyMulCommutative> _val(new TyMulCommutative(std::move(_z), std::move(_x), std::move(_y), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsMulCommutative(std::move(_val)));
}
void ConsMulCommutative::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("MulCommutative");
  archive(CEREAL_NVP(mul_commutative));
}

TyMulNeg::TyMulNeg
        (std::shared_ptr<TyRegister> _z,
         std::shared_ptr<TyValue> _mx,
         std::shared_ptr<TyValue> _my,
         std::shared_ptr<TyValue> _x,
         std::shared_ptr<TyValue> _y,
         std::shared_ptr<TySize> _sz)
        : z(std::move(_z)), mx(std::move(_mx)), my(std::move(_my)), x(std::move(_x)),
          y(std::move(_y)), sz(std::move(_sz)) {}

void TyMulNeg::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(mx), CEREAL_NVP(my),
          CEREAL_NVP(x), CEREAL_NVP(y), CEREAL_NVP(sz));
}

ConsMulNeg::ConsMulNeg(std::shared_ptr<TyMulNeg> _mul_neg)
        : mul_neg(std::move(_mul_neg)) {}

void ConsMulNeg::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("MulNeg");
  archive(CEREAL_NVP(mul_neg));
}

std::shared_ptr<TyInfrule> ConsMulNeg::make
        (std::shared_ptr<TyRegister> _z,
         std::shared_ptr<TyValue> _mx,
         std::shared_ptr<TyValue> _my,
         std::shared_ptr<TyValue> _x,
         std::shared_ptr<TyValue> _y,
         std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyMulNeg> _mul_neg
          (new TyMulNeg
                   (std::move(_z), std::move(_mx), std::move(_my),
                    std::move(_x), std::move(_y), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsMulNeg(std::move(_mul_neg)));
}

TyMulMone::TyMulMone(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TySize> _sz) : z(std::move(_z)), x(std::move(_x)), sz(std::move(_sz)){
}
void TyMulMone::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(sz));
}

ConsMulMone::ConsMulMone(std::shared_ptr<TyMulMone> _mul_mone) : mul_mone(std::move(_mul_mone)){
}
std::shared_ptr<TyInfrule> ConsMulMone::make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TySize> _sz){
  std::shared_ptr<TyMulMone> _val(new TyMulMone(std::move(_z), std::move(_x), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsMulMone(std::move(_val)));
}
void ConsMulMone::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("MulMone");
  archive(CEREAL_NVP(mul_mone));
}

TyNegVal::TyNegVal
          (std::shared_ptr<TyConstInt> _c1,
           std::shared_ptr<TyConstInt> _c2,
           std::shared_ptr<TySize> _sz)
           : c1(std::move(_c1)), c2(std::move(_c2)),sz(std::move(_sz)) {}

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

std::shared_ptr<TyInfrule> ConsNegVal::make
  (std::shared_ptr<TyConstInt> _c1,
   std::shared_ptr<TyConstInt> _c2,
   std::shared_ptr<TySize> _sz) {
  std::shared_ptr<TyNegVal> _neg_val(new TyNegVal
   (std::move(_c1), std::move(_c2), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsNegVal(std::move(_neg_val)));
}

TyNoaliasGlobalAlloca::TyNoaliasGlobalAlloca(std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y) : x(std::move(_x)), y(std::move(_y)){
}
void TyNoaliasGlobalAlloca::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
}

ConsNoaliasGlobalAlloca::ConsNoaliasGlobalAlloca(std::shared_ptr<TyNoaliasGlobalAlloca> _noalias_global_alloca) : noalias_global_alloca(std::move(_noalias_global_alloca)){
}
std::shared_ptr<TyInfrule> ConsNoaliasGlobalAlloca::make(std::shared_ptr<TyRegister> _x, 
        std::shared_ptr<TyRegister> _y){
  std::shared_ptr<TyNoaliasGlobalAlloca> _val(new TyNoaliasGlobalAlloca(std::move(_x), std::move(_y)));
  return std::shared_ptr<TyInfrule>(new ConsNoaliasGlobalAlloca(std::move(_val)));
}
void ConsNoaliasGlobalAlloca::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("NoaliasGlobalAlloca");
  archive(CEREAL_NVP(noalias_global_alloca));
}

TyNoaliasGlobalGlobal::TyNoaliasGlobalGlobal(std::shared_ptr<TyRegister> _x, 
        std::shared_ptr<TyRegister> _y) : x(std::move(_x)), y(std::move(_y)){
}
void TyNoaliasGlobalGlobal::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
}

ConsNoaliasGlobalGlobal::ConsNoaliasGlobalGlobal(std::shared_ptr<TyNoaliasGlobalGlobal> _noalias_global_global) : noalias_global_global(std::move(_noalias_global_global)){
}
std::shared_ptr<TyInfrule> ConsNoaliasGlobalGlobal::make(std::shared_ptr<TyRegister> _x, 
        std::shared_ptr<TyRegister> _y){
  std::shared_ptr<TyNoaliasGlobalGlobal> _val(new TyNoaliasGlobalGlobal(std::move(_x), std::move(_y)));
  return std::shared_ptr<TyInfrule>(new ConsNoaliasGlobalGlobal(std::move(_val)));
}
void ConsNoaliasGlobalGlobal::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("NoaliasGlobalGlobal");
  archive(CEREAL_NVP(noalias_global_global));
}

TyNoaliasLessthan::TyNoaliasLessthan(std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _xprime, std::shared_ptr<TyValue> _yprime) : x(std::move(_x)), y(std::move(_y)), xprime(std::move(_xprime)), yprime(std::move(_yprime)){
}
void TyNoaliasLessthan::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(xprime));
  archive(CEREAL_NVP(yprime));
}

ConsNoaliasLessthan::ConsNoaliasLessthan(std::shared_ptr<TyNoaliasLessthan> _noalias_lessthan) : noalias_lessthan(std::move(_noalias_lessthan)){
}
std::shared_ptr<TyInfrule> ConsNoaliasLessthan::make(std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _xprime, std::shared_ptr<TyValue> _yprime){
  std::shared_ptr<TyNoaliasLessthan> _val(new TyNoaliasLessthan(std::move(_x), std::move(_y), std::move(_xprime), std::move(_yprime)));
  return std::shared_ptr<TyInfrule>(new ConsNoaliasLessthan(std::move(_val)));
}
void ConsNoaliasLessthan::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("NoaliasLessthan");
  archive(CEREAL_NVP(noalias_lessthan));
}

TyOrCommutative::TyOrCommutative(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz) : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)), sz(std::move(_sz)){
}
void TyOrCommutative::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(sz));
}

ConsOrCommutative::ConsOrCommutative(std::shared_ptr<TyOrCommutative> _or_commutative) : or_commutative(std::move(_or_commutative)){
}
std::shared_ptr<TyInfrule> ConsOrCommutative::make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz){
  std::shared_ptr<TyOrCommutative> _val(new TyOrCommutative(std::move(_z), std::move(_x), std::move(_y), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsOrCommutative(std::move(_val)));
}
void ConsOrCommutative::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("OrCommutative");
  archive(CEREAL_NVP(or_commutative));
}

TyOrOr::TyOrOr(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz) : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)), a(std::move(_a)), b(std::move(_b)), sz(std::move(_sz)){
}
void TyOrOr::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(b));
  archive(CEREAL_NVP(sz));
}

ConsOrOr::ConsOrOr(std::shared_ptr<TyOrOr> _or_or) : or_or(std::move(_or_or)){
}
std::shared_ptr<TyInfrule> ConsOrOr::make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz){
  std::shared_ptr<TyOrOr> _val(new TyOrOr(std::move(_z), std::move(_x), std::move(_y), std::move(_a), std::move(_b), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsOrOr(std::move(_val)));
}
void ConsOrOr::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("OrOr");
  archive(CEREAL_NVP(or_or));
}

TyOrOr2::TyOrOr2(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _yprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz) : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)), yprime(std::move(_yprime)), a(std::move(_a)), b(std::move(_b)), sz(std::move(_sz)){
}
void TyOrOr2::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(yprime));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(b));
  archive(CEREAL_NVP(sz));
}

ConsOrOr2::ConsOrOr2(std::shared_ptr<TyOrOr2> _or_or2) : or_or2(std::move(_or_or2)){
}
std::shared_ptr<TyInfrule> ConsOrOr2::make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _yprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz){
  std::shared_ptr<TyOrOr2> _val(new TyOrOr2(std::move(_z), std::move(_x), std::move(_y), std::move(_yprime), std::move(_a), std::move(_b), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsOrOr2(std::move(_val)));
}
void ConsOrOr2::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("OrOr2");
  archive(CEREAL_NVP(or_or2));
}

TyOrXor::TyOrXor(std::shared_ptr<TyValue> _w, std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz) : w(std::move(_w)), z(std::move(_z)), x(std::move(_x)), y(std::move(_y)), a(std::move(_a)), b(std::move(_b)), sz(std::move(_sz)){
}
void TyOrXor::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(w));
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(b));
  archive(CEREAL_NVP(sz));
}

ConsOrXor::ConsOrXor(std::shared_ptr<TyOrXor> _or_xor) : or_xor(std::move(_or_xor)){
}
std::shared_ptr<TyInfrule> ConsOrXor::make(std::shared_ptr<TyValue> _w, std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz){
  std::shared_ptr<TyOrXor> _val(new TyOrXor(std::move(_w), std::move(_z), std::move(_x), std::move(_y), std::move(_a), std::move(_b), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsOrXor(std::move(_val)));
}
void ConsOrXor::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("OrXor");
  archive(CEREAL_NVP(or_xor));
}

TyOrXor2::TyOrXor2(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x1, std::shared_ptr<TyValue> _y1, std::shared_ptr<TyValue> _x2, std::shared_ptr<TyValue> _y2, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz) : z(std::move(_z)), x1(std::move(_x1)), y1(std::move(_y1)), x2(std::move(_x2)), y2(std::move(_y2)), a(std::move(_a)), b(std::move(_b)), sz(std::move(_sz)){
}
void TyOrXor2::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x1));
  archive(CEREAL_NVP(y1));
  archive(CEREAL_NVP(x2));
  archive(CEREAL_NVP(y2));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(b));
  archive(CEREAL_NVP(sz));
}


ConsOrXor2::ConsOrXor2(std::shared_ptr<TyOrXor2> _or_xor2) : or_xor2(std::move(_or_xor2)){
}
std::shared_ptr<TyInfrule> ConsOrXor2::make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x1, std::shared_ptr<TyValue> _y1, std::shared_ptr<TyValue> _x2, std::shared_ptr<TyValue> _y2, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz){
  std::shared_ptr<TyOrXor2> _val(new TyOrXor2(std::move(_z), std::move(_x1), std::move(_y1), std::move(_x2), std::move(_y2), std::move(_a), std::move(_b), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsOrXor2(std::move(_val)));
}
void ConsOrXor2::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("OrXor2");
  archive(CEREAL_NVP(or_xor2));
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

std::shared_ptr<TyInfrule> ConsSubRemove::make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz){
  std::shared_ptr<TySubRemove> _val(new TySubRemove(std::move(_z), std::move(_y), std::move(_a), std::move(_b), std::move(_sz)));
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

TyMulShl::TyMulShl(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _a, std::shared_ptr<TySize> _sz) : z(std::move(_z)), y(std::move(_y)), x(std::move(_x)), a(std::move(_a)), sz(std::move(_sz)){
}
void TyMulShl::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(sz));
}

ConsMulShl::ConsMulShl(std::shared_ptr<TyMulShl> _mul_shl) : mul_shl(std::move(_mul_shl)){
}
std::shared_ptr<TyInfrule> ConsMulShl::make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _a, std::shared_ptr<TySize> _sz){
  std::shared_ptr<TyMulShl> _val(new TyMulShl(std::move(_z), std::move(_y), std::move(_x), std::move(_a), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsMulShl(std::move(_val)));
}
void ConsMulShl::serialize(cereal::JSONOutputArchive& archive) const{
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

TyTransitivityPointerLhs::TyTransitivityPointerLhs(std::shared_ptr<TyValue> _p, std::shared_ptr<TyValue> _q, std::shared_ptr<TyValue> _v, std::shared_ptr<TyExpr> _loadq) : p(std::move(_p)), q(std::move(_q)), v(std::move(_v)), loadq(std::move(_loadq)){
}
void TyTransitivityPointerLhs::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(p));
  archive(CEREAL_NVP(q));
  archive(CEREAL_NVP(v));
  archive(CEREAL_NVP(loadq));
}

ConsTransitivityPointerLhs::ConsTransitivityPointerLhs(std::shared_ptr<TyTransitivityPointerLhs> _transitivity_pointer_lhs) : transitivity_pointer_lhs(std::move(_transitivity_pointer_lhs)){
}
std::shared_ptr<TyInfrule> ConsTransitivityPointerLhs::make(std::shared_ptr<TyValue> _p, std::shared_ptr<TyValue> _q, std::shared_ptr<TyValue> _v, std::shared_ptr<TyExpr> _loadq){
  std::shared_ptr<TyTransitivityPointerLhs> _val(new TyTransitivityPointerLhs(std::move(_p), std::move(_q), std::move(_v), std::move(_loadq)));
  return std::shared_ptr<TyInfrule>(new ConsTransitivityPointerLhs(std::move(_val)));
}
void ConsTransitivityPointerLhs::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("TransitivityPointerLhs");
  archive(CEREAL_NVP(transitivity_pointer_lhs));
}

TyTransitivityPointerRhs::TyTransitivityPointerRhs(std::shared_ptr<TyValue> _p, std::shared_ptr<TyValue> _q, std::shared_ptr<TyValue> _v, std::shared_ptr<TyExpr> _loadp) : p(std::move(_p)), q(std::move(_q)), v(std::move(_v)), loadp(std::move(_loadp)){
}
void TyTransitivityPointerRhs::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(p));
  archive(CEREAL_NVP(q));
  archive(CEREAL_NVP(v));
  archive(CEREAL_NVP(loadp));
}

ConsTransitivityPointerRhs::ConsTransitivityPointerRhs(std::shared_ptr<TyTransitivityPointerRhs> _transitivity_pointer_rhs) : transitivity_pointer_rhs(std::move(_transitivity_pointer_rhs)){
}
std::shared_ptr<TyInfrule> ConsTransitivityPointerRhs::make(std::shared_ptr<TyValue> _p, std::shared_ptr<TyValue> _q, std::shared_ptr<TyValue> _v, std::shared_ptr<TyExpr> _loadp){
  std::shared_ptr<TyTransitivityPointerRhs> _val(new TyTransitivityPointerRhs(std::move(_p), std::move(_q), std::move(_v), std::move(_loadp)));
  return std::shared_ptr<TyInfrule>(new ConsTransitivityPointerRhs(std::move(_val)));
}
void ConsTransitivityPointerRhs::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("TransitivityPointerRhs");
  archive(CEREAL_NVP(transitivity_pointer_rhs));
}

TyTransitivityTgt::TyTransitivityTgt
(std::shared_ptr<TyExpr> _e1,
 std::shared_ptr<TyExpr> _e2,
 std::shared_ptr<TyExpr> _e3)
  : e1(std::move(_e1)), e2(std::move(_e2)), e3(std::move(_e3)) {}
  
void TyTransitivityTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(e1), CEREAL_NVP(e2), CEREAL_NVP(e3));
}

ConsTransitivityTgt::ConsTransitivityTgt(std::shared_ptr<TyTransitivityTgt> _transitivity_tgt)
  : transitivity_tgt(std::move(_transitivity_tgt)) {}

void ConsTransitivityTgt::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("TransitivityTgt");
  archive(CEREAL_NVP(transitivity_tgt));
}

std::shared_ptr<TyInfrule> ConsTransitivityTgt::make
(std::shared_ptr<TyExpr> _e1,
 std::shared_ptr<TyExpr> _e2,
 std::shared_ptr<TyExpr> _e3) {
  std::shared_ptr<TyTransitivityTgt> _transitivity_tgt
    (new TyTransitivityTgt(std::move(_e1), std::move(_e2), std::move(_e3)));
  return std::shared_ptr<TyInfrule>(new ConsTransitivityTgt(std::move(_transitivity_tgt)));
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

ConsReplaceRhsOpt::ConsReplaceRhsOpt(std::shared_ptr<TyReplaceRhsOpt> _replace_rhs_opt)
    : replace_rhs_opt(std::move(_replace_rhs_opt)) {}

void ConsReplaceRhsOpt::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("ReplaceRhsOpt");
  archive(CEREAL_NVP(replace_rhs_opt));
}
std::shared_ptr<TyInfrule> ConsReplaceRhsOpt::make(std::shared_ptr<TyRegister> _x,
                                                   std::shared_ptr<TyValue> _y,
                                                   std::shared_ptr<TyExpr> _e1,
                                                   std::shared_ptr<TyExpr> _e2,
                                                   std::shared_ptr<TyExpr> _e2_p) {
  std::shared_ptr<TyReplaceRhsOpt> _replace_rhs_opt(
          new TyReplaceRhsOpt(std::move(_x), std::move(_y), std::move(_e1),
                              std::move(_e2), std::move(_e2_p)));
  return std::shared_ptr<TyInfrule>(
          new ConsReplaceRhsOpt(std::move(_replace_rhs_opt)));
}

TyUdivZext::TyUdivZext(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyRegister> _k, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz1, std::shared_ptr<TySize> _sz2) : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)), k(std::move(_k)), a(std::move(_a)), b(std::move(_b)), sz1(std::move(_sz1)), sz2(std::move(_sz2)){
}
void TyUdivZext::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(k));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(b));
  archive(CEREAL_NVP(sz1));
  archive(CEREAL_NVP(sz2));
}

ConsUdivZext::ConsUdivZext(std::shared_ptr<TyUdivZext> _udiv_zext) : udiv_zext(std::move(_udiv_zext)){
}
std::shared_ptr<TyInfrule> ConsUdivZext::make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyRegister> _k, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz1, std::shared_ptr<TySize> _sz2){
  std::shared_ptr<TyUdivZext> _val(new TyUdivZext(std::move(_z), std::move(_x), std::move(_y), std::move(_k), std::move(_a), std::move(_b), std::move(_sz1), std::move(_sz2)));
  return std::shared_ptr<TyInfrule>(new ConsUdivZext(std::move(_val)));
}
void ConsUdivZext::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("UdivZext");
  archive(CEREAL_NVP(udiv_zext));
}

TyUremZext::TyUremZext(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyRegister> _k, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz1, std::shared_ptr<TySize> _sz2) : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)), k(std::move(_k)), a(std::move(_a)), b(std::move(_b)), sz1(std::move(_sz1)), sz2(std::move(_sz2)){
}
void TyUremZext::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(k));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(b));
  archive(CEREAL_NVP(sz1));
  archive(CEREAL_NVP(sz2));
}

ConsUremZext::ConsUremZext(std::shared_ptr<TyUremZext> _urem_zext) : urem_zext(std::move(_urem_zext)){
}
std::shared_ptr<TyInfrule> ConsUremZext::make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y, std::shared_ptr<TyRegister> _k, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _sz1, std::shared_ptr<TySize> _sz2){
  std::shared_ptr<TyUremZext> _val(new TyUremZext(std::move(_z), std::move(_x), std::move(_y), std::move(_k), std::move(_a), std::move(_b), std::move(_sz1), std::move(_sz2)));
  return std::shared_ptr<TyInfrule>(new ConsUremZext(std::move(_val)));
}
void ConsUremZext::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("UremZext");
  archive(CEREAL_NVP(urem_zext));
}

TyIntroGhost::TyIntroGhost(std::shared_ptr<TyValue> _x, std::shared_ptr<TyRegister> _g) : x(std::move(_x)), g(std::move(_g)){
}

void TyIntroGhost::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(g));
}

ConsIntroGhost::ConsIntroGhost(std::shared_ptr<TyIntroGhost> _intro_ghost) : intro_ghost(std::move(_intro_ghost)) {}

std::shared_ptr<TyInfrule> ConsIntroGhost::make(std::shared_ptr<TyValue> _x, std::shared_ptr<TyRegister> _g) {
  std::shared_ptr<TyIntroGhost> _val(new TyIntroGhost(std::move(_x), std::move(_g)));
  return std::shared_ptr<TyInfrule>(new ConsIntroGhost(std::move(_val)));
}

void ConsIntroGhost::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IntroGhost");
  archive(CEREAL_NVP(intro_ghost));
}

TyIntroEq::TyIntroEq(std::shared_ptr<TyValue> _x) : x(std::move(_x)){
}

void TyIntroEq::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(x));
}

ConsIntroEq::ConsIntroEq(std::shared_ptr<TyIntroEq> _intro_eq) : intro_eq(std::move(_intro_eq)) {}

std::shared_ptr<TyInfrule> ConsIntroEq::make(std::shared_ptr<TyValue> _x) {
  std::shared_ptr<TyIntroEq> _val(new TyIntroEq(std::move(_x)));
  return std::shared_ptr<TyInfrule>(new ConsIntroEq(std::move(_val)));
}

void ConsIntroEq::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IntroEq");
  archive(CEREAL_NVP(intro_eq));
}


TyXorCommutative::TyXorCommutative(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz) : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)), sz(std::move(_sz)){
}
void TyXorCommutative::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(sz));
}

ConsXorCommutative::ConsXorCommutative(std::shared_ptr<TyXorCommutative> _xor_commutative) : xor_commutative(std::move(_xor_commutative)){
}
std::shared_ptr<TyInfrule> ConsXorCommutative::make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz){
  std::shared_ptr<TyXorCommutative> _val(new TyXorCommutative(std::move(_z), std::move(_x), std::move(_y), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsXorCommutative(std::move(_val)));
}
void ConsXorCommutative::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("XorCommutative");
  archive(CEREAL_NVP(xor_commutative));
}

TyXorCommutativeTgt::TyXorCommutativeTgt(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz) : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)), sz(std::move(_sz)){
}
void TyXorCommutativeTgt::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(sz));
}

ConsXorCommutativeTgt::ConsXorCommutativeTgt(std::shared_ptr<TyXorCommutativeTgt> _xor_commutative_tgt) : xor_commutative_tgt(std::move(_xor_commutative_tgt)){
}
std::shared_ptr<TyInfrule> ConsXorCommutativeTgt::make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _sz){
  std::shared_ptr<TyXorCommutativeTgt> _val(new TyXorCommutativeTgt(std::move(_z), std::move(_x), std::move(_y), std::move(_sz)));
  return std::shared_ptr<TyInfrule>(new ConsXorCommutativeTgt(std::move(_val)));
}
void ConsXorCommutativeTgt::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("XorCommutativeTgt");
  archive(CEREAL_NVP(xor_commutative_tgt));
}



} // llvmberry
