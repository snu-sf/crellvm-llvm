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

TyAddAssociative::TyAddAssociative(std::unique_ptr<TyRegister> _x,
                                   std::unique_ptr<TyRegister> _y,
                                   std::unique_ptr<TyRegister> _z,
                                   std::unique_ptr<TyConstInt> _c1,
                                   std::unique_ptr<TyConstInt> _c2,
                                   std::unique_ptr<TyConstInt> _c3,
                                   std::unique_ptr<TySize> _sz)
    : x(std::move(_x)), y(std::move(_y)), z(std::move(_z)), c1(std::move(_c1)),
      c2(std::move(_c2)), c3(std::move(_c3)), sz(std::move(_sz)) {}

void TyAddAssociative::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(x), CEREAL_NVP(y), CEREAL_NVP(z), CEREAL_NVP(c1),
          CEREAL_NVP(c2), CEREAL_NVP(c3), CEREAL_NVP(sz));
}

TyAddConstNot::TyAddConstNot(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyValue> _x, std::unique_ptr<TyConstInt> _c1, std::unique_ptr<TyConstInt> _c2, std::unique_ptr<TySize> _sz) : z(std::move(_z)), y(std::move(_y)), x(std::move(_x)), c1(std::move(_c1)), c2(std::move(_c2)), sz(std::move(_sz)){
}
void TyAddConstNot::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(c1));
  archive(CEREAL_NVP(c2));
  archive(CEREAL_NVP(sz));
}

ConsAddConstNot::ConsAddConstNot(std::unique_ptr<TyAddConstNot> _add_const_not) : add_const_not(std::move(_add_const_not)){
}
std::unique_ptr<TyInfrule> ConsAddConstNot::make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyValue> _x, std::unique_ptr<TyConstInt> _c1, std::unique_ptr<TyConstInt> _c2, std::unique_ptr<TySize> _sz){
  std::unique_ptr<TyAddConstNot> _val(new TyAddConstNot(std::move(_z), std::move(_y), std::move(_x), std::move(_c1), std::move(_c2), std::move(_sz)));
  return std::unique_ptr<TyInfrule>(new ConsAddConstNot(std::move(_val)));
}
void ConsAddConstNot::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("AddConstNot");
  archive(CEREAL_NVP(add_const_not));
}

TyAddMask::TyAddMask(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyRegister> _yprime, std::unique_ptr<TyValue> _x, std::unique_ptr<TyConstInt> _c1, std::unique_ptr<TyConstInt> _c2, std::unique_ptr<TySize> _sz) : z(std::move(_z)), y(std::move(_y)), yprime(std::move(_yprime)), x(std::move(_x)), c1(std::move(_c1)), c2(std::move(_c2)), sz(std::move(_sz)){
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

ConsAddMask::ConsAddMask(std::unique_ptr<TyAddMask> _add_mask) : add_mask(std::move(_add_mask)){
}
std::unique_ptr<TyInfrule> ConsAddMask::make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyRegister> _yprime, std::unique_ptr<TyValue> _x, std::unique_ptr<TyConstInt> _c1, std::unique_ptr<TyConstInt> _c2, std::unique_ptr<TySize> _sz){
  std::unique_ptr<TyAddMask> _val(new TyAddMask(std::move(_z), std::move(_y), std::move(_yprime), std::move(_x), std::move(_c1), std::move(_c2), std::move(_sz)));
  return std::unique_ptr<TyInfrule>(new ConsAddMask(std::move(_val)));
}
void ConsAddMask::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("AddMask");
  archive(CEREAL_NVP(add_mask));
}

TyAddSelectZero::TyAddSelectZero(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _x, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyValue> _c, std::unique_ptr<TyValue> _n, std::unique_ptr<TyValue> _a, std::unique_ptr<TySize> _sz) : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)), c(std::move(_c)), n(std::move(_n)), a(std::move(_a)), sz(std::move(_sz)){
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

ConsAddSelectZero::ConsAddSelectZero(std::unique_ptr<TyAddSelectZero> _add_select_zero) : add_select_zero(std::move(_add_select_zero)){
}
std::unique_ptr<TyInfrule> ConsAddSelectZero::make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _x, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyValue> _c, std::unique_ptr<TyValue> _n, std::unique_ptr<TyValue> _a, std::unique_ptr<TySize> _sz){
  std::unique_ptr<TyAddSelectZero> _val(new TyAddSelectZero(std::move(_z), std::move(_x), std::move(_y), std::move(_c), std::move(_n), std::move(_a), std::move(_sz)));
  return std::unique_ptr<TyInfrule>(new ConsAddSelectZero(std::move(_val)));
}
void ConsAddSelectZero::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("AddSelectZero");
  archive(CEREAL_NVP(add_select_zero));
}

TyAddSelectZero2::TyAddSelectZero2(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _x, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyValue> _c, std::unique_ptr<TyValue> _n, std::unique_ptr<TyValue> _a, std::unique_ptr<TySize> _sz) : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)), c(std::move(_c)), n(std::move(_n)), a(std::move(_a)), sz(std::move(_sz)){
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

ConsAddSelectZero2::ConsAddSelectZero2(std::unique_ptr<TyAddSelectZero2> _add_select_zero2) : add_select_zero2(std::move(_add_select_zero2)){
}
std::unique_ptr<TyInfrule> ConsAddSelectZero2::make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _x, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyValue> _c, std::unique_ptr<TyValue> _n, std::unique_ptr<TyValue> _a, std::unique_ptr<TySize> _sz){
  std::unique_ptr<TyAddSelectZero2> _val(new TyAddSelectZero2(std::move(_z), std::move(_x), std::move(_y), std::move(_c), std::move(_n), std::move(_a), std::move(_sz)));
  return std::unique_ptr<TyInfrule>(new ConsAddSelectZero2(std::move(_val)));
}
void ConsAddSelectZero2::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("AddSelectZero2");
  archive(CEREAL_NVP(add_select_zero2));
}

TyAddSub::TyAddSub(std::unique_ptr<TyRegister> _minusy,
                   std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x,
                   std::unique_ptr<TyValue> _y, std::unique_ptr<TySize> _sz)
    : minusy(std::move(_minusy)), z(std::move(_z)), x(std::move(_x)),
      y(std::move(_y)), sz(std::move(_sz)) {}

void TyAddSub::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(minusy), CEREAL_NVP(z), CEREAL_NVP(x), CEREAL_NVP(y),
          CEREAL_NVP(sz));
}

TyAddCommutative::TyAddCommutative(std::unique_ptr<TyRegister> _z,
                                   std::unique_ptr<TyValue> _x,
                                   std::unique_ptr<TyValue> _y,
                                   std::unique_ptr<TySize> _sz)
    : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)), sz(std::move(_sz)) {
}

void TyAddCommutative::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(x), CEREAL_NVP(y), CEREAL_NVP(sz));
}

ConsAddAssociative::ConsAddAssociative(
    std::unique_ptr<TyAddAssociative> _add_associative)
    : add_associative(std::move(_add_associative)) {}

void ConsAddAssociative::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("AddAssociative");
  archive(CEREAL_NVP(add_associative));
}

std::unique_ptr<TyInfrule> ConsAddAssociative::make(
    std::unique_ptr<TyRegister> _x, std::unique_ptr<TyRegister> _y,
    std::unique_ptr<TyRegister> _z, std::unique_ptr<TyConstInt> _c1,
    std::unique_ptr<TyConstInt> _c2, std::unique_ptr<TyConstInt> _c3,
    std::unique_ptr<TySize> _sz) {
  std::unique_ptr<TyAddAssociative> _add_assoc(new TyAddAssociative(
      std::move(_x), std::move(_y), std::move(_z), std::move(_c1),
      std::move(_c2), std::move(_c3), std::move(_sz)));
  return std::unique_ptr<TyInfrule>(
      new ConsAddAssociative(std::move(_add_assoc)));
}

ConsAddSub::ConsAddSub(std::unique_ptr<TyAddSub> _add_sub)
    : add_sub(std::move(_add_sub)) {}

void ConsAddSub::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("AddSub");
  archive(CEREAL_NVP(add_sub));
}

std::unique_ptr<TyInfrule> ConsAddSub::make(std::unique_ptr<TyRegister> _minusy,
                                            std::unique_ptr<TyRegister> _z,
                                            std::unique_ptr<TyValue> _x,
                                            std::unique_ptr<TyValue> _y,
                                            std::unique_ptr<TySize> _sz) {
  std::unique_ptr<TyAddSub> _add_sub(
      new TyAddSub(std::move(_minusy), std::move(_z), std::move(_x),
                   std::move(_y), std::move(_sz)));
  return std::unique_ptr<TyInfrule>(new ConsAddSub(std::move(_add_sub)));
}

ConsAddCommutative::ConsAddCommutative(
    std::unique_ptr<TyAddCommutative> _add_comm)
    : add_commutative(std::move(_add_comm)) {}

void ConsAddCommutative::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("AddCommutative");
  archive(CEREAL_NVP(add_commutative));
}

std::unique_ptr<TyInfrule> ConsAddCommutative::make(
    std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x,
    std::unique_ptr<TyValue> _y, std::unique_ptr<TySize> _sz) {
  std::unique_ptr<TyAddCommutative> _add_comm(new TyAddCommutative(
      std::move(_z), std::move(_x), std::move(_y), std::move(_sz)));
  return std::unique_ptr<TyInfrule>(
      new ConsAddCommutative(std::move(_add_comm)));
}

TyAddOnebit::TyAddOnebit(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y) : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)){
}
void TyAddOnebit::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
}

ConsAddOnebit::ConsAddOnebit(std::unique_ptr<TyAddOnebit> _add_onebit) : add_onebit(std::move(_add_onebit)){
}
std::unique_ptr<TyInfrule> ConsAddOnebit::make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y){
  std::unique_ptr<TyAddOnebit> _val(new TyAddOnebit(std::move(_z), std::move(_x), std::move(_y)));
  return std::unique_ptr<TyInfrule>(new ConsAddOnebit(std::move(_val)));
}
void ConsAddOnebit::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("AddOnebit");
  archive(CEREAL_NVP(add_onebit));
}

TyAddShift::TyAddShift(std::unique_ptr<TyRegister> _y,
                       std::unique_ptr<TyValue> _v, std::unique_ptr<TySize> _sz)
    : y(std::move(_y)), v(std::move(_v)), sz(std::move(_sz)) {}

void TyAddShift::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(y), CEREAL_NVP(v), CEREAL_NVP(sz));
}

ConsAddShift::ConsAddShift(std::unique_ptr<TyAddShift> _add_shift)
    : add_shift(std::move(_add_shift)) {}

void ConsAddShift::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("AddShift");
  archive(CEREAL_NVP(add_shift));
}

std::unique_ptr<TyInfrule> ConsAddShift::make(std::unique_ptr<TyRegister> _y,
                                              std::unique_ptr<TyValue> _v,
                                              std::unique_ptr<TySize> _sz) {
  std::unique_ptr<TyAddShift> _add_shift(
      new TyAddShift(std::move(_y), std::move(_v), std::move(_sz)));
  return std::unique_ptr<TyInfrule>(new ConsAddShift(std::move(_add_shift)));
}

TyAddSignbit::TyAddSignbit(std::unique_ptr<TyRegister> _x,
                           std::unique_ptr<TyValue> _e1,
                           std::unique_ptr<TyValue> _e2,
                           std::unique_ptr<TySize> _sz)
    : x(std::move(_x)), e1(std::move(_e1)), e2(std::move(_e2)),
      sz(std::move(_sz)) {}

void TyAddSignbit::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(e1));
  archive(CEREAL_NVP(e2));
  archive(CEREAL_NVP(sz));
}

ConsAddSignbit::ConsAddSignbit(std::unique_ptr<TyAddSignbit> _add_signbit)
    : add_signbit(std::move(_add_signbit)) {}

void ConsAddSignbit::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("AddSignbit");
  archive(CEREAL_NVP(add_signbit));
}

std::unique_ptr<TyInfrule> ConsAddSignbit::make(std::unique_ptr<TyRegister> _x,
                                                std::unique_ptr<TyValue> _e1,
                                                std::unique_ptr<TyValue> _e2,
                                                std::unique_ptr<TySize> _sz) {
  std::unique_ptr<TyAddSignbit> _add_signbit(new TyAddSignbit(
      std::move(_x), std::move(_e1), std::move(_e2), std::move(_sz)));
  return std::unique_ptr<TyInfrule>(
      new ConsAddSignbit(std::move(_add_signbit)));
}

TyAddZextBool::TyAddZextBool(std::unique_ptr<TyRegister> _x, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyValue> _b, std::unique_ptr<TyConstInt> _c, std::unique_ptr<TyConstInt> _cprime, std::unique_ptr<TySize> _sz) : x(std::move(_x)), y(std::move(_y)), b(std::move(_b)), c(std::move(_c)), cprime(std::move(_cprime)), sz(std::move(_sz)){
}
void TyAddZextBool::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(b));
  archive(CEREAL_NVP(c));
  archive(CEREAL_NVP(cprime));
  archive(CEREAL_NVP(sz));
}

ConsAddZextBool::ConsAddZextBool(std::unique_ptr<TyAddZextBool> _add_zext_bool) : add_zext_bool(std::move(_add_zext_bool)){
}
std::unique_ptr<TyInfrule> ConsAddZextBool::make(std::unique_ptr<TyRegister> _x, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyValue> _b, std::unique_ptr<TyConstInt> _c, std::unique_ptr<TyConstInt> _cprime, std::unique_ptr<TySize> _sz){
  std::unique_ptr<TyAddZextBool> _val(new TyAddZextBool(std::move(_x), std::move(_y), std::move(_b), std::move(_c), std::move(_cprime), std::move(_sz)));
  return std::unique_ptr<TyInfrule>(new ConsAddZextBool(std::move(_val)));
}
void ConsAddZextBool::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("AddZextBool");
  archive(CEREAL_NVP(add_zext_bool));
}

TySubAdd::TySubAdd
        (std::unique_ptr<TyRegister> _z,
         std::unique_ptr<TyValue> _my,
         std::unique_ptr<TyRegister> _x,
         std::unique_ptr<TyValue> _y,
         std::unique_ptr<TySize> _sz)
        : z(std::move(_z)), my(std::move(_my)), x(std::move(_x)),
          y(std::move(_y)), sz(std::move(_sz)) {}

void TySubAdd::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(my), CEREAL_NVP(x),
          CEREAL_NVP(y), CEREAL_NVP(sz));
}

ConsSubAdd::ConsSubAdd(std::unique_ptr<TySubAdd> _sub_add)
        : sub_add(std::move(_sub_add)) {}

void ConsSubAdd::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("SubAdd");
  archive(CEREAL_NVP(sub_add));
}

std::unique_ptr<TyInfrule> ConsSubAdd::make
        (std::unique_ptr<TyRegister> _z,
         std::unique_ptr<TyValue> _my,
         std::unique_ptr<TyRegister> _x,
         std::unique_ptr<TyValue> _y,
         std::unique_ptr<TySize> _sz) {
  std::unique_ptr<TySubAdd> _sub_add
          (new TySubAdd
                   (std::move(_z), std::move(_my), std::move(_x),
                    std::move(_y), std::move(_sz)));
  return std::unique_ptr<TyInfrule>(new ConsSubAdd(std::move(_sub_add)));
}

TySubMone::TySubMone(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TySize> _sz) : z(std::move(_z)), x(std::move(_x)), sz(std::move(_sz)){
}
void TySubMone::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(sz));
}

ConsSubMone::ConsSubMone(std::unique_ptr<TySubMone> _sub_mone) : sub_mone(std::move(_sub_mone)){
}
std::unique_ptr<TyInfrule> ConsSubMone::make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TySize> _sz){
  std::unique_ptr<TySubMone> _val(new TySubMone(std::move(_z), std::move(_x), std::move(_sz)));
  return std::unique_ptr<TyInfrule>(new ConsSubMone(std::move(_val)));
}
void ConsSubMone::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SubMone");
  archive(CEREAL_NVP(sub_mone));
}

TySubOnebit::TySubOnebit(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y) : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)){
}
void TySubOnebit::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
}

ConsSubOnebit::ConsSubOnebit(std::unique_ptr<TySubOnebit> _sub_onebit) : sub_onebit(std::move(_sub_onebit)){
}
std::unique_ptr<TyInfrule> ConsSubOnebit::make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TyValue> _y){
  std::unique_ptr<TySubOnebit> _val(new TySubOnebit(std::move(_z), std::move(_x), std::move(_y)));
  return std::unique_ptr<TyInfrule>(new ConsSubOnebit(std::move(_val)));
}
void ConsSubOnebit::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SubOnebit");
  archive(CEREAL_NVP(sub_onebit));
}

TySubSdiv::TySubSdiv(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyValue> _x, std::unique_ptr<TyConstInt> _c, std::unique_ptr<TyConstInt> _cprime, std::unique_ptr<TySize> _sz) : z(std::move(_z)), y(std::move(_y)), x(std::move(_x)), c(std::move(_c)), cprime(std::move(_cprime)), sz(std::move(_sz)){
}
void TySubSdiv::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(c));
  archive(CEREAL_NVP(cprime));
  archive(CEREAL_NVP(sz));
}

ConsSubSdiv::ConsSubSdiv(std::unique_ptr<TySubSdiv> _sub_sdiv) : sub_sdiv(std::move(_sub_sdiv)){
}
std::unique_ptr<TyInfrule> ConsSubSdiv::make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyValue> _x, std::unique_ptr<TyConstInt> _c, std::unique_ptr<TyConstInt> _cprime, std::unique_ptr<TySize> _sz){
  std::unique_ptr<TySubSdiv> _val(new TySubSdiv(std::move(_z), std::move(_y), std::move(_x), std::move(_c), std::move(_cprime), std::move(_sz)));
  return std::unique_ptr<TyInfrule>(new ConsSubSdiv(std::move(_val)));
}
void ConsSubSdiv::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SubSdiv");
  archive(CEREAL_NVP(sub_sdiv));
}



TySubShl::TySubShl(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyValue> _mx, std::unique_ptr<TyValue> _a, std::unique_ptr<TySize> _sz) : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)), mx(std::move(_mx)), a(std::move(_a)), sz(std::move(_sz)){
}
void TySubShl::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z));
  archive(CEREAL_NVP(x));
  archive(CEREAL_NVP(y));
  archive(CEREAL_NVP(mx));
  archive(CEREAL_NVP(a));
  archive(CEREAL_NVP(sz));
}

ConsSubShl::ConsSubShl(std::unique_ptr<TySubShl> _sub_shl) : sub_shl(std::move(_sub_shl)){
}
std::unique_ptr<TyInfrule> ConsSubShl::make(std::unique_ptr<TyRegister> _z, std::unique_ptr<TyValue> _x, std::unique_ptr<TyRegister> _y, std::unique_ptr<TyValue> _mx, std::unique_ptr<TyValue> _a, std::unique_ptr<TySize> _sz){
  std::unique_ptr<TySubShl> _val(new TySubShl(std::move(_z), std::move(_x), std::move(_y), std::move(_mx), std::move(_a), std::move(_sz)));
  return std::unique_ptr<TyInfrule>(new ConsSubShl(std::move(_val)));
}
void ConsSubShl::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SubShl");
  archive(CEREAL_NVP(sub_shl));
}


TyMulNeg::TyMulNeg
          (std::unique_ptr<TyRegister> _z,
           std::unique_ptr<TyValue> _mx,
           std::unique_ptr<TyValue> _my,
           std::unique_ptr<TyValue> _x,
           std::unique_ptr<TyValue> _y,
           std::unique_ptr<TySize> _sz)
          : z(std::move(_z)), mx(std::move(_mx)), my(std::move(_my)), x(std::move(_x)),
            y(std::move(_y)), sz(std::move(_sz)) {}

  void TyMulNeg::serialize(cereal::JSONOutputArchive &archive) const {
    archive(CEREAL_NVP(z), CEREAL_NVP(mx), CEREAL_NVP(my),
            CEREAL_NVP(x), CEREAL_NVP(y), CEREAL_NVP(sz));
  }

  ConsMulNeg::ConsMulNeg(std::unique_ptr<TyMulNeg> _mul_neg)
          : mul_neg(std::move(_mul_neg)) {}

  void ConsMulNeg::serialize(cereal::JSONOutputArchive &archive) const {
    archive.makeArray();
    archive.writeName();

    archive.saveValue("MulNeg");
    archive(CEREAL_NVP(mul_neg));
  }

  std::unique_ptr<TyInfrule> ConsMulNeg::make
          (std::unique_ptr<TyRegister> _z,
           std::unique_ptr<TyValue> _mx,
           std::unique_ptr<TyValue> _my,
           std::unique_ptr<TyValue> _x,
           std::unique_ptr<TyValue> _y,
           std::unique_ptr<TySize> _sz) {
    std::unique_ptr<TyMulNeg> _mul_neg
            (new TyMulNeg
                     (std::move(_z), std::move(_mx), std::move(_my),
                      std::move(_x), std::move(_y), std::move(_sz)));
    return std::unique_ptr<TyInfrule>(new ConsMulNeg(std::move(_mul_neg)));
  }

  TyNegVal::TyNegVal
            (std::unique_ptr<TyConstInt> _c1,
             std::unique_ptr<TyConstInt> _c2,
             std::unique_ptr<TySize> _sz)
             : c1(std::move(_c1)), c2(std::move(_c2)),sz(std::move(_sz)) {}

    void TyNegVal::serialize(cereal::JSONOutputArchive &archive) const {
          archive(CEREAL_NVP(c1), CEREAL_NVP(c2), CEREAL_NVP(sz));
            }

      ConsNegVal::ConsNegVal(std::unique_ptr<TyNegVal> _neg_val)
                  : neg_val(std::move(_neg_val)) {}

        void ConsNegVal::serialize(cereal::JSONOutputArchive &archive) const {
              archive.makeArray();
              archive.writeName();

              archive.saveValue("NegVal");
              archive(CEREAL_NVP(neg_val));
             }

          std::unique_ptr<TyInfrule> ConsNegVal::make
                      (std::unique_ptr<TyConstInt> _c1,
                       std::unique_ptr<TyConstInt> _c2,
                       std::unique_ptr<TySize> _sz) {
                       std::unique_ptr<TyNegVal> _neg_val
                       (new TyNegVal
                            (std::move(_c1), std::move(_c2), std::move(_sz)));
          return std::unique_ptr<TyInfrule>(new ConsNegVal(std::move(_neg_val)));
          }

 TySubRemove::TySubRemove(std::unique_ptr<TyRegister> _z,
                         std::unique_ptr<TyRegister> _y,
                         std::unique_ptr<TyValue> _a,
                         std::unique_ptr<TyValue> _b,
                         std::unique_ptr<TySize> _sz)
    : z(std::move(_z)), y(std::move(_y)), a(std::move(_a)), b(std::move(_b)),
      sz(std::move(_sz)) {}

void TySubRemove::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(y), CEREAL_NVP(a), CEREAL_NVP(b),
          CEREAL_NVP(sz));
}

ConsSubRemove::ConsSubRemove(std::unique_ptr<TySubRemove> _sub_remove)
    : sub_remove(std::move(_sub_remove)) {}

void ConsSubRemove::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("SubRemove");
  archive(CEREAL_NVP(sub_remove));
}

std::unique_ptr<TyInfrule> ConsSubRemove::make(std::unique_ptr<TyRegister> _z,
                                               std::unique_ptr<TyRegister> _y,
                                               std::unique_ptr<TyValue> _a,
                                               std::unique_ptr<TyValue> _b,
                                               std::unique_ptr<TySize> _sz) {
  std::unique_ptr<TySubRemove> _sub_remove(
      new TySubRemove(std::move(_z), std::move(_y), std::move(_a),
                      std::move(_b), std::move(_sz)));
  return std::unique_ptr<TyInfrule>(new ConsSubRemove(std::move(_sub_remove)));
}

TyMulBool::TyMulBool(std::unique_ptr<TyRegister> _z,
                     std::unique_ptr<TyRegister> _x,
                     std::unique_ptr<TyRegister> _y)
    : z(std::move(_z)), x(std::move(_x)), y(std::move(_y)) {}

void TyMulBool::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(z), CEREAL_NVP(x), CEREAL_NVP(y));
}

ConsMulBool::ConsMulBool(std::unique_ptr<TyMulBool> _mul_bool)
    : mul_bool(std::move(_mul_bool)) {}

void ConsMulBool::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("MulBool");
  archive(CEREAL_NVP(mul_bool));
}

std::unique_ptr<TyInfrule> ConsMulBool::make(std::unique_ptr<TyRegister> _z,
                                             std::unique_ptr<TyRegister> _x,
                                             std::unique_ptr<TyRegister> _y) {
  std::unique_ptr<TyMulBool> _mul_bool(
      new TyMulBool(std::move(_z), std::move(_x), std::move(_y)));

  return std::unique_ptr<TyInfrule>(new ConsMulBool(std::move(_mul_bool)));
}

}
