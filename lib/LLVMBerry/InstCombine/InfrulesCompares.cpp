#include <string>
#include <cassert>
#include <cereal/archives/json.hpp>
#include "llvm/LLVMBerry/Structure.h"
#include "llvm/LLVMBerry/ValidationUnit.h"
#include "llvm/LLVMBerry/InstCombine/InfrulesCompares.h"

namespace llvmberry {

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

}
