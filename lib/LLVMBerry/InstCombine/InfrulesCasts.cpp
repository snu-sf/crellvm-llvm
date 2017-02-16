#include <string>
#include <cassert>
#include <cereal/archives/json.hpp>
#include "llvm/LLVMBerry/Structure.h"
#include "llvm/LLVMBerry/ValidationUnit.h"
#include "llvm/LLVMBerry/InstCombine/InfrulesCasts.h"

namespace llvmberry {

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

TyPtrtointInttoptr::TyPtrtointInttoptr(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid, std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty, std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty) : src(_src), mid(_mid), dst(_dst), srcty(_srcty), midty(_midty), dstty(_dstty){
}
void TyPtrtointInttoptr::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(src), CEREAL_NVP(mid), CEREAL_NVP(dst), CEREAL_NVP(srcty), CEREAL_NVP(midty), CEREAL_NVP(dstty));
}

ConsPtrtointInttoptr::ConsPtrtointInttoptr(std::shared_ptr<TyPtrtointInttoptr> _ptrtoint_inttoptr) : ptrtoint_inttoptr(_ptrtoint_inttoptr){
}
std::shared_ptr<TyInfrule> ConsPtrtointInttoptr::make(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid, std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty, std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty){
  std::shared_ptr<TyPtrtointInttoptr> _val(new TyPtrtointInttoptr(_src, _mid, _dst, _srcty, _midty, _dstty));
  return std::shared_ptr<TyInfrule>(new ConsPtrtointInttoptr(_val));
}
void ConsPtrtointInttoptr::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("PtrtointInttoptr");
  archive(CEREAL_NVP(ptrtoint_inttoptr));
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

TySextTruncAshr::TySextTruncAshr(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _xprime, std::shared_ptr<TyValue> _v, std::shared_ptr<TySize> _s1, std::shared_ptr<TySize> _s2, std::shared_ptr<TyConstInt> _i3) : z(_z), x(_x), xprime(_xprime), v(_v), s1(_s1), s2(_s2), i3(_i3){
}
void TySextTruncAshr::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(z), CEREAL_NVP(x), CEREAL_NVP(xprime), CEREAL_NVP(v), CEREAL_NVP(s1), CEREAL_NVP(s2), CEREAL_NVP(i3));
}

ConsSextTruncAshr::ConsSextTruncAshr(std::shared_ptr<TySextTruncAshr> _sext_trunc_ashr) : sext_trunc_ashr(_sext_trunc_ashr){
}
std::shared_ptr<TyInfrule> ConsSextTruncAshr::make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _xprime, std::shared_ptr<TyValue> _v, std::shared_ptr<TySize> _s1, std::shared_ptr<TySize> _s2, std::shared_ptr<TyConstInt> _i3){
  std::shared_ptr<TySextTruncAshr> _val(new TySextTruncAshr(_z, _x, _xprime, _v, _s1, _s2, _i3));
  return std::shared_ptr<TyInfrule>(new ConsSextTruncAshr(_val));
}
void ConsSextTruncAshr::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SextTruncAshr");
  archive(CEREAL_NVP(sext_trunc_ashr));
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

}
