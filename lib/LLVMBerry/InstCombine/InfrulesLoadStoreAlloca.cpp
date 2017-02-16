#include <string>
#include <cassert>
#include <cereal/archives/json.hpp>
#include "llvm/LLVMBerry/Structure.h"
#include "llvm/LLVMBerry/ValidationUnit.h"
#include "llvm/LLVMBerry/InstCombine/InfrulesLoadStoreAlloca.h"

namespace llvmberry {

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

TyPtrtointZero::TyPtrtointZero(std::shared_ptr<TyValueType> _ptrty, std::shared_ptr<TyValueType> _intty) : ptrty(_ptrty), intty(_intty){
}
void TyPtrtointZero::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(ptrty), CEREAL_NVP(intty));
}

ConsPtrtointZero::ConsPtrtointZero(std::shared_ptr<TyPtrtointZero> _ptrtoint_zero) : ptrtoint_zero(_ptrtoint_zero){
}
std::shared_ptr<TyInfrule> ConsPtrtointZero::make(std::shared_ptr<TyValueType> _ptrty, std::shared_ptr<TyValueType> _intty){
  std::shared_ptr<TyPtrtointZero> _val(new TyPtrtointZero(_ptrty, _intty));
  return std::shared_ptr<TyInfrule>(new ConsPtrtointZero(_val));
}
void ConsPtrtointZero::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("PtrtointZero");
  archive(CEREAL_NVP(ptrtoint_zero));
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

}
