#ifndef INFRULES_LOAD_STORE_ALLOCA
#define INFRUELS_LOAD_STORE_ALLOCA

#include "llvm/LLVMBerry/Structure.h"
#include "llvm/IR/Instructions.h"
#include "cereal/types/memory.hpp"
#include "cereal/archives/json.hpp"

namespace llvmberry {

struct TyGepzero {
public:
  TyGepzero(std::shared_ptr<TyValue> _vprime, std::shared_ptr<TyExpr> _gepinst);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> vprime;
  std::shared_ptr<TyExpr> gepinst;
};

struct ConsGepzero : public TyInfrule {
public:
  ConsGepzero(std::shared_ptr<TyGepzero> _gepzero);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _vprime,
                                         std::shared_ptr<TyExpr> _gepinst);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyGepzero> gepzero;
};

struct TyBitcastptr {
public:
  TyBitcastptr(std::shared_ptr<TyValue> _vprime,
               std::shared_ptr<TyExpr> _bitcastinst);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> vprime;
  std::shared_ptr<TyExpr> bitcastinst;
};

struct ConsBitcastptr : public TyInfrule {
public:
  ConsBitcastptr(std::shared_ptr<TyBitcastptr> _bitcastptr);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _vprime,
                                         std::shared_ptr<TyExpr> _bitcastinst);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyBitcastptr> bitcastptr;
};

struct TyPtrtointZero{
public : 
  TyPtrtointZero(std::shared_ptr<TyValueType> _ptrty, std::shared_ptr<TyValueType> _intty);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyValueType> ptrty;
  std::shared_ptr<TyValueType> intty;
};

struct ConsPtrtointZero : public TyInfrule{
public : 
  ConsPtrtointZero(std::shared_ptr<TyPtrtointZero> _ptrtoint_zero);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValueType> _ptrty, std::shared_ptr<TyValueType> _intty);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyPtrtointZero> ptrtoint_zero;
};

struct TyPtrtointLoad {
public:
  TyPtrtointLoad(std::shared_ptr<TyValue> _ptr,
                 std::shared_ptr<TyValueType> _ptrty,
                 std::shared_ptr<TyValue> _v1,
                 std::shared_ptr<TyValueType> _intty,
                 std::shared_ptr<TyValue> _v2, std::shared_ptr<TySize> _a);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> ptr;
  std::shared_ptr<TyValueType> ptrty;
  std::shared_ptr<TyValue> v1;
  std::shared_ptr<TyValueType> intty;
  std::shared_ptr<TyValue> v2;
  std::shared_ptr<TySize> a;
};

struct ConsPtrtointLoad : public TyInfrule {
public:
  ConsPtrtointLoad(std::shared_ptr<TyPtrtointLoad> _ptrtoint_load);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyValue> _ptr, std::shared_ptr<TyValueType> _ptrty,
       std::shared_ptr<TyValue> _v1, std::shared_ptr<TyValueType> _intty,
       std::shared_ptr<TyValue> _v2, std::shared_ptr<TySize> _a);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyPtrtointLoad> ptrtoint_load;
};

struct TyBitcastLoad {
public:
  TyBitcastLoad(std::shared_ptr<TyValue> _ptr,
                std::shared_ptr<TyValueType> _ptrty,
                std::shared_ptr<TyValue> _v1,
                std::shared_ptr<TyValueType> _ptrty2,
                std::shared_ptr<TyValue> _v2, std::shared_ptr<TySize> _a);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> ptr;
  std::shared_ptr<TyValueType> ptrty;
  std::shared_ptr<TyValue> v1;
  std::shared_ptr<TyValueType> ptrty2;
  std::shared_ptr<TyValue> v2;
  std::shared_ptr<TySize> a;
};

struct ConsBitcastLoad : public TyInfrule {
public:
  ConsBitcastLoad(std::shared_ptr<TyBitcastLoad> _bitcast_load);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyValue> _ptr, std::shared_ptr<TyValueType> _ptrty,
       std::shared_ptr<TyValue> _v1, std::shared_ptr<TyValueType> _ptrty2,
       std::shared_ptr<TyValue> _v2, std::shared_ptr<TySize> _a);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyBitcastLoad> bitcast_load;
};

struct TyInttoptrLoad {
public:
  TyInttoptrLoad(std::shared_ptr<TyValue> _ptr,
                 std::shared_ptr<TyValueType> _intty,
                 std::shared_ptr<TyValue> _v1,
                 std::shared_ptr<TyValueType> _ptrty,
                 std::shared_ptr<TyValue> _v2, std::shared_ptr<TySize> _a);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> ptr;
  std::shared_ptr<TyValueType> intty;
  std::shared_ptr<TyValue> v1;
  std::shared_ptr<TyValueType> ptrty;
  std::shared_ptr<TyValue> v2;
  std::shared_ptr<TySize> a;
};

struct ConsInttoptrLoad : public TyInfrule {
public:
  ConsInttoptrLoad(std::shared_ptr<TyInttoptrLoad> _inttoptr_load);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyValue> _ptr, std::shared_ptr<TyValueType> _intty,
       std::shared_ptr<TyValue> _v1, std::shared_ptr<TyValueType> _ptrty,
       std::shared_ptr<TyValue> _v2, std::shared_ptr<TySize> _a);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyInttoptrLoad> inttoptr_load;
};

} // llvmberry

#endif
