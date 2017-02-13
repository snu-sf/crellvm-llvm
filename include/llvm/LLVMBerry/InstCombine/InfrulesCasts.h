#ifndef INFRULES_CASTS
#define INFRUELS_CASTS

#include "llvm/LLVMBerry/Structure.h"
#include "llvm/IR/Instructions.h"
#include "cereal/types/memory.hpp"
#include "cereal/archives/json.hpp"

namespace llvmberry {

struct TyBitcastBitcast {
public:
  TyBitcastBitcast(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
                   std::shared_ptr<TyValue> _dst,
                   std::shared_ptr<TyValueType> _srcty,
                   std::shared_ptr<TyValueType> _midty,
                   std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsBitcastBitcast : public TyInfrule {
public:
  ConsBitcastBitcast(std::shared_ptr<TyBitcastBitcast> _bitcast_bitcast);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyBitcastBitcast> bitcast_bitcast;
};

struct TyFpextBitcast {
public:
  TyFpextBitcast(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
                 std::shared_ptr<TyValue> _dst,
                 std::shared_ptr<TyValueType> _srcty,
                 std::shared_ptr<TyValueType> _midty,
                 std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsFpextBitcast : public TyInfrule {
public:
  ConsFpextBitcast(std::shared_ptr<TyFpextBitcast> _fpext_bitcast);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyFpextBitcast> fpext_bitcast;
};

struct TyFptosiBitcast {
public:
  TyFptosiBitcast(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
                  std::shared_ptr<TyValue> _dst,
                  std::shared_ptr<TyValueType> _srcty,
                  std::shared_ptr<TyValueType> _midty,
                  std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsFptosiBitcast : public TyInfrule {
public:
  ConsFptosiBitcast(std::shared_ptr<TyFptosiBitcast> _fptosi_bitcast);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyFptosiBitcast> fptosi_bitcast;
};

struct TyFptouiBitcast {
public:
  TyFptouiBitcast(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
                  std::shared_ptr<TyValue> _dst,
                  std::shared_ptr<TyValueType> _srcty,
                  std::shared_ptr<TyValueType> _midty,
                  std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsFptouiBitcast : public TyInfrule {
public:
  ConsFptouiBitcast(std::shared_ptr<TyFptouiBitcast> _fptoui_bitcast);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyFptouiBitcast> fptoui_bitcast;
};

struct TyFptruncBitcast {
public:
  TyFptruncBitcast(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
                   std::shared_ptr<TyValue> _dst,
                   std::shared_ptr<TyValueType> _srcty,
                   std::shared_ptr<TyValueType> _midty,
                   std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsFptruncBitcast : public TyInfrule {
public:
  ConsFptruncBitcast(std::shared_ptr<TyFptruncBitcast> _fptrunc_bitcast);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyFptruncBitcast> fptrunc_bitcast;
};

struct TyInttoptrBitcast {
public:
  TyInttoptrBitcast(std::shared_ptr<TyValue> _src,
                    std::shared_ptr<TyValue> _mid,
                    std::shared_ptr<TyValue> _dst,
                    std::shared_ptr<TyValueType> _srcty,
                    std::shared_ptr<TyValueType> _midty,
                    std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsInttoptrBitcast : public TyInfrule {
public:
  ConsInttoptrBitcast(std::shared_ptr<TyInttoptrBitcast> _inttoptr_bitcast);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyInttoptrBitcast> inttoptr_bitcast;
};

struct TyPtrtointBitcast {
public:
  TyPtrtointBitcast(std::shared_ptr<TyValue> _src,
                    std::shared_ptr<TyValue> _mid,
                    std::shared_ptr<TyValue> _dst,
                    std::shared_ptr<TyValueType> _srcty,
                    std::shared_ptr<TyValueType> _midty,
                    std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsPtrtointBitcast : public TyInfrule {
public:
  ConsPtrtointBitcast(std::shared_ptr<TyPtrtointBitcast> _inttoptr_bitcast);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyPtrtointBitcast> inttoptr_bitcast;
};

struct TySextBitcast {
public:
  TySextBitcast(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
                std::shared_ptr<TyValue> _dst,
                std::shared_ptr<TyValueType> _srcty,
                std::shared_ptr<TyValueType> _midty,
                std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsSextBitcast : public TyInfrule {
public:
  ConsSextBitcast(std::shared_ptr<TySextBitcast> _sext_bitcast);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySextBitcast> sext_bitcast;
};

struct TySitofpBitcast {
public:
  TySitofpBitcast(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
                  std::shared_ptr<TyValue> _dst,
                  std::shared_ptr<TyValueType> _srcty,
                  std::shared_ptr<TyValueType> _midty,
                  std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsSitofpBitcast : public TyInfrule {
public:
  ConsSitofpBitcast(std::shared_ptr<TySitofpBitcast> _sitofp_bitcast);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySitofpBitcast> sitofp_bitcast;
};

struct TyTruncBitcast {
public:
  TyTruncBitcast(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
                 std::shared_ptr<TyValue> _dst,
                 std::shared_ptr<TyValueType> _srcty,
                 std::shared_ptr<TyValueType> _midty,
                 std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsTruncBitcast : public TyInfrule {
public:
  ConsTruncBitcast(std::shared_ptr<TyTruncBitcast> _trunc_bitcast);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyTruncBitcast> trunc_bitcast;
};

struct TyUitofpBitcast {
public:
  TyUitofpBitcast(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
                  std::shared_ptr<TyValue> _dst,
                  std::shared_ptr<TyValueType> _srcty,
                  std::shared_ptr<TyValueType> _midty,
                  std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsUitofpBitcast : public TyInfrule {
public:
  ConsUitofpBitcast(std::shared_ptr<TyUitofpBitcast> _uitofp_bitcast);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyUitofpBitcast> uitofp_bitcast;
};

struct TyZextBitcast {
public:
  TyZextBitcast(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
                std::shared_ptr<TyValue> _dst,
                std::shared_ptr<TyValueType> _srcty,
                std::shared_ptr<TyValueType> _midty,
                std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsZextBitcast : public TyInfrule {
public:
  ConsZextBitcast(std::shared_ptr<TyZextBitcast> _zext_bitcast);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyZextBitcast> zext_bitcast;
};

struct TyBitcastFpext {
public:
  TyBitcastFpext(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
                 std::shared_ptr<TyValue> _dst,
                 std::shared_ptr<TyValueType> _srcty,
                 std::shared_ptr<TyValueType> _midty,
                 std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsBitcastFpext : public TyInfrule {
public:
  ConsBitcastFpext(std::shared_ptr<TyBitcastFpext> _bitcast_fpext);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyBitcastFpext> bitcast_fpext;
};

struct TyFpextFpext {
public:
  TyFpextFpext(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
               std::shared_ptr<TyValue> _dst,
               std::shared_ptr<TyValueType> _srcty,
               std::shared_ptr<TyValueType> _midty,
               std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsFpextFpext : public TyInfrule {
public:
  ConsFpextFpext(std::shared_ptr<TyFpextFpext> _fpext_fpext);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyFpextFpext> fpext_fpext;
};

struct TyFptosiFpext {
public:
  TyFptosiFpext(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
                std::shared_ptr<TyValue> _dst,
                std::shared_ptr<TyValueType> _srcty,
                std::shared_ptr<TyValueType> _midty,
                std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsFptosiFpext : public TyInfrule {
public:
  ConsFptosiFpext(std::shared_ptr<TyFptosiFpext> _fpext_fptosi);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyFptosiFpext> fpext_fptosi;
};

struct TyFptouiFpext {
public:
  TyFptouiFpext(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
                std::shared_ptr<TyValue> _dst,
                std::shared_ptr<TyValueType> _srcty,
                std::shared_ptr<TyValueType> _midty,
                std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsFptouiFpext : public TyInfrule {
public:
  ConsFptouiFpext(std::shared_ptr<TyFptouiFpext> _fpext_fptoui);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyFptouiFpext> fpext_fptoui;
};

struct TyFptruncFpext {
public:
  TyFptruncFpext(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
                 std::shared_ptr<TyValue> _dst,
                 std::shared_ptr<TyValueType> _srcty,
                 std::shared_ptr<TyValueType> _midty,
                 std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsFptruncFpext : public TyInfrule {
public:
  ConsFptruncFpext(std::shared_ptr<TyFptruncFpext> _fptrunc_fpext);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyFptruncFpext> fptrunc_fpext;
};

struct TyBitcastFptosi {
public:
  TyBitcastFptosi(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
                  std::shared_ptr<TyValue> _dst,
                  std::shared_ptr<TyValueType> _srcty,
                  std::shared_ptr<TyValueType> _midty,
                  std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsBitcastFptosi : public TyInfrule {
public:
  ConsBitcastFptosi(std::shared_ptr<TyBitcastFptosi> _bitcast_fptosi);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyBitcastFptosi> bitcast_fptosi;
};

struct TyBitcastFptoui {
public:
  TyBitcastFptoui(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
                  std::shared_ptr<TyValue> _dst,
                  std::shared_ptr<TyValueType> _srcty,
                  std::shared_ptr<TyValueType> _midty,
                  std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsBitcastFptoui : public TyInfrule {
public:
  ConsBitcastFptoui(std::shared_ptr<TyBitcastFptoui> _bitcast_fptoui);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyBitcastFptoui> bitcast_fptoui;
};

struct TyBitcastFptrunc {
public:
  TyBitcastFptrunc(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
                   std::shared_ptr<TyValue> _dst,
                   std::shared_ptr<TyValueType> _srcty,
                   std::shared_ptr<TyValueType> _midty,
                   std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsBitcastFptrunc : public TyInfrule {
public:
  ConsBitcastFptrunc(std::shared_ptr<TyBitcastFptrunc> _bitcast_fptrunc);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyBitcastFptrunc> bitcast_fptrunc;
};

struct TyBitcastInttoptr {
public:
  TyBitcastInttoptr(std::shared_ptr<TyValue> _src,
                    std::shared_ptr<TyValue> _mid,
                    std::shared_ptr<TyValue> _dst,
                    std::shared_ptr<TyValueType> _srcty,
                    std::shared_ptr<TyValueType> _midty,
                    std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsBitcastInttoptr : public TyInfrule {
public:
  ConsBitcastInttoptr(std::shared_ptr<TyBitcastInttoptr> _bitcast_inttoptr);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyBitcastInttoptr> bitcast_inttoptr;
};

struct TyPtrtointInttoptr{
public : 
  TyPtrtointInttoptr(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid, std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty, std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsPtrtointInttoptr : public TyInfrule{
public : 
  ConsPtrtointInttoptr(std::shared_ptr<TyPtrtointInttoptr> _ptrtoint_inttoptr);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid, std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty, std::shared_ptr<TyValueType> _midty, std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyPtrtointInttoptr> ptrtoint_inttoptr;
};

struct TyTruncPtrtoint {
public:
  TyTruncPtrtoint(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
                  std::shared_ptr<TyValue> _dst,
                  std::shared_ptr<TyValueType> _srcty,
                  std::shared_ptr<TyValueType> _midty,
                  std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsTruncPtrtoint : public TyInfrule {
public:
  ConsTruncPtrtoint(std::shared_ptr<TyTruncPtrtoint> _ptrtoint_trunc);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyTruncPtrtoint> ptrtoint_trunc;
};

struct TyBitcastSext {
public:
  TyBitcastSext(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
                std::shared_ptr<TyValue> _dst,
                std::shared_ptr<TyValueType> _srcty,
                std::shared_ptr<TyValueType> _midty,
                std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsBitcastSext : public TyInfrule {
public:
  ConsBitcastSext(std::shared_ptr<TyBitcastSext> _bitcast_sext);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyBitcastSext> bitcast_sext;
};

struct TySextSext {
public:
  TySextSext(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
             std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
             std::shared_ptr<TyValueType> _midty,
             std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsSextSext : public TyInfrule {
public:
  ConsSextSext(std::shared_ptr<TySextSext> _sext_sext);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySextSext> sext_sext;
};

struct TySitofpSext {
public:
  TySitofpSext(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
               std::shared_ptr<TyValue> _dst,
               std::shared_ptr<TyValueType> _srcty,
               std::shared_ptr<TyValueType> _midty,
               std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsSitofpSext : public TyInfrule {
public:
  ConsSitofpSext(std::shared_ptr<TySitofpSext> _sext_sitofp);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySitofpSext> sext_sitofp;
};

struct TyTruncSext {
public:
  TyTruncSext(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
              std::shared_ptr<TyValue> _dst,
              std::shared_ptr<TyValueType> _srcty,
              std::shared_ptr<TyValueType> _midty,
              std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsTruncSext : public TyInfrule {
public:
  ConsTruncSext(std::shared_ptr<TyTruncSext> _trunc_sext);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyTruncSext> trunc_sext;
};

struct TyBitcastSitofp {
public:
  TyBitcastSitofp(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
                  std::shared_ptr<TyValue> _dst,
                  std::shared_ptr<TyValueType> _srcty,
                  std::shared_ptr<TyValueType> _midty,
                  std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsBitcastSitofp : public TyInfrule {
public:
  ConsBitcastSitofp(std::shared_ptr<TyBitcastSitofp> _bitcast_sitofp);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyBitcastSitofp> bitcast_sitofp;
};

struct TyBitcastTrunc {
public:
  TyBitcastTrunc(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
                 std::shared_ptr<TyValue> _dst,
                 std::shared_ptr<TyValueType> _srcty,
                 std::shared_ptr<TyValueType> _midty,
                 std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsBitcastTrunc : public TyInfrule {
public:
  ConsBitcastTrunc(std::shared_ptr<TyBitcastTrunc> _bitcast_trunc);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyBitcastTrunc> bitcast_trunc;
};

struct TyTruncTrunc {
public:
  TyTruncTrunc(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
               std::shared_ptr<TyValue> _dst,
               std::shared_ptr<TyValueType> _srcty,
               std::shared_ptr<TyValueType> _midty,
               std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsTruncTrunc : public TyInfrule {
public:
  ConsTruncTrunc(std::shared_ptr<TyTruncTrunc> _trunc_trunc);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyTruncTrunc> trunc_trunc;
};

struct TyBitcastUitofp {
public:
  TyBitcastUitofp(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
                  std::shared_ptr<TyValue> _dst,
                  std::shared_ptr<TyValueType> _srcty,
                  std::shared_ptr<TyValueType> _midty,
                  std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsBitcastUitofp : public TyInfrule {
public:
  ConsBitcastUitofp(std::shared_ptr<TyBitcastUitofp> _bitcast_uitofp);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyBitcastUitofp> bitcast_uitofp;
};

struct TyBitcastZext {
public:
  TyBitcastZext(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
                std::shared_ptr<TyValue> _dst,
                std::shared_ptr<TyValueType> _srcty,
                std::shared_ptr<TyValueType> _midty,
                std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsBitcastZext : public TyInfrule {
public:
  ConsBitcastZext(std::shared_ptr<TyBitcastZext> _bitcast_zext);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyBitcastZext> bitcast_zext;
};

struct TySextZext {
public:
  TySextZext(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
             std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
             std::shared_ptr<TyValueType> _midty,
             std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsSextZext : public TyInfrule {
public:
  ConsSextZext(std::shared_ptr<TySextZext> _zext_sext);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySextZext> zext_sext;
};

struct TySitofpZext {
public:
  TySitofpZext(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
               std::shared_ptr<TyValue> _dst,
               std::shared_ptr<TyValueType> _srcty,
               std::shared_ptr<TyValueType> _midty,
               std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsSitofpZext : public TyInfrule {
public:
  ConsSitofpZext(std::shared_ptr<TySitofpZext> _sitofp_zext);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySitofpZext> sitofp_zext;
};

struct TyTruncZext {
public:
  TyTruncZext(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
              std::shared_ptr<TyValue> _dst,
              std::shared_ptr<TyValueType> _srcty,
              std::shared_ptr<TyValueType> _midty,
              std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsTruncZext : public TyInfrule {
public:
  ConsTruncZext(std::shared_ptr<TyTruncZext> _trunc_zext);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyTruncZext> trunc_zext;
};

struct TyUitofpZext {
public:
  TyUitofpZext(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
               std::shared_ptr<TyValue> _dst,
               std::shared_ptr<TyValueType> _srcty,
               std::shared_ptr<TyValueType> _midty,
               std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsUitofpZext : public TyInfrule {
public:
  ConsUitofpZext(std::shared_ptr<TyUitofpZext> _zext_uitofp);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyUitofpZext> zext_uitofp;
};

struct TyZextZext {
public:
  TyZextZext(std::shared_ptr<TyValue> _src, std::shared_ptr<TyValue> _mid,
             std::shared_ptr<TyValue> _dst, std::shared_ptr<TyValueType> _srcty,
             std::shared_ptr<TyValueType> _midty,
             std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsZextZext : public TyInfrule {
public:
  ConsZextZext(std::shared_ptr<TyZextZext> _zext_zext);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyZextZext> zext_zext;
};

struct TyTruncOnebit {
public:
  TyTruncOnebit(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
                std::shared_ptr<TyValue> _y, std::shared_ptr<TySize> _orgsz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> orgsz;
};

struct ConsTruncOnebit : public TyInfrule {
public:
  ConsTruncOnebit(std::shared_ptr<TyTruncOnebit> _trunc_onebit);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TySize> _orgsz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyTruncOnebit> trunc_onebit;
};

struct TyZextTruncAndXor {
public:
  TyZextTruncAndXor(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
                    std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _w,
                    std::shared_ptr<TyValue> _y,
                    std::shared_ptr<TyValue> _yprime,
                    std::shared_ptr<TyConstant> _c, std::shared_ptr<TySize> _s,
                    std::shared_ptr<TySize> _sprime);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> v;
  std::shared_ptr<TyValue> w;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> yprime;
  std::shared_ptr<TyConstant> c;
  std::shared_ptr<TySize> s;
  std::shared_ptr<TySize> sprime;
};

struct ConsZextTruncAndXor : public TyInfrule {
public:
  ConsZextTruncAndXor(std::shared_ptr<TyZextTruncAndXor> _zext_trunc_and_xor);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
       std::shared_ptr<TyValue> _v, std::shared_ptr<TyValue> _w,
       std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _yprime,
       std::shared_ptr<TyConstant> _c, std::shared_ptr<TySize> _s,
       std::shared_ptr<TySize> _sprime);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyZextTruncAndXor> zext_trunc_and_xor;
};

struct TyZextXor {
public:
  TyZextXor(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _y,
            std::shared_ptr<TyValue> _yprime, std::shared_ptr<TyValue> _x,
            std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> yprime;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TySize> s;
};

struct ConsZextXor : public TyInfrule {
public:
  ConsZextXor(std::shared_ptr<TyZextXor> _zext_xor);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TyValue> _yprime,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyZextXor> zext_xor;
};

struct TySextTruncAshr{
public : 
  TySextTruncAshr(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _xprime, std::shared_ptr<TyValue> _v, std::shared_ptr<TySize> _s1, std::shared_ptr<TySize> _s2, std::shared_ptr<TyConstInt> _i3);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> xprime;
  std::shared_ptr<TyValue> v;
  std::shared_ptr<TySize> s1;
  std::shared_ptr<TySize> s2;
  std::shared_ptr<TyConstInt> i3;
};

struct ConsSextTruncAshr : public TyInfrule{
public : 
  ConsSextTruncAshr(std::shared_ptr<TySextTruncAshr> _sext_trunc_ashr);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _xprime, std::shared_ptr<TyValue> _v, std::shared_ptr<TySize> _s1, std::shared_ptr<TySize> _s2, std::shared_ptr<TyConstInt> _i3);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TySextTruncAshr> sext_trunc_ashr;
};

struct TyBitcastSametype {
public:
  TyBitcastSametype(std::shared_ptr<TyValue> _src,
                    std::shared_ptr<TyValue> _dst,
                    std::shared_ptr<TyValueType> _tty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> tty;
};

struct ConsBitcastSametype : public TyInfrule {
public:
  ConsBitcastSametype(std::shared_ptr<TyBitcastSametype> _bitcast_sametype);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _tty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyBitcastSametype> bitcast_sametype;
};

struct TyBitcastPtrtoint {
public:
  TyBitcastPtrtoint(std::shared_ptr<TyValue> _src,
                    std::shared_ptr<TyValue> _mid,
                    std::shared_ptr<TyValue> _dst,
                    std::shared_ptr<TyValueType> _srcty,
                    std::shared_ptr<TyValueType> _midty,
                    std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> src;
  std::shared_ptr<TyValue> mid;
  std::shared_ptr<TyValue> dst;
  std::shared_ptr<TyValueType> srcty;
  std::shared_ptr<TyValueType> midty;
  std::shared_ptr<TyValueType> dstty;
};

struct ConsBitcastPtrtoint : public TyInfrule {
public:
  ConsBitcastPtrtoint(std::shared_ptr<TyBitcastPtrtoint> _bitcast_ptrtoint);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _src,
                                         std::shared_ptr<TyValue> _mid,
                                         std::shared_ptr<TyValue> _dst,
                                         std::shared_ptr<TyValueType> _srcty,
                                         std::shared_ptr<TyValueType> _midty,
                                         std::shared_ptr<TyValueType> _dstty);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyBitcastPtrtoint> bitcast_ptrtoint;
};

} // llvmberry

#endif
