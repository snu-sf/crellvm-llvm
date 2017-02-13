#ifndef INFRULES_CASTS
#define INFRUELS_CASTS

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



#endif
