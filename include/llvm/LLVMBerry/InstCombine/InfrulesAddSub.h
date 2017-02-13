#ifndef INFRULES_ADD_SUB
#define INFRUELS_ADD_SUB

struct TyAddSignbit {
public:
  TyAddSignbit(std::shared_ptr<TyRegister> _x, std::shared_ptr<TyValue> _e1,
               std::shared_ptr<TyValue> _e2, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> x;
  std::shared_ptr<TyValue> e1;
  std::shared_ptr<TyValue> e2;
  std::shared_ptr<TySize> sz;
};

struct ConsAddSignbit : TyInfrule {
public:
  ConsAddSignbit(std::shared_ptr<TyAddSignbit> _add_signbit);
  void serialize(cereal::JSONOutputArchive &archive) const;
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _x,
                                         std::shared_ptr<TyValue> _e1,
                                         std::shared_ptr<TyValue> _e2,
                                         std::shared_ptr<TySize> _sz);

private:
  std::shared_ptr<TyAddSignbit> add_signbit;
};

struct TyAddZextBool {
public:
  TyAddZextBool(std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y,
                std::shared_ptr<TyValue> _b, std::shared_ptr<TyConstInt> _c,
                std::shared_ptr<TyConstInt> _cprime,
                std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> x;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TyConstInt> c;
  std::shared_ptr<TyConstInt> cprime;
  std::shared_ptr<TySize> sz;
};

struct ConsAddZextBool : public TyInfrule {
public:
  ConsAddZextBool(std::shared_ptr<TyAddZextBool> _add_zext_bool);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyRegister> _x, std::shared_ptr<TyRegister> _y,
       std::shared_ptr<TyValue> _b, std::shared_ptr<TyConstInt> _c,
       std::shared_ptr<TyConstInt> _cprime, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyAddZextBool> add_zext_bool;
};

struct TyAddOnebit {
public:
  TyAddOnebit(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x,
              std::shared_ptr<TyValue> _y);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
};

struct ConsAddOnebit : public TyInfrule {
public:
  ConsAddOnebit(std::shared_ptr<TyAddOnebit> _add_onebit);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _y);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyAddOnebit> add_onebit;
};

struct TyAddShift {
public:
  TyAddShift(std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> v,
             std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyValue> v;
  std::shared_ptr<TySize> sz;
};

struct ConsAddShift : TyInfrule {
public:
  ConsAddShift(std::shared_ptr<TyAddShift> _add_shift);
  void serialize(cereal::JSONOutputArchive &archive) const;
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _y,
                                         std::shared_ptr<TyValue> _v,
                                         std::shared_ptr<TySize> _sz);

private:
  std::shared_ptr<TyAddShift> add_shift;
};

struct TyAddDistSub {
public:
  TyAddDistSub(std::shared_ptr<TyRegister> _z,
               std::shared_ptr<TyRegister> _minusx,
               std::shared_ptr<TyValue> _minusy, std::shared_ptr<TyRegister> _w,
               std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
               std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> minusx;
  std::shared_ptr<TyValue> minusy;
  std::shared_ptr<TyRegister> w;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> sz;
};

struct ConsAddDistSub : public TyInfrule {
public:
  ConsAddDistSub(std::shared_ptr<TyAddDistSub> _add_dist_sub);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _minusx,
       std::shared_ptr<TyValue> _minusy, std::shared_ptr<TyRegister> _w,
       std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
       std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyAddDistSub> add_dist_sub;
};

struct TyAddSub {
public:
  TyAddSub(std::shared_ptr<TyRegister> _minusy, std::shared_ptr<TyRegister> _z,
           std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
           std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> minusy;
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> sz;
};

struct ConsAddSub : TyInfrule {
public:
  ConsAddSub(std::shared_ptr<TyAddSub> _add_sub);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _minusy,
                                         std::shared_ptr<TyRegister> _z,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TySize> _sz);

private:
  std::shared_ptr<TyAddSub> add_sub;
};

struct TyAddConstNot {
public:
  TyAddConstNot(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y,
                std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c1,
                std::shared_ptr<TyConstInt> _c2, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyConstInt> c1;
  std::shared_ptr<TyConstInt> c2;
  std::shared_ptr<TySize> sz;
};

struct ConsAddConstNot : public TyInfrule {
public:
  ConsAddConstNot(std::shared_ptr<TyAddConstNot> _add_const_not);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y,
       std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c1,
       std::shared_ptr<TyConstInt> _c2, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyAddConstNot> add_const_not;
};

struct TySubAdd {
public:
  TySubAdd(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _my,
           std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
           std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> my;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> sz;
};

struct ConsSubAdd : TyInfrule {
public:
  ConsSubAdd(std::shared_ptr<TySubAdd> _sub_add);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z,
                                         std::shared_ptr<TyValue> _my,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TySize> _sz);

private:
  std::shared_ptr<TySubAdd> sub_add;
};

struct TySubOnebit {
public:
  TySubOnebit(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x,
              std::shared_ptr<TyValue> _y);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
};

struct ConsSubOnebit : public TyInfrule {
public:
  ConsSubOnebit(std::shared_ptr<TySubOnebit> _sub_onebit);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _y);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySubOnebit> sub_onebit;
};

struct TySubMone {
public:
  TySubMone(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x,
            std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TySize> sz;
};

struct ConsSubMone : public TyInfrule {
public:
  ConsSubMone(std::shared_ptr<TySubMone> _sub_mone);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySubMone> sub_mone;
};

struct TySubConstNot {
public:
  TySubConstNot(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y,
                std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c1,
                std::shared_ptr<TyConstInt> _c2, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyConstInt> c1;
  std::shared_ptr<TyConstInt> c2;
  std::shared_ptr<TySize> sz;
};

struct ConsSubConstNot : public TyInfrule {
public:
  ConsSubConstNot(std::shared_ptr<TySubConstNot> _sub_const_not);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y,
       std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c1,
       std::shared_ptr<TyConstInt> _c2, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySubConstNot> sub_const_not;
};

struct TySubConstAdd {
public:
  TySubConstAdd(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y,
                std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c1,
                std::shared_ptr<TyConstInt> _c2,
                std::shared_ptr<TyConstInt> _c3, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyConstInt> c1;
  std::shared_ptr<TyConstInt> c2;
  std::shared_ptr<TyConstInt> c3;
  std::shared_ptr<TySize> sz;
};

struct ConsSubConstAdd : public TyInfrule {
public:
  ConsSubConstAdd(std::shared_ptr<TySubConstAdd> _sub_const_add);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y,
       std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c1,
       std::shared_ptr<TyConstInt> _c2, std::shared_ptr<TyConstInt> _c3,
       std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySubConstAdd> sub_const_add;
};

struct TySubRemove {
public:
  TySubRemove(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y,
              std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
              std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> sz;
};

struct ConsSubRemove : TyInfrule {
public:
  ConsSubRemove(std::shared_ptr<TySubRemove> _sub_remove);
  void serialize(cereal::JSONOutputArchive &archive) const;

  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z,
                                         std::shared_ptr<TyRegister> _y,
                                         std::shared_ptr<TyValue> _a,
                                         std::shared_ptr<TyValue> _b,
                                         std::shared_ptr<TySize> _sz);

private:
  std::shared_ptr<TySubRemove> sub_remove;
};

struct TySubSub {
public:
  TySubSub(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x,
           std::shared_ptr<TyValue> _y, std::shared_ptr<TyValue> _w,
           std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> w;
  std::shared_ptr<TySize> sz;
};

struct ConsSubSub : public TyInfrule {
public:
  ConsSubSub(std::shared_ptr<TySubSub> _sub_sub);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyRegister> _z,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TyValue> _w,
                                         std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySubSub> sub_sub;
};

struct TySubOrXor {
public:
  TySubOrXor(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _a,
             std::shared_ptr<TyValue> _b, std::shared_ptr<TyRegister> _x,
             std::shared_ptr<TyRegister> _y, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TyRegister> x;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TySize> sz;
};

struct ConsSubOrXor : public TyInfrule {
public:
  ConsSubOrXor(std::shared_ptr<TySubOrXor> _sub_or_xor);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _a,
       std::shared_ptr<TyValue> _b, std::shared_ptr<TyRegister> _x,
       std::shared_ptr<TyRegister> _y, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySubOrXor> sub_or_xor;
};

struct TySubSdiv {
public:
  TySubSdiv(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y,
            std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c,
            std::shared_ptr<TyConstInt> _cprime, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyConstInt> c;
  std::shared_ptr<TyConstInt> cprime;
  std::shared_ptr<TySize> sz;
};

struct ConsSubSdiv : public TyInfrule {
public:
  ConsSubSdiv(std::shared_ptr<TySubSdiv> _sub_sdiv);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyRegister> _y,
       std::shared_ptr<TyValue> _x, std::shared_ptr<TyConstInt> _c,
       std::shared_ptr<TyConstInt> _cprime, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySubSdiv> sub_sdiv;
};

struct TySubShl {
public:
  TySubShl(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x,
           std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _mx,
           std::shared_ptr<TyValue> _a, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyRegister> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyRegister> y;
  std::shared_ptr<TyValue> mx;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TySize> sz;
};

struct ConsSubShl : public TyInfrule {
public:
  ConsSubShl(std::shared_ptr<TySubShl> _sub_shl);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyRegister> _z, std::shared_ptr<TyValue> _x,
       std::shared_ptr<TyRegister> _y, std::shared_ptr<TyValue> _mx,
       std::shared_ptr<TyValue> _a, std::shared_ptr<TySize> _sz);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TySubShl> sub_shl;
};


#endif
