#ifndef INFRULES_COMPARES
#define INFRUELS_COMPARES

#include "llvm/Crellvm/Structure.h"
#include "llvm/IR/Instructions.h"
#include "cereal/types/memory.hpp"
#include "cereal/archives/json.hpp"

namespace crellvm {

struct TyIcmpEqXorNot{
public : 
  TyIcmpEqXorNot(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> zprime;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> s;
};

struct ConsIcmpEqXorNot : public TyInfrule {
public : 
  ConsIcmpEqXorNot(std::shared_ptr<TyIcmpEqXorNot> _icmp_eq_xor_not);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyIcmpEqXorNot> icmp_eq_xor_not;
};

struct TyIcmpNeXor {
public : 
  TyIcmpNeXor(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> s;
};

struct ConsIcmpNeXor : public TyInfrule {
public : 
  ConsIcmpNeXor(std::shared_ptr<TyIcmpNeXor> _icmp_ne_xor);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyIcmpNeXor> icmp_ne_xor;
};

struct TyIcmpUgtAndNot{
public : 
  TyIcmpUgtAndNot(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> zprime;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> s;
};

struct ConsIcmpUgtAndNot : public TyInfrule{
public : 
  ConsIcmpUgtAndNot(std::shared_ptr<TyIcmpUgtAndNot> _icmp_ugt_and_not);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyIcmpUgtAndNot> icmp_ugt_and_not;
};

struct TyIcmpUltAndNot{
public : 
  TyIcmpUltAndNot(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> zprime;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> s;
};

struct ConsIcmpUltAndNot : public TyInfrule{
public : 
  ConsIcmpUltAndNot(std::shared_ptr<TyIcmpUltAndNot> _icmp_ult_and_not);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyIcmpUltAndNot> icmp_ult_and_not;
};

struct TyIcmpSgtAndNot{
public : 
  TyIcmpSgtAndNot(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> zprime;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> s;
};

struct ConsIcmpSgtAndNot : public TyInfrule{
public : 
  ConsIcmpSgtAndNot(std::shared_ptr<TyIcmpSgtAndNot> _icmp_sgt_and_not);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyIcmpSgtAndNot> icmp_sgt_and_not;
};

struct TyIcmpSltAndNot{
public : 
  TyIcmpSltAndNot(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> zprime;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> s;
};

struct ConsIcmpSltAndNot : public TyInfrule{
public : 
  ConsIcmpSltAndNot(std::shared_ptr<TyIcmpSltAndNot> _icmp_slt_and_not);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyIcmpSltAndNot> icmp_slt_and_not;
};

struct TyIcmpUgeOrNot{
public : 
  TyIcmpUgeOrNot(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> zprime;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> s;
};

struct ConsIcmpUgeOrNot : public TyInfrule{
public : 
  ConsIcmpUgeOrNot(std::shared_ptr<TyIcmpUgeOrNot> _icmp_uge_or_not);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyIcmpUgeOrNot> icmp_uge_or_not;
};

struct TyIcmpUleOrNot{
public : 
  TyIcmpUleOrNot(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> zprime;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> s;
};

struct ConsIcmpUleOrNot : public TyInfrule{
public : 
  ConsIcmpUleOrNot(std::shared_ptr<TyIcmpUleOrNot> _icmp_sle_or_not);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyIcmpUleOrNot> icmp_ule_or_not;
};

struct TyIcmpSgeOrNot{
public : 
  TyIcmpSgeOrNot(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> zprime;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> s;
};

struct ConsIcmpSgeOrNot : public TyInfrule {
public : 
  ConsIcmpSgeOrNot(std::shared_ptr<TyIcmpSgeOrNot> _icmp_sge_or_not);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyIcmpSgeOrNot> icmp_sge_or_not;
};

struct TyIcmpSleOrNot{
public : 
  TyIcmpSleOrNot(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> zprime;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> s;
};

struct ConsIcmpSleOrNot : public TyInfrule{
public : 
  ConsIcmpSleOrNot(std::shared_ptr<TyIcmpSleOrNot> _icmp_sle_or_not);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _zprime, std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b, std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive& archive) const;

private : 
  std::shared_ptr<TyIcmpSleOrNot> icmp_sle_or_not;
};

struct TyIcmpEqSub {
public:
  TyIcmpEqSub(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
              std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
              std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> s;
};

struct ConsIcmpEqSub : public TyInfrule {
public:
  ConsIcmpEqSub(std::shared_ptr<TyIcmpEqSub> _icmp_eq_sub);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _a,
                                         std::shared_ptr<TyValue> _b,
                                         std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyIcmpEqSub> icmp_eq_sub;
};

struct TyIcmpNeSub {
public:
  TyIcmpNeSub(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _x,
              std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
              std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> s;
};

struct ConsIcmpNeSub : public TyInfrule {
public:
  ConsIcmpNeSub(std::shared_ptr<TyIcmpNeSub> _icmp_ne_sub);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _a,
                                         std::shared_ptr<TyValue> _b,
                                         std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyIcmpNeSub> icmp_ne_sub;
};

struct TyIcmpEqSrem {
public:
  TyIcmpEqSrem(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _w,
               std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
               std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> w;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> s;
};

struct ConsIcmpEqSrem : public TyInfrule {
public:
  ConsIcmpEqSrem(std::shared_ptr<TyIcmpEqSrem> _icmp_eq_srem);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z,
                                         std::shared_ptr<TyValue> _w,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyIcmpEqSrem> icmp_eq_srem;
};

struct TyIcmpNeSrem {
public:
  TyIcmpNeSrem(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _w,
               std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
               std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> w;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TySize> s;
};

struct ConsIcmpNeSrem : public TyInfrule {
public:
  ConsIcmpNeSrem(std::shared_ptr<TyIcmpNeSrem> _icmp_ne_srem);
  static std::shared_ptr<TyInfrule> make(std::shared_ptr<TyValue> _z,
                                         std::shared_ptr<TyValue> _w,
                                         std::shared_ptr<TyValue> _x,
                                         std::shared_ptr<TyValue> _y,
                                         std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyIcmpNeSrem> icmp_ne_srem;
};

struct TyIcmpEqAddAdd {
public:
  TyIcmpEqAddAdd(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _w,
                 std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
                 std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
                 std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> w;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> s;
};

struct ConsIcmpEqAddAdd : public TyInfrule {
public:
  ConsIcmpEqAddAdd(std::shared_ptr<TyIcmpEqAddAdd> _icmp_eq_add_add);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _w,
       std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
       std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
       std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyIcmpEqAddAdd> icmp_eq_add_add;
};

struct TyIcmpEqSubSub {
public:
  TyIcmpEqSubSub(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _w,
                 std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
                 std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
                 std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> w;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> s;
};

struct ConsIcmpEqSubSub : public TyInfrule {
public:
  ConsIcmpEqSubSub(std::shared_ptr<TyIcmpEqSubSub> _icmp_eq_sub_sub);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _w,
       std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
       std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
       std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyIcmpEqSubSub> icmp_eq_sub_sub;
};

struct TyIcmpEqXorXor {
public:
  TyIcmpEqXorXor(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _w,
                 std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
                 std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
                 std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> w;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> s;
};

struct ConsIcmpEqXorXor : public TyInfrule {
public:
  ConsIcmpEqXorXor(std::shared_ptr<TyIcmpEqXorXor> _icmp_eq_xor_xor);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _w,
       std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
       std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
       std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyIcmpEqXorXor> icmp_eq_xor_xor;
};

struct TyIcmpNeAddAdd {
public:
  TyIcmpNeAddAdd(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _w,
                 std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
                 std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
                 std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> w;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> s;
};

struct ConsIcmpNeAddAdd : public TyInfrule {
public:
  ConsIcmpNeAddAdd(std::shared_ptr<TyIcmpNeAddAdd> _icmp_ne_add_add);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _w,
       std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
       std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
       std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyIcmpNeAddAdd> icmp_ne_add_add;
};

struct TyIcmpNeSubSub {
public:
  TyIcmpNeSubSub(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _w,
                 std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
                 std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
                 std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> w;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> s;
};

struct ConsIcmpNeSubSub : public TyInfrule {
public:
  ConsIcmpNeSubSub(std::shared_ptr<TyIcmpNeSubSub> _icmp_ne_sub_sub);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _w,
       std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
       std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
       std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyIcmpNeSubSub> icmp_ne_sub_sub;
};

struct TyIcmpNeXorXor {
public:
  TyIcmpNeXorXor(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _w,
                 std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
                 std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
                 std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyValue> z;
  std::shared_ptr<TyValue> w;
  std::shared_ptr<TyValue> x;
  std::shared_ptr<TyValue> y;
  std::shared_ptr<TyValue> a;
  std::shared_ptr<TyValue> b;
  std::shared_ptr<TySize> s;
};

struct ConsIcmpNeXorXor : public TyInfrule {
public:
  ConsIcmpNeXorXor(std::shared_ptr<TyIcmpNeXorXor> _icmp_ne_xor_xor);
  static std::shared_ptr<TyInfrule>
  make(std::shared_ptr<TyValue> _z, std::shared_ptr<TyValue> _w,
       std::shared_ptr<TyValue> _x, std::shared_ptr<TyValue> _y,
       std::shared_ptr<TyValue> _a, std::shared_ptr<TyValue> _b,
       std::shared_ptr<TySize> _s);
  void serialize(cereal::JSONOutputArchive &archive) const;

private:
  std::shared_ptr<TyIcmpNeXorXor> icmp_ne_xor_xor;
};

} // crellvm

#endif
