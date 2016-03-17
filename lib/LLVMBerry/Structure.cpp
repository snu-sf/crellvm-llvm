#include <sstream>
#include <fstream>
#include <string>
#include <cassert>
#include <cereal/archives/json.hpp>
#include <cereal/types/vector.hpp>
#include <cereal/types/memory.hpp>
#include <cereal/types/polymorphic.hpp>
#include "llvm/LLVMBerry/Structure.h"

namespace cereal {
  [[noreturn]] void throw_exception(std::exception const &e) { std::exit(1); }

  template <class T>
  void save( cereal::JSONOutputArchive &archive, std::unique_ptr<T> const &ptr) {
    ptr->serialize(archive);
  }
} // cereal

namespace {

  std::string toString(llvmberry::TyScope scope) {
    switch (scope) {
    case llvmberry::Source:
      return std::string("Source");
    case llvmberry::Target:
      return std::string("Target");
    default:
      assert(false && "Scope toString");
    }
  }

  std::string toString(llvmberry::TyTag tag) {
    switch (tag) {
    case llvmberry::Physical:
      return std::string("Physical");
    case llvmberry::Previous:
      return std::string("Previous");
    case llvmberry::Ghost:
      return std::string("Ghost");
    default:
      assert(false && "Tag toString");
    }
  }

  std::string toString(llvmberry::TyFloatType float_type) {
    switch (float_type) {
    case llvmberry::FloatType:
      return std::string("FloatType");
    case llvmberry::DoubleType:
      return std::string("DoubleType");
    case llvmberry::FP128Type:
      return std::string("FP128Type");
    case llvmberry::X86_FP80Type:
      return std::string("X86_FP80Type");
    default:
      assert(false && "FloatType toString");
    }
  }

} // anonymous

namespace llvmberry {

  std::string getVariable(const llvm::Value &value) {
    std::string val;

    if (llvm::isa<llvm::GlobalValue>(value)) {
      val = std::string("@");
    } else {
      val = std::string("%");
    }

    val += std::string(value.getName().data());

    return val;
  }

  bool name_instructions(llvm::Function &F) {
    for (llvm::Function::arg_iterator AI = F.arg_begin(), AE = F.arg_end();
         AI != AE; ++AI)
      if (!AI->hasName() && !AI->getType()->isVoidTy())
        AI->setName("arg");

    for (llvm::Function::iterator BB = F.begin(), E = F.end(); BB != E; ++BB) {
      if (!BB->hasName())
        BB->setName("bb");

      for (llvm::BasicBlock::iterator I = BB->begin(), E = BB->end(); I != E; ++I)
        if (!I->hasName() && !I->getType()->isVoidTy())
          I->setName("tmp");
    }

    return true;
  }

  /* position */

  TyPositionPhinode::TyPositionPhinode(std::string _block_name, std::string _prev_block_name)
    : block_name(_block_name), prev_block_name(_prev_block_name) {}

  void TyPositionPhinode::serialize(cereal::JSONOutputArchive &archive) const {
    archive(CEREAL_NVP(block_name), CEREAL_NVP(prev_block_name));
  }

  TyPositionCommand::TyPositionCommand(enum TyScope _scope, std::string _register_name)
    : scope(_scope), register_name(_register_name) {}

  void TyPositionCommand::serialize(cereal::JSONOutputArchive &archive) const {
    archive(cereal::make_nvp("scope", toString(scope)),
            CEREAL_NVP(register_name));
  }

  ConsPhinode::ConsPhinode(std::unique_ptr<TyPositionPhinode> _position_phinode)
    : position_phinode(std::move(_position_phinode)) {}

  ConsPhinode::ConsPhinode(std::string _block_name, std::string _prev_block_name)
    : position_phinode(new TyPositionPhinode(_block_name, _prev_block_name)) { }

  void ConsPhinode::serialize(cereal::JSONOutputArchive &archive) const {
    archive.makeArray();
    archive.writeName();

    archive.saveValue("Phinode");
    archive(CEREAL_NVP(position_phinode));
  }

  ConsCommand::ConsCommand(std::unique_ptr<TyPositionCommand> _position_command)
    : position_command(std::move(_position_command)) {}

  ConsCommand::ConsCommand(enum TyScope _scope, std::string _register_name)
    : position_command(new TyPositionCommand(_scope, _register_name)) {}

  void ConsCommand::serialize(cereal::JSONOutputArchive &archive) const {
    archive.makeArray();
    archive.writeName();

    archive.saveValue("Command");
    archive(CEREAL_NVP(position_command));
  }

  std::unique_ptr<TyPosition> ConsCommand::make(enum TyScope _scope, std::string _register_name) {
    return std::unique_ptr<TyPosition>(new ConsCommand(_scope, _register_name));
  }

  /* value */

  // register

  TyRegister::TyRegister(std::string _name, enum TyTag _tag)
    : name(_name), tag(_tag) {}

  void TyRegister::serialize(cereal::JSONOutputArchive &archive) const {
    archive(CEREAL_NVP(name), cereal::make_nvp("tag", toString(tag)));
  }

  std::unique_ptr<TyRegister> TyRegister::make(std::string _name, enum TyTag _tag) {
    return std::unique_ptr<TyRegister>(new TyRegister(_name, _tag));
  }

  // constant
  ConsIntType::ConsIntType(int _value) : value(_value) {}

  void ConsIntType::serialize(cereal::JSONOutputArchive &archive) const {
    archive.makeArray();
    archive.writeName();

    archive.saveValue("IntType");
    archive(CEREAL_NVP(value));
  }

  TyConstInt::TyConstInt(int _int_value, std::unique_ptr<TyIntType> _int_type)
    : int_value(_int_value), int_type(std::move(_int_type)) { }

  TyConstInt::TyConstInt(int _int_value, int _value)
    : int_value(_int_value), int_type(new ConsIntType(_value)) { }

  void TyConstInt::serialize(cereal::JSONOutputArchive &archive) const {
    archive(cereal::make_nvp("int_value", int_value), CEREAL_NVP(int_type));
  }

  std::unique_ptr<TyConstInt> TyConstInt::make(int _int_value, int _value) {
    return std::unique_ptr<TyConstInt>(new TyConstInt(_int_value, _value));
  }

  TyConstFloat::TyConstFloat(float _float_value, enum TyFloatType _float_type)
    : float_value(_float_value), float_type(_float_type){ }

  void TyConstFloat::serialize(cereal::JSONOutputArchive &archive) const {
    archive(CEREAL_NVP(float_value),
            cereal::make_nvp("float_type", toString(float_type)));
  }

  ConsConstInt::ConsConstInt(std::unique_ptr<TyConstInt> _const_int)
    : const_int(std::move(_const_int)) { }

  ConsConstInt::ConsConstInt(int _int_value, int _value)
    : const_int(new TyConstInt(_int_value, _value)) { }

  void ConsConstInt::serialize(cereal::JSONOutputArchive &archive) const {
    archive.makeArray();
    archive.writeName();

    archive.saveValue("ConstInt");
    archive(CEREAL_NVP(const_int));
  }

  ConsConstFloat::ConsConstFloat(std::unique_ptr<TyConstFloat> _const_float)
    : const_float(std::move(_const_float)){ }

  ConsConstFloat::ConsConstFloat(float _float_value, enum TyFloatType _float_type)
    : const_float(new TyConstFloat(_float_value, _float_type)) { }

  void ConsConstFloat::serialize(cereal::JSONOutputArchive &archive) const {
    archive.makeArray();
    archive.writeName();

    archive.saveValue("ConstFloat");
    archive(CEREAL_NVP(const_float));
  }

  ConsSize::ConsSize(int _size) : size(_size) {}

  void ConsSize::serialize(cereal::JSONOutputArchive &archive) const {
    archive.makeArray();
    archive.writeName();

    archive.saveValue("Size");
    archive(CEREAL_NVP(size));
  }

  std::unique_ptr<TySize> ConsSize::make(int _size) {
    return std::unique_ptr<TySize>(new ConsSize(_size));
  }

  /* Propagate */

  // propagate expr

  ConsVar::ConsVar(std::unique_ptr<TyRegister> _register_name)
    : register_name(std::move(_register_name)) { }

  ConsVar::ConsVar(std::string _name, enum TyTag _tag)
    : register_name(new TyRegister(_name, _tag)) { }

  void ConsVar::serialize(cereal::JSONOutputArchive &archive) const {
    archive.makeArray();
    archive.writeName();

    archive.saveValue("Var");
    archive(CEREAL_NVP(register_name));
  }

  std::unique_ptr<TyPropagateExpr> ConsVar::make
  (std::string _name, enum TyTag _tag) {
    return std::unique_ptr<TyPropagateExpr>(new ConsVar(_name, _tag));
  }

  ConsRhs::ConsRhs(std::unique_ptr<TyRegister> _register_name)
    : register_name(std::move(_register_name)) {}

  ConsRhs::ConsRhs(std::string _name, enum TyTag _tag)
    : register_name(new TyRegister(_name, _tag)) { }

  void ConsRhs::serialize(cereal::JSONOutputArchive &archive) const {
    archive.makeArray();
    archive.writeName();

    archive.saveValue("Rhs");
    archive(CEREAL_NVP(register_name));
  }

  std::unique_ptr<TyPropagateExpr> ConsRhs::make(std::string _name, enum TyTag _tag) {
    return std::unique_ptr<TyPropagateExpr>(new ConsRhs(_name, _tag));
  }

  ConsConst::ConsConst(std::unique_ptr<TyConstant> _constant)
    : constant(std::move(_constant)) {}

  ConsConst::ConsConst(int _int_value, int _value)
    : constant(new ConsConstInt(_int_value, _value)) { }

  ConsConst::ConsConst(float _float_value, enum TyFloatType _float_type)
    : constant(new ConsConstFloat(_float_value, _float_type)) { }

  void ConsConst::serialize(cereal::JSONOutputArchive &archive) const {
    archive.makeArray();
    archive.writeName();

    archive.saveValue("Const");
    archive(CEREAL_NVP(constant));
  }

  // propagate object

  TyPropagateLessdef::TyPropagateLessdef
  (std::unique_ptr<TyPropagateExpr> _lhs,
   std::unique_ptr<TyPropagateExpr> _rhs,
   enum TyScope _scope)
    : lhs(std::move(_lhs)), rhs(std::move(_rhs)), scope(_scope) {}

  void TyPropagateLessdef::serialize(cereal::JSONOutputArchive &archive) const {
    archive(CEREAL_NVP(lhs), CEREAL_NVP(rhs), cereal::make_nvp("scope", toString(scope)));
  }

  std::unique_ptr<TyPropagateLessdef> TyPropagateLessdef::make
  (std::unique_ptr<TyPropagateExpr> _lhs,
   std::unique_ptr<TyPropagateExpr> _rhs,
   enum TyScope _scope) {
    return std::unique_ptr<TyPropagateLessdef>
      (new TyPropagateLessdef(std::move(_lhs), std::move(_rhs), _scope));

  }

  TyPropagateNoalias::TyPropagateNoalias
  (std::unique_ptr<TyRegister> _lhs,
   std::unique_ptr<TyRegister> _rhs,
   enum TyScope _scope)
    : lhs(std::move(_lhs)), rhs(std::move(_rhs)), scope(_scope) {}

  TyPropagateNoalias::TyPropagateNoalias
  (std::string _lhs_name, enum TyTag _lhs_tag,
   std::string _rhs_name, enum TyTag _rhs_tag,
   enum TyScope _scope)
    : lhs(new TyRegister(_lhs_name, _lhs_tag)),
      rhs(new TyRegister(_rhs_name, _rhs_tag)),
      scope(_scope) { }

  void TyPropagateNoalias::serialize(cereal::JSONOutputArchive &archive) const {
    archive(CEREAL_NVP(lhs), CEREAL_NVP(rhs), cereal::make_nvp("scope", toString(scope)));
  }

  ConsLessdef::ConsLessdef(std::unique_ptr<TyPropagateLessdef> _propagate_lessdef)
    : propagate_lessdef(std::move(_propagate_lessdef)) {}

  void ConsLessdef::serialize(cereal::JSONOutputArchive &archive) const {
    archive.makeArray();
    archive.writeName();

    archive.saveValue("Lessdef");
    archive(CEREAL_NVP(propagate_lessdef));
  }

  std::unique_ptr<TyPropagateObject> ConsLessdef::make
  (std::unique_ptr<TyPropagateExpr> _lhs,
   std::unique_ptr<TyPropagateExpr> _rhs,
   enum TyScope _scope) {
    auto ty_prop_ld = TyPropagateLessdef::make
      (std::move(_lhs), std::move(_rhs), std::move(_scope));
    return std::unique_ptr<TyPropagateObject>(new ConsLessdef(std::move(ty_prop_ld)));
  }

  ConsNoalias::ConsNoalias(std::unique_ptr<TyPropagateNoalias> _propagate_noalias)
    : propagate_noalias(std::move(_propagate_noalias)) { }

  ConsNoalias::ConsNoalias
  (std::string _lhs_name, enum TyTag _lhs_tag,
   std::string _rhs_name, enum TyTag _rhs_tag,
   enum TyScope _scope)
    : propagate_noalias(new TyPropagateNoalias(_lhs_name, _lhs_tag, _rhs_name, _rhs_tag, _scope)) { }

  void ConsNoalias::serialize(cereal::JSONOutputArchive &archive) const {
    archive.makeArray();
    archive.writeName();

    archive.saveValue("Noalias");
    archive(CEREAL_NVP(propagate_noalias));
  }

  ConsMaydiff::ConsMaydiff(std::unique_ptr<TyRegister> _register_name)
    : register_name(std::move(_register_name)) {}

  ConsMaydiff::ConsMaydiff(std::string _name, enum TyTag _tag)
    : register_name(new TyRegister(_name, _tag)) { }

  void ConsMaydiff::serialize(cereal::JSONOutputArchive &archive) const {
    archive.makeArray();
    archive.writeName();

    archive.saveValue("Maydiff");
    archive(CEREAL_NVP(register_name));
  }

  // propagate range

  ConsBounds::ConsBounds(std::unique_ptr<TyPosition> _from,
                         std::unique_ptr<TyPosition> _to)
    : from(std::move(_from)), to(std::move(_to)) {}

  void ConsBounds::serialize(cereal::JSONOutputArchive &archive) const {
    archive.makeArray();
    archive.writeName();

    archive.saveValue("Bounds");
    archive.startNode();
    archive.makeArray();
    archive(from, to);
    archive.finishNode();
  }

  std::unique_ptr<TyPropagateRange> ConsBounds::make
  (std::unique_ptr<TyPosition> _from,
   std::unique_ptr<TyPosition> _to) {
    return std::unique_ptr<TyPropagateRange>
      (new ConsBounds(std::move(_from), std::move(_to)));
  }

  ConsGlobal::ConsGlobal() {}

  void ConsGlobal::serialize(cereal::JSONOutputArchive &archive) const {
    archive.makeArray();
    archive.writeName();

    archive.saveValue("Global");
  }

  TyPropagate::TyPropagate(std::unique_ptr<TyPropagateObject> _propagate,
                           std::unique_ptr<TyPropagateRange> _propagate_range)
    : propagate(std::move(_propagate)),
      propagate_range(std::move(_propagate_range)) {}

  void TyPropagate::serialize(cereal::JSONOutputArchive &archive) const {
    archive(CEREAL_NVP(propagate), CEREAL_NVP(propagate_range));
  }

  /* inference rule */

  TyAddAssociative::TyAddAssociative
  (std::unique_ptr<TyRegister> _x,
   std::unique_ptr<TyRegister> _y,
   std::unique_ptr<TyRegister> _z,
   std::unique_ptr<TyConstInt> _c1,
   std::unique_ptr<TyConstInt> _c2,
   std::unique_ptr<TyConstInt> _c3,
   std::unique_ptr<TySize> _sz)
    : x(std::move(_x)), y(std::move(_y)), z(std::move(_z)),
      c1(std::move(_c1)), c2(std::move(_c2)), c3(std::move(_c3)),
      sz(std::move(_sz)) {}

  void TyAddAssociative::serialize(cereal::JSONOutputArchive &archive) const {
    archive(CEREAL_NVP(x), CEREAL_NVP(y), CEREAL_NVP(z),
            CEREAL_NVP(c1), CEREAL_NVP(c2), CEREAL_NVP(c3),
            CEREAL_NVP(sz));
  }

  ConsAddAssociative::ConsAddAssociative(std::unique_ptr<TyAddAssociative> _add_associative)
    : add_associative(std::move(_add_associative)) {}

  void ConsAddAssociative::serialize(cereal::JSONOutputArchive &archive) const {
    archive.makeArray();
    archive.writeName();

    archive.saveValue("AddAssociative");
    archive(CEREAL_NVP(add_associative));
  }

  std::unique_ptr<TyInfrule> ConsAddAssociative::make
  (std::unique_ptr<TyRegister> _x,
   std::unique_ptr<TyRegister> _y,
   std::unique_ptr<TyRegister> _z,
   std::unique_ptr<TyConstInt> _c1,
   std::unique_ptr<TyConstInt> _c2,
   std::unique_ptr<TyConstInt> _c3,
   std::unique_ptr<TySize> _sz) {
    std::unique_ptr<TyAddAssociative> _add_assoc
      (new TyAddAssociative
       (std::move(_x), std::move(_y), std::move(_z),
        std::move(_c1), std::move(_c2), std::move(_c3), std::move(_sz)));
    return std::unique_ptr<TyInfrule>(new ConsAddAssociative(std::move(_add_assoc)));
  }

  TySubAdd::TySubAdd
          (std::unique_ptr<TyRegister> _z,
           std::unique_ptr<TyRegister> _my,
           std::unique_ptr<TyRegister> _x,
           std::unique_ptr<TyRegister> _y,
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
           std::unique_ptr<TyRegister> _my,
           std::unique_ptr<TyRegister> _x,
           std::unique_ptr<TyRegister> _y,
           std::unique_ptr<TySize> _sz) {
    std::unique_ptr<TySubAdd> _sub_add
            (new TySubAdd
                     (std::move(_z), std::move(_my), std::move(_x),
                      std::move(_y), std::move(_sz)));
    return std::unique_ptr<TyInfrule>(new ConsSubAdd(std::move(_sub_add)));
  }

  ConsPropagate::ConsPropagate(std::unique_ptr<TyPropagate> _propagate)
    : propagate(std::move(_propagate)) {}

  void ConsPropagate::serialize(cereal::JSONOutputArchive &archive) const {
    archive.makeArray();
    archive.writeName();

    archive.saveValue("Propagate");
    archive(CEREAL_NVP(propagate));
  }

  std::unique_ptr<TyCommand> ConsPropagate::make
  (std::unique_ptr<TyPropagate> _propagate) {
    return std::unique_ptr<TyCommand>(new ConsPropagate(std::move(_propagate)));
  }

  std::unique_ptr<TyCommand> ConsPropagate::make
  (std::unique_ptr<TyPropagateObject> _obj,
   std::unique_ptr<TyPropagateRange> _range) {
    std::unique_ptr<TyPropagate> _propagate(new TyPropagate(std::move(_obj), std::move(_range)));
    return std::unique_ptr<TyCommand>(new ConsPropagate(std::move(_propagate)));
  }

  ConsInfrule::ConsInfrule(std::unique_ptr<TyPosition> _position,
                           std::unique_ptr<TyInfrule> _infrule)
    : position(std::move(_position)), infrule(std::move(_infrule)) {}

  void ConsInfrule::serialize(cereal::JSONOutputArchive &archive) const {
    archive.makeArray();
    archive.writeName();

    archive.saveValue("Infrule");
    archive.startNode();
    archive.makeArray();
    archive(CEREAL_NVP(position));
    archive(CEREAL_NVP(infrule));
    archive.finishNode();
  }

  std::unique_ptr<TyCommand> ConsInfrule::make
  (std::unique_ptr<TyPosition> _position,
   std::unique_ptr<TyInfrule> _infrule) {
    return std::unique_ptr<TyCommand>(new ConsInfrule(std::move(_position), std::move(_infrule)));
  }

  // core hint

  CoreHint::CoreHint() {}

  CoreHint::CoreHint(std::string _module_id, std::string _function_id,
                     std::string _opt_name)
    : module_id(_module_id), function_id(_function_id), opt_name(_opt_name) {
  }

  void CoreHint::addCommand(std::unique_ptr<TyCommand> c) {
    commands.push_back(std::move(c));
  }

  void CoreHint::addSrcNopPosition(std::unique_ptr<TyPosition> position) {
    src_nop_positions.push_back(std::move(position));
  }

  void CoreHint::addTgtNopPosition(std::unique_ptr<TyPosition> position) {
    tgt_nop_positions.push_back(std::move(position));
  }

  void CoreHint::serialize(cereal::JSONOutputArchive &archive) const {
    std::string function_id = std::string("@") + this->function_id;
    archive(CEREAL_NVP(module_id));
    archive(CEREAL_NVP(function_id));
    archive(CEREAL_NVP(opt_name));
    archive(CEREAL_NVP(src_nop_positions));
    archive(CEREAL_NVP(tgt_nop_positions));
    archive(CEREAL_NVP(commands));
  }

} // llvmberry
