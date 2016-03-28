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

namespace cereal {
[[noreturn]] void throw_exception(std::exception const &e) { std::exit(1); }

template <class T>
void save(cereal::JSONOutputArchive &archive, std::unique_ptr<T> const &ptr) {
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
  case llvmberry::HalfType:
    return std::string("HalfType");
  case llvmberry::FloatType:
    return std::string("FloatType");
  case llvmberry::DoubleType:
    return std::string("DoubleType");
  case llvmberry::FP128Type:
    return std::string("FP128Type");
  case llvmberry::PPC_FP128Type:
    return std::string("PPC_FP128Type");
  case llvmberry::X86_FP80Type:
    return std::string("X86_FP80Type");
  default:
    assert(false && "FloatType toString");
  }
}

} // anonymous

namespace llvmberry {
/// @return the index of the BasicBlock w.r.t. the parent function.
std::string getBasicBlockIndex(const llvm::BasicBlock *block) {
  if (!block || !(block->getParent())) {
    std::stringstream retStream;
    retStream << ((unsigned int)-1);
    return retStream.str();
  }

  // If a block has its own name, just return it.
  if (block->hasName()) {
    return block->getName();
  }

  // If else, calculate the index and return it.
  const llvm::Function *parent = block->getParent();
  const llvm::Function::BasicBlockListType &blockList =
      parent->getBasicBlockList();

  unsigned int idx = 0;
  for (llvm::Function::const_iterator itr = blockList.begin();
       itr != blockList.end(); ++itr) {
    if (block == &(*itr)) {
      std::stringstream retStream;
      retStream << idx;
      return (retStream.str());
    }

    idx++;
  }
  std::stringstream retStream;
  retStream << ((unsigned int)-1);
  return retStream.str();
}

std::string getVariable(const llvm::Value &value) {
  std::string val;

  if (llvm::isa<llvm::GlobalValue>(value)) {
    val = std::string("@");
  } else if (llvm::isa<llvm::Instruction>(value) ||
             llvm::isa<llvm::Argument>(value)) {
    val = std::string("%");
  } else {
    assert("value must be a global value or an instruction" && false);
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

// insert nop at tgt where I is at src
void insertTgtNopAtSrcI(CoreHint &hints, llvm::Instruction *I) {
  if (I == I->getParent()->getFirstNonPHI()) {
    std::string nop_block_name = getBasicBlockIndex(I->getParent());
    hints.addTgtNopPosition(ConsPhinodeCurrentBlockName::make(nop_block_name));
  } else if (!llvm::isa<llvm::PHINode>(I)) {
    llvm::BasicBlock::iterator prevI = I;
    prevI--;
    std::string nop_prev_reg = getVariable(*prevI);
    hints.addTgtNopPosition(ConsCommandRegisterName::make(nop_prev_reg));
  }
}

// insert nop at src where I is at tgt
void insertSrcNopAtTgtI(CoreHint &hints, llvm::Instruction *I) {
  if (I == I->getParent()->getFirstNonPHI()) {
    std::string nop_block_name = getBasicBlockIndex(I->getParent());
    hints.addSrcNopPosition(ConsPhinodeCurrentBlockName::make(nop_block_name));
  } else if (!llvm::isa<llvm::PHINode>(I)) {
    llvm::BasicBlock::iterator prevI = I;
    prevI--;
    std::string nop_prev_reg = getVariable(*prevI);
    hints.addSrcNopPosition(ConsCommandRegisterName::make(nop_prev_reg));
  }
}

void generateHintforNegValue(llvm::Value *V, llvm::BinaryOperator &I) {

  if (llvm::BinaryOperator::isNeg(V)) {
    if (llvmberry::ValidationUnit::Exists()) {
      llvmberry::ValidationUnit::GetInstance()->intrude
              ([&V, &I]
                       (llvmberry::ValidationUnit::Dictionary &data, llvmberry::CoreHint &hints) {

                std::string reg0_name = llvmberry::getVariable(I);  //z = x -my
                std::string reg1_name = llvmberry::getVariable(*V); //my

                hints.addCommand
                        (llvmberry::ConsPropagate::make
                                 (llvmberry::ConsLessdef::make
                                          (llvmberry::ConsVar::make(reg1_name, llvmberry::Physical), //my = -y
                                           llvmberry::ConsRhs::make(reg1_name, llvmberry::Physical, llvmberry::       Source),
                                           llvmberry::Source),
                                  llvmberry::ConsBounds::make
                                          (llvmberry::ConsCommand::make(llvmberry::Source,
                                                                        reg1_name), //From my to z = x -my
                                           llvmberry::ConsCommand::make(llvmberry::Source, reg0_name)))
                        );
              }
              );
    }
  }
  // Constants can be considered to be negated values if they can be folded.
  if (llvm::ConstantInt *C = llvm::dyn_cast<llvm::ConstantInt>(V)) {
    if (llvmberry::ValidationUnit::Exists()) {
      llvmberry::ValidationUnit::GetInstance()->intrude
              ([&I, &V, &C]
                       (llvmberry::ValidationUnit::Dictionary &data, llvmberry::CoreHint &hints) {

                std::string reg0_name = llvmberry::getVariable(I);  //z = x -my

                unsigned sz_bw = I.getType()->getPrimitiveSizeInBits();
                int c1 = (int)C->getSExtValue();
                int c2 = std::abs(c1);

                hints.addCommand
                        (llvmberry::ConsInfrule::make
                                 (llvmberry::ConsCommand::make (llvmberry::Source, reg0_name), llvmberry::ConsNegVal::make
                                         (llvmberry::TyConstInt::make(c1,sz_bw),
                                          llvmberry::TyConstInt::make(c2,sz_bw),
                                          llvmberry::ConsSize::make(sz_bw))));
              }
              );
    }
  }
//  if(ConstantDataVector *C = dyn_cast<ConstantDataVector>(V))
//  {
//  Todo
//  }
}

/* position */

TyPositionPhinode::TyPositionPhinode(std::string _block_name,
                                     std::string _prev_block_name)
    : block_name(_block_name), prev_block_name(_prev_block_name) {}

void TyPositionPhinode::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(block_name), CEREAL_NVP(prev_block_name));
}

TyPositionCommand::TyPositionCommand(enum TyScope _scope,
                                     std::string _register_name)
    : scope(_scope), register_name(_register_name) {}

void TyPositionCommand::serialize(cereal::JSONOutputArchive &archive) const {
  archive(cereal::make_nvp("scope", toString(scope)),
          CEREAL_NVP(register_name));
}

ConsPhinode::ConsPhinode(std::unique_ptr<TyPositionPhinode> _position_phinode)
    : position_phinode(std::move(_position_phinode)) {}

ConsPhinode::ConsPhinode(std::string _block_name, std::string _prev_block_name)
    : position_phinode(new TyPositionPhinode(_block_name, _prev_block_name)) {}

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

std::unique_ptr<TyPosition> ConsCommand::make(enum TyScope _scope,
                                              std::string _register_name) {
  return std::unique_ptr<TyPosition>(new ConsCommand(_scope, _register_name));
}

ConsPhinodeCurrentBlockName::ConsPhinodeCurrentBlockName(
    std::string _block_name)
    : block_name(_block_name) {}

void ConsPhinodeCurrentBlockName::serialize(
    cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  std::string s("PhinodeCurrentBlockName");
  archive(s);
  archive(block_name);
}

std::unique_ptr<TyNopPosition>
ConsPhinodeCurrentBlockName::make(std::string _block_name) {
  return std::unique_ptr<TyNopPosition>(
      new ConsPhinodeCurrentBlockName(_block_name));
}

ConsCommandRegisterName::ConsCommandRegisterName(std::string _register_name)
    : register_name(_register_name) {}

void ConsCommandRegisterName::serialize(
    cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  std::string s("CommandRegisterName");
  archive(s);
  archive(register_name);
}

std::unique_ptr<TyNopPosition>
ConsCommandRegisterName::make(std::string _register_name) {
  return std::unique_ptr<TyNopPosition>(
      new ConsCommandRegisterName(_register_name));
}

/* value */

// register

TyRegister::TyRegister(std::string _name, enum TyTag _tag)
    : name(_name), tag(_tag) {}

void TyRegister::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(name), cereal::make_nvp("tag", toString(tag)));
}

std::unique_ptr<TyRegister> TyRegister::make(std::string _name,
                                             enum TyTag _tag) {
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
    : int_value(_int_value), int_type(std::move(_int_type)) {}

TyConstInt::TyConstInt(int _int_value, int _bitwidth)
    : int_value(_int_value), int_type(new ConsIntType(_bitwidth)) {}

void TyConstInt::serialize(cereal::JSONOutputArchive &archive) const {
  archive(cereal::make_nvp("int_value", int_value), CEREAL_NVP(int_type));
}

std::unique_ptr<TyConstInt> TyConstInt::make(int _int_value, int _value) {
  return std::unique_ptr<TyConstInt>(new TyConstInt(_int_value, _value));
}

TyConstFloat::TyConstFloat(double _float_value, enum TyFloatType _float_type)
    : float_value(_float_value), float_type(_float_type) {}

void TyConstFloat::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(float_value),
          cereal::make_nvp("float_type", toString(float_type)));
}

std::unique_ptr<TyConstFloat> TyConstFloat::make(double _float_value,
                                                 enum TyFloatType _float_type) {
  return std::unique_ptr<TyConstFloat>(
      new TyConstFloat(_float_value, _float_type));
}

// value
std::unique_ptr<TyValue> TyValue::make(const llvm::Value &value) {
  if (llvm::isa<llvm::Instruction>(value) ||
      llvm::isa<llvm::GlobalValue>(value) || llvm::isa<llvm::Argument>(value)) {
    return std::unique_ptr<TyValue>(
        new ConsId(TyRegister::make(getVariable(value), llvmberry::Physical)));
  } else if (llvm::isa<llvm::ConstantInt>(value)) {
    const llvm::ConstantInt *v = llvm::dyn_cast<llvm::ConstantInt>(&value);
    return std::unique_ptr<TyValue>(
        new ConsConstVal(std::unique_ptr<TyConstant>(new ConsConstInt(
            TyConstInt::make(v->getSExtValue(), v->getBitWidth())))));
  } else if (llvm::isa<llvm::ConstantFP>(value)) {
    const llvm::ConstantFP *v = llvm::dyn_cast<llvm::ConstantFP>(&value);
    const llvm::APFloat &apf = v->getValueAPF();
    const llvm::Type *typ = v->getType();

    llvmberry::TyFloatType fty;
    if (typ->isHalfTy())
      fty = llvmberry::HalfType;
    else if (typ->isFloatTy())
      fty = llvmberry::FloatType;
    else if (typ->isDoubleTy())
      fty = llvmberry::DoubleType;
    else if (typ->isX86_FP80Ty())
      fty = llvmberry::X86_FP80Type;
    else if (typ->isFP128Ty())
      fty = llvmberry::FP128Type;
    else if (typ->isPPC_FP128Ty())
      fty = llvmberry::PPC_FP128Type;
    else
      assert("Unknown floating point type" && false);

    return std::unique_ptr<TyValue>(
        new ConsConstVal(std::unique_ptr<TyConstant>(new ConsConstFloat(
            TyConstFloat::make(apf.convertToDouble(), fty)))));
  } else {
    assert("Unknown value type" && false);
  }
}

ConsConstInt::ConsConstInt(std::unique_ptr<TyConstInt> _const_int)
    : const_int(std::move(_const_int)) {}

ConsConstInt::ConsConstInt(int _int_value, int _value)
    : const_int(new TyConstInt(_int_value, _value)) {}

void ConsConstInt::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("ConstInt");
  archive(CEREAL_NVP(const_int));
}

ConsConstFloat::ConsConstFloat(std::unique_ptr<TyConstFloat> _const_float)
    : const_float(std::move(_const_float)) {}

ConsConstFloat::ConsConstFloat(float _float_value, enum TyFloatType _float_type)
    : const_float(new TyConstFloat(_float_value, _float_type)) {}

void ConsConstFloat::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("ConstFloat");
  archive(CEREAL_NVP(const_float));
}

ConsId::ConsId(std::unique_ptr<TyRegister> _register)
    : reg(std::move(_register)) {}

void ConsId::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("Id");
  archive(CEREAL_NVP(reg));
}

std::unique_ptr<TyValue> ConsId::make(std::string _name, enum TyTag _tag) {
  std::unique_ptr<TyRegister> _reg(new TyRegister(_name, _tag));
  return std::unique_ptr<TyValue>(new ConsId(std::move(_reg)));
}

ConsConstVal::ConsConstVal(std::unique_ptr<TyConstant> _constant)
    : constant(std::move(_constant)) {}

void ConsConstVal::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("ConstVal");
  archive(CEREAL_NVP(constant));
}

// size

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
    : register_name(std::move(_register_name)) {}

ConsVar::ConsVar(std::string _name, enum TyTag _tag)
    : register_name(new TyRegister(_name, _tag)) {}

void ConsVar::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("Var");
  archive(CEREAL_NVP(register_name));
}

std::unique_ptr<TyExpr> ConsVar::make(std::string _name,
                                               enum TyTag _tag) {
  return std::unique_ptr<TyExpr>(new ConsVar(_name, _tag));
}

ConsRhs::ConsRhs(std::unique_ptr<TyRegister> _register_name, enum TyScope _scope)
    : register_name(std::move(_register_name)), scope(_scope) {}

ConsRhs::ConsRhs(std::string _name, enum TyTag _tag, enum TyScope _scope)
    : register_name(new TyRegister(_name, _tag)), scope(_scope) {}

void ConsRhs::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("Rhs");
  archive.startNode();
  archive.makeArray();
  archive(CEREAL_NVP(register_name), cereal::make_nvp("scope", toString(scope)));
  archive.finishNode();

}

std::unique_ptr<TyExpr> ConsRhs::make(std::string _name,
                                               enum TyTag _tag,
                                               enum TyScope _scope) {
  return std::unique_ptr<TyExpr>(new ConsRhs(_name, _tag, _scope));
}

ConsConst::ConsConst(std::unique_ptr<TyConstant> _constant)
    : constant(std::move(_constant)) {}

ConsConst::ConsConst(int _int_value, int _value)
    : constant(new ConsConstInt(_int_value, _value)) {}

ConsConst::ConsConst(float _float_value, enum TyFloatType _float_type)
    : constant(new ConsConstFloat(_float_value, _float_type)) {}

void ConsConst::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("Const");
  archive(CEREAL_NVP(constant));
}

// propagate object

TyPropagateLessdef::TyPropagateLessdef(std::unique_ptr<TyExpr> _lhs,
                                       std::unique_ptr<TyExpr> _rhs,
                                       enum TyScope _scope)
    : lhs(std::move(_lhs)), rhs(std::move(_rhs)), scope(_scope) {}

void TyPropagateLessdef::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(lhs), CEREAL_NVP(rhs),
          cereal::make_nvp("scope", toString(scope)));
}

std::unique_ptr<TyPropagateLessdef>
TyPropagateLessdef::make(std::unique_ptr<TyExpr> _lhs,
                         std::unique_ptr<TyExpr> _rhs,
                         enum TyScope _scope) {
  return std::unique_ptr<TyPropagateLessdef>(
      new TyPropagateLessdef(std::move(_lhs), std::move(_rhs), _scope));
}

TyPropagateNoalias::TyPropagateNoalias(std::unique_ptr<TyRegister> _lhs,
                                       std::unique_ptr<TyRegister> _rhs,
                                       enum TyScope _scope)
    : lhs(std::move(_lhs)), rhs(std::move(_rhs)), scope(_scope) {}

TyPropagateNoalias::TyPropagateNoalias(std::string _lhs_name,
                                       enum TyTag _lhs_tag,
                                       std::string _rhs_name,
                                       enum TyTag _rhs_tag, enum TyScope _scope)
    : lhs(new TyRegister(_lhs_name, _lhs_tag)),
      rhs(new TyRegister(_rhs_name, _rhs_tag)), scope(_scope) {}

void TyPropagateNoalias::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(lhs), CEREAL_NVP(rhs),
          cereal::make_nvp("scope", toString(scope)));
}

ConsLessdef::ConsLessdef(std::unique_ptr<TyPropagateLessdef> _propagate_lessdef)
    : propagate_lessdef(std::move(_propagate_lessdef)) {}

void ConsLessdef::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("Lessdef");
  archive(CEREAL_NVP(propagate_lessdef));
}

std::unique_ptr<TyPropagateObject>
ConsLessdef::make(std::unique_ptr<TyExpr> _lhs,
                  std::unique_ptr<TyExpr> _rhs, enum TyScope _scope) {
  auto ty_prop_ld = TyPropagateLessdef::make(std::move(_lhs), std::move(_rhs),
                                             std::move(_scope));
  return std::unique_ptr<TyPropagateObject>(
      new ConsLessdef(std::move(ty_prop_ld)));
}

ConsNoalias::ConsNoalias(std::unique_ptr<TyPropagateNoalias> _propagate_noalias)
    : propagate_noalias(std::move(_propagate_noalias)) {}

ConsNoalias::ConsNoalias(std::string _lhs_name, enum TyTag _lhs_tag,
                         std::string _rhs_name, enum TyTag _rhs_tag,
                         enum TyScope _scope)
    : propagate_noalias(new TyPropagateNoalias(_lhs_name, _lhs_tag, _rhs_name,
                                               _rhs_tag, _scope)) {}

void ConsNoalias::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("Noalias");
  archive(CEREAL_NVP(propagate_noalias));
}

ConsMaydiff::ConsMaydiff(std::unique_ptr<TyRegister> _register_name)
    : register_name(std::move(_register_name)) {}

ConsMaydiff::ConsMaydiff(std::string _name, enum TyTag _tag)
    : register_name(new TyRegister(_name, _tag)) {}

std::unique_ptr<TyPropagateObject> ConsMaydiff::make(std::string _name,
                                                     enum TyTag _tag) {
  return std::unique_ptr<TyPropagateObject>(
      new ConsMaydiff(TyRegister::make(_name, _tag)));
}

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

std::unique_ptr<TyPropagateRange>
ConsBounds::make(std::unique_ptr<TyPosition> _from,
                 std::unique_ptr<TyPosition> _to) {
  return std::unique_ptr<TyPropagateRange>(
      new ConsBounds(std::move(_from), std::move(_to)));
}

ConsGlobal::ConsGlobal() {}

void ConsGlobal::serialize(cereal::JSONOutputArchive &archive) const {
  // archive.makeArray();
  // archive.writeName();

  // archive.saveValue("Global"); /* TODO: how to print this in JSON */
  archive.setNextName("propagate_range");
  std::string s("Global");
  archive(s);
}

std::unique_ptr<TyPropagateRange> ConsGlobal::make() {
  return std::unique_ptr<TyPropagateRange>(new ConsGlobal());
}

TyPropagate::TyPropagate(std::unique_ptr<TyPropagateObject> _propagate,
                         std::unique_ptr<TyPropagateRange> _propagate_range)
    : propagate(std::move(_propagate)),
      propagate_range(std::move(_propagate_range)) {}

void TyPropagate::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(propagate));
  if (propagate_range->isGlobal()) {
    propagate_range->serialize(archive);
  } else {
    archive(CEREAL_NVP(propagate_range));
  }
}

/* inference rule */

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
  archive(CEREAL_NVP(x), CEREAL_NVP(e1), CEREAL_NVP(e2), CEREAL_NVP(sz));
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

ConsPropagate::ConsPropagate(std::unique_ptr<TyPropagate> _propagate)
    : propagate(std::move(_propagate)) {}

void ConsPropagate::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("Propagate");
  archive(CEREAL_NVP(propagate));
}

std::unique_ptr<TyCommand>
ConsPropagate::make(std::unique_ptr<TyPropagate> _propagate) {
  return std::unique_ptr<TyCommand>(new ConsPropagate(std::move(_propagate)));
}

std::unique_ptr<TyCommand>
ConsPropagate::make(std::unique_ptr<TyPropagateObject> _obj,
                    std::unique_ptr<TyPropagateRange> _range) {
  std::unique_ptr<TyPropagate> _propagate(
      new TyPropagate(std::move(_obj), std::move(_range)));
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

std::unique_ptr<TyCommand>
ConsInfrule::make(std::unique_ptr<TyPosition> _position,
                  std::unique_ptr<TyInfrule> _infrule) {
  return std::unique_ptr<TyCommand>(
      new ConsInfrule(std::move(_position), std::move(_infrule)));
}

// core hint

CoreHint::CoreHint() {}

CoreHint::CoreHint(std::string _module_id, std::string _function_id,
                   std::string _opt_name)
    : module_id(_module_id), function_id(_function_id), opt_name(_opt_name) {}

void CoreHint::addCommand(std::unique_ptr<TyCommand> c) {
  commands.push_back(std::move(c));
}

void CoreHint::addSrcNopPosition(std::unique_ptr<TyNopPosition> position) {
  src_nop_positions.push_back(std::move(position));
}

void CoreHint::addTgtNopPosition(std::unique_ptr<TyNopPosition> position) {
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
