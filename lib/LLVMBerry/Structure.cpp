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
#include "llvm/LLVMBerry/Infrules.h"

namespace cereal {
 [[noreturn]] void throw_exception(std::exception const &e){ std::exit(1); }
}
namespace boost{
  void throw_exception(std::exception const &e){ 
    std::cerr << " boost::throw_exception(e) called." << std::endl; 
    std::cerr << e.what() << std::endl; 
    std::exit(1); 
  }
}

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

unsigned int getRawInstrIndex(const llvm::Instruction &instr) {
  const llvm::BasicBlock *parent = instr.getParent();
  const llvm::BasicBlock::InstListType &instList = parent->getInstList();

  unsigned int idx = 0;
  for (llvm::BasicBlock::const_iterator itr = instList.begin();
       itr != instList.end(); ++itr) {
    if (&instr == &(*itr))
      return idx;
    idx++;
  }

  return (unsigned int)-1;
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
             llvm::isa<llvm::Argument>(value) ||
             llvm::isa<llvm::ConstantExpr>(value)) {
    val = std::string("%");
  } else {
    assert("value must be a global value or an instruction" && false);
  }

  val += std::string(value.getName().data());

  if (val == "%") val = "";

  return val;
}

std::string toString(llvmberry::TyFbop bop){
  switch (bop) {
    case llvmberry::BopFadd:
      return std::string("BopFadd");
    case llvmberry::BopFsub:
      return std::string("BopFsub");
    case llvmberry::BopFmul:
      return std::string("BopFmul");
    case llvmberry::BopFdiv:
      return std::string("BopFdiv");
    case llvmberry::BopFrem:
      return std::string("BopFrem");
  default:
    assert(false && "Fbop toString");
  }
}

std::string toString(llvmberry::TyBop bop){
  switch (bop) {
    case llvmberry::BopAdd:
      return std::string("BopAdd");
    case llvmberry::BopSub:
      return std::string("BopSub");
    case llvmberry::BopMul:
      return std::string("BopMul");
    case llvmberry::BopUdiv:
      return std::string("BopUdiv");
    case llvmberry::BopSdiv:
      return std::string("BopSdiv");
    case llvmberry::BopUrem:
      return std::string("BopUrem");
    case llvmberry::BopSrem:
      return std::string("BopSrem");
    case llvmberry::BopShl:
      return std::string("BopShl");
    case llvmberry::BopLshr:
      return std::string("BopLshr");
    case llvmberry::BopAshr:
      return std::string("BopAshr");
    case llvmberry::BopAnd:
      return std::string("BopAnd");
    case llvmberry::BopOr:
      return std::string("BopOr");
    case llvmberry::BopXor:
      return std::string("BopXor");
  default:
    assert(false && "Bop toString");
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

std::string toString(llvmberry::TyCond cond){
  switch(cond) {
  case llvmberry::CondEq:
    return std::string("CondEq");
  case llvmberry::CondNe:
    return std::string("CondNe");
  case llvmberry::CondUgt:
    return std::string("CondUgt");
  case llvmberry::CondUge:
    return std::string("CondUge");
  case llvmberry::CondUlt:
    return std::string("CondUlt");
  case llvmberry::CondUle:
    return std::string("CondUle");
  case llvmberry::CondSgt:
    return std::string("CondSgt");
  case llvmberry::CondSge:
    return std::string("CondSge");
  case llvmberry::CondSlt:
    return std::string("CondSlt");
  case llvmberry::CondSle:
    return std::string("CondSle");
 default:
    assert(false && "Cond toString");
  }
}

std::string toString(llvmberry::TyFCond fcond) {
  switch(fcond){
    case llvmberry::CondFfalse:
      return std::string("CondFfalse");
    case llvmberry::CondFoeq:
      return std::string("CondFoeq");
    case llvmberry::CondFogt:
      return std::string("CondFogt");
    case llvmberry::CondFoge:
      return std::string("CondFoge");
    case llvmberry::CondFolt:
      return std::string("CondFolt");
    case llvmberry::CondFole:
      return std::string("CondFole");
    case llvmberry::CondFone:
      return std::string("CondFone");
    case llvmberry::CondFord:
      return std::string("CondFord");
    case llvmberry::CondFuno:
      return std::string("CondFuno");
    case llvmberry::CondFueq:
      return std::string("CondFueq");
    case llvmberry::CondFugt:
      return std::string("CondFugt");
    case llvmberry::CondFuge:
      return std::string("CondFuge");
    case llvmberry::CondFult:
      return std::string("CondFult");
    case llvmberry::CondFule:
      return std::string("CondFule");
    case llvmberry::CondFune:
      return std::string("CondFune");
    case llvmberry::CondFtrue:
      return std::string("CondFtrue");
    default:
      assert("llvmberry::toString(llvmberry::TyFCond fcond) : unknown fcond" && false);
  }
}

llvmberry::TyFloatType getFloatType(llvm::Type *typ){
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
  return fty;
}


bool isFloatOpcode(llvm::Instruction::BinaryOps ops){
  switch(ops){
  case llvm::Instruction::FAdd:
  case llvm::Instruction::FSub:
  case llvm::Instruction::FMul:
  case llvm::Instruction::FDiv:
  case llvm::Instruction::FRem:
    return true;
  default:
    return false;
  }
}

TyFbop getFbop(llvm::Instruction::BinaryOps ops){
  TyFbop tyfbop;
  switch(ops){
  case llvm::Instruction::FAdd:
    tyfbop = llvmberry::BopFadd; break;
  case llvm::Instruction::FSub:
    tyfbop = llvmberry::BopFsub; break;
  case llvm::Instruction::FMul:
    tyfbop = llvmberry::BopFmul; break;
  case llvm::Instruction::FDiv:
    tyfbop = llvmberry::BopFdiv; break;
  case llvm::Instruction::FRem:
    tyfbop = llvmberry::BopFrem; break;
  default:
    assert("llvmberry::getFbop(llvm::Instruction::BinaryOps) : unknown opcode" && false);
  }
  return tyfbop;
}

TyBop getBop(llvm::Instruction::BinaryOps ops){
  TyBop bop;
  switch(ops){
  case llvm::Instruction::Add:
    bop = llvmberry::BopAdd; break;
  case llvm::Instruction::Sub:
    bop = llvmberry::BopSub; break;
  case llvm::Instruction::Mul:
    bop = llvmberry::BopMul; break;
  case llvm::Instruction::UDiv:
    bop = llvmberry::BopUdiv; break;
  case llvm::Instruction::SDiv:
    bop = llvmberry::BopSdiv; break;
  case llvm::Instruction::URem:
    bop = llvmberry::BopUrem; break;
  case llvm::Instruction::SRem:
    bop = llvmberry::BopSrem; break;
  case llvm::Instruction::Shl:
    bop = llvmberry::BopShl; break;
  case llvm::Instruction::LShr:
    bop = llvmberry::BopLshr; break;
  case llvm::Instruction::AShr:
    bop = llvmberry::BopAshr; break;
  case llvm::Instruction::And:
    bop = llvmberry::BopAnd; break;
  case llvm::Instruction::Or:
    bop = llvmberry::BopOr; break;
  case llvm::Instruction::Xor:
    bop = llvmberry::BopXor; break;
  default:
    assert("llvmberry::getBop(llvm::Instruction::BinaryOps) : unknown opcode" && false);
  }
  return bop;
}

TyCond getIPredicate(llvm::ICmpInst::Predicate prd) {
TyCond predicate;

  switch(prd){
    case llvm::ICmpInst::ICMP_EQ:
      predicate = llvmberry::CondEq; break;
    case llvm::ICmpInst::ICMP_NE:
      predicate = llvmberry::CondNe; break;
    case llvm::ICmpInst::ICMP_UGT:
      predicate = llvmberry::CondUgt; break;
    case llvm::ICmpInst::ICMP_UGE:
      predicate = llvmberry::CondUge; break;
    case llvm::ICmpInst::ICMP_ULT:
      predicate = llvmberry::CondUlt; break;
    case llvm::ICmpInst::ICMP_ULE:
      predicate = llvmberry::CondUle; break;
    case llvm::ICmpInst::ICMP_SGT:
      predicate = llvmberry::CondSgt; break;
    case llvm::ICmpInst::ICMP_SGE:
      predicate = llvmberry::CondSge; break;
    case llvm::ICmpInst::ICMP_SLT:
      predicate = llvmberry::CondSlt; break;
    case llvm::ICmpInst::ICMP_SLE:
      predicate = llvmberry::CondSle; break;
    default:
      assert("llvmberry::getIPredicate(llvm::Instruction::BinaryOps) : unknown opcode" && false);
  }
  return predicate;
}

TyFCond getFPredicate(llvm::FCmpInst::Predicate prd) {
TyFCond predicate;

  switch(prd){
    case llvm::FCmpInst::FCMP_FALSE:
      predicate = llvmberry::CondFfalse; break;
    case llvm::FCmpInst::FCMP_OEQ:
      predicate = llvmberry::CondFoeq; break;
    case llvm::FCmpInst::FCMP_OGT:
      predicate = llvmberry::CondFogt; break;
    case llvm::FCmpInst::FCMP_OGE:
      predicate = llvmberry::CondFoge; break;
    case llvm::FCmpInst::FCMP_OLT:
      predicate = llvmberry::CondFolt; break;
    case llvm::FCmpInst::FCMP_OLE:
      predicate = llvmberry::CondFole; break;
    case llvm::FCmpInst::FCMP_ONE:
      predicate = llvmberry::CondFone; break;
    case llvm::FCmpInst::FCMP_ORD:
      predicate = llvmberry::CondFord; break;
    case llvm::FCmpInst::FCMP_UNO:
      predicate = llvmberry::CondFuno; break;
    case llvm::FCmpInst::FCMP_UEQ:
      predicate = llvmberry::CondFueq; break;
    case llvm::FCmpInst::FCMP_UGT:
      predicate = llvmberry::CondFugt; break;
    case llvm::FCmpInst::FCMP_UGE:
      predicate = llvmberry::CondFuge; break;
    case llvm::FCmpInst::FCMP_ULT:
      predicate = llvmberry::CondFult; break;
    case llvm::FCmpInst::FCMP_ULE:
      predicate = llvmberry::CondFule; break;
    case llvm::FCmpInst::FCMP_UNE:
      predicate = llvmberry::CondFune; break;
    case llvm::FCmpInst::FCMP_TRUE:
      predicate = llvmberry::CondFtrue; break;
    default:
      assert("llvmberry::getFPredicate(llvm::Instruction::BinaryOps) : unknown opcode" && false);
  }
  return predicate;
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

int getCommandIndex(const llvm::Value &V) {
  if (!llvm::isa<llvm::Instruction>(V))
    return -1; // not an instruction

  const llvm::Instruction *instr = llvm::dyn_cast<llvm::Instruction>(&V);

  if (llvm::isa<llvm::PHINode>(instr)) {
    return -1; // A phinode is not a command
  } else if (instr->isTerminator()) {
    return -1; // A terminator is not a command
  } else {
    unsigned int rawIndex = getRawInstrIndex(*instr);
    const llvm::BasicBlock *parent = instr->getParent();
    unsigned int firstNonPhiRawIndex =
        getRawInstrIndex(*parent->getFirstNonPHI());
    return (int)(rawIndex - firstNonPhiRawIndex);
  }
}

int getTerminatorIndex(const llvm::TerminatorInst *instr) {
  if (instr->isTerminator()) {
    unsigned int rawIndex = getRawInstrIndex(*instr);
    const llvm::BasicBlock *parent = instr->getParent();
    unsigned int firstNonPhiRawIndex =
        getRawInstrIndex(*parent->getFirstNonPHI());
    return (int)(rawIndex - firstNonPhiRawIndex);
  } else {
    return -1; // not a terminator
  }
}

std::shared_ptr<TyExpr> makeExpr_fromStoreInst(const llvm::StoreInst* si) {
  llvm::Value* Val = si->getOperand(0);

  if (llvm::ConstantInt* C = llvm::dyn_cast<llvm::ConstantInt>(Val)) {
    int storeval = C->getSExtValue();
    int bitwidth = C->getBitWidth();

    return std::shared_ptr<llvmberry::TyExpr>
              (new ConsConst(storeval, bitwidth));
  } else {
    std::string reg_stored = getVariable(*Val);

    return ConsVar::make(reg_stored, Physical);
  }
}

/* position */

TyPositionPhinode::TyPositionPhinode(std::string _prev_block_name)
    : prev_block_name(_prev_block_name) {}

void TyPositionPhinode::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(prev_block_name));
}

TyPositionCommand::TyPositionCommand(int _index, std::string _register_name)
    : index(_index), register_name(_register_name) {}

void TyPositionCommand::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(index), CEREAL_NVP(register_name));
}

ConsPhinode::ConsPhinode(std::shared_ptr<TyPositionPhinode> _position_phinode)
    : position_phinode(std::move(_position_phinode)) {}

void ConsPhinode::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("Phinode");
  archive(CEREAL_NVP(position_phinode));
}

ConsCommand::ConsCommand(std::shared_ptr<TyPositionCommand> _position_command)
    : position_command(std::move(_position_command)) {}

void ConsCommand::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("Command");
  archive(CEREAL_NVP(position_command));
}

TyPosition::TyPosition(enum TyScope _scope, std::string _block_name,
                       std::shared_ptr<TyInstrIndex> _instr_index)
    : scope(_scope), block_name(_block_name),
      instr_index(std::move(_instr_index)) {}

void TyPosition::serialize(cereal::JSONOutputArchive &archive) const {
  archive(cereal::make_nvp("scope", ::toString(scope)), CEREAL_NVP(block_name), CEREAL_NVP(instr_index));
}

std::shared_ptr<TyPosition> TyPosition::make(enum TyScope _scope,
                                             std::string _block_name,
                                             std::string _prev_block_name) {
  std::shared_ptr<TyPositionPhinode> _pos_phi(
      new TyPositionPhinode(_prev_block_name));

  std::shared_ptr<TyInstrIndex> _phi(new ConsPhinode(std::move(_pos_phi)));

  return std::shared_ptr<TyPosition>(
      new TyPosition(_scope, _block_name, std::move(_phi)));
}

std::shared_ptr<TyPosition>
TyPosition::make_start_of_block(enum TyScope _scope, std::string _block_name) {
  return TyPosition::make(_scope, _block_name, "");
}

std::shared_ptr<TyPosition> TyPosition::make(enum TyScope _scope,
                                             const llvm::Instruction &I) {
  std::string empty_str = "";
  return std::move(TyPosition::make(_scope, I, empty_str));
}

std::shared_ptr<TyPosition> TyPosition::make(enum TyScope _scope,
                                             const llvm::Instruction &I, std::string _prev_block_name) {

  std::string _block_name = getBasicBlockIndex(I.getParent());
  std::string _register_name = getVariable(I);

  std::shared_ptr<TyInstrIndex> _instr_index;

  if (llvm::isa<llvm::PHINode>(I)) {
    std::shared_ptr<TyPositionPhinode> _pos_phi(new TyPositionPhinode(_prev_block_name));

    std::shared_ptr<TyInstrIndex> _phi(new ConsPhinode(std::move(_pos_phi)));

    _instr_index = std::move(_phi);
  } else {
    int _index;
    if (llvm::isa<llvm::TerminatorInst>(I)) {
      _index = getTerminatorIndex(llvm::dyn_cast<llvm::TerminatorInst>(&I));
    } else {
      _index = getCommandIndex(I);
    }
    std::shared_ptr<TyPositionCommand> _pos_cmd(
        new TyPositionCommand(_index, _register_name));

    std::shared_ptr<TyInstrIndex> _cmd(new ConsCommand(std::move(_pos_cmd)));

    _instr_index = std::move(_cmd);
  }

  return std::shared_ptr<TyPosition>(
      new TyPosition(_scope, _block_name, std::move(_instr_index)));
}

std::shared_ptr<TyPosition>
TyPosition::make_end_of_block(enum TyScope _scope, const llvm::BasicBlock &BB) {

  const llvm::TerminatorInst *term = BB.getTerminator();

  std::string _block_name = getBasicBlockIndex(&BB);
  std::string _register_name = "";

  int _index = getTerminatorIndex(term);

  std::shared_ptr<TyPositionCommand> _pos_cmd(
      new TyPositionCommand(_index, _register_name));

  std::shared_ptr<TyInstrIndex> _cmd(new ConsCommand(std::move(_pos_cmd)));

  return std::shared_ptr<TyPosition>(
      new TyPosition(_scope, _block_name, std::move(_cmd)));

}

/* value */

// register

TyRegister::TyRegister(std::string _name, enum TyTag _tag)
    : name(_name), tag(_tag) {}

void TyRegister::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(name), cereal::make_nvp("tag", ::toString(tag)));
}

std::shared_ptr<TyRegister> TyRegister::make(std::string _name,
                                             enum TyTag _tag) {
  return std::shared_ptr<TyRegister>(new TyRegister(_name, _tag));
}

// constant
ConsIntType::ConsIntType(int _value) : value(_value) {}

void ConsIntType::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("IntType");
  archive(CEREAL_NVP(value));
}

TyConstInt::TyConstInt(int64_t _int_value, std::shared_ptr<TyIntType> _int_type)
    : int_value(_int_value), int_type(std::move(_int_type)) {}

TyConstInt::TyConstInt(int64_t _int_value, int _bitwidth)
    : int_value(_int_value), int_type(new ConsIntType(_bitwidth)) {}

void TyConstInt::serialize(cereal::JSONOutputArchive &archive) const {
  archive(cereal::make_nvp("int_value", int_value), CEREAL_NVP(int_type));
}

std::shared_ptr<TyConstInt> TyConstInt::make(int64_t _int_value, int _value) {
  return std::shared_ptr<TyConstInt>(new TyConstInt(_int_value, _value));
}

TyConstFloat::TyConstFloat(double _float_value, enum TyFloatType _float_type)
    : float_value(_float_value), float_type(_float_type) {}

void TyConstFloat::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(float_value),
          cereal::make_nvp("float_type", toString(float_type)));
}

std::shared_ptr<TyConstFloat> TyConstFloat::make(double _float_value,
                                                 enum TyFloatType _float_type) {
  return std::shared_ptr<TyConstFloat>(
      new TyConstFloat(_float_value, _float_type));
}

// value
std::shared_ptr<TyValue> TyValue::make(const llvm::Value &value, enum TyTag _tag) {
  if (llvm::isa<llvm::Instruction>(value) ||
      llvm::isa<llvm::GlobalValue>(value) || llvm::isa<llvm::Argument>(value)) {
    return std::shared_ptr<TyValue>(
        new ConsId(TyRegister::make(getVariable(value), _tag)));
  } else if (llvm::isa<llvm::ConstantInt>(value)) {
    const llvm::ConstantInt *v = llvm::dyn_cast<llvm::ConstantInt>(&value);
    return std::shared_ptr<TyValue>(
        new ConsConstVal(std::shared_ptr<TyConstant>(new ConsConstInt(
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

    return std::shared_ptr<TyValue>(
        new ConsConstVal(std::shared_ptr<TyConstant>(new ConsConstFloat(
            TyConstFloat::make(apf.convertToDouble(), fty)))));
  } else if (llvm::isa<llvm::UndefValue>(value)) {
      return std::shared_ptr<TyValue>(
        new ConsConstVal(std::shared_ptr<TyConstant>
                          (new ConsConstUndef
                            (TyValueType::make(*value.getType())))));
  } else {
    assert("Unknown value type" && false);
  }
}

ConsConstInt::ConsConstInt(std::shared_ptr<TyConstInt> _const_int)
    : const_int(std::move(_const_int)) {}

ConsConstInt::ConsConstInt(int64_t _int_value, int _bitwidth)
    : const_int(new TyConstInt(_int_value, _bitwidth)) {}

void ConsConstInt::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("ConstInt");
  archive(CEREAL_NVP(const_int));
}

ConsConstFloat::ConsConstFloat(std::shared_ptr<TyConstFloat> _const_float)
    : const_float(std::move(_const_float)) {}

ConsConstFloat::ConsConstFloat(float _float_value, enum TyFloatType _float_type)
    : const_float(new TyConstFloat(_float_value, _float_type)) {}

void ConsConstFloat::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("ConstFloat");
  archive(CEREAL_NVP(const_float));
}

ConsConstUndef::ConsConstUndef(std::shared_ptr<TyValueType> _value_type)
    : value_type(std::move(_value_type)) {}

void ConsConstUndef::serialize(cereal::JSONOutputArchive& archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("ConstUndef");
  archive(CEREAL_NVP(value_type));
}

ConsId::ConsId(std::shared_ptr<TyRegister> _register)
    : reg(std::move(_register)) {}

void ConsId::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("Id");
  archive(CEREAL_NVP(reg));
}

std::shared_ptr<TyValue> ConsId::make(std::string _name, enum TyTag _tag) {
  std::shared_ptr<TyRegister> _reg(new TyRegister(_name, _tag));
  return std::shared_ptr<TyValue>(new ConsId(std::move(_reg)));
}

ConsConstVal::ConsConstVal(std::shared_ptr<TyConstant> _constant)
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

std::shared_ptr<TySize> ConsSize::make(int _size) {
  return std::shared_ptr<TySize>(new ConsSize(_size));
}

// valuetype

std::shared_ptr<TyValueType> TyValueType::make(const llvm::Type &type) {
  TyValueType *vt;
  if (const llvm::IntegerType *itype = llvm::dyn_cast<llvm::IntegerType>(&type)) {
    vt = new ConsIntValueType(std::move(std::shared_ptr<TyIntType>
              (new ConsIntType(itype->getBitWidth()))));
  } else if (const llvm::PointerType *ptype = llvm::dyn_cast<llvm::PointerType>(&type)) {
    vt = new ConsPtrType(ptype->getAddressSpace(), 
        std::move(TyValueType::make(*ptype->getPointerElementType())));
  } else if (const llvm::StructType *stype = llvm::dyn_cast<llvm::StructType>(&type)) {
    assert(stype->hasName());
    vt = new ConsNamedType(stype->getName().str());
  } else if (type.isHalfTy()) {
    vt = new ConsFloatValueType(HalfType);
  } else if (type.isFloatTy()) {
    vt = new ConsFloatValueType(FloatType);
  } else if (type.isDoubleTy()) {
    vt = new ConsFloatValueType(DoubleType);
  } else if (type.isFP128Ty()) {
    vt = new ConsFloatValueType(FP128Type);
  } else if (type.isPPC_FP128Ty()) {
    vt = new ConsFloatValueType(PPC_FP128Type);
  } else if (type.isX86_FP80Ty()) {
    vt = new ConsFloatValueType(X86_FP80Type);
  } else {
    assert("TyValueType::make(const llvmType &) : unknown value type" && false);
    vt = nullptr;
  }
    
  return std::shared_ptr<TyValueType>(vt);
}

ConsIntValueType::ConsIntValueType(std::shared_ptr<TyIntType> _int_type) : int_type(std::move(_int_type)) {}

void ConsIntValueType::serialize(cereal::JSONOutputArchive& archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IntValueType");
  archive(CEREAL_NVP(int_type));
}

ConsFloatValueType::ConsFloatValueType(TyFloatType _float_type) : float_type(_float_type) {}

void ConsFloatValueType::serialize(cereal::JSONOutputArchive& archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("FloatValueType");
  archive(cereal::make_nvp("float_type", toString(float_type)));
}

ConsNamedType::ConsNamedType(std::string _s) : s(std::move(_s)) {}

void ConsNamedType::serialize(cereal::JSONOutputArchive& archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("NamedType");
  archive(CEREAL_NVP(s));
}

ConsPtrType::ConsPtrType(int _address_space, std::shared_ptr<TyValueType> _valuetype) 
    : address_space(_address_space), valuetype(std::move(_valuetype)) {}

void ConsPtrType::serialize(cereal::JSONOutputArchive& archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("PtrType");

  archive.startNode();
  archive.makeArray();
  archive(cereal::make_nvp("address_space", address_space));
  archive(CEREAL_NVP(valuetype));
  archive.finishNode();
}

// instruction

std::shared_ptr<TyInstruction> TyInstruction::make(const llvm::Instruction &i) {
  if (const llvm::BinaryOperator *bo = llvm::dyn_cast<llvm::BinaryOperator>(&i)) {
    if(isFloatOpcode(bo->getOpcode()))
      return std::shared_ptr<TyInstruction>(new ConsFloatBinaryOp(
        std::move(TyFloatBinaryOperator::make(*bo))));
    else
      return std::shared_ptr<TyInstruction>(new ConsBinaryOp(
        std::move(TyBinaryOperator::make(*bo))));
  } else if (const llvm::ICmpInst *icmp = llvm::dyn_cast<llvm::ICmpInst>(&i)) {
    return std::shared_ptr<TyInstruction>(new ConsICmpInst(std::move(TyICmpInst::make(*icmp))));
  } else if (const llvm::FCmpInst *fcmp = llvm::dyn_cast<llvm::FCmpInst>(&i)) {
    return std::shared_ptr<TyInstruction>(new ConsFCmpInst(std::move(TyFCmpInst::make(*fcmp))));
  } else if (const llvm::LoadInst *li = llvm::dyn_cast<llvm::LoadInst>(&i)) {
    return std::shared_ptr<TyInstruction>(new ConsLoadInst(std::move(TyLoadInst::make(*li))));
  } else if (const llvm::StoreInst *si = llvm::dyn_cast<llvm::StoreInst>(&i)) {
    return std::shared_ptr<TyInstruction>(new ConsLoadInst(std::move(TyLoadInst::make(*si))));
  } else {
    assert("TyInstruction::make : unsupporting instruction type" && false);
    return std::shared_ptr<TyInstruction>(nullptr);
  }
}

std::shared_ptr<TyBinaryOperator> TyBinaryOperator::make(const llvm::BinaryOperator &bopinst){
  llvmberry::TyBop bop = llvmberry::getBop(bopinst.getOpcode());
  return std::shared_ptr<TyBinaryOperator>(new TyBinaryOperator(bop, TyValueType::make(*bopinst.getType()),
        TyValue::make(*bopinst.getOperand(0)), TyValue::make(*bopinst.getOperand(1))));
}

std::shared_ptr<TyFloatBinaryOperator> TyFloatBinaryOperator::make(const llvm::BinaryOperator &bopinst){
  llvmberry::TyFbop bop = llvmberry::getFbop(bopinst.getOpcode());
  return std::shared_ptr<TyFloatBinaryOperator>(new TyFloatBinaryOperator(bop, TyValueType::make(*bopinst.getType()),
        TyValue::make(*bopinst.getOperand(0)), TyValue::make(*bopinst.getOperand(1))));
}

std::shared_ptr<TyICmpInst> TyICmpInst::make(const llvm::ICmpInst &icmpInst){
  llvmberry::TyCond predicate = llvmberry::getIPredicate(icmpInst.getPredicate());
  return std::shared_ptr<TyICmpInst>(new TyICmpInst(predicate, TyValueType::make(*icmpInst.getType()),
                                    TyValue::make(*icmpInst.getOperand(0)), TyValue::make(*icmpInst.getOperand(1))));
}

std::shared_ptr<TyFCmpInst> TyFCmpInst::make(const llvm::FCmpInst &fcmpInst){
  llvmberry::TyFCond predicate = llvmberry::getFPredicate(fcmpInst.getPredicate());
  return std::shared_ptr<TyFCmpInst>(new TyFCmpInst(predicate, TyValueType::make(*fcmpInst.getType()),
                                    TyValue::make(*fcmpInst.getOperand(0)), TyValue::make(*fcmpInst.  getOperand(1))));
}

std::shared_ptr<TyLoadInst> TyLoadInst::make(const llvm::LoadInst &li) {
  return std::shared_ptr<TyLoadInst>(new TyLoadInst(
        TyValueType::make(*li.getPointerOperand()->getType()),
        TyValueType::make(*li.getType()),
        TyValue::make(*li.getPointerOperand()),
        li.getAlignment()));
}

std::shared_ptr<TyLoadInst> TyLoadInst::make(const llvm::StoreInst &si) {
  return std::shared_ptr<TyLoadInst>(new TyLoadInst(
        TyValueType::make(*si.getOperand(1)->getType()),
        TyValueType::make(*si.getOperand(0)->getType()),
        TyValue::make(*si.getOperand(1)),
        si.getAlignment()));
}

ConsBinaryOp::ConsBinaryOp(std::shared_ptr<TyBinaryOperator> _binary_operator) : binary_operator(std::move(_binary_operator)){
}
std::shared_ptr<TyInstruction> ConsBinaryOp::make(TyBop _opcode, std::shared_ptr<TyValueType> _operandtype, std::shared_ptr<TyValue> _operand1, std::shared_ptr<TyValue> _operand2){
  std::shared_ptr<TyBinaryOperator> _val(new TyBinaryOperator(_opcode, std::move(_operandtype), std::move(_operand1), std::move(_operand2)));
  return std::shared_ptr<TyInstruction>(new ConsBinaryOp(std::move(_val)));
}
std::shared_ptr<TyInstruction> ConsBinaryOp::make(const llvm::BinaryOperator &bop){
  return std::shared_ptr<TyInstruction>(new ConsBinaryOp(std::move(TyBinaryOperator::make(bop))));
}
void ConsBinaryOp::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("BinaryOp");
  archive(CEREAL_NVP(binary_operator));
}

ConsFloatBinaryOp::ConsFloatBinaryOp(std::shared_ptr<TyFloatBinaryOperator> _binary_operator) : binary_operator(std::move(_binary_operator)){
}
std::shared_ptr<TyInstruction> ConsFloatBinaryOp::make(TyFbop _opcode, std::shared_ptr<TyValueType> _operandtype, std::shared_ptr<TyValue> _operand1, std::shared_ptr<TyValue> _operand2){
  std::shared_ptr<TyFloatBinaryOperator> _val(new TyFloatBinaryOperator(_opcode, std::move(_operandtype), std::move(_operand1), std::move(_operand2)));
  return std::shared_ptr<TyInstruction>(new ConsFloatBinaryOp(std::move(_val)));
}
std::shared_ptr<TyInstruction> ConsFloatBinaryOp::make(const llvm::BinaryOperator &bop){
  return std::shared_ptr<TyInstruction>(new ConsFloatBinaryOp(std::move(TyFloatBinaryOperator::make(bop))));
}
void ConsFloatBinaryOp::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("FloatBinaryOp");
  archive(CEREAL_NVP(binary_operator));
}

ConsICmpInst::ConsICmpInst(std::shared_ptr<TyICmpInst> _icmp_inst) : icmp_inst(std::move(_icmp_inst)){
}
std::shared_ptr<TyInstruction> ConsICmpInst::make(TyCond _predicate, std::shared_ptr<TyValueType> _operandtype, std::shared_ptr<TyValue> _operand1, std::shared_ptr<TyValue> _operand2){
  std::shared_ptr<TyICmpInst> _val(new TyICmpInst(_predicate, std::move(_operandtype), std::move(_operand1), std::move(_operand2)));
  return std::shared_ptr<TyInstruction>(new ConsICmpInst(std::move(_val)));
}
std::shared_ptr<TyInstruction> ConsICmpInst::make(const llvm::ICmpInst &iCmpInst){
  return std::shared_ptr<TyInstruction>(new ConsICmpInst(std::move(TyICmpInst::make(iCmpInst))));
}
void ConsICmpInst::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("ICmpInst");
  archive(CEREAL_NVP(icmp_inst));
}

ConsFCmpInst::ConsFCmpInst(std::shared_ptr<TyFCmpInst> _fcmp_inst) : fcmp_inst(std::move(_fcmp_inst)){
}
std::shared_ptr<TyInstruction> ConsFCmpInst::make(TyFCond _predicate, std::shared_ptr<TyValueType> _operandtype, std::shared_ptr<TyValue> _operand1, std::shared_ptr<TyValue>_operand2){
  std::shared_ptr<TyFCmpInst> _val(new TyFCmpInst(_predicate, std::move(_operandtype), std::          move(_operand1), std::move(_operand2)));
  return std::shared_ptr<TyInstruction>(new ConsFCmpInst(std::move(_val)));
}
std::shared_ptr<TyInstruction> ConsFCmpInst::make(const llvm::FCmpInst &fCmpInst){
  return std::shared_ptr<TyInstruction>(new ConsFCmpInst(std::move(TyFCmpInst::make(fCmpInst))));
}
void ConsFCmpInst::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("FCmpInst");
  archive(CEREAL_NVP(fcmp_inst));
}

ConsLoadInst::ConsLoadInst(std::shared_ptr<TyLoadInst> _load_inst) : load_inst(std::move(_load_inst)){
}
std::shared_ptr<TyInstruction> ConsLoadInst::make(std::shared_ptr<TyValueType> _pointertype, std::shared_ptr<TyValueType> _valtype, std::shared_ptr<TyValue> _ptrvalue, int _align){
  std::shared_ptr<TyLoadInst> _val(new TyLoadInst(std::move(_pointertype), std::move(_valtype), std::move(_ptrvalue), std::move(_align)));
  return std::shared_ptr<TyInstruction>(new ConsLoadInst(std::move(_val)));
}
std::shared_ptr<TyInstruction> ConsLoadInst::make(const llvm::LoadInst &li){
  return std::shared_ptr<TyInstruction>(new ConsLoadInst(std::move(TyLoadInst::make(li))));
}
void ConsLoadInst::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("LoadInst");
  archive(CEREAL_NVP(load_inst));
}

TyBinaryOperator::TyBinaryOperator(TyBop _opcode, std::shared_ptr<TyValueType> _operandtype, std::shared_ptr<TyValue> _operand1, std::shared_ptr<TyValue> _operand2) : opcode(std::move(_opcode)), operandtype(std::move(_operandtype)), operand1(std::move(_operand1)), operand2(std::move(_operand2)){
}
void TyBinaryOperator::serialize(cereal::JSONOutputArchive& archive) const{
  archive(cereal::make_nvp("opcode", toString(opcode)));
  archive(CEREAL_NVP(operandtype));
  archive(CEREAL_NVP(operand1));
  archive(CEREAL_NVP(operand2));
}

TyFloatBinaryOperator::TyFloatBinaryOperator(TyFbop _opcode, std::shared_ptr<TyValueType> _operandtype, std::shared_ptr<TyValue> _operand1, std::shared_ptr<TyValue> _operand2) : opcode(_opcode), operandtype(std::move(_operandtype)), operand1(std::move(_operand1)), operand2(std::move(_operand2)){
}
void TyFloatBinaryOperator::serialize(cereal::JSONOutputArchive& archive) const{
  archive(cereal::make_nvp("opcode", toString(opcode)));
  archive(CEREAL_NVP(operandtype));
  archive(CEREAL_NVP(operand1));
  archive(CEREAL_NVP(operand2));
}

TyICmpInst::TyICmpInst(TyCond _predicate, std::shared_ptr<TyValueType> _operandtype, std::shared_ptr<TyValue> _operand1, std::shared_ptr<TyValue> _operand2) : predicate(_predicate), operandtype(std::move(_operandtype)), operand1(std::move(_operand1)), operand2(std::move(_operand2)){
}
void TyICmpInst::serialize(cereal::JSONOutputArchive& archive) const{
  archive(cereal::make_nvp("predicate", toString(predicate)));
  archive(CEREAL_NVP(operandtype));
  archive(CEREAL_NVP(operand1));
  archive(CEREAL_NVP(operand2));
}

TyFCmpInst::TyFCmpInst(TyFCond _predicate, std::shared_ptr<TyValueType> _operandtype, std::shared_ptr<TyValue> _operand1, std::shared_ptr<TyValue> _operand2) : predicate(std::move(_predicate)), operandtype(std::move(_operandtype)), operand1(std::move(_operand1)), operand2(std::move(_operand2)){
}
void TyFCmpInst::serialize(cereal::JSONOutputArchive& archive) const{
  archive(cereal::make_nvp("predicate", toString(predicate)));
  archive(CEREAL_NVP(operandtype));
  archive(CEREAL_NVP(operand1));
  archive(CEREAL_NVP(operand2));
}

TyLoadInst::TyLoadInst(std::shared_ptr<TyValueType> _pointertype, std::shared_ptr<TyValueType> _valtype, std::shared_ptr<TyValue> _ptrvalue, int _align) : pointertype(std::move(_pointertype)), valtype(std::move(_valtype)), ptrvalue(std::move(_ptrvalue)), align(std::move(_align)){
}
void TyLoadInst::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(pointertype));
  archive(CEREAL_NVP(valtype));
  archive(CEREAL_NVP(ptrvalue));
  archive(CEREAL_NVP(align));
}

/* Propagate */

// propagate expr
// ConsVar or ConsConst
std::shared_ptr<TyExpr> TyExpr::make(const llvm::Value &value, enum TyTag _tag) {
  std::shared_ptr<TyValue> vptr = TyValue::make(value, _tag);
  TyValue *v = vptr.get();
  if(ConsId *cid = dynamic_cast<ConsId *>(v)){
    return std::shared_ptr<TyExpr>(new ConsVar(cid->reg));
  }else if(ConsConstVal *ccv = dynamic_cast<ConsConstVal *>(v)){
    return std::shared_ptr<TyExpr>(new ConsConst(ccv->constant));
  }else{
    assert("Unknown value type" && false);
  }
}

ConsVar::ConsVar(std::shared_ptr<TyRegister> _register_name)
    : register_name(std::move(_register_name)) {}

ConsVar::ConsVar(std::string _name, enum TyTag _tag)
    : register_name(new TyRegister(_name, _tag)) {}

void ConsVar::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("Var");
  archive(CEREAL_NVP(register_name));
}

std::shared_ptr<TyExpr> ConsVar::make(std::string _name,
                                               enum TyTag _tag) {
  return std::shared_ptr<TyExpr>(new ConsVar(_name, _tag));
}

ConsRhs::ConsRhs(std::shared_ptr<TyRegister> _register_name, enum TyScope _scope)
    : register_name(std::move(_register_name)), scope(_scope) {}

ConsRhs::ConsRhs(std::string _name, enum TyTag _tag, enum TyScope _scope)
    : register_name(new TyRegister(_name, _tag)), scope(_scope) {}

void ConsRhs::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("Rhs");
  archive.startNode();
  archive.makeArray();
  archive(CEREAL_NVP(register_name), cereal::make_nvp("scope", ::toString(scope)));
  archive.finishNode();

}

std::shared_ptr<TyExpr> ConsRhs::make(std::string _name,
                                               enum TyTag _tag,
                                               enum TyScope _scope) {
  return std::shared_ptr<TyExpr>(new ConsRhs(_name, _tag, _scope));
}

ConsConst::ConsConst(std::shared_ptr<TyConstant> _constant)
    : constant(std::move(_constant)) {}

ConsConst::ConsConst(int _int_value, int _bitwidth)
    : constant(new ConsConstInt(_int_value, _bitwidth)) {}

ConsConst::ConsConst(float _float_value, enum TyFloatType _float_type)
    : constant(new ConsConstFloat(_float_value, _float_type)) {}

void ConsConst::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("Const");
  archive(CEREAL_NVP(constant));
}

ConsInsn::ConsInsn(std::shared_ptr<TyInstruction> _instruction) : instruction(std::move(_instruction)){
}
std::shared_ptr<TyExpr> ConsInsn::make(const llvm::Instruction &i){
  return std::shared_ptr<TyExpr>(new ConsInsn(std::move(TyInstruction::make(i))));
}
std::shared_ptr<TyExpr> ConsInsn::make(std::shared_ptr<TyInstruction> _instruction) {
  return std::shared_ptr<TyExpr>(new ConsInsn(std::move(_instruction)));
}
void ConsInsn::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("Insn");
  archive(CEREAL_NVP(instruction));
}

// propagate object

TyPropagateLessdef::TyPropagateLessdef(std::shared_ptr<TyExpr> _lhs,
                                       std::shared_ptr<TyExpr> _rhs,
                                       enum TyScope _scope)
    : lhs(std::move(_lhs)), rhs(std::move(_rhs)), scope(_scope) {}

void TyPropagateLessdef::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(lhs), CEREAL_NVP(rhs),
          cereal::make_nvp("scope", ::toString(scope)));
}

std::shared_ptr<TyPropagateLessdef>
TyPropagateLessdef::make(std::shared_ptr<TyExpr> _lhs,
                         std::shared_ptr<TyExpr> _rhs,
                         enum TyScope _scope) {
  return std::shared_ptr<TyPropagateLessdef>(
      new TyPropagateLessdef(std::move(_lhs), std::move(_rhs), _scope));
}

TyPropagateNoalias::TyPropagateNoalias(std::shared_ptr<TyRegister> _lhs,
                                       std::shared_ptr<TyRegister> _rhs,
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
          cereal::make_nvp("scope", ::toString(scope)));
}

TyPropagateAlloca::TyPropagateAlloca(std::shared_ptr<TyRegister> _p, 
                                     enum TyScope _scope) 
    : p(std::move(_p)), scope(std::move(_scope)) {
}

void TyPropagateAlloca::serialize(cereal::JSONOutputArchive& archive) const {
  archive(CEREAL_NVP(p));
  archive(cereal::make_nvp("scope", ::toString(scope)));
}

TyPropagatePrivate::TyPropagatePrivate(std::shared_ptr<TyRegister> _p, 
                                       enum TyScope _scope) 
    : p(std::move(_p)), scope(std::move(_scope)) {
}

void TyPropagatePrivate::serialize(cereal::JSONOutputArchive& archive) const {
  archive(CEREAL_NVP(p));
  archive(cereal::make_nvp("scope", ::toString(scope)));
}

ConsLessdef::ConsLessdef(std::shared_ptr<TyPropagateLessdef> _propagate_lessdef)
    : propagate_lessdef(std::move(_propagate_lessdef)) {}

void ConsLessdef::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("Lessdef");
  archive(CEREAL_NVP(propagate_lessdef));
}

std::shared_ptr<TyPropagateObject>
ConsLessdef::make(std::shared_ptr<TyExpr> _lhs,
                  std::shared_ptr<TyExpr> _rhs, enum TyScope _scope) {
  auto ty_prop_ld = TyPropagateLessdef::make(std::move(_lhs), std::move(_rhs),
                                             std::move(_scope));
  return std::shared_ptr<TyPropagateObject>(
      new ConsLessdef(std::move(ty_prop_ld)));
}

ConsNoalias::ConsNoalias(std::shared_ptr<TyPropagateNoalias> _propagate_noalias)
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

ConsAlloca::ConsAlloca(std::shared_ptr<TyPropagateAlloca> _propagate_alloca) 
    : propagate_alloca(std::move(_propagate_alloca)) {}

std::shared_ptr<TyPropagateObject> ConsAlloca::make(std::shared_ptr<TyRegister> _p, 
                                                    enum TyScope _scope) {
  std::shared_ptr<TyPropagateAlloca> _val
                    (new TyPropagateAlloca(std::move(_p), _scope));

  return std::shared_ptr<TyPropagateObject>(new ConsAlloca(std::move(_val)));
}

void ConsAlloca::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("Alloca");
  archive(CEREAL_NVP(propagate_alloca));
}

ConsMaydiff::ConsMaydiff(std::shared_ptr<TyRegister> _register_name)
    : register_name(std::move(_register_name)) {}

ConsMaydiff::ConsMaydiff(std::string _name, enum TyTag _tag)
    : register_name(new TyRegister(_name, _tag)) {}

std::shared_ptr<TyPropagateObject> ConsMaydiff::make(std::string _name,
                                                     enum TyTag _tag) {
  return std::shared_ptr<TyPropagateObject>
          (new ConsMaydiff(TyRegister::make(_name, _tag)));
}

void ConsMaydiff::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("Maydiff");
  archive(CEREAL_NVP(register_name));
}

ConsPrivate::ConsPrivate(std::shared_ptr<TyPropagatePrivate> _propagate_private) 
    : propagate_private(std::move(_propagate_private)) {}

std::shared_ptr<TyPropagateObject> ConsPrivate::make(std::shared_ptr<TyRegister> _p, 
                                                     enum TyScope _scope) {
  std::shared_ptr<TyPropagatePrivate> _val
                    (new TyPropagatePrivate(std::move(_p), _scope));

  return std::shared_ptr<TyPropagateObject>(new ConsPrivate(std::move(_val)));
}

void ConsPrivate::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("Private");
  archive(CEREAL_NVP(propagate_private));
}

// propagate range

ConsBounds::ConsBounds(std::shared_ptr<TyPosition> _from,
                       std::shared_ptr<TyPosition> _to)
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

std::shared_ptr<TyPropagateRange>
ConsBounds::make(std::shared_ptr<TyPosition> _from,
                 std::shared_ptr<TyPosition> _to) {
  return std::shared_ptr<TyPropagateRange>(
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

std::shared_ptr<TyPropagateRange> ConsGlobal::make() {
  return std::shared_ptr<TyPropagateRange>(new ConsGlobal());
}

TyPropagate::TyPropagate(std::shared_ptr<TyPropagateObject> _propagate,
                         std::shared_ptr<TyPropagateRange> _propagate_range)
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

ConsPropagate::ConsPropagate(std::shared_ptr<TyPropagate> _propagate)
    : propagate(std::move(_propagate)) {}

void ConsPropagate::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("Propagate");
  archive(CEREAL_NVP(propagate));
}

std::shared_ptr<TyCommand>
ConsPropagate::make(std::shared_ptr<TyPropagate> _propagate) {
  return std::shared_ptr<TyCommand>(new ConsPropagate(std::move(_propagate)));
}

std::shared_ptr<TyCommand>
ConsPropagate::make(std::shared_ptr<TyPropagateObject> _obj,
                    std::shared_ptr<TyPropagateRange> _range) {
  std::shared_ptr<TyPropagate> _propagate(
      new TyPropagate(std::move(_obj), std::move(_range)));
  return std::shared_ptr<TyCommand>(new ConsPropagate(std::move(_propagate)));
}

ConsInfrule::ConsInfrule(std::shared_ptr<TyPosition> _position,
                         std::shared_ptr<TyInfrule> _infrule)
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

std::shared_ptr<TyCommand>
ConsInfrule::make(std::shared_ptr<TyPosition> _position,
                  std::shared_ptr<TyInfrule> _infrule) {
  return std::shared_ptr<TyCommand>(
      new ConsInfrule(std::move(_position), std::move(_infrule)));
}

// core hint

CoreHint::CoreHint() {}

CoreHint::CoreHint(std::string _module_id, std::string _function_id,
                   std::string _opt_name)
    : module_id(_module_id), function_id(_function_id), opt_name(_opt_name) {}

void CoreHint::addCommand(std::shared_ptr<TyCommand> c) {
  commands.push_back(std::move(c));
}

void CoreHint::addNopPosition(std::shared_ptr<TyPosition> position) {
  nop_positions.push_back(std::move(position));
}

void CoreHint::serialize(cereal::JSONOutputArchive &archive) const {
  std::string function_id = std::string("@") + this->function_id;
  archive(CEREAL_NVP(module_id));
  archive(CEREAL_NVP(function_id));
  archive(CEREAL_NVP(opt_name));
  archive(CEREAL_NVP(description));
  archive(CEREAL_NVP(commands));
  archive(CEREAL_NVP(nop_positions));
}

const std::string &CoreHint::getDescription() const {
  return description;
}

void CoreHint::setDescription(const std::string &desc){
  this->description = desc;
}

void CoreHint::setOptimizationName(const std::string &name){
  this->opt_name = name;
}

} // llvmberry
