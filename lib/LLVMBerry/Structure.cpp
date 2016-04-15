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
             llvm::isa<llvm::Argument>(value)) {
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

// insert nop at tgt where I is at src
void insertTgtNopAtSrcI(CoreHint &hints, llvm::Instruction *I) {
  std::string empty_str = "";
  if (I == I->getParent()->getFirstNonPHI()) {
    std::string nop_block_name = getBasicBlockIndex(I->getParent());
    hints.addNopPosition(TyPosition::make(TyScope::Target, nop_block_name, empty_str));
  } else if (!llvm::isa<llvm::PHINode>(I)) {
    llvm::BasicBlock::iterator prevI = I;
    prevI--;
    hints.addNopPosition(TyPosition::make(TyScope::Target, *prevI));
  }
}

// insert nop at src where I is at tgt
void insertSrcNopAtTgtI(CoreHint &hints, llvm::Instruction *I) {
  std::string empty_str = "";
  if (I == I->getParent()->getFirstNonPHI()) {
    std::string nop_block_name = getBasicBlockIndex(I->getParent());
    hints.addNopPosition(TyPosition::make(TyScope::Source, nop_block_name, empty_str));
  } else if (!llvm::isa<llvm::PHINode>(I)) {
    llvm::BasicBlock::iterator prevI = I;
    prevI--;
    hints.addNopPosition(TyPosition::make(TyScope::Source, *prevI));
  }
}

void propagateInstruction(llvm::Instruction *from, llvm::Instruction *to, TyScope scope) {
  if (llvmberry::ValidationUnit::Exists()) {
    llvmberry::ValidationUnit::GetInstance()->intrude([&from, &to, &scope](
        llvmberry::ValidationUnit::Dictionary &data,
        llvmberry::CoreHint &hints) {
      std::string reg_name = llvmberry::getVariable(*from);

      if(scope == llvmberry::Source){
        hints.addCommand(llvmberry::ConsPropagate::make(
            llvmberry::ConsLessdef::make(
                llvmberry::ConsVar::make(reg_name, llvmberry::Physical),
                llvmberry::ConsRhs::make(reg_name, llvmberry::Physical, scope),
                scope),
            llvmberry::ConsBounds::make(
                llvmberry::TyPosition::make(scope, *from),
                llvmberry::TyPosition::make(scope, *to))));
      }else if(scope == llvmberry::Target){
        hints.addCommand(llvmberry::ConsPropagate::make(
            llvmberry::ConsLessdef::make(
                llvmberry::ConsRhs::make(reg_name, llvmberry::Physical, scope),
                llvmberry::ConsVar::make(reg_name, llvmberry::Physical),
                scope),
            llvmberry::ConsBounds::make(
                llvmberry::TyPosition::make(scope, *from),
                llvmberry::TyPosition::make(scope, *to))));
      }else{
        assert("propagateInstruction() : scope is neither llvmberry::Source nor llvmberry::Target" && false);
      }
    });
  }
}

void applyCommutativity(llvm::Instruction *position, llvm::BinaryOperator *expression, TyScope scope){
  if (!llvmberry::ValidationUnit::Exists()){
    return;
  }
  llvmberry::ValidationUnit::GetInstance()->intrude([&position, &expression, &scope](
      llvmberry::ValidationUnit::Dictionary &data, llvmberry::CoreHint &hints){
    int bitwidth = llvmberry::isFloatOpcode(expression->getOpcode()) ? -1 : expression->getType()->getIntegerBitWidth();
    std::string regname = llvmberry::getVariable(*expression);
    if(scope == llvmberry::Source){
      switch(expression->getOpcode()){
      case llvm::Instruction::Add :
        hints.addCommand(llvmberry::ConsInfrule::make(
          llvmberry::TyPosition::make(llvmberry::Source, *position),
          llvmberry::ConsAddCommutative::make(
            llvmberry::TyRegister::make(regname, llvmberry::Physical),
            llvmberry::TyValue::make(*expression->getOperand(0)),
            llvmberry::TyValue::make(*expression->getOperand(1)),
            llvmberry::ConsSize::make(bitwidth))));
        break;
      case llvm::Instruction::And :
        hints.addCommand(llvmberry::ConsInfrule::make(
          llvmberry::TyPosition::make(llvmberry::Source, *position),
          llvmberry::ConsAndCommutative::make(
            llvmberry::TyRegister::make(regname, llvmberry::Physical),
            llvmberry::TyValue::make(*expression->getOperand(0)),
            llvmberry::TyValue::make(*expression->getOperand(1)),
            llvmberry::ConsSize::make(bitwidth))));
        break;
      case llvm::Instruction::Mul : 
        hints.addCommand(llvmberry::ConsInfrule::make(
          llvmberry::TyPosition::make(llvmberry::Source, *position),
          llvmberry::ConsMulCommutative::make(
            llvmberry::TyRegister::make(regname, llvmberry::Physical),
            llvmberry::TyValue::make(*expression->getOperand(0)),
            llvmberry::TyValue::make(*expression->getOperand(1)),
            llvmberry::ConsSize::make(bitwidth))));
        break;
      case llvm::Instruction::Or : 
        hints.addCommand(llvmberry::ConsInfrule::make(
          llvmberry::TyPosition::make(llvmberry::Source, *position),
          llvmberry::ConsOrCommutative::make(
            llvmberry::TyRegister::make(regname, llvmberry::Physical),
            llvmberry::TyValue::make(*expression->getOperand(0)),
            llvmberry::TyValue::make(*expression->getOperand(1)),
            llvmberry::ConsSize::make(bitwidth))));
        break;
      case llvm::Instruction::Xor :
        hints.addCommand(llvmberry::ConsInfrule::make(
          llvmberry::TyPosition::make(llvmberry::Source, *position),
          llvmberry::ConsXorCommutative::make(
            llvmberry::TyRegister::make(regname, llvmberry::Physical),
            llvmberry::TyValue::make(*expression->getOperand(0)),
            llvmberry::TyValue::make(*expression->getOperand(1)),
            llvmberry::ConsSize::make(bitwidth))));
        break;
      default:
        assert("applyCommutativity() : we don't support commutativity rule for this binary operator");
      }
    }else if(scope == llvmberry::Target){
      switch(expression->getOpcode()){
      case llvm::Instruction::Add :
        hints.addCommand(llvmberry::ConsInfrule::make(
          llvmberry::TyPosition::make(llvmberry::Target, *position),
          llvmberry::ConsAddCommutativeTgt::make(
            llvmberry::TyRegister::make(regname, llvmberry::Physical),
            llvmberry::TyValue::make(*expression->getOperand(0)),
            llvmberry::TyValue::make(*expression->getOperand(1)),
            llvmberry::ConsSize::make(bitwidth))));
        break;
       case llvm::Instruction::Xor :
        hints.addCommand(llvmberry::ConsInfrule::make(
          llvmberry::TyPosition::make(llvmberry::Target, *position),
          llvmberry::ConsXorCommutativeTgt::make(
            llvmberry::TyRegister::make(regname, llvmberry::Physical),
            llvmberry::TyValue::make(*expression->getOperand(0)),
            llvmberry::TyValue::make(*expression->getOperand(1)),
            llvmberry::ConsSize::make(bitwidth))));
        break;
      case llvm::Instruction::FAdd :
        hints.addCommand(llvmberry::ConsInfrule::make(
          llvmberry::TyPosition::make(llvmberry::Target, *position),
          llvmberry::ConsFaddCommutativeTgt::make(
            llvmberry::TyRegister::make(regname, llvmberry::Physical),
            llvmberry::TyValue::make(*expression->getOperand(0)),
            llvmberry::TyValue::make(*expression->getOperand(1)),
            llvmberry::getFloatType(expression->getType()))));
      default:
        assert("applyCommutativity() : we don't support commutativity rule for this binary operator");
      }
    }else{
      assert("applyCommutativity() : scope is neither llvmberry::Source nor llvmberry::Target" && false);
    }
  }); 
}

void generateHintForNegValue(llvm::Value *V, llvm::BinaryOperator &I, TyScope scope) {
  if (llvm::BinaryOperator::isNeg(V)) {
    if (llvmberry::ValidationUnit::Exists()) {
      llvmberry::ValidationUnit::GetInstance()->intrude([&V, &I, &scope](
          llvmberry::ValidationUnit::Dictionary &data,
          llvmberry::CoreHint &hints) {

        std::string reg0_name = llvmberry::getVariable(I); // z = x -my
        std::string reg1_name = llvmberry::getVariable(*V); // my

        llvm::Instruction *Vins = llvm::dyn_cast<llvm::Instruction>(V);

        hints.addCommand(llvmberry::ConsPropagate::make(
            llvmberry::ConsLessdef::make(
                llvmberry::ConsVar::make(reg1_name, llvmberry::Physical), // my = -y
                llvmberry::ConsRhs::make(reg1_name, llvmberry::Physical, scope),
                scope),
            llvmberry::ConsBounds::make(
                llvmberry::TyPosition::make(scope, *Vins), // From my to z = x -my
                llvmberry::TyPosition::make(scope, I))));

        hints.addCommand(llvmberry::ConsPropagate::make(
            llvmberry::ConsLessdef::make(
                llvmberry::ConsRhs::make(reg1_name, llvmberry::Physical, scope),
                llvmberry::ConsVar::make(reg1_name, llvmberry::Physical), // my = -y
                scope),
            llvmberry::ConsBounds::make(
                llvmberry::TyPosition::make(scope, *Vins), // From my to z = x -my
                llvmberry::TyPosition::make(scope, I))));
      });
    }
  }
  // Constants can be considered to be negated values if they can be folded.
  if (llvm::ConstantInt *C = llvm::dyn_cast<llvm::ConstantInt>(V)) {
    if (llvmberry::ValidationUnit::Exists()) {
      llvmberry::ValidationUnit::GetInstance()->intrude([&I, &V, &C, &scope](
          llvmberry::ValidationUnit::Dictionary &data,
          llvmberry::CoreHint &hints) {

        std::string reg0_name = llvmberry::getVariable(I); // z = x -my

        unsigned sz_bw = I.getType()->getPrimitiveSizeInBits();
        int64_t c1 = C->getSExtValue();
        int64_t c2 = -c1;

        hints.addCommand(llvmberry::ConsInfrule::make(
            llvmberry::TyPosition::make(scope, I),
            llvmberry::ConsNegVal::make(llvmberry::TyConstInt::make(c1, sz_bw),
                                        llvmberry::TyConstInt::make(c2, sz_bw),
                                        llvmberry::ConsSize::make(sz_bw))));

        hints.addCommand(llvmberry::ConsInfrule::make(
            llvmberry::TyPosition::make(scope, I),
            llvmberry::ConsNegVal::make(llvmberry::TyConstInt::make(c1, sz_bw),
                                        llvmberry::TyConstInt::make(c2, sz_bw),
                                        llvmberry::ConsSize::make(sz_bw))));
      });
    }
  }
  //  if(ConstantDataVector *C = dyn_cast<ConstantDataVector>(V))
  //  {
  //  Todo
  //  }
}

void generateHintForAddSelectZero(llvm::BinaryOperator *Z, 
                                  llvm::BinaryOperator *X, 
                                  llvm::SelectInst *Y, 
                                  bool needs_commutativity,
                                  bool is_leftform){
  assert(Z);
  assert(X);
  assert(Y);
  llvmberry::ValidationUnit::GetInstance()->intrude([&Z, &X, &Y, 
                needs_commutativity, is_leftform](
      llvmberry::ValidationUnit::Dictionary &data,
      llvmberry::CoreHint &hints) {
    // is_leftform == true : 
    //   <src>                          |     <tgt>
    // X = n - a                        | Z = n - a
    // Y = select c ? x : 0             | Y = select c ? x : 0
    // Z = Y + a                        | Z = select c ? n : a
 
    // is_leftform == false : 
    //   <src>                          |     <tgt>
    // X = n - a                        | Z = n - a
    // Y = select c ? 0 : x             | Y = select c ? 0 : x
    // Z = Y + a                        | Z = select c ? a : n
    
    llvm::Value *c = Y->getCondition();
    llvm::Value *n = X->getOperand(0);
    llvm::Value *a = X->getOperand(1);
    llvm::Value *a_Z = Z->getOperand(1);
    int bitwidth = Z->getType()->getIntegerBitWidth();

    // prepare variables
    std::string reg_y_name = llvmberry::getVariable(*Y);
    std::string reg_z_name = llvmberry::getVariable(*Z);
    std::string reg_x_name = llvmberry::getVariable(*X);

    // Propagate "X = n - a"
    hints.addCommand(llvmberry::ConsPropagate::make(
        llvmberry::ConsLessdef::make(
            llvmberry::ConsVar::make(reg_x_name, llvmberry::Physical),
            llvmberry::ConsRhs::make(reg_x_name, llvmberry::Physical, llvmberry::Source),
            llvmberry::Source),
        llvmberry::ConsBounds::make(
            llvmberry::TyPosition::make(llvmberry::Source, *X),
            llvmberry::TyPosition::make(llvmberry::Source, *Z))));

    // Propagate "Y = select c ? x : 0" or "Y = select c ? 0 : x"
    hints.addCommand(llvmberry::ConsPropagate::make(
        llvmberry::ConsLessdef::make(
            llvmberry::ConsVar::make(reg_y_name, llvmberry::Physical),
            llvmberry::ConsRhs::make(reg_y_name, llvmberry::Physical, llvmberry::Source),
            llvmberry::Source),
        llvmberry::ConsBounds::make(
            llvmberry::TyPosition::make(llvmberry::Source, *Y),
            llvmberry::TyPosition::make(llvmberry::Source, *Z))));

    if(needs_commutativity){
      hints.addCommand(llvmberry::ConsInfrule::make(
        llvmberry::TyPosition::make(llvmberry::Source, *Z),
        llvmberry::ConsAddCommutative::make(
            llvmberry::TyRegister::make(reg_z_name, llvmberry::Physical),
            llvmberry::TyValue::make(*Y),
            llvmberry::TyValue::make(*a_Z),
            llvmberry::ConsSize::make(bitwidth))));
    }
    
    if(is_leftform){
      hints.addCommand(llvmberry::ConsInfrule::make(
        llvmberry::TyPosition::make(llvmberry::Source, *Z),
        llvmberry::ConsAddSelectZero::make(
            llvmberry::TyRegister::make(reg_z_name, llvmberry::Physical),
            llvmberry::TyRegister::make(reg_x_name, llvmberry::Physical),
            llvmberry::TyRegister::make(reg_y_name, llvmberry::Physical),
            llvmberry::TyValue::make(*c),
            llvmberry::TyValue::make(*n),
            llvmberry::TyValue::make(*a),
            llvmberry::ConsSize::make(bitwidth))));
    }else{
      hints.addCommand(llvmberry::ConsInfrule::make(
        llvmberry::TyPosition::make(llvmberry::Source, *Z),
        llvmberry::ConsAddSelectZero2::make(
            llvmberry::TyRegister::make(reg_z_name, llvmberry::Physical),
            llvmberry::TyRegister::make(reg_x_name, llvmberry::Physical),
            llvmberry::TyRegister::make(reg_y_name, llvmberry::Physical),
            llvmberry::TyValue::make(*c),
            llvmberry::TyValue::make(*n),
            llvmberry::TyValue::make(*a),
            llvmberry::ConsSize::make(bitwidth))));
    }
  });
}

void generateHintForOrXor(llvm::BinaryOperator &I, llvm::Value *op0, llvm::Value *op1, bool needsCommutativity){
  llvmberry::ValidationUnit::GetInstance()->intrude([&I, &op0, &op1, &needsCommutativity](
      llvmberry::ValidationUnit::Dictionary &data,
      llvmberry::CoreHint &hints) {
    //    <src>    |   <tgt>
    // X = B ^ -1  | X = B ^ -1
    // Y = A & X   | Y = A & X
    // Z = A ^ B   | Z = A ^ B
    // W = Y | Z   | W = A ^ B
    llvm::BinaryOperator *W = &I;
    llvm::BinaryOperator *Z = llvm::dyn_cast<llvm::BinaryOperator>(op1);
    llvm::BinaryOperator *Y = llvm::dyn_cast<llvm::BinaryOperator>(op0);
    llvm::BinaryOperator *X = llvm::dyn_cast<llvm::BinaryOperator>(Y->getOperand(1));
    assert(X);
    assert(Y);
    assert(Z);
    assert(W);
    llvm::Value *A = Z->getOperand(0);
    llvm::Value *B = Z->getOperand(1);
    int bitwidth = W->getType()->getIntegerBitWidth();

    llvmberry::propagateInstruction(X, W, llvmberry::Source);
    llvmberry::propagateInstruction(Y, W, llvmberry::Source);
    llvmberry::propagateInstruction(Z, W, llvmberry::Source);
    if(X->getOperand(1) == B){
      llvmberry::applyCommutativity(W, X, llvmberry::Source);
    }

    if(needsCommutativity){
      llvmberry::applyCommutativity(W, W, llvmberry::Source);
    }
   
    hints.addCommand(llvmberry::ConsInfrule::make(
        llvmberry::TyPosition::make(llvmberry::Source, *W),
        llvmberry::ConsOrXor::make(
            llvmberry::TyValue::make(*W), 
            llvmberry::TyValue::make(*Z), 
            llvmberry::TyValue::make(*X), 
            llvmberry::TyValue::make(*Y), 
            llvmberry::TyValue::make(*A), 
            llvmberry::TyValue::make(*B), 
            llvmberry::ConsSize::make(bitwidth))));

  });
}

void generateHintForOrXor2(llvm::BinaryOperator &I, 
        llvm::Value *X1_val, llvm::Value *X2_val,
        llvm::Value *A, llvm::Value *B,
        bool needsY1Commutativity, bool needsY2Commutativity){
  llvmberry::ValidationUnit::GetInstance()->intrude([&I, &X1_val, &X2_val, &A, &B,
      &needsY1Commutativity,
      &needsY2Commutativity](llvmberry::ValidationUnit::Dictionary &data,
      llvmberry::CoreHint &hints) {
    //     <src>           <tgt>
    // X1 = B  ^ -1  | X1 =  B ^ -1
    // Y1 = A  & X1  | Y1 =  A & X1
    // X2 = A  ^ -1  | X2 =  A ^ -1
    // Y2 = X2 & B   | Y2 = X2 & B
    // Z =  Y1 | Y2  | Z =   A ^ B
    llvm::BinaryOperator *Z = &I;
    llvm::BinaryOperator *Y1 = llvm::dyn_cast<llvm::BinaryOperator>(Z->getOperand(0));
    llvm::BinaryOperator *Y2 = llvm::dyn_cast<llvm::BinaryOperator>(Z->getOperand(1));
    llvm::BinaryOperator *X1 = llvm::dyn_cast<llvm::BinaryOperator>(X1_val);
    llvm::BinaryOperator *X2 = llvm::dyn_cast<llvm::BinaryOperator>(X2_val);
    assert(Y1);
    assert(Y2);
    assert(X1);
    assert(X2);
    int bitwidth = Z->getType()->getIntegerBitWidth();
  
    llvmberry::propagateInstruction(X1, Z, llvmberry::Source);
    llvmberry::propagateInstruction(X2, Z, llvmberry::Source);
    llvmberry::propagateInstruction(Y1, Z, llvmberry::Source);
    llvmberry::propagateInstruction(Y2, Z, llvmberry::Source);
    if(X1->getOperand(1) == B)
      llvmberry::applyCommutativity(Z, X1, llvmberry::Source);
    if(X2->getOperand(1) == A)
      llvmberry::applyCommutativity(Z, X2, llvmberry::Source);
    if(needsY1Commutativity)
      llvmberry::applyCommutativity(Z, Y1, llvmberry::Source);
    if(needsY2Commutativity)
      llvmberry::applyCommutativity(Z, Y2, llvmberry::Source);
   
    hints.addCommand(llvmberry::ConsInfrule::make(
      llvmberry::TyPosition::make(llvmberry::Target, *Z),
      llvmberry::ConsOrXor2::make(
          llvmberry::TyValue::make(*Z), 
          llvmberry::TyValue::make(*X1), 
          llvmberry::TyValue::make(*Y1), 
          llvmberry::TyValue::make(*X2), 
          llvmberry::TyValue::make(*Y2), 
          llvmberry::TyValue::make(*A), 
          llvmberry::TyValue::make(*B), 
          llvmberry::ConsSize::make(bitwidth))));
   
  });

}

std::unique_ptr<TyExpr> makeExpr_fromStoreInst(const llvm::StoreInst* si) {
  llvm::Value* Val = si->getOperand(0);

  if (llvm::ConstantInt* C = llvm::dyn_cast<llvm::ConstantInt>(Val)) {
    int storeval = C->getSExtValue();
    int bitwidth = C->getBitWidth();

    return std::unique_ptr<llvmberry::TyExpr>
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

ConsPhinode::ConsPhinode(std::unique_ptr<TyPositionPhinode> _position_phinode)
    : position_phinode(std::move(_position_phinode)) {}

void ConsPhinode::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("Phinode");
  archive(CEREAL_NVP(position_phinode));
}

ConsCommand::ConsCommand(std::unique_ptr<TyPositionCommand> _position_command)
    : position_command(std::move(_position_command)) {}

void ConsCommand::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("Command");
  archive(CEREAL_NVP(position_command));
}

TyPosition::TyPosition(enum TyScope _scope, std::string _block_name,
                       std::unique_ptr<TyInstrIndex> _instr_index)
    : scope(_scope), block_name(_block_name),
      instr_index(std::move(_instr_index)) {}

void TyPosition::serialize(cereal::JSONOutputArchive &archive) const {
  archive(cereal::make_nvp("scope", ::toString(scope)), CEREAL_NVP(block_name), CEREAL_NVP(instr_index));
}

std::unique_ptr<TyPosition> TyPosition::make(enum TyScope _scope,
                                             std::string _block_name,
                                             std::string _prev_block_name) {
  std::unique_ptr<TyPositionPhinode> _pos_phi(
      new TyPositionPhinode(_prev_block_name));

  std::unique_ptr<TyInstrIndex> _phi(new ConsPhinode(std::move(_pos_phi)));

  return std::unique_ptr<TyPosition>(
      new TyPosition(_scope, _block_name, std::move(_phi)));
}

std::unique_ptr<TyPosition> TyPosition::make(enum TyScope _scope,
                                             const llvm::Instruction &I) {
  std::string empty_str = "";
  return std::move(TyPosition::make(_scope, I, empty_str));
}

std::unique_ptr<TyPosition> TyPosition::make(enum TyScope _scope,
                                             const llvm::Instruction &I, std::string _prev_block_name) {

  std::string _block_name = getBasicBlockIndex(I.getParent());
  std::string _register_name = getVariable(I);

  std::unique_ptr<TyInstrIndex> _instr_index;

  if (llvm::isa<llvm::PHINode>(I)) {
    std::unique_ptr<TyPositionPhinode> _pos_phi(new TyPositionPhinode(_prev_block_name));

    std::unique_ptr<TyInstrIndex> _phi(new ConsPhinode(std::move(_pos_phi)));

    _instr_index = std::move(_phi);
  } else {
    int _index;
    if (llvm::isa<llvm::TerminatorInst>(I)) {
      _index = getTerminatorIndex(llvm::dyn_cast<llvm::TerminatorInst>(&I));
    } else {
      _index = getCommandIndex(I);
    }
    std::unique_ptr<TyPositionCommand> _pos_cmd(
        new TyPositionCommand(_index, _register_name));

    std::unique_ptr<TyInstrIndex> _cmd(new ConsCommand(std::move(_pos_cmd)));

    _instr_index = std::move(_cmd);
  }

  return std::unique_ptr<TyPosition>(
      new TyPosition(_scope, _block_name, std::move(_instr_index)));
}

std::unique_ptr<TyPosition>
TyPosition::make_end_of_block(enum TyScope _scope, const llvm::BasicBlock &BB) {

  const llvm::TerminatorInst *term = BB.getTerminator();

  std::string _block_name = getBasicBlockIndex(&BB);
  std::string _register_name = "";

  int _index = getTerminatorIndex(term);

  std::unique_ptr<TyPositionCommand> _pos_cmd(
      new TyPositionCommand(_index, _register_name));

  std::unique_ptr<TyInstrIndex> _cmd(new ConsCommand(std::move(_pos_cmd)));

  return std::unique_ptr<TyPosition>(
      new TyPosition(_scope, _block_name, std::move(_cmd)));

}


/* value */

// register

TyRegister::TyRegister(std::string _name, enum TyTag _tag)
    : name(_name), tag(_tag) {}

void TyRegister::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(name), cereal::make_nvp("tag", ::toString(tag)));
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

TyConstInt::TyConstInt(int64_t _int_value, std::unique_ptr<TyIntType> _int_type)
    : int_value(_int_value), int_type(std::move(_int_type)) {}

TyConstInt::TyConstInt(int64_t _int_value, int _bitwidth)
    : int_value(_int_value), int_type(new ConsIntType(_bitwidth)) {}

void TyConstInt::serialize(cereal::JSONOutputArchive &archive) const {
  archive(cereal::make_nvp("int_value", int_value), CEREAL_NVP(int_type));
}

std::unique_ptr<TyConstInt> TyConstInt::make(int64_t _int_value, int _value) {
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
std::unique_ptr<TyValue> TyValue::make(const llvm::Value &value, enum TyTag _tag) {
  if (llvm::isa<llvm::Instruction>(value) ||
      llvm::isa<llvm::GlobalValue>(value) || llvm::isa<llvm::Argument>(value)) {
    return std::unique_ptr<TyValue>(
        new ConsId(TyRegister::make(getVariable(value), _tag)));
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

ConsConstInt::ConsConstInt(int64_t _int_value, int _bitwidth)
    : const_int(new TyConstInt(_int_value, _bitwidth)) {}

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


// valuetype

std::unique_ptr<TyValueType> TyValueType::make(const llvm::Type &type){
  TyValueType *vt;
  if(const llvm::IntegerType *itype = llvm::dyn_cast<llvm::IntegerType>(&type)){
    vt = new ConsIntValueType(std::move(std::unique_ptr<TyIntType>(new ConsIntType(itype->getBitWidth()))));
  }else if(const llvm::PointerType *ptype = llvm::dyn_cast<llvm::PointerType>(&type)){
    vt = new ConsPtrType(ptype->getAddressSpace(), 
        std::move(TyValueType::make(*ptype->getPointerElementType())));
  }else if(const llvm::StructType *stype = llvm::dyn_cast<llvm::StructType>(&type)){
    assert(stype->hasName());
    vt = new ConsNamedType(stype->getName().str());
  }else if(type.isHalfTy()){
    vt = new ConsFloatValueType(HalfType);
  }else if(type.isFloatTy()){
    vt = new ConsFloatValueType(FloatType);
  }else if(type.isDoubleTy()){
    vt = new ConsFloatValueType(DoubleType);
  }else if(type.isFP128Ty()){
    vt = new ConsFloatValueType(FP128Type);
  }else if(type.isPPC_FP128Ty()){
    vt = new ConsFloatValueType(PPC_FP128Type);
  }else if(type.isX86_FP80Ty()){
    vt = new ConsFloatValueType(X86_FP80Type);
  }else{
    assert("TyValueType::make(const llvmType &) : unknown value type" && false);
    vt = nullptr;
  }
    return std::unique_ptr<TyValueType>(vt);
  }

  ConsIntValueType::ConsIntValueType(std::unique_ptr<TyIntType> _int_type) : int_type(std::move(_int_type)){
  }
  void ConsIntValueType::serialize(cereal::JSONOutputArchive& archive) const{
    archive.makeArray();
    archive.writeName();
    archive.saveValue("IntValueType");
    archive(CEREAL_NVP(int_type));
  }

  ConsFloatValueType::ConsFloatValueType(TyFloatType _float_type) : float_type(_float_type){
  }
  void ConsFloatValueType::serialize(cereal::JSONOutputArchive& archive) const{
    archive.makeArray();
    archive.writeName();
    archive.saveValue("FloatValueType");
    archive(cereal::make_nvp("float_type", toString(float_type)));
}

ConsNamedType::ConsNamedType(std::string _s) : s(std::move(_s)){
}
void ConsNamedType::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("NamedType");
  archive(CEREAL_NVP(s));
}

ConsPtrType::ConsPtrType(int _address_space, std::unique_ptr<TyValueType> _valuetype) : address_space(_address_space), valuetype(std::move(_valuetype)){
}
void ConsPtrType::serialize(cereal::JSONOutputArchive& archive) const{
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

std::unique_ptr<TyInstruction> TyInstruction::make(const llvm::Instruction &i){
  if (const llvm::BinaryOperator *bo = llvm::dyn_cast<llvm::BinaryOperator>(&i)) {
    if(isFloatOpcode(bo->getOpcode()))
      return std::unique_ptr<TyInstruction>(new ConsFloatBinaryOp(
        std::move(TyFloatBinaryOperator::make(*bo))));
    else
      return std::unique_ptr<TyInstruction>(new ConsBinaryOp(
        std::move(TyBinaryOperator::make(*bo))));
  } else if (const llvm::LoadInst *li = llvm::dyn_cast<llvm::LoadInst>(&i)) {
    return std::unique_ptr<TyInstruction>(new ConsLoadInst(std::move(TyLoadInst::make(*li))));
  } else if (const llvm::StoreInst *si = llvm::dyn_cast<llvm::StoreInst>(&i)) {
    return std::unique_ptr<TyInstruction>(new ConsLoadInst(std::move(TyLoadInst::make(*si))));
  } else {
    assert("TyInstruction::make : unsupporting instruction type" && false);
    return std::unique_ptr<TyInstruction>(nullptr);
  }
}

std::unique_ptr<TyBinaryOperator> TyBinaryOperator::make(const llvm::BinaryOperator &bopinst){
  llvmberry::TyBop bop = llvmberry::getBop(bopinst.getOpcode());
  return std::unique_ptr<TyBinaryOperator>(new TyBinaryOperator(bop, TyValueType::make(*bopinst.getType()),
        TyValue::make(*bopinst.getOperand(0)), TyValue::make(*bopinst.getOperand(1))));
}

std::unique_ptr<TyFloatBinaryOperator> TyFloatBinaryOperator::make(const llvm::BinaryOperator &bopinst){
  llvmberry::TyFbop bop = llvmberry::getFbop(bopinst.getOpcode());
  return std::unique_ptr<TyFloatBinaryOperator>(new TyFloatBinaryOperator(bop, TyValueType::make(*bopinst.getType()),
        TyValue::make(*bopinst.getOperand(0)), TyValue::make(*bopinst.getOperand(1))));
}

std::unique_ptr<TyLoadInst> TyLoadInst::make(const llvm::LoadInst &li) {
  return std::unique_ptr<TyLoadInst>(new TyLoadInst(
        TyValueType::make(*li.getPointerOperand()->getType()),
        TyValueType::make(*li.getType()),
        TyValue::make(*li.getPointerOperand()),
        li.getAlignment()));
}

std::unique_ptr<TyLoadInst> TyLoadInst::make(const llvm::StoreInst &si) {
  return std::unique_ptr<TyLoadInst>(new TyLoadInst(
        TyValueType::make(*si.getOperand(1)->getType()),
        TyValueType::make(*si.getOperand(0)->getType()),
        TyValue::make(*si.getOperand(1)),
        si.getAlignment()));
}

ConsBinaryOp::ConsBinaryOp(std::unique_ptr<TyBinaryOperator> _binary_operator) : binary_operator(std::move(_binary_operator)){
}
std::unique_ptr<TyInstruction> ConsBinaryOp::make(TyBop _opcode, std::unique_ptr<TyValueType> _operandtype, std::unique_ptr<TyValue> _operand1, std::unique_ptr<TyValue> _operand2){
  std::unique_ptr<TyBinaryOperator> _val(new TyBinaryOperator(_opcode, std::move(_operandtype), std::move(_operand1), std::move(_operand2)));
  return std::unique_ptr<TyInstruction>(new ConsBinaryOp(std::move(_val)));
}
std::unique_ptr<TyInstruction> ConsBinaryOp::make(const llvm::BinaryOperator &bop){
  return std::unique_ptr<TyInstruction>(new ConsBinaryOp(std::move(TyBinaryOperator::make(bop))));
}
void ConsBinaryOp::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("BinaryOp");
  archive(CEREAL_NVP(binary_operator));
}

ConsFloatBinaryOp::ConsFloatBinaryOp(std::unique_ptr<TyFloatBinaryOperator> _binary_operator) : binary_operator(std::move(_binary_operator)){
}
std::unique_ptr<TyInstruction> ConsFloatBinaryOp::make(TyFbop _opcode, std::unique_ptr<TyValueType> _operandtype, std::unique_ptr<TyValue> _operand1, std::unique_ptr<TyValue> _operand2){
  std::unique_ptr<TyFloatBinaryOperator> _val(new TyFloatBinaryOperator(_opcode, std::move(_operandtype), std::move(_operand1), std::move(_operand2)));
  return std::unique_ptr<TyInstruction>(new ConsFloatBinaryOp(std::move(_val)));
}
std::unique_ptr<TyInstruction> ConsFloatBinaryOp::make(const llvm::BinaryOperator &bop){
  return std::unique_ptr<TyInstruction>(new ConsFloatBinaryOp(std::move(TyFloatBinaryOperator::make(bop))));
}
void ConsFloatBinaryOp::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("FloatBinaryOp");
  archive(CEREAL_NVP(binary_operator));
}

ConsLoadInst::ConsLoadInst(std::unique_ptr<TyLoadInst> _load_inst) : load_inst(std::move(_load_inst)){
}
std::unique_ptr<TyInstruction> ConsLoadInst::make(std::unique_ptr<TyValueType> _pointertype, std::unique_ptr<TyValueType> _valtype, std::unique_ptr<TyValue> _ptrvalue, int _align){
  std::unique_ptr<TyLoadInst> _val(new TyLoadInst(std::move(_pointertype), std::move(_valtype), std::move(_ptrvalue), std::move(_align)));
  return std::unique_ptr<TyInstruction>(new ConsLoadInst(std::move(_val)));
}
std::unique_ptr<TyInstruction> ConsLoadInst::make(const llvm::LoadInst &li){
  return std::unique_ptr<TyInstruction>(new ConsLoadInst(std::move(TyLoadInst::make(li))));
}
void ConsLoadInst::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("LoadInst");
  archive(CEREAL_NVP(load_inst));
}

TyBinaryOperator::TyBinaryOperator(TyBop _opcode, std::unique_ptr<TyValueType> _operandtype, std::unique_ptr<TyValue> _operand1, std::unique_ptr<TyValue> _operand2) : opcode(std::move(_opcode)), operandtype(std::move(_operandtype)), operand1(std::move(_operand1)), operand2(std::move(_operand2)){
}
void TyBinaryOperator::serialize(cereal::JSONOutputArchive& archive) const{
  archive(cereal::make_nvp("opcode", toString(opcode)));
  archive(CEREAL_NVP(operandtype));
  archive(CEREAL_NVP(operand1));
  archive(CEREAL_NVP(operand2));
}

TyFloatBinaryOperator::TyFloatBinaryOperator(TyFbop _opcode, std::unique_ptr<TyValueType> _operandtype, std::unique_ptr<TyValue> _operand1, std::unique_ptr<TyValue> _operand2) : opcode(_opcode), operandtype(std::move(_operandtype)), operand1(std::move(_operand1)), operand2(std::move(_operand2)){
}
void TyFloatBinaryOperator::serialize(cereal::JSONOutputArchive& archive) const{
  archive(cereal::make_nvp("opcode", toString(opcode)));
  archive(CEREAL_NVP(operandtype));
  archive(CEREAL_NVP(operand1));
  archive(CEREAL_NVP(operand2));
}

TyLoadInst::TyLoadInst(std::unique_ptr<TyValueType> _pointertype, std::unique_ptr<TyValueType> _valtype, std::unique_ptr<TyValue> _ptrvalue, int _align) : pointertype(std::move(_pointertype)), valtype(std::move(_valtype)), ptrvalue(std::move(_ptrvalue)), align(std::move(_align)){
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
std::unique_ptr<TyExpr> TyExpr::make(const llvm::Value &value, enum TyTag _tag) {
  std::unique_ptr<TyValue> vptr = TyValue::make(value, _tag);
  TyValue *v = vptr.get();
  if(ConsId *cid = dynamic_cast<ConsId *>(v)){
    return std::unique_ptr<TyExpr>(new ConsVar(std::unique_ptr<TyRegister>(cid->reg.release())));
  }else if(ConsConstVal *ccv = dynamic_cast<ConsConstVal *>(v)){
    return std::unique_ptr<TyExpr>(new ConsConst(std::unique_ptr<TyConstant>(ccv->constant.release())));
  }else{
    assert("Unknown value type" && false);
  }
}

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
  archive(CEREAL_NVP(register_name), cereal::make_nvp("scope", ::toString(scope)));
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

ConsInsn::ConsInsn(std::unique_ptr<TyInstruction> _instruction) : instruction(std::move(_instruction)){
}
std::unique_ptr<TyExpr> ConsInsn::make(const llvm::Instruction &i){
  return std::unique_ptr<TyExpr>(new ConsInsn(std::move(TyInstruction::make(i))));
}
std::unique_ptr<TyExpr> ConsInsn::make(std::unique_ptr<TyInstruction> _instruction) {
  return std::unique_ptr<TyExpr>(new ConsInsn(std::move(_instruction)));
}
void ConsInsn::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("Insn");
  archive(CEREAL_NVP(instruction));
}

// propagate object

TyPropagateLessdef::TyPropagateLessdef(std::unique_ptr<TyExpr> _lhs,
                                       std::unique_ptr<TyExpr> _rhs,
                                       enum TyScope _scope)
    : lhs(std::move(_lhs)), rhs(std::move(_rhs)), scope(_scope) {}

void TyPropagateLessdef::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(lhs), CEREAL_NVP(rhs),
          cereal::make_nvp("scope", ::toString(scope)));
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
          cereal::make_nvp("scope", ::toString(scope)));
}

TyPropagateAlloca::TyPropagateAlloca(std::unique_ptr<TyRegister> _p, 
                                     enum TyScope _scope) 
    : p(std::move(_p)), scope(std::move(_scope)) {
}

void TyPropagateAlloca::serialize(cereal::JSONOutputArchive& archive) const {
  archive(CEREAL_NVP(p));
  archive(cereal::make_nvp("scope", ::toString(scope)));
}

TyPropagatePrivate::TyPropagatePrivate(std::unique_ptr<TyRegister> _p, 
                                       enum TyScope _scope) 
    : p(std::move(_p)), scope(std::move(_scope)) {
}

void TyPropagatePrivate::serialize(cereal::JSONOutputArchive& archive) const {
  archive(CEREAL_NVP(p));
  archive(cereal::make_nvp("scope", ::toString(scope)));
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

ConsAlloca::ConsAlloca(std::unique_ptr<TyPropagateAlloca> _propagate_alloca) 
    : propagate_alloca(std::move(_propagate_alloca)) {}

std::unique_ptr<TyPropagateObject> ConsAlloca::make(std::unique_ptr<TyRegister> _p, 
                                                    enum TyScope _scope) {
  std::unique_ptr<TyPropagateAlloca> _val
                    (new TyPropagateAlloca(std::move(_p), _scope));

  return std::unique_ptr<TyPropagateObject>(new ConsAlloca(std::move(_val)));
}

void ConsAlloca::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("Alloca");
  archive(CEREAL_NVP(propagate_alloca));
}

ConsMaydiff::ConsMaydiff(std::unique_ptr<TyRegister> _register_name)
    : register_name(std::move(_register_name)) {}

ConsMaydiff::ConsMaydiff(std::string _name, enum TyTag _tag)
    : register_name(new TyRegister(_name, _tag)) {}

std::unique_ptr<TyPropagateObject> ConsMaydiff::make(std::string _name,
                                                     enum TyTag _tag) {
  return std::unique_ptr<TyPropagateObject>
          (new ConsMaydiff(TyRegister::make(_name, _tag)));
}

void ConsMaydiff::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("Maydiff");
  archive(CEREAL_NVP(register_name));
}

ConsPrivate::ConsPrivate(std::unique_ptr<TyPropagatePrivate> _propagate_private) 
    : propagate_private(std::move(_propagate_private)) {}

std::unique_ptr<TyPropagateObject> ConsPrivate::make(std::unique_ptr<TyRegister> _p, 
                                                     enum TyScope _scope) {
  std::unique_ptr<TyPropagatePrivate> _val
                    (new TyPropagatePrivate(std::move(_p), _scope));

  return std::unique_ptr<TyPropagateObject>(new ConsPrivate(std::move(_val)));
}

void ConsPrivate::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("Private");
  archive(CEREAL_NVP(propagate_private));
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

void CoreHint::addNopPosition(std::unique_ptr<TyPosition> position) {
  nop_positions.push_back(std::move(position));
}

void CoreHint::serialize(cereal::JSONOutputArchive &archive) const {
  std::string function_id = std::string("@") + this->function_id;
  archive(CEREAL_NVP(module_id));
  archive(CEREAL_NVP(function_id));
  archive(CEREAL_NVP(opt_name));
  archive(CEREAL_NVP(commands));
  archive(CEREAL_NVP(nop_positions));
}

} // llvmberry
