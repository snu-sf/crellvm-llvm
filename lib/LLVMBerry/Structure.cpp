#include <sstream>
#include <fstream>
#include <string>
#include <cassert>
#include <iomanip>
#include <cereal/archives/json.hpp>
#include <cereal/types/vector.hpp>
#include <cereal/types/memory.hpp>
#include <cereal/types/polymorphic.hpp>
#include <cfloat>
#include "llvm/LLVMBerry/Structure.h"
#include "llvm/LLVMBerry/ValidationUnit.h"
#include "llvm/LLVMBerry/Infrules.h"
#include "llvm/LLVMBerry/RuntimeOptions.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/StringExtras.h"

namespace cereal {
[[noreturn]] void throw_exception(std::exception const &e) { std::exit(1); }
}
namespace boost {
void throw_exception(std::exception const &e) {
  std::cerr << " boost::throw_exception(e) called." << std::endl;
  std::cerr << e.what() << std::endl;
  std::exit(1);
}
}

namespace {

std::string toString(llvmberry::CoreHint::RETURN_CODE return_code) {
  switch (return_code) {
  case llvmberry::CoreHint::ACTUAL:
    return std::string("ACTUAL");
  case llvmberry::CoreHint::ADMITTED:
    return std::string("ADMITTED");
  case llvmberry::CoreHint::FAIL:
    return std::string("FAIL");
  default:
    assert(false && "RETURN_CODE toString");
  }
  return "";
}

std::string toString(llvmberry::CoreHint::AUTO_OPT auto_opt) {
  switch (auto_opt) {
  case llvmberry::CoreHint::AUTO_GVN:
    return std::string("AUTO_GVN");
  case llvmberry::CoreHint::AUTO_SROA:
    return std::string("AUTO_SROA");
  case llvmberry::CoreHint::AUTO_INSTCOMBINE:
    return std::string("AUTO_INSTCOMBINE");
  case llvmberry::CoreHint::AUTO_DEFAULT:
    return std::string("AUTO_DEFAULT");
  default:
    assert(false && "RETURN_CODE toString");
  }
  return "";
}

std::string toString(llvmberry::TyScope scope) {
  switch (scope) {
  case llvmberry::Source:
    return std::string("Source");
  case llvmberry::Target:
    return std::string("Target");
  default:
    assert(false && "Scope toString");
  }
  return "";
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
  return "";
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



// Below code PrintEscapedString is exactly copied from lib/IR/AsmWriter.cpp

// PrintEscapedString - Print each character of the specified string, escaping
// it if it is not printable or if it is an escape char.
static void PrintEscapedString(llvm::StringRef Name, llvm::raw_ostream &Out) {
  for (unsigned i = 0, e = Name.size(); i != e; ++i) {
    unsigned char C = Name[i];
    if (isprint(C) && C != '\\' && C != '"')
      Out << C;
    else
      Out << '\\' << llvm::hexdigit(C >> 4) << llvm::hexdigit(C & 0x0F);
  }
}

// Below code PrefixType is exactly copied from lib/IR/AsmWriter.cpp
enum PrefixType {
  GlobalPrefix,
  ComdatPrefix,
  LabelPrefix,
  LocalPrefix,
  NoPrefix
};

// Below code PrintLLVMName is exactly copied from lib/IR/AsmWriter.cpp

/// PrintLLVMName - Turn the specified name into an 'LLVM name', which is either
/// prefixed with % (if the string only contains simple characters) or is
/// surrounded with ""'s (if it has special chars in it).  Print it out.
static void PrintLLVMName(llvm::raw_ostream &OS, llvm::StringRef Name, PrefixType Prefix) {
  assert(!Name.empty() && "Cannot get empty name!");
  switch (Prefix) {
  case NoPrefix: break;
  case GlobalPrefix: OS << '@'; break;
  case ComdatPrefix: OS << '$'; break;
  case LabelPrefix:  break;
  case LocalPrefix:  OS << '%'; break;
  }

  // Scan the name to see if it needs quotes first.
  bool NeedsQuotes = isdigit(static_cast<unsigned char>(Name[0]));
  if (!NeedsQuotes) {
    for (unsigned i = 0, e = Name.size(); i != e; ++i) {
      // By making this unsigned, the value passed in to isalnum will always be
      // in the range 0-255.  This is important when building with MSVC because
      // its implementation will assert.  This situation can arise when dealing
      // with UTF-8 multibyte characters.
      unsigned char C = Name[i];
      if (!isalnum(static_cast<unsigned char>(C)) && C != '-' && C != '.' &&
          C != '_') {
        NeedsQuotes = true;
        break;
      }
    }
  }

  // If we didn't need any quotes, just write out the name in one blast.
  if (!NeedsQuotes) {
    OS << Name;
    return;
  }

  // Okay, we need quotes.  Output the quotes and escape any scary characters as
  // needed.
  OS << '"';
  PrintEscapedString(Name, OS);
  OS << '"';
}

// Below code PrintLLVMName is exactly copied from lib/IR/AsmWriter.cpp

/// PrintLLVMName - Turn the specified name into an 'LLVM name', which is either
/// prefixed with % (if the string only contains simple characters) or is
/// surrounded with ""'s (if it has special chars in it).  Print it out.
static void PrintLLVMName(llvm::raw_ostream &OS, const llvm::Value *V) {
  PrintLLVMName(OS, V->getName(),
                llvm::isa<llvm::GlobalValue>(V) ? GlobalPrefix : LocalPrefix);
}




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
    std::string tempstr;
    llvm::raw_string_ostream rso(tempstr);
    PrintLLVMName(rso, block);
    assert(rso.str()[0] == '%');
    return rso.str().substr(1); //to remove %
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
  if (value.hasName()) {
    std::string tempstr;
    llvm::raw_string_ostream rso(tempstr);
    PrintLLVMName(rso, &value);
    return rso.str();
  }

  std::string val;

  if (llvm::isa<llvm::GlobalValue>(value)) {
    val = std::string("@");
  } else if (llvm::isa<llvm::Instruction>(value) ||
             llvm::isa<llvm::Argument>(value) ||
             llvm::isa<llvm::ConstantExpr>(value) ||
             llvm::isa<llvm::ConstantPointerNull>(value) ||
             llvm::isa<llvm::UndefValue>(value)) {
    val = std::string("%");
  } else {
    assert("value must be a global value or an instruction" && false);
  }

  val += std::string(value.getName().data());

  if (val == "%")
    val = "";

  return val;
}

llvm::Instruction *getPHIResolved(llvm::Instruction *I, llvm::BasicBlock *PB) {
  llvm::Instruction *result = I->clone();
  for (unsigned i = 0, e = I->getNumOperands(); i != e; ++i) {
    llvm::Value *Op = I->getOperand(i);
    if (llvm::PHINode *OpPHI = llvm::dyn_cast<llvm::PHINode>(Op)) {
      if (I->getParent() != OpPHI->getParent())
        continue;
      result->setOperand(i, OpPHI->getIncomingValueForBlock(PB));
    }
  }
  return result;
}

std::string toString(llvmberry::TyFbop bop) {
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
  return "";
}

std::string toString(llvmberry::TyBop bop) {
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
  return "";
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
  return "";
}

std::string toString(llvmberry::TyIcmpPred cond) {
  switch (cond) {
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
  return "";
}

std::string toString(llvmberry::TyFcmpPred fcond) {
  switch (fcond) {
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
    assert("llvmberry::toString(llvmberry::TyFCond fcond) : unknown fcond" &&
           false);
  }
  return "";
}

llvmberry::TyFloatType getFloatType(llvm::Type *typ) {
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

bool isFloatOpcode(llvm::Instruction::BinaryOps ops) {
  switch (ops) {
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

TyFbop getFbop(llvm::Instruction::BinaryOps ops) {
  TyFbop tyfbop;
  switch (ops) {
  case llvm::Instruction::FAdd:
    tyfbop = llvmberry::BopFadd;
    break;
  case llvm::Instruction::FSub:
    tyfbop = llvmberry::BopFsub;
    break;
  case llvm::Instruction::FMul:
    tyfbop = llvmberry::BopFmul;
    break;
  case llvm::Instruction::FDiv:
    tyfbop = llvmberry::BopFdiv;
    break;
  case llvm::Instruction::FRem:
    tyfbop = llvmberry::BopFrem;
    break;
  default:
    assert(
        "llvmberry::getFbop(llvm::Instruction::BinaryOps) : unknown opcode" &&
        false);
  }
  return tyfbop;
}

TyBop getBop(llvm::Instruction::BinaryOps ops) {
  TyBop bop;
  switch (ops) {
  case llvm::Instruction::Add:
    bop = llvmberry::BopAdd;
    break;
  case llvm::Instruction::Sub:
    bop = llvmberry::BopSub;
    break;
  case llvm::Instruction::Mul:
    bop = llvmberry::BopMul;
    break;
  case llvm::Instruction::UDiv:
    bop = llvmberry::BopUdiv;
    break;
  case llvm::Instruction::SDiv:
    bop = llvmberry::BopSdiv;
    break;
  case llvm::Instruction::URem:
    bop = llvmberry::BopUrem;
    break;
  case llvm::Instruction::SRem:
    bop = llvmberry::BopSrem;
    break;
  case llvm::Instruction::Shl:
    bop = llvmberry::BopShl;
    break;
  case llvm::Instruction::LShr:
    bop = llvmberry::BopLshr;
    break;
  case llvm::Instruction::AShr:
    bop = llvmberry::BopAshr;
    break;
  case llvm::Instruction::And:
    bop = llvmberry::BopAnd;
    break;
  case llvm::Instruction::Or:
    bop = llvmberry::BopOr;
    break;
  case llvm::Instruction::Xor:
    bop = llvmberry::BopXor;
    break;
  default:
    assert("llvmberry::getBop(llvm::Instruction::BinaryOps) : unknown opcode" &&
           false);
  }
  return bop;
}

TyIcmpPred getIcmpPred(llvm::ICmpInst::Predicate prd) {
  TyIcmpPred predicate;

  switch (prd) {
  case llvm::ICmpInst::ICMP_EQ:
    predicate = llvmberry::CondEq;
    break;
  case llvm::ICmpInst::ICMP_NE:
    predicate = llvmberry::CondNe;
    break;
  case llvm::ICmpInst::ICMP_UGT:
    predicate = llvmberry::CondUgt;
    break;
  case llvm::ICmpInst::ICMP_UGE:
    predicate = llvmberry::CondUge;
    break;
  case llvm::ICmpInst::ICMP_ULT:
    predicate = llvmberry::CondUlt;
    break;
  case llvm::ICmpInst::ICMP_ULE:
    predicate = llvmberry::CondUle;
    break;
  case llvm::ICmpInst::ICMP_SGT:
    predicate = llvmberry::CondSgt;
    break;
  case llvm::ICmpInst::ICMP_SGE:
    predicate = llvmberry::CondSge;
    break;
  case llvm::ICmpInst::ICMP_SLT:
    predicate = llvmberry::CondSlt;
    break;
  case llvm::ICmpInst::ICMP_SLE:
    predicate = llvmberry::CondSle;
    break;
  default:
    assert("llvmberry::getIcmpPred(llvm::ICmpInst::Predicate prd) : unknown "
           "predicate" &&
           false);
  }
  return predicate;
}

TyFcmpPred getFcmpPred(llvm::FCmpInst::Predicate prd) {
  TyFcmpPred predicate;

  switch (prd) {
  case llvm::FCmpInst::FCMP_FALSE:
    predicate = llvmberry::CondFfalse;
    break;
  case llvm::FCmpInst::FCMP_OEQ:
    predicate = llvmberry::CondFoeq;
    break;
  case llvm::FCmpInst::FCMP_OGT:
    predicate = llvmberry::CondFogt;
    break;
  case llvm::FCmpInst::FCMP_OGE:
    predicate = llvmberry::CondFoge;
    break;
  case llvm::FCmpInst::FCMP_OLT:
    predicate = llvmberry::CondFolt;
    break;
  case llvm::FCmpInst::FCMP_OLE:
    predicate = llvmberry::CondFole;
    break;
  case llvm::FCmpInst::FCMP_ONE:
    predicate = llvmberry::CondFone;
    break;
  case llvm::FCmpInst::FCMP_ORD:
    predicate = llvmberry::CondFord;
    break;
  case llvm::FCmpInst::FCMP_UNO:
    predicate = llvmberry::CondFuno;
    break;
  case llvm::FCmpInst::FCMP_UEQ:
    predicate = llvmberry::CondFueq;
    break;
  case llvm::FCmpInst::FCMP_UGT:
    predicate = llvmberry::CondFugt;
    break;
  case llvm::FCmpInst::FCMP_UGE:
    predicate = llvmberry::CondFuge;
    break;
  case llvm::FCmpInst::FCMP_ULT:
    predicate = llvmberry::CondFult;
    break;
  case llvm::FCmpInst::FCMP_ULE:
    predicate = llvmberry::CondFule;
    break;
  case llvm::FCmpInst::FCMP_UNE:
    predicate = llvmberry::CondFune;
    break;
  case llvm::FCmpInst::FCMP_TRUE:
    predicate = llvmberry::CondFtrue;
    break;
  default:
    assert("llvmberry::getFCmpPred(llvm::FCmpInst::Predicate pred) : unknown "
           "predicate" &&
           false);
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

bool name_instruction(llvm::Instruction &I) {
  llvm::BasicBlock *BB = I.getParent();
  if (!BB->hasName())
    BB->setName("bb");

  if (!I.hasName() && !I.getType()->isVoidTy())
    I.setName("tmp");

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
    : position_phinode(_position_phinode) {}

void ConsPhinode::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("Phinode");
  archive(CEREAL_NVP(position_phinode));
}

ConsCommand::ConsCommand(std::shared_ptr<TyPositionCommand> _position_command)
    : position_command(_position_command) {}

void ConsCommand::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("Command");
  archive(CEREAL_NVP(position_command));
}

TyPosition::TyPosition(enum TyScope _scope, std::string _block_name,
                       std::shared_ptr<TyInstrIndex> _instr_index)
    : scope(_scope), block_name(_block_name),
      instr_index(_instr_index) {}

void TyPosition::serialize(cereal::JSONOutputArchive &archive) const {
  archive(cereal::make_nvp("scope", ::toString(scope)), CEREAL_NVP(block_name),
          CEREAL_NVP(instr_index));
}

std::shared_ptr<TyPosition> TyPosition::make(enum TyScope _scope,
                                             std::string _block_name,
                                             std::string _prev_block_name) {
  std::shared_ptr<TyPositionPhinode> _pos_phi(
      new TyPositionPhinode(_prev_block_name));

  std::shared_ptr<TyInstrIndex> _phi(new ConsPhinode(_pos_phi));

  return std::shared_ptr<TyPosition>(
      new TyPosition(_scope, _block_name, _phi));
}

std::shared_ptr<TyPosition>
TyPosition::make_start_of_block(enum TyScope _scope, std::string _block_name) {
  return TyPosition::make(_scope, _block_name, "");
}

std::shared_ptr<TyPosition> TyPosition::make(enum TyScope _scope,
                                             const llvm::Instruction &I) {
  std::string empty_str = "";
  return TyPosition::make(_scope, I, empty_str);
}

std::shared_ptr<TyPosition> TyPosition::make(enum TyScope _scope,
                                             const llvm::Instruction &I,
                                             std::string _prev_block_name) {

  std::string _block_name = getBasicBlockIndex(I.getParent());
  std::string _register_name = getVariable(I);

  std::shared_ptr<TyInstrIndex> _instr_index;

  if (llvm::isa<llvm::PHINode>(I)) {
    std::shared_ptr<TyPositionPhinode> _pos_phi(
        new TyPositionPhinode(_prev_block_name));

    std::shared_ptr<TyInstrIndex> _phi(new ConsPhinode(_pos_phi));

    _instr_index = _phi;
  } else {
    int _index;
    if (llvm::isa<llvm::TerminatorInst>(I)) {
      _index = getTerminatorIndex(llvm::dyn_cast<llvm::TerminatorInst>(&I));
    } else {
      _index = getCommandIndex(I);
    }
    std::shared_ptr<TyPositionCommand> _pos_cmd(
        new TyPositionCommand(_index, _register_name));

    std::shared_ptr<TyInstrIndex> _cmd(new ConsCommand(_pos_cmd));

    _instr_index = _cmd;
  }

  return std::shared_ptr<TyPosition>(
      new TyPosition(_scope, _block_name, _instr_index));
}

std::shared_ptr<TyPosition> TyPosition::make(enum TyScope _scope,
                                             const llvm::Instruction &I,
                                             int index,
                                             std::string _prev_block_name) {
  std::string _block_name = getBasicBlockIndex(I.getParent());
  std::string _register_name = getVariable(I);

  std::shared_ptr<TyInstrIndex> _instr_index;

  if (llvm::isa<llvm::PHINode>(I)) {
    std::shared_ptr<TyPositionPhinode> _pos_phi(
        new TyPositionPhinode(_prev_block_name));
    std::shared_ptr<TyInstrIndex> _phi(new ConsPhinode(_pos_phi));
    _instr_index = _phi;
  } else if (index < 0) {
    // if index is less than 0, it means start of block
    return TyPosition::make_start_of_block(_scope, _block_name);
  } else {
    std::shared_ptr<TyPositionCommand> _pos_cmd(
        new TyPositionCommand(index, _register_name));
    std::shared_ptr<TyInstrIndex> _cmd(new ConsCommand(_pos_cmd));

    _instr_index = _cmd;
  }

  return std::shared_ptr<TyPosition>(
      new TyPosition(_scope, _block_name, _instr_index));
}
std::shared_ptr<TyPosition> TyPosition::make(enum TyScope _scope,
                                             const llvm::BasicBlock &BB,
                                             int index) {

  std::string _block_name = getBasicBlockIndex(&BB);
  std::string _register_name = "";

  std::shared_ptr<TyInstrIndex> _instr_index;
   if (index < 0) {
    // if index is less than 0, it means start of block
    return TyPosition::make_start_of_block(_scope, _block_name);
  } else {
    std::shared_ptr<TyPositionCommand> _pos_cmd(
            new TyPositionCommand(index, _register_name));
    std::shared_ptr<TyInstrIndex> _cmd(new ConsCommand(_pos_cmd));

    _instr_index = _cmd;
  }

  return std::shared_ptr<TyPosition>(
          new TyPosition(_scope, _block_name, _instr_index));
}


std::shared_ptr<TyPosition>
TyPosition::make_end_of_block(enum TyScope _scope, const llvm::BasicBlock &BB) {

  const llvm::TerminatorInst *term = BB.getTerminator();

  std::string _block_name = getBasicBlockIndex(&BB);
  std::string _register_name = "";

  int _index = getTerminatorIndex(term);

  std::shared_ptr<TyPositionCommand> _pos_cmd(
      new TyPositionCommand(_index, _register_name));

  std::shared_ptr<TyInstrIndex> _cmd(new ConsCommand(_pos_cmd));

  return std::shared_ptr<TyPosition>(
      new TyPosition(_scope, _block_name, _cmd));
}

std::shared_ptr<TyPosition>
TyPosition::make_end_of_block(enum TyScope _scope, const llvm::BasicBlock &BB,
                              int index) {
  std::string _block_name = getBasicBlockIndex(&BB);
  std::string _register_name = "";

  int _index = index;

  std::shared_ptr<TyPositionCommand> _pos_cmd(
      new TyPositionCommand(_index, _register_name));

  std::shared_ptr<TyInstrIndex> _cmd(new ConsCommand(_pos_cmd));

  return std::shared_ptr<TyPosition>(
      new TyPosition(_scope, _block_name, _cmd));
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

bool TyRegister::isSame(std::shared_ptr<TyRegister> r1,
                        std::shared_ptr<TyRegister> r2) {
  return (r1->name==r2->name && r1->tag==r2->tag);
}

std::string TyRegister::getName() {
  return name;
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
    : int_value(_int_value), int_type(_int_type) {}

TyConstInt::TyConstInt(int64_t _int_value, int _bitwidth)
    : int_value(_int_value), int_type(new ConsIntType(_bitwidth)) {}

void TyConstInt::serialize(cereal::JSONOutputArchive &archive) const {
  std::string str;
  std::stringstream ss;
  ss << int_value;
  ss >> str;
  archive(cereal::make_nvp("int_value", str), CEREAL_NVP(int_type));
}

std::shared_ptr<TyConstInt> TyConstInt::make(int64_t _int_value, int _value) {
  return std::shared_ptr<TyConstInt>(new TyConstInt(_int_value, _value));
}

std::shared_ptr<TyConstInt> TyConstInt::make(const llvm::ConstantInt &ci) {
  return TyConstInt::make(ci.getSExtValue(), ci.getBitWidth());
}

TyConstFloat::TyConstFloat(double _float_value, enum TyFloatType _float_type)
    : float_value(_float_value), float_type(_float_type) {}

void TyConstFloat::serialize(cereal::JSONOutputArchive &archive) const {
  std::stringstream ss;
  ss << std::setprecision(DECIMAL_DIG) // No digit loss
     << float_value;
  std::string res = ss.str();
  archive(cereal::make_nvp("float_value", res));
  archive(cereal::make_nvp("float_type", toString(float_type)));
}

std::shared_ptr<TyConstFloat> TyConstFloat::make(double _float_value,
                                                 enum TyFloatType _float_type) {
  return std::shared_ptr<TyConstFloat>(
      new TyConstFloat(_float_value, _float_type));
}

TyConstGlobalVarAddr::TyConstGlobalVarAddr(
    std::string _var_id, std::shared_ptr<TyValueType> _var_type)
    : var_id(_var_id), var_type(_var_type) {}
void TyConstGlobalVarAddr::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(var_id));
  archive(CEREAL_NVP(var_type));
}

std::shared_ptr<TyConstGlobalVarAddr>
TyConstGlobalVarAddr::make(const llvm::GlobalObject &gv) {
  llvm::Type *ty = gv.getType();
  assert(ty->isPointerTy() &&
         "Global variables must be pointers to their locations."); // jylee
  ty = ty->getPointerElementType();

  return std::shared_ptr<TyConstGlobalVarAddr>(new TyConstGlobalVarAddr(
      std::string("@") + std::string(gv.getName().data()),
      TyValueType::make(*ty)));
}

/*
 * Value
 */
std::shared_ptr<TyValue> TyValue::make(const llvm::Value &value,
                                       enum TyTag _tag) {
  if (llvm::isa<llvm::Instruction>(value) || llvm::isa<llvm::Argument>(value)) {
    return std::shared_ptr<TyValue>(
        new ConsId(TyRegister::make(getVariable(value), _tag)));
  } else if (const llvm::ConstantExpr *ce =
                 llvm::dyn_cast<llvm::ConstantExpr>(&value)) {
    // Constant expressions have two kinds of forms :
    // (1) %x = add i32 1, 2
    //     ^^^^^^^^^^^^^^^^^
    // (2) %x = add i32 %y, add (i32 1, i32 2)
    //                      ^^^^^^^^^^^^^^^^^^
    // For the case (1), TyValue::make returns register id "%x"
    // For the case (2), TyValue::make returns a constant expression "add (i32
    // 1, i32 2)"
    if (ce->getName().str() != "") {
      return std::shared_ptr<TyValue>(
          new ConsId(TyRegister::make(getVariable(*ce), _tag)));
    } else {
      return std::shared_ptr<TyValue>(new ConsConstVal(TyConstant::make(*ce)));
    }
  } else if (const llvm::Constant *c = llvm::dyn_cast<llvm::Constant>(&value)) {
    return std::shared_ptr<TyValue>(new ConsConstVal(TyConstant::make(*c)));
  } else {
    assert("Unknown value type" && false);
  }
  return nullptr;
}

std::shared_ptr<TyInstruction>
instructionWithGhostIdxs(const llvm::Instruction &i,
                         std::vector<int> ghostIdxs) {
  if (ghostIdxs.size() == 0)
    return TyInstruction::make(i);
  if (auto si = llvm::dyn_cast<llvm::SelectInst>(&i)) {
    // TODO generalize a bit more to support previous?
    llvmberry::TyTag tags[3] = {llvmberry::Physical, llvmberry::Physical,
                                llvmberry::Physical};
    for (auto idx : ghostIdxs) {
      assert(0 <= idx && idx < i.getNumOperands());
      tags[idx] = llvmberry::Ghost;
    }

    auto ret = std::shared_ptr<TySelectInst>(
        new TySelectInst(TyValue::make(*(si->getCondition()), tags[0]),
                         TyValueType::make(*(si->getType())),
                         TyValue::make(*(si->getTrueValue()), tags[1]),
                         TyValue::make(*(si->getFalseValue()), tags[2])));

    return std::shared_ptr<TyInstruction>(new ConsSelectInst(ret));
  } else {
    std::string output;
    llvm::raw_string_ostream rso(output);
    i.print(rso);
    rso.str();
    std::cerr << output << std::endl;
    assert("instructionWithGhostIdxs : unsupporting instruction type" && false);
    return std::shared_ptr<TyInstruction>(nullptr);
  }
}

ConsConstInt::ConsConstInt(std::shared_ptr<TyConstInt> _const_int)
    : const_int(_const_int) {}

ConsConstInt::ConsConstInt(int64_t _int_value, int _bitwidth)
    : const_int(new TyConstInt(_int_value, _bitwidth)) {}

void ConsConstInt::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("ConstInt");
  archive(CEREAL_NVP(const_int));
}

ConsConstFloat::ConsConstFloat(std::shared_ptr<TyConstFloat> _const_float)
    : const_float(_const_float) {}

ConsConstFloat::ConsConstFloat(float _float_value, enum TyFloatType _float_type)
    : const_float(new TyConstFloat(_float_value, _float_type)) {}

void ConsConstFloat::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("ConstFloat");
  archive(CEREAL_NVP(const_float));
}

ConsConstGlobalVarAddr::ConsConstGlobalVarAddr(
    std::shared_ptr<TyConstGlobalVarAddr> _const_global_var_addr)
    : const_global_var_addr(_const_global_var_addr) {}
std::shared_ptr<TyConstant>
ConsConstGlobalVarAddr::make(std::string _var_id,
                             std::shared_ptr<TyValueType> _var_type) {
  std::shared_ptr<TyConstGlobalVarAddr> _val(
      new TyConstGlobalVarAddr(_var_id, _var_type));
  return std::shared_ptr<TyConstant>(
      new ConsConstGlobalVarAddr(_val));
}
std::shared_ptr<TyConstant>
ConsConstGlobalVarAddr::make(const llvm::GlobalVariable &gv) {
  return std::shared_ptr<TyConstant>(
      new ConsConstGlobalVarAddr(TyConstGlobalVarAddr::make(gv)));
}

ConsConstZeroInitializer::ConsConstZeroInitializer(std::shared_ptr<TyValueType> _value_type) : value_type(_value_type){
}
void ConsConstZeroInitializer::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("ConstZeroInitializer");
  if (auto p = std::dynamic_pointer_cast<ConsVoidType>(value_type)) {
    archive.writeName();
    archive.saveValue("VoidType");
  } else {
    archive(CEREAL_NVP(value_type));
  }
}

// constant expressions

std::shared_ptr<TyConstantExpr>
TyConstantExpr::make(const llvm::ConstantExpr &ce) {
  if (ce.getOpcode() == llvm::Instruction::GetElementPtr)
    return ConsConstExprGetElementPtr::make(ce);
  else if (ce.getOpcode() == llvm::Instruction::BitCast)
    return ConsConstExprBitcast::make(ce);
  else if (ce.getOpcode() == llvm::Instruction::IntToPtr)
    return ConsConstExprInttoptr::make(ce);
  else if (ce.getOpcode() == llvm::Instruction::PtrToInt)
    return ConsConstExprPtrtoint::make(ce);
  else if (llvm::Instruction::isBinaryOp(ce.getOpcode()))
    return ConsConstExprBinaryOp::make(ce);
  std::string output;
  llvm::raw_string_ostream rso(output);
  ce.print(rso);
  rso.str();
  std::cerr << "TyConstantExpr::make : Unsupported const expr : " << output << std::endl;
  assert("TyConstantExpr::make() : unsupported constant expression" && false);
  return nullptr;
}

ConsConstExprGetElementPtr::ConsConstExprGetElementPtr(
    std::shared_ptr<TyConstExprGetElementPtr> _const_expr_get_element_ptr)
    : const_expr_get_element_ptr(_const_expr_get_element_ptr) {}

std::shared_ptr<TyConstantExpr> ConsConstExprGetElementPtr::make(
    std::shared_ptr<TyValueType> _srcelemty, std::shared_ptr<TyConstant> _v,
    std::vector<std::shared_ptr<TyConstant>> _idxlist,
    std::shared_ptr<TyValueType> _dstty, bool _is_inbounds) {
  std::shared_ptr<TyConstExprGetElementPtr> _val(new TyConstExprGetElementPtr(
      _srcelemty, _v, _idxlist, _dstty, _is_inbounds));
  return std::shared_ptr<TyConstantExpr>(
      new ConsConstExprGetElementPtr(_val));
}

void ConsConstExprGetElementPtr::serialize(
    cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("ConstExprGetElementPtr");
  archive(CEREAL_NVP(const_expr_get_element_ptr));
}

std::shared_ptr<TyConstantExpr>
ConsConstExprGetElementPtr::make(const llvm::ConstantExpr &ce) {
  llvm::GetElementPtrInst *gepi = llvm::dyn_cast<llvm::GetElementPtrInst>(
      const_cast<llvm::ConstantExpr &>(ce).getAsInstruction());
  assert(gepi);
  llvm::Constant *ptr =
      llvm::dyn_cast<llvm::Constant>(gepi->getPointerOperand());
  assert(ptr);
  std::vector<std::shared_ptr<TyConstant>> idxlist;

  for (auto itr = gepi->idx_begin(); itr != gepi->idx_end(); itr++) {
    llvm::Constant *idx = llvm::dyn_cast<llvm::Constant>(*itr);
    assert(idx);
    idxlist.push_back(TyConstant::make(*idx));
  }
  bool inBounds = gepi->isInBounds();
  llvm::Type *srcty = gepi->getSourceElementType();
  llvm::Type *destty = gepi->getResultElementType();
  delete gepi;

  return make(TyValueType::make(*srcty), TyConstant::make(*ptr), idxlist,
              TyValueType::make(*destty), inBounds);
}

TyConstExprGetElementPtr::TyConstExprGetElementPtr(
    std::shared_ptr<TyValueType> _srcelemty, std::shared_ptr<TyConstant> _v,
    std::vector<std::shared_ptr<TyConstant>> _idxlist,
    std::shared_ptr<TyValueType> _dstty, bool _is_inbounds)
    : srcelemty(_srcelemty), v(_v), idxlist(_idxlist), dstty(_dstty),
      is_inbounds(_is_inbounds) {}
void TyConstExprGetElementPtr::serialize(
    cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(srcelemty));
  archive(CEREAL_NVP(v));
  archive(CEREAL_NVP(idxlist));
  archive(CEREAL_NVP(dstty));
  archive(CEREAL_NVP(is_inbounds));
}

TyConstExprBitcast::TyConstExprBitcast(std::shared_ptr<TyConstant> _v,
                                       std::shared_ptr<TyValueType> _dstty)
    : v(_v), dstty(_dstty) {}
void TyConstExprBitcast::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(v), CEREAL_NVP(dstty));
}

ConsConstExprBitcast::ConsConstExprBitcast(
    std::shared_ptr<TyConstExprBitcast> _const_expr_bitcast)
    : const_expr_bitcast(_const_expr_bitcast) {}

std::shared_ptr<TyConstantExpr>
ConsConstExprBitcast::make(std::shared_ptr<TyConstant> _v,
                           std::shared_ptr<TyValueType> _dstty) {
  std::shared_ptr<TyConstExprBitcast> _val(new TyConstExprBitcast(_v, _dstty));
  return std::shared_ptr<TyConstantExpr>(new ConsConstExprBitcast(_val));
}

std::shared_ptr<TyConstantExpr>
ConsConstExprBitcast::make(const llvm::ConstantExpr &ce) {
  llvm::BitCastInst *bi = llvm::dyn_cast<llvm::BitCastInst>(
      const_cast<llvm::ConstantExpr &>(ce).getAsInstruction());
  assert(bi);
  llvm::Constant *ptr = llvm::dyn_cast<llvm::Constant>(bi->getOperand(0));
  auto tyc = TyConstant::make(*ptr);
  auto tyvt = TyValueType::make(*bi->getDestTy());
  delete bi;
  return make(tyc, tyvt);
}
void ConsConstExprBitcast::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("ConstExprBitcast");
  archive(CEREAL_NVP(const_expr_bitcast));
}

ConsConstExprInttoptr::ConsConstExprInttoptr(std::shared_ptr<TyConstExprInttoptr> _const_expr_inttoptr) : const_expr_inttoptr(_const_expr_inttoptr){
}
std::shared_ptr<TyConstantExpr> ConsConstExprInttoptr::make(std::shared_ptr<TyConstant> _v, std::shared_ptr<TyValueType> _dstty){
  std::shared_ptr<TyConstExprInttoptr> _val(new TyConstExprInttoptr(_v, _dstty));
  return std::shared_ptr<TyConstantExpr>(new ConsConstExprInttoptr(_val));
}
std::shared_ptr<TyConstantExpr>
ConsConstExprInttoptr::make(const llvm::ConstantExpr &ce) {
  llvm::Constant *ptr = llvm::dyn_cast<llvm::Constant>(ce.getOperand(0));
  auto tyc = TyConstant::make(*ptr);
  auto tyvt = TyValueType::make(*ce.getType());
  return make(tyc, tyvt);
}
void ConsConstExprInttoptr::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("ConstExprInttoptr");
  archive(CEREAL_NVP(const_expr_inttoptr));
}

TyConstExprInttoptr::TyConstExprInttoptr(std::shared_ptr<TyConstant> _v, std::shared_ptr<TyValueType> _dstty) : v(_v), dstty(_dstty){
}
void TyConstExprInttoptr::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(v), CEREAL_NVP(dstty));
}

ConsConstExprPtrtoint::ConsConstExprPtrtoint(std::shared_ptr<TyConstExprPtrtoint> _const_expr_ptrtoint) : const_expr_ptrtoint(_const_expr_ptrtoint){
}
std::shared_ptr<TyConstantExpr> ConsConstExprPtrtoint::make(std::shared_ptr<TyConstant> _v, std::shared_ptr<TyValueType> _dstty){
  std::shared_ptr<TyConstExprPtrtoint> _val(new TyConstExprPtrtoint(_v, _dstty));
  return std::shared_ptr<TyConstantExpr>(new ConsConstExprPtrtoint(_val));
}
std::shared_ptr<TyConstantExpr>
ConsConstExprPtrtoint::make(const llvm::ConstantExpr &ce) {
  llvm::Constant *ptr = llvm::dyn_cast<llvm::Constant>(ce.getOperand(0));
  auto tyc = TyConstant::make(*ptr);
  auto tyvt = TyValueType::make(*ce.getType());
  return make(tyc, tyvt);
}
void ConsConstExprPtrtoint::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("ConstExprPtrtoint");
  archive(CEREAL_NVP(const_expr_ptrtoint));
}

TyConstExprPtrtoint::TyConstExprPtrtoint(std::shared_ptr<TyConstant> _v, std::shared_ptr<TyValueType> _dstty) : v(_v), dstty(_dstty){
}
void TyConstExprPtrtoint::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(v), CEREAL_NVP(dstty));
}

// ConsConstExprBinaryOp

ConsConstExprBinaryOp::ConsConstExprBinaryOp(std::shared_ptr<TyConstExprBinaryOp> _const_expr_binaryop) : const_expr_binaryop(_const_expr_binaryop){
}
std::shared_ptr<TyConstantExpr> ConsConstExprBinaryOp::make(TyBop tbop, std::shared_ptr<TyConstant> _v1, std::shared_ptr<TyConstant> _v2){
  std::shared_ptr<TyConstExprBinaryOp> _val(new TyConstExprBinaryOp(tbop, _v1, _v2));
  return std::shared_ptr<TyConstantExpr>(new ConsConstExprBinaryOp(_val));
}
std::shared_ptr<TyConstantExpr>
ConsConstExprBinaryOp::make(const llvm::ConstantExpr &ce) {
  TyBop bop = getBop((llvm::Instruction::BinaryOps)ce.getOpcode());
  llvm::Constant *v1 = llvm::dyn_cast<llvm::Constant>(ce.getOperand(0));
  llvm::Constant *v2 = llvm::dyn_cast<llvm::Constant>(ce.getOperand(1));
  auto c1 = TyConstant::make(*v1);
  auto c2 = TyConstant::make(*v2);
  return make(bop, c1, c2);
}
void ConsConstExprBinaryOp::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("ConstExprBinaryOp");
  archive(CEREAL_NVP(const_expr_binaryop));
}

TyConstExprBinaryOp::TyConstExprBinaryOp(TyBop tbop, std::shared_ptr<TyConstant> _v1, std::shared_ptr<TyConstant> _v2) : opcode(tbop), v1(_v1), v2(_v2) {}
void TyConstExprBinaryOp::serialize(cereal::JSONOutputArchive& archive) const{
  archive(cereal::make_nvp("opcode", toString(opcode)));
  archive(CEREAL_NVP(v1));
  archive(CEREAL_NVP(v2));
}


void ConsConstGlobalVarAddr::serialize(
    cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("ConstGlobalVarAddr");
  archive(CEREAL_NVP(const_global_var_addr));
}

ConsConstUndef::ConsConstUndef(std::shared_ptr<TyValueType> _value_type)
    : value_type(_value_type) {}

void ConsConstUndef::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("ConstUndef");
  archive(CEREAL_NVP(value_type));
}

ConsConstNull::ConsConstNull(int _address_space,
                             std::shared_ptr<TyValueType> _value_type)
    : address_space(_address_space), value_type(_value_type) {}

void ConsConstNull::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("ConstNull");

  archive.startNode();
  archive.makeArray();
  archive(cereal::make_nvp("address_space", address_space));
  archive(CEREAL_NVP(value_type));
  archive.finishNode();
}

ConsConstDataVector::ConsConstDataVector(
    std::shared_ptr<TyValueType> _elem_type,
    std::vector<std::shared_ptr<TyConstant> > &_elements)
    : elem_type(_elem_type), elements(_elements) {}
void ConsConstDataVector::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("ConstDataVector");
  archive.startNode();
  archive.makeArray();
  if (auto p = std::dynamic_pointer_cast<ConsVoidType>(elem_type)) {
    archive.writeName();
    archive.saveValue("VoidType");
  } else {
    archive(CEREAL_NVP(elem_type));
  }
  archive.startNode();
  archive.makeArray();
  archive(CEREAL_NVP(elements));
  archive.finishNode();
  archive.finishNode();
}

// values

ConsId::ConsId(std::shared_ptr<TyRegister> _register)
    : reg(_register) {}

void ConsId::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("Id");
  archive(CEREAL_NVP(reg));
}

std::shared_ptr<TyValue> ConsId::make(std::string _name, enum TyTag _tag) {
  std::shared_ptr<TyRegister> _reg(new TyRegister(_name, _tag));
  return std::shared_ptr<TyValue>(new ConsId(_reg));
}

ConsConstVal::ConsConstVal(std::shared_ptr<TyConstant> _constant)
    : constant(_constant) {}

void ConsConstVal::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("ConstVal");
  archive(CEREAL_NVP(constant));
}

ConsConstExpr::ConsConstExpr(std::shared_ptr<TyConstantExpr> _constant_expr)
    : constant_expr(_constant_expr) {}
void ConsConstExpr::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("ConstExpr");
  archive(CEREAL_NVP(constant_expr));
}
std::shared_ptr<TyConstant> ConsConstExpr::make(const llvm::ConstantExpr &gv) {
  return TyConstant::make(gv);
}

std::shared_ptr<TyConstant> TyConstant::make(const llvm::Constant &value) {
  if (const llvm::ConstantExpr *ce =
          llvm::dyn_cast<llvm::ConstantExpr>(&value)) {
    return std::shared_ptr<TyConstant>(
        new ConsConstExpr(TyConstantExpr::make(*ce)));

  } else if (const llvm::ConstantInt *v =
                 llvm::dyn_cast<llvm::ConstantInt>(&value)) {
    return std::shared_ptr<TyConstant>(new ConsConstInt(
        TyConstInt::make(v->getSExtValue(), v->getBitWidth())));

  } else if (const llvm::ConstantFP *v =
                 llvm::dyn_cast<llvm::ConstantFP>(&value)) {
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

    if (fty == llvmberry::DoubleType) {
      return std::shared_ptr<TyConstant>(
          new ConsConstFloat(TyConstFloat::make(apf.convertToDouble(), fty)));
    } else if (fty == llvmberry::FloatType) {
      return std::shared_ptr<TyConstant>(
          new ConsConstFloat(TyConstFloat::make(apf.convertToFloat(), fty)));
    } else {
      ValidationUnit::GetInstance()->intrude
        ([]
         (Dictionary &data, CoreHint &hints) {
        hints.appendToDescription("TyConstant::make() : Unsupported floating point type");
        hints.setReturnCodeToAdmitted();
      });

      return nullptr;
    }
  } else if (const llvm::GlobalVariable *gv =
                 llvm::dyn_cast<llvm::GlobalVariable>(&value)) {
    return std::shared_ptr<TyConstant>(
        new ConsConstGlobalVarAddr(TyConstGlobalVarAddr::make(*gv)));
  } else if (const llvm::Function *f = llvm::dyn_cast<llvm::Function>(&value)) {
    // NOTE : Vellvm uses const_gid to represent both global variable and
    // function
    return std::shared_ptr<TyConstant>(
        new ConsConstGlobalVarAddr(TyConstGlobalVarAddr::make(*f)));
  } else if (llvm::isa<llvm::UndefValue>(value)) {
    return std::shared_ptr<TyConstant>(
        new ConsConstUndef(TyValueType::make(*value.getType())));
  } else if (const llvm::ConstantPointerNull *null_val =
                 llvm::dyn_cast<llvm::ConstantPointerNull>(&value)) {
    const llvm::PointerType *ptype = null_val->getType();

    return std::shared_ptr<TyConstant>(
        new ConsConstNull(ptype->getAddressSpace(),
                          TyValueType::make(*ptype->getPointerElementType())));
  } else if (const llvm::ConstantDataVector *dv =
                 llvm::dyn_cast<llvm::ConstantDataVector>(&value)) {
    const llvm::Type *elemty = dv->getElementType();
    std::vector<std::shared_ptr<TyConstant> > elems;
    for (unsigned i = 0; i < dv->getNumElements(); i++) {
      elems.push_back(TyConstant::make(*dv->getElementAsConstant(i)));
    }
    return std::shared_ptr<TyConstant>(
        new ConsConstDataVector(TyValueType::make(*elemty), elems));
  } else if (const llvm::ConstantAggregateZero *az = 
                  llvm::dyn_cast<llvm::ConstantAggregateZero>(&value)) {
    return std::shared_ptr<TyConstant>(
        new ConsConstZeroInitializer(TyValueType::make(*az->getType())));
  }
  std::string output;
  llvm::raw_string_ostream rso(output);
  value.print(rso);
  rso.str();
  std::cerr << output << std::endl;
  assert("TyConstant::make() : unsupported value" && false);
  return nullptr;
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

/*
 * pointer
 */
TyPointer::TyPointer(std::shared_ptr<TyValue> _v,
                     std::shared_ptr<TyValueType> _ty)
    : v(_v), ty(_ty) {}

void TyPointer::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(v));
  archive(CEREAL_NVP(ty));
}

std::shared_ptr<TyPointer> TyPointer::make(const llvm::Value &v) {
  llvm::Type *ty = v.getType();
  assert(ty->isPointerTy());
  return std::shared_ptr<TyPointer>(
      new TyPointer(TyValue::make(v), TyValueType::make(*ty)));
}

std::shared_ptr<TyPointer> TyPointer::makeWithElementType(const llvm::Value &v) {
  llvm::PointerType *ty = llvm::dyn_cast<llvm::PointerType>(v.getType());
  assert(ty && "ty must be a pointer type.");
  return std::shared_ptr<TyPointer>(
      new TyPointer(TyValue::make(v), TyValueType::make(*(ty->getElementType()))));
}


// valuetype

std::shared_ptr<TyValueType> TyValueType::make(const llvm::Type &type) {
  TyValueType *vt;
  if (const llvm::IntegerType *itype =
          llvm::dyn_cast<llvm::IntegerType>(&type)) {
    vt = new ConsIntValueType(
        std::shared_ptr<TyIntType>(new ConsIntType(itype->getBitWidth())));
  } else if (const llvm::PointerType *ptype =
                 llvm::dyn_cast<llvm::PointerType>(&type)) {
    vt = new ConsPtrType(
        ptype->getAddressSpace(),
        TyValueType::make(*ptype->getPointerElementType()));
  } else if (const llvm::StructType *stype =
                 llvm::dyn_cast<llvm::StructType>(&type)) {
    if (stype->hasName())
      vt = new ConsNamedType(stype->getName().str());
    else {
      std::vector<std::shared_ptr<TyValueType>> elemtys;
      for (auto itr = stype->element_begin(); itr != stype->element_end(); itr++)
        elemtys.push_back(TyValueType::make(*(*itr)));
      vt = new ConsStructType(elemtys);
    }
  } else if (const llvm::ArrayType *atype =
                 llvm::dyn_cast<llvm::ArrayType>(&type)) {
    vt = new ConsArrayType(atype->getNumElements(),
                           TyValueType::make(*atype->getElementType()));
  } else if (const llvm::VectorType *vtype = 
                 llvm::dyn_cast<llvm::VectorType>(&type)) {
    vt = new ConsVectorType(vtype->getNumElements(),
                           TyValueType::make(*vtype->getElementType()));
  } else if (const llvm::FunctionType *ftype =
                 llvm::dyn_cast<llvm::FunctionType>(&type)) {
    std::vector<std::shared_ptr<TyValueType>> argtys;
    for (auto itr = ftype->param_begin(); itr != ftype->param_end(); itr++)
      argtys.push_back(TyValueType::make(**itr));
    vt = new ConsFunctionType(TyValueType::make(*ftype->getReturnType()),
                              argtys, ftype->isVarArg(), ftype->getNumParams());
  } else if (type.isVoidTy()) {
    vt = new ConsVoidType();
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
    std::string output;
    llvm::raw_string_ostream rso(output);
    type.print(rso);
    rso.str();
    std::cerr << output << std::endl;
    assert("TyValueType::make(const llvmType &) : unknown value type" && false);
    vt = nullptr;
  }

  return std::shared_ptr<TyValueType>(vt);
}

ConsVoidType::ConsVoidType() {}
void ConsVoidType::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("VoidType");
}

ConsIntValueType::ConsIntValueType(std::shared_ptr<TyIntType> _int_type)
    : int_type(_int_type) {}

void ConsIntValueType::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IntValueType");
  archive(CEREAL_NVP(int_type));
}

ConsFloatValueType::ConsFloatValueType(TyFloatType _float_type)
    : float_type(_float_type) {}

void ConsFloatValueType::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("FloatValueType");
  archive(cereal::make_nvp("float_type", toString(float_type)));
}

ConsNamedType::ConsNamedType(std::string _s) : s(_s) {}

void ConsNamedType::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("NamedType");
  archive(CEREAL_NVP(s));
}

ConsPtrType::ConsPtrType(int _address_space,
                         std::shared_ptr<TyValueType> _valuetype)
    : address_space(_address_space), valuetype(_valuetype) {}

void ConsPtrType::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("PtrType");

  archive.startNode();
  archive.makeArray();
  archive(cereal::make_nvp("address_space", address_space));
  archive(CEREAL_NVP(valuetype));
  archive.finishNode();
}

ConsArrayType::ConsArrayType(uint64_t _array_size,
                             std::shared_ptr<TyValueType> _valuetype)
    : array_size(_array_size), valuetype(_valuetype) {}

void ConsArrayType::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("ArrayType");

  archive.startNode();
  archive.makeArray();
  archive(cereal::make_nvp("array_size", array_size));
  archive(CEREAL_NVP(valuetype));
  archive.finishNode();
}

ConsVectorType::ConsVectorType(uint64_t _array_size,
                             std::shared_ptr<TyValueType> _valuetype)
    : array_size(_array_size), valuetype(_valuetype) {}

void ConsVectorType::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("VectorType");

  archive.startNode();
  archive.makeArray();
  archive(cereal::make_nvp("array_size", array_size));
  archive(CEREAL_NVP(valuetype));
  archive.finishNode();
}

ConsFunctionType::ConsFunctionType(
    std::shared_ptr<TyValueType> _ret_type,
    std::vector<std::shared_ptr<TyValueType>> &_arg_ty_list, bool _is_vararg,
    int _vararg_size)
    : ret_type(_ret_type), arg_ty_list(_arg_ty_list), is_vararg(_is_vararg),
      vararg_size(_vararg_size) {}
void ConsFunctionType::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("FunctionType");
  archive.startNode();
  archive.makeArray();
  if(std::shared_ptr<ConsVoidType> p = std::dynamic_pointer_cast<ConsVoidType>(ret_type)){
    archive.writeName();
    archive.saveValue("VoidType");
  }else
    archive(CEREAL_NVP(ret_type));
  archive.startNode();
  archive.makeArray();
  archive(CEREAL_NVP(arg_ty_list));
  archive.finishNode();
  archive(CEREAL_NVP(is_vararg));
  archive(CEREAL_NVP(vararg_size));
  archive.finishNode();
}

ConsStructType::ConsStructType(std::vector<std::shared_ptr<TyValueType>>& _vec_value_type) : vec_value_type(_vec_value_type){
}
void ConsStructType::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("StructType");
  archive.startNode();
  archive.makeArray();
  archive(CEREAL_NVP(vec_value_type));
  archive.finishNode();
}

// instruction

std::shared_ptr<TyInstruction> TyInstruction::make(const llvm::Instruction &i) {
  if (const llvm::BinaryOperator *bo =
          llvm::dyn_cast<llvm::BinaryOperator>(&i)) {
    if (isFloatOpcode(bo->getOpcode()))
      return std::shared_ptr<TyInstruction>(
          new ConsFloatBinaryOp(TyFloatBinaryOperator::make(*bo)));
    else
      return std::shared_ptr<TyInstruction>(
          new ConsBinaryOp(TyBinaryOperator::make(*bo)));
  } else if (const llvm::ICmpInst *icmp = llvm::dyn_cast<llvm::ICmpInst>(&i)) {
    return std::shared_ptr<TyInstruction>(
        new ConsICmpInst(TyICmpInst::make(*icmp)));
  } else if (const llvm::FCmpInst *fcmp = llvm::dyn_cast<llvm::FCmpInst>(&i)) {
    return std::shared_ptr<TyInstruction>(
        new ConsFCmpInst(TyFCmpInst::make(*fcmp)));
  } else if (const llvm::AllocaInst *ai = llvm::dyn_cast<llvm::AllocaInst>(&i)) {
    return std::shared_ptr<TyInstruction>(
        new ConsLoadInst(TyLoadInst::make(*ai)));
  } else if (const llvm::LoadInst *li = llvm::dyn_cast<llvm::LoadInst>(&i)) {
    return std::shared_ptr<TyInstruction>(
        new ConsLoadInst(TyLoadInst::make(*li)));
  } else if (const llvm::StoreInst *si = llvm::dyn_cast<llvm::StoreInst>(&i)) {
    return std::shared_ptr<TyInstruction>(
        new ConsLoadInst(TyLoadInst::make(*si)));
  } else if (const llvm::SelectInst *si = llvm::dyn_cast<llvm::SelectInst>(&i)) {
    return std::shared_ptr<TyInstruction>(
        new ConsSelectInst(TySelectInst::make(*si)));
  } else if (const llvm::BitCastInst *bci =
                 llvm::dyn_cast<llvm::BitCastInst>(&i)) {
    return std::shared_ptr<TyInstruction>(
        new ConsBitCastInst(TyBitCastInst::make(*bci)));
  } else if (const llvm::IntToPtrInst *itpi =
                 llvm::dyn_cast<llvm::IntToPtrInst>(&i)) {
    return std::shared_ptr<TyInstruction>(
        new ConsIntToPtrInst(TyIntToPtrInst::make(*itpi)));
  } else if (const llvm::PtrToIntInst *ptii =
                 llvm::dyn_cast<llvm::PtrToIntInst>(&i)) {
    return std::shared_ptr<TyInstruction>(
        new ConsPtrToIntInst(TyPtrToIntInst::make(*ptii)));
  } else if (const llvm::GetElementPtrInst *gepi =
                 llvm::dyn_cast<llvm::GetElementPtrInst>(&i)) {
    return std::shared_ptr<TyInstruction>(
        new ConsGetElementPtrInst(TyGetElementPtrInst::make(*gepi)));
  } else if (const llvm::FPExtInst *fpei = llvm::dyn_cast<llvm::FPExtInst>(&i)) {
    return std::shared_ptr<TyInstruction>(
        new ConsFpextInst(TyFpextInst::make(*fpei)));
  } else if (const llvm::FPTruncInst *fpti = llvm::dyn_cast<llvm::FPTruncInst>(&i)) {
    return std::shared_ptr<TyInstruction>(
        new ConsFptruncInst(TyFptruncInst::make(*fpti)));
  } else if (const llvm::ZExtInst *zi = llvm::dyn_cast<llvm::ZExtInst>(&i)) {
    return std::shared_ptr<TyInstruction>(
        new ConsZextInst(TyZextInst::make(*zi)));
  } else if (const llvm::SExtInst *si = llvm::dyn_cast<llvm::SExtInst>(&i)) {
    return std::shared_ptr<TyInstruction>(
        new ConsSextInst(TySextInst::make(*si)));
  } else if (const llvm::TruncInst *ti = llvm::dyn_cast<llvm::TruncInst>(&i)) {
    return std::shared_ptr<TyInstruction>(
        new ConsTruncInst(TyTruncInst::make(*ti)));
  } else if (const llvm::UIToFPInst *ui = llvm::dyn_cast<llvm::UIToFPInst>(&i)) {
    return std::shared_ptr<TyInstruction>(
        new ConsUitofpInst(TyUitofpInst::make(*ui)));
  } else if (const llvm::FPToSIInst *fp =
                 llvm::dyn_cast<llvm::FPToSIInst>(&i)) {
    return std::shared_ptr<TyInstruction>(
        new ConsFptosiInst(TyFptosiInst::make(*fp)));
  } else if (const llvm::SIToFPInst *si = llvm::dyn_cast<llvm::SIToFPInst>(&i)) {
    return std::shared_ptr<TyInstruction>(
        new ConsSitofpInst(TySitofpInst::make(*si)));
  } else if (const llvm::InsertValueInst *ivi = llvm::dyn_cast<llvm::InsertValueInst>(&i)) {
    return std::shared_ptr<TyInstruction>(
        new ConsInsertValueInst(TyInsertValueInst::make(*ivi)));
  } else if (const llvm::ExtractValueInst *evi = llvm::dyn_cast<llvm::ExtractValueInst>(&i)) {
    return std::shared_ptr<TyInstruction>(
        new ConsExtractValueInst(TyExtractValueInst::make(*evi)));
  } else {
    std::string output;
    llvm::raw_string_ostream rso(output);
    i.print(rso);
    rso.str();
    std::cerr << output << std::endl;
    assert("TyInstruction::make : unsupporting instruction type" && false);
    return std::shared_ptr<TyInstruction>(nullptr);
  }
}

// instruction make classes

std::shared_ptr<TyBitCastInst>
TyBitCastInst::make(const llvm::BitCastInst &bci) {
  return std::shared_ptr<TyBitCastInst>(new TyBitCastInst(
      TyValueType::make(*bci.getSrcTy()), TyValue::make(*bci.getOperand(0)),
      TyValueType::make(*bci.getDestTy())));
}

std::shared_ptr<TyIntToPtrInst>
TyIntToPtrInst::make(const llvm::IntToPtrInst &itpi) {
  return std::shared_ptr<TyIntToPtrInst>(new TyIntToPtrInst(
      TyValueType::make(*itpi.getSrcTy()), TyValue::make(*itpi.getOperand(0)),
      TyValueType::make(*itpi.getDestTy())));
}

std::shared_ptr<TyPtrToIntInst>
TyPtrToIntInst::make(const llvm::PtrToIntInst &ptii) {
  return std::shared_ptr<TyPtrToIntInst>(new TyPtrToIntInst(
      TyValueType::make(*ptii.getSrcTy()), TyValue::make(*ptii.getOperand(0)),
      TyValueType::make(*ptii.getDestTy())));
}

std::shared_ptr<TyGetElementPtrInst>
TyGetElementPtrInst::make(const llvm::GetElementPtrInst &gepi) {
  std::vector<std::pair<std::shared_ptr<TySize>, std::shared_ptr<TyValue>>>
      indexes;
  for (llvm::User::const_op_iterator i = gepi.idx_begin(); i != gepi.idx_end();
       i++) {
    const llvm::Value *v = i->get();
    const llvm::Type *ty = v->getType();
    assert(ty->isIntegerTy());
    indexes.push_back(std::make_pair(ConsSize::make(ty->getIntegerBitWidth()),
                                     TyValue::make(*v)));
  }
  return std::shared_ptr<TyGetElementPtrInst>(new TyGetElementPtrInst(
      TyValueType::make(*gepi.getSourceElementType()),
      TyValueType::make(*gepi.getType()->getElementType()),
      TyValue::make(*gepi.getPointerOperand()), indexes, gepi.isInBounds()));
}

std::shared_ptr<TyBinaryOperator>
TyBinaryOperator::make(const llvm::BinaryOperator &bopinst) {
  llvmberry::TyBop bop = llvmberry::getBop(bopinst.getOpcode());
  return std::shared_ptr<TyBinaryOperator>(
      new TyBinaryOperator(bop, TyValueType::make(*bopinst.getType()),
                           TyValue::make(*bopinst.getOperand(0)),
                           TyValue::make(*bopinst.getOperand(1))));
}

std::shared_ptr<TyFloatBinaryOperator>
TyFloatBinaryOperator::make(const llvm::BinaryOperator &bopinst) {
  llvmberry::TyFbop bop = llvmberry::getFbop(bopinst.getOpcode());
  return std::shared_ptr<TyFloatBinaryOperator>(
      new TyFloatBinaryOperator(bop, TyValueType::make(*bopinst.getType()),
                                TyValue::make(*bopinst.getOperand(0)),
                                TyValue::make(*bopinst.getOperand(1))));
}

std::shared_ptr<TyICmpInst> TyICmpInst::make(const llvm::ICmpInst &icmpInst) {
  llvmberry::TyIcmpPred predicate =
      llvmberry::getIcmpPred(icmpInst.getPredicate());
  return std::shared_ptr<TyICmpInst>(new TyICmpInst(
      predicate, TyValueType::make(*icmpInst.getOperand(0)->getType()),
      TyValue::make(*icmpInst.getOperand(0)),
      TyValue::make(*icmpInst.getOperand(1))));
}

std::shared_ptr<TyFCmpInst> TyFCmpInst::make(const llvm::FCmpInst &fcmpInst) {
  llvmberry::TyFcmpPred predicate =
      llvmberry::getFcmpPred(fcmpInst.getPredicate());
  return std::shared_ptr<TyFCmpInst>(new TyFCmpInst(
      predicate, TyValueType::make(*fcmpInst.getOperand(0)->getType()),
      TyValue::make(*fcmpInst.getOperand(0)),
      TyValue::make(*fcmpInst.getOperand(1))));
}

std::shared_ptr<TyLoadInst> TyLoadInst::make(const llvm::AllocaInst &ai) {
  return std::shared_ptr<TyLoadInst>(new TyLoadInst(
      TyValueType::make(*ai.getType()),
      TyValueType::make(*ai.getAllocatedType()), TyValue::make(ai),
      1/*ai.getAlignment()*/));
}

std::shared_ptr<TyLoadInst> TyLoadInst::make(const llvm::LoadInst &li) {
  return std::shared_ptr<TyLoadInst>(new TyLoadInst(
      TyValueType::make(*li.getPointerOperand()->getType()),
      TyValueType::make(*li.getType()), TyValue::make(*li.getPointerOperand()),
      1/*li.getAlignment()*/));
}

std::shared_ptr<TyLoadInst> TyLoadInst::make(const llvm::StoreInst &si) {
  return std::shared_ptr<TyLoadInst>(
      new TyLoadInst(TyValueType::make(*si.getOperand(1)->getType()),
                     TyValueType::make(*si.getOperand(0)->getType()),
                     TyValue::make(*si.getOperand(1)), 
                     1/*si.getAlignment()*/));
}

std::shared_ptr<TyLoadInst> TyLoadInst::makeAlignOne(llvm::Instruction *i) {
  if (llvm::LoadInst* li = llvm::dyn_cast<llvm::LoadInst>(i)) {
    return std::shared_ptr<TyLoadInst>(new TyLoadInst(
        TyValueType::make(*li->getPointerOperand()->getType()),
        TyValueType::make(*li->getType()), TyValue::make(*li->getPointerOperand()),
        1));
  } else if (llvm::StoreInst* si = llvm::dyn_cast<llvm::StoreInst>(i)) {
    return std::shared_ptr<TyLoadInst>(
        new TyLoadInst(TyValueType::make(*si->getOperand(1)->getType()),
                       TyValueType::make(*si->getOperand(0)->getType()),
                       TyValue::make(*si->getOperand(1)), 1));
  } else if (llvm::AllocaInst* ai = llvm::dyn_cast<llvm::AllocaInst>(i)) {
    return std::shared_ptr<TyLoadInst>(new TyLoadInst(
        TyValueType::make(*ai->getType()),
        TyValueType::make(*ai->getAllocatedType()), TyValue::make(*ai),
        1));
  } else {
    assert("Instruction should be Load, Store, or Alloca" && false);
    return nullptr;
  }
}

std::shared_ptr<TySelectInst>
TySelectInst::make(const llvm::SelectInst &si) {
  return std::shared_ptr<TySelectInst>(new TySelectInst(
      TyValue::make(*si.getCondition()),
      TyValueType::make(*si.getType()),
      TyValue::make(*si.getTrueValue()), 
      TyValue::make(*si.getFalseValue())));
}

std::shared_ptr<TyFpextInst>
TyFpextInst::make(const llvm::FPExtInst &fpei) {
  return std::shared_ptr<TyFpextInst>(new TyFpextInst(
      TyValueType::make(*fpei.getSrcTy()), TyValue::make(*fpei.getOperand(0)),
      TyValueType::make(*fpei.getDestTy())));
}

std::shared_ptr<TyFptruncInst>
TyFptruncInst::make(const llvm::FPTruncInst &fpti) {
  return std::shared_ptr<TyFptruncInst>(new TyFptruncInst(
      TyValueType::make(*fpti.getSrcTy()), TyValue::make(*fpti.getOperand(0)),
      TyValueType::make(*fpti.getDestTy())));
}

std::shared_ptr<TyZextInst>
TyZextInst::make(const llvm::ZExtInst &zei) {
  return std::shared_ptr<TyZextInst>(new TyZextInst(
      TyValueType::make(*zei.getSrcTy()), TyValue::make(*zei.getOperand(0)),
      TyValueType::make(*zei.getDestTy())));
}

std::shared_ptr<TySextInst>
TySextInst::make(const llvm::SExtInst &sei) {
  return std::shared_ptr<TySextInst>(new TySextInst(
      TyValueType::make(*sei.getSrcTy()), TyValue::make(*sei.getOperand(0)),
      TyValueType::make(*sei.getDestTy())));
}

std::shared_ptr<TyTruncInst>
TyTruncInst::make(const llvm::TruncInst &ti) {
  return std::shared_ptr<TyTruncInst>(new TyTruncInst(
      TyValueType::make(*ti.getSrcTy()), TyValue::make(*ti.getOperand(0)),
      TyValueType::make(*ti.getDestTy())));
}

std::shared_ptr<TyFptosiInst> TyFptosiInst::make(const llvm::FPToSIInst &ftsi) {
  return std::shared_ptr<TyFptosiInst>(new TyFptosiInst(
      TyValueType::make(*ftsi.getSrcTy()), TyValue::make(*ftsi.getOperand(0)),
      TyValueType::make(*ftsi.getDestTy())));
}

std::shared_ptr<TySitofpInst>
TySitofpInst::make(const llvm::SIToFPInst &stfi) {
  return std::shared_ptr<TySitofpInst>(new TySitofpInst(
      TyValueType::make(*stfi.getSrcTy()), TyValue::make(*stfi.getOperand(0)),
      TyValueType::make(*stfi.getDestTy())));
}

std::shared_ptr<TyUitofpInst>
TyUitofpInst::make(const llvm::UIToFPInst &utfi) {
  return std::shared_ptr<TyUitofpInst>(new TyUitofpInst(
      TyValueType::make(*utfi.getSrcTy()), TyValue::make(*utfi.getOperand(0)),
      TyValueType::make(*utfi.getDestTy())));
}

std::shared_ptr<TyExtractValueInst>
TyExtractValueInst::make(const llvm::ExtractValueInst &evi) {
  std::vector<unsigned> indexes;
  for (auto itr = evi.idx_begin(); itr != evi.idx_end(); itr++) {
    indexes.push_back(*itr);
  }
  return std::shared_ptr<TyExtractValueInst>(new TyExtractValueInst(
      TyValueType::make(*evi.getAggregateOperand()->getType()),
      TyValue::make(*evi.getAggregateOperand()),
      indexes,
      TyValueType::make(*evi.getType())));
}

std::shared_ptr<TyInsertValueInst>
TyInsertValueInst::make(const llvm::InsertValueInst &ivi) {
  std::vector<unsigned> indexes;
  for (auto itr = ivi.idx_begin(); itr != ivi.idx_end(); itr++) {
    indexes.push_back(*itr);
  }
  return std::shared_ptr<TyInsertValueInst>(new TyInsertValueInst(
      TyValueType::make(*ivi.getAggregateOperand()->getType()),
      TyValue::make(*ivi.getAggregateOperand()),
      TyValueType::make(*ivi.getInsertedValueOperand()->getType()),
      TyValue::make(*ivi.getInsertedValueOperand()),
      indexes));
}

// instruction constructor classes

ConsBinaryOp::ConsBinaryOp(std::shared_ptr<TyBinaryOperator> _binary_operator)
    : binary_operator(_binary_operator) {}
std::shared_ptr<TyInstruction>
ConsBinaryOp::make(TyBop _opcode, std::shared_ptr<TyValueType> _operandtype,
                   std::shared_ptr<TyValue> _operand1,
                   std::shared_ptr<TyValue> _operand2) {
  std::shared_ptr<TyBinaryOperator> _val(
      new TyBinaryOperator(_opcode, _operandtype,
                           _operand1, _operand2));
  return std::shared_ptr<TyInstruction>(new ConsBinaryOp(_val));
}
std::shared_ptr<TyInstruction>
ConsBinaryOp::make(const llvm::BinaryOperator &bop) {
  return std::shared_ptr<TyInstruction>(
      new ConsBinaryOp(TyBinaryOperator::make(bop)));
}
void ConsBinaryOp::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("BinaryOp");
  archive(CEREAL_NVP(binary_operator));
}
std::shared_ptr<TyValue> ConsBinaryOp::get_op(int i) {
  return binary_operator->get_op(i);
}
void ConsBinaryOp::replace_op(int i, std::shared_ptr<TyValue> val) {
  binary_operator->replace_op(i, val);
}

ConsFloatBinaryOp::ConsFloatBinaryOp(
    std::shared_ptr<TyFloatBinaryOperator> _binary_operator)
    : binary_operator(_binary_operator) {}
std::shared_ptr<TyInstruction> ConsFloatBinaryOp::make(
    TyFbop _opcode, std::shared_ptr<TyValueType> _operandtype,
    std::shared_ptr<TyValue> _operand1, std::shared_ptr<TyValue> _operand2) {
  std::shared_ptr<TyFloatBinaryOperator> _val(
      new TyFloatBinaryOperator(_opcode, _operandtype,
                                _operand1, _operand2));
  return std::shared_ptr<TyInstruction>(new ConsFloatBinaryOp(_val));
}
std::shared_ptr<TyInstruction>
ConsFloatBinaryOp::make(const llvm::BinaryOperator &bop) {
  return std::shared_ptr<TyInstruction>(
      new ConsFloatBinaryOp(TyFloatBinaryOperator::make(bop)));
}
void ConsFloatBinaryOp::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("FloatBinaryOp");
  archive(CEREAL_NVP(binary_operator));
}
std::shared_ptr<TyValue> ConsFloatBinaryOp::get_op(int i) {
  return binary_operator->get_op(i);
}
void ConsFloatBinaryOp::replace_op(int i, std::shared_ptr<TyValue> val) {
  binary_operator->replace_op(i, val);
}

ConsICmpInst::ConsICmpInst(std::shared_ptr<TyICmpInst> _icmp_inst)
    : icmp_inst(_icmp_inst) {}
std::shared_ptr<TyInstruction> ConsICmpInst::make(
    TyIcmpPred _predicate, std::shared_ptr<TyValueType> _operandtype,
    std::shared_ptr<TyValue> _operand1, std::shared_ptr<TyValue> _operand2) {
  std::shared_ptr<TyICmpInst> _val(
      new TyICmpInst(_predicate, _operandtype, _operand1, _operand2));
  return std::shared_ptr<TyInstruction>(new ConsICmpInst(_val));
}
std::shared_ptr<TyInstruction>
ConsICmpInst::make(const llvm::ICmpInst &iCmpInst) {
  return std::shared_ptr<TyInstruction>(
      new ConsICmpInst(TyICmpInst::make(iCmpInst)));
}
void ConsICmpInst::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("ICmpInst");
  archive(CEREAL_NVP(icmp_inst));
}
std::shared_ptr<TyValue> ConsICmpInst::get_op(int i) {
  return icmp_inst->get_op(i);

}
void ConsICmpInst::replace_op(int i, std::shared_ptr<TyValue> val) {
  icmp_inst->replace_op(i, val);
}

ConsFCmpInst::ConsFCmpInst(std::shared_ptr<TyFCmpInst> _fcmp_inst)
    : fcmp_inst(_fcmp_inst) {}
std::shared_ptr<TyInstruction> ConsFCmpInst::make(
    TyFcmpPred _predicate, std::shared_ptr<TyValueType> _operandtype,
    std::shared_ptr<TyValue> _operand1, std::shared_ptr<TyValue> _operand2) {
  std::shared_ptr<TyFCmpInst> _val(
      new TyFCmpInst(_predicate, _operandtype, _operand1,_operand2));
  return std::shared_ptr<TyInstruction>(new ConsFCmpInst(_val));
}
std::shared_ptr<TyInstruction>
ConsFCmpInst::make(const llvm::FCmpInst &fCmpInst) {
  return std::shared_ptr<TyInstruction>(
      new ConsFCmpInst(TyFCmpInst::make(fCmpInst)));
}
void ConsFCmpInst::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("FCmpInst");
  archive(CEREAL_NVP(fcmp_inst));
}
std::shared_ptr<TyValue> ConsFCmpInst::get_op(int i) {
  return fcmp_inst->get_op(i);
}
void ConsFCmpInst::replace_op(int i, std::shared_ptr<TyValue> val) {
  fcmp_inst->replace_op(i, val);
}

ConsSelectInst::ConsSelectInst(std::shared_ptr<TySelectInst> _select_inst) : select_inst(_select_inst){
}
std::shared_ptr<TyInstruction> ConsSelectInst::make(std::shared_ptr<TyValue> _cond, std::shared_ptr<TyValueType> _valty, std::shared_ptr<TyValue> _trueval, std::shared_ptr<TyValue> _falseval){
  std::shared_ptr<TySelectInst> _val(new TySelectInst(_cond, _valty, _trueval, _falseval));
  return std::shared_ptr<TyInstruction>(new ConsSelectInst(_val));
}
std::shared_ptr<TyInstruction>
ConsSelectInst::make(const llvm::SelectInst &si) {
  return std::shared_ptr<TyInstruction>(
      new ConsSelectInst(TySelectInst::make(si)));
}
void ConsSelectInst::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SelectInst");
  archive(CEREAL_NVP(select_inst));
}
std::shared_ptr<TyValue> ConsSelectInst::get_op(int i) {
  return select_inst->get_op(i);
}
void ConsSelectInst::replace_op(int i, std::shared_ptr<TyValue> val) {
  select_inst->replace_op(i, val);
}

ConsLoadInst::ConsLoadInst(std::shared_ptr<TyLoadInst> _load_inst)
    : load_inst(_load_inst) {}
std::shared_ptr<TyInstruction>
ConsLoadInst::make(std::shared_ptr<TyValueType> _pointertype,
                   std::shared_ptr<TyValueType> _valtype,
                   std::shared_ptr<TyValue> _ptrvalue, int _align) {
  std::shared_ptr<TyLoadInst> _val(
      new TyLoadInst(_pointertype, _valtype,
                     _ptrvalue, _align));
  return std::shared_ptr<TyInstruction>(new ConsLoadInst(_val));
}
std::shared_ptr<TyInstruction> ConsLoadInst::make(const llvm::LoadInst &li) {
  return std::shared_ptr<TyInstruction>(
      new ConsLoadInst(TyLoadInst::make(li)));
}
void ConsLoadInst::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("LoadInst");
  archive(CEREAL_NVP(load_inst));
}
std::shared_ptr<TyValue> ConsLoadInst::get_op(int i) {
  return load_inst->get_op(i);
}
void ConsLoadInst::replace_op(int i, std::shared_ptr<TyValue> val) {
  load_inst->replace_op(i, val);
}

std::shared_ptr<TyLoadInst> ConsLoadInst::getTyLoadInst() {
  return load_inst;
}

ConsBitCastInst::ConsBitCastInst(std::shared_ptr<TyBitCastInst> _bit_cast_inst)
    : bit_cast_inst(_bit_cast_inst) {}
std::shared_ptr<TyInstruction>
ConsBitCastInst::make(std::shared_ptr<TyValueType> _fromty,
                      std::shared_ptr<TyValue> _v,
                      std::shared_ptr<TyValueType> _toty) {
  std::shared_ptr<TyBitCastInst> _val(
      new TyBitCastInst(_fromty, _v, _toty));
  return std::shared_ptr<TyInstruction>(new ConsBitCastInst(_val));
}
std::shared_ptr<TyInstruction> ConsBitCastInst::make(const llvm::BitCastInst &li) {
  return std::shared_ptr<TyInstruction>(
      new ConsBitCastInst(TyBitCastInst::make(li)));
}
void ConsBitCastInst::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("BitCastInst");
  archive(CEREAL_NVP(bit_cast_inst));
}
std::shared_ptr<TyValue> ConsBitCastInst::get_op(int i) {
  return bit_cast_inst->get_op(i);
}
void ConsBitCastInst::replace_op(int i, std::shared_ptr<TyValue> val) {
  bit_cast_inst->replace_op(i, val);
}

ConsIntToPtrInst::ConsIntToPtrInst(
    std::shared_ptr<TyIntToPtrInst> _int_to_ptr_inst)
    : int_to_ptr_inst(_int_to_ptr_inst) {}
std::shared_ptr<TyInstruction>
ConsIntToPtrInst::make(std::shared_ptr<TyValueType> _fromty,
                       std::shared_ptr<TyValue> _v,
                       std::shared_ptr<TyValueType> _toty) {
  std::shared_ptr<TyIntToPtrInst> _val(new TyIntToPtrInst(_fromty, _v, _toty));
  return std::shared_ptr<TyInstruction>(new ConsIntToPtrInst(_val));
}
std::shared_ptr<TyInstruction> ConsIntToPtrInst::make(const llvm::IntToPtrInst &li) {
  return std::shared_ptr<TyInstruction>(
      new ConsIntToPtrInst(TyIntToPtrInst::make(li)));
}
void ConsIntToPtrInst::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("IntToPtrInst");
  archive(CEREAL_NVP(int_to_ptr_inst));
}
std::shared_ptr<TyValue> ConsIntToPtrInst::get_op(int i) {
  return int_to_ptr_inst->get_op(i);
}
void ConsIntToPtrInst::replace_op(int i, std::shared_ptr<TyValue> val) {
  int_to_ptr_inst->replace_op(i, val);
}

ConsPtrToIntInst::ConsPtrToIntInst(
    std::shared_ptr<TyPtrToIntInst> _ptr_to_int_inst)
    : ptr_to_int_inst(_ptr_to_int_inst) {}
std::shared_ptr<TyInstruction>
ConsPtrToIntInst::make(std::shared_ptr<TyValueType> _fromty,
                       std::shared_ptr<TyValue> _v,
                       std::shared_ptr<TyValueType> _toty) {
  std::shared_ptr<TyPtrToIntInst> _val(new TyPtrToIntInst(_fromty, _v, _toty));
  return std::shared_ptr<TyInstruction>(new ConsPtrToIntInst(_val));
}
std::shared_ptr<TyInstruction> ConsPtrToIntInst::make(const llvm::PtrToIntInst &li) {
  return std::shared_ptr<TyInstruction>(
      new ConsPtrToIntInst(TyPtrToIntInst::make(li)));
}
void ConsPtrToIntInst::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("PtrToIntInst");
  archive(CEREAL_NVP(ptr_to_int_inst));
}
std::shared_ptr<TyValue> ConsPtrToIntInst::get_op(int i) {
  return ptr_to_int_inst->get_op(i);
}
void ConsPtrToIntInst::replace_op(int i, std::shared_ptr<TyValue> val) {
  ptr_to_int_inst->replace_op(i, val);
}

ConsGetElementPtrInst::ConsGetElementPtrInst(
    std::shared_ptr<TyGetElementPtrInst> _get_element_ptr_inst)
    : get_element_ptr_inst(_get_element_ptr_inst) {}
std::shared_ptr<TyInstruction> ConsGetElementPtrInst::make(
    std::shared_ptr<TyValueType> _ty, std::shared_ptr<TyValueType> _ptrty,
    std::shared_ptr<TyValue> _ptr,
    std::vector<std::pair<std::shared_ptr<TySize>, std::shared_ptr<TyValue>>>
        &_indexes,
    bool _is_inbounds) {
  std::shared_ptr<TyGetElementPtrInst> _val(
      new TyGetElementPtrInst(_ty, _ptrty,
                              _ptr, _indexes, _is_inbounds));
  return std::shared_ptr<TyInstruction>(
      new ConsGetElementPtrInst(_val));
}
std::shared_ptr<TyInstruction> ConsGetElementPtrInst::make(const llvm::GetElementPtrInst &li) {
  return std::shared_ptr<TyInstruction>(
      new ConsGetElementPtrInst(TyGetElementPtrInst::make(li)));
}
void ConsGetElementPtrInst::serialize(
    cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("GetElementPtrInst");
  archive(CEREAL_NVP(get_element_ptr_inst));
}
std::shared_ptr<TyValue> ConsGetElementPtrInst::get_op(int i) {
  return get_element_ptr_inst->get_op(i);
}
void ConsGetElementPtrInst::replace_op(int i, std::shared_ptr<TyValue> val) {
  get_element_ptr_inst->replace_op(i, val);
}

ConsFpextInst::ConsFpextInst(std::shared_ptr<TyFpextInst> _fpext_inst) : fpext_inst(_fpext_inst){
}
std::shared_ptr<TyInstruction> ConsFpextInst::make(std::shared_ptr<TyValueType> _fromty, std::shared_ptr<TyValue> _v, std::shared_ptr<TyValueType> _toty){
  std::shared_ptr<TyFpextInst> _val(new TyFpextInst(_fromty, _v, _toty));
  return std::shared_ptr<TyInstruction>(new ConsFpextInst(_val));
}
std::shared_ptr<TyInstruction> ConsFpextInst::make(const llvm::FPExtInst &li) {
  return std::shared_ptr<TyInstruction>(
      new ConsFpextInst(TyFpextInst::make(li)));
}
void ConsFpextInst::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("FpextInst");
  archive(CEREAL_NVP(fpext_inst));
}
std::shared_ptr<TyValue> ConsFpextInst::get_op(int i) {
  return fpext_inst->get_op(i);
}
void ConsFpextInst::replace_op(int i, std::shared_ptr<TyValue> val) {
  fpext_inst->replace_op(i, val);
}

ConsFptruncInst::ConsFptruncInst(std::shared_ptr<TyFptruncInst> _fptrunc_inst) : fptrunc_inst(_fptrunc_inst){
}
std::shared_ptr<TyInstruction> ConsFptruncInst::make(std::shared_ptr<TyValueType> _fromty, std::shared_ptr<TyValue> _v, std::shared_ptr<TyValueType> _toty){
  std::shared_ptr<TyFptruncInst> _val(new TyFptruncInst(_fromty, _v, _toty));
  return std::shared_ptr<TyInstruction>(new ConsFptruncInst(_val));
}
std::shared_ptr<TyInstruction> ConsFptruncInst::make(const llvm::FPTruncInst &li) {
  return std::shared_ptr<TyInstruction>(
      new ConsFptruncInst(TyFptruncInst::make(li)));
}
void ConsFptruncInst::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("FptruncInst");
  archive(CEREAL_NVP(fptrunc_inst));
}
std::shared_ptr<TyValue> ConsFptruncInst::get_op(int i) {
  return fptrunc_inst->get_op(i);
}
void ConsFptruncInst::replace_op(int i, std::shared_ptr<TyValue> val) {
  fptrunc_inst->replace_op(i, val);
}

ConsSextInst::ConsSextInst(std::shared_ptr<TySextInst> _sext_inst) : sext_inst(_sext_inst){
}
std::shared_ptr<TyInstruction> ConsSextInst::make(std::shared_ptr<TyValueType> _fromty, std::shared_ptr<TyValue> _v, std::shared_ptr<TyValueType> _toty){
  std::shared_ptr<TySextInst> _val(new TySextInst(_fromty, _v, _toty));
  return std::shared_ptr<TyInstruction>(new ConsSextInst(_val));
}
std::shared_ptr<TyInstruction> ConsSextInst::make(const llvm::SExtInst &si) {
  return std::shared_ptr<TyInstruction>(
      new ConsSextInst(TySextInst::make(si)));
}
void ConsSextInst::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SextInst");
  archive(CEREAL_NVP(sext_inst));
}
std::shared_ptr<TyValue> ConsSextInst::get_op(int i) {
  return sext_inst->get_op(i);
}
void ConsSextInst::replace_op(int i, std::shared_ptr<TyValue> val) {
  sext_inst->replace_op(i, val);
}

ConsZextInst::ConsZextInst(std::shared_ptr<TyZextInst> _zext_inst) : zext_inst(_zext_inst){
}
std::shared_ptr<TyInstruction> ConsZextInst::make(std::shared_ptr<TyValueType> _fromty, std::shared_ptr<TyValue> _v, std::shared_ptr<TyValueType> _toty){
  std::shared_ptr<TyZextInst> _val(new TyZextInst(_fromty, _v, _toty));
  return std::shared_ptr<TyInstruction>(new ConsZextInst(_val));
}
std::shared_ptr<TyInstruction> ConsZextInst::make(const llvm::ZExtInst &zi) {
  return std::shared_ptr<TyInstruction>(
      new ConsZextInst(TyZextInst::make(zi)));
}
void ConsZextInst::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("ZextInst");
  archive(CEREAL_NVP(zext_inst));
}
std::shared_ptr<TyValue> ConsZextInst::get_op(int i) {
  return zext_inst->get_op(i);
}
void ConsZextInst::replace_op(int i, std::shared_ptr<TyValue> val) {
  zext_inst->replace_op(i, val);
}

ConsTruncInst::ConsTruncInst(std::shared_ptr<TyTruncInst> _trunc_inst) : trunc_inst(_trunc_inst){
}
std::shared_ptr<TyInstruction> ConsTruncInst::make(std::shared_ptr<TyValueType> _fromty, std::shared_ptr<TyValue> _v, std::shared_ptr<TyValueType> _toty){
  std::shared_ptr<TyTruncInst> _val(new TyTruncInst(_fromty, _v, _toty));
  return std::shared_ptr<TyInstruction>(new ConsTruncInst(_val));
}
std::shared_ptr<TyInstruction> ConsTruncInst::make(const llvm::TruncInst &ti) {
  return std::shared_ptr<TyInstruction>(
      new ConsTruncInst(TyTruncInst::make(ti)));
}
void ConsTruncInst::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("TruncInst");
  archive(CEREAL_NVP(trunc_inst));
}
std::shared_ptr<TyValue> ConsTruncInst::get_op(int i) {
  return trunc_inst->get_op(i);
}
void ConsTruncInst::replace_op(int i, std::shared_ptr<TyValue> val) {
  trunc_inst->replace_op(i, val);
}

ConsFptosiInst::ConsFptosiInst(std::shared_ptr<TyFptosiInst> _fptosi_inst)
    : fptosi_inst(_fptosi_inst) {}
std::shared_ptr<TyInstruction>
ConsFptosiInst::make(std::shared_ptr<TyValueType> _fromty,
                     std::shared_ptr<TyValue> _v,
                     std::shared_ptr<TyValueType> _toty) {
  std::shared_ptr<TyFptosiInst> _val(new TyFptosiInst(_fromty, _v, _toty));
  return std::shared_ptr<TyInstruction>(new ConsFptosiInst(_val));
}
std::shared_ptr<TyInstruction>
ConsFptosiInst::make(const llvm::FPToSIInst &ftsi) {
  return std::shared_ptr<TyInstruction>(
      new ConsFptosiInst(TyFptosiInst::make(ftsi)));
}
void ConsFptosiInst::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("FptosiInst");
  archive(CEREAL_NVP(fptosi_inst));
}
std::shared_ptr<TyValue> ConsFptosiInst::get_op(int i) {
  return fptosi_inst->get_op(i);
}
void ConsFptosiInst::replace_op(int i, std::shared_ptr<TyValue> val) {
  fptosi_inst->replace_op(i, val);
}

ConsSitofpInst::ConsSitofpInst(std::shared_ptr<TySitofpInst> _sitofp_inst) : sitofp_inst(_sitofp_inst){
}
std::shared_ptr<TyInstruction> ConsSitofpInst::make(std::shared_ptr<TyValueType> _fromty, std::shared_ptr<TyValue> _v, std::shared_ptr<TyValueType> _toty){
  std::shared_ptr<TySitofpInst> _val(new TySitofpInst(_fromty, _v, _toty));
  return std::shared_ptr<TyInstruction>(new ConsSitofpInst(_val));
}
std::shared_ptr<TyInstruction> ConsSitofpInst::make(const llvm::SIToFPInst &stfi) {
  return std::shared_ptr<TyInstruction>(
      new ConsSitofpInst(TySitofpInst::make(stfi)));
}
void ConsSitofpInst::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("SitofpInst");
  archive(CEREAL_NVP(sitofp_inst));
}
std::shared_ptr<TyValue> ConsSitofpInst::get_op(int i) {
  return sitofp_inst->get_op(i);
}
void ConsSitofpInst::replace_op(int i, std::shared_ptr<TyValue> val) {
  sitofp_inst->replace_op(i, val);
}

ConsUitofpInst::ConsUitofpInst(std::shared_ptr<TyUitofpInst> _uitofp_inst) : uitofp_inst(_uitofp_inst){
}
std::shared_ptr<TyInstruction> ConsUitofpInst::make(std::shared_ptr<TyValueType> _fromty, std::shared_ptr<TyValue> _v, std::shared_ptr<TyValueType> _toty){
  std::shared_ptr<TyUitofpInst> _val(new TyUitofpInst(_fromty, _v, _toty));
  return std::shared_ptr<TyInstruction>(new ConsUitofpInst(_val));
}
std::shared_ptr<TyInstruction> ConsUitofpInst::make(const llvm::UIToFPInst &utfi) {
  return std::shared_ptr<TyInstruction>(
      new ConsUitofpInst(TyUitofpInst::make(utfi)));
}
void ConsUitofpInst::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("UitofpInst");
  archive(CEREAL_NVP(uitofp_inst));
}
std::shared_ptr<TyValue> ConsUitofpInst::get_op(int i) {
  return uitofp_inst->get_op(i);
}
void ConsUitofpInst::replace_op(int i, std::shared_ptr<TyValue> val) {
  uitofp_inst->replace_op(i, val);
}

ConsInsertValueInst::ConsInsertValueInst(std::shared_ptr<TyInsertValueInst> _insert_value_inst) : insert_value_inst(_insert_value_inst){
}
std::shared_ptr<TyInstruction> ConsInsertValueInst::make(std::shared_ptr<TyValueType> _aggrty, std::shared_ptr<TyValue> _aggrv, std::shared_ptr<TyValueType> _argty, std::shared_ptr<TyValue> _argv, std::vector<unsigned> _idx){
  std::shared_ptr<TyInsertValueInst> _val(new TyInsertValueInst(_aggrty, _aggrv, _argty, _argv, _idx));
  return std::shared_ptr<TyInstruction>(new ConsInsertValueInst(_val));
}
void ConsInsertValueInst::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("InsertValueInst");
  archive(CEREAL_NVP(insert_value_inst));
}
std::shared_ptr<TyValue> ConsInsertValueInst::get_op(int i) {
  return insert_value_inst->get_op(i);
}
void ConsInsertValueInst::replace_op(int i, std::shared_ptr<TyValue> val) {
  insert_value_inst->replace_op(i, val);
}

ConsExtractValueInst::ConsExtractValueInst(std::shared_ptr<TyExtractValueInst> _extract_value_inst) : extract_value_inst(_extract_value_inst){
}
std::shared_ptr<TyInstruction> ConsExtractValueInst::make(std::shared_ptr<TyValueType> _aggrty, std::shared_ptr<TyValue> _aggrv, std::vector<unsigned> _idx, std::shared_ptr<TyValueType> _retty){
  std::shared_ptr<TyExtractValueInst> _val(new TyExtractValueInst(_aggrty, _aggrv, _idx, _retty));
  return std::shared_ptr<TyInstruction>(new ConsExtractValueInst(_val));
}
void ConsExtractValueInst::serialize(cereal::JSONOutputArchive& archive) const{
  archive.makeArray();
  archive.writeName();
  archive.saveValue("ExtractValueInst");
  archive(CEREAL_NVP(extract_value_inst));
}
std::shared_ptr<TyValue> ConsExtractValueInst::get_op(int i) {
  return extract_value_inst->get_op(i);
}
void ConsExtractValueInst::replace_op(int i, std::shared_ptr<TyValue> val) {
  extract_value_inst->replace_op(i, val);
}

// instruction type classes

TyBinaryOperator::TyBinaryOperator(TyBop _opcode,
                                   std::shared_ptr<TyValueType> _operandtype,
                                   std::shared_ptr<TyValue> _operand1,
                                   std::shared_ptr<TyValue> _operand2)
    : opcode(_opcode), operandtype(_operandtype),
      operand1(_operand1), operand2(_operand2) {}
void TyBinaryOperator::serialize(cereal::JSONOutputArchive &archive) const {
  archive(cereal::make_nvp("opcode", toString(opcode)));
  archive(CEREAL_NVP(operandtype));
  archive(CEREAL_NVP(operand1));
  archive(CEREAL_NVP(operand2));
}

TyFloatBinaryOperator::TyFloatBinaryOperator(
    TyFbop _opcode, std::shared_ptr<TyValueType> _operandtype,
    std::shared_ptr<TyValue> _operand1, std::shared_ptr<TyValue> _operand2)
    : opcode(_opcode), operandtype(_operandtype),
      operand1(_operand1), operand2(_operand2) {}
void TyFloatBinaryOperator::serialize(
    cereal::JSONOutputArchive &archive) const {
  archive(cereal::make_nvp("opcode", toString(opcode)));
  archive(CEREAL_NVP(operandtype));
  archive(CEREAL_NVP(operand1));
  archive(CEREAL_NVP(operand2));
}

TyICmpInst::TyICmpInst(TyIcmpPred _predicate,
                       std::shared_ptr<TyValueType> _operandtype,
                       std::shared_ptr<TyValue> _operand1,
                       std::shared_ptr<TyValue> _operand2)
    : predicate(_predicate), operandtype(_operandtype),
      operand1(_operand1), operand2(_operand2) {}
void TyICmpInst::serialize(cereal::JSONOutputArchive &archive) const {
  archive(cereal::make_nvp("predicate", toString(predicate)));
  archive(CEREAL_NVP(operandtype));
  archive(CEREAL_NVP(operand1));
  archive(CEREAL_NVP(operand2));
}

TyFCmpInst::TyFCmpInst(TyFcmpPred _predicate,
                       std::shared_ptr<TyValueType> _operandtype,
                       std::shared_ptr<TyValue> _operand1,
                       std::shared_ptr<TyValue> _operand2)
    : predicate(_predicate), operandtype(_operandtype),
      operand1(_operand1), operand2(_operand2) {}
void TyFCmpInst::serialize(cereal::JSONOutputArchive &archive) const {
  archive(cereal::make_nvp("predicate", toString(predicate)));
  archive(CEREAL_NVP(operandtype));
  archive(CEREAL_NVP(operand1));
  archive(CEREAL_NVP(operand2));
}

TyLoadInst::TyLoadInst(std::shared_ptr<TyValueType> _pointertype,
                       std::shared_ptr<TyValueType> _valtype,
                       std::shared_ptr<TyValue> _ptrvalue, int _align)
    : pointertype(_pointertype), valtype(_valtype),
      ptrvalue(_ptrvalue), align(_align) {}

void TyLoadInst::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(pointertype));
  archive(CEREAL_NVP(valtype));
  archive(CEREAL_NVP(ptrvalue));
  archive(CEREAL_NVP(align));
}

std::shared_ptr<TyValue> TyLoadInst::getPtrValue() {
  return ptrvalue;
}

TySelectInst::TySelectInst(std::shared_ptr<TyValue> _cond, std::shared_ptr<TyValueType> _valty, std::shared_ptr<TyValue> _trueval, std::shared_ptr<TyValue> _falseval) : cond(_cond), valty(_valty), trueval(_trueval), falseval(_falseval){
}
void TySelectInst::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(cond), CEREAL_NVP(valty), CEREAL_NVP(trueval), CEREAL_NVP(falseval));
}

TyBitCastInst::TyBitCastInst(std::shared_ptr<TyValueType> _fromty,
                             std::shared_ptr<TyValue> _v,
                             std::shared_ptr<TyValueType> _toty)
    : fromty(_fromty), v(_v), toty(_toty) {}
void TyBitCastInst::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(fromty));
  archive(CEREAL_NVP(v));
  archive(CEREAL_NVP(toty));
}

TyPtrToIntInst::TyPtrToIntInst(std::shared_ptr<TyValueType> _fromty,
                               std::shared_ptr<TyValue> _v,
                               std::shared_ptr<TyValueType> _toty)
    : fromty(_fromty), v(_v), toty(_toty) {}
void TyPtrToIntInst::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(fromty));
  archive(CEREAL_NVP(v));
  archive(CEREAL_NVP(toty));
}

TyIntToPtrInst::TyIntToPtrInst(std::shared_ptr<TyValueType> _fromty,
                               std::shared_ptr<TyValue> _v,
                               std::shared_ptr<TyValueType> _toty)
    : fromty(_fromty), v(_v), toty(_toty) {}
void TyIntToPtrInst::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(fromty));
  archive(CEREAL_NVP(v));
  archive(CEREAL_NVP(toty));
}

TyGetElementPtrInst::TyGetElementPtrInst(
    std::shared_ptr<TyValueType> _ty, std::shared_ptr<TyValueType> _retty,
    std::shared_ptr<TyValue> _ptr,
    std::vector<std::pair<std::shared_ptr<TySize>, std::shared_ptr<TyValue>>>
        &_indexes,
    bool _is_inbounds)
    : ty(_ty), retty(_retty), ptr(_ptr), indexes(_indexes),
      is_inbounds(_is_inbounds) {}
void TyGetElementPtrInst::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(ty));
  archive(CEREAL_NVP(retty));
  archive(CEREAL_NVP(ptr));
  archive(CEREAL_NVP(indexes));
  archive(CEREAL_NVP(is_inbounds));
}

TyFpextInst::TyFpextInst(std::shared_ptr<TyValueType> _fromty, std::shared_ptr<TyValue> _v, std::shared_ptr<TyValueType> _toty) : fromty(_fromty), v(_v), toty(_toty){
}
void TyFpextInst::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(fromty), CEREAL_NVP(v), CEREAL_NVP(toty));
}

TyFptruncInst::TyFptruncInst(std::shared_ptr<TyValueType> _fromty, std::shared_ptr<TyValue> _v, std::shared_ptr<TyValueType> _toty) : fromty(_fromty), v(_v), toty(_toty){
}
void TyFptruncInst::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(fromty), CEREAL_NVP(v), CEREAL_NVP(toty));
}

TyZextInst::TyZextInst(std::shared_ptr<TyValueType> _fromty, std::shared_ptr<TyValue> _v, std::shared_ptr<TyValueType> _toty) : fromty(_fromty), v(_v), toty(_toty){
}
void TyZextInst::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(fromty), CEREAL_NVP(v), CEREAL_NVP(toty));
}

TySextInst::TySextInst(std::shared_ptr<TyValueType> _fromty, std::shared_ptr<TyValue> _v, std::shared_ptr<TyValueType> _toty) : fromty(_fromty), v(_v), toty(_toty){
}
void TySextInst::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(fromty), CEREAL_NVP(v), CEREAL_NVP(toty));
}

TyTruncInst::TyTruncInst(std::shared_ptr<TyValueType> _fromty, std::shared_ptr<TyValue> _v, std::shared_ptr<TyValueType> _toty) : fromty(_fromty), v(_v), toty(_toty){
}
void TyTruncInst::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(fromty), CEREAL_NVP(v), CEREAL_NVP(toty));
}

TyFptosiInst::TyFptosiInst(std::shared_ptr<TyValueType> _fromty,
                           std::shared_ptr<TyValue> _v,
                           std::shared_ptr<TyValueType> _toty)
    : fromty(_fromty), v(_v), toty(_toty) {}
void TyFptosiInst::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(fromty), CEREAL_NVP(v), CEREAL_NVP(toty));
}

TySitofpInst::TySitofpInst(std::shared_ptr<TyValueType> _fromty, std::shared_ptr<TyValue> _v, std::shared_ptr<TyValueType> _toty) : fromty(_fromty), v(_v), toty(_toty){
}
void TySitofpInst::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(fromty), CEREAL_NVP(v), CEREAL_NVP(toty));
}

TyUitofpInst::TyUitofpInst(std::shared_ptr<TyValueType> _fromty, std::shared_ptr<TyValue> _v, std::shared_ptr<TyValueType> _toty) : fromty(_fromty), v(_v), toty(_toty){
}
void TyUitofpInst::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(fromty), CEREAL_NVP(v), CEREAL_NVP(toty));
}

TyInsertValueInst::TyInsertValueInst(std::shared_ptr<TyValueType> _aggrty, std::shared_ptr<TyValue> _aggrv, std::shared_ptr<TyValueType> _argty, std::shared_ptr<TyValue> _argv, std::vector<unsigned> _idx) : aggrty(_aggrty), aggrv(_aggrv), argty(_argty), argv(_argv), idx(_idx){
}
void TyInsertValueInst::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(aggrty), CEREAL_NVP(aggrv), CEREAL_NVP(argty), CEREAL_NVP(argv), CEREAL_NVP(idx));
}

TyExtractValueInst::TyExtractValueInst(std::shared_ptr<TyValueType> _aggrty, std::shared_ptr<TyValue> _aggrv, std::vector<unsigned> _idx, std::shared_ptr<TyValueType> _retty) : aggrty(_aggrty), aggrv(_aggrv), idx(_idx), retty(_retty){
}
void TyExtractValueInst::serialize(cereal::JSONOutputArchive& archive) const{
  archive(CEREAL_NVP(aggrty), CEREAL_NVP(aggrv), CEREAL_NVP(idx), CEREAL_NVP(retty));
}

// get_op and replace_op of Instructions
std::shared_ptr<TyValue> TyBinaryOperator::get_op(int i) {
  switch (i) {
  case 0: return operand1;
  case 1: return operand2;
  }
  return NULL;
}
void TyBinaryOperator::replace_op(int i, std::shared_ptr<TyValue> val) {
  switch (i) {
  case 0: operand1 = val; break;
  case 1: operand2 = val; break;
  }
}

std::shared_ptr<TyValue> TyFloatBinaryOperator::get_op(int i) {
  switch (i) {
  case 0: return operand1;
  case 1: return operand2;
  }
  return NULL;
}
void TyFloatBinaryOperator::replace_op(int i, std::shared_ptr<TyValue> val) {
  switch (i) {
  case 0: operand1 = val; break;
  case 1: operand2 = val; break;
  }
}

std::shared_ptr<TyValue> TyICmpInst::get_op(int i) {
  switch (i) {
  case 0: return operand1;
  case 1: return operand2;
  }
  return NULL;
}
void TyICmpInst::replace_op(int i, std::shared_ptr<TyValue> val) {
  switch (i) {
  case 0: operand1 = val; break;
  case 1: operand2 = val; break;
  }
}

std::shared_ptr<TyValue> TyFCmpInst::get_op(int i) {
  switch (i) {
  case 0: return operand1;
  case 1: return operand2;
  }
  return NULL;
}
void TyFCmpInst::replace_op(int i, std::shared_ptr<TyValue> val) {
  switch (i) {
  case 0: operand1 = val; break;
  case 1: operand2 = val; break;
  }
}

std::shared_ptr<TyValue> TySelectInst::get_op(int i) {
  switch (i) {
  case 0: return cond;
  case 1: return trueval;
  case 2: return falseval;
  }
  return NULL;
}
void TySelectInst::replace_op(int i, std::shared_ptr<TyValue> val) {
  switch (i) {
  case 0: cond = val; break;
  case 1: trueval = val; break;
  case 2: falseval = val; break;
  }
}

std::shared_ptr<TyValue> TyLoadInst::get_op(int i) {
  if (i == 0) return ptrvalue;
  return NULL;
}
void TyLoadInst::replace_op(int i, std::shared_ptr<TyValue> val) {
  if (i == 0) ptrvalue = val;
}

std::shared_ptr<TyValue> TyBitCastInst::get_op(int i) {
  if (i == 0) return v;
  return NULL;
}
void TyBitCastInst::replace_op(int i, std::shared_ptr<TyValue> val) {
  if (i == 0) v = val;
}

std::shared_ptr<TyValue> TyIntToPtrInst::get_op(int i) {
  if (i == 0) return v;
  return NULL;
}
void TyIntToPtrInst::replace_op(int i, std::shared_ptr<TyValue> val) {
  if (i == 0) v = val;
}

std::shared_ptr<TyValue> TyPtrToIntInst::get_op(int i) {
  if (i == 0) return v;
  return NULL;
}
void TyPtrToIntInst::replace_op(int i, std::shared_ptr<TyValue> val) {
  if (i == 0) v = val;
}

std::shared_ptr<TyValue> TyGetElementPtrInst::get_op(int i) {
  if (i == 0) return ptr;
  else if (i <= indexes.size())
    return indexes[i-1].second;
  return NULL;
}
void TyGetElementPtrInst::replace_op(int i, std::shared_ptr<TyValue> val) {
  if (i == 0) ptr = val;
  else if (i <= indexes.size())
    indexes[i-1].second = val;
}

std::shared_ptr<TyValue> TyFpextInst::get_op(int i) {
  if (i == 0) return v;
  return NULL;
}
void TyFpextInst::replace_op(int i, std::shared_ptr<TyValue> val) {
  if (i == 0) v = val;
}

std::shared_ptr<TyValue> TyFptruncInst::get_op(int i) {
  if (i == 0) return v;
  return NULL;
}
void TyFptruncInst::replace_op(int i, std::shared_ptr<TyValue> val) {
  if (i == 0) v = val;
}

std::shared_ptr<TyValue> TySextInst::get_op(int i) {
  if (i == 0) return v;
  return NULL;
}
void TySextInst::replace_op(int i, std::shared_ptr<TyValue> val) {
  if (i == 0) v = val;
}

std::shared_ptr<TyValue> TyZextInst::get_op(int i) {
  if (i == 0) return v;
  return NULL;
}
void TyZextInst::replace_op(int i, std::shared_ptr<TyValue> val) {
  if (i == 0) v = val;
}

std::shared_ptr<TyValue> TyTruncInst::get_op(int i) {
  if (i == 0) return v;
  return NULL;
}
void TyTruncInst::replace_op(int i, std::shared_ptr<TyValue> val) {
  if (i == 0) v = val;
}

std::shared_ptr<TyValue> TyFptosiInst::get_op(int i) {
  if (i == 0) return v;
  return NULL;
}
void TyFptosiInst::replace_op(int i, std::shared_ptr<TyValue> val) {
  if (i == 0) v = val;
}

std::shared_ptr<TyValue> TySitofpInst::get_op(int i) {
  if (i == 0) return v;
  return NULL;
}
void TySitofpInst::replace_op(int i, std::shared_ptr<TyValue> val) {
  if (i == 0) v = val;
}

std::shared_ptr<TyValue> TyUitofpInst::get_op(int i) {
  if (i == 0) return v;
  return NULL;
}
void TyUitofpInst::replace_op(int i, std::shared_ptr<TyValue> val) {
  if (i == 0) v = val;
}

std::shared_ptr<TyValue> TyInsertValueInst::get_op(int i) {
  switch (i) {
  case 0: return aggrv;
  case 1: return argv;
  }
  return NULL;
}
void TyInsertValueInst::replace_op(int i, std::shared_ptr<TyValue> val) {
  switch (i) {
  case 0: aggrv = val; break;
  case 1: argv = val; break;
  }
}

std::shared_ptr<TyValue> TyExtractValueInst::get_op(int i) {
  switch (i) {
  case 0: return aggrv;
  }
  return NULL;
}
void TyExtractValueInst::replace_op(int i, std::shared_ptr<TyValue> val) {
  switch (i) {
  case 0: aggrv = val; break;
  }
}

// propagate expr
// ConsVar or ConsConst

std::shared_ptr<TyExpr> TyExpr::make(const std::shared_ptr<TyValue> vptr) {
  TyValue *v = vptr.get();
  if (ConsId *cid = dynamic_cast<ConsId *>(v)) {
    return std::shared_ptr<TyExpr>(new ConsVar(cid->reg));
  } else if (ConsConstVal *ccv = dynamic_cast<ConsConstVal *>(v)) {
    return std::shared_ptr<TyExpr>(new ConsConst(ccv->constant));
  } else {
    assert("Unknown value type" && false);
    return nullptr;
  }
  return nullptr;
}

std::shared_ptr<TyExpr> TyExpr::make(const llvm::Value &value,
                                     enum TyTag _tag) {
  return make(TyValue::make(value, _tag));
}

ConsVar::ConsVar(std::shared_ptr<TyRegister> _register_name)
    : register_name(_register_name) {}

ConsVar::ConsVar(std::string _name, enum TyTag _tag)
    : register_name(new TyRegister(_name, _tag)) {}

void ConsVar::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("Var");
  archive(CEREAL_NVP(register_name));
}

std::shared_ptr<TyExpr> ConsVar::make(std::string _name, enum TyTag _tag) {
  return std::shared_ptr<TyExpr>(new ConsVar(_name, _tag));
}

std::shared_ptr<TyRegister> ConsVar::getTyReg() {
  return register_name;
}

void ConsVar::updateTyReg(std::shared_ptr<TyRegister> newTyReg) {
  register_name = newTyReg;
}

ConsRhs::ConsRhs(std::shared_ptr<TyRegister> _register_name,
                 enum TyScope _scope)
    : register_name(_register_name), scope(_scope) {}

ConsRhs::ConsRhs(std::string _name, enum TyTag _tag, enum TyScope _scope)
    : register_name(new TyRegister(_name, _tag)), scope(_scope) {}

void ConsRhs::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("Rhs");
  archive.startNode();
  archive.makeArray();
  archive(CEREAL_NVP(register_name),
          cereal::make_nvp("scope", ::toString(scope)));
  archive.finishNode();
}

std::shared_ptr<TyExpr> ConsRhs::make(std::string _name, enum TyTag _tag,
                                      enum TyScope _scope) {
  return std::shared_ptr<TyExpr>(new ConsRhs(_name, _tag, _scope));
}

ConsConst::ConsConst(std::shared_ptr<TyConstant> _constant)
    : constant(_constant) {}

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

std::shared_ptr<TyExpr> ConsConst::make(int _int_value, int _bitwidth) {
  return std::shared_ptr<TyExpr>(new ConsConst(_int_value, _bitwidth));
}

std::shared_ptr<TyConstant> ConsConst::getTyConst() {
  return constant;
}

ConsInsn::ConsInsn(std::shared_ptr<TyInstruction> _instruction)
    : instruction(_instruction) {}
std::shared_ptr<TyExpr> ConsInsn::make(const llvm::Instruction &i) {
  return std::shared_ptr<TyExpr>(
      new ConsInsn(TyInstruction::make(i)));
}
std::shared_ptr<TyExpr>
ConsInsn::make(std::shared_ptr<TyInstruction> _instruction) {
  return std::shared_ptr<TyExpr>(new ConsInsn(_instruction));
}

std::shared_ptr<TyInstruction> ConsInsn::getTyInsn() {
  return instruction;
}

void ConsInsn::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("Insn");
  archive(CEREAL_NVP(instruction));
}

std::shared_ptr<TyValue> ConsInsn::get_op(int i) {
  return instruction->get_op(i);
}

void ConsInsn::replace_op(int i, std::shared_ptr<TyValue> val) {
  instruction->replace_op(i, val);
}

/* Propagate */

// propagate object

TyPropagateLessdef::TyPropagateLessdef(std::shared_ptr<TyExpr> _lhs,
                                       std::shared_ptr<TyExpr> _rhs,
                                       enum TyScope _scope)
    : lhs(_lhs), rhs(_rhs), scope(_scope) {
  assert(_lhs);
  assert(_rhs);
}

void TyPropagateLessdef::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(lhs), CEREAL_NVP(rhs),
          cereal::make_nvp("scope", ::toString(scope)));
}

std::shared_ptr<TyPropagateLessdef>
TyPropagateLessdef::make(std::shared_ptr<TyExpr> _lhs,
                         std::shared_ptr<TyExpr> _rhs, enum TyScope _scope) {
  return std::shared_ptr<TyPropagateLessdef>(
      new TyPropagateLessdef(_lhs, _rhs, _scope));
}

std::shared_ptr<TyExpr> TyPropagateLessdef::getLhs() {
  return lhs;
}

std::shared_ptr<TyExpr> TyPropagateLessdef::getRhs() {
  return rhs;
}

void TyPropagateLessdef::updateRhs(std::shared_ptr<TyExpr> newExpr) {
  rhs = newExpr;
}

TyPropagateNoalias::TyPropagateNoalias(std::shared_ptr<TyPointer> _lhs,
                                       std::shared_ptr<TyPointer> _rhs,
                                       enum TyScope _scope)
    : lhs(_lhs), rhs(_rhs), scope(_scope) {}

void TyPropagateNoalias::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(lhs), CEREAL_NVP(rhs),
          cereal::make_nvp("scope", ::toString(scope)));
}

std::shared_ptr<TyPropagateNoalias>
TyPropagateNoalias::make(std::shared_ptr<TyPointer> _lhs,
                         std::shared_ptr<TyPointer> _rhs, enum TyScope _scope) {
  return std::shared_ptr<TyPropagateNoalias>(
      new TyPropagateNoalias(_lhs, _rhs, _scope));
}

TyPropagateDiffblock::TyPropagateDiffblock(std::shared_ptr<TyValue> _lhs,
                                           std::shared_ptr<TyValue> _rhs,
                                           enum TyScope _scope)
    : lhs(_lhs), rhs(_rhs), scope(_scope) {}

void TyPropagateDiffblock::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(lhs), CEREAL_NVP(rhs),
          cereal::make_nvp("scope", ::toString(scope)));
}

std::shared_ptr<TyPropagateDiffblock>
TyPropagateDiffblock::make(std::shared_ptr<TyValue> _lhs,
                           std::shared_ptr<TyValue> _rhs, enum TyScope _scope) {
  return std::shared_ptr<TyPropagateDiffblock>(
      new TyPropagateDiffblock(_lhs, _rhs, _scope));
}

TyPropagateUnique::TyPropagateUnique(std::string _register_name,
                                     enum TyScope _scope)
    : register_name(_register_name), scope(_scope) {}

void TyPropagateUnique::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(register_name));
  archive(cereal::make_nvp("scope", ::toString(scope)));
}

TyPropagatePrivate::TyPropagatePrivate(std::shared_ptr<TyRegister> _p,
                                       enum TyScope _scope)
    : p(_p), scope(_scope) {}

void TyPropagatePrivate::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(p));
  archive(cereal::make_nvp("scope", ::toString(scope)));
}

ConsLessdef::ConsLessdef(std::shared_ptr<TyPropagateLessdef> _propagate_lessdef)
    : propagate_lessdef(_propagate_lessdef) {}

void ConsLessdef::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("Lessdef");
  archive(CEREAL_NVP(propagate_lessdef));
}

std::shared_ptr<TyPropagateObject>
ConsLessdef::make(std::shared_ptr<TyExpr> _lhs, std::shared_ptr<TyExpr> _rhs,
                  enum TyScope _scope) {
  auto ty_prop_ld = TyPropagateLessdef::make(_lhs, _rhs,
                                             _scope);
  return std::shared_ptr<TyPropagateObject>(
      new ConsLessdef(ty_prop_ld));
}

ConsNoalias::ConsNoalias(std::shared_ptr<TyPropagateNoalias> _propagate_noalias)
    : propagate_noalias(_propagate_noalias) {}

std::shared_ptr<TyPropagateObject>
ConsNoalias::make(std::shared_ptr<TyPointer> _lhs,
                  std::shared_ptr<TyPointer> _rhs, enum TyScope _scope) {
  return std::shared_ptr<TyPropagateObject>(
      new ConsNoalias(TyPropagateNoalias::make(_lhs, _rhs, _scope)));
}

void ConsNoalias::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("Noalias");
  archive(CEREAL_NVP(propagate_noalias));
}

ConsDiffblock::ConsDiffblock(
    std::shared_ptr<TyPropagateDiffblock> _propagate_diffblock)
    : propagate_diffblock(_propagate_diffblock) {}

std::shared_ptr<TyPropagateObject>
ConsDiffblock::make(std::shared_ptr<TyValue> _lhs,
                    std::shared_ptr<TyValue> _rhs, enum TyScope _scope) {
  return std::shared_ptr<TyPropagateObject>(
      new ConsDiffblock(TyPropagateDiffblock::make(_lhs, _rhs, _scope)));
}

void ConsDiffblock::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("Diffblock");
  archive(CEREAL_NVP(propagate_diffblock));
}

ConsUnique::ConsUnique(std::shared_ptr<TyPropagateUnique> _propagate_unique)
    : propagate_unique(_propagate_unique) {}

std::shared_ptr<TyPropagateObject>
ConsUnique::make(std::string _register_name, enum TyScope _scope) {
  std::shared_ptr<TyPropagateUnique> _val(
      new TyPropagateUnique(_register_name, _scope));

  return std::shared_ptr<TyPropagateObject>(new ConsUnique(_val));
}

void ConsUnique::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("Unique");
  archive(CEREAL_NVP(propagate_unique));
}

ConsMaydiff::ConsMaydiff(std::shared_ptr<TyRegister> _register_name)
    : register_name(_register_name) {}

ConsMaydiff::ConsMaydiff(std::string _name, enum TyTag _tag)
    : register_name(new TyRegister(_name, _tag)) {}

std::shared_ptr<TyPropagateObject> ConsMaydiff::make(std::string _name,
                                                     enum TyTag _tag) {
  return std::shared_ptr<TyPropagateObject>(
      new ConsMaydiff(TyRegister::make(_name, _tag)));
}

void ConsMaydiff::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("Maydiff");
  archive(CEREAL_NVP(register_name));
}

ConsPrivate::ConsPrivate(std::shared_ptr<TyPropagatePrivate> _propagate_private)
    : propagate_private(_propagate_private) {}

std::shared_ptr<TyPropagateObject>
ConsPrivate::make(std::shared_ptr<TyRegister> _p, enum TyScope _scope) {
  std::shared_ptr<TyPropagatePrivate> _val(
      new TyPropagatePrivate(_p, _scope));

  return std::shared_ptr<TyPropagateObject>(new ConsPrivate(_val));
}

void ConsPrivate::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();
  archive.saveValue("Private");
  archive(CEREAL_NVP(propagate_private));
}

// propagate range

ConsBounds::ConsBounds(std::shared_ptr<TyPosition> _from,
                       std::shared_ptr<TyPosition> _to)
    : from(_from), to(_to) {}

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
      new ConsBounds(_from, _to));
}

ConsBoundSet::ConsBoundSet(std::shared_ptr<TyPosition> _from,
                           std::shared_ptr<std::vector<std::shared_ptr<TyPosition>>> _to_set)
        : from(_from), to_set(_to_set) {}

void ConsBoundSet::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("BoundSet");
  archive.startNode();
  archive.makeArray();
  archive(from, *to_set.get());
  archive.finishNode();
}

std::shared_ptr<TyPropagateRange>
ConsBoundSet::make(std::shared_ptr<TyPosition> _from,
                   std::shared_ptr<std::vector<std::shared_ptr<TyPosition>>> _to_set) {
  return std::shared_ptr<TyPropagateRange>(
          new ConsBoundSet(_from, _to_set));
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
    : propagate(_propagate), propagate_range(_propagate_range) {}

void TyPropagate::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(propagate));
  if (propagate_range->isGlobal()) {
    propagate_range->serialize(archive);
  } else {
    archive(CEREAL_NVP(propagate_range));
  }
}

ConsPropagate::ConsPropagate(std::shared_ptr<TyPropagate> _propagate)
    : propagate(_propagate) {}

void ConsPropagate::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("Propagate");
  archive(CEREAL_NVP(propagate));
}

std::shared_ptr<TyCommand>
ConsPropagate::make(std::shared_ptr<TyPropagate> _propagate) {
  return std::shared_ptr<TyCommand>(new ConsPropagate(_propagate));
}

std::shared_ptr<TyCommand>
ConsPropagate::make(std::shared_ptr<TyPropagateObject> _obj,
                    std::shared_ptr<TyPropagateRange> _range) {
  std::shared_ptr<TyPropagate> _propagate(
      new TyPropagate(_obj, _range));
  return std::shared_ptr<TyCommand>(new ConsPropagate(_propagate));
}

ConsInfrule::ConsInfrule(std::shared_ptr<TyPosition> _position,
                         std::shared_ptr<TyInfrule> _infrule)
    : position(_position), infrule(_infrule) {}

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
      new ConsInfrule(_position, _infrule));
}

TyCppDebugInfo::TyCppDebugInfo(const std::string &_file_name,
                               int _line_number)
    : file_name(_file_name), line_number(_line_number) {}
void TyCppDebugInfo::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(file_name), CEREAL_NVP(line_number));
}
std::shared_ptr<TyCppDebugInfo> TyCppDebugInfo::make(const char *_file_name,
                                                int _line_number) {
  return std::shared_ptr<TyCppDebugInfo>(
      new TyCppDebugInfo(std::string(_file_name), _line_number));
}

// core hint

CoreHint::CoreHint() : return_code(CoreHint::ACTUAL), auto_option(CoreHint::AUTO_DEFAULT) {}

CoreHint::CoreHint(std::string _module_id, std::string _function_id,
                   std::string _opt_name, std::string _description)
    : module_id(_module_id), function_id(_function_id), opt_name(_opt_name),
      description(_description), return_code(CoreHint::ACTUAL), auto_option(CoreHint::AUTO_DEFAULT){}

const std::string &CoreHint::getDescription() const {
  return this->description;
}

void CoreHint::setDescription(const std::string &desc) {
  this->description = desc;
}

void CoreHint::appendToDescription(const std::string &desc) {
  this->description += "\n" + desc;
}

// User may not need to use this function; it may only be used inside intrude.
const CoreHint::RETURN_CODE &CoreHint::getReturnCode() const {
  return this->return_code;
}

void CoreHint::setReturnCodeToAdmitted() {
  assert(this->return_code == CoreHint::ACTUAL);
  this->return_code = ADMITTED;
}

void CoreHint::setReturnCodeToFail() {
  assert(this->return_code == CoreHint::ACTUAL);
  this->return_code = FAIL;
}

void CoreHint::addCommand(std::shared_ptr<TyCommand> c) {
  commands.push_back(std::make_pair(c,
                                    llvmberry::TyCppDebugInfo::make("", 0)));
}

void CoreHint::addCommand(std::shared_ptr<TyCommand> c,
                          std::shared_ptr<TyCppDebugInfo> d) {
  commands.push_back(std::make_pair(c, d));
}

void CoreHint::addNopPosition(std::shared_ptr<TyPosition> position) {
  nop_positions.push_back(position);
}

void CoreHint::serialize(cereal::JSONOutputArchive &archive) const {
  std::string function_id = std::string("@") + this->function_id;
  archive(CEREAL_NVP(module_id));
  archive(CEREAL_NVP(function_id));
  archive(CEREAL_NVP(opt_name));
  archive(CEREAL_NVP(description));
  archive(cereal::make_nvp("return_code", ::toString(return_code)));
  archive(cereal::make_nvp("auto_option", ::toString(auto_option)));
  archive(CEREAL_NVP(commands));
  archive(CEREAL_NVP(nop_positions));
}

void CoreHint::setOptimizationName(const std::string &name) {
  this->opt_name = name;
}

void CoreHint::setAutoOption(CoreHint::AUTO_OPT opt) {
  this->auto_option = opt;
}

void intrude(std::function<void()> func) {
  if (RuntimeOptions::IgnorePass(ValidationUnit::GetCurrentPass()))
    return;
  func();
}

} // llvmberry
