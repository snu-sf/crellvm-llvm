#include <sstream>
#include <fstream>
#include <string>
#include <cassert>
// #include "llvm/LLVMBerry/Structure.h"
#include <cereal/archives/json.hpp>
#include <cereal/types/vector.hpp>
#include <cereal/types/memory.hpp>
#include <cereal/types/polymorphic.hpp>
#include "Structure.h"

namespace cereal {
  [[noreturn]] void throw_exception(std::exception const &e) { std::exit(1); }

  template <class T>
  void save( cereal::JSONOutputArchive &archive, std::unique_ptr<T> const &ptr) {
    ptr->serialize(archive);
    // archive( *ptr );
  }

//   template <class T>
// void save( cereal::JSONOutputArchive &archive,
// 	   std::vector<std::unique_ptr<T>> const &v) {
//     archive.makeArray();
//   // archive( *ptr );
// }


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

// // unsigned int getRawInstrIndex(const llvm::Instruction &instr) {
// //   const llvm::BasicBlock *parent = instr.getParent();
// //   const llvm::BasicBlock::InstListType &instList = parent->getInstList();

// //   unsigned int idx = 0;
// //   for (llvm::BasicBlock::const_iterator itr = instList.begin();
// //        itr != instList.end(); ++itr) {
// //     if (&instr == &(*itr))
// //       return idx;
// //     idx++;
// //   }

// //   return (unsigned int)-1;
// // }

// /// @return the index of the BasicBlock w.r.t. the parent function.
// std::string getBasicBlockIndex(const llvm::BasicBlock *block) {
//   if (!block || !(block->getParent())) {
//     std::stringstream retStream;
//     retStream << ((unsigned int)-1);
//     return retStream.str();
//   }

//   // If a block has its own name, just return it.
//   if (block->hasName()) {
//     return block->getName();
//   }

//   // If else, calculate the index and return it.
//   const llvm::Function *parent = block->getParent();
//   const llvm::Function::BasicBlockListType &blockList =
//       parent->getBasicBlockList();

//   unsigned int idx = 0;
//   for (llvm::Function::const_iterator itr = blockList.begin();
//        itr != blockList.end(); ++itr) {
//     if (block == &(*itr)) {
//       std::stringstream retStream;
//       retStream << idx;
//       return (retStream.str());
//     }

//     idx++;
//   }
//   std::stringstream retStream;
//   retStream << ((unsigned int)-1);
//   return retStream.str();
// }

} // anonymous

namespace llvmberry {

// std::string getVariable(const llvm::Value &value) {
//   std::string val;

//   if (llvm::isa<llvm::GlobalValue>(value)) {
//     val = std::string("@");
//   } else {
//     val = std::string("%");
//   }

//   val += std::string(value.getName().data());

//   return val;
// }

// bool name_instructions(llvm::Function &F) {
//   for (llvm::Function::arg_iterator AI = F.arg_begin(), AE = F.arg_end();
//        AI != AE; ++AI)
//     if (!AI->hasName() && !AI->getType()->isVoidTy())
//       AI->setName("arg");

//   for (llvm::Function::iterator BB = F.begin(), E = F.end(); BB != E; ++BB) {
//     if (!BB->hasName())
//       BB->setName("bb");

//     for (llvm::BasicBlock::iterator I = BB->begin(), E = BB->end(); I != E; ++I)
//       if (!I->hasName() && !I->getType()->isVoidTy())
//         I->setName("tmp");
//   }

//   return true;
// }

// position

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
// Constant classes
ConsIntType::ConsIntType(bool _signed_flag, int _value)
  : signed_flag(_signed_flag), value(_value) {}

void ConsIntType::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("IntType");
  archive.startNode();
  archive.makeArray();
  archive(CEREAL_NVP(signed_flag), CEREAL_NVP(value));
  archive.finishNode();
  //archive(CEREAL_NVP(signed_flag), CEREAL_NVP(value));
}

TyConstInt::TyConstInt(int _int_value, std::unique_ptr<TyIntType> _int_type)
 : int_value(_int_value), int_type(std::move(_int_type)) {}

void TyConstInt::serialize(cereal::JSONOutputArchive &archive) const {
  archive(cereal::make_nvp("int_value", int_value), CEREAL_NVP(int_type));
}

ConsConstInt::ConsConstInt(std::unique_ptr<TyConstInt> _const_int)
  : const_int(std::move(_const_int)) {}

void ConsConstInt::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("ConstInt");
  archive(CEREAL_NVP(const_int));
}

TyConstFloat::TyConstFloat(float _float_value, enum TyFloatType _float_type)
        : float_value(_float_value), float_type(_float_type){ }

void TyConstFloat::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(float_value), 
      cereal::make_nvp("float_type", toString(float_type)));
}

ConsConstFloat::ConsConstFloat(std::unique_ptr<TyConstFloat> _const_float)
        : const_float(std::move(_const_float)){ }

void ConsConstFloat::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("ConstFloat");
  archive(CEREAL_NVP(const_float));
}

ConsSize::ConsSize(int _size)
        : size(_size) {}

void ConsSize::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("Size");
  archive(CEREAL_NVP(size));
}
// register

TyRegister::TyRegister(std::string _name, enum TyTag _tag)
        : name(_name), tag(_tag) {}

void TyRegister::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(name), cereal::make_nvp("tag", toString(tag)));
}

// * Propagate *

// propagate expr

ConsVar::ConsVar(std::unique_ptr<TyRegister> _register_name)
        : register_name(std::move(_register_name)) {}

void ConsVar::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("Var");
  archive(CEREAL_NVP(register_name));
}

ConsRhs::ConsRhs(std::unique_ptr<TyRegister> _register_name)
        : register_name(std::move(_register_name)) {}

void ConsRhs::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("Rhs");
  archive(CEREAL_NVP(register_name));
}

ConsConst::ConsConst(std::unique_ptr<TyConstant> _constant)
        : constant(std::move(_constant)) {}

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


ConsLessdef::ConsLessdef(std::unique_ptr<TyPropagateLessdef> _propagate_lessdef)
        : propagate_lessdef(std::move(_propagate_lessdef)) {}

void ConsLessdef::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("Lessdef");
  archive(CEREAL_NVP(propagate_lessdef));
}
TyPropagateNoalias::TyPropagateNoalias
        (std::unique_ptr<TyRegister> _lhs,
         std::unique_ptr<TyRegister> _rhs,
         enum TyScope _scope)
        : lhs(std::move(_lhs)), rhs(std::move(_rhs)), scope(_scope) {}

void TyPropagateNoalias::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(lhs), CEREAL_NVP(rhs), cereal::make_nvp("scope", toString(scope)));
}

ConsNoalias::ConsNoalias(std::unique_ptr<TyPropagateNoalias> _propagate_noalias)
        : propagate_noalias(std::move(_propagate_noalias)) { }

void ConsNoalias::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("Noalias");
  archive(CEREAL_NVP(propagate_noalias));
}

ConsMaydiff::ConsMaydiff(std::unique_ptr<TyRegister> _register_name)
        : register_name(std::move(_register_name)) {}

void ConsMaydiff::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("Maydiff");
  archive(CEREAL_NVP(register_name));
}

// propagate range

ConsGlobal::ConsGlobal() {}

void ConsGlobal::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("Global"); /* TODO: how to print this in JSON */
}

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



// propagate

TyPropagate::TyPropagate(std::unique_ptr<TyPropagateObject> _propagate,
                         std::unique_ptr<TyPropagateRange> _propagate_range)
        : propagate(std::move(_propagate)),
          propagate_range(std::move(_propagate_range)) {}

void TyPropagate::serialize(cereal::JSONOutputArchive &archive) const {
  archive(CEREAL_NVP(propagate), CEREAL_NVP(propagate_range));
}

ConsPropagate::ConsPropagate(std::unique_ptr<TyPropagate> _propagate)
        : propagate(std::move(_propagate)) {}

void ConsPropagate::serialize(cereal::JSONOutputArchive &archive) const {
  archive.makeArray();
  archive.writeName();

  archive.saveValue("Propagate");
  archive(CEREAL_NVP(propagate));
}

void CoreHint::serialize(cereal::JSONOutputArchive &archive) const {
  std::string function_id = std::string("@") + this->function_id;
  archive(CEREAL_NVP(module_id));
  archive(CEREAL_NVP(function_id));
  archive(CEREAL_NVP(opt_name));
  archive(CEREAL_NVP(src_nop_positions));
  archive(CEREAL_NVP(tgt_nop_positions));
  archive(CEREAL_NVP(commands));
  // archive.setNextName("commands");
  // archive.startNode();
  // archive.makeArray();
  // archive.writeName();
  // for (auto it = commands.begin(); it != commands.end(); ++it) {
  //   archive.startNode();
  //   (*it)->serialize(archive);
  //   archive.finishNode();
  // }
  // archive.finishNode();
}

// class CoreHint

CoreHint::CoreHint() {}

CoreHint::CoreHint(std::string _module_id, std::string _function_id,
                   std::string _opt_name)
        : module_id(_module_id), function_id(_function_id), opt_name(_opt_name) {
  // _src_nop_positions.push_back(std::unique_ptr<int>(new int(3)));
}

void CoreHint::addCommand(std::unique_ptr<TyCommand> c) {
  commands.push_back(std::move(c));
}

void CoreHint::addSrcNopPosition(std::unique_ptr<TyPosition> position) {
  src_nop_positions.push_back(std::move(position));
}

// void CoreHint::addAddedInstrPosition(const Position &p) {
//   added_instr_positions.push_back(p);
// }


// inference rules

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

TyAddAssociative::TyAddAssociative(std::unique_ptr<TyRegister> _x,
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

} // llvmberry
