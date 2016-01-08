#include "cereal/archives/json.hpp"
#include <sstream>
#include <iostream>
#include <fstream>
#include <string>
#include "cereal/types/vector.hpp"
#include <cassert>
#include "llvm/LLVMberry/Structure.h"

namespace cereal {
  [[noreturn]] void throw_exception(std::exception const & e) {
    std::exit(1);
  }
}
//enum scope { Source = 0, Target };
//enum instr_type { Command = 0, Phinode, Terminal };
//enum age { Old = 0, New };

static std::string toString(enum llvmberry::structure::instr_type instrType) {
  switch (instrType) {
    case llvmberry::structure::Command:
      return std::string("Command");
    case llvmberry::structure::Phinode:
      return std::string("Phinode");
    case llvmberry::structure::Terminal:
      return std::string("Terminator");
    default:
      assert(false);
      return std::string("");
  }
}

static std::string toString(enum llvmberry::structure::scope scope) {
  switch (scope) {
    case llvmberry::structure::Source:
      return std::string("Source");
    case llvmberry::structure::Target:
      return std::string("Target");
    default:
      assert(false);
      return std::string("");
  }
}

namespace llvmberry {
namespace structure {
//struct position {
//  std::string block_index;
//  std::string instr_type;
//  int instr_index;

//implementation of position::
position::position() {}
  
position::position(std::string _block_index, enum instr_type _instr_type, int _instr_index):
  block_index(_block_index), instr_type(toString(_instr_type)), instr_index(_instr_index) {
}

void position::serialize(cereal::JSONOutputArchive & archive) {
  archive(CEREAL_NVP(block_index),
          CEREAL_NVP(instr_type),
          CEREAL_NVP(instr_index));
}
//};

struct variable {
  std::string name;
  age itsage;
};

/*struct command {
public:
  virtual void serialize(cereal::JSONOutputArchive &archive){}
  virtual const char * getCommandName() = 0;
};*/

struct infrule_base {
  position itsposition;
};

//struct PropagateInstr : public command {
//  position propagate_from;
//  position propagate_to; 
//  std::string scope;

// implementation of PropagateInstr::
PropagateInstr::PropagateInstr() {}
  
PropagateInstr::PropagateInstr(position _propagate_from, position _propagate_to, enum scope _scope):
  propagate_from(_propagate_from), propagate_to(_propagate_to), scope(toString(_scope)) {}

const char* PropagateInstr::getCommandName(){
  return "PropagateInstr";
}
void PropagateInstr::serialize(cereal::JSONOutputArchive& archive) {
  archive(CEREAL_NVP(propagate_from),
          CEREAL_NVP(propagate_to),
          CEREAL_NVP(scope));
}
//};

struct AddAssoc : public command, public infrule_base{
  infrule_base position;
  variable lhs;
  variable rhs;
};

struct RemoveMaydiff : public command, public infrule_base{
  infrule_base position;
  variable itsvariable;
};


void serialize(cereal::JSONOutputArchive &archive, command *&c){
  archive.makeArray();
  archive.startNode();
  archive.saveValue(c->getCommandName());
  c->serialize(archive);
  archive.finishNode();
}

//struct hints : public PropagateInstr {
//  std::string function_id, module_id, opt_name;
//  std::vector<command *> commands;

// implementation of hints:: 
hints::hints() {}

hints::hints(std::string _module_id, std::string _function_id, std::string _opt_name):
  module_id(_module_id), function_id(_function_id), opt_name(_opt_name) {
}

//template<typename T>
void hints::addCommand(command *c){
  commands.push_back(c);
}

void hints::serialize(cereal::JSONOutputArchive &archive) {
  std::string function_id = std::string("@") + this->function_id;
  archive(CEREAL_NVP(module_id));
  archive(CEREAL_NVP(function_id));
  archive(CEREAL_NVP(opt_name));
  archive(CEREAL_NVP(commands));
}

} // structure
} // hintgen

/*
void write_json(std::stringstream &ss) {
  cereal::JSONOutputArchive oarchive(ss);

  position p1("bb", Command, 0), p2("bb", Command, 1);
  PropagateInstr *pi = new PropagateInstr(p1, p2, Source);
  PropagateInstr *pi2 = new PropagateInstr(p2, p1, Target);

  hints temp("foo", "example.bc", "add_assoc");
  temp.addCommand(pi);
  temp.addCommand(pi2);
  temp.serialize(oarchive);
  delete pi;
  delete pi2;
}

}

int main()
{
  std::stringstream ss; 
  std::ofstream ofs("json_output.txt");
  llvmberry::write_json(ss);
  std::cout << ss.str() << std::endl;
  ofs << ss.str() << std::endl;
  
  return 0;
}
*/
