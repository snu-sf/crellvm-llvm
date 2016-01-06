#include <cereal/archives/json.hpp>
#include <sstream>
#include <iostream>
#include <fstream>
#include <string>
#include <cereal/types/vector.hpp>
#include <cassert>

namespace cereal {
  [[noreturn]] void throw_exception(std::exception const & e) {
    std::exit(1);
  }
}
enum scope { Source = 0, Target };
enum instr_type { Command = 0, Phinode, Terminal };
enum age { Old = 0, New };

std::string toString(enum instr_type instrType) {
  switch (instrType) {
    case Command:
      return std::string("Command");
    case Phinode:
      return std::string("Phinode");
    case Terminal:
      return std::string("Terminator");
    default:
      assert(false);
      return std::string("");
  }
}

std::string toString(enum scope scope) {
  switch (scope) {
    case Source:
      return std::string("Source");
    case Target:
      return std::string("Target");
    default:
      assert(false);
      return std::string("");
  }
}

namespace hintgen {
class HintFactory {
public:
struct position {
  std::string block_index;
  std::string instr_type;
  int instr_index;

//implementation of 
position() {}
  
position(std::string _block_index, enum instr_type _instr_type, int _instr_index):
  block_index(_block_index), instr_type(toString(_instr_type)), instr_index(_instr_index) {}

void serialize(cereal::JSONOutputArchive & archive) {
  archive(CEREAL_NVP(block_index),
          CEREAL_NVP(instr_type),
          CEREAL_NVP(instr_index));
}
};

struct variable {
  std::string name;
  age itsage;
};

struct command {
public:
  virtual void serialize(cereal::JSONOutputArchive &archive){}
  virtual const char * getCommandName() = 0;
};

struct infrule_base {
  position itsposition;
};

struct PropagateInstr : public command {
  position propagate_from;
  position propagate_to; 
  std::string scope;

// implementation of 
PropagateInstr() {}
  
PropagateInstr(position _propagate_from, position _propagate_to, enum scope _scope):
  propagate_from(_propagate_from), propagate_to(_propagate_to), scope(toString(_scope)) {}

const char* getCommandName(){
  return "PropagateInstr";
}
void serialize(cereal::JSONOutputArchive& archive) {
  archive(CEREAL_NVP(propagate_from),
          CEREAL_NVP(propagate_to),
          CEREAL_NVP(scope));
}
};

struct AddAssoc : public command {
  infrule_base position;
  variable lhs;
  variable rhs;
};

struct RemoveMaydiff : public command { 
  infrule_base position;
  variable itsvariable;
};


void serialize(cereal::JSONOutputArchive &archive, command *& c) {
  archive.makeArray();
  archive.startNode();
  archive.saveValue(c->getCommandName());
  c->serialize(archive);
  archive.finishNode();
}

struct hints {
  std::string function_id, module_id, opt_name;
  std::vector<command *> commands;

// implementation of  
hints(std::string _function_id, std::string _module_id, std::string _opt_name):
  function_id(_function_id), module_id(_module_id), opt_name(_opt_name) {
}

//template<typename T>
void addCommand(command *c){
  commands.push_back(c);
}

void serialize(cereal::JSONOutputArchive &archive) {
  archive(CEREAL_NVP(function_id));
  archive(CEREAL_NVP(module_id));
  archive(CEREAL_NVP(opt_name));
  archive(CEREAL_NVP(commands));
}
};

}; //hintfactory
} //hintgen

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
  hintgen::write_json(ss);
  std::cout << ss.str() << std::endl;
  ofs << ss.str() << std::endl;
  
  return 0;
}
*/
