#include <iostream>
#include <cereal/archives/json.hpp>
#include <cereal/types/vector.hpp>

namespace hintgen {
enum scope { Source = 0, Target };
enum instr_type { Command = 0, Phinode, Terminal };
enum age { Old = 0, New };

class HintFactory {
public:
struct position {
public:
  position();
  position(std::string _block_index, enum instr_type _instr_type, int _instr_index);
  void serialize(cereal::JSONOutputArchive &archive);

private:
  std::string block_index;
  std::string instr_type;
  int instr_index;
};

/*
struct variable {

};
*/

struct command {
public:
  virtual void serialize(cereal::JSONOutputArchive &archive){}
  virtual const char* getCommandName()=0;
};

class PropagateInstr : public command {
public:
  PropagateInstr();
  PropagateInstr(position _propagate_from, position _propagate_to, enum scope _scope);
  virtual const char* getCommandName();
  virtual void serialize(cereal::JSONOutputArchive &archive);

private:
  position propagate_from;
  position propagate_to;
  std::string scope;
};

class hints {
public:
  hints(std::string _function_id, std::string _module_id, std::string _opt_name);
  void addCommand(command *c);
  void serialize(cereal::JSONOutputArchive &archive);
  
private: 
  std::string function_id, module_id, opt_name;
  std::vector<command *> commands;
};

/*
void serialize(cereal::JSONOutputArchive &archive, command *& c) {
  archive.makeArray();
  archive.startNode();
  archive.saveValue(c->getCommandName());
  c->serialize(archive);
  archive.finishNode();
}
*/

}; // HintFactory
} // hintgen
