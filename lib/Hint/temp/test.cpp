#include "cereal/archives/json.hpp"
#include "cereal/types/vector.hpp"
#include <sstream>
#include <iostream>
#include <fstream>
#include <string>
//#include "Structure.h"
#include "llvm/Hint/Sender.h"
//#include <llvm/Support/Casting.h>

using namespace hintgen::structure;
void write_json(std::stringstream &ss){
  cereal::JSONOutputArchive oarchive(ss);

  position p1("bb", Command, 0), p2("bb",Command,1);
  PropagateInstr *pi = new PropagateInstr(p1,p2,Source);
  PropagateInstr *pi2 = new PropagateInstr(p2,p1,Target);

  //hints temp("foo", "example.bc", "add_assoc");
  //temp.addCommand(pi);
  //temp.addCommand(pi2);
  
  hintgen::HintFactory::getDefaultInstance().createHintsInstance("foo","example.2.json","add_assoc");
  //hintgen::HintFactory::getDefaultInstance().createHintsInstance(fileNumber, *Result); //tochange
  //temp.serialize(oarchive);

  hintgen::HintFactory::getDefaultInstance().getHintInstance().front()->addCommand(pi2);
  hintgen::HintFactory::getDefaultInstance().getHintInstance().front()->addCommand(pi);
  //hintgen::HintFactory::getDefaultInstance().getHintInstance().front()->serialize(oarchive);
  hintgen::HintSender::flushHints();
  delete pi;
  delete pi2;
}

int main() {
  std::stringstream ss;
  std::ofstream ofs("test.txt");
  write_json(ss);

  ofs << ss.str() << std::endl;

  return 0;
}
