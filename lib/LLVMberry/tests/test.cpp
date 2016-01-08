#include <cereal/archives/json.hpp>
#include <cereal/types/vector.hpp>
#include <sstream>
#include <iostream>
#include <fstream>
#include <string>
//#include "Structure.h"
#include "llvm/Hint/Factory.h"
//#include <llvm/Support/Casting.h>

using namespace hintgen::structure;
void write_json(std::stringstream &ss){
  cereal::JSONOutputArchive oarchive(ss);

  position p1("bb", Command, 0), p2("bb",Command,1);
  PropagateInstr *pi = new PropagateInstr(p1,p2,Source);
  PropagateInstr *pi2 = new PropagateInstr(p2,p1,Target);

  hints temp("foo", "example.bc", "add_assoc");
  temp.addCommand(pi);
  temp.addCommand(pi2);
  
  hintgen::HintFactory::getDefaultInstance().createHintsInstance("foo2","example.2.bc","add_assoc");
  hints* tmp = (hints*)hintgen::HintFactory::getDefaultInstance().instances.begin();
  temp.serialize(oarchive);
  
  //fac.begin.serialize(oarchive);
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
