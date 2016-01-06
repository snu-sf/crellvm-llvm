#include "llvm/Hint/Sender.h"

namespace hintgen {

//void HintSender::write_json(std::stringstream &ss, structure::hints *hint)
void HintSender::write_json(std::stringstream &ss)
{
  cereal::JSONOutputArchive oarchive(ss);

  //HintFactory::theOne.instances.front()->serialize(oarchive);
  //hint->serialize(oarchive);
}

void HintSender::flushHints() {
  std::stringstream ss;
  std::ofstream ofs("temp.txt");
  //write_json(ss, this->hints);
  write_json(ss);

  ofs << ss.str() << std::endl;
}

} // hintgen
