#include "llvm/Hint/Factory.h"
#include "cereal/archives/json.hpp"
#include "cereal/types/vector.hpp"
#include <sstream>
#include <fstream>

namespace hintgen {

class HintSender {
public:
  //static void write_json(std::stringstream &ss, structure::hints *hint);
  static void write_json(std::stringstream &ss);
  static void flushHints();
};

} // hintgen
