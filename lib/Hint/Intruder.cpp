#include <llvm/Intruder.h>

void boost::throw_exception(std::exception const &e) {
  // This function is called whenever exception is "expected" in the intruder library."
}
