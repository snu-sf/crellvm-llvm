#include <llvm/Intruder.h>
#include <iostream>

void boost::throw_exception(std::exception const &e) {
  // This function is called whenever exception is "expected" in the intruder library."
  std::cout << "boost throw_exception" << std::endl;
}
