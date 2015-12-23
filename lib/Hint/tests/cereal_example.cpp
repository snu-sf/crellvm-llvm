#include <cereal/types/vector.hpp>
#include <cereal/archives/json.hpp>
#include <iostream>

int main() {
  cereal::JSONOutputArchive archive(std::cout);
 
  bool arr[] = {true, false};
  std::vector<int> vec = {1, 2, 3 ,4, 5};
  archive(CEREAL_NVP(vec), arr);
}
