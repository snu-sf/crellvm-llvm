// g++ -std=c++11 -I/Users/jeehoon/Works/cereal/include -o test -DCEREAL_NO_EXCEPTIONS -fno-exceptions test.cpp

#include <cereal/archives/json.hpp>
#include <sstream>
#include <iostream>

namespace cereal {
  [[noreturn]] void throw_exception(std::exception const & e) {
    std::exit(1);
  }
}

struct MyClass
{
  int x, y, z;

  MyClass() {
  }

  MyClass(int _x, int _y, int _z):
    x(_x), y(_y), z(_z) {
  }

  template<class Archive>
  void serialize(Archive & archive)
  {
    archive(CEREAL_NVP(x),
            CEREAL_NVP(y),
            CEREAL_NVP(z));
  }
};

void write_json(std::ostream &ss) {
  cereal::JSONOutputArchive oarchive(ss);

  MyClass m1(1, 2, 3), m2(4, 5, 6), m3(7, 8, 9);
  oarchive(CEREAL_NVP(m1),
           CEREAL_NVP(m2),
           CEREAL_NVP(m3));
}

void read_json(std::istream &ss) {
  cereal::JSONInputArchive iarchive(ss);

  MyClass m1, m2, m3;
  iarchive(m1, m2, m3);

  std::cout << m1.x << ' ' << m1.y << ' ' << m1.z << std::endl;
  std::cout << m2.x << ' ' << m2.y << ' ' << m2.z << std::endl;
  std::cout << m3.x << ' ' << m3.y << ' ' << m3.z << std::endl;
}

int main()
{
  std::stringstream s1;
  write_json(s1);
  std::cout << s1.str() << std::endl;

  std::stringstream s2;
  write_json(s2);
  read_json(s2);

  return 0;
}
