#include <map>
#include <string>
#include <functional>
#include <set>
#include <iostream>
#include <boost/any.hpp>

class Intruder {
 public:
  typedef std::map<std::string, boost::any> Dictionary;
 
  void intrude(std::function<void(Dictionary &)> func) {
    func(_data);
  }
 private:
  Dictionary _data;
};
