#include <map>
#include <string>
#include <functional>
#include <set>
#include <iostream>
#include <boost/any.hpp>
#include "llvm/Hint/Sender.h"

class HintGenerate {
public:
  typedef std::map<std::string, boost::any> Dictionary;
 
  void intrude(std::function<void(Dictionary &)> func) {
    func(_data);
  }
  Hints *release()
  { Hints *t = workingptr; this->workingptr = nullptr; this->_data.clear(); return t; }

private:
  Dictionary _data;
  Hints *workingptr;

};

class HintFactory {

};
