#include <map>
#include <vector>
#include <iostream>
#include <set>
#include "../../../include/llvm/Hint/Intruder.h"
#include "../../../include/llvm/Hint/Sender.h"

int main() {
  HintGenerate intruder;

/*  
  / generate hints in lambda function & put hints into data
  intruder.intruder([&material1, &material2](HintGenerate::Dictionary &data) {
    PropagateInstr *pi = new PropagateInstr();
    pi->setXX(material1);
    data[??] = pi; //e.g. data["instr_propagate"] = pi;
  });
  intruder.flush(); //intruder::hints.serialize(archive); 
*/

  std::vector<int> vals;
  for (int i=0; i<10; ++i) {
    vals.push_back(i);
  }

  intruder.intrude([](HintGenerate::Dictionary &data) {
    data["precond"] = std::vector<int>();
  });

  intruder.intrude([](HintGenerate::Dictionary &data) {
    auto &precond = boost::any_cast<std::vector<int> &>(data["precond"]);
    precond.push_back(11);
  });

  intruder.intrude([&vals](HintGenerate::Dictionary &data) {
    auto &precond = boost::any_cast<std::vector<int> &>(data["precond"]);
    precond.push_back(vals[3]);
  });

  intruder.intrude([&vals](HintGenerate::Dictionary &data) {
    auto &precond = boost::any_cast<std::vector<int> &>(data["precond"]);
    precond.push_back(vals[7]);
  });

  intruder.intrude([](HintGenerate::Dictionary &data) {
    data["maydiff"] = std::set<int>();
  });

  intruder.intrude([&vals](HintGenerate::Dictionary &data) {
    auto &maydiff = boost::any_cast<std::set<int> &>(data["maydiff"]);
    maydiff.insert(vals[0]);
  });

  intruder.intrude([](HintGenerate::Dictionary &data) {
    auto &maydiff = boost::any_cast<std::set<int> &>(data["maydiff"]);
    maydiff.insert(11);
  });

  intruder.intrude([&vals](HintGenerate::Dictionary &data) {
    auto &maydiff = boost::any_cast<std::set<int> &>(data["maydiff"]);
    maydiff.insert(vals[7]);
  });

  intruder.intrude([](HintGenerate::Dictionary &data) {
    auto &maydiff = boost::any_cast<const std::set<int> &>(data["maydiff"]);
    auto &precond = boost::any_cast<const std::vector<int> &>(data["precond"]);

    for (auto i = maydiff.begin(); i != maydiff.end(); ++i) {
      std::cout << "maydiffs" << std::endl;
      std::cout << *i << std::endl;
    }

    for (auto j = precond.begin(); j != precond.end(); ++j) {
      std::cout << "preconds" << std::endl;
      std::cout << *j << std::endl;
    }
  });
}
