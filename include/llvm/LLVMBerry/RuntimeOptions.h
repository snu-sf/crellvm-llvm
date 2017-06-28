#include "llvm/LLVMBerry/ValidationUnit.h"
#include "llvm/Support/CommandLine.h"
#include <string>
#include <vector>
#include <algorithm>
#include <iostream>

namespace llvmberry {

class RuntimeOptions {
public:
  static void Init(std::string outputDir, std::string whitelist,
                   std::string passwhitelist, bool compactjson,
                   bool nocommit) {
    _DefaultOutputDir = outputDir;
    _OptWhiteListEnabled = false;
    _OptPassWhiteListEnabled = false;
    _CompactJson = compactjson;
    _NoCommit = nocommit;
    if (!(whitelist == "-"))
      _SetWhiteList(whitelist);
    if (!(passwhitelist == "-"))
      _SetPassWhiteList(passwhitelist);
  }

  static bool UseCompactJson()
  { return _CompactJson; }
  static bool NoCommit()
  { return _NoCommit; }
  static bool IgnoreOpt(const std::string &optname) {
    return _OptWhiteListEnabled && 
      std::find(_OptWhiteList.begin(), _OptWhiteList.end(), optname) 
        == _OptWhiteList.end();
  }
  static bool IgnorePass(ValidationUnit::PASS curpass) {
    return _OptPassWhiteListEnabled &&
      std::find(_OptPassWhiteList.begin(), _OptPassWhiteList.end(), curpass)
        == _OptPassWhiteList.end();
  }
  static const std::string &GetDefaultOutputDir()
  { return _DefaultOutputDir; }

private:
  static std::string _DefaultOutputDir;
  static std::vector<std::string> _OptWhiteList;
  static bool _OptWhiteListEnabled;
  static std::vector<ValidationUnit::PASS> _OptPassWhiteList;
  static bool _OptPassWhiteListEnabled;
  static bool _CompactJson;
  static bool _NoCommit;

private:
  static void _SetWhiteList(const std::string &str) {
    std::stringstream ss(str);
    std::string optname;
    while (std::getline(ss, optname, ',')) {
      _OptWhiteList.push_back(optname);
    }
    _OptWhiteListEnabled = true;
  }

  static void _SetPassWhiteList(const std::string &str) {
    std::stringstream ss(str);
    std::string optname;
    while (std::getline(ss, optname, ',')) {
      std::transform(optname.begin(), optname.end(), optname.begin(), ::tolower);
      if (optname == "gvn")
        _OptPassWhiteList.push_back(ValidationUnit::GVN);
      else if (optname == "mem2reg")
        _OptPassWhiteList.push_back(ValidationUnit::MEM2REG);
      else if (optname == "pre")
        _OptPassWhiteList.push_back(ValidationUnit::PRE);
      else if (optname == "instcombine")
        _OptPassWhiteList.push_back(ValidationUnit::INSTCOMBINE);
      else if (optname == "licm")
        _OptPassWhiteList.push_back(ValidationUnit::LICM);
      else
        assert(false && "Invalid pass white list");
    }
    _OptPassWhiteListEnabled = true;
  }
};

}
