#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"
#include <fstream>
#include <sstream>
#include <string>
#include <algorithm>
#include "llvm/LLVMBerry/ValidationUnit.h"

namespace {

static std::string makeFullFilename(std::string org_filename,
                                    std::string extension) {
  if (llvmberry::defaultOutputDir.empty())
    return org_filename + extension;
  else {
    if (org_filename.rfind("/") == std::string::npos)
      return llvmberry::defaultOutputDir + "/" + org_filename + extension;
    else
      return llvmberry::defaultOutputDir + "/" +
             org_filename.substr(org_filename.rfind("/")) + extension;
  }
}

} // anonymous

namespace llvmberry {

std::string defaultOutputDir = "";
std::vector<std::string> optWhiteList;
bool optWhiteListEnabled = false;
std::vector<ValidationUnit::PASS> optPassWhiteList;
bool optPassWhiteListEnabled = false;

void setWhiteList(const std::string &str) {
  std::stringstream ss(str);
  std::string optname;
  while (std::getline(ss, optname, ',')) {
    optWhiteList.push_back(optname);
  }
  optWhiteListEnabled = true;
}

void setPassWhiteList(const std::string &str) {
  std::stringstream ss(str);
  std::string optname;
  while (std::getline(ss, optname, ',')) {
    std::transform(optname.begin(), optname.end(), optname.begin(), ::tolower);
    if (optname == "gvn")
      optPassWhiteList.push_back(ValidationUnit::GVN);
    else if (optname == "mem2reg")
      optPassWhiteList.push_back(ValidationUnit::MEM2REG);
    else if (optname == "pre")
      optPassWhiteList.push_back(ValidationUnit::PRE);
    else if (optname == "instcombine")
      optPassWhiteList.push_back(ValidationUnit::INSTCOMBINE);
    else
      assert(false && "Invalid pass white list");
  }
  optPassWhiteListEnabled = true;
}

void writeModuleToBuffer(const llvm::Module &module, std::string *buffer) {
  llvm::raw_string_ostream strstream(*buffer);
  WriteBitcodeToFile(&module, strstream);
}

void writeModuleToFile(const llvm::Module &module,
                       const std::string &filename) {
  std::error_code errorInfo;
  llvm::StringRef filename_stref = filename.c_str();

  std::unique_ptr<llvm::tool_output_file> outputFile(new llvm::tool_output_file(
      filename_stref, errorInfo, llvm::sys::fs::OpenFlags::F_Text));

  if (errorInfo.value() != 0) {
    exit(1);
  }

  WriteBitcodeToFile(&module, outputFile->os());
  outputFile->keep();
}

// class ValidationUnit
// constructor
ValidationUnit::ValidationUnit(const std::string &optname, llvm::Function *func)
    : _filename(), _optname(optname), _srcfile_buffer(nullptr), _func(func),
      _corehint(), _data(), isAborted(false) {
  if (optWhiteListEnabled && 
      std::find(optWhiteList.begin(), optWhiteList.end(), optname) == 
        optWhiteList.end()) {
    this->isAborted = true;
  } else if (optPassWhiteListEnabled &&
      std::find(optPassWhiteList.begin(), optPassWhiteList.end(), _CurrentPass) 
        == optPassWhiteList.end()) {
    this->isAborted = true;
  } else {
    this->begin();
  }
}

// destructor
ValidationUnit::~ValidationUnit() {
  if (!isAborted && !_optname.compare("mem2reg"))
    this->commit();
  else
    this->abort();
  if (_srcfile_buffer != nullptr)
    delete _srcfile_buffer;
}

// public functions
const std::string &ValidationUnit::getOptimizationName() const {
  return _Instance->_optname;
}

const std::string &ValidationUnit::getDescription() const {
  return _Instance->_corehint.getDescription();
}

void ValidationUnit::setOptimizationName(const std::string &str) {
  _optname = str;
  _Instance->_corehint.setOptimizationName(str);
}

void ValidationUnit::setDescription(const std::string &str) {
  _Instance->_corehint.setDescription(str);
}

void ValidationUnit::setIsAborted() {
  isAborted = true;
}

void ValidationUnit::intrude(
    std::function<void(Dictionary &, CoreHint &)> func) {
  if (isAborted || _optname.compare("mem2reg"))
    return;
  if (_corehint.getReturnCode() != CoreHint::ACTUAL)
    return;
  func(_data, _corehint);
}

// private functions
void ValidationUnit::begin() {
  assert(!isAborted);

  // get module & module name
  const llvm::Module *module = _func->getParent();
  std::string moduleName = module->getModuleIdentifier();
  moduleName = moduleName.substr(0, moduleName.find_last_of("."));

  // set filename prefix
  std::stringstream ss;
  ss << moduleName << '.' << _func->getName().str() << '.'
     << ValidationUnit::_Counter++;
  _filename = ss.str();

  // print src
  llvmberry::name_instructions(*_func);
  _srcfile_buffer = new std::string();
  if (_optname.compare("mem2reg")) {}
  else
  writeModuleToBuffer(*module, _srcfile_buffer);
  // writeModuleToFile(*module, makeFullFilename(_filename, ".src.bc.org"));

  // set corehints
  std::string mid = module->getModuleIdentifier();
  std::string fid = _func->getName().str();
  _corehint = CoreHint(mid, fid, _optname);
}

void ValidationUnit::commit() {
  assert(!isAborted);

  // print src
  std::ofstream src_ofs(makeFullFilename(_filename, ".src.bc"), std::ios::out);
  src_ofs << *_srcfile_buffer;
  src_ofs.close();
  if (src_ofs.fail() || src_ofs.bad()) {
    std::cerr
        << "LLVMBerry : ValidationUnit::commit() : Fail to write source file"
        << std::endl;
    std::exit(-10);
  }

  // print tgt
  llvmberry::name_instructions(*_func);
  const llvm::Module *module = _func->getParent();
  std::string tgt_ss;
  writeModuleToBuffer(*module, &tgt_ss);
  std::ofstream tgt_ofs(makeFullFilename(_filename, ".tgt.bc"));
  tgt_ofs << tgt_ss;
  tgt_ofs.close();
  if (tgt_ofs.fail() || tgt_ofs.bad()) {
    std::cerr
        << "LLVMBerry : ValidationUnit::commit() : Fail to write target file"
        << std::endl;
    std::exit(-11);
  }

  // print corehints
  std::ofstream ofs(makeFullFilename(_filename, ".hint.json"));
  cereal::JSONOutputArchive oarchive(ofs);
  _corehint.serialize(oarchive);
  ofs << std::endl;
  if (ofs.fail() || ofs.bad()) {
    std::cerr
        << "LLVMBerry : ValidationUnit::commit() : Fail to write core hint file"
        << std::endl;
    std::exit(-12);
  }
}

void ValidationUnit::abort() {}

// static members
ValidationUnit *ValidationUnit::_Instance = nullptr;
ValidationUnit::PASS ValidationUnit::_CurrentPass = ValidationUnit::NOTHING;
int ValidationUnit::_Counter = 0;

ValidationUnit *ValidationUnit::GetInstance() {
  assert(Exists() && "No ValidationUnit exists");
  return _Instance;
}

void ValidationUnit::StartPass(PASS pass) {
  assert(_CurrentPass == NOTHING);
  _CurrentPass = pass;
  PassDictionary::Create();
}

void ValidationUnit::EndPass() {
  _CurrentPass = NOTHING;
  PassDictionary::Destroy();
}

bool ValidationUnit::Exists() {
  if (_Instance)
    return true;
  else
    return false;
}

void ValidationUnit::Begin(const std::string &optname, llvm::Function *func) {
  assert(!Exists() && "ValidationUnit already exists");
  assert(func && "Function cannot be null");
  _Instance = new ValidationUnit(optname, func);
}

bool ValidationUnit::BeginIfNotExists(const std::string &optname,
                                      llvm::Function *func) {
  if (Exists())
    return false;
  _Instance = new ValidationUnit(optname, func);
  return true;
}

void ValidationUnit::End() {
  assert(Exists() && "No ValidationUnit exists");
  delete _Instance;
  _Instance = nullptr;
}

void ValidationUnit::Abort() {
  _Instance->setIsAborted();
  End();
}

bool ValidationUnit::EndIfExists() {
  if (!Exists())
    return false;
  delete _Instance;
  _Instance = nullptr;
  return true;
}

} // llvmberry
