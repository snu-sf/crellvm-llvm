#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"
#include <fstream>
#include "llvm/LLVMBerry/ValidationUnit.h"

void boost::throw_exception(std::exception const &e) {
  // This function is called whenever exception is "expected" in the ValidationUnit library."
  std::cout << "boost throw_exception" << std::endl;
}

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

void writeModuleToBuffer(const llvm::Module &module,
                         std::string *buffer) {
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
    : _filename(), _optname(optname), _func(func), _corehint(), _data(),
      _return_code(COMMIT) {
  this->begin();
}

// destructor
ValidationUnit::~ValidationUnit() {
  switch (_return_code) {
  case COMMIT:
    this->commit();
    break;
  case ABORT:
    this->abort();
    break;
  default:
    assert(false && "Not a possible return code");
    break;
  }
  delete _srcfile_buffer;
}

// public functions
const std::string &ValidationUnit::getOptimizationName() {
  return _Instance->_optname;
}

void ValidationUnit::setReturnCode(RETURN_CODE return_code) {
  _return_code = return_code;
}

void ValidationUnit::intrude(
    std::function<void(Dictionary &, CoreHint &)> func) {
  func(_data, _corehint);
}

// private functions
void ValidationUnit::begin() {
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
  writeModuleToBuffer(*module, _srcfile_buffer);
  //writeModuleToFile(*module, makeFullFilename(_filename, ".src.bc.org"));

  // set corehints
  std::string mid = module->getModuleIdentifier();
  std::string fid = _func->getName().str();
  _corehint = CoreHint(mid, fid, _optname);
}

void ValidationUnit::commit() {
  // print src
  std::ofstream src_ofs(makeFullFilename(_filename, ".src.bc"), std::ios::out);
  src_ofs << *_srcfile_buffer;
  src_ofs.close();

  // print tgt
  llvmberry::name_instructions(*_func);
  const llvm::Module *module = _func->getParent();
  writeModuleToFile(*module, makeFullFilename(_filename, ".tgt.bc"));

  // print corehints
  std::ofstream ofs(makeFullFilename(_filename, ".hint.json"));
  cereal::JSONOutputArchive oarchive(ofs);
  _corehint.serialize(oarchive);
  ofs << std::endl;
}

void ValidationUnit::abort() {}

// static members
ValidationUnit *ValidationUnit::_Instance = nullptr;

int ValidationUnit::_Counter = 0;

ValidationUnit *ValidationUnit::GetInstance() {
  assert(Exists() && "No ValidationUnit exists");
  return _Instance;
}

bool ValidationUnit::Exists() {
  if (_Instance)
    return true;
  else
    return false;
}

void ValidationUnit::Begin(const std::string &optname, llvm::Function *func) {
  assert(!Exists() && "ValidationUnit already exists");
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

bool ValidationUnit::EndIfExists() {
  if (!Exists())
    return false;
  delete _Instance;
  _Instance = nullptr;
  return true;
}

} // llvmberry
