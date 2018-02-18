#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"
#include <fstream>
#include <sstream>
#include <string>
#include <algorithm>
#include "llvm/Crellvm/ValidationUnit.h"
#include "llvm/Crellvm/RuntimeOptions.h"

////// Crellvm option
llvm::cl::opt<std::string>
    CrellvmOutputDirectory("crellvm-outputdir",
       llvm::cl::desc("Specify output directory of Crellvm"),
       llvm::cl::value_desc("dir"));

static llvm::cl::opt<std::string>
    CrellvmWhiteList("crellvm-whitelist",
       llvm::cl::desc("Enable hint generation for specific "
                "optimizations only"),
       llvm::cl::value_desc("opt-names(use commas)"),
       llvm::cl::init("-"));

static llvm::cl::opt<std::string>
    CrellvmPassWhiteList("crellvm-passwhitelist",
       llvm::cl::desc("Enable hint generation for specific "
                "passes only (instcombine|mem2reg|gvn|pre|licm)"),
       llvm::cl::value_desc("pass-names(use commas)"),
       llvm::cl::init("-"));

static llvm::cl::opt<bool>
    CrellvmCompactJson("crellvm-compactjson",
        llvm::cl::desc("Use compact json for hint"));

static llvm::cl::opt<bool>
    CrellvmNoCommit("crellvm-nocommit",
        llvm::cl::desc("Use compact json for hint"));


namespace crellvm {

std::string RuntimeOptions::_DefaultOutputDir;
std::vector<std::string> RuntimeOptions::_OptWhiteList;
bool RuntimeOptions::_OptWhiteListEnabled = false;
std::vector<ValidationUnit::PASS> RuntimeOptions::_OptPassWhiteList;
bool RuntimeOptions::_OptPassWhiteListEnabled = false;
bool RuntimeOptions::_CompactJson = false;
bool RuntimeOptions::_NoCommit = false;

std::string makeFullFilename(std::string org_filename,
                                    std::string extension) {
  std::string defaultOutputDir = RuntimeOptions::GetDefaultOutputDir();
  if (defaultOutputDir.empty())
    return org_filename + extension;
  else {
    if (org_filename.rfind("/") == std::string::npos)
      return defaultOutputDir + "/" + org_filename + extension;
    else
      return defaultOutputDir + "/" +
             org_filename.substr(org_filename.rfind("/")) + extension;
  }
}

void writeModuleToBuffer(const llvm::Module &module, std::string *buffer,
    llvm::Function *F) {
  llvm::raw_string_ostream strstream(*buffer);
  WriteBitcodeToFile(&module, strstream, false, F);
}

void writeModuleToFile(const llvm::Module &module,
                       const std::string &filename,
                       llvm::Function *F) {
  std::error_code errorInfo;
  llvm::StringRef filename_stref = filename.c_str();

  std::unique_ptr<llvm::tool_output_file> outputFile(new llvm::tool_output_file(
      filename_stref, errorInfo, llvm::sys::fs::OpenFlags::F_Text));

  if (errorInfo.value() != 0) {
    exit(1);
  }

  WriteBitcodeToFile(&module, outputFile->os(), false, F);
  outputFile->keep();
}

// class ValidationUnit
// constructor
ValidationUnit::ValidationUnit(const std::string &optname, llvm::Function *func, bool rename)
    : _filename(), _optname(optname), _srcfile_buffer(nullptr), _func(func),
      _corehint(), _data(), isAborted(false) {
  if (RuntimeOptions::IgnoreOpt(optname))
     this->isAborted = true;
  else if (RuntimeOptions::IgnorePass(_CurrentPass)) {
    this->isAborted = true;
  } else {
    this->begin(rename);
  }
}

// destructor
ValidationUnit::~ValidationUnit() {
  if (!isAborted)
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
  if (isAborted)
    return;
  if (_corehint.getReturnCode() != CoreHint::ACTUAL)
    return;
  func(_data, _corehint);
}

static CoreHint::AUTO_OPT getAutoOptOf(ValidationUnit::PASS pass) {
  switch (pass) {
  case ValidationUnit::GVN:
  case ValidationUnit::PRE:
    return CoreHint::AUTO_GVN;
  case ValidationUnit::MEM2REG:
    return CoreHint::AUTO_SROA;
  case ValidationUnit::INSTCOMBINE:
    return CoreHint::AUTO_INSTCOMBINE;
  case ValidationUnit::LICM:
    return CoreHint::AUTO_LICM;
  default:
    return CoreHint::AUTO_DEFAULT;
  }
}

static std::shared_ptr<TyPostpropInfo> getPostpropOptOf(ValidationUnit::PASS pass) {
  static auto _forGVN = TyPostpropInfo::make(TyPostpropInfo::POSTPROP_GVN, 99999999);
  static auto _NONE = TyPostpropInfo::make(TyPostpropInfo::POSTPROP_NONE, 0);
  
  switch (pass) {
  case ValidationUnit::GVN:
  case ValidationUnit::PRE:
    return _forGVN;
  default:
    return _NONE;
  }
}

// private functions
void ValidationUnit::begin(bool rename) {
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
  if (rename)
    crellvm::name_instructions(*_func);
  _srcfile_buffer = new std::string();
  if (!RuntimeOptions::NoCommit())
    writeModuleToBuffer(*module, _srcfile_buffer, _func);
  // writeModuleToFile(*module, makeFullFilename(_filename, ".src.bc.org"));

  // set corehints
  std::string mid = module->getModuleIdentifier();
  std::string fid = _func->getName().str();
  _corehint = CoreHint(mid, fid, _optname);
  _corehint.setAutoOption(getAutoOptOf(_CurrentPass));
  _corehint.setPostpropOption(getPostpropOptOf(_CurrentPass));
}

void ValidationUnit::commit() {
  assert(!isAborted);
  
  if (RuntimeOptions::NoCommit())
    return;
  
  // print src
  std::ofstream src_ofs(makeFullFilename(_filename, ".src.bc"), std::ios::out);
  src_ofs << *_srcfile_buffer;
  src_ofs.close();
  if (src_ofs.fail() || src_ofs.bad()) {
    std::cerr
        << "Crellvm : ValidationUnit::commit() : Fail to write source file"
        << std::endl;
    std::exit(-10);
  }

  // print tgt
  const llvm::Module *module = _func->getParent();
  std::string tgt_ss;
  writeModuleToBuffer(*module, &tgt_ss, _func);
  std::ofstream tgt_ofs(makeFullFilename(_filename, ".tgt.bc"));
  tgt_ofs << tgt_ss;
  tgt_ofs.close();
  if (tgt_ofs.fail() || tgt_ofs.bad()) {
    std::cerr
        << "Crellvm : ValidationUnit::commit() : Fail to write target file"
        << std::endl;
    std::exit(-11);
  }

  // print corehints
  std::ofstream ofs(makeFullFilename(_filename, ".hint.json"));
  cereal::JSONOutputArchive::Options jsonoption = 
      RuntimeOptions::UseCompactJson() ?
        cereal::JSONOutputArchive::Options::NoIndent() : 
        cereal::JSONOutputArchive::Options::Default();
  cereal::JSONOutputArchive oarchive(ofs, jsonoption);
  _corehint.serialize(oarchive);
  ofs << std::endl;
  if (ofs.fail() || ofs.bad()) {
    std::cerr
        << "Crellvm : ValidationUnit::commit() : Fail to write core hint file"
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

  // JYLEE : I put this code here because I could't find where to place this
  // kind of 'global Crellvm-beginning' code. each of `opt` and `clang` has
  // own main(), and it is too burdensome to place this RuntimeOptions::Init in
  // both main() functions considering that we're maintaining only llvm repo,
  // not clang repo..
  RuntimeOptions::Init(CrellvmOutputDirectory, CrellvmWhiteList,
                            CrellvmPassWhiteList, CrellvmCompactJson,
                            CrellvmNoCommit);
}

ValidationUnit::PASS ValidationUnit::GetCurrentPass() {
  return ValidationUnit::_CurrentPass;
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

void ValidationUnit::Begin(const std::string &optname, llvm::Instruction &I, bool rename) {
  ValidationUnit::Begin(optname, I.getParent()->getParent(), rename);
}

void ValidationUnit::Begin(const std::string &optname, llvm::Function *func, bool rename) {
  if (Exists()) {
    std::cout << "ValidationUnit already exists!! : " << GetInstance()->getOptimizationName() << std::endl;
    assert(!Exists() && "ValidationUnit already exists");
  }
  assert(func && "Function cannot be null");
  _Instance = new ValidationUnit(optname, func, rename);
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

} // crellvm
