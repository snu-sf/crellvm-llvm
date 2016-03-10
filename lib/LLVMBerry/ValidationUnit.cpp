#include "llvm/LLVMberry/ValidationUnit.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FileSystem.h"
#include <cassert>
#include <sstream>
#include <fstream>
#include <iostream>

namespace llvmberry {

void writeModuleToFile(const llvm::Module& module, const std::string& filename) {
  std::error_code errorInfo;
  llvm::StringRef filename_stref = filename.c_str();
  
  std::unique_ptr<llvm::tool_output_file> outputFile(new llvm::tool_output_file(filename_stref, errorInfo, llvm::sys::fs::OpenFlags::F_Text));
  // TODO: check OpenFlags
  
  if (errorInfo.value()!=0) { exit(1); }

  WriteBitcodeToFile(&module, outputFile->os());
  outputFile->keep();
}

bool inst_namer(llvm::Function &F) {
  for (llvm::Function::arg_iterator AI=F.arg_begin(), AE=F.arg_end(); AI!=AE; ++AI)
    if (!AI->hasName() && !AI->getType()->isVoidTy()) AI->setName("arg");

  for (llvm::Function::iterator BB=F.begin(), E=F.end(); BB!=E;++BB) {
    if(!BB->hasName()) BB->setName("bb");

    for (llvm::BasicBlock::iterator I=BB->begin(), E=BB->end(); I!=E; ++I)
      if (!I->hasName() && !I->getType()->isVoidTy()) I->setName("tmp");
  }

  return true;
}

int ValidationUnit::_counter = 0;
ValidationUnit *ValidationUnit::_instance = nullptr;

ValidationUnit *ValidationUnit::getInstance() {
  assert(ValidationUnit::_instance);
  return ValidationUnit::_instance;
}

ValidationUnit::ValidationUnit(const std::string& _optname, llvm::Function* func)
  : _filename(), _func(func), _hints(), _data() {
  // get module & module name
  const llvm::Module* module = func->getParent();
  std::string moduleName = module->getModuleIdentifier();
  moduleName = moduleName.substr(0, moduleName.find_last_of("."));

  // set filename prefix
  std::stringstream ss;
  ss << moduleName << '.' << func->getName().str() << '.' << ValidationUnit::_counter++;
  _filename = ss.str();

  // print src
  inst_namer(*func);
  writeModuleToFile(*module, _filename + ".src.bc");

  // set hints
  std::string mid = module->getModuleIdentifier();
  std::string fid = func->getName().str();
  _hints = structure::hints(fid, mid, _optname);

  // install this instance
  assert(ValidationUnit::_instance == nullptr);
  ValidationUnit::_instance = this;
}

void ValidationUnit::intrude(std::function<void(Dictionary &, structure::hints &)> func) {
  func(_data, _hints);
}

void ValidationUnit::commit() {
  // print tgt
  inst_namer(*_func);
  const llvm::Module* module = _func->getParent();
  writeModuleToFile(*module, _filename + ".tgt.bc");

  // print hints
  std::ofstream ofs(_filename + ".hint.json");
  cereal::JSONOutputArchive oarchive(ofs);
  _hints.serialize(oarchive);
  ofs << std::endl;

  uninstall();
}

void ValidationUnit::abort() {
  uninstall();
}

void ValidationUnit::uninstall() {
  assert(ValidationUnit::_instance == this);
  ValidationUnit::_instance = nullptr;
}

} // llvmberry
