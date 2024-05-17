#ifndef SC_BACKEND_CONST_RESOLVE_H
#define SC_BACKEND_CONST_RESOLVE_H

#include <unordered_map>

#include "assembly.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instruction.h"

namespace sc::backend::const_map {
using FnConstMap = std::unordered_map<llvm::Constant *, llvm::Instruction *>;
using ModuleConstMap = std::unordered_map<llvm::Function *, FnConstMap>;

class ConstMap {
private:
  ModuleConstMap map;

public:
  llvm::Instruction *resolve_constant(llvm::Function *F, llvm::IntegerType *ty,
                                      assembly::IntTy value,
                                      llvm::Instruction *insert_before);
  llvm::Instruction *resolve_constant(llvm::Function *F, llvm::PointerType *pty,
                                      assembly::IntTy value,
                                      llvm::Instruction *__insert_before);
  llvm::Instruction *resolve_constant(llvm::Function *F, llvm::Constant *cst,
                                      llvm::Instruction *insert_before);
};
} // namespace sc::backend::const_map
#endif // SC_BACKEND_CONST_RESOLVE_H
