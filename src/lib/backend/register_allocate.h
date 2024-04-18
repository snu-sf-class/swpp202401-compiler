#ifndef SC_BACKEND_REGISTER_ALLOCATE_H
#define SC_BACKEND_REGISTER_ALLOCATE_H

#include "const_map.h"
#include "symbol.h"
#include "llvm/IR/PassManager.h"

namespace sc::backend::reg_alloc {
class RegisterAllocatePass : public llvm::PassInfoMixin<RegisterAllocatePass> {
private:
  symbol::SymbolMap *SM;
  const_map::ConstMap *CM;

public:
  RegisterAllocatePass(symbol::SymbolMap &, const_map::ConstMap &);
  llvm::PreservedAnalyses run(llvm::Module &, llvm::ModuleAnalysisManager &);
};
} // namespace sc::backend::reg_alloc
#endif // SC_BACKEND_REGISTER_ALLOCATE_H
