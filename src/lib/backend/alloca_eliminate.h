#ifndef SC_BACKEND_ALLOCA_ELIMINATE_H
#define SC_BACKEND_ALLOCA_ELIMINATE_H

#include "llvm/IR/PassManager.h"

namespace sc::backend::alloca_elim {
class AllocaEliminatePass : public llvm::PassInfoMixin<AllocaEliminatePass> {
public:
  llvm::PreservedAnalyses run(llvm::Module &, llvm::ModuleAnalysisManager &);
};
} // namespace sc::backend::alloca_elim
#endif // SC_BACKEND_ALLOCA_ELIMINATE_H
