#ifndef SC_BACKEND_UNDEF_ELIMINATE_H
#define SC_BACKEND_UNDEF_ELIMINATE_H

#include "llvm/IR/PassManager.h"

namespace sc::backend::undef_elim {
class UndefEliminatePass : public llvm::PassInfoMixin<UndefEliminatePass> {
public:
  llvm::PreservedAnalyses run(llvm::Module &, llvm::ModuleAnalysisManager &);
};
} // namespace sc::backend::undef_elim
#endif // SC_BACKEND_UNDEF_ELIMINATE_H
