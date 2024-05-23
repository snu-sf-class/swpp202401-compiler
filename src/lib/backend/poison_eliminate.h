#ifndef SC_BACKEND_POISON_ELIMINATE_H
#define SC_BACKEND_POISON_ELIMINATE_H

#include "llvm/IR/PassManager.h"

namespace sc::backend::poison_elim {
class PoisonEliminatePass : public llvm::PassInfoMixin<PoisonEliminatePass> {
public:
  llvm::PreservedAnalyses run(llvm::Module &, llvm::ModuleAnalysisManager &);
};
} // namespace sc::backend::poison_elim
#endif // SC_BACKEND_POISON_ELIMINATE_H
