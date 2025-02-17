#ifndef SC_BACKEND_FREEZE_ELIMINATE_H
#define SC_BACKEND_FREEZE_ELIMINATE_H

#include "llvm/IR/PassManager.h"

namespace sc::backend::freeze_elim {
class FreezeEliminatePass : public llvm::PassInfoMixin<FreezeEliminatePass> {
public:
  llvm::PreservedAnalyses run(llvm::Module &, llvm::ModuleAnalysisManager &);
};
} // namespace sc::backend::freeze_elim
#endif // SC_BACKEND_FREEZE_ELIMINATE_H
