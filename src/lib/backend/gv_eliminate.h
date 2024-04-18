#ifndef SC_BACKEND_GV_ELIMINATE_H
#define SC_BACKEND_GV_ELIMINATE_H

#include "llvm/IR/PassManager.h"

namespace sc::backend::gv_elim {
class GVEliminatePass : public llvm::PassInfoMixin<GVEliminatePass> {
public:
  llvm::PreservedAnalyses run(llvm::Module &, llvm::ModuleAnalysisManager &);
};
} // namespace sc::backend::gv_elim
#endif // SC_BACKEND_GV_ELIMINATE_H
