#ifndef SC_BACKEND_GEP_ELIMINATE_H
#define SC_BACKEND_GEP_ELIMINATE_H

#include "llvm/IR/PassManager.h"

namespace sc::backend::gep_elim {
class GEPEliminatePass : public llvm::PassInfoMixin<GEPEliminatePass> {
public:
  llvm::PreservedAnalyses run(llvm::Module &, llvm::ModuleAnalysisManager &);
};
} // namespace sc::backend::gep_elim
#endif // SC_BACKEND_GEP_ELIMINATE_H
