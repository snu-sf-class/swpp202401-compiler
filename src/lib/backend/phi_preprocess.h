#ifndef SC_BACKEND_PHI_PREPROCESS_H
#define SC_BACKEND_PHI_PREPROCESS_H

#include "llvm/IR/PassManager.h"

namespace sc::backend::phi_prep {
class PHIPreprocessPass : public llvm::PassInfoMixin<PHIPreprocessPass> {
public:
  llvm::PreservedAnalyses run(llvm::Module &, llvm::ModuleAnalysisManager &);
};
} // namespace sc::backend::phi_prep
#endif // SC_BACKEND_PHI_PREPROCESS_H
