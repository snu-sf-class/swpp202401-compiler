#ifndef SC_BACKEND_TRUNC_ADJUST_H
#define SC_BACKEND_TRUNC_ADJUST_H

#include "llvm/IR/PassManager.h"

namespace sc::backend::trunc_adjust {
class TruncateAdjustPass : public llvm::PassInfoMixin<TruncateAdjustPass> {
public:
  llvm::PreservedAnalyses run(llvm::Module &, llvm::ModuleAnalysisManager &);
};
} // namespace sc::backend::trunc_adjust
#endif // SC_BACKEND_TRUNC_ADJUST_H
