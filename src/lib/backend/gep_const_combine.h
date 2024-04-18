#ifndef SC_BACKEND_GEP_CONST_COMBINE_H
#define SC_BACKEND_GEP_CONST_COMBINE_H

#include "llvm/IR/PassManager.h"

namespace sc::backend::gc_comb {
class GEPConstCombinePass : public llvm::PassInfoMixin<GEPConstCombinePass> {
public:
  llvm::PreservedAnalyses run(llvm::Module &, llvm::ModuleAnalysisManager &);
};
} // namespace sc::backend::gc_comb
#endif // SC_BACKEND_GEP_CONST_COMBINE_H
