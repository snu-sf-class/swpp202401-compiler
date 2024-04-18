#ifndef SC_BACKEND_CONST_EXPR_ELIMINATE_H
#define SC_BACKEND_CONST_EXPR_ELIMINATE_H

#include "llvm/IR/PassManager.h"

namespace sc::backend::ce_elim {
class ConstExprEliminatePass
    : public llvm::PassInfoMixin<ConstExprEliminatePass> {
public:
  llvm::PreservedAnalyses run(llvm::Module &, llvm::ModuleAnalysisManager &);
};
} // namespace sc::backend::ce_elim
#endif // SC_BACKEND_CONST_EXPR_ELIMINATE_H
