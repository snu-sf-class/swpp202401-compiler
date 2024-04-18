#ifndef SC_BACKEND_SEXT_ELIMINATE_H
#define SC_BACKEND_SEXT_ELIMINATE_H

#include "llvm/IR/PassManager.h"

namespace sc::backend::sext_elim {
class SignExtendEliminatePass
    : public llvm::PassInfoMixin<SignExtendEliminatePass> {
public:
  llvm::PreservedAnalyses run(llvm::Module &, llvm::ModuleAnalysisManager &);
};
} // namespace sc::backend::sext_elim
#endif // SC_BACKEND_SEXT_ELIMINATE_H
