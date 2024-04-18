#ifndef SC_CHECK_ORACLE_H
#define SC_CHECK_ORACLE_H

#include "llvm/IR/PassManager.h"

namespace sc::backend::check_oracle {
class CheckOraclePass : public llvm::PassInfoMixin<CheckOraclePass> {
public:
  llvm::PreservedAnalyses run(llvm::Module &, llvm::ModuleAnalysisManager &);
};
} // namespace sc::backend::check_oracle
#endif // SC_CHECK_ORACLE_H
