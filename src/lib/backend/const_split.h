#ifndef SC_BACKEND_CONST_SPLIT_H
#define SC_BACKEND_CONST_SPLIT_H

#include "llvm/IR/PassManager.h"

#include "const_map.h"

namespace sc::backend::const_split {
class ConstantSplitPass : public llvm::PassInfoMixin<ConstantSplitPass> {
private:
  const_map::ConstMap *CM;

public:
  ConstantSplitPass(const_map::ConstMap &);
  llvm::PreservedAnalyses run(llvm::Module &, llvm::ModuleAnalysisManager &);
};
} // namespace sc::backend::const_split
#endif // SC_BACKEND_CONST_SPLIT_H
