#include "freeze_eliminate.h"

#include "llvm/IR/Instructions.h"

#include <set>

namespace sc::backend::freeze_elim {
llvm::PreservedAnalyses
FreezeEliminatePass::run(llvm::Module &M, llvm::ModuleAnalysisManager &MAM) {
  llvm::IntegerType *Int64Ty = llvm::Type::getInt64Ty(M.getContext());
  std::set<llvm::FreezeInst *> trashBin;
  for (llvm::Function &F : M) {
    trashBin.clear();
    for (llvm::BasicBlock &BB : F) {
      for (llvm::Instruction &I : BB) {
        if (auto *FI = llvm::dyn_cast<llvm::FreezeInst>(&I)) {
          FI->replaceAllUsesWith(FI->getOperand(0));
          trashBin.insert(FI);
        }
      }
    }
    for (auto *I : trashBin) {
      I->eraseFromParent();
    }
  }
  return llvm::PreservedAnalyses::all();
}
} // namespace sc::backend::freeze_elim
