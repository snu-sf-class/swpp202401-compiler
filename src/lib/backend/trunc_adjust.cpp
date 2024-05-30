#include "trunc_adjust.h"

#include "llvm/IR/Constants.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Operator.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Support/Casting.h"

#include <utility>
#include <vector>

using namespace llvm;

namespace sc::backend::trunc_adjust {
PreservedAnalyses TruncateAdjustPass::run(Module &M,
                                          ModuleAnalysisManager &MAM) {
  std::vector<std::pair<llvm::TruncInst *, llvm::BinaryOperator *>>
      insert_pairs;
  for (auto &F : M) {
    for (auto &BB : F) {
      for (auto &I : BB) {
        if (auto trunc_inst = llvm::dyn_cast<llvm::TruncInst>(&I)) {
          const auto one = llvm::ConstantInt::get(trunc_inst->getType(), 1);
          const auto mov = llvm::BinaryOperator::CreateMul(trunc_inst, one);
          insert_pairs.push_back(std::make_pair(trunc_inst, mov));
        }
      }
    }
  }

  for (auto &[TI, mov] : insert_pairs) {
    mov->insertAfter(TI);
    TI->replaceUsesWithIf(mov,
                          [mov](auto &use) { return use.getUser() != mov; });
  }
  return PreservedAnalyses::all();
}
} // namespace sc::backend::trunc_adjust
