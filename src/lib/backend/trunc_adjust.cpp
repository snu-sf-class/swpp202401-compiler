#include "trunc_adjust.h"

#include "llvm/IR/Constants.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Support/Casting.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

using namespace llvm;

namespace sc::backend::trunc_adjust {
PreservedAnalyses TruncateAdjustPass::run(Module &M,
                                          ModuleAnalysisManager &MAM) {
  for (auto &F : M) {
    for (auto &BB : F) {
      for (auto &I : BB) {
        if (auto trunc_inst = llvm::dyn_cast<llvm::TruncInst>(&I)) {
          const auto afterBits = trunc_inst->getType()->getIntegerBitWidth();

          const auto mask = llvm::ConstantInt::get(
              trunc_inst->getOperand(0)->getType(), (1llu << afterBits) - 1);
          const auto bw_and = llvm::BinaryOperator::CreateAnd(
              trunc_inst->getOperand(0), mask, "", &I);
          trunc_inst->replaceUsesOfWith(trunc_inst->getOperand(0), bw_and);
        }
      }
    }
  }
  return PreservedAnalyses::all();
}
} // namespace sc::backend::trunc_adjust