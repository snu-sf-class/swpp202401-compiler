#include "sext_eliminate.h"
#include "../../static_error.h"

#include "llvm/IR/Constants.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Support/Casting.h"

using namespace llvm;
using namespace static_error;

namespace sc::backend::sext_elim {

class InvalidConstantError : public static_error::Error<InvalidConstantError> {
public:
  const char *what() const noexcept {
    return "Invalid constant error: this constant cannot be used";
  }
};

PreservedAnalyses SignExtendEliminatePass::run(Module &M,
                                               ModuleAnalysisManager &MAM) {
  for (auto &F : M) {
    for (auto &BB : F) {
      for (auto &I : BB) {
        if (auto sext_inst = llvm::dyn_cast<llvm::SExtInst>(&I)) {
          const auto beforeBits =
              sext_inst->getOperand(0)->getType()->getIntegerBitWidth();
          const auto afterBits = sext_inst->getType()->getIntegerBitWidth();

          const auto val_extended = llvm::ZExtInst::CreateZExtOrBitCast(
              sext_inst->getOperand(0), sext_inst->getType(), "", &I);
          const auto mask = llvm::ConstantInt::get(
              sext_inst->getType(), 1llu << (afterBits - beforeBits));
          const auto shl =
              llvm::BinaryOperator::CreateMul(val_extended, mask, "", &I);
          const auto ashr = llvm::BinaryOperator::CreateSDiv(shl, mask, "", &I);
          sext_inst->replaceAllUsesWith(ashr);
        }
      }
    }
  }
  return PreservedAnalyses::all();
}
} // namespace sc::backend::sext_elim