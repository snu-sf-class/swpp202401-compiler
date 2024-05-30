#include "const_split.h"

#include "arch.h"
#include "const_map.h"

#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/Casting.h"

#include <string_view>

using namespace llvm;
using namespace static_error;
using namespace std::string_view_literals;

namespace sc::backend::const_split {
ConstantSplitPass::ConstantSplitPass(const_map::ConstMap &__CM) : CM(&__CM) {}

class InvalidConstantError : public static_error::Error<InvalidConstantError> {
public:
  const char *what() const noexcept {
    return "Invalid constant error: this constant cannot be used";
  }
};

PreservedAnalyses ConstantSplitPass::run(Module &M,
                                         ModuleAnalysisManager &MAM) {
  llvm::IntegerType *Int64Ty = llvm::Type::getInt64Ty(M.getContext());

  for (auto &F : M) {
    if (F.isDeclaration()) {
      continue;
    }

    for (auto &BB : F) {
      for (auto &I : BB) {
        if (auto switch_inst = llvm::dyn_cast<llvm::SwitchInst>(&I)) {
          if (auto const_switch_cond = llvm::dyn_cast<llvm::ConstantInt>(
                  switch_inst->getCondition())) {
            const auto CI = CM->resolve_constant(&F, const_switch_cond, &I);
            switch_inst->setCondition(CI);
          } else if (auto const_switch_vec_cond =
                         llvm::dyn_cast<llvm::ConstantDataVector>(
                             switch_inst->getCondition())) {
            const auto CI = CM->resolve_constant(&F, const_switch_cond, &I);
            switch_inst->setCondition(CI);
          }
          continue;
        }

        if (auto call_inst = llvm::dyn_cast<llvm::CallInst>(&I)) {
          if (call_inst->getCalledFunction()->getName().equals("const_i64"sv)) {
            continue;
          }
        }

        for (size_t i = 0; i < I.getNumOperands(); i++) {
          Constant *const_operand = nullptr;
          if (const auto null_operand =
                  llvm::dyn_cast<llvm::ConstantPointerNull>(I.getOperand(i))) {
            const_operand = llvm::ConstantInt::get(
                llvm::IntegerType::getInt64Ty(I.getContext()), NULL_PTR);
          } else if (const auto vector_operand =
                         llvm::dyn_cast<llvm::ConstantDataVector>(
                             I.getOperand(i))) {
            const_operand = vector_operand;
          } else {
            const_operand = llvm::dyn_cast<llvm::ConstantInt>(I.getOperand(i));
          }

          if (const_operand) {
            const auto CI = CM->resolve_constant(&F, const_operand, &I);
            I.replaceUsesOfWith(I.getOperand(i), CI);
          }
        }
      }
    }
  }
  return PreservedAnalyses::all();
}
} // namespace sc::backend::const_split
