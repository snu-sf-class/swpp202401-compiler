#include "poison_eliminate.h"

#include "../../static_error.h"

#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Support/Casting.h"

#include <format>
#include <optional>

using namespace static_error;
namespace {
class InvalidTypeError : public Error<InvalidTypeError> {
public:
  const char *what() const noexcept {
    return "Poison elimination error: unsupported type";
  }
};
template <typename E>
class ErrorWithPoisonElim : public Error<ErrorWithPoisonElim<E>> {
private:
  std::string message;

public:
  ErrorWithPoisonElim(E &&__err, const llvm::PoisonValue &__poison) noexcept {
    std::string poison;
    llvm::raw_string_ostream rso(poison);
    rso << __poison;

    message = std::format("{}\n[{}]", __err.what(), poison);
  }

  const char *what() const noexcept { return message.c_str(); }
};
} // namespace

namespace sc::backend::poison_elim {
llvm::PreservedAnalyses
PoisonEliminatePass::run(llvm::Module &M, llvm::ModuleAnalysisManager &MAM) {
  for (auto &F : M) {
    for (auto &BB : F) {
      for (auto &I : BB) {
        for (size_t idx = 0; idx < I.getNumOperands(); idx++) {
          if (auto poison =
                  llvm::dyn_cast<llvm::PoisonValue>(I.getOperand(idx))) {
            std::optional<llvm::Value *> poison_repl;
            const auto poison_ty = poison->getType();
            if (const auto ptr_ty =
                    llvm::dyn_cast<llvm::PointerType>(poison_ty)) {
              poison_repl = llvm::ConstantPointerNull::get(ptr_ty);
            } else if (const auto int_ty =
                           llvm::dyn_cast<llvm::IntegerType>(poison_ty)) {
              poison_repl = llvm::ConstantInt::get(int_ty, 0);
            } else if (const auto vec_ty =
                           llvm::dyn_cast<llvm::VectorType>(poison_ty)) {
              poison_repl =
                  llvm::ConstantVector::getSplat(vec_ty->getElementCount(), 0);
            } else {
              throw ErrorWithPoisonElim(InvalidTypeError(), *poison);
            }
            I.replaceUsesOfWith(poison, *poison_repl);
          }
        }
      }
    }
  }
  return llvm::PreservedAnalyses::all();
}
} // namespace sc::backend::poison_elim
