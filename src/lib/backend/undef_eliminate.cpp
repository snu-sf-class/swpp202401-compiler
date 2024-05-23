#include "undef_eliminate.h"

#include "../../static_error.h"

#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Casting.h"

#include <format>
#include <optional>

using namespace static_error;
namespace {
class InvalidTypeError : public Error<InvalidTypeError> {
public:
  const char *what() const noexcept {
    return "Undef elimination error: unsupported type";
  }
};
template <typename E>
class ErrorWithUndefElim : public Error<ErrorWithUndefElim<E>> {
private:
  std::string message;

public:
  ErrorWithUndefElim(E &&__err, const llvm::UndefValue &__undef) noexcept {
    std::string undef;
    llvm::raw_string_ostream rso(undef);
    rso << __undef;

    message = std::format("{}\n[{}]", __err.what(), undef);
  }

  const char *what() const noexcept { return message.c_str(); }
};
} // namespace

namespace sc::backend::undef_elim {
llvm::PreservedAnalyses
UndefEliminatePass::run(llvm::Module &M, llvm::ModuleAnalysisManager &MAM) {
  for (auto &F : M) {
    for (auto &BB : F) {
      for (auto &I : BB) {
        for (size_t idx = 0; idx < I.getNumOperands(); idx++) {
          if (auto undef =
                  llvm::dyn_cast<llvm::UndefValue>(I.getOperand(idx))) {
            std::optional<llvm::Value *> undef_repl;
            const auto undef_ty = undef->getType();
            if (const auto ptr_ty =
                    llvm::dyn_cast<llvm::PointerType>(undef_ty)) {
              undef_repl = llvm::ConstantPointerNull::get(ptr_ty);
            } else if (const auto int_ty =
                           llvm::dyn_cast<llvm::IntegerType>(undef_ty)) {
              undef_repl = llvm::ConstantInt::get(int_ty, 0);
            } else if (const auto vec_ty =
                           llvm::dyn_cast<llvm::VectorType>(undef_ty)) {
              undef_repl =
                  llvm::ConstantVector::getSplat(vec_ty->getElementCount(), 0);
            } else {
              throw ErrorWithUndefElim(InvalidTypeError(), *undef);
            }
            I.replaceUsesOfWith(undef, *undef_repl);
          }
        }
      }
    }
  }
  return llvm::PreservedAnalyses::all();
}
} // namespace sc::backend::undef_elim
