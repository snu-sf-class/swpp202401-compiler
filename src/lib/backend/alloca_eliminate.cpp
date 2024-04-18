#include "alloca_eliminate.h"

#include "analysis.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"

#include <concepts>
#include <expected>

using namespace std::string_literals;
using namespace static_error;

namespace {
class NonStaticAllocaError : public Error<NonStaticAllocaError> {
public:
  const char *what() const noexcept {
    return "alloca elimination error: alloca is not static";
  }
};

template <typename E> class ErrorWithAlloca : public Error<ErrorWithAlloca<E>> {
private:
  std::string message;

public:
  ErrorWithAlloca(E &&__err, const llvm::AllocaInst &__alloca) noexcept {
    std::string alloca;
    llvm::raw_string_ostream rso(alloca);
    rso << __alloca;

    message =
        std::string(__err.what()).append("\n["s).append(alloca).append("]"s);
  }

  const char *what() const noexcept { return message.c_str(); }
};

template <std::movable T, typename E>
T unwrapOrThrowWithAlloca(std::expected<T, E> &&__res,
                          const llvm::AllocaInst &__alloca) {
  using ResType = std::expected<T, E>;

  if (!__res.has_value()) {
    auto err = __res.error();
    throw ErrorWithAlloca(std::move(err), __alloca);
  }

  return __res.value();
}
} // namespace

namespace sc::backend::alloca_elim {
llvm::PreservedAnalyses
AllocaEliminatePass::run(llvm::Module &M, llvm::ModuleAnalysisManager &MAM) {
  llvm::IntegerType *Int64Ty = llvm::Type::getInt64Ty(M.getContext());
  llvm::FunctionCallee decr_sp =
      M.getOrInsertFunction("$decr_sp", Int64Ty, Int64Ty);
  std::vector<llvm::Instruction *> trashBin;

  for (llvm::Function &F : M) {
    uint64_t acc = 0UL;
    for (llvm::BasicBlock &BB : F)
      for (llvm::Instruction &I : BB) {
        if (llvm::AllocaInst *AI = llvm::dyn_cast<llvm::AllocaInst>(&I)) {
          if (!AI->isStaticAlloca()) {
            throw ErrorWithAlloca(NonStaticAllocaError(), *AI);
          }
          const auto num_elems =
              llvm::dyn_cast<llvm::ConstantInt>(AI->getArraySize())
                  ->getZExtValue();
          const auto req_size =
              unwrapOrThrowWithAlloca(
                  analysis::tryCalculateSize(AI->getAllocatedType()), *AI) *
              num_elems;
          const auto alloc_size = (req_size + 7UL) & (UINT64_MAX - 7UL);
          acc += alloc_size;
        }
      }

    if (acc) {
      llvm::Instruction *FI = F.getEntryBlock().getFirstNonPHI();
      llvm::ConstantInt *Const = llvm::ConstantInt::get(Int64Ty, acc, true);
      llvm::Value *Args[] = {Const};
      llvm::CallInst *CI = llvm::CallInst::Create(
          decr_sp, llvm::ArrayRef<llvm::Value *>(Args), "", FI);
      acc = 0UL;
      trashBin.clear();
      for (llvm::BasicBlock &BB : F)
        for (llvm::Instruction &I : BB)
          if (llvm::AllocaInst *AI = llvm::dyn_cast<llvm::AllocaInst>(&I)) {
            llvm::ConstantInt *offset =
                llvm::ConstantInt::get(Int64Ty, acc, true);
            llvm::BinaryOperator *Sub =
                llvm::BinaryOperator::CreateAdd(CI, offset, "", AI);
            llvm::CastInst *Cast = llvm::CastInst::CreateBitOrPointerCast(
                Sub, AI->getType(), "", AI);
            AI->replaceAllUsesWith(Cast);
            trashBin.emplace_back(AI);
            const auto req_size = unwrapOrThrowWithAlloca(
                analysis::tryCalculateSize(AI->getAllocatedType()), *AI);
            const auto alloc_size = (req_size + 7UL) & (UINT64_MAX - 7UL);
            acc += alloc_size;
          }
      for (llvm::Instruction *AI : trashBin)
        AI->eraseFromParent();
    }
  }
  bool updated = true;
  while (updated) {
    updated = false;
    trashBin.clear();
    for (llvm::Function &F : M)
      for (llvm::BasicBlock &BB : F)
        for (llvm::Instruction &I : BB)
          if (llvm::IntToPtrInst *ITPI =
                  llvm::dyn_cast<llvm::IntToPtrInst>(&I)) {
            if (ITPI->hasNUses(0)) {
              updated = true;
              trashBin.emplace_back(ITPI);
            } else
              for (llvm::User *U : ITPI->users())
                if (llvm::PtrToIntInst *PTII =
                        llvm::dyn_cast<llvm::PtrToIntInst>(U)) {
                  updated = true;
                  llvm::Value *oper = ITPI->getOperand(0);
                  U->replaceAllUsesWith(oper);
                  trashBin.emplace_back(PTII);
                }
          }
    for (llvm::Instruction *I : trashBin)
      I->eraseFromParent();
  }
  return llvm::PreservedAnalyses::all();
}
} // namespace sc::backend::alloca_elim
