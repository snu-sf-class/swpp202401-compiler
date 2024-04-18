#include "gv_eliminate.h"

#include "analysis.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Instructions.h"

#include <concepts>
#include <optional>

using namespace std::string_literals;
using namespace static_error;

namespace {
constexpr uint64_t START_ADDRESS = 204800UL;

class NoMainError : public Error<NoMainError> {
public:
  const char *what() const noexcept {
    return "GV elimination error: no main function to bind the GV";
  }
};

class InvalidEntryBlockError : public Error<InvalidEntryBlockError> {
public:
  const char *what() const noexcept {
    return "GV elimination error: unable to bind the malloc into the entry "
           "block";
  }
};

template <typename E> class ErrorWithGV : public Error<ErrorWithGV<E>> {
private:
  std::string message;

public:
  ErrorWithGV(E &&__err, const llvm::GlobalVariable &__gv) noexcept {
    std::string gv;
    llvm::raw_string_ostream rso(gv);
    rso << __gv;

    message = std::string(__err.what()).append("\n["s).append(gv).append("]"s);
  }

  const char *what() const noexcept { return message.c_str(); }
};

template <std::movable T, ErrorLike E>
T unwrapOrThrowWithGV(std::expected<T, E> &&__res,
                      const llvm::GlobalVariable &__gv) {
  using ResType = std::expected<T, E>;

  if (!__res.has_value()) {
    auto err = __res.error();
    throw ErrorWithGV(std::move(err), __gv);
  }

  return __res.value();
}
} // namespace

namespace sc::backend::gv_elim {
llvm::PreservedAnalyses GVEliminatePass::run(llvm::Module &M,
                                             llvm::ModuleAnalysisManager &MAM) {
  std::optional<unsigned int> address_space;
  llvm::IntegerType *Int64Ty = llvm::Type::getInt64Ty(M.getContext());
  uint64_t acc = 0UL;
  std::vector<llvm::GlobalVariable *> gvs;
  for (llvm::GlobalVariable &gv : M.globals()) {
    if (!address_space.has_value()) {
      address_space = gv.getAddressSpace();
    }
    gvs.push_back(&gv);
    uint64_t addr = START_ADDRESS + acc;
    std::vector<llvm::PtrToIntInst *> trashBin;
    std::vector<llvm::Use *> uses;
    for (llvm::Use &use : gv.uses())
      uses.push_back(&use);
    for (llvm::Use *use : uses) {
      llvm::User *user = use->getUser();
      if (llvm::Instruction *I = llvm::dyn_cast<llvm::Instruction>(user)) {
        if (llvm::PtrToIntInst *PTII = llvm::dyn_cast<llvm::PtrToIntInst>(I)) {
          llvm::Constant *C =
              llvm::ConstantInt::get(PTII->getType(), addr, true);
          PTII->replaceAllUsesWith(C);
          trashBin.push_back(PTII);
        } else {
          llvm::ConstantInt *C = llvm::ConstantInt::get(Int64Ty, addr, true);
          llvm::CastInst *CI =
              llvm::CastInst::CreateBitOrPointerCast(C, gv.getType(), "", I);
          use->set(CI);
        }
      }
    }
    for (llvm::PtrToIntInst *PTII : trashBin)
      PTII->eraseFromParent();
    const auto gv_size =
        unwrapOrThrowWithGV(analysis::tryCalculateSize(gv.getValueType()), gv);
    const auto alloc_size = (gv_size + 7UL) & (UINT64_MAX - 7UL);
    acc += alloc_size;
  }
  for (llvm::GlobalVariable *gv : gvs)
    gv->eraseFromParent();

  if (acc) {
    llvm::Function *F = M.getFunction("main");
    if (!F) {
      throw NoMainError();
    }

    llvm::IntegerType *Int64Ty = llvm::Type::getInt64Ty(M.getContext());
    llvm::FunctionCallee FC = M.getOrInsertFunction(
        "malloc",
        llvm::PointerType::get(M.getContext(), address_space.value_or(0)),
        Int64Ty);
    llvm::ConstantInt *Const = llvm::ConstantInt::get(Int64Ty, acc, true);
    llvm::Instruction *I = F->getEntryBlock().getFirstNonPHI();
    if (!I) {
      throw InvalidEntryBlockError();
    }

    llvm::CallInst *CI = llvm::CallInst::Create(FC, {Const}, "", I);
  }
  return llvm::PreservedAnalyses::all();
}
} // namespace sc::backend::gv_elim
