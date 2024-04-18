#include "analysis.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/Support/Casting.h"

#include <set>

namespace {
const std::set<unsigned int> MoveInsts = {
    llvm::Instruction::BitCast, llvm::Instruction::PtrToInt,
    llvm::Instruction::IntToPtr, llvm::Instruction::ZExt,
    llvm::Instruction::Trunc};
const std::set<unsigned int> NoRegInsts = {
    llvm::Instruction::Store, llvm::Instruction::Ret, llvm::Instruction::Switch,
    llvm::Instruction::Br};
} // namespace

using namespace std::string_literals;

namespace sc::backend::analysis {
UnknownSizeTypeError::UnknownSizeTypeError(llvm::Type *__ty) noexcept {
  message = "Type of unknown size: Unable to calculate the size of "s.append(
      __ty->getStructName());
}

llvm::Instruction *isMoveInst(llvm::Value *v) {
  llvm::Instruction *i = llvm::dyn_cast<llvm::Instruction>(v);
  return (i && MoveInsts.count(i->getOpcode())) ? i : nullptr;
}

bool isReg(llvm::Value *v) {
  while (llvm::Instruction *inst = isMoveInst(v))
    v = inst->getOperand(0);
  llvm::Instruction *I = llvm::dyn_cast<llvm::Instruction>(v);
  llvm::CallInst *CI = llvm::dyn_cast<llvm::CallInst>(v);
  return !(llvm::isa<llvm::Constant>(v) || llvm::isa<llvm::Argument>(v) ||
           (I && NoRegInsts.count(I->getOpcode())) ||
           (CI && (CI->getType()->isVoidTy() ||
                   CI->getCalledFunction()->getName().equals("$decr_sp"))));
}

std::expected<uint64_t, UnknownSizeTypeError>
tryCalculateSize(llvm::Type *__ty) noexcept {
  using RetType = std::expected<uint64_t, UnknownSizeTypeError>;

  if (llvm::isa<llvm::PointerType>(__ty)) {
    return RetType(8UL);
  } else if (llvm::isa<llvm::IntegerType>(__ty)) {
    switch (__ty->getIntegerBitWidth()) {
    case 1:
    case 8:
      return RetType(1UL);
    case 16:
      return RetType(2UL);
    case 32:
      return RetType(4UL);
    case 64:
      return RetType(8UL);
    default:
      return RetType::unexpected_type(UnknownSizeTypeError(__ty));
    }
  } else if (llvm::isa<llvm::ArrayType>(__ty)) {
    auto elem_size_res = tryCalculateSize(__ty->getArrayElementType());
    return elem_size_res.transform([__ty](const auto elem_sz) {
      return elem_sz * __ty->getArrayNumElements();
    });
  } else if (const auto vec_ty = llvm::dyn_cast<llvm::VectorType>(__ty)) {
    auto elem_size_res = tryCalculateSize(vec_ty->getElementType());
    return elem_size_res.transform([vec_ty](const auto elem_sz) {
      return elem_sz * vec_ty->getElementCount().getFixedValue();
    });
  } else {
    return RetType::unexpected_type(UnknownSizeTypeError(__ty));
  }
}
} // namespace sc::backend::analysis
