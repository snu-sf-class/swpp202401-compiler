#include "const_map.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/TypeSize.h"

#include <optional>
#include <utility>

namespace sc::backend::const_map {
llvm::Instruction *
ConstMap::resolve_constant(llvm::Function *F, llvm::IntegerType *ty,
                           assembly::IntTy value,
                           llvm::Instruction *__insert_before) {
  const auto cst = llvm::ConstantInt::get(ty, value);
  return resolve_constant(F, cst, __insert_before);
}

llvm::Instruction *
ConstMap::resolve_constant(llvm::Function *F, llvm::PointerType *pty,
                           assembly::IntTy value,
                           llvm::Instruction *__insert_before) {
  const auto i64_ty = llvm::IntegerType::getInt64Ty(pty->getContext());
  const auto cst = llvm::ConstantInt::get(i64_ty, value);
  return resolve_constant(F, cst, __insert_before);
}

llvm::Instruction *
ConstMap::resolve_constant(llvm::Function *F, llvm::Constant *cst,
                           llvm::Instruction *__insert_before) {
  if (!map.contains(F)) {
    map.insert(std::make_pair(F, FnConstMap()));
  }
  auto &fn_map = map.at(F);

  const auto Int64Ty = llvm::IntegerType::getInt64Ty(F->getContext());
  if (!fn_map.contains(cst)) {
    llvm::FunctionCallee const_i64 =
        F->getParent()->getOrInsertFunction("const_i64", Int64Ty, Int64Ty);

    const auto insert_before = __insert_before->getParent()->isEntryBlock()
                                   ? __insert_before
                                   : F->getEntryBlock().getTerminator();

    if (const auto csti = llvm::dyn_cast<llvm::ConstantInt>(cst)) {
      const auto const_operand_u64 =
          llvm::ConstantInt::get(Int64Ty, csti->getZExtValue());
      if (!fn_map.contains(const_operand_u64)) {
        llvm::Value *arg_ref[] = {const_operand_u64};
        const auto CI = llvm::CallInst::Create(
            const_i64, llvm::ArrayRef<llvm::Value *>(arg_ref), "",
            insert_before);
        fn_map.insert(std::make_pair(const_operand_u64, CI));
      }

      const auto ty = csti->getIntegerType();
      if (ty != Int64Ty) {
        const auto TI = llvm::TruncInst::CreateIntegerCast(
            fn_map.at(const_operand_u64), ty, false, "", insert_before);
        fn_map.insert(std::make_pair(cst, TI));
      }
    } else if (const auto cstv =
                   llvm::dyn_cast<llvm::ConstantDataVector>(cst)) {
      if (const auto splat_value = cstv->getSplatValue()) {
        const auto splat_inst =
            resolve_constant(F, splat_value, __insert_before);
        const auto elem_bw = splat_value->getType()->getIntegerBitWidth();

        std::optional<llvm::FunctionCallee> vbcast;
        switch (elem_bw) {
        case 32: {
          const auto Int32Ty =
              llvm::IntegerType::get(Int64Ty->getContext(), 32);
          const auto VecI32x8Ty =
              llvm::VectorType::get(Int32Ty, llvm::ElementCount::getFixed(8));
          vbcast = F->getParent()->getOrInsertFunction("vbcast_i32x8",
                                                       VecI32x8Ty, Int32Ty);
          break;
        }
        case 64: {
          const auto VecI64x4Ty =
              llvm::VectorType::get(Int64Ty, llvm::ElementCount::getFixed(4));
          vbcast = F->getParent()->getOrInsertFunction("vbcast_i64x4",
                                                       VecI64x4Ty, Int64Ty);
          break;
        }
        default:
          llvm::errs() << "Tried to resolve vector of unsupported type!\n";
          break;
        }

        const auto CI = llvm::CallInst::Create(
            *vbcast, llvm::ArrayRef<llvm::Value *>(splat_inst), "",
            insert_before);
        fn_map.insert(std::make_pair(cst, CI));
      } else {
        llvm::errs() << "Tried to resolve non-splat vector!\n";
      }
    }
  }

  return fn_map.at(cst);
}
} // namespace sc::backend::const_map
