#include "const_map.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
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
ConstMap::resolve_constant(llvm::Function *F, llvm::ConstantInt *cst,
                           llvm::Instruction *__insert_before) {
  const auto Int64Ty = llvm::IntegerType::getInt64Ty(F->getContext());
  if (!map.contains(F)) {
    map.insert(std::make_pair(F, FnConstMap()));
  }
  auto &fn_map = map.at(F);

  if (!fn_map.contains(cst)) {
    llvm::FunctionCallee const_i64 =
        F->getParent()->getOrInsertFunction("const_i64", Int64Ty, Int64Ty);

    const auto insert_before = __insert_before->getParent()->isEntryBlock()
                                   ? __insert_before
                                   : F->getEntryBlock().getTerminator();

    const auto const_operand_u64 =
        llvm::ConstantInt::get(Int64Ty, cst->getZExtValue());
    if (!fn_map.contains(const_operand_u64)) {
      llvm::Value *arg_ref[] = {const_operand_u64};
      const auto CI = llvm::CallInst::Create(
          const_i64, llvm::ArrayRef<llvm::Value *>(arg_ref), "", insert_before);
      fn_map.insert(std::make_pair(const_operand_u64, CI));
    }

    const auto ty = cst->getIntegerType();
    if (ty != Int64Ty) {
      const auto TI = llvm::TruncInst::CreateIntegerCast(
          fn_map.at(const_operand_u64), ty, false, "", insert_before);
      fn_map.insert(std::make_pair(cst, TI));
    }
  }

  return fn_map.at(cst);
}
} // namespace sc::backend::const_map
