#include "phi_preprocess.h"

#include "analysis.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Support/Casting.h"

#include <set>

namespace sc::backend::phi_prep {
llvm::PreservedAnalyses
PHIPreprocessPass::run(llvm::Module &M, llvm::ModuleAnalysisManager &MAM) {
  llvm::IntegerType *Int64Ty = llvm::Type::getInt64Ty(M.getContext());

  std::vector<llvm::PHINode *> phis;
  for (llvm::Function &F : M)
    for (llvm::BasicBlock &BB : F)
      for (llvm::Instruction &I : BB)
        if (llvm::PHINode *phi = llvm::dyn_cast<llvm::PHINode>(&I))
          phis.emplace_back(phi);

  // All incoming values of phi node becomes a register instruction.
  for (llvm::PHINode *phi : phis)
    for (int i = 0; i < phi->getNumIncomingValues(); i++) {
      llvm::Value *v = phi->getIncomingValue(i);
      if (!analysis::isReg(v)) {
        llvm::Instruction *t = phi->getIncomingBlock(i)->getTerminator();
        llvm::Type *type = v->getType();
        if (type->isVectorTy()) {
          const auto vty = llvm::dyn_cast<llvm::VectorType>(type);
          v = llvm::BinaryOperator::CreateMul(
              v,
              llvm::ConstantVector::getSplat(
                  vty->getElementCount(),
                  llvm::ConstantInt::get(vty->getElementType(), 1)),
              "", t);
        } else if (!type->isIntegerTy())
          v = llvm::CastInst::CreateBitOrPointerCast(v, Int64Ty, "", t);
        v = llvm::BinaryOperator::CreateMul(
            v, llvm::ConstantInt::get(v->getType(), 1UL, true), "", t);
        if (!type->isIntegerTy() && !type->isVectorTy())
          v = llvm::CastInst::CreateBitOrPointerCast(v, type, "", t);
        phi->setIncomingValue(i, v);
      }
    }

  // No two phi nodes share operands
  std::set<llvm::Value *> visit;
  for (llvm::PHINode *phi : phis)
    for (int i = 0; i < phi->getNumIncomingValues(); i++) {
      llvm::Value *v = phi->getIncomingValue(i);
      llvm::Value *w = v;
      while (llvm::Instruction *next = analysis::isMoveInst(w))
        w = next->getOperand(0);
      if (visit.count(w)) {
        llvm::Instruction *t = phi->getIncomingBlock(i)->getTerminator();
        llvm::Type *type = v->getType();
        if (type->isVectorTy()) {
          const auto vty = llvm::dyn_cast<llvm::VectorType>(type);
          v = llvm::BinaryOperator::CreateMul(
              v,
              llvm::ConstantVector::getSplat(
                  vty->getElementCount(),
                  llvm::ConstantInt::get(vty->getElementType(), 1)),
              "", t);
        } else if (!type->isIntegerTy())
          v = llvm::CastInst::CreateBitOrPointerCast(v, Int64Ty, "", t);
        v = llvm::BinaryOperator::CreateMul(
            v, llvm::ConstantInt::get(v->getType(), 1UL, true), "", t);
        if (!type->isIntegerTy() && !type->isVectorTy())
          v = llvm::CastInst::CreateBitOrPointerCast(v, type, "", t);
        phi->setIncomingValue(i, v);
        visit.insert(v);
      } else {
        visit.insert(w);
      }
    }

  // No incoming value of phi node is phi node.
  for (llvm::PHINode *phi : phis)
    for (int i = 0; i < phi->getNumIncomingValues(); i++) {
      llvm::Value *v = phi->getIncomingValue(i);
      llvm::Value *w = v;
      while (llvm::Instruction *next = analysis::isMoveInst(w))
        w = next->getOperand(0);
      if (llvm::isa<llvm::PHINode>(w)) {
        llvm::Instruction *t = phi->getIncomingBlock(i)->getTerminator();
        llvm::Type *type = v->getType();
        if (type->isVectorTy()) {
          const auto vty = llvm::dyn_cast<llvm::VectorType>(type);
          v = llvm::BinaryOperator::CreateMul(
              v,
              llvm::ConstantVector::getSplat(
                  vty->getElementCount(),
                  llvm::ConstantInt::get(vty->getElementType(), 1)),
              "", t);
        } else if (!type->isIntegerTy())
          v = llvm::CastInst::CreateBitOrPointerCast(v, Int64Ty, "", t);
        v = llvm::BinaryOperator::CreateMul(
            v, llvm::ConstantInt::get(v->getType(), 1UL, true), "", t);
        if (!type->isIntegerTy() && !type->isVectorTy())
          v = llvm::CastInst::CreateBitOrPointerCast(v, type, "", t);
        phi->setIncomingValue(i, v);
      }
    }
  return llvm::PreservedAnalyses::all();
}
} // namespace sc::backend::phi_prep
