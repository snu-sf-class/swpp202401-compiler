#include "gep_const_combine.h"

#include "llvm/IR/PatternMatch.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Transforms/Scalar/DCE.h"

using namespace llvm;
using namespace llvm::PatternMatch;

namespace sc::backend::gc_comb {
PreservedAnalyses GEPConstCombinePass::run(Module &M,
                                           ModuleAnalysisManager &MAM) {
  FunctionAnalysisManager &FAM =
      MAM.getResult<FunctionAnalysisManagerModuleProxy>(M).getManager();

  for (Function &F : M) {
    bool updated = true;
    while (updated) {
      updated = false;
      for (BasicBlock &BB : F)
        for (Instruction &I : BB) {
          if (!isa<BinaryOperator>(I))
            continue;
          Value *V0, *V1;
          ConstantInt *C0, *C1;
          if (match(&I, m_Add(m_ConstantInt(C0), m_ConstantInt(C1)))) {
            updated = true;
            Constant *C = ConstantInt::get(
                I.getType(), C0->getSExtValue() + C1->getSExtValue(), true);
            I.replaceAllUsesWith(C);
          } else if (match(&I, m_Sub(m_ConstantInt(C0), m_ConstantInt(C1)))) {
            updated = true;
            Constant *C = ConstantInt::get(
                I.getType(), C0->getSExtValue() - C1->getSExtValue(), true);
            I.replaceAllUsesWith(C);
          } else if (match(&I, m_Mul(m_ConstantInt(C0), m_ConstantInt(C1)))) {
            updated = true;
            Constant *C = ConstantInt::get(
                I.getType(), C0->getSExtValue() * C1->getSExtValue(), true);
            I.replaceAllUsesWith(C);
          } else if (match(&I, m_Add(m_ConstantInt(C0), m_Value(V0)))) {
            updated = true;
            I.setOperand(0, V0);
            I.setOperand(1, C0);
          } else if (match(&I, m_Mul(m_ConstantInt(C0), m_Value(V0)))) {
            updated = true;
            I.setOperand(0, V0);
            I.setOperand(1, C0);
          } else if (match(&I, m_Add(m_Value(V0), m_ConstantInt(C0))) &&
                     C0->isZero()) {
            updated = true;
            I.replaceAllUsesWith(V0);
          } else if (match(&I, m_Sub(m_Value(V0), m_ConstantInt(C0))) &&
                     C0->isZero()) {
            updated = true;
            I.replaceAllUsesWith(V0);
          } else if (match(&I, m_Sub(m_ConstantInt(C0), m_Value(V0))) &&
                     C0->isZero()) {
            updated = true;
            Constant *C = ConstantInt::get(I.getType(), -1, true);
            BinaryOperator *B = BinaryOperator::CreateMul(V0, C, "", &I);
            I.replaceAllUsesWith(B);
          } else if (match(&I, m_Mul(m_Value(V0), m_ConstantInt(C0))) &&
                     C0->isZero()) {
            updated = true;
            I.replaceAllUsesWith(C0);
          } else if (match(&I, m_Mul(m_Value(V0), m_ConstantInt(C0))) &&
                     C0->isOne()) {
            updated = true;
            I.replaceAllUsesWith(V0);
          } else if (match(&I, m_Add(m_Value(V0),
                                     m_Mul(m_Value(V1), m_ConstantInt(C0)))) &&
                     C0->isMinusOne()) {
            updated = true;
            BinaryOperator *B = BinaryOperator::CreateSub(V0, V1, "", &I);
            I.replaceAllUsesWith(B);
          } else if (match(&I, m_Sub(m_Value(V0),
                                     m_Mul(m_Value(V1), m_ConstantInt(C0)))) &&
                     C0->isMinusOne()) {
            updated = true;
            BinaryOperator *B = BinaryOperator::CreateAdd(V0, V1, "", &I);
            I.replaceAllUsesWith(B);
          } else if (match(&I, m_Add(m_Add(m_Value(V0), m_ConstantInt(C0)),
                                     m_ConstantInt(C1)))) {
            updated = true;
            Constant *C = ConstantInt::get(
                I.getType(), C0->getSExtValue() + C1->getSExtValue(), true);
            BinaryOperator *B = BinaryOperator::CreateAdd(V0, C, "", &I);
            I.replaceAllUsesWith(B);
          } else if (match(&I, m_Add(m_Sub(m_Value(V0), m_ConstantInt(C0)),
                                     m_ConstantInt(C1)))) {
            updated = true;
            Constant *C = ConstantInt::get(
                I.getType(), -C0->getSExtValue() + C1->getSExtValue(), true);
            BinaryOperator *B = BinaryOperator::CreateAdd(V0, C, "", &I);
            I.replaceAllUsesWith(B);
          } else if (match(&I, m_Add(m_Sub(m_ConstantInt(C0), m_Value(V0)),
                                     m_ConstantInt(C1)))) {
            updated = true;
            Constant *C = ConstantInt::get(
                I.getType(), C0->getSExtValue() + C1->getSExtValue(), true);
            BinaryOperator *B = BinaryOperator::CreateSub(C, V0, "", &I);
            I.replaceAllUsesWith(B);
          } else if (match(&I, m_Sub(m_Add(m_Value(V0), m_ConstantInt(C0)),
                                     m_ConstantInt(C1)))) {
            updated = true;
            Constant *C = ConstantInt::get(
                I.getType(), C0->getSExtValue() - C1->getSExtValue(), true);
            BinaryOperator *B = BinaryOperator::CreateAdd(V0, C, "", &I);
            I.replaceAllUsesWith(B);
          } else if (match(&I, m_Sub(m_Sub(m_Value(V0), m_ConstantInt(C0)),
                                     m_ConstantInt(C1)))) {
            updated = true;
            Constant *C = ConstantInt::get(
                I.getType(), -C0->getSExtValue() - C1->getSExtValue(), true);
            BinaryOperator *B = BinaryOperator::CreateAdd(V0, C, "", &I);
            I.replaceAllUsesWith(B);
          } else if (match(&I, m_Sub(m_Sub(m_ConstantInt(C0), m_Value(V0)),
                                     m_ConstantInt(C1)))) {
            updated = true;
            Constant *C = ConstantInt::get(
                I.getType(), C0->getSExtValue() - C1->getSExtValue(), true);
            BinaryOperator *B = BinaryOperator::CreateSub(C, V0, "", &I);
            I.replaceAllUsesWith(B);
          } else if (match(&I, m_Mul(m_Add(m_Value(V0), m_ConstantInt(C0)),
                                     m_ConstantInt(C1)))) {
            updated = true;
            BinaryOperator *B0 = BinaryOperator::CreateMul(V0, C1, "", &I);
            Constant *C = ConstantInt::get(
                I.getType(), C0->getSExtValue() * C1->getSExtValue(), true);
            BinaryOperator *B = BinaryOperator::CreateAdd(B0, C, "", &I);
            I.replaceAllUsesWith(B);
          } else if (match(&I, m_Mul(m_Sub(m_Value(V0), m_ConstantInt(C0)),
                                     m_ConstantInt(C1)))) {
            updated = true;
            BinaryOperator *B0 = BinaryOperator::CreateMul(V0, C1, "", &I);
            Constant *C = ConstantInt::get(
                I.getType(), -C0->getSExtValue() * C1->getSExtValue(), true);
            BinaryOperator *B = BinaryOperator::CreateAdd(B0, C, "", &I);
            I.replaceAllUsesWith(B);
          } else if (match(&I, m_Mul(m_Mul(m_Value(V0), m_ConstantInt(C0)),
                                     m_ConstantInt(C1)))) {
            updated = true;
            Constant *C = ConstantInt::get(
                I.getType(), C0->getSExtValue() * C1->getSExtValue(), true);
            BinaryOperator *B = BinaryOperator::CreateMul(V0, C, "", &I);
            I.replaceAllUsesWith(B);
          } else if (match(&I, m_Mul(m_Sub(m_ConstantInt(C0), m_Value(V0)),
                                     m_ConstantInt(C1)))) {
            updated = true;
            Constant *C2 =
                ConstantInt::get(I.getType(), -C1->getSExtValue(), true);
            BinaryOperator *B0 = BinaryOperator::CreateMul(V0, C2, "", &I);
            Constant *C = ConstantInt::get(
                I.getType(), C0->getSExtValue() * C1->getSExtValue(), true);
            BinaryOperator *B = BinaryOperator::CreateAdd(B0, C, "", &I);
            I.replaceAllUsesWith(B);
          } else if (match(&I, m_Sub(m_ConstantInt(C0),
                                     m_Add(m_Value(V0), m_ConstantInt(C1))))) {
            updated = true;
            Constant *C = ConstantInt::get(
                I.getType(), C0->getSExtValue() - C1->getSExtValue(), true);
            BinaryOperator *B = BinaryOperator::CreateSub(C, V0, "", &I);
            I.replaceAllUsesWith(B);
          } else if (match(&I, m_Sub(m_ConstantInt(C0),
                                     m_Sub(m_Value(V0), m_ConstantInt(C1))))) {
            updated = true;
            Constant *C = ConstantInt::get(
                I.getType(), C0->getSExtValue() + C1->getSExtValue(), true);
            BinaryOperator *B = BinaryOperator::CreateSub(C, V0, "", &I);
            I.replaceAllUsesWith(B);
          } else if (match(&I, m_Sub(m_ConstantInt(C0),
                                     m_Sub(m_ConstantInt(C1), m_Value(V0))))) {
            updated = true;
            Constant *C = ConstantInt::get(
                I.getType(), C0->getSExtValue() - C1->getSExtValue(), true);
            BinaryOperator *B = BinaryOperator::CreateAdd(V0, C, "", &I);
            I.replaceAllUsesWith(B);
          } else if (match(&I, m_Add(m_Add(m_Value(V0), m_ConstantInt(C0)),
                                     m_Value(V1)))) {
            updated = true;
            BinaryOperator *B0 = BinaryOperator::CreateAdd(V0, V1, "", &I);
            BinaryOperator *B = BinaryOperator::CreateAdd(B0, C0, "", &I);
            I.replaceAllUsesWith(B);
          } else if (match(&I, m_Add(m_Sub(m_Value(V0), m_ConstantInt(C0)),
                                     m_Value(V1)))) {
            updated = true;
            Constant *C =
                ConstantInt::get(I.getType(), -C0->getSExtValue(), true);
            BinaryOperator *B0 = BinaryOperator::CreateAdd(V0, V1, "", &I);
            BinaryOperator *B = BinaryOperator::CreateAdd(B0, C, "", &I);
            I.replaceAllUsesWith(B);
          } else if (match(&I, m_Sub(m_Add(m_Value(V0), m_ConstantInt(C0)),
                                     m_Value(V1)))) {
            updated = true;
            BinaryOperator *B0 = BinaryOperator::CreateSub(V0, V1, "", &I);
            BinaryOperator *B = BinaryOperator::CreateAdd(B0, C0, "", &I);
            I.replaceAllUsesWith(B);
          } else if (match(&I, m_Sub(m_Sub(m_Value(V0), m_ConstantInt(C0)),
                                     m_Value(V1)))) {
            updated = true;
            Constant *C =
                ConstantInt::get(I.getType(), -C0->getSExtValue(), true);
            BinaryOperator *B0 = BinaryOperator::CreateSub(V0, V1, "", &I);
            BinaryOperator *B = BinaryOperator::CreateAdd(B0, C, "", &I);
            I.replaceAllUsesWith(B);
          } else if (match(&I, m_Add(m_Sub(m_ConstantInt(C0), m_Value(V0)),
                                     m_Value(V1)))) {
            updated = true;
            BinaryOperator *B0 = BinaryOperator::CreateSub(V1, V0, "", &I);
            BinaryOperator *B = BinaryOperator::CreateAdd(B0, C0, "", &I);
            I.replaceAllUsesWith(B);
          } else if (match(&I, m_Sub(m_Sub(m_ConstantInt(C0), m_Value(V0)),
                                     m_Value(V1)))) {
            updated = true;
            BinaryOperator *B0 = BinaryOperator::CreateAdd(V0, V1, "", &I);
            BinaryOperator *B = BinaryOperator::CreateSub(C0, B0, "", &I);
            I.replaceAllUsesWith(B);
          } else if (match(&I, m_Add(m_Value(V0),
                                     m_Add(m_Value(V1), m_ConstantInt(C0))))) {
            updated = true;
            BinaryOperator *B0 = BinaryOperator::CreateAdd(V0, V1, "", &I);
            BinaryOperator *B = BinaryOperator::CreateAdd(B0, C0, "", &I);
            I.replaceAllUsesWith(B);
          } else if (match(&I, m_Add(m_Value(V0),
                                     m_Sub(m_Value(V1), m_ConstantInt(C0))))) {
            updated = true;
            Constant *C =
                ConstantInt::get(I.getType(), -C0->getSExtValue(), true);
            BinaryOperator *B0 = BinaryOperator::CreateAdd(V0, V1, "", &I);
            BinaryOperator *B = BinaryOperator::CreateAdd(B0, C, "", &I);
            I.replaceAllUsesWith(B);
          } else if (match(&I, m_Sub(m_Value(V0),
                                     m_Add(m_Value(V1), m_ConstantInt(C0))))) {
            updated = true;
            Constant *C =
                ConstantInt::get(I.getType(), -C0->getSExtValue(), true);
            BinaryOperator *B0 = BinaryOperator::CreateSub(V0, V1, "", &I);
            BinaryOperator *B = BinaryOperator::CreateAdd(B0, C, "", &I);
            I.replaceAllUsesWith(B);
          } else if (match(&I, m_Sub(m_Value(V0),
                                     m_Sub(m_Value(V1), m_ConstantInt(C0))))) {
            updated = true;
            BinaryOperator *B0 = BinaryOperator::CreateSub(V0, V1, "", &I);
            BinaryOperator *B = BinaryOperator::CreateAdd(B0, C0, "", &I);
            I.replaceAllUsesWith(B);
          } else if (match(&I, m_Add(m_Value(V0),
                                     m_Sub(m_ConstantInt(C0), m_Value(V1))))) {
            updated = true;
            BinaryOperator *B0 = BinaryOperator::CreateSub(V0, V1, "", &I);
            BinaryOperator *B = BinaryOperator::CreateAdd(B0, C0, "", &I);
            I.replaceAllUsesWith(B);
          } else if (match(&I, m_Sub(m_Value(V0),
                                     m_Sub(m_ConstantInt(C0), m_Value(V1))))) {
            updated = true;
            Constant *C =
                ConstantInt::get(I.getType(), -C0->getSExtValue(), true);
            BinaryOperator *B0 = BinaryOperator::CreateAdd(V0, V1, "", &I);
            BinaryOperator *B = BinaryOperator::CreateAdd(B0, C, "", &I);
            I.replaceAllUsesWith(B);
          }
        }
      DCEPass().run(F, FAM);
    }
  }
  return llvm::PreservedAnalyses::all();
}
} // namespace sc::backend::gc_comb
