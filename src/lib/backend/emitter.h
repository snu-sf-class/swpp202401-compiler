#ifndef SC_BACKEND_EMITTER_H
#define SC_BACKEND_EMITTER_H

#include "symbol.h"
#include "llvm/IR/InstVisitor.h"

#include <string>
#include <vector>

namespace sc::backend::emitter {
class AssemblyEmitter : public llvm::InstVisitor<AssemblyEmitter> {
private:
  std::vector<std::string> assembly_lines;

public:
  AssemblyEmitter(const symbol::SymbolMap &SM) noexcept;
  void visitFunction(llvm::Function &);
  void visitBasicBlock(llvm::BasicBlock &);
  void visitICmpInst(llvm::ICmpInst &);
  void visitLoadInst(llvm::LoadInst &);
  void visitStoreInst(llvm::StoreInst &);
  // void visitSExtInst(llvm::SExtInst &);
  void visitSelectInst(llvm::SelectInst &);
  void visitExtractElementInst(llvm::ExtractElementInst &I);
  void visitInsertElementInst(llvm::InsertElementInst &I);
  void visitCallInst(llvm::CallInst &);
  void visitReturnInst(llvm::ReturnInst &);
  void visitBranchInst(llvm::BranchInst &);
  void visitSwitchInst(llvm::SwitchInst &);
  void visitBinaryOperator(llvm::BinaryOperator &);
  void visitUnreachableInst(llvm::UnreachableInst &);

  std::string getAssembly() noexcept;
};
} // namespace sc::backend::emitter

#endif // SC_BACKEND_EMITTER_H