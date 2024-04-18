#include "backend.h"

#include "backend/alloca_eliminate.h"
#include "backend/assembly.h"
#include "backend/check_oracle.h"
#include "backend/const_expr_eliminate.h"
#include "backend/const_map.h"
#include "backend/const_split.h"
#include "backend/emitter.h"
#include "backend/gep_const_combine.h"
#include "backend/gep_eliminate.h"
#include "backend/gv_eliminate.h"
#include "backend/phi_preprocess.h"
#include "backend/register_allocate.h"
#include "backend/sext_eliminate.h"
#include "print_ir.h"

#include "llvm/IR/PassManager.h"
#include "llvm/Transforms/Scalar/EarlyCSE.h"

using namespace std::string_literals;

namespace sc::backend {
BackendInternalError::BackendInternalError(const std::exception &__e) noexcept {
  message = "exception thrown from backend\n"s + __e.what();
}

std::expected<std::string, BackendInternalError>
emitAssembly(std::unique_ptr<llvm::Module> &&__M,
             llvm::ModuleAnalysisManager &__MAM) noexcept {
  using RetType = std::expected<std::string, BackendInternalError>;

  try {
    llvm::ModulePassManager MPM;
    MPM.addPass(check_oracle::CheckOraclePass());
    MPM.addPass(ce_elim::ConstExprEliminatePass());
    MPM.addPass(gep_elim::GEPEliminatePass());
    MPM.addPass(gv_elim::GVEliminatePass());
    MPM.addPass(alloca_elim::AllocaEliminatePass());
    MPM.addPass(gc_comb::GEPConstCombinePass());
    MPM.run(*__M, __MAM);
  } catch (const std::exception &e) {
    return RetType::unexpected_type(BackendInternalError(e));
  }
  sc::print_ir::printIRIfVerbose(*__M, "After backend passes"s);

  symbol::SymbolMap SM;
  const_map::ConstMap CM;
  try {
    llvm::ModulePassManager MPM;
    MPM.addPass(phi_prep::PHIPreprocessPass());
    MPM.addPass(sext_elim::SignExtendEliminatePass());
    MPM.addPass(const_split::ConstantSplitPass(CM));
    MPM.addPass(reg_alloc::RegisterAllocatePass(SM, CM));
    MPM.run(*__M, __MAM);
  } catch (const std::exception &e) {
    return RetType::unexpected_type(BackendInternalError(e));
  }
  sc::print_ir::printIRIfVerbose(*__M, "After register allocation"s);

  std::string assembly;
  try {
    auto AE = emitter::AssemblyEmitter(SM);
    AE.visit(*__M);
    assembly = AE.getAssembly();
  } catch (const std::exception &e) {
    return RetType::unexpected_type(BackendInternalError(e));
  }

  return RetType(assembly);
}
} // namespace sc::backend
