#include "opt.h"

#include "../static_error.h"
#include "llvm/Analysis/CGSCCPassManager.h"
#include "llvm/Transforms/Scalar/LoopPassManager.h"

#include "print_ir.h"

using namespace std::string_literals;

namespace sc::opt {
OptInternalError::OptInternalError(const std::exception &__e) noexcept {
  message = "exception thrown from opt\n"s + __e.what();
}

std::expected<std::unique_ptr<llvm::Module>, OptInternalError>
optimizeIR(std::unique_ptr<llvm::Module> &&__M,
           llvm::ModuleAnalysisManager &__MAM) noexcept {
  using RetType =
      std::expected<std::unique_ptr<llvm::Module>, OptInternalError>;

  try {
    llvm::LoopPassManager LPM;
    llvm::FunctionPassManager FPM;
    llvm::CGSCCPassManager CGPM;
    llvm::ModulePassManager MPM;

    // Add loop-level opt passes below

    FPM.addPass(llvm::createFunctionToLoopPassAdaptor(std::move(LPM)));
    // Add function-level opt passes below

    CGPM.addPass(llvm::createCGSCCToFunctionPassAdaptor(std::move(FPM)));
    // Add CGSCC-level opt passes below

    MPM.addPass(llvm::createModuleToPostOrderCGSCCPassAdaptor(std::move(CGPM)));
    // Add module-level opt passes below

    MPM.run(*__M, __MAM);
    sc::print_ir::printIRIfVerbose(*__M, "After optimization");
  } catch (const std::exception &e) {
    return RetType::unexpected_type(OptInternalError(e));
  }

  return RetType(std::move(__M));
}
} // namespace sc::opt
