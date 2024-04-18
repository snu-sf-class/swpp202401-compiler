#include "check_oracle.h"
#include "emitter.h"
#include "symbol.h"

#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"

using namespace llvm;
using namespace static_error;

namespace sc::backend::check_oracle {

class InvalidOracleError : public static_error::Error<InvalidOracleError> {
public:
  const char *what() const noexcept {
    return "Invalid oracle error: length of the oracle function must "
           "not exceed 50 LLVM IR instructions";
  }
};

PreservedAnalyses CheckOraclePass::run(Module &M, ModuleAnalysisManager &MAM) {
  for (Function &F : M) {
    if (F.getName().str() == "oracle") {
      auto numI = 0;
      for (BasicBlock &BB : F) {
        for (auto it = BB.begin(); it != BB.end(); ++it) {
          numI++;
        }
      }

      // crash if oracle contains more than 50 LLVM IR instructions
      // function calls and aload instructions are checked by the
      // swpp-interpreter
      if (numI > 50)
        throw InvalidOracleError();
      break;
    }
  }
  return PreservedAnalyses::all();
}
} // namespace sc::backend::check_oracle
