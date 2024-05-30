#ifndef SC_LIB_BACKEND_H
#define SC_LIB_BACKEND_H

/**
 * @file backend.h
 * @author SWPP TAs (swpp@sf.snu.ac.kr)
 * @brief Assembly emission module
 *
 * Note: We're not offering documents for modules inside the backend, because
 * you really don't have to look into them in order to develop new optimization
 * passes.
 * Emitting the assembly relies on a number of complex algorithms, but
 * what you only need to know is that the backend will emit the correct assembly
 * as long as you feed it an appropriate LLVM IR program.
 * @version 2024.1.13
 * @date 2024-05-31
 * @copyright Copyright (c) 2022-2024 SWPP TAs
 */

#include "../static_error.h"
#include "llvm/IR/PassManager.h"

#include <expected>

namespace sc::backend {
/**
 * @brief Exception thrown while applying the backend transformations
 * or emitting the assembly
 */
class BackendInternalError : public static_error::Error<BackendInternalError> {
private:
  std::string message;

public:
  /**
   * @brief Construct a new BackendInternalError object
   *
   * This is a type cast interface for exceptions thrown during the
   * backend transformation & assembly emission process.
   * @param e Any exception
   */
  BackendInternalError(const std::exception &e) noexcept;

  /**
   * @brief Read the exception
   * @return Exception message in C-String format
   */
  const char *what() const noexcept { return message.c_str(); }
};

/**
 * @brief Emit the swpp assembly program from the optimized LLVM IR program
 *
 * This function transforms the given IR program into non-SSA, more
 * machine-friendly code, and emits the swpp assembly program from it.
 *
 * @param M IR program to emit into swpp assembly
 * @param MAM Reference to cross-registered ModuleAnalysisManager
 * @return std::expected<std::string, BackendInternalError> String that contains
 * the swpp assembly program if none of the transformation and emission fails,
 * otherwise an error from the failed step
 */
std::expected<std::string, BackendInternalError>
emitAssembly(std::unique_ptr<llvm::Module> &&M,
             llvm::ModuleAnalysisManager &MAM) noexcept;
} // namespace sc::backend
#endif // SC_LIB_BACKEND_H
