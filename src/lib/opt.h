#ifndef SC_LIB_OPT_H
#define SC_LIB_OPT_H

/**
 * @file opt.h
 * @author SWPP TAs (swpp@sf.snu.ac.kr)
 * @brief Optimization module
 * @version 2024.1.13
 * @date 2024-05-31
 * @copyright Copyright (c) 2022-2024 SWPP TAs
 */

#include "../static_error.h"
#include "llvm/IR/PassManager.h"

#include <expected>
#include <memory>
#include <string>

namespace sc::opt {
/**
 * @brief Exception thrown while applying the optimization pass
 */
class OptInternalError : public static_error::Error<OptInternalError> {
private:
  std::string message;

public:
  /**
   * @brief Construct a new OptInternalError object
   *
   * This is a type cast interface for exceptions thrown during the
   * optimization process.
   * @param e Any exception
   */
  OptInternalError(const std::exception &e) noexcept;

  /**
   * @brief Read the exception
   * @return Exception message in C-String format
   */
  const char *what() const noexcept { return message.c_str(); }
};

/**
 * @brief Optimize the LLVM IR program using the implemented optimization passes
 *
 * This function applies the optimization passes to the given IR program in the
 * designated order, with help of various PassManagers.
 *
 * @param M IR program to optimize
 * @param MAM Reference to cross-registered ModuleAnalysisManager
 * @return std::expected<std::unique_ptr<llvm::Module>, OptInternalError>
 * Optimized IR program if none of the passes fail, otherwise an error from the
 * failed pass
 */
std::expected<std::unique_ptr<llvm::Module>, OptInternalError>
optimizeIR(std::unique_ptr<llvm::Module> &&M,
           llvm::ModuleAnalysisManager &MAM) noexcept;
} // namespace sc::opt
#endif // SC_LIB_OPT_H
