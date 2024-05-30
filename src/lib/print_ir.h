#ifndef SC_LIB_PRINT_IR_H
#define SC_LIB_PRINT_IR_H

/**
 * @file print_ir.h
 * @author SWPP TAs (swpp@sf.snu.ac.kr)
 * @brief Module to conditionally print the given LLVM IR program
 * @version 2024.1.13
 * @date 2024-05-31
 * @copyright Copyright (c) 2022-2024 SWPP TAs
 */

#include "llvm/IR/Module.h"

#include <string>

namespace sc::print_ir {
/**
 * @brief Start to print the IR whenever printIRIfVerbose() is called
 */
void setVerbose() noexcept;

/**
 * @brief Print the given IR program if setVerbose() was called before
 * @param pass_name Name of the last pass that was applied to this IR
 */
void printIRIfVerbose(const llvm::Module &,
                      const std::string_view pass_name) noexcept;
} // namespace sc::print_ir

#endif // SC_LIB_PRINT_IR_H
