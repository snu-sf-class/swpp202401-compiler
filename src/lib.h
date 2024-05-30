#ifndef SC_LIB_H
#define SC_LIB_H

/**
 * @file lib.h
 * @author SWPP TAs (swpp@sf.snu.ac.kr)
 * @brief swpp-compiler main module
 * @version 2024.1.13
 * @date 2024-05-31
 * @copyright Copyright (c) 2022-2024 SWPP TAs
 */

#include "static_error.h"

#include <expected>
#include <string>

namespace sc {
/**
 * @brief Exception thrown by main module
 */
class SWPPCompilerError : public static_error::Error<SWPPCompilerError> {
private:
  std::string message;

public:
  /**
   * @brief Construct a new SWPPCompilerError object
   *
   * This is a type cast interface of every exception that is thrown during the
   * compilation process.
   * @tparam E Any subtype of Error
   */
  template <typename E> SWPPCompilerError(static_error::Error<E> &&) noexcept;

  /**
   * @brief Read the exception
   * @return Exception message in C-String format
   */
  const char *what() const noexcept { return message.c_str(); };
};

/**
 * @brief Compile an LLVM IR program into swpp assembly program
 *
 * This function compiles an LLVM IR program as following:
 * 1. Read the IR program code from the input file
 * 2. Parse the IR program into llvm::Module
 * 3. Apply optimization passes to the IR program
 * 4. Emit swpp assembly program from the optimized IR program
 * 5. Write the emitted assembly into the output file
 *
 * @param input_filename Relative path to the IR program source file (.ll)
 * @param output_filename Relative path of the destination file (.s)
 * @param verbose_printing Defaults to false. If set to true,
 * sc::print_ir::printIRIfVerbose() will print the current IR program.
 * @return std::expected<size_t, SWPPCompilerError> 0 if compilation succeeds,
 * otherwise an error that contains the reason of failure
 */
std::expected<size_t, SWPPCompilerError>
compile(const std::string_view input_filename,
        const std::string_view output_filename,
        const bool verbose_printing) noexcept;
} // namespace sc
#endif // SC_LIB_H
