#ifndef SC_ARGS_H
#define SC_ARGS_H
/**
 * @file args.h
 * @author SWPP TAs (swpp@sf.snu.ac.kr)
 * @brief swpp-compiler argument parser module
 * @version 2024.1.13
 * @date 2024-05-31
 * @copyright Copyright (c) 2022-2024 SWPP TAs
 */

#include "static_error.h"

#include <expected>
#include <string>

namespace scargs {
/**
 * @brief Parsed swpp-compiler arguments
 */
typedef struct {
  std::string input_filename;
  std::string output_filename;
  bool verbose_printing;
} Args;

/**
 * @brief Exception thrown by scargs module
 */
class ArgsParseError : public static_error::Error<ArgsParseError> {
private:
  std::string message;

public:
  /**
   * @brief Construct a new ArgsParseError object
   * @param message Message to show upon failure
   */
  ArgsParseError(const std::string_view message) noexcept;

  /**
   * @brief Read the exception
   * @return Exception message in C-String format
   */
  const char *what() const noexcept { return message.c_str(); }
};

/**
 * @brief Parse the command line arguments into swpp-compiler arguments
 *
 * This function parses the argc and argv from main() as following:
 * * If argc == 3, it assumes that argv[1] and argv[2] each contains input
 * filename and output filename, and follow the default verbosity level (false).
 * * If argc == 4, it assumes that argv[1] and argv[2] each contains input
 * filename and output filename, and set verbosity level to true if argv[3] is
 * "--verbose".
 * * Otherwise, it will fail to parse and return an error.
 *
 * @param argc Number of command line arguments
 * @param argv Array of command line arguments in C-String format
 * @return std::expected<Args, ArgsParseError> Parsed arguments if parsing
 * succeeds, otherwise an error that contains the reason of failure
 */
std::expected<Args, ArgsParseError> parse(const int argc,
                                          char *const argv[]) noexcept;
} // namespace scargs
#endif // SC_ARGS_H
