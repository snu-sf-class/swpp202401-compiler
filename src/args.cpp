#include "args.h"

namespace scargs {
ArgsParseError::ArgsParseError(const std::string_view __message) noexcept {
  using namespace std::string_literals;
  message = "unable to parse command line argument(s)\n"s;
  message.append(__message);
}

std::expected<Args, ArgsParseError> parse(const int __argc,
                                          char *const __argv[]) noexcept {
  using namespace std::string_literals;
  using RetType = std::expected<Args, ArgsParseError>;

  const auto usage_message =
      "(USAGE: swpp-compiler <input.ll> <output.s> [--verbose])"s;

  bool verbose_printing;
  if (__argc == 3) {
    verbose_printing = false;
  } else if (__argc == 4) {
    if (__argv[3] == "--verbose"s) {
      verbose_printing = true;
    } else {
      const auto message =
          "unrecognized option \""s.append(__argv[3]).append("\" ").append(
              usage_message);
      return RetType::unexpected_type(ArgsParseError(message));
    }
  } else {
    const auto message = "invalid number of arguments "s.append(usage_message);
    return RetType::unexpected_type(ArgsParseError(message));
  }

  auto args = Args{__argv[1], __argv[2], verbose_printing};
  return RetType(std::move(args));
}
} // namespace scargs
