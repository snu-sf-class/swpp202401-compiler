#include "print_ir.h"

namespace {
static bool is_verbose = false;
}

namespace sc::print_ir {
void setVerbose() noexcept { is_verbose = true; }

void printIRIfVerbose(const llvm::Module &__M,
                      const std::string_view __pass_name) noexcept {
  using namespace std::string_literals;
  if (is_verbose) {
    auto pass_delimiter = "----------------------------------------\n"s;
    pass_delimiter.reserve(140);
    pass_delimiter.append("          "s);
    pass_delimiter.append(__pass_name);
    pass_delimiter.append("\n");
    pass_delimiter.append("----------------------------------------\n");

    llvm::outs() << pass_delimiter;
    llvm::outs() << __M;
  }
}
} // namespace sc::print_ir
