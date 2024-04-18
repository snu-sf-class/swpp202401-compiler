#include "parser.h"

#include "llvm/AsmParser/Parser.h"
#include "llvm/Support/SourceMgr.h"

namespace sc::parser {
ParserError::ParserError(const std::string_view __message) noexcept {
  using namespace std::string_literals;
  message = "parser error: unable to parse input IR\n"s.append(__message);
}

std::expected<std::unique_ptr<llvm::Module>, ParserError>
parseIR(const std::string_view __code, const std::string_view __filename,
        llvm::LLVMContext &__context) noexcept {
  using RetType = std::expected<std::unique_ptr<llvm::Module>, ParserError>;

  llvm::SMDiagnostic diag;
  const auto code_buffer = llvm::MemoryBufferRef(__code, __filename);
  auto M = llvm::parseAssembly(code_buffer, diag, __context);
  if (!M) {
    std::string message;
    llvm::raw_string_ostream os(message);
    diag.print("", os);
    return RetType::unexpected_type(ParserError(message));
  }

  return RetType(std::move(M));
}
} // namespace sc::parser
