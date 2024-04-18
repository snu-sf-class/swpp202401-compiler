#ifndef SC_BACKEND_SYMBOL_H
#define SC_BACKEND_SYMBOL_H

#include "../../static_error.h"
#include "symbol/base.h"
#include "llvm/IR/Value.h"

#include <cstdint>
#include <expected>
#include <span>
#include <string>
#include <unordered_map>
#include <variant>

namespace sc::backend::symbol {
class Symbol {
private:
  enum class SymbolType {
    GENERAL,
    ARGUMENT,
    STACK_PTR,
    VECTOR_REG,
    INTEGER_CONSTANT,
    VECTOR_CONSTANT,
    FUNC_NAME,
    BB_LABEL
  };
  using DataTy = std::variant<Register, Argument, StackPtr, Constant,
                              VectorRegister, FunctionName, BasicBlockLabel>;

  SymbolType type;
  DataTy data;
  Symbol(const SymbolType type, DataTy &&data) noexcept;

public:
  Symbol(Symbol &&other) = default;
  Symbol &operator=(Symbol &&other) noexcept = default;

  std::string getName() const noexcept;

  static Symbol createRegisterSymbol(std::string &&name) noexcept;
  static Symbol createRegisterSymbol(int num) noexcept;
  static Symbol createArgumentSymbol(std::string &&name) noexcept;
  static Symbol createArgumentSymbol(int num) noexcept;
  static Symbol createStackPtrSymbol() noexcept;
  static Symbol createVectorRegisterSymbol(std::string &&name) noexcept;
  static Symbol createVectorRegisterSymbol(uint8_t rid) noexcept;
  static Symbol createConstantSymbol(std::string &&name) noexcept;
  static Symbol createConstantSymbol(uint64_t num) noexcept;
  static Symbol createFunctionNameSymbol(std::string &&name) noexcept;
  static Symbol createBasicBlockLabelSymbol(std::string &&name) noexcept;
};

class DuplicateAssignmentError
    : public static_error::Error<DuplicateAssignmentError> {
private:
  std::string message;

public:
  DuplicateAssignmentError(llvm::Value *const) noexcept;
  const char *what() const noexcept { return message.c_str(); }
};

class SymbolMap {
private:
  std::unordered_map<llvm::Value *, symbol::Symbol> sym_map;

public:
  const symbol::Symbol *getSymbol(llvm::Value *const) const noexcept;
  std::expected<const symbol::Symbol *, DuplicateAssignmentError>
  addSymbol(llvm::Value *const value, symbol::Symbol &&symbol) noexcept;
  void clear() noexcept;
};
} // namespace sc::backend::symbol
#endif // SC_BACKEND_SYMBOL_H
