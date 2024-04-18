#include "symbol.h"

#include <format>

using namespace std::string_literals;

namespace sc::backend::symbol {
Symbol::Symbol(const SymbolType __type, DataTy &&__data) noexcept
    : type(__type), data(std::move(__data)) {}

std::string Symbol::getName() const noexcept {
  return std::visit([](auto &&sym) { return sym.getName(); }, data);
}

Symbol Symbol::createRegisterSymbol(std::string &&__name) noexcept {
  auto reg = Register(std::move(__name));
  return Symbol(SymbolType::GENERAL, std::move(reg));
}

Symbol Symbol::createRegisterSymbol(int __num) noexcept {
  auto reg = Register('r' + std::to_string(__num));
  return Symbol(SymbolType::GENERAL, std::move(reg));
}

Symbol Symbol::createArgumentSymbol(std::string &&__name) noexcept {
  auto reg = Argument(std::move(__name));
  return Symbol(SymbolType::ARGUMENT, std::move(reg));
}

Symbol Symbol::createArgumentSymbol(int __num) noexcept {
  auto reg = Argument("arg" + std::to_string(__num));
  return Symbol(SymbolType::ARGUMENT, std::move(reg));
}

Symbol Symbol::createStackPtrSymbol() noexcept {
  auto reg = StackPtr("sp");
  return Symbol(SymbolType::STACK_PTR, std::move(reg));
}

Symbol Symbol::createConstantSymbol(std::string &&__name) noexcept {
  auto reg = Constant(std::move(__name));
  return Symbol(SymbolType::INTEGER_CONSTANT, std::move(reg));
}

Symbol Symbol::createConstantSymbol(uint64_t __num) noexcept {
  auto val_str = std::to_string(__num);
  return createConstantSymbol(std::move(val_str));
}

Symbol Symbol::createVectorRegisterSymbol(std::string &&__name) noexcept {
  auto reg = Argument(std::move(__name));
  return Symbol(SymbolType::VECTOR_REG, std::move(reg));
}

Symbol Symbol::createVectorRegisterSymbol(uint8_t __rid) noexcept {
  auto reg_name = "v"s.append(std::to_string(__rid));
  return createVectorRegisterSymbol(std::move(reg_name));
}

Symbol Symbol::createFunctionNameSymbol(std::string &&__name) noexcept {
  auto reg = FunctionName(std::move(__name));
  return Symbol(SymbolType::FUNC_NAME, std::move(reg));
}

Symbol Symbol::createBasicBlockLabelSymbol(std::string &&__name) noexcept {
  auto reg = BasicBlockLabel(std::move(__name));
  return Symbol(SymbolType::BB_LABEL, std::move(reg));
}

DuplicateAssignmentError::DuplicateAssignmentError(
    llvm::Value *const __value) noexcept {
  using namespace std::string_literals;
  message = "symbol map error: tried to assign more than one symbol to"s.append(
      __value->getName());
}

const symbol::Symbol *
SymbolMap::getSymbol(llvm::Value *const __value) const noexcept {
  auto itr = sym_map.find(__value);
  if (itr == sym_map.cend()) {
    return nullptr;
  } else {
    return &itr->second;
  }
}

std::expected<const symbol::Symbol *, DuplicateAssignmentError>
SymbolMap::addSymbol(llvm::Value *const __value,
                     symbol::Symbol &&__symbol) noexcept {
  using RetType =
      std::expected<const symbol::Symbol *, DuplicateAssignmentError>;
  const auto [itr, success] = sym_map.try_emplace(__value, std::move(__symbol));
  if (success) {
    return RetType(&itr->second);
  } else {
    return RetType::unexpected_type(DuplicateAssignmentError(__value));
  }
}

void SymbolMap::clear() noexcept { sym_map.clear(); }
} // namespace sc::backend::symbol
