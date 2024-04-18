#include "base.h"

namespace sc::backend::symbol::base {
Register::Register(std::string &&__name) noexcept
    : SymbolBase(std::move(__name)) {}

Argument::Argument(std::string &&__name) noexcept
    : SymbolBase(std::move(__name)) {}

StackPtr::StackPtr(std::string &&__name) noexcept
    : SymbolBase(std::move(__name)) {}

Constant::Constant(std::string &&__name) noexcept
    : SymbolBase(std::move(__name)) {}

VectorRegister::VectorRegister(std::string &&__name) noexcept
    : SymbolBase(std::move(__name)) {}

FunctionName::FunctionName(std::string &&__name) noexcept
    : SymbolBase(std::move(__name)) {}

BasicBlockLabel::BasicBlockLabel(std::string &&__name) noexcept
    : SymbolBase(std::move(__name)) {}
} // namespace sc::backend::symbol::base
