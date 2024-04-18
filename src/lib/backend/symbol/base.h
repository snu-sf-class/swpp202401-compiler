#ifndef SC_LIB_BACKEND_SYMBOL_BASE_H
#define SC_LIB_BACKEND_SYMBOL_BASE_H

#include <string>

namespace sc::backend::symbol::base {
template <typename T> class SymbolBase {
private:
  std::string name;
  friend T;
  SymbolBase(std::string &&name) noexcept : name(std::move(name)) {}

public:
  SymbolBase(SymbolBase &&) noexcept = default;
  SymbolBase &operator=(SymbolBase &&) = default;

  std::string getName() const noexcept { return name; };
};

class Register : public SymbolBase<Register> {
public:
  Register(std::string &&name) noexcept;
};

class Argument : public SymbolBase<Argument> {
public:
  Argument(std::string &&name) noexcept;
};

class StackPtr : public SymbolBase<StackPtr> {
public:
  StackPtr(std::string &&name) noexcept;
};

class Constant : public SymbolBase<Constant> {
public:
  Constant(std::string &&name) noexcept;
};

class VectorRegister : public SymbolBase<VectorRegister> {
public:
  VectorRegister(std::string &&name) noexcept;
};

class FunctionName : public SymbolBase<FunctionName> {
public:
  FunctionName(std::string &&name) noexcept;
};

class BasicBlockLabel : public SymbolBase<BasicBlockLabel> {
public:
  BasicBlockLabel(std::string &&name) noexcept;
};
} // namespace sc::backend::symbol::base

namespace sc::backend::symbol {
using Register = base::Register;
using Argument = base::Argument;
using StackPtr = base::StackPtr;
using Constant = base::Constant;
using VectorRegister = base::VectorRegister;
using FunctionName = base::FunctionName;
using BasicBlockLabel = base::BasicBlockLabel;
} // namespace sc::backend::symbol
#endif // SC_LIB_BACKEND_SYMBOL_BASE_H
