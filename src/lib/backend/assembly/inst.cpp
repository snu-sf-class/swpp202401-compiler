#include "inst.h"

#include <algorithm>
#include <numeric>
#include <variant>

using namespace std::string_literals;

namespace {
std::string joinTokens(std::vector<std::string> &&__tokens) noexcept {
  const auto collection_len = std::accumulate(
      __tokens.cbegin(), __tokens.cend(), static_cast<size_t>(0),
      [](const size_t len, const auto &line) -> size_t {
        return len + line.size() + 1; // trailing whitespace or colon
      });

  std::string joined_string;
  joined_string.reserve(collection_len);
  for (const auto &token : __tokens) {
    joined_string.append(token);
    joined_string.append(" "s);
  }

  // remove trailing whitespace
  joined_string.pop_back();
  return joined_string;
}

using namespace sc::backend::assembly::inst;

std::string getToken(const IcmpCondition __cond) noexcept {
  switch (__cond) {
  case IcmpCondition::EQ:
    return "eq"s;
  case IcmpCondition::NE:
    return "ne"s;
  case IcmpCondition::UGT:
    return "ugt"s;
  case IcmpCondition::UGE:
    return "uge"s;
  case IcmpCondition::ULT:
    return "ult"s;
  case IcmpCondition::ULE:
    return "ule"s;
  case IcmpCondition::SGT:
    return "sgt"s;
  case IcmpCondition::SGE:
    return "sge"s;
  case IcmpCondition::SLT:
    return "slt"s;
  case IcmpCondition::SLE:
    return "sle"s;
  default: // unreachable
    return ";"s;
  }
}

std::string getToken(const std::string_view __name) noexcept {
  return std::string(__name);
}

std::string getToken(const ScalarRegisterTy &__value) noexcept {
  using super::register_t::getToken;
  return std::visit([](const auto &val) { return getToken(val); }, __value);
}

std::string getToken(const ScalarValueTy &__value) noexcept {
  using ::getToken;
  using super::int_t::getToken;
  return std::visit([](const auto &val) { return getToken(val); }, __value);
}

template <typename T>
std::vector<std::string> __unpackOperandTokens(const T &__arg) noexcept {
  std::vector<std::string> tokens;
  tokens.reserve(4); // sufficient for most ops (except switch and call)

  tokens.push_back(getToken(__arg));
  return tokens;
}

template <typename T, typename... Ts>
std::vector<std::string> __unpackOperandTokens(const T &__arg,
                                               Ts... __args) noexcept {
  auto tokens = __unpackOperandTokens(__args...);
  tokens.push_back(getToken(__arg));
  return tokens;
}

template <typename... Ts>
std::vector<std::string> collectOperandTokens(Ts... __args) noexcept {
  auto operands_rev = __unpackOperandTokens(__args...);
  // operands are unpacked in reverse order
  std::reverse(operands_rev.begin(), operands_rev.end());
  return operands_rev;
}

template <typename... Ts>
std::vector<std::string> collectOpTokens(std::string &&__opname,
                                         Ts... __args) noexcept {
  auto operand_tokens = collectOperandTokens(__args...);

  std::vector<std::string> tokens = {std::move(__opname)};
  tokens.reserve(operand_tokens.size() + 1);
  std::move(operand_tokens.begin(), operand_tokens.end(),
            std::back_inserter(tokens));
  return tokens;
}

std::vector<std::string>
prependTarget(const GeneralRegister __target,
              std::vector<std::string> &&__tokens) noexcept {
  std::vector<std::string> tokens = {getToken(__target), "="s};
  tokens.reserve(tokens.size() + __tokens.size());
  std::move(__tokens.begin(), __tokens.end(), std::back_inserter(tokens));
  return tokens;
}

std::vector<std::string> getIntBinaryOpTokens(const GeneralRegister __target,
                                              std::string &&__op_name,
                                              const ScalarRegisterTy &__lhs,
                                              const ScalarRegisterTy &__rhs,
                                              const BitWidth __bw) noexcept {
  auto op_tokens = collectOpTokens(std::move(__op_name), __lhs, __rhs, __bw);
  return prependTarget(__target, std::move(op_tokens));
}

// TODO: prependTarget function for Vector Register
std::vector<std::string>
prependTargetVector(const VectorRegister __target,
                    std::vector<std::string> &&__tokens) noexcept {
  std::vector<std::string> tokens = {getToken(__target), "="s};
  tokens.reserve(tokens.size() + __tokens.size());
  std::move(__tokens.begin(), __tokens.end(), std::back_inserter(tokens));
  return tokens;
}

std::vector<std::string> getVectorOpTokens(const VectorRegister __target,
                                           std::string &&__op_name,
                                           const VectorRegister &__vec1,
                                           const VectorRegister &__vec2,
                                           const VectorWidth __bw) noexcept {
  auto op_tokens = collectOpTokens(std::move(__op_name), __vec1, __vec2, __bw);
  return prependTargetVector(__target, std::move(op_tokens));
}

constexpr size_t max_argc = 16;
} // namespace

namespace sc::backend::assembly::inst {
//---------------------------------------------------------
// class FunctionStartInst
//---------------------------------------------------------
FunctionStartInst::FunctionStartInst(std::string &&__name,
                                     const IntTy __argc) noexcept
    : AbstractInst(), name(std::move(__name)), argc(__argc) {}

std::expected<FunctionStartInst, InvalidFunctionArgcError>
FunctionStartInst::tryCreate(std::string &&__name,
                             const IntTy __argc) noexcept {
  using RetType = std::expected<FunctionStartInst, InvalidFunctionArgcError>;

  if (__argc > max_argc) {
    return RetType::unexpected_type(InvalidFunctionArgcError());
  }

  return RetType(inst::FunctionStartInst(std::move(__name), __argc));
}

std::string FunctionStartInst::getAssembly() const noexcept {
  auto joined_tokens = joinTokens(collectOpTokens("start"s, name, argc));
  joined_tokens.append(":"s);
  return joined_tokens;
}

//---------------------------------------------------------
// class FunctionEndInst
//---------------------------------------------------------
FunctionEndInst::FunctionEndInst(std::string &&__name) noexcept
    : AbstractInst(), name(std::move(__name)) {}

FunctionEndInst FunctionEndInst::create(std::string &&__name) noexcept {
  return FunctionEndInst(std::move(__name));
}

std::string FunctionEndInst::getAssembly() const noexcept {
  return joinTokens(collectOpTokens("end"s, name));
}

//---------------------------------------------------------
// class BasicBlockInst
//---------------------------------------------------------
BasicBlockInst::BasicBlockInst(std::string &&__name) noexcept : AbstractInst() {
  name = "."s;
  name.reserve(__name.size() + 1);
  std::move(__name.begin(), __name.end(), std::back_inserter(name));
}

BasicBlockInst BasicBlockInst::create(std::string &&__name) noexcept {
  return BasicBlockInst(std::move(__name));
}

std::string BasicBlockInst::getAssembly() const noexcept { return name + ":"s; }

//---------------------------------------------------------
// class CommentInst
//---------------------------------------------------------
CommentInst::CommentInst(std::string &&__message) noexcept
    : AbstractInst(), message(std::move(__message)) {}

CommentInst CommentInst::create(std::string &&__message) noexcept {
  return CommentInst(std::move(__message));
}

std::string CommentInst::getAssembly() const noexcept {
  return joinTokens(collectOpTokens(";"s, message));
}

//---------------------------------------------------------
// class ReturnInst
//---------------------------------------------------------
ReturnInst::ReturnInst() noexcept : AbstractInst() {}

ReturnInst::ReturnInst(const ScalarRegisterTy __value) noexcept
    : AbstractInst() {
  if (std::holds_alternative<GeneralRegister>(__value)) {
    value = std::get<GeneralRegister>(__value);
  } else if (std::holds_alternative<ArgumentRegister>(__value)) {
    value = std::get<ArgumentRegister>(__value);
  }
}

ReturnInst ReturnInst::createVoid() noexcept { return ReturnInst(); }

ReturnInst ReturnInst::create(const ScalarRegisterTy __value) noexcept {
  return ReturnInst(__value);
}

std::string ReturnInst::getAssembly() const noexcept {
  if (value.has_value())
    return joinTokens(collectOpTokens("ret"s, value.value()));
  else
    return "ret"s;
}

//---------------------------------------------------------
// class JumpInst
//---------------------------------------------------------
JumpInst::JumpInst(std::string &&__label) noexcept
    : AbstractInst(), label(std::move(__label)) {}

JumpInst JumpInst::create(std::string &&__label) noexcept {
  return JumpInst(std::move(__label));
}

std::string JumpInst::getAssembly() const noexcept {
  return joinTokens(collectOpTokens("br"s, "."s.append(label)));
}

//---------------------------------------------------------
// class BranchInst
//---------------------------------------------------------
BranchInst::BranchInst(const ScalarRegisterTy __condition,
                       std::string &&__label_true,
                       std::string &&__label_false) noexcept
    : AbstractInst(), condition(__condition),
      label_true(std::move(__label_true)),
      label_false(std::move(__label_false)) {}

BranchInst BranchInst::create(const ScalarRegisterTy __condition,
                              std::string &&__label_true,
                              std::string &&__label_false) noexcept {
  return BranchInst(__condition, std::move(__label_true),
                    std::move(__label_false));
}

std::string BranchInst::getAssembly() const noexcept {
  return joinTokens(collectOpTokens("br"s, condition, "."s.append(label_true),
                                    "."s.append(label_false)));
}

//---------------------------------------------------------
// class SwitchInst
//---------------------------------------------------------
SwitchInst::SwitchInst(const ScalarRegisterTy __condition,
                       std::string &&__label_default) noexcept
    : AbstractInst(), condition(__condition),
      label_default(std::move(__label_default)) {}

void SwitchInst::addCase(const IntTy __value, std::string &&__label) noexcept {
  cases.insert({__value, std::move(__label)});
}

std::expected<SwitchInst, SwitchInst::MismatchingCaseError>
SwitchInst::tryCreate(const ScalarRegisterTy __cond,
                      std::vector<IntTy> &&__cases,
                      std::vector<std::string> &&__labels,
                      std::string &&__label_default) noexcept {
  using MismatchingCaseError = SwitchInst::MismatchingCaseError;
  using RetType = std::expected<SwitchInst, MismatchingCaseError>;

  if (__cases.size() != __labels.size()) {
    return RetType::unexpected_type(MismatchingCaseError());
  }

  auto inst = SwitchInst(__cond, std::move(__label_default));
  for (auto cs = __cases.crbegin(); cs != __cases.crend(); cs++) {
    inst.addCase(*cs, std::move(__labels.back()));
    __labels.pop_back();
  }

  return RetType(std::move(inst));
}

std::string SwitchInst::getAssembly() const noexcept {
  auto tokens = collectOpTokens("switch"s, condition);

  tokens.reserve(tokens.size() + cases.size() * 2 + 1);
  for (const auto &case_pair : cases) {
    const auto cs = case_pair.first;
    const auto label = case_pair.second;

    auto case_tokens = collectOperandTokens(cs, std::move("."s.append(label)));
    std::move(case_tokens.begin(), case_tokens.end(),
              std::back_inserter(tokens));
  }

  tokens.push_back("."s.append(label_default));
  return joinTokens(std::move(tokens));
}

//---------------------------------------------------------
// class MallocInst
//---------------------------------------------------------
MallocInst::MallocInst(const GeneralRegister __target,
                       const ScalarRegisterTy __size) noexcept
    : AbstractInst(), target(__target), size(__size) {}

MallocInst MallocInst::create(const GeneralRegister __target,
                              const ScalarRegisterTy __size) noexcept {
  return MallocInst(__target, __size);
}

std::string MallocInst::getAssembly() const noexcept {
  return joinTokens(prependTarget(target, collectOpTokens("malloc"s, size)));
}

//---------------------------------------------------------
// class FreeInst
//---------------------------------------------------------
FreeInst::FreeInst(const ScalarRegisterTy __ptr) noexcept
    : AbstractInst(), ptr(__ptr) {}

FreeInst FreeInst::create(const ScalarRegisterTy __ptr) noexcept {
  return FreeInst(__ptr);
}

std::string FreeInst::getAssembly() const noexcept {
  return joinTokens(collectOpTokens("free"s, ptr));
}

//---------------------------------------------------------
// class LoadInst
//---------------------------------------------------------
LoadInst::LoadInst(const GeneralRegister __target, const AccessWidth __size,
                   const ScalarRegisterTy __ptr) noexcept
    : AbstractInst(), target(__target), size(__size), ptr(__ptr) {}

LoadInst LoadInst::create(const GeneralRegister __target,
                          const AccessWidth __size,
                          const ScalarRegisterTy __ptr) noexcept {
  return LoadInst(__target, __size, __ptr);
}

std::string LoadInst::getAssembly() const noexcept {
  return joinTokens(prependTarget(target, collectOpTokens("load"s, size, ptr)));
}

//---------------------------------------------------------
// class StoreInst
//---------------------------------------------------------
StoreInst::StoreInst(const AccessWidth __size, const ScalarRegisterTy __value,
                     const ScalarRegisterTy __ptr) noexcept
    : AbstractInst(), size(__size), value(__value), ptr(__ptr) {}

StoreInst StoreInst::create(const AccessWidth __size,
                            const ScalarRegisterTy __value,
                            const ScalarRegisterTy __ptr) noexcept {
  return StoreInst(__size, __value, __ptr);
}

std::string StoreInst::getAssembly() const noexcept {
  return joinTokens(collectOpTokens("store", size, value, ptr));
}

//---------------------------------------------------------
// class IntAddInst
//---------------------------------------------------------
IntAddInst::IntAddInst(const GeneralRegister __target, const BitWidth __bw,
                       const ScalarRegisterTy __lhs,
                       const ScalarRegisterTy __rhs) noexcept
    : IntBinaryOpInst(__target, __bw, __lhs, __rhs) {}

IntAddInst IntAddInst::create(const GeneralRegister __target,
                              const BitWidth __bw, const ScalarRegisterTy __lhs,
                              const ScalarRegisterTy __rhs) noexcept {
  return IntAddInst(__target, __bw, __lhs, __rhs);
}

std::string IntAddInst::getAssembly() const noexcept {
  return joinTokens(getIntBinaryOpTokens(target, "add"s, lhs, rhs, bw));
}

//---------------------------------------------------------
// class IntSubInst
//---------------------------------------------------------
IntSubInst::IntSubInst(const GeneralRegister __target, const BitWidth __bw,
                       const ScalarRegisterTy __lhs,
                       const ScalarRegisterTy __rhs) noexcept
    : IntBinaryOpInst(__target, __bw, __lhs, __rhs) {}

IntSubInst IntSubInst::create(const GeneralRegister __target,
                              const BitWidth __bw, const ScalarRegisterTy __lhs,
                              const ScalarRegisterTy __rhs) noexcept {
  return IntSubInst(__target, __bw, __lhs, __rhs);
}

std::string IntSubInst::getAssembly() const noexcept {
  return joinTokens(getIntBinaryOpTokens(target, "sub"s, lhs, rhs, bw));
}

//---------------------------------------------------------
// class IntMulInst
//---------------------------------------------------------
IntMulInst::IntMulInst(const GeneralRegister __target, const BitWidth __bw,
                       const ScalarRegisterTy __lhs,
                       const ScalarRegisterTy __rhs) noexcept
    : IntBinaryOpInst(__target, __bw, __lhs, __rhs) {}

IntMulInst IntMulInst::create(const GeneralRegister __target,
                              const BitWidth __bw, const ScalarRegisterTy __lhs,
                              const ScalarRegisterTy __rhs) noexcept {
  return IntMulInst(__target, __bw, __lhs, __rhs);
}

std::string IntMulInst::getAssembly() const noexcept {
  return joinTokens(getIntBinaryOpTokens(target, "mul"s, lhs, rhs, bw));
}

//---------------------------------------------------------
// class IntUDivInst
//---------------------------------------------------------
IntUDivInst::IntUDivInst(const GeneralRegister __target, const BitWidth __bw,
                         const ScalarRegisterTy __lhs,
                         const ScalarRegisterTy __rhs) noexcept
    : IntBinaryOpInst(__target, __bw, __lhs, __rhs) {}

IntUDivInst IntUDivInst::create(const GeneralRegister __target,
                                const BitWidth __bw,
                                const ScalarRegisterTy __lhs,
                                const ScalarRegisterTy __rhs) noexcept {
  return IntUDivInst(__target, __bw, __lhs, __rhs);
}

std::string IntUDivInst::getAssembly() const noexcept {
  return joinTokens(getIntBinaryOpTokens(target, "udiv"s, lhs, rhs, bw));
}

//---------------------------------------------------------
// class IntSDivInst
//---------------------------------------------------------
IntSDivInst::IntSDivInst(const GeneralRegister __target, const BitWidth __bw,
                         const ScalarRegisterTy __lhs,
                         const ScalarRegisterTy __rhs) noexcept
    : IntBinaryOpInst(__target, __bw, __lhs, __rhs) {}

IntSDivInst IntSDivInst::create(const GeneralRegister __target,
                                const BitWidth __bw,
                                const ScalarRegisterTy __lhs,
                                const ScalarRegisterTy __rhs) noexcept {
  return IntSDivInst(__target, __bw, __lhs, __rhs);
}

std::string IntSDivInst::getAssembly() const noexcept {
  return joinTokens(getIntBinaryOpTokens(target, "sdiv"s, lhs, rhs, bw));
}

//---------------------------------------------------------
// class IntURemInst
//---------------------------------------------------------
IntURemInst::IntURemInst(const GeneralRegister __target, const BitWidth __bw,
                         const ScalarRegisterTy __lhs,
                         const ScalarRegisterTy __rhs) noexcept
    : IntBinaryOpInst(__target, __bw, __lhs, __rhs) {}

IntURemInst IntURemInst::create(const GeneralRegister __target,
                                const BitWidth __bw,
                                const ScalarRegisterTy __lhs,
                                const ScalarRegisterTy __rhs) noexcept {
  return IntURemInst(__target, __bw, __lhs, __rhs);
}

std::string IntURemInst::getAssembly() const noexcept {
  return joinTokens(getIntBinaryOpTokens(target, "urem"s, lhs, rhs, bw));
}

//---------------------------------------------------------
// class IntSRemInst
//---------------------------------------------------------
IntSRemInst::IntSRemInst(const GeneralRegister __target, const BitWidth __bw,
                         const ScalarRegisterTy __lhs,
                         const ScalarRegisterTy __rhs) noexcept
    : IntBinaryOpInst(__target, __bw, __lhs, __rhs) {}

IntSRemInst IntSRemInst::create(const GeneralRegister __target,
                                const BitWidth __bw,
                                const ScalarRegisterTy __lhs,
                                const ScalarRegisterTy __rhs) noexcept {
  return IntSRemInst(__target, __bw, __lhs, __rhs);
}

std::string IntSRemInst::getAssembly() const noexcept {
  return joinTokens(getIntBinaryOpTokens(target, "srem"s, lhs, rhs, bw));
}

//---------------------------------------------------------
// class IntAndInst
//---------------------------------------------------------
IntAndInst::IntAndInst(const GeneralRegister __target, const BitWidth __bw,
                       const ScalarRegisterTy __lhs,
                       const ScalarRegisterTy __rhs) noexcept
    : IntBinaryOpInst(__target, __bw, __lhs, __rhs) {}

IntAndInst IntAndInst::create(const GeneralRegister __target,
                              const BitWidth __bw, const ScalarRegisterTy __lhs,
                              const ScalarRegisterTy __rhs) noexcept {
  return IntAndInst(__target, __bw, __lhs, __rhs);
}

std::string IntAndInst::getAssembly() const noexcept {
  return joinTokens(getIntBinaryOpTokens(target, "and"s, lhs, rhs, bw));
}

//---------------------------------------------------------
// class IntOrInst
//---------------------------------------------------------
IntOrInst::IntOrInst(const GeneralRegister __target, const BitWidth __bw,
                     const ScalarRegisterTy __lhs,
                     const ScalarRegisterTy __rhs) noexcept
    : IntBinaryOpInst(__target, __bw, __lhs, __rhs) {}

IntOrInst IntOrInst::create(const GeneralRegister __target, const BitWidth __bw,
                            const ScalarRegisterTy __lhs,
                            const ScalarRegisterTy __rhs) noexcept {
  return IntOrInst(__target, __bw, __lhs, __rhs);
}

std::string IntOrInst::getAssembly() const noexcept {
  return joinTokens(getIntBinaryOpTokens(target, "or"s, lhs, rhs, bw));
}

//---------------------------------------------------------
// class IntXorInst
//---------------------------------------------------------
IntXorInst::IntXorInst(const GeneralRegister __target, const BitWidth __bw,
                       const ScalarRegisterTy __lhs,
                       const ScalarRegisterTy __rhs) noexcept
    : IntBinaryOpInst(__target, __bw, __lhs, __rhs) {}

IntXorInst IntXorInst::create(const GeneralRegister __target,
                              const BitWidth __bw, const ScalarRegisterTy __lhs,
                              const ScalarRegisterTy __rhs) noexcept {
  return IntXorInst(__target, __bw, __lhs, __rhs);
}

std::string IntXorInst::getAssembly() const noexcept {
  return joinTokens(getIntBinaryOpTokens(target, "xor"s, lhs, rhs, bw));
}

//---------------------------------------------------------
// class IntShlInst
//---------------------------------------------------------
IntShlInst::IntShlInst(const GeneralRegister __target, const BitWidth __bw,
                       const ScalarRegisterTy __lhs,
                       const ScalarRegisterTy __rhs) noexcept
    : IntBinaryOpInst(__target, __bw, __lhs, __rhs) {}

IntShlInst IntShlInst::create(const GeneralRegister __target,
                              const BitWidth __bw, const ScalarRegisterTy __lhs,
                              const ScalarRegisterTy __rhs) noexcept {
  return IntShlInst(__target, __bw, __lhs, __rhs);
}

std::string IntShlInst::getAssembly() const noexcept {
  return joinTokens(getIntBinaryOpTokens(target, "shl"s, lhs, rhs, bw));
}

//---------------------------------------------------------
// class IntLShrInst
//---------------------------------------------------------
IntLShrInst::IntLShrInst(const GeneralRegister __target, const BitWidth __bw,
                         const ScalarRegisterTy __lhs,
                         const ScalarRegisterTy __rhs) noexcept
    : IntBinaryOpInst(__target, __bw, __lhs, __rhs) {}

IntLShrInst IntLShrInst::create(const GeneralRegister __target,
                                const BitWidth __bw,
                                const ScalarRegisterTy __lhs,
                                const ScalarRegisterTy __rhs) noexcept {
  return IntLShrInst(__target, __bw, __lhs, __rhs);
}

std::string IntLShrInst::getAssembly() const noexcept {
  return joinTokens(getIntBinaryOpTokens(target, "lshr"s, lhs, rhs, bw));
}

//---------------------------------------------------------
// class IntAShrInst
//---------------------------------------------------------
IntAShrInst::IntAShrInst(const GeneralRegister __target, const BitWidth __bw,
                         const ScalarRegisterTy __lhs,
                         const ScalarRegisterTy __rhs) noexcept
    : IntBinaryOpInst(__target, __bw, __lhs, __rhs) {}

IntAShrInst IntAShrInst::create(const GeneralRegister __target,
                                const BitWidth __bw,
                                const ScalarRegisterTy __lhs,
                                const ScalarRegisterTy __rhs) noexcept {
  return IntAShrInst(__target, __bw, __lhs, __rhs);
}

std::string IntAShrInst::getAssembly() const noexcept {
  return joinTokens(getIntBinaryOpTokens(target, "ashr"s, lhs, rhs, bw));
}

//---------------------------------------------------------
// class IntCompInst
//---------------------------------------------------------
IntCompInst::IntCompInst(const GeneralRegister __target,
                         const IcmpCondition __cond, const BitWidth __bw,
                         const ScalarRegisterTy __lhs,
                         const ScalarRegisterTy __rhs) noexcept
    : IntBinaryOpInst(__target, __bw, __lhs, __rhs), cond(__cond) {}

IntCompInst IntCompInst::create(const GeneralRegister __target,
                                const IcmpCondition __cond, const BitWidth __bw,
                                const ScalarRegisterTy __lhs,
                                const ScalarRegisterTy __rhs) noexcept {
  return IntCompInst(__target, __cond, __bw, __lhs, __rhs);
}

std::string IntCompInst::getAssembly() const noexcept {
  return joinTokens(
      prependTarget(target, collectOpTokens("icmp"s, cond, lhs, rhs, bw)));
}

//---------------------------------------------------------
// class SelectInst
//---------------------------------------------------------
SelectInst::SelectInst(const GeneralRegister __target,
                       const ScalarRegisterTy __cond,
                       const ScalarRegisterTy __lhs,
                       const ScalarRegisterTy __rhs) noexcept
    : AbstractInst(), target(__target), cond(__cond), lhs(__lhs), rhs(__rhs) {}

SelectInst SelectInst::create(const GeneralRegister __target,
                              const ScalarRegisterTy __cond,
                              const ScalarRegisterTy __lhs,
                              const ScalarRegisterTy __rhs) noexcept {
  return SelectInst(__target, __cond, __lhs, __rhs);
}

std::string SelectInst::getAssembly() const noexcept {
  return joinTokens(
      prependTarget(target, collectOpTokens("select"s, cond, lhs, rhs)));
}

//---------------------------------------------------------
// class CallInst
//---------------------------------------------------------
CallInst::CallInst(std::string &&__name) noexcept
    : AbstractInst(), name(std::move(__name)) {}

CallInst::CallInst(const GeneralRegister __target,
                   std::string &&__name) noexcept
    : CallInst(std::move(__name)) {
  target = __target;
}

void CallInst::addArg(const ScalarRegisterTy __arg) noexcept {
  args.push_back(__arg);
}

std::expected<CallInst, InvalidFunctionArgcError>
CallInst::tryCreateDiscarding(std::string &&__name,
                              std::vector<ScalarRegisterTy> &&__args) noexcept {
  using RetType = std::expected<CallInst, InvalidFunctionArgcError>;
  if (__args.size() > max_argc) {
    return RetType::unexpected_type(InvalidFunctionArgcError());
  }

  auto inst = CallInst(std::move(__name));
  for (auto arg = std::make_move_iterator(__args.begin());
       arg != std::make_move_iterator(__args.end()); arg++) {
    inst.addArg(*arg);
  }

  return RetType(std::move(inst));
}

std::expected<CallInst, InvalidFunctionArgcError>
CallInst::tryCreateReturning(const GeneralRegister __target,
                             std::string &&__name,
                             std::vector<ScalarRegisterTy> &&__args) noexcept {
  using RetType = std::expected<CallInst, InvalidFunctionArgcError>;
  if (__args.size() > max_argc) {
    return RetType::unexpected_type(InvalidFunctionArgcError());
  }

  auto inst = CallInst(__target, std::move(__name));
  for (auto arg = std::make_move_iterator(__args.begin());
       arg != std::make_move_iterator(__args.end()); arg++) {
    inst.addArg(*arg);
  }

  return RetType(std::move(inst));
}

std::string CallInst::getAssembly() const noexcept {
  auto tokens = collectOpTokens("call", name);
  if (target.has_value()) {
    tokens = prependTarget(*target, std::move(tokens));
  }

  tokens.reserve(tokens.size() + args.size());
  for (const auto &arg : args) {
    tokens.push_back(getToken(arg));
  }

  return joinTokens(std::move(tokens));
}

//---------------------------------------------------------
// class RecursiveCallInst
//---------------------------------------------------------
RecursiveCallInst::RecursiveCallInst() noexcept : AbstractInst() {}

RecursiveCallInst::RecursiveCallInst(const GeneralRegister __target) noexcept
    : RecursiveCallInst() {
  target = __target;
}

void RecursiveCallInst::addArg(const ScalarRegisterTy __arg) noexcept {
  args.push_back(__arg);
}

std::expected<RecursiveCallInst, InvalidFunctionArgcError>
RecursiveCallInst::tryCreateDiscarding(
    std::vector<ScalarRegisterTy> &&__args) noexcept {
  using RetType = std::expected<RecursiveCallInst, InvalidFunctionArgcError>;
  if (__args.size() > max_argc) {
    return RetType::unexpected_type(InvalidFunctionArgcError());
  }

  auto inst = RecursiveCallInst();
  inst.args = std::move(__args);
  return RetType(std::move(inst));
}

std::expected<RecursiveCallInst, InvalidFunctionArgcError>
RecursiveCallInst::tryCreateReturning(
    const GeneralRegister __target,
    std::vector<ScalarRegisterTy> &&__args) noexcept {
  using RetType = std::expected<RecursiveCallInst, InvalidFunctionArgcError>;
  if (__args.size() > max_argc) {
    return RetType::unexpected_type(InvalidFunctionArgcError());
  }

  auto inst = RecursiveCallInst(__target);
  inst.args = std::move(__args);

  return RetType(std::move(inst));
}

std::string RecursiveCallInst::getAssembly() const noexcept {
  std::vector<std::string> tokens = {"rcall"s};
  if (target.has_value()) {
    tokens = prependTarget(*target, std::move(tokens));
  }

  tokens.reserve(tokens.size() + args.size());
  for (const auto &arg : args) {
    tokens.push_back(getToken(arg));
  }

  return joinTokens(std::move(tokens));
}

//---------------------------------------------------------
// class AssertEqInst
//---------------------------------------------------------
AssertEqInst::AssertEqInst(const ScalarRegisterTy __lhs,
                           const ScalarValueTy __rhs) noexcept
    : AbstractInst(), lhs(__lhs), rhs(__rhs) {}

AssertEqInst AssertEqInst::create(const ScalarRegisterTy __lhs,
                                  const ScalarValueTy __rhs) noexcept {
  return AssertEqInst(__lhs, __rhs);
}

std::string AssertEqInst::getAssembly() const noexcept {
  return joinTokens(collectOpTokens("assert_eq"s, lhs, rhs));
}

//---------------------------------------------------------
// class AsyncLoadInst
//---------------------------------------------------------
AsyncLoadInst::AsyncLoadInst(const GeneralRegister __target,
                             const AccessWidth __size,
                             const ScalarRegisterTy __ptr) noexcept
    : AbstractInst(), target(__target), size(__size), ptr(__ptr) {}

AsyncLoadInst AsyncLoadInst::create(const GeneralRegister __target,
                                    const AccessWidth __size,
                                    const ScalarRegisterTy __ptr) noexcept {
  return AsyncLoadInst(__target, __size, __ptr);
}

std::string AsyncLoadInst::getAssembly() const noexcept {
  return joinTokens(
      prependTarget(target, collectOpTokens("aload"s, size, ptr)));
}

//---------------------------------------------------------
// class InvalidNumOperandsError
//---------------------------------------------------------
InvalidNumOperandsError::InvalidNumOperandsError(
    const size_t __expected, const size_t __actual) noexcept {
  message = "Expected "s.append(std::to_string(__expected))
                .append(" operands; got "s)
                .append(std::to_string(__actual));
}

//---------------------------------------------------------
// class IntSumInst
//---------------------------------------------------------
IntSumInst::IntSumInst(const GeneralRegister __target,
                       const ScalarRegisterTy __v1, const ScalarRegisterTy __v2,
                       const ScalarRegisterTy __v3, const ScalarRegisterTy __v4,
                       const ScalarRegisterTy __v5, const ScalarRegisterTy __v6,
                       const ScalarRegisterTy __v7, const ScalarRegisterTy __v8,
                       const BitWidth __bw) noexcept
    : AbstractInst(), target(__target), v1(std::move(__v1)),
      v2(std::move(__v2)), v3(std::move(__v3)), v4(std::move(__v4)),
      v5(std::move(__v5)), v6(std::move(__v6)), v7(std::move(__v7)),
      v8(std::move(__v8)), bw(__bw) {}

std::expected<IntSumInst, InvalidNumOperandsError>
IntSumInst::tryCreate(const GeneralRegister __target,
                      std::vector<ScalarRegisterTy> &&__operands,
                      const BitWidth __bw) noexcept {
  using RetType = std::expected<IntSumInst, InvalidNumOperandsError>;

  constexpr size_t allowed_num_operands = 8;
  const size_t num_operands = __operands.size();
  if (num_operands != allowed_num_operands) {
    return RetType::unexpected_type(
        InvalidNumOperandsError(allowed_num_operands, num_operands));
  }

  const auto v1 = __operands[0];
  const auto v2 = __operands[1];
  const auto v3 = __operands[2];
  const auto v4 = __operands[3];
  const auto v5 = __operands[4];
  const auto v6 = __operands[5];
  const auto v7 = __operands[6];
  const auto v8 = __operands[7];

  return RetType(IntSumInst(__target, v1, v2, v3, v4, v5, v6, v7, v8, __bw));
}

std::string IntSumInst::getAssembly() const noexcept {
  return joinTokens(prependTarget(
      target, collectOpTokens("sum"s, v1, v2, v3, v4, v5, v6, v7, v8, bw)));
}

//---------------------------------------------------------
// class IntIncrInst
//---------------------------------------------------------
IntIncrInst::IntIncrInst(const GeneralRegister __target,
                         const ScalarRegisterTy __arg,
                         const BitWidth __bw) noexcept
    : AbstractInst(), target(__target), arg(__arg), bw(__bw) {}

IntIncrInst IntIncrInst::create(const GeneralRegister __target,
                                const ScalarRegisterTy __arg,
                                const BitWidth __bw) noexcept {
  return IntIncrInst(__target, __arg, __bw);
}

std::string IntIncrInst::getAssembly() const noexcept {
  return joinTokens(prependTarget(target, collectOpTokens("incr"s, arg, bw)));
}

//---------------------------------------------------------
// class IntDecrInst
//---------------------------------------------------------
IntDecrInst::IntDecrInst(const GeneralRegister __target,
                         const ScalarRegisterTy __arg,
                         const BitWidth __bw) noexcept
    : AbstractInst(), target(__target), arg(__arg), bw(__bw) {}

IntDecrInst IntDecrInst::create(const GeneralRegister __target,
                                const ScalarRegisterTy __arg,
                                const BitWidth __bw) noexcept {
  return IntDecrInst(__target, __arg, __bw);
}

std::string IntDecrInst::getAssembly() const noexcept {
  return joinTokens(prependTarget(target, collectOpTokens("decr"s, arg, bw)));
}

//---------------------------------------------------------
// class ConstantInst
//---------------------------------------------------------
ConstantInst::ConstantInst(const GeneralRegister __target,
                           const IntTy __value) noexcept
    : AbstractInst(), target(__target), value(__value) {}

ConstantInst ConstantInst::create(const GeneralRegister __target,
                                  const IntTy __value) noexcept {
  return ConstantInst(__target, __value);
}

std::string ConstantInst::getAssembly() const noexcept {
  return joinTokens(prependTarget(target, collectOpTokens("const"s, value)));
}

//---------------------------------------------------------
// class VectorLoadInst
//---------------------------------------------------------
VectorLoadInst::VectorLoadInst(const VectorRegister __target,
                               const ScalarRegisterTy __ptr) noexcept
    : AbstractInst(), target(__target), ptr(__ptr) {}

VectorLoadInst VectorLoadInst::create(const VectorRegister __target,
                                      const ScalarRegisterTy __ptr) noexcept {
  return VectorLoadInst(__target, __ptr);
}

std::string VectorLoadInst::getAssembly() const noexcept {
  return joinTokens(
      prependTargetVector(target, collectOpTokens("vload"s, ptr)));
}

//---------------------------------------------------------
// class VectorStoreInst
//---------------------------------------------------------
VectorStoreInst::VectorStoreInst(const VectorRegister __value,
                                 const ScalarRegisterTy __ptr) noexcept
    : AbstractInst(), value(__value), ptr(__ptr) {}

VectorStoreInst VectorStoreInst::create(const VectorRegister __value,
                                        const ScalarRegisterTy __ptr) noexcept {
  return VectorStoreInst(__value, __ptr);
}

std::string VectorStoreInst::getAssembly() const noexcept {
  return joinTokens(collectOpTokens("vstore", value, ptr));
}

//---------------------------------------------------------
// class VectorAddInst
//---------------------------------------------------------
VectorAddInst::VectorAddInst(const VectorRegister __target,
                             const VectorRegister __vec1,
                             const VectorRegister __vec2,
                             const VectorWidth __bw) noexcept
    : VectorInst(__target, __vec1, __vec2, __bw) {}

VectorAddInst VectorAddInst::create(const VectorRegister __target,
                                    const VectorRegister __vec1,
                                    const VectorRegister __vec2,
                                    const VectorWidth __bw) noexcept {
  return VectorAddInst(__target, __vec1, __vec2, __bw);
}

std::string VectorAddInst::getAssembly() const noexcept {
  return joinTokens(getVectorOpTokens(target, "vadd"s, vec1, vec2, bw));
}

//---------------------------------------------------------
// class VectorSubInst
//---------------------------------------------------------

VectorSubInst::VectorSubInst(const VectorRegister __target,
                             const VectorRegister __vec1,
                             const VectorRegister __vec2,
                             const VectorWidth __bw) noexcept
    : VectorInst(__target, __vec1, __vec2, __bw) {}

VectorSubInst VectorSubInst::create(const VectorRegister __target,
                                    const VectorRegister __vec1,
                                    const VectorRegister __vec2,
                                    const VectorWidth __bw) noexcept {
  return VectorSubInst(__target, __vec1, __vec2, __bw);
}

std::string VectorSubInst::getAssembly() const noexcept {
  return joinTokens(getVectorOpTokens(target, "vsub"s, vec1, vec2, bw));
}
//---------------------------------------------------------
// class VectorMulInst
//---------------------------------------------------------

VectorMulInst::VectorMulInst(const VectorRegister __target,
                             const VectorRegister __vec1,
                             const VectorRegister __vec2,
                             const VectorWidth __bw) noexcept
    : VectorInst(__target, __vec1, __vec2, __bw) {}

VectorMulInst VectorMulInst::create(const VectorRegister __target,
                                    const VectorRegister __vec1,
                                    const VectorRegister __vec2,
                                    const VectorWidth __bw) noexcept {
  return VectorMulInst(__target, __vec1, __vec2, __bw);
}

std::string VectorMulInst::getAssembly() const noexcept {
  return joinTokens(getVectorOpTokens(target, "vmul"s, vec1, vec2, bw));
}

//---------------------------------------------------------
// class VectorUDivInst
//---------------------------------------------------------

VectorUDivInst::VectorUDivInst(const VectorRegister __target,
                               const VectorRegister __vec1,
                               const VectorRegister __vec2,
                               const VectorWidth __bw) noexcept
    : VectorInst(__target, __vec1, __vec2, __bw) {}

VectorUDivInst VectorUDivInst::create(const VectorRegister __target,
                                      const VectorRegister __vec1,
                                      const VectorRegister __vec2,
                                      const VectorWidth __bw) noexcept {
  return VectorUDivInst(__target, __vec1, __vec2, __bw);
}

std::string VectorUDivInst::getAssembly() const noexcept {
  return joinTokens(getVectorOpTokens(target, "vudiv"s, vec1, vec2, bw));
}

//---------------------------------------------------------
// class VectorSDivInst
//---------------------------------------------------------

VectorSDivInst::VectorSDivInst(const VectorRegister __target,
                               const VectorRegister __vec1,
                               const VectorRegister __vec2,
                               const VectorWidth __bw) noexcept
    : VectorInst(__target, __vec1, __vec2, __bw) {}

VectorSDivInst VectorSDivInst::create(const VectorRegister __target,
                                      const VectorRegister __vec1,
                                      const VectorRegister __vec2,
                                      const VectorWidth __bw) noexcept {
  return VectorSDivInst(__target, __vec1, __vec2, __bw);
}

std::string VectorSDivInst::getAssembly() const noexcept {
  return joinTokens(getVectorOpTokens(target, "vsdiv"s, vec1, vec2, bw));
}
//---------------------------------------------------------
// class VectorURemInst
//---------------------------------------------------------

VectorURemInst::VectorURemInst(const VectorRegister __target,
                               const VectorRegister __vec1,
                               const VectorRegister __vec2,
                               const VectorWidth __bw) noexcept
    : VectorInst(__target, __vec1, __vec2, __bw) {}

VectorURemInst VectorURemInst::create(const VectorRegister __target,
                                      const VectorRegister __vec1,
                                      const VectorRegister __vec2,
                                      const VectorWidth __bw) noexcept {
  return VectorURemInst(__target, __vec1, __vec2, __bw);
}

std::string VectorURemInst::getAssembly() const noexcept {
  return joinTokens(getVectorOpTokens(target, "vurem"s, vec1, vec2, bw));
}

//---------------------------------------------------------
// class VectorSRemInst
//---------------------------------------------------------

VectorSRemInst::VectorSRemInst(const VectorRegister __target,
                               const VectorRegister __vec1,
                               const VectorRegister __vec2,
                               const VectorWidth __bw) noexcept
    : VectorInst(__target, __vec1, __vec2, __bw) {}

VectorSRemInst VectorSRemInst::create(const VectorRegister __target,
                                      const VectorRegister __vec1,
                                      const VectorRegister __vec2,
                                      const VectorWidth __bw) noexcept {
  return VectorSRemInst(__target, __vec1, __vec2, __bw);
}

std::string VectorSRemInst::getAssembly() const noexcept {
  return joinTokens(getVectorOpTokens(target, "vsrem"s, vec1, vec2, bw));
}

//---------------------------------------------------------
// class VectorAndInst
//---------------------------------------------------------

VectorAndInst::VectorAndInst(const VectorRegister __target,
                             const VectorRegister __vec1,
                             const VectorRegister __vec2,
                             const VectorWidth __bw) noexcept
    : VectorInst(__target, __vec1, __vec2, __bw) {}

VectorAndInst VectorAndInst::create(const VectorRegister __target,
                                    const VectorRegister __vec1,
                                    const VectorRegister __vec2,
                                    const VectorWidth __bw) noexcept {
  return VectorAndInst(__target, __vec1, __vec2, __bw);
}

std::string VectorAndInst::getAssembly() const noexcept {
  return joinTokens(getVectorOpTokens(target, "vand"s, vec1, vec2, bw));
}

//---------------------------------------------------------
// class VectorOrInst
//---------------------------------------------------------

VectorOrInst::VectorOrInst(const VectorRegister __target,
                           const VectorRegister __vec1,
                           const VectorRegister __vec2,
                           const VectorWidth __bw) noexcept
    : VectorInst(__target, __vec1, __vec2, __bw) {}

VectorOrInst VectorOrInst::create(const VectorRegister __target,
                                  const VectorRegister __vec1,
                                  const VectorRegister __vec2,
                                  const VectorWidth __bw) noexcept {
  return VectorOrInst(__target, __vec1, __vec2, __bw);
}

std::string VectorOrInst::getAssembly() const noexcept {
  return joinTokens(getVectorOpTokens(target, "vor"s, vec1, vec2, bw));
}

//---------------------------------------------------------
// class VectorXorInst
//---------------------------------------------------------

VectorXorInst::VectorXorInst(const VectorRegister __target,
                             const VectorRegister __vec1,
                             const VectorRegister __vec2,
                             const VectorWidth __bw) noexcept
    : VectorInst(__target, __vec1, __vec2, __bw) {}

VectorXorInst VectorXorInst::create(const VectorRegister __target,
                                    const VectorRegister __vec1,
                                    const VectorRegister __vec2,
                                    const VectorWidth __bw) noexcept {
  return VectorXorInst(__target, __vec1, __vec2, __bw);
}

std::string VectorXorInst::getAssembly() const noexcept {
  return joinTokens(getVectorOpTokens(target, "vxor"s, vec1, vec2, bw));
}

//---------------------------------------------------------
// class VectorShlInst
//---------------------------------------------------------

VectorShlInst::VectorShlInst(const VectorRegister __target,
                             const VectorRegister __vec1,
                             const VectorRegister __vec2,
                             const VectorWidth __bw) noexcept
    : VectorInst(__target, __vec1, __vec2, __bw) {}

VectorShlInst VectorShlInst::create(const VectorRegister __target,
                                    const VectorRegister __vec1,
                                    const VectorRegister __vec2,
                                    const VectorWidth __bw) noexcept {
  return VectorShlInst(__target, __vec1, __vec2, __bw);
}

std::string VectorShlInst::getAssembly() const noexcept {
  return joinTokens(getVectorOpTokens(target, "vshl"s, vec1, vec2, bw));
}

//---------------------------------------------------------
// class VectorLShrInst
//---------------------------------------------------------

VectorLShrInst::VectorLShrInst(const VectorRegister __target,
                               const VectorRegister __vec1,
                               const VectorRegister __vec2,
                               const VectorWidth __bw) noexcept
    : VectorInst(__target, __vec1, __vec2, __bw) {}

VectorLShrInst VectorLShrInst::create(const VectorRegister __target,
                                      const VectorRegister __vec1,
                                      const VectorRegister __vec2,
                                      const VectorWidth __bw) noexcept {
  return VectorLShrInst(__target, __vec1, __vec2, __bw);
}

std::string VectorLShrInst::getAssembly() const noexcept {
  return joinTokens(getVectorOpTokens(target, "vlshr"s, vec1, vec2, bw));
}

//---------------------------------------------------------
// class VectorAShrInst
//---------------------------------------------------------

VectorAShrInst::VectorAShrInst(const VectorRegister __target,
                               const VectorRegister __vec1,
                               const VectorRegister __vec2,
                               const VectorWidth __bw) noexcept
    : VectorInst(__target, __vec1, __vec2, __bw) {}

VectorAShrInst VectorAShrInst::create(const VectorRegister __target,
                                      const VectorRegister __vec1,
                                      const VectorRegister __vec2,
                                      const VectorWidth __bw) noexcept {
  return VectorAShrInst(__target, __vec1, __vec2, __bw);
}

std::string VectorAShrInst::getAssembly() const noexcept {
  return joinTokens(getVectorOpTokens(target, "vashr"s, vec1, vec2, bw));
}

//---------------------------------------------------------
// class VectorIncrInst
//---------------------------------------------------------

VectorIncrInst::VectorIncrInst(const VectorRegister __target,
                               const VectorRegister __vec,
                               const VectorWidth __bw) noexcept
    : AbstractInst(), target(__target), vec(__vec), bw(__bw) {}

VectorIncrInst VectorIncrInst::create(const VectorRegister __target,
                                      const VectorRegister __vec,
                                      const VectorWidth __bw) noexcept {
  return VectorIncrInst(__target, __vec, __bw);
}

std::string VectorIncrInst::getAssembly() const noexcept {
  return joinTokens(
      prependTargetVector(target, collectOpTokens("vincr"s, vec, bw)));
}

//---------------------------------------------------------
// class VectorDecrInst
//---------------------------------------------------------

VectorDecrInst::VectorDecrInst(const VectorRegister __target,
                               const VectorRegister __vec,
                               const VectorWidth __bw) noexcept
    : AbstractInst(), target(__target), vec(__vec), bw(__bw) {}

VectorDecrInst VectorDecrInst::create(const VectorRegister __target,
                                      const VectorRegister __vec,
                                      const VectorWidth __bw) noexcept {
  return VectorDecrInst(__target, __vec, __bw);
}

std::string VectorDecrInst::getAssembly() const noexcept {
  return joinTokens(
      prependTargetVector(target, collectOpTokens("vdecr"s, vec, bw)));
}

//---------------------------------------------------------
// class VectorCompInst
//---------------------------------------------------------

VectorCompInst::VectorCompInst(const VectorRegister __target,
                               const IcmpCondition __cond,
                               const VectorRegister __vec1,
                               const VectorRegister __vec2,
                               const VectorWidth __bw) noexcept
    : VectorInst(__target, __vec1, __vec2, __bw), cond(__cond) {}

VectorCompInst VectorCompInst::create(const VectorRegister __target,
                                      const IcmpCondition __cond,
                                      const VectorRegister __vec1,
                                      const VectorRegister __vec2,
                                      const VectorWidth __bw) noexcept {
  return VectorCompInst(__target, __cond, __vec1, __vec2, __bw);
}

std::string VectorCompInst::getAssembly() const noexcept {
  return joinTokens(prependTargetVector(
      target, collectOpTokens("vicmp"s, cond, vec1, vec2, bw)));
}

//---------------------------------------------------------
// class VectorSelectInst
//---------------------------------------------------------

VectorSelectInst::VectorSelectInst(const VectorRegister __target,
                                   const VectorRegister __vec_cond,
                                   const VectorRegister __vec_true,
                                   const VectorRegister __vec_false,
                                   const VectorWidth __bw) noexcept
    : VectorInst(__target, __vec_true, __vec_false, __bw),
      vec_cond(__vec_cond) {}

VectorSelectInst VectorSelectInst::create(const VectorRegister __target,
                                          const VectorRegister __vec_cond,
                                          const VectorRegister __vec_true,
                                          const VectorRegister __vec_false,
                                          const VectorWidth __bw) noexcept {
  return VectorSelectInst(__target, __vec_cond, __vec_true, __vec_false, __bw);
}

std::string VectorSelectInst::getAssembly() const noexcept {
  return joinTokens(prependTargetVector(
      target, collectOpTokens("vselect"s, vec_cond, vec1, vec2, bw)));
}

// Parallel Arithmetics

//---------------------------------------------------------
// class VectorParallelAddInst
//---------------------------------------------------------
VectorParallelAddInst::VectorParallelAddInst(const VectorRegister __target,
                                             const VectorRegister __vec1,
                                             const VectorRegister __vec2,
                                             const VectorWidth __bw) noexcept
    : VectorInst(__target, __vec1, __vec2, __bw) {}

VectorParallelAddInst VectorParallelAddInst::create(
    const VectorRegister __target, const VectorRegister __vec1,
    const VectorRegister __vec2, const VectorWidth __bw) noexcept {
  return VectorParallelAddInst(__target, __vec1, __vec2, __bw);
}

std::string VectorParallelAddInst::getAssembly() const noexcept {
  return joinTokens(getVectorOpTokens(target, "vpadd"s, vec1, vec2, bw));
}

//---------------------------------------------------------
// class VectorParallelSubInst
//---------------------------------------------------------

VectorParallelSubInst::VectorParallelSubInst(const VectorRegister __target,
                                             const VectorRegister __vec1,
                                             const VectorRegister __vec2,
                                             const VectorWidth __bw) noexcept
    : VectorInst(__target, __vec1, __vec2, __bw) {}

VectorParallelSubInst VectorParallelSubInst::create(
    const VectorRegister __target, const VectorRegister __vec1,
    const VectorRegister __vec2, const VectorWidth __bw) noexcept {
  return VectorParallelSubInst(__target, __vec1, __vec2, __bw);
}

std::string VectorParallelSubInst::getAssembly() const noexcept {
  return joinTokens(getVectorOpTokens(target, "vpsub"s, vec1, vec2, bw));
}

//---------------------------------------------------------
// class VectorParallelMulInst
//---------------------------------------------------------

VectorParallelMulInst::VectorParallelMulInst(const VectorRegister __target,
                                             const VectorRegister __vec1,
                                             const VectorRegister __vec2,
                                             const VectorWidth __bw) noexcept
    : VectorInst(__target, __vec1, __vec2, __bw) {}

VectorParallelMulInst VectorParallelMulInst::create(
    const VectorRegister __target, const VectorRegister __vec1,
    const VectorRegister __vec2, const VectorWidth __bw) noexcept {
  return VectorParallelMulInst(__target, __vec1, __vec2, __bw);
}

std::string VectorParallelMulInst::getAssembly() const noexcept {
  return joinTokens(getVectorOpTokens(target, "vpmul"s, vec1, vec2, bw));
}

//---------------------------------------------------------
// class VectorParallelUDivInst
//---------------------------------------------------------

VectorParallelUDivInst::VectorParallelUDivInst(const VectorRegister __target,
                                               const VectorRegister __vec1,
                                               const VectorRegister __vec2,
                                               const VectorWidth __bw) noexcept
    : VectorInst(__target, __vec1, __vec2, __bw) {}

VectorParallelUDivInst VectorParallelUDivInst::create(
    const VectorRegister __target, const VectorRegister __vec1,
    const VectorRegister __vec2, const VectorWidth __bw) noexcept {
  return VectorParallelUDivInst(__target, __vec1, __vec2, __bw);
}

std::string VectorParallelUDivInst::getAssembly() const noexcept {
  return joinTokens(getVectorOpTokens(target, "vpudiv"s, vec1, vec2, bw));
}

//---------------------------------------------------------
// class VectorParallelSDivInst
//---------------------------------------------------------

VectorParallelSDivInst::VectorParallelSDivInst(const VectorRegister __target,
                                               const VectorRegister __vec1,
                                               const VectorRegister __vec2,
                                               const VectorWidth __bw) noexcept
    : VectorInst(__target, __vec1, __vec2, __bw) {}

VectorParallelSDivInst VectorParallelSDivInst::create(
    const VectorRegister __target, const VectorRegister __vec1,
    const VectorRegister __vec2, const VectorWidth __bw) noexcept {
  return VectorParallelSDivInst(__target, __vec1, __vec2, __bw);
}

std::string VectorParallelSDivInst::getAssembly() const noexcept {
  return joinTokens(getVectorOpTokens(target, "vpsdiv"s, vec1, vec2, bw));
}

//---------------------------------------------------------
// class VectorParallelURemInst
//---------------------------------------------------------

VectorParallelURemInst::VectorParallelURemInst(const VectorRegister __target,
                                               const VectorRegister __vec1,
                                               const VectorRegister __vec2,
                                               const VectorWidth __bw) noexcept
    : VectorInst(__target, __vec1, __vec2, __bw) {}

VectorParallelURemInst VectorParallelURemInst::create(
    const VectorRegister __target, const VectorRegister __vec1,
    const VectorRegister __vec2, const VectorWidth __bw) noexcept {
  return VectorParallelURemInst(__target, __vec1, __vec2, __bw);
}

std::string VectorParallelURemInst::getAssembly() const noexcept {
  return joinTokens(getVectorOpTokens(target, "vpurem"s, vec1, vec2, bw));
}

//---------------------------------------------------------
// class VectorParallelSRemInst
//---------------------------------------------------------

VectorParallelSRemInst::VectorParallelSRemInst(const VectorRegister __target,
                                               const VectorRegister __vec1,
                                               const VectorRegister __vec2,
                                               const VectorWidth __bw) noexcept
    : VectorInst(__target, __vec1, __vec2, __bw) {}

VectorParallelSRemInst VectorParallelSRemInst::create(
    const VectorRegister __target, const VectorRegister __vec1,
    const VectorRegister __vec2, const VectorWidth __bw) noexcept {
  return VectorParallelSRemInst(__target, __vec1, __vec2, __bw);
}

std::string VectorParallelSRemInst::getAssembly() const noexcept {
  return joinTokens(getVectorOpTokens(target, "vpsrem"s, vec1, vec2, bw));
}

//---------------------------------------------------------
// class VectorParallelAndInst
//---------------------------------------------------------

VectorParallelAndInst::VectorParallelAndInst(const VectorRegister __target,
                                             const VectorRegister __vec1,
                                             const VectorRegister __vec2,
                                             const VectorWidth __bw) noexcept
    : VectorInst(__target, __vec1, __vec2, __bw) {}

VectorParallelAndInst VectorParallelAndInst::create(
    const VectorRegister __target, const VectorRegister __vec1,
    const VectorRegister __vec2, const VectorWidth __bw) noexcept {
  return VectorParallelAndInst(__target, __vec1, __vec2, __bw);
}

std::string VectorParallelAndInst::getAssembly() const noexcept {
  return joinTokens(getVectorOpTokens(target, "vpand"s, vec1, vec2, bw));
}

//---------------------------------------------------------
// class VectorParallelOrInst
//---------------------------------------------------------

VectorParallelOrInst::VectorParallelOrInst(const VectorRegister __target,
                                           const VectorRegister __vec1,
                                           const VectorRegister __vec2,
                                           const VectorWidth __bw) noexcept
    : VectorInst(__target, __vec1, __vec2, __bw) {}

VectorParallelOrInst VectorParallelOrInst::create(
    const VectorRegister __target, const VectorRegister __vec1,
    const VectorRegister __vec2, const VectorWidth __bw) noexcept {
  return VectorParallelOrInst(__target, __vec1, __vec2, __bw);
}

std::string VectorParallelOrInst::getAssembly() const noexcept {
  return joinTokens(getVectorOpTokens(target, "vpor"s, vec1, vec2, bw));
}

//---------------------------------------------------------
// class VectorParallelXorInst
//---------------------------------------------------------

VectorParallelXorInst::VectorParallelXorInst(const VectorRegister __target,
                                             const VectorRegister __vec1,
                                             const VectorRegister __vec2,
                                             const VectorWidth __bw) noexcept
    : VectorInst(__target, __vec1, __vec2, __bw) {}

VectorParallelXorInst VectorParallelXorInst::create(
    const VectorRegister __target, const VectorRegister __vec1,
    const VectorRegister __vec2, const VectorWidth __bw) noexcept {
  return VectorParallelXorInst(__target, __vec1, __vec2, __bw);
}

std::string VectorParallelXorInst::getAssembly() const noexcept {
  return joinTokens(getVectorOpTokens(target, "vpxor"s, vec1, vec2, bw));
}

//---------------------------------------------------------
// class VectorParallelShlInst
//---------------------------------------------------------

VectorParallelShlInst::VectorParallelShlInst(const VectorRegister __target,
                                             const VectorRegister __vec1,
                                             const VectorRegister __vec2,
                                             const VectorWidth __bw) noexcept
    : VectorInst(__target, __vec1, __vec2, __bw) {}

VectorParallelShlInst VectorParallelShlInst::create(
    const VectorRegister __target, const VectorRegister __vec1,
    const VectorRegister __vec2, const VectorWidth __bw) noexcept {
  return VectorParallelShlInst(__target, __vec1, __vec2, __bw);
}

std::string VectorParallelShlInst::getAssembly() const noexcept {
  return joinTokens(getVectorOpTokens(target, "vpshl"s, vec1, vec2, bw));
}

//---------------------------------------------------------
// class VectorParallelLShrInst
//---------------------------------------------------------

VectorParallelLShrInst::VectorParallelLShrInst(const VectorRegister __target,
                                               const VectorRegister __vec1,
                                               const VectorRegister __vec2,
                                               const VectorWidth __bw) noexcept
    : VectorInst(__target, __vec1, __vec2, __bw) {}

VectorParallelLShrInst VectorParallelLShrInst::create(
    const VectorRegister __target, const VectorRegister __vec1,
    const VectorRegister __vec2, const VectorWidth __bw) noexcept {
  return VectorParallelLShrInst(__target, __vec1, __vec2, __bw);
}

std::string VectorParallelLShrInst::getAssembly() const noexcept {
  return joinTokens(getVectorOpTokens(target, "vplshr"s, vec1, vec2, bw));
}

//---------------------------------------------------------
// class VectorParallelAShrInst
//---------------------------------------------------------

VectorParallelAShrInst::VectorParallelAShrInst(const VectorRegister __target,
                                               const VectorRegister __vec1,
                                               const VectorRegister __vec2,
                                               const VectorWidth __bw) noexcept
    : VectorInst(__target, __vec1, __vec2, __bw) {}

VectorParallelAShrInst VectorParallelAShrInst::create(
    const VectorRegister __target, const VectorRegister __vec1,
    const VectorRegister __vec2, const VectorWidth __bw) noexcept {
  return VectorParallelAShrInst(__target, __vec1, __vec2, __bw);
}

std::string VectorParallelAShrInst::getAssembly() const noexcept {
  return joinTokens(getVectorOpTokens(target, "vpashr"s, vec1, vec2, bw));
}

//---------------------------------------------------------
// class VectorParallelCompInst
//---------------------------------------------------------

VectorParallelCompInst::VectorParallelCompInst(const VectorRegister __target,
                                               const IcmpCondition __cond,
                                               const VectorRegister __vec1,
                                               const VectorRegister __vec2,
                                               const VectorWidth __bw) noexcept
    : VectorInst(__target, __vec1, __vec2, __bw), cond(__cond) {}

VectorParallelCompInst VectorParallelCompInst::create(
    const VectorRegister __target, const IcmpCondition __cond,
    const VectorRegister __vec1, const VectorRegister __vec2,
    const VectorWidth __bw) noexcept {
  return VectorParallelCompInst(__target, __cond, __vec1, __vec2, __bw);
}

std::string VectorParallelCompInst::getAssembly() const noexcept {
  return joinTokens(prependTargetVector(
      target, collectOpTokens("vpicmp"s, cond, vec1, vec2, bw)));
}

//---------------------------------------------------------
// class VectorParallelSelectInst
//---------------------------------------------------------

VectorParallelSelectInst::VectorParallelSelectInst(
    const VectorRegister __target, const VectorRegister __vec_cond,
    const VectorRegister __vec1, const VectorRegister __vec2,
    const VectorWidth __bw) noexcept
    : VectorInst(__target, __vec1, __vec2, __bw), vec_cond(__vec_cond) {}

VectorParallelSelectInst VectorParallelSelectInst::create(
    const VectorRegister __target, const VectorRegister __vec_cond,
    const VectorRegister __vec1, const VectorRegister __vec2,
    const VectorWidth __bw) noexcept {
  return VectorParallelSelectInst(__target, __vec_cond, __vec1, __vec2, __bw);
}

std::string VectorParallelSelectInst::getAssembly() const noexcept {
  return joinTokens(prependTargetVector(
      target, collectOpTokens("vpselect"s, vec_cond, vec1, vec2, bw)));
}

//---------------------------------------------------------
// class VectorBroadcastInst
//---------------------------------------------------------

VectorBroadcastInst::VectorBroadcastInst(const VectorRegister __target,
                                         const ScalarRegisterTy __val,
                                         const VectorWidth __bw) noexcept
    : AbstractInst(), target(__target), val(__val), bw(__bw) {}

VectorBroadcastInst
VectorBroadcastInst::create(const VectorRegister __target,
                            const ScalarRegisterTy __val,
                            const VectorWidth __bw) noexcept {
  return VectorBroadcastInst(__target, __val, __bw);
}

std::string VectorBroadcastInst::getAssembly() const noexcept {
  return joinTokens(
      prependTargetVector(target, collectOpTokens("vbcast"s, val, bw)));
}

//---------------------------------------------------------
// class VectorExtractInst
//---------------------------------------------------------

VectorExtractInst::VectorExtractInst(const GeneralRegister __target,
                                     const VectorRegister __vec,
                                     const ScalarRegisterTy __idx,
                                     const VectorWidth __bw) noexcept
    : AbstractInst(), target(__target), vec(__vec), idx(__idx), bw(__bw) {}

VectorExtractInst VectorExtractInst::create(const GeneralRegister __target,
                                            const VectorRegister __vec,
                                            const ScalarRegisterTy __idx,
                                            const VectorWidth __bw) noexcept {
  return VectorExtractInst(__target, __vec, __idx, __bw);
}

std::string VectorExtractInst::getAssembly() const noexcept {
  return joinTokens(
      prependTarget(target, collectOpTokens("vextct"s, vec, idx, bw)));
}

//---------------------------------------------------------
// class VectorUpdateInst
//---------------------------------------------------------

VectorUpdateInst::VectorUpdateInst(const VectorRegister __target,
                                   const VectorRegister __vec,
                                   const ScalarRegisterTy __val,
                                   const ScalarRegisterTy __idx,
                                   const VectorWidth __bw) noexcept
    : AbstractInst(), target(__target), vec(__vec), val(__val), idx(__idx),
      bw(__bw) {}

VectorUpdateInst VectorUpdateInst::create(const VectorRegister __target,
                                          const VectorRegister __vec,
                                          const ScalarRegisterTy __val,
                                          const ScalarRegisterTy __idx,
                                          const VectorWidth __bw) noexcept {
  return VectorUpdateInst(__target, __vec, __val, __idx, __bw);
}

std::string VectorUpdateInst::getAssembly() const noexcept {
  return joinTokens(prependTargetVector(
      target, collectOpTokens("vupdate"s, vec, val, idx, bw)));
}

} // namespace sc::backend::assembly::inst
