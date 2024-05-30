#include "emitter.h"

#include "arch.h"
#include "assembly.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/Support/Casting.h"

#include <charconv>
#include <concepts>
#include <format>
#include <map>
#include <numeric>
#include <optional>
#include <variant>

using namespace std::string_literals;
using namespace static_error;

namespace {
const sc::backend::symbol::SymbolMap *__SM;

using namespace sc::backend::assembly;

std::string collectStrings(const std::vector<std::string> &__lines) noexcept {
  const auto collection_len =
      std::accumulate(__lines.cbegin(), __lines.cend(), (size_t)0,
                      [](const size_t len, const auto &line) -> size_t {
                        // +1 for \n
                        return len + line.size() + 1;
                      });

  std::string collected_string;
  collected_string.reserve(collection_len);
  for (const auto &line : __lines) {
    collected_string.append(line + "\n"s);
  }

  return collected_string;
}

class UnresolvedSymbolError : public Error<UnresolvedSymbolError> {
private:
  std::string message;

public:
  UnresolvedSymbolError(llvm::Value *const __value) noexcept {
    message = "unresolved symbol error: "s.append(__value->getName())
                  .append(" does not exist in the symbol map"s);
  }

  const char *what() const noexcept { return message.c_str(); }
};

std::expected<std::string, UnresolvedSymbolError>
tryGetName(llvm::Value *const __value) noexcept {
  using namespace llvm;
  using RetType = std::expected<std::string, UnresolvedSymbolError>;

  if (!__value || __value->getType()->isVoidTy()) {
    return RetType("0"s);
  } else if (isa<ConstantPointerNull>(__value)) {
    return RetType(std::format("{}", NULL_PTR));
  } else if (isa<ConstantInt>(__value)) {
    // return the value itself.
    return RetType(
        std::to_string(dyn_cast<ConstantInt>(__value)->getZExtValue()));
  } else {
    const auto sym_ptr = __SM->getSymbol(__value);
    if (!sym_ptr) {
      return RetType::unexpected_type(UnresolvedSymbolError(__value));
    } else {
      return RetType(sym_ptr->getName());
    }
  }
}

class CalculateBitWidthError : public Error<CalculateBitWidthError> {
private:
  std::string message;

public:
  CalculateBitWidthError(llvm::Type *const __ty) noexcept {
    message = "bitwidth calculating error: unable to calculate Bitwidth from "s
                  .append(__ty->getStructName());
  }

  const char *what() const noexcept { return message.c_str(); }
};

// TODO: Add Vector Bitwidth?
std::expected<BitWidth, CalculateBitWidthError>
tryCalculateBitWidth(llvm::Type *const __ty) noexcept {
  using RetType = std::expected<BitWidth, CalculateBitWidthError>;

  if (__ty->isPointerTy()) {
    return RetType(BitWidth::QWORD);
  } else if (!__ty->isIntegerTy()) {
    return RetType::unexpected_type(CalculateBitWidthError(__ty));
  }

  switch (__ty->getIntegerBitWidth()) {
  case 1:
    return RetType(BitWidth::BIT);
  case 8:
    return RetType(BitWidth::BYTE);
  case 16:
    return RetType(BitWidth::WORD);
  case 32:
    return RetType(BitWidth::DWORD);
  case 64:
    return RetType(BitWidth::QWORD);
  default:
    return RetType::unexpected_type(CalculateBitWidthError(__ty));
  }
}

class CalculateAccessWidthError : public Error<CalculateAccessWidthError> {
private:
  std::string message;

public:
  CalculateAccessWidthError(llvm::Type *const __ty) noexcept {
    message =
        "access width calculating error: unable to calculate AccessWidth from "s
            .append(__ty->getStructName());
  }

  const char *what() const noexcept { return message.c_str(); }
};

std::expected<AccessWidth, CalculateAccessWidthError>
tryCalculateAccessWidth(llvm::Type *const __ty) noexcept {
  using RetType = std::expected<AccessWidth, CalculateAccessWidthError>;

  if (__ty->isPointerTy()) {
    return RetType(AccessWidth::QWORD);
  } else if (__ty->isIntegerTy()) {
    switch (__ty->getIntegerBitWidth()) {
    case 8:
      return RetType(AccessWidth::BYTE);
    case 16:
      return RetType(AccessWidth::WORD);
    case 32:
      return RetType(AccessWidth::DWORD);
    case 64:
      return RetType(AccessWidth::QWORD);
    default:
      return RetType::unexpected_type(CalculateAccessWidthError(__ty));
    }
  } else if (__ty->isVectorTy()) {
    return RetType(AccessWidth::VECTOR);
  } else {
    return RetType::unexpected_type(CalculateAccessWidthError(__ty));
  }
}

class CalculateVectorWidthError : public Error<CalculateVectorWidthError> {
private:
  std::string message;

public:
  CalculateVectorWidthError(llvm::Type *const __ty) noexcept {
    message =
        "vector width calculating error: unable to calculate VectorWidth from "s
            .append(__ty->getStructName());
  }

  const char *what() const noexcept { return message.c_str(); }
};

std::expected<VectorWidth, CalculateVectorWidthError>
tryCalculateVectorWidth(llvm::Type *const __ty) noexcept {
  using RetType = std::expected<VectorWidth, CalculateVectorWidthError>;

  const auto vec_ty = llvm::dyn_cast<llvm::VectorType>(__ty);
  if (vec_ty && vec_ty->getElementType()->isIntegerTy()) {
    switch (vec_ty->getElementType()->getIntegerBitWidth()) {
    case 32:
      return RetType(VectorWidth::DWORD);
    case 64:
      return RetType(VectorWidth::QWORD);
    default:
      return RetType::unexpected_type(CalculateVectorWidthError(__ty));
    }
  } else {
    return RetType::unexpected_type(CalculateVectorWidthError(__ty));
  }
}

const std::map<std::string, IcmpCondition, std::less<>> icmp_table = {
    {"eq"s, IcmpCondition::EQ},   {"ne"s, IcmpCondition::NE},
    {"ugt"s, IcmpCondition::UGT}, {"uge"s, IcmpCondition::UGE},
    {"ult"s, IcmpCondition::ULT}, {"ule"s, IcmpCondition::ULE},
    {"sgt"s, IcmpCondition::SGT}, {"sge"s, IcmpCondition::SGE},
    {"slt"s, IcmpCondition::SLT}, {"sle"s, IcmpCondition::SLE},
};

class ParseIcmpConditionError : public Error<ParseIcmpConditionError> {
private:
  std::string message;

public:
  ParseIcmpConditionError(const std::string_view __op_str) noexcept {
    message = "icmp condition parsing error: unable to parse "s.append(__op_str)
                  .append(" into IcmpCondition"s);
  }

  const char *what() const noexcept { return message.c_str(); }
};

std::expected<IcmpCondition, ParseIcmpConditionError>
tryParseIcmpCondition(const std::string_view __op_str) noexcept {
  using RetType = std::expected<IcmpCondition, ParseIcmpConditionError>;

  const auto itr = icmp_table.find(__op_str);
  if (itr == icmp_table.cend()) {
    return RetType::unexpected_type(ParseIcmpConditionError(__op_str));
  }
  return RetType(itr->second);
}

const std::map<std::string, GeneralRegister, std::less<>> general_reg_table = {
    {"r1"s, GeneralRegister::R1},   {"r2"s, GeneralRegister::R2},
    {"r3"s, GeneralRegister::R3},   {"r4"s, GeneralRegister::R4},
    {"r5"s, GeneralRegister::R5},   {"r6"s, GeneralRegister::R6},
    {"r7"s, GeneralRegister::R7},   {"r8"s, GeneralRegister::R8},
    {"r9"s, GeneralRegister::R9},   {"r10"s, GeneralRegister::R10},
    {"r11"s, GeneralRegister::R11}, {"r12"s, GeneralRegister::R12},
    {"r13"s, GeneralRegister::R13}, {"r14"s, GeneralRegister::R14},
    {"r15"s, GeneralRegister::R15}, {"r16"s, GeneralRegister::R16},
    {"r17"s, GeneralRegister::R17}, {"r18"s, GeneralRegister::R18},
    {"r19"s, GeneralRegister::R19}, {"r20"s, GeneralRegister::R20},
    {"r21"s, GeneralRegister::R21}, {"r22"s, GeneralRegister::R22},
    {"r23"s, GeneralRegister::R23}, {"r24"s, GeneralRegister::R24},
    {"r25"s, GeneralRegister::R25}, {"r26"s, GeneralRegister::R26},
    {"r27"s, GeneralRegister::R27}, {"r28"s, GeneralRegister::R28},
    {"r29"s, GeneralRegister::R29}, {"r30"s, GeneralRegister::R30},
    {"r31"s, GeneralRegister::R31}, {"r32"s, GeneralRegister::R32},
    {"sp"s, GeneralRegister::SP}};

class ParseGeneralRegisterError : public Error<ParseGeneralRegisterError> {
private:
  std::string message;

public:
  ParseGeneralRegisterError(const std::string_view __vreg_str) noexcept {
    message = std::format("unable to parse {} as GeneralRegister", __vreg_str);
  }

  const char *what() const noexcept { return message.c_str(); }
};

std::expected<GeneralRegister, ParseGeneralRegisterError>
tryParseGeneralRegister(const std::string_view __vreg_str) noexcept {
  using RetType = std::expected<GeneralRegister, ParseGeneralRegisterError>;

  const auto itr = general_reg_table.find(__vreg_str);
  if (itr == general_reg_table.cend()) {
    return RetType::unexpected_type(ParseGeneralRegisterError(__vreg_str));
  }

  return RetType(itr->second);
}

const std::map<std::string, ArgumentRegister, std::less<>> argument_reg_table =
    {
        {"arg1"s, ArgumentRegister::A1},   {"arg2"s, ArgumentRegister::A2},
        {"arg3"s, ArgumentRegister::A3},   {"arg4"s, ArgumentRegister::A4},
        {"arg5"s, ArgumentRegister::A5},   {"arg6"s, ArgumentRegister::A6},
        {"arg7"s, ArgumentRegister::A7},   {"arg8"s, ArgumentRegister::A8},
        {"arg9"s, ArgumentRegister::A9},   {"arg10"s, ArgumentRegister::A10},
        {"arg11"s, ArgumentRegister::A11}, {"arg12"s, ArgumentRegister::A12},
        {"arg13"s, ArgumentRegister::A13}, {"arg14"s, ArgumentRegister::A14},
        {"arg15"s, ArgumentRegister::A15}, {"arg16"s, ArgumentRegister::A16},
};

class ParseArgumentRegisterError : public Error<ParseArgumentRegisterError> {
private:
  std::string message;

public:
  ParseArgumentRegisterError(const std::string_view __vreg_str) noexcept {
    message = std::format("unable to parse {} as ArgumentRegister", __vreg_str);
  }

  const char *what() const noexcept { return message.c_str(); }
};

std::expected<ArgumentRegister, ParseArgumentRegisterError>
tryParseArgumentRegister(const std::string_view __vreg_str) noexcept {
  using RetType = std::expected<ArgumentRegister, ParseArgumentRegisterError>;

  const auto itr = argument_reg_table.find(__vreg_str);
  if (itr == argument_reg_table.cend()) {
    return RetType::unexpected_type(ParseArgumentRegisterError(__vreg_str));
  }

  return RetType(itr->second);
}

class ParseScalarRegisterError : public Error<ParseScalarRegisterError> {
private:
  std::string message;

public:
  ParseScalarRegisterError(ParseGeneralRegisterError &&__err) noexcept {
    message =
        std::format("unable to parse as ScalarRegister\n({})", __err.what());
  }

  ParseScalarRegisterError(ParseArgumentRegisterError &&__err) noexcept {
    message =
        std::format("unable to parse as ScalarRegister\n({})", __err.what());
  }

  const char *what() const noexcept { return message.c_str(); }
};

std::expected<ScalarRegisterTy, ParseScalarRegisterError>
tryParseScalarRegister(const std::string_view __value_str) noexcept {
  using RetType = std::expected<ScalarRegisterTy, ParseScalarRegisterError>;

  return tryParseGeneralRegister(__value_str)
      .transform([](const auto reg) { return ScalarRegisterTy(reg); })
      .or_else([__value_str](auto &&_) {
        return tryParseArgumentRegister(__value_str)
            .transform([](const auto reg) { return ScalarRegisterTy(reg); });
      })
      .transform_error(
          [](auto &&err) { return ParseScalarRegisterError(std::move(err)); });
}

class ParseIntError : public Error<ParseIntError> {
private:
  std::string message;

public:
  ParseIntError(const std::string_view __int_str) noexcept {
    message = std::format("unable to parse {} as IntTy", __int_str);
  }

  const char *what() const noexcept { return message.c_str(); }
};

std::expected<IntTy, ParseIntError>
tryParseInt(const std::string_view __int_str) {
  using RetType = std::expected<IntTy, ParseIntError>;

  const auto str_ptr = __int_str.data();
  IntTy value;
  const auto fc_res =
      std::from_chars(str_ptr, str_ptr + __int_str.size(), value);
  if (fc_res.ptr != str_ptr + __int_str.size()) {
    return RetType::unexpected_type(ParseIntError(__int_str));
  }

  return RetType(value);
}

class ParseScalarValueError : public Error<ParseScalarValueError> {
private:
  std::string message;

public:
  ParseScalarValueError(ParseScalarRegisterError &&__err) noexcept {
    message = std::format("unable to parse as ScalarValue\n({})", __err.what());
  }

  ParseScalarValueError(ParseIntError &&__err) noexcept {
    message = std::format("unable to parse as ScalarValue\n({})", __err.what());
  }

  const char *what() const noexcept { return message.c_str(); }
};

std::expected<ScalarValueTy, ParseScalarValueError>
tryParseScalarValue(const std::string_view __value_str) noexcept {
  using RetType = std::expected<ScalarValueTy, ParseScalarValueError>;

  return tryParseScalarRegister(__value_str)
      .transform([](const auto reg) { return ScalarValueTy(reg); })
      .or_else([__value_str](auto &&_) {
        return tryParseInt(__value_str).transform([](const auto reg) {
          return ScalarValueTy(reg);
        });
      })
      .transform_error(
          [](auto &&err) { return ParseScalarValueError(std::move(err)); });
}

// TODO: VectorRegister

const std::map<std::string, VectorRegister, std::less<>> vector_reg_table = {
    {"v1"s, VectorRegister::V1},   {"v2"s, VectorRegister::V2},
    {"v3"s, VectorRegister::V3},   {"v4"s, VectorRegister::V4},
    {"v5"s, VectorRegister::V5},   {"v6"s, VectorRegister::V6},
    {"ve"s, VectorRegister::V7},   {"v8"s, VectorRegister::V8},
    {"v9"s, VectorRegister::V9},   {"v10"s, VectorRegister::V10},
    {"v11"s, VectorRegister::V11}, {"v12"s, VectorRegister::V12},
    {"v13"s, VectorRegister::V13}, {"v14"s, VectorRegister::V14},
    {"v15"s, VectorRegister::V15}, {"v16"s, VectorRegister::V16},
};

class ParseVectorRegisterError : public Error<ParseVectorRegisterError> {
private:
  std::string message;

public:
  ParseVectorRegisterError(const std::string_view __vreg_str) noexcept {
    message = std::format("unable to parse {} as VectorRegister", __vreg_str);
  }

  const char *what() const noexcept { return message.c_str(); }
};

std::expected<VectorRegister, ParseVectorRegisterError>
tryParseVectorRegister(const std::string_view __vreg_str) noexcept {
  using RetType = std::expected<VectorRegister, ParseVectorRegisterError>;

  const auto itr = vector_reg_table.find(__vreg_str);
  if (itr == vector_reg_table.cend()) {
    return RetType::unexpected_type(ParseVectorRegisterError(__vreg_str));
  }

  return RetType(itr->second);
}

class IllFormedInstError : public Error<IllFormedInstError> {
private:
  std::string message;

public:
  IllFormedInstError(const std::string_view __message) noexcept {
    message = "ill-formed instruction error: "s.append(__message);
  }

  const char *what() const noexcept { return message.c_str(); }
};

template <typename E>
class ErrorWithInstruction : public Error<ErrorWithInstruction<E>> {
private:
  std::string message;

public:
  ErrorWithInstruction(E &&__err, const llvm::Instruction &__inst) noexcept {
    std::string inst;
    llvm::raw_string_ostream rso(inst);
    rso << __inst;

    message =
        std::string(__err.what()).append("\n["s).append(inst).append("]"s);
  }

  const char *what() const noexcept { return message.c_str(); }
};

template <typename E>
class ErrorWithBasicBlock : public Error<ErrorWithBasicBlock<E>> {
private:
  std::string message;

public:
  ErrorWithBasicBlock(E &&__err, const llvm::BasicBlock &__bblock) noexcept {
    std::string bblock;
    llvm::raw_string_ostream rso(bblock);
    rso << __bblock;

    message =
        std::string(__err.what()).append("\n["s).append(bblock).append("]"s);
  }

  const char *what() const noexcept { return message.c_str(); }
};

template <typename E>
class ErrorWithFunction : public Error<ErrorWithFunction<E>> {
private:
  std::string message;

public:
  ErrorWithFunction(E &&__err, const llvm::Function &__func) noexcept {
    std::string func;
    llvm::raw_string_ostream rso(func);
    rso << __func;

    message =
        std::string(__err.what()).append("\n["s).append(func).append("]"s);
  }

  const char *what() const noexcept { return message.c_str(); }
};

template <std::movable T, ErrorLike E>
T unwrapOrThrowWithInst(std::expected<T, E> &&__res,
                        const llvm::Instruction &__inst) {
  using ResType = std::expected<T, E>;

  if (!__res.has_value()) {
    auto err = __res.error();
    throw ErrorWithInstruction(std::move(err), __inst);
  }

  return std::move(__res).value();
}

template <std::movable T, ErrorLike E>
T unwrapOrThrowWithBB(std::expected<T, E> &&__res,
                      const llvm::BasicBlock &__bblock) {
  using ResType = std::expected<T, E>;

  if (!__res.has_value()) {
    auto err = __res.error();
    throw ErrorWithBasicBlock(std::move(err), __bblock);
  }

  return std::move(__res).value();
}

template <std::movable T, ErrorLike E>
T unwrapOrThrowWithFunc(std::expected<T, E> &&__res,
                        const llvm::Function &__func) {
  using ResType = std::expected<T, E>;

  if (!__res.has_value()) {
    auto err = __res.error();
    throw ErrorWithFunction(std::move(err), __func);
  }

  return std::move(__res).value();
}

class IllFormedIntrinsicError : public Error<IllFormedIntrinsicError> {
private:
  std::string message;

public:
  IllFormedIntrinsicError(llvm::CallInst &__inst) noexcept {
    message = "ill-formed intrinsic error: signature of "s
                  .append(__inst.getCalledFunction()->getName())
                  .append(" does not match the definition");
  }

  const char *what() const noexcept { return message.c_str(); }
};

bool isMallocIntrinsic(llvm::CallInst &__inst) noexcept {
  const auto fn_name = __inst.getCalledFunction()->getName();
  return (fn_name == "malloc");
}

std::string emitFromMallocIntrinsic(llvm::CallInst &__inst) {
  const auto target_str = unwrapOrThrowWithInst(tryGetName(&__inst), __inst);
  const auto target =
      unwrapOrThrowWithInst(tryParseGeneralRegister(target_str), __inst);

  const auto size_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(0)), __inst);
  const auto size =
      unwrapOrThrowWithInst(tryParseScalarRegister(size_str), __inst);

  const auto inst = sc::backend::assembly::MallocInst::create(target, size);
  return inst.getAssembly();
}

bool isFreeIntrinsic(llvm::CallInst &__inst) noexcept {
  const auto fn_name = __inst.getCalledFunction()->getName();
  return (fn_name == "free");
}

std::string emitFromFreeIntrinsic(llvm::CallInst &__inst) {
  const auto ptr_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(0)), __inst);
  auto ptr = unwrapOrThrowWithInst(tryParseScalarRegister(ptr_str), __inst);

  const auto inst = sc::backend::assembly::FreeInst::create(std::move(ptr));
  return inst.getAssembly();
}

bool isDecrSPIntrinsic(llvm::CallInst &__inst) noexcept {
  const auto fn_name = __inst.getCalledFunction()->getName();
  return (fn_name == "$decr_sp");
}

std::string emitFromDecrSPIntrinsic(llvm::CallInst &__inst) {
  const auto arg_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(0)), __inst);
  auto arg = unwrapOrThrowWithInst(tryParseScalarValue(arg_str), __inst);

  if (std::holds_alternative<IntTy>(arg)) {
    const auto const_arg = std::get<IntTy>(arg);
    const auto dummy_str = unwrapOrThrowWithInst(tryGetName(&__inst), __inst);
    const auto dummy =
        unwrapOrThrowWithInst(tryParseGeneralRegister(dummy_str), __inst);
    const auto const_inst =
        sc::backend::assembly::ConstantInst::create(dummy, const_arg);
    const auto inst = sc::backend::assembly::IntSubInst::create(
        GeneralRegister::SP, BitWidth::QWORD, GeneralRegister::SP, dummy);
    return std::format("{}\n{}", const_inst.getAssembly(), inst.getAssembly());
  } else {
    const auto scalar_arg = std::get<ScalarRegisterTy>(arg);
    const auto inst = sc::backend::assembly::IntSubInst::create(
        GeneralRegister::SP, BitWidth::QWORD, GeneralRegister::SP, scalar_arg);
    return std::format("{}", inst.getAssembly());
  }
}

bool isAsyncLoadIntrinsic(llvm::CallInst &__inst) noexcept {
  const auto fn_name = __inst.getCalledFunction()->getName();
  if (fn_name == "aload_i8" || fn_name == "aload_i16" ||
      fn_name == "aload_i32" || fn_name == "aload_i64") {
    return true;
  } else {
    return false;
  }
}

std::expected<AccessWidth, IllFormedIntrinsicError>
tryCalculateAsyncLoadAccessWidth(llvm::CallInst &__inst) noexcept {
  using RetType = std::expected<AccessWidth, IllFormedIntrinsicError>;

  const auto ret_ty = __inst.getType();
  if (ret_ty->isIntegerTy() && __inst.arg_size() == 1) {
    const auto arg_ty =
        llvm::dyn_cast<llvm::PointerType>(__inst.getArgOperand(0)->getType());
    if (arg_ty && arg_ty->isLoadableOrStorableType(ret_ty)) {
      auto aw_res = tryCalculateAccessWidth(ret_ty);
      return aw_res.transform_error(
          [&__inst](auto &&err) { return IllFormedIntrinsicError(__inst); });
    }
  }
  return RetType::unexpected_type(IllFormedIntrinsicError(__inst));
}

std::string emitFromAsyncLoadIntrinsic(llvm::CallInst &__inst) {
  const auto size =
      unwrapOrThrowWithInst(tryCalculateAsyncLoadAccessWidth(__inst), __inst);

  const auto target_str = unwrapOrThrowWithInst(tryGetName(&__inst), __inst);
  const auto target =
      unwrapOrThrowWithInst(tryParseGeneralRegister(target_str), __inst);

  const auto ptr_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(0)), __inst);
  auto ptr = unwrapOrThrowWithInst(tryParseScalarRegister(ptr_str), __inst);

  const auto inst = sc::backend::assembly::AsyncLoadInst::create(
      target, size, std::move(ptr));
  return inst.getAssembly();
}

bool isIntSumIntrinsic(llvm::CallInst &__inst) noexcept {
  const auto fn_name = __inst.getCalledFunction()->getName();
  if (fn_name == "int_sum_i1" || fn_name == "int_sum_i8" ||
      fn_name == "int_sum_i16" || fn_name == "int_sum_i32" ||
      fn_name == "int_sum_i64") {
    return true;
  } else {
    return false;
  }
}

std::expected<BitWidth, IllFormedIntrinsicError>
tryCalculateIntSumBitWidth(llvm::CallInst &__inst) noexcept {
  using RetType = std::expected<BitWidth, IllFormedIntrinsicError>;

  const auto ret_ty = __inst.getType();
  if (ret_ty->isIntegerTy() && __inst.arg_size() == 8) {
    const auto is_tys_match =
        std::accumulate(__inst.arg_begin(), __inst.arg_end(), true,
                        [ret_ty](const bool acc, const auto &arg) {
                          return acc && (ret_ty == arg->getType());
                        });
    if (is_tys_match) {
      auto bw_res = tryCalculateBitWidth(ret_ty);
      return bw_res.transform_error(
          [&__inst](auto &&err) { return IllFormedIntrinsicError(__inst); });
    }
  }
  return RetType::unexpected_type(IllFormedIntrinsicError(__inst));
}

std::string emitFromIntSumIntrinsic(llvm::CallInst &__inst) {
  const auto bw =
      unwrapOrThrowWithInst(tryCalculateIntSumBitWidth(__inst), __inst);

  const auto target_str = unwrapOrThrowWithInst(tryGetName(&__inst), __inst);
  const auto target =
      unwrapOrThrowWithInst(tryParseGeneralRegister(target_str), __inst);

  std::vector<ScalarRegisterTy> args;
  args.reserve(__inst.arg_size());
  std::transform(
      __inst.arg_begin(), __inst.arg_end(), std::back_inserter(args),
      [&__inst](auto &arg) {
        const auto arg_str = unwrapOrThrowWithInst(tryGetName(arg), __inst);
        return unwrapOrThrowWithInst(tryParseScalarRegister(arg_str), __inst);
      });

  const auto inst = unwrapOrThrowWithInst(
      sc::backend::assembly::IntSumInst::tryCreate(target, std::move(args), bw),
      __inst);
  return inst.getAssembly();
}

bool isIntIncrIntrinsic(llvm::CallInst &__inst) noexcept {
  const auto fn_name = __inst.getCalledFunction()->getName();
  if (fn_name == "incr_i1" || fn_name == "incr_i8" || fn_name == "incr_i16" ||
      fn_name == "incr_i32" || fn_name == "incr_i64") {
    return true;
  } else {
    return false;
  }
}

std::expected<BitWidth, IllFormedIntrinsicError>
tryCalculateIntIncrDecrBitWidth(llvm::CallInst &__inst) noexcept {
  using RetType = std::expected<BitWidth, IllFormedIntrinsicError>;

  const auto ret_ty = __inst.getType();
  if (ret_ty->isIntegerTy() && __inst.arg_size() == 1) {
    const auto arg_ty = __inst.getArgOperand(0)->getType();
    if (ret_ty == arg_ty) {
      auto bw_res = tryCalculateBitWidth(ret_ty);
      return bw_res.transform_error(
          [&__inst](auto &&err) { return IllFormedIntrinsicError(__inst); });
    }
  }
  return RetType::unexpected_type(IllFormedIntrinsicError(__inst));
}

std::string emitFromIntIncrIntrinsic(llvm::CallInst &__inst) {
  const auto bw =
      unwrapOrThrowWithInst(tryCalculateIntIncrDecrBitWidth(__inst), __inst);

  const auto target_str = unwrapOrThrowWithInst(tryGetName(&__inst), __inst);
  const auto target =
      unwrapOrThrowWithInst(tryParseGeneralRegister(target_str), __inst);

  const auto arg_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(0)), __inst);
  auto arg = unwrapOrThrowWithInst(tryParseScalarRegister(arg_str), __inst);

  const auto inst = sc::backend::assembly::IntIncrInst::create(target, arg, bw);
  return inst.getAssembly();
}

bool isIntDecrIntrinsic(llvm::CallInst &__inst) noexcept {
  const auto fn_name = __inst.getCalledFunction()->getName();
  if (fn_name == "decr_i1" || fn_name == "decr_i8" || fn_name == "decr_i16" ||
      fn_name == "decr_i32" || fn_name == "decr_i64") {
    return true;
  } else {
    return false;
  }
}

std::string emitFromIntDecrIntrinsic(llvm::CallInst &__inst) {
  const auto bw =
      unwrapOrThrowWithInst(tryCalculateIntIncrDecrBitWidth(__inst), __inst);

  const auto target_str = unwrapOrThrowWithInst(tryGetName(&__inst), __inst);
  const auto target =
      unwrapOrThrowWithInst(tryParseGeneralRegister(target_str), __inst);

  const auto arg_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(0)), __inst);
  auto arg = unwrapOrThrowWithInst(tryParseScalarRegister(arg_str), __inst);

  const auto inst = sc::backend::assembly::IntDecrInst::create(target, arg, bw);
  return inst.getAssembly();
}

bool isIntAssertionIntrinsic(llvm::CallInst &__inst) {
  const auto fn_name = __inst.getCalledFunction()->getName();
  if (fn_name == "assert_eq_i1" || fn_name == "assert_eq_i8" ||
      fn_name == "assert_eq_i16" || fn_name == "assert_eq_i32" ||
      fn_name == "assert_eq_i64") {
    return true;
  } else {
    return false;
  }
}

std::expected<BitWidth, IllFormedIntrinsicError>
tryCalculateIntAssertionBitWidth(llvm::CallInst &__inst) noexcept {
  using RetType = std::expected<BitWidth, IllFormedIntrinsicError>;

  llvm::Type *assert_ty = nullptr;
  auto &ctx = __inst.getContext();

  const auto fn_name = __inst.getCalledFunction()->getName();
  if (fn_name == "assert_eq_i1") {
    assert_ty = llvm::Type::getInt1Ty(ctx);
  } else if (fn_name == "assert_eq_i8") {
    assert_ty = llvm::Type::getInt8Ty(ctx);
  } else if (fn_name == "assert_eq_i16") {
    assert_ty = llvm::Type::getInt16Ty(ctx);
  } else if (fn_name == "assert_eq_i32") {
    assert_ty = llvm::Type::getInt32Ty(ctx);
  } else if (fn_name == "assert_eq_i64") {
    assert_ty = llvm::Type::getInt64Ty(ctx);
  }

  if (assert_ty && __inst.arg_size() == 2) {
    const auto is_tys_match =
        std::accumulate(__inst.arg_begin(), __inst.arg_end(), true,
                        [assert_ty](const bool acc, const auto &arg) {
                          return acc && (assert_ty == arg->getType());
                        });
    if (is_tys_match) {
      auto bw_res = tryCalculateBitWidth(assert_ty);
      return bw_res.transform_error(
          [&__inst](auto &&err) { return IllFormedIntrinsicError(__inst); });
    }
  }
  return RetType::unexpected_type(IllFormedIntrinsicError(__inst));
}

std::string emitFromIntAssertionIntrinsic(llvm::CallInst &__inst) {
  const auto bw =
      unwrapOrThrowWithInst(tryCalculateIntAssertionBitWidth(__inst), __inst);

  const auto arg1_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(0)), __inst);
  auto arg1 = unwrapOrThrowWithInst(tryParseScalarRegister(arg1_str), __inst);

  const auto arg2_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(1)), __inst);
  auto arg2 = unwrapOrThrowWithInst(tryParseScalarValue(arg2_str), __inst);

  const auto inst = sc::backend::assembly::AssertEqInst::create(arg1, arg2);
  return inst.getAssembly();
}

bool isIntConstantIntrinsic(llvm::CallInst &__inst) noexcept {
  const auto fn_name = __inst.getCalledFunction()->getName();
  if (fn_name == "const_i64") {
    return true;
  } else {
    return false;
  }
}

std::string emitFromIntConstantIntrinsic(llvm::CallInst &__inst) {
  const auto target_str = unwrapOrThrowWithInst(tryGetName(&__inst), __inst);
  const auto target =
      unwrapOrThrowWithInst(tryParseGeneralRegister(target_str), __inst);

  const auto arg_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(0)), __inst);
  auto arg = unwrapOrThrowWithInst(tryParseInt(arg_str), __inst);

  const auto inst = sc::backend::assembly::ConstantInst::create(target, arg);
  return inst.getAssembly();
}

// Shared with all Vector Parallel Instructions with 2 args
std::expected<VectorWidth, IllFormedIntrinsicError>
tryCalculateVectorWidth(llvm::CallInst &__inst) noexcept {
  using RetType = std::expected<VectorWidth, IllFormedIntrinsicError>;
  const auto ret_ty = __inst.getType();

  if (ret_ty->isVectorTy() && __inst.arg_size() == 2) {
    const auto v1_ty = __inst.getArgOperand(0)->getType();
    const auto v2_ty = __inst.getArgOperand(1)->getType();
    if (ret_ty == v1_ty && ret_ty == v2_ty) {
      return tryCalculateVectorWidth(ret_ty).transform_error(
          [&__inst](auto _) { return IllFormedIntrinsicError(__inst); });
    }
  }
  return RetType::unexpected_type(IllFormedIntrinsicError(__inst));
}

// Incr, Decr
std::expected<VectorWidth, IllFormedIntrinsicError>
tryCalculateVectorIncrDecrBitWidth(llvm::CallInst &__inst) noexcept {
  using RetType = std::expected<VectorWidth, IllFormedIntrinsicError>;
  const auto ret_ty = __inst.getType();

  if (ret_ty->isVectorTy() && __inst.arg_size() == 1) {
    const auto v_ty = __inst.getArgOperand(0)->getType();
    if (ret_ty == v_ty) {
      return tryCalculateVectorWidth(ret_ty).transform_error(
          [&__inst](auto _) { return IllFormedIntrinsicError(__inst); });
    }
  }
  return RetType::unexpected_type(IllFormedIntrinsicError(__inst));
}

// Comp, Select
std::expected<VectorWidth, IllFormedIntrinsicError>
tryCalculateVectorSelectBitWidth(llvm::CallInst &__inst) noexcept {
  using RetType = std::expected<VectorWidth, IllFormedIntrinsicError>;
  const auto ret_ty = __inst.getType();

  if (ret_ty->isVectorTy() && __inst.arg_size() == 3) {
    const auto v1_ty = __inst.getArgOperand(0)->getType();
    const auto v2_ty = __inst.getArgOperand(1)->getType();
    const auto v3_ty = __inst.getArgOperand(2)->getType();
    if (ret_ty == v1_ty && ret_ty == v2_ty && ret_ty == v3_ty) {
      return tryCalculateVectorWidth(ret_ty).transform_error(
          [&__inst](auto _) { return IllFormedIntrinsicError(__inst); });
    }
  }
  return RetType::unexpected_type(IllFormedIntrinsicError(__inst));
}

// Elementwise Vector Arithmetics

bool isVectorIncrIntrinsic(llvm::CallInst &__inst) {
  const auto fn_name = __inst.getCalledFunction()->getName();
  return (fn_name == "vincr_i32x8" || fn_name == "vincr_i64x4");
}

std::string emitFromVectorIncrIntrinsic(llvm::CallInst &__inst) {
  const auto bw =
      unwrapOrThrowWithInst(tryCalculateVectorIncrDecrBitWidth(__inst), __inst);

  const auto target_str = unwrapOrThrowWithInst(tryGetName(&__inst), __inst);
  const auto target =
      unwrapOrThrowWithInst(tryParseVectorRegister(target_str), __inst);

  const auto vec_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(0)), __inst);
  auto vec = unwrapOrThrowWithInst(tryParseVectorRegister(vec_str), __inst);

  const auto inst =
      sc::backend::assembly::VectorIncrInst::create(target, std::move(vec), bw);
  return inst.getAssembly();
}

bool isVectorDecrIntrinsic(llvm::CallInst &__inst) {
  const auto fn_name = __inst.getCalledFunction()->getName();
  return (fn_name == "vdecr_i32x8" || fn_name == "vdecr_i64x4");
}

std::string emitFromVectorDecrIntrinsic(llvm::CallInst &__inst) {
  const auto bw =
      unwrapOrThrowWithInst(tryCalculateVectorIncrDecrBitWidth(__inst), __inst);

  const auto target_str = unwrapOrThrowWithInst(tryGetName(&__inst), __inst);
  const auto target =
      unwrapOrThrowWithInst(tryParseVectorRegister(target_str), __inst);

  const auto vec_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(0)), __inst);
  auto vec = unwrapOrThrowWithInst(tryParseVectorRegister(vec_str), __inst);

  const auto inst =
      sc::backend::assembly::VectorDecrInst::create(target, std::move(vec), bw);
  return inst.getAssembly();
}

bool isVectorCompIntrinsic(llvm::CallInst &__inst) {
  const auto fn_name = __inst.getCalledFunction()->getName();
  return fn_name.starts_with("vicmp_"s) &&
         (fn_name.ends_with("i64x4"s) || fn_name.ends_with("i32x8"s));
}

std::string emitFromVectorCompIntrinsic(llvm::CallInst &__inst) {
  const auto bw = unwrapOrThrowWithInst(
      tryCalculateVectorWidth(__inst), __inst);

  const auto target_str = unwrapOrThrowWithInst(tryGetName(&__inst), __inst);
  const auto target =
      unwrapOrThrowWithInst(tryParseVectorRegister(target_str), __inst);

  const auto vcond_str = __inst.getCalledFunction()
                             ->getName()
                             .substr("vicmp_"s.length(), 3)
                             .rtrim("_");

  const auto vcond =
      unwrapOrThrowWithInst(tryParseIcmpCondition(vcond_str), __inst);

  const auto v1_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(0)), __inst);
  const auto v1 = unwrapOrThrowWithInst(tryParseVectorRegister(v1_str), __inst);

  const auto v2_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(1)), __inst);
  const auto v2 = unwrapOrThrowWithInst(tryParseVectorRegister(v2_str), __inst);

  const auto inst = sc::backend::assembly::VectorCompInst::create(
      target, std::move(vcond), v1, v2, bw);
  return inst.getAssembly();
}

bool isVectorSelectIntrinsic(llvm::CallInst &__inst) {
  const auto fn_name = __inst.getCalledFunction()->getName();
  return (fn_name == "vselect_i32x8" || fn_name == "vselect_i64x4");
}

std::string emitFromVectorSelectIntrinsic(llvm::CallInst &__inst) {
  const auto bw = unwrapOrThrowWithInst(
      tryCalculateVectorSelectBitWidth(__inst), __inst);

  const auto target_str = unwrapOrThrowWithInst(tryGetName(&__inst), __inst);
  const auto target =
      unwrapOrThrowWithInst(tryParseVectorRegister(target_str), __inst);

  const auto vcond_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(0)), __inst);
  auto vcond = unwrapOrThrowWithInst(tryParseVectorRegister(vcond_str), __inst);

  const auto vtrue_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(1)), __inst);
  auto vtrue = unwrapOrThrowWithInst(tryParseVectorRegister(vtrue_str), __inst);

  const auto vfalse_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(2)), __inst);
  auto vfalse =
      unwrapOrThrowWithInst(tryParseVectorRegister(vfalse_str), __inst);

  const auto inst = sc::backend::assembly::VectorSelectInst::create(
      target, std::move(vcond), std::move(vtrue), std::move(vfalse), bw);
  return inst.getAssembly();
}

// Parallel Vector Arithmetics

bool isVectorParallelAddIntrinsic(llvm::CallInst &__inst) {
  const auto fn_name = __inst.getCalledFunction()->getName();
  if (fn_name == "vpadd_i32x8" || fn_name == "vpadd_i64x4") {
    return true;
  } else {
    return false;
  }
}

std::string emitFromVectorParallelAddIntrinsic(llvm::CallInst &__inst) {
  // Calculate the bit width of the vector elements
  const auto bw =
      unwrapOrThrowWithInst(tryCalculateVectorWidth(__inst), __inst);

  // Get the target vector register
  const auto target_str = unwrapOrThrowWithInst(tryGetName(&__inst), __inst);
  const auto target =
      unwrapOrThrowWithInst(tryParseVectorRegister(target_str), __inst);

  // Get the first argument vector register
  const auto v1_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(0)), __inst);
  auto v1 = unwrapOrThrowWithInst(tryParseVectorRegister(v1_str), __inst);

  // Get the second argument vector register
  const auto v2_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(1)), __inst);
  auto v2 = unwrapOrThrowWithInst(tryParseVectorRegister(v2_str), __inst);

  // Create the vector add instruction
  const auto inst =
      sc::backend::assembly::VectorParallelAddInst::create(target, v1, v2, bw);

  // Return the assembly code
  return inst.getAssembly();
}

bool isVectorParallelSubIntrinsic(llvm::CallInst &__inst) {
  const auto fn_name = __inst.getCalledFunction()->getName();
  if (fn_name == "vpsub_i32x8" || fn_name == "vpsub_i64x4") {
    return true;
  } else {
    return false;
  }
}

std::string emitFromVectorParallelSubIntrinsic(llvm::CallInst &__inst) {
  const auto bw =
      unwrapOrThrowWithInst(tryCalculateVectorWidth(__inst), __inst);

  const auto target_str = unwrapOrThrowWithInst(tryGetName(&__inst), __inst);
  const auto target =
      unwrapOrThrowWithInst(tryParseVectorRegister(target_str), __inst);

  const auto v1_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(0)), __inst);
  auto v1 = unwrapOrThrowWithInst(tryParseVectorRegister(v1_str), __inst);

  const auto v2_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(1)), __inst);
  auto v2 = unwrapOrThrowWithInst(tryParseVectorRegister(v2_str), __inst);

  const auto inst =
      sc::backend::assembly::VectorParallelSubInst::create(target, v1, v2, bw);
  return inst.getAssembly();
}

bool isVectorParallelMulIntrinsic(llvm::CallInst &__inst) {
  const auto fn_name = __inst.getCalledFunction()->getName();
  if (fn_name == "vpmul_i32x8" || fn_name == "vpmul_i64x4") {
    return true;
  } else {
    return false;
  }
}

std::string emitFromVectorParallelMulIntrinsic(llvm::CallInst &__inst) {
  const auto bw =
      unwrapOrThrowWithInst(tryCalculateVectorWidth(__inst), __inst);

  const auto target_str = unwrapOrThrowWithInst(tryGetName(&__inst), __inst);
  const auto target =
      unwrapOrThrowWithInst(tryParseVectorRegister(target_str), __inst);

  const auto v1_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(0)), __inst);
  auto v1 = unwrapOrThrowWithInst(tryParseVectorRegister(v1_str), __inst);

  const auto v2_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(1)), __inst);
  auto v2 = unwrapOrThrowWithInst(tryParseVectorRegister(v2_str), __inst);

  const auto inst =
      sc::backend::assembly::VectorParallelMulInst::create(target, v1, v2, bw);
  return inst.getAssembly();
}

bool isVectorParallelUDivIntrinsic(llvm::CallInst &__inst) {
  const auto fn_name = __inst.getCalledFunction()->getName();
  return (fn_name == "vpudiv_i32x8" || fn_name == "vpudiv_i64x4");
}

std::string emitFromVectorParallelUDivIntrinsic(llvm::CallInst &__inst) {
  const auto bw =
      unwrapOrThrowWithInst(tryCalculateVectorWidth(__inst), __inst);

  const auto target_str = unwrapOrThrowWithInst(tryGetName(&__inst), __inst);
  const auto target =
      unwrapOrThrowWithInst(tryParseVectorRegister(target_str), __inst);

  const auto v1_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(0)), __inst);
  auto v1 = unwrapOrThrowWithInst(tryParseVectorRegister(v1_str), __inst);

  const auto v2_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(1)), __inst);
  auto v2 = unwrapOrThrowWithInst(tryParseVectorRegister(v2_str), __inst);

  const auto inst =
      sc::backend::assembly::VectorParallelUDivInst::create(target, v1, v2, bw);
  return inst.getAssembly();
}

bool isVectorParallelSDivIntrinsic(llvm::CallInst &__inst) {
  const auto fn_name = __inst.getCalledFunction()->getName();
  return (fn_name == "vpsdiv_i32x8" || fn_name == "vpsdiv_i64x4");
}

std::string emitFromVectorParallelSDivIntrinsic(llvm::CallInst &__inst) {
  const auto bw =
      unwrapOrThrowWithInst(tryCalculateVectorWidth(__inst), __inst);

  const auto target_str = unwrapOrThrowWithInst(tryGetName(&__inst), __inst);
  const auto target =
      unwrapOrThrowWithInst(tryParseVectorRegister(target_str), __inst);

  const auto v1_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(0)), __inst);
  auto v1 = unwrapOrThrowWithInst(tryParseVectorRegister(v1_str), __inst);

  const auto v2_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(1)), __inst);
  auto v2 = unwrapOrThrowWithInst(tryParseVectorRegister(v2_str), __inst);

  const auto inst =
      sc::backend::assembly::VectorParallelSDivInst::create(target, v1, v2, bw);
  return inst.getAssembly();
}

bool isVectorParallelURemIntrinsic(llvm::CallInst &__inst) {
  const auto fn_name = __inst.getCalledFunction()->getName();
  return (fn_name == "vpurem_i32x8" || fn_name == "vpurem_i64x4");
}

std::string emitFromVectorParallelURemIntrinsic(llvm::CallInst &__inst) {
  const auto bw =
      unwrapOrThrowWithInst(tryCalculateVectorWidth(__inst), __inst);

  const auto target_str = unwrapOrThrowWithInst(tryGetName(&__inst), __inst);
  const auto target =
      unwrapOrThrowWithInst(tryParseVectorRegister(target_str), __inst);

  const auto v1_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(0)), __inst);
  auto v1 = unwrapOrThrowWithInst(tryParseVectorRegister(v1_str), __inst);

  const auto v2_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(1)), __inst);
  auto v2 = unwrapOrThrowWithInst(tryParseVectorRegister(v2_str), __inst);

  const auto inst =
      sc::backend::assembly::VectorParallelURemInst::create(target, v1, v2, bw);
  return inst.getAssembly();
}

bool isVectorParallelSRemIntrinsic(llvm::CallInst &__inst) {
  const auto fn_name = __inst.getCalledFunction()->getName();
  return (fn_name == "vpsrem_i32x8" || fn_name == "vpsrem_i64x4");
}

std::string emitFromVectorParallelSRemIntrinsic(llvm::CallInst &__inst) {
  const auto bw =
      unwrapOrThrowWithInst(tryCalculateVectorWidth(__inst), __inst);

  const auto target_str = unwrapOrThrowWithInst(tryGetName(&__inst), __inst);
  const auto target =
      unwrapOrThrowWithInst(tryParseVectorRegister(target_str), __inst);

  const auto v1_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(0)), __inst);
  auto v1 = unwrapOrThrowWithInst(tryParseVectorRegister(v1_str), __inst);

  const auto v2_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(1)), __inst);
  auto v2 = unwrapOrThrowWithInst(tryParseVectorRegister(v2_str), __inst);

  const auto inst =
      sc::backend::assembly::VectorParallelSRemInst::create(target, v1, v2, bw);
  return inst.getAssembly();
}

bool isVectorParallelAndIntrinsic(llvm::CallInst &__inst) {
  const auto fn_name = __inst.getCalledFunction()->getName();
  return (fn_name == "vpand_i32x8" || fn_name == "vpand_i64x4");
}

std::string emitFromVectorParallelAndIntrinsic(llvm::CallInst &__inst) {
  const auto bw =
      unwrapOrThrowWithInst(tryCalculateVectorWidth(__inst), __inst);

  const auto target_str = unwrapOrThrowWithInst(tryGetName(&__inst), __inst);
  const auto target =
      unwrapOrThrowWithInst(tryParseVectorRegister(target_str), __inst);

  const auto v1_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(0)), __inst);
  auto v1 = unwrapOrThrowWithInst(tryParseVectorRegister(v1_str), __inst);

  const auto v2_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(1)), __inst);
  auto v2 = unwrapOrThrowWithInst(tryParseVectorRegister(v2_str), __inst);

  const auto inst =
      sc::backend::assembly::VectorParallelAndInst::create(target, v1, v2, bw);
  return inst.getAssembly();
}

bool isVectorParallelOrIntrinsic(llvm::CallInst &__inst) {
  const auto fn_name = __inst.getCalledFunction()->getName();
  return (fn_name == "vpor_i32x8" || fn_name == "vpor_i64x4");
}

std::string emitFromVectorParallelOrIntrinsic(llvm::CallInst &__inst) {
  const auto bw =
      unwrapOrThrowWithInst(tryCalculateVectorWidth(__inst), __inst);

  const auto target_str = unwrapOrThrowWithInst(tryGetName(&__inst), __inst);
  const auto target =
      unwrapOrThrowWithInst(tryParseVectorRegister(target_str), __inst);

  const auto v1_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(0)), __inst);
  auto v1 = unwrapOrThrowWithInst(tryParseVectorRegister(v1_str), __inst);

  const auto v2_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(1)), __inst);
  auto v2 = unwrapOrThrowWithInst(tryParseVectorRegister(v2_str), __inst);

  const auto inst =
      sc::backend::assembly::VectorParallelOrInst::create(target, v1, v2, bw);
  return inst.getAssembly();
}

bool isVectorParallelXorIntrinsic(llvm::CallInst &__inst) {
  const auto fn_name = __inst.getCalledFunction()->getName();
  return (fn_name == "vpxor_i32x8" || fn_name == "vpxor_i64x4");
}

std::string emitFromVectorParallelXorIntrinsic(llvm::CallInst &__inst) {
  const auto bw =
      unwrapOrThrowWithInst(tryCalculateVectorWidth(__inst), __inst);

  const auto target_str = unwrapOrThrowWithInst(tryGetName(&__inst), __inst);
  const auto target =
      unwrapOrThrowWithInst(tryParseVectorRegister(target_str), __inst);

  const auto v1_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(0)), __inst);
  auto v1 = unwrapOrThrowWithInst(tryParseVectorRegister(v1_str), __inst);

  const auto v2_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(1)), __inst);
  auto v2 = unwrapOrThrowWithInst(tryParseVectorRegister(v2_str), __inst);

  const auto inst =
      sc::backend::assembly::VectorParallelXorInst::create(target, v1, v2, bw);
  return inst.getAssembly();
}

bool isVectorParallelShlIntrinsic(llvm::CallInst &__inst) {
  const auto fn_name = __inst.getCalledFunction()->getName();
  return (fn_name == "vpshl_i32x8" || fn_name == "vpshl_i64x4");
}

std::string emitFromVectorParallelShlIntrinsic(llvm::CallInst &__inst) {
  const auto bw =
      unwrapOrThrowWithInst(tryCalculateVectorWidth(__inst), __inst);

  const auto target_str = unwrapOrThrowWithInst(tryGetName(&__inst), __inst);
  const auto target =
      unwrapOrThrowWithInst(tryParseVectorRegister(target_str), __inst);

  const auto v1_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(0)), __inst);
  auto v1 = unwrapOrThrowWithInst(tryParseVectorRegister(v1_str), __inst);

  const auto v2_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(1)), __inst);
  auto v2 = unwrapOrThrowWithInst(tryParseVectorRegister(v2_str), __inst);

  const auto inst =
      sc::backend::assembly::VectorParallelShlInst::create(target, v1, v2, bw);
  return inst.getAssembly();
}

bool isVectorParallelLShrIntrinsic(llvm::CallInst &__inst) {
  const auto fn_name = __inst.getCalledFunction()->getName();
  return (fn_name == "vplshr_i32x8" || fn_name == "vplshr_i64x4");
}

std::string emitFromVectorParallelLShrIntrinsic(llvm::CallInst &__inst) {
  const auto bw =
      unwrapOrThrowWithInst(tryCalculateVectorWidth(__inst), __inst);

  const auto target_str = unwrapOrThrowWithInst(tryGetName(&__inst), __inst);
  const auto target =
      unwrapOrThrowWithInst(tryParseVectorRegister(target_str), __inst);

  const auto v1_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(0)), __inst);
  auto v1 = unwrapOrThrowWithInst(tryParseVectorRegister(v1_str), __inst);

  const auto v2_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(1)), __inst);
  auto v2 = unwrapOrThrowWithInst(tryParseVectorRegister(v2_str), __inst);

  const auto inst =
      sc::backend::assembly::VectorParallelLShrInst::create(target, v1, v2, bw);
  return inst.getAssembly();
}

bool isVectorParallelAShrIntrinsic(llvm::CallInst &__inst) {
  const auto fn_name = __inst.getCalledFunction()->getName();
  return (fn_name == "vpashr_i32x8" || fn_name == "vpashr_i64x4");
}

std::string emitFromVectorParallelAShrIntrinsic(llvm::CallInst &__inst) {
  const auto bw =
      unwrapOrThrowWithInst(tryCalculateVectorWidth(__inst), __inst);

  const auto target_str = unwrapOrThrowWithInst(tryGetName(&__inst), __inst);
  const auto target =
      unwrapOrThrowWithInst(tryParseVectorRegister(target_str), __inst);

  const auto v1_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(0)), __inst);
  const auto v1 = unwrapOrThrowWithInst(tryParseVectorRegister(v1_str), __inst);

  const auto v2_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(1)), __inst);
  const auto v2 = unwrapOrThrowWithInst(tryParseVectorRegister(v2_str), __inst);

  const auto inst =
      sc::backend::assembly::VectorParallelAShrInst::create(target, v1, v2, bw);
  return inst.getAssembly();
}

bool isVectorParallelCompIntrinsic(llvm::CallInst &__inst) {
  const auto fn_name = __inst.getCalledFunction()->getName();
  return fn_name.starts_with("vpicmp_"s) &&
         (fn_name.ends_with("i64x4"s) || fn_name.ends_with("i32x8"s));
}

std::string emitFromVectorParallelCompIntrinsic(llvm::CallInst &__inst) {
  const auto bw = unwrapOrThrowWithInst(
      tryCalculateVectorWidth(__inst), __inst);

  const auto target_str = unwrapOrThrowWithInst(tryGetName(&__inst), __inst);
  const auto target =
      unwrapOrThrowWithInst(tryParseVectorRegister(target_str), __inst);

  const auto vcond_str = __inst.getCalledFunction()
                             ->getName()
                             .substr("vpicmp_"s.length(), 3)
                             .rtrim("_");

  const auto vcond =
      unwrapOrThrowWithInst(tryParseIcmpCondition(vcond_str), __inst);

  const auto v1_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(0)), __inst);
  const auto v1 = unwrapOrThrowWithInst(tryParseVectorRegister(v1_str), __inst);

  const auto v2_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(1)), __inst);
  const auto v2 = unwrapOrThrowWithInst(tryParseVectorRegister(v2_str), __inst);

  const auto inst = sc::backend::assembly::VectorParallelCompInst::create(
      target, std::move(vcond), v1, v2, bw);
  return inst.getAssembly();
}

bool isVectorParallelSelectIntrinsic(llvm::CallInst &__inst) {
  const auto fn_name = __inst.getCalledFunction()->getName();
  return (fn_name == "vpselect_i32x8" || fn_name == "vpselect_i64x4");
}

std::string emitFromVectorParallelSelectIntrinsic(llvm::CallInst &__inst) {
  const auto bw = unwrapOrThrowWithInst(
      tryCalculateVectorSelectBitWidth(__inst), __inst);

  const auto target_str = unwrapOrThrowWithInst(tryGetName(&__inst), __inst);
  const auto target =
      unwrapOrThrowWithInst(tryParseVectorRegister(target_str), __inst);

  const auto vcond_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(0)), __inst);
  auto vcond = unwrapOrThrowWithInst(tryParseVectorRegister(vcond_str), __inst);

  const auto vlhs_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(1)), __inst);
  auto vlhs = unwrapOrThrowWithInst(tryParseVectorRegister(vlhs_str), __inst);

  const auto vrhs_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(2)), __inst);
  auto vrhs = unwrapOrThrowWithInst(tryParseVectorRegister(vrhs_str), __inst);

  const auto inst = sc::backend::assembly::VectorParallelSelectInst::create(
      target, std::move(vcond), std::move(vlhs), std::move(vrhs), bw);
  return inst.getAssembly();
}

bool isVectorBroadcastIntrinsic(llvm::CallInst &__inst) {
  const auto fn_name = __inst.getCalledFunction()->getName();
  return (fn_name == "vbcast_i32x8" || fn_name == "vbcast_i64x4");
}

std::string emitFromVectorBroadcastIntrinsic(llvm::CallInst &__inst) {
  auto bw = VectorWidth::DWORD;
  const auto fn_name = __inst.getCalledFunction()->getName();
  if (fn_name.ends_with("i32x8")) {
    bw = VectorWidth::DWORD;
  } else if (fn_name.ends_with("i64x4")) {
    bw = VectorWidth::QWORD;
  }

  const auto target_str = unwrapOrThrowWithInst(tryGetName(&__inst), __inst);
  const auto target =
      unwrapOrThrowWithInst(tryParseVectorRegister(target_str), __inst);

  const auto value_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getArgOperand(0)), __inst);
  const auto value =
      unwrapOrThrowWithInst(tryParseScalarRegister(value_str), __inst);

  const auto inst =
      sc::backend::assembly::VectorBroadcastInst::create(target, value, bw);
  return inst.getAssembly();
}

// Recursive Call
std::string emitFromRecursiveCallIntrinsic(llvm::CallInst &__inst) {
  const auto fn = __inst.getCalledFunction();

  std::vector<ScalarRegisterTy> args;
  args.reserve(__inst.arg_size());
  std::transform(
      __inst.arg_begin(), __inst.arg_end(), std::back_inserter(args),
      [&__inst](auto &arg) {
        const auto arg_str = unwrapOrThrowWithInst(tryGetName(arg), __inst);
        return unwrapOrThrowWithInst(tryParseScalarRegister(arg_str), __inst);
      });

  if (fn->getReturnType()->isVoidTy()) {
    const auto inst = unwrapOrThrowWithInst(
        sc::backend::assembly::RecursiveCallInst::tryCreateDiscarding(
            std::move(args)),
        __inst);
    return inst.getAssembly();

  } else {
    const auto target_str = unwrapOrThrowWithInst(tryGetName(&__inst), __inst);
    const auto target =
        unwrapOrThrowWithInst(tryParseGeneralRegister(target_str), __inst);

    const auto inst = unwrapOrThrowWithInst(
        sc::backend::assembly::RecursiveCallInst::tryCreateReturning(
            target, std::move(args)),
        __inst);
    return inst.getAssembly();
  }
}

std::optional<sc::backend::assembly::FunctionEndInst> function_to_close;

constexpr unsigned VECTOR_REGISTER_WIDTH = 256;

bool isSupportedVectorType(llvm::VectorType *__vtype) {
  const auto elem_ty = __vtype->getElementType();
  if (elem_ty->isIntegerTy()) {
    const auto elem_bw = elem_ty->getIntegerBitWidth();
    switch (elem_bw) {
    case 32:
    case 64: {
      const auto num_elems = __vtype->getElementCount().getFixedValue();
      if (elem_bw * num_elems == VECTOR_REGISTER_WIDTH) {
        return true;
      }
    }
    }
  }
  return false;
}

std::string visitScalarBinaryOperator(llvm::BinaryOperator &__op) {
  const auto target_str = unwrapOrThrowWithInst(tryGetName(&__op), __op);
  const auto target =
      unwrapOrThrowWithInst(tryParseGeneralRegister(target_str), __op);

  auto bw = unwrapOrThrowWithInst(tryCalculateBitWidth(__op.getType()), __op);

  const auto arg1_str =
      unwrapOrThrowWithInst(tryGetName(__op.getOperand(0)), __op);
  const auto arg1 =
      unwrapOrThrowWithInst(tryParseScalarRegister(arg1_str), __op);
  const auto arg2_str =
      unwrapOrThrowWithInst(tryGetName(__op.getOperand(1)), __op);
  const auto arg2 =
      unwrapOrThrowWithInst(tryParseScalarRegister(arg2_str), __op);

  switch (__op.getOpcode()) {
  case llvm::Instruction::UDiv:
    return IntUDivInst::create(target, bw, arg1, arg2).getAssembly();

  case llvm::Instruction::SDiv:
    return IntSDivInst::create(target, bw, arg1, arg2).getAssembly();

  case llvm::Instruction::URem:
    return IntURemInst::create(target, bw, arg1, arg2).getAssembly();

  case llvm::Instruction::SRem:
    return IntSRemInst::create(target, bw, arg1, arg2).getAssembly();

  case llvm::Instruction::Mul:
    return IntMulInst::create(target, bw, arg1, arg2).getAssembly();

  case llvm::Instruction::Shl:
    return IntShlInst::create(target, bw, arg1, arg2).getAssembly();

  case llvm::Instruction::AShr:
    return IntAShrInst::create(target, bw, arg1, arg2).getAssembly();

  case llvm::Instruction::LShr:
    return IntLShrInst::create(target, bw, arg1, arg2).getAssembly();

  case llvm::Instruction::And:
    return IntAndInst::create(target, bw, arg1, arg2).getAssembly();

  case llvm::Instruction::Or:
    return IntOrInst::create(target, bw, arg1, arg2).getAssembly();

  case llvm::Instruction::Xor:
    return IntXorInst::create(target, bw, arg1, arg2).getAssembly();

  case llvm::Instruction::Add:
    return IntAddInst::create(target, bw, arg1, arg2).getAssembly();

  case llvm::Instruction::Sub:
    return IntSubInst::create(target, bw, arg1, arg2).getAssembly();

  default:
    throw ErrorWithInstruction(IllFormedInstError("invalid binary operation"s),
                               __op);
  }
}

std::string visitVectorBinaryOperator(llvm::BinaryOperator &__op) {
  const auto target_str = unwrapOrThrowWithInst(tryGetName(&__op), __op);
  const auto target =
      unwrapOrThrowWithInst(tryParseVectorRegister(target_str), __op);

  auto bw =
      unwrapOrThrowWithInst(tryCalculateVectorWidth(__op.getType()), __op);

  const auto arg1_str =
      unwrapOrThrowWithInst(tryGetName(__op.getOperand(0)), __op);
  const auto arg1 =
      unwrapOrThrowWithInst(tryParseVectorRegister(arg1_str), __op);
  const auto arg2_str =
      unwrapOrThrowWithInst(tryGetName(__op.getOperand(1)), __op);
  const auto arg2 =
      unwrapOrThrowWithInst(tryParseVectorRegister(arg2_str), __op);

  switch (__op.getOpcode()) {
  case llvm::Instruction::UDiv:
    return VectorUDivInst::create(target, arg1, arg2, bw).getAssembly();

  case llvm::Instruction::SDiv:
    return VectorSDivInst::create(target, arg1, arg2, bw).getAssembly();

  case llvm::Instruction::URem:
    return VectorURemInst::create(target, arg1, arg2, bw).getAssembly();

  case llvm::Instruction::SRem:
    return VectorSRemInst::create(target, arg1, arg2, bw).getAssembly();

  case llvm::Instruction::Mul:
    return VectorMulInst::create(target, arg1, arg2, bw).getAssembly();

  case llvm::Instruction::Shl:
    return VectorShlInst::create(target, arg1, arg2, bw).getAssembly();

  case llvm::Instruction::AShr:
    return VectorAShrInst::create(target, arg1, arg2, bw).getAssembly();

  case llvm::Instruction::LShr:
    return VectorLShrInst::create(target, arg1, arg2, bw).getAssembly();

  case llvm::Instruction::And:
    return VectorAndInst::create(target, arg1, arg2, bw).getAssembly();

  case llvm::Instruction::Or:
    return VectorOrInst::create(target, arg1, arg2, bw).getAssembly();

  case llvm::Instruction::Xor:
    return VectorXorInst::create(target, arg1, arg2, bw).getAssembly();

  case llvm::Instruction::Add:
    return VectorAddInst::create(target, arg1, arg2, bw).getAssembly();

  case llvm::Instruction::Sub:
    return VectorSubInst::create(target, arg1, arg2, bw).getAssembly();

  default:
    throw ErrorWithInstruction(IllFormedInstError("invalid binary operation"s),
                               __op);
  }
}
} // namespace

namespace sc::backend::emitter {
AssemblyEmitter::AssemblyEmitter(const symbol::SymbolMap &__SM) noexcept {
  ::__SM = &__SM;
}

void AssemblyEmitter::visitFunction(llvm::Function &__function) {
  if (function_to_close.has_value()) {
    const auto end_fn = std::move(*function_to_close);
    assembly_lines.push_back(end_fn.getAssembly());
    function_to_close.reset();

    // insert an empty line between two functions
    assembly_lines.push_back("");
  }

  if (__function.isDeclaration()) {
    // skip if it is function declaration
    return;
  }

  auto fn_name = unwrapOrThrowWithFunc(tryGetName(&__function), __function);
  const auto inst =
      unwrapOrThrowWithFunc(assembly::FunctionStartInst::tryCreate(
                                std::string(fn_name), __function.arg_size()),
                            __function);

  assembly_lines.push_back(inst.getAssembly());
  function_to_close = assembly::FunctionEndInst::create(std::move(fn_name));
}

void AssemblyEmitter::visitBasicBlock(llvm::BasicBlock &__basic_block) {
  auto bb_name = unwrapOrThrowWithBB(tryGetName(&__basic_block), __basic_block);
  const auto inst = assembly::BasicBlockInst::create(std::move(bb_name));
  assembly_lines.push_back(inst.getAssembly());
}

void AssemblyEmitter::visitICmpInst(llvm::ICmpInst &__inst) {
  const auto target_str = unwrapOrThrowWithInst(tryGetName(&__inst), __inst);
  const auto target =
      unwrapOrThrowWithInst(tryParseGeneralRegister(target_str), __inst);

  const auto cond_str = __inst.getPredicateName(__inst.getPredicate()).str();
  const auto cond =
      unwrapOrThrowWithInst(tryParseIcmpCondition(cond_str), __inst);

  const auto lhs_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getOperand(0)), __inst);
  auto lhs = unwrapOrThrowWithInst(tryParseScalarRegister(lhs_str), __inst);

  const auto rhs_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getOperand(1)), __inst);
  auto rhs = unwrapOrThrowWithInst(tryParseScalarRegister(rhs_str), __inst);

  const auto bw = unwrapOrThrowWithInst(
      tryCalculateBitWidth(__inst.getOperand(0)->getType()), __inst);

  const auto inst = assembly::IntCompInst::create(
      target, cond, bw, std::move(lhs), std::move(rhs));
  assembly_lines.push_back(inst.getAssembly());
}

void AssemblyEmitter::visitLoadInst(llvm::LoadInst &__inst) {
  const auto ptr_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getOperand(0)), __inst);
  const auto ptr =
      unwrapOrThrowWithInst(tryParseScalarRegister(ptr_str), __inst);

  const auto aw =
      unwrapOrThrowWithInst(tryCalculateAccessWidth(__inst.getType()), __inst);
  if (aw == AccessWidth::VECTOR) {
    const auto target_str = unwrapOrThrowWithInst(tryGetName(&__inst), __inst);
    const auto target =
        unwrapOrThrowWithInst(tryParseVectorRegister(target_str), __inst);

    const auto inst = assembly::VectorLoadInst::create(target, ptr);
    assembly_lines.push_back(inst.getAssembly());
  } else {
    const auto target_str = unwrapOrThrowWithInst(tryGetName(&__inst), __inst);
    const auto target =
        unwrapOrThrowWithInst(tryParseGeneralRegister(target_str), __inst);

    const auto inst = assembly::LoadInst::create(target, aw, std::move(ptr));
    assembly_lines.push_back(inst.getAssembly());
  }
}

void AssemblyEmitter::visitStoreInst(llvm::StoreInst &__inst) {
  const auto ptr_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getOperand(1)), __inst);
  const auto ptr =
      unwrapOrThrowWithInst(tryParseScalarRegister(ptr_str), __inst);

  const auto aw = unwrapOrThrowWithInst(
      tryCalculateAccessWidth(__inst.getValueOperand()->getType()), __inst);
  if (aw == AccessWidth::VECTOR) {
    const auto value_str =
        unwrapOrThrowWithInst(tryGetName(__inst.getOperand(0)), __inst);
    const auto value =
        unwrapOrThrowWithInst(tryParseVectorRegister(value_str), __inst);

    const auto inst = assembly::VectorStoreInst::create(value, ptr);
    assembly_lines.push_back(inst.getAssembly());
  } else {
    const auto value_str =
        unwrapOrThrowWithInst(tryGetName(__inst.getOperand(0)), __inst);
    const auto value =
        unwrapOrThrowWithInst(tryParseScalarRegister(value_str), __inst);

    const auto inst =
        assembly::StoreInst::create(aw, std::move(value), std::move(ptr));
    assembly_lines.push_back(inst.getAssembly());
  }
}

// void AssemblyEmitter::visitSExtInst(llvm::SExtInst &__inst) {
//   auto beforeBits = __inst.getOperand(0)->getType()->getIntegerBitWidth();
//   auto afterBits = __inst.getType()->getIntegerBitWidth();

//   const auto target_str = unwrapOrThrowWithInst(tryGetName(&__inst), __inst);
//   const auto target =
//       unwrapOrThrowWithInst(tryParseGeneralRegister(target_str), __inst);

//   auto bw =
//       unwrapOrThrowWithInst(tryCalculateBitWidth(__inst.getType()), __inst);

//   const auto arg1_str =
//       unwrapOrThrowWithInst(tryGetName(__inst.getOperand(0)), __inst);
//   auto arg1 = unwrapOrThrowWithInst(tryParseScalarRegister(arg1_str),
//   __inst);

//   llvm::Constant *arg2_raw = llvm::ConstantInt::get(
//       __inst.getType(), 1llu << (afterBits - beforeBits));
//   const auto arg2_str = unwrapOrThrowWithInst(tryGetName(arg2_raw), __inst);
//   auto arg2 = unwrapOrThrowWithInst(tryParseScalarRegister(arg2_str),
//   __inst);

//   const auto inst1 = assembly::IntMulInst::create(target, bw, arg1, arg2);
//   assembly_lines.push_back(inst1.getAssembly());

//   const auto arg3_str = unwrapOrThrowWithInst(tryGetName(&__inst), __inst);
//   auto arg3 = unwrapOrThrowWithInst(tryParseScalarRegister(arg3_str),
//   __inst);

//   const auto inst2 =
//       assembly::IntSDivInst::create(target, bw, target, arg2);
//   assembly_lines.push_back(inst2.getAssembly());
// }

void AssemblyEmitter::visitSelectInst(llvm::SelectInst &__inst) {
  const auto target_str = unwrapOrThrowWithInst(tryGetName(&__inst), __inst);
  const auto target =
      unwrapOrThrowWithInst(tryParseGeneralRegister(target_str), __inst);

  const auto cond_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getCondition()), __inst);
  auto cond = unwrapOrThrowWithInst(tryParseScalarRegister(cond_str), __inst);

  const auto value_true_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getTrueValue()), __inst);
  auto value_true =
      unwrapOrThrowWithInst(tryParseScalarRegister(value_true_str), __inst);

  const auto value_false_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getFalseValue()), __inst);
  auto value_false =
      unwrapOrThrowWithInst(tryParseScalarRegister(value_false_str), __inst);

  const auto inst = assembly::SelectInst::create(
      target, std::move(cond), std::move(value_true), std::move(value_false));
  assembly_lines.push_back(inst.getAssembly());
}

void AssemblyEmitter::visitExtractElementInst(
    llvm::ExtractElementInst &__inst) {
  const auto target_str = unwrapOrThrowWithInst(tryGetName(&__inst), __inst);
  const auto target =
      unwrapOrThrowWithInst(tryParseGeneralRegister(target_str), __inst);

  if (!isSupportedVectorType(__inst.getVectorOperandType())) {
    throw ErrorWithInstruction(IllFormedInstError("unsupported vector type"s),
                               __inst);
  }
  const auto vec_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getVectorOperand()), __inst);
  auto vec = unwrapOrThrowWithInst(tryParseVectorRegister(vec_str), __inst);

  const auto idx_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getIndexOperand()), __inst);
  auto idx = unwrapOrThrowWithInst(tryParseScalarRegister(idx_str), __inst);

  const auto bw_int =
      __inst.getVectorOperandType()->getElementType()->getIntegerBitWidth();
  auto bw = VectorWidth::DWORD;
  if (bw_int == 64) {
    bw = VectorWidth::QWORD;
  }

  const auto inst = assembly::VectorExtractInst::create(target, vec, idx, bw);
  assembly_lines.push_back(inst.getAssembly());
}

void AssemblyEmitter::visitInsertElementInst(llvm::InsertElementInst &__inst) {
  const auto target_str = unwrapOrThrowWithInst(tryGetName(&__inst), __inst);
  const auto target =
      unwrapOrThrowWithInst(tryParseVectorRegister(target_str), __inst);

  if (!isSupportedVectorType(
          llvm::dyn_cast<llvm::VectorType>(__inst.getType()))) {
    throw ErrorWithInstruction(IllFormedInstError("unsupported vector type"s),
                               __inst);
  }
  const auto vec_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getOperand(0)), __inst);
  auto vec = unwrapOrThrowWithInst(tryParseVectorRegister(vec_str), __inst);

  const auto val_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getOperand(1)), __inst);
  auto val = unwrapOrThrowWithInst(tryParseScalarRegister(val_str), __inst);

  const auto idx_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getOperand(2)), __inst);
  auto idx = unwrapOrThrowWithInst(tryParseScalarRegister(idx_str), __inst);

  const auto bw_int = __inst.getOperand(1)->getType()->getIntegerBitWidth();
  auto bw = VectorWidth::DWORD;
  if (bw_int == 64) {
    bw = VectorWidth::QWORD;
  }

  const auto inst =
      assembly::VectorUpdateInst::create(target, vec, val, idx, bw);
  assembly_lines.push_back(inst.getAssembly());
}

// TODO
void AssemblyEmitter::visitCallInst(llvm::CallInst &__inst) {
  // handle swpp intrinsics firsthand
  if (isMallocIntrinsic(__inst)) {
    assembly_lines.push_back(emitFromMallocIntrinsic(__inst));
    return;
  } else if (isFreeIntrinsic(__inst)) {
    assembly_lines.push_back(emitFromFreeIntrinsic(__inst));
    return;
  } else if (isDecrSPIntrinsic(__inst)) {
    assembly_lines.push_back(emitFromDecrSPIntrinsic(__inst));
    return;
  } else if (isAsyncLoadIntrinsic(__inst)) {
    assembly_lines.push_back(emitFromAsyncLoadIntrinsic(__inst));
    return;
  } else if (isIntSumIntrinsic(__inst)) {
    assembly_lines.push_back(emitFromIntSumIntrinsic(__inst));
    return;
  } else if (isIntIncrIntrinsic(__inst)) {
    assembly_lines.push_back(emitFromIntIncrIntrinsic(__inst));
    return;
  } else if (isIntDecrIntrinsic(__inst)) {
    assembly_lines.push_back(emitFromIntDecrIntrinsic(__inst));
    return;
  } else if (isIntConstantIntrinsic(__inst)) {
    assembly_lines.push_back(emitFromIntConstantIntrinsic(__inst));
    return;
  } else if (isIntAssertionIntrinsic(__inst)) {
    assembly_lines.push_back(emitFromIntAssertionIntrinsic(__inst));
    return;
  } else if (isVectorIncrIntrinsic(__inst)) {
    assembly_lines.push_back(emitFromVectorIncrIntrinsic(__inst));
    return;
  } else if (isVectorDecrIntrinsic(__inst)) {
    assembly_lines.push_back(emitFromVectorDecrIntrinsic(__inst));
    return;
  } else if (isVectorCompIntrinsic(__inst)) {
    assembly_lines.push_back(emitFromVectorCompIntrinsic(__inst));
    return;
  } else if (isVectorSelectIntrinsic(__inst)) {
    assembly_lines.push_back(emitFromVectorSelectIntrinsic(__inst));
    return;
  } else if (isVectorParallelAddIntrinsic(__inst)) {
    assembly_lines.push_back(emitFromVectorParallelAddIntrinsic(__inst));
    return;
  } else if (isVectorParallelSubIntrinsic(__inst)) {
    assembly_lines.push_back(emitFromVectorParallelSubIntrinsic(__inst));
    return;
  } else if (isVectorParallelMulIntrinsic(__inst)) {
    assembly_lines.push_back(emitFromVectorParallelMulIntrinsic(__inst));
    return;
  } else if (isVectorParallelUDivIntrinsic(__inst)) {
    assembly_lines.push_back(emitFromVectorParallelUDivIntrinsic(__inst));
    return;
  } else if (isVectorParallelSDivIntrinsic(__inst)) {
    assembly_lines.push_back(emitFromVectorParallelSDivIntrinsic(__inst));
    return;
  } else if (isVectorParallelURemIntrinsic(__inst)) {
    assembly_lines.push_back(emitFromVectorParallelURemIntrinsic(__inst));
    return;
  } else if (isVectorParallelSRemIntrinsic(__inst)) {
    assembly_lines.push_back(emitFromVectorParallelSRemIntrinsic(__inst));
    return;
  } else if (isVectorParallelAndIntrinsic(__inst)) {
    assembly_lines.push_back(emitFromVectorParallelAndIntrinsic(__inst));
    return;
  } else if (isVectorParallelOrIntrinsic(__inst)) {
    assembly_lines.push_back(emitFromVectorParallelOrIntrinsic(__inst));
    return;
  } else if (isVectorParallelXorIntrinsic(__inst)) {
    assembly_lines.push_back(emitFromVectorParallelXorIntrinsic(__inst));
    return;
  } else if (isVectorParallelShlIntrinsic(__inst)) {
    assembly_lines.push_back(emitFromVectorParallelShlIntrinsic(__inst));
    return;
  } else if (isVectorParallelLShrIntrinsic(__inst)) {
    assembly_lines.push_back(emitFromVectorParallelLShrIntrinsic(__inst));
    return;
  } else if (isVectorParallelAShrIntrinsic(__inst)) {
    assembly_lines.push_back(emitFromVectorParallelAShrIntrinsic(__inst));
    return;
  } else if (isVectorParallelCompIntrinsic(__inst)) {
    assembly_lines.push_back(emitFromVectorParallelCompIntrinsic(__inst));
    return;
  } else if (isVectorParallelSelectIntrinsic(__inst)) {
    assembly_lines.push_back(emitFromVectorParallelSelectIntrinsic(__inst));
    return;
  } else if (isVectorBroadcastIntrinsic(__inst)) {
    assembly_lines.push_back(emitFromVectorBroadcastIntrinsic(__inst));
    return;
  }

  const auto fn = __inst.getCalledFunction();
  const auto fn_parent = __inst.getFunction();
  auto fn_name = fn->getName().str();
  auto fn_parent_name = fn_parent->getName().str();

  if (__inst.isTailCall() && fn_name == fn_parent_name) {
    assembly_lines.push_back(emitFromRecursiveCallIntrinsic(__inst));
    return;
  }

  std::vector<ScalarRegisterTy> args;
  args.reserve(__inst.arg_size());
  std::transform(
      __inst.arg_begin(), __inst.arg_end(), std::back_inserter(args),
      [&__inst](auto &arg) {
        const auto arg_str = unwrapOrThrowWithInst(tryGetName(arg), __inst);
        return unwrapOrThrowWithInst(tryParseScalarRegister(arg_str), __inst);
      });

  if (fn->getReturnType()->isVoidTy()) {
    const auto inst =
        unwrapOrThrowWithInst(assembly::CallInst::tryCreateDiscarding(
                                  std::move(fn_name), std::move(args)),
                              __inst);
    assembly_lines.push_back(inst.getAssembly());
  } else {
    const auto target_str = unwrapOrThrowWithInst(tryGetName(&__inst), __inst);
    const auto target =
        unwrapOrThrowWithInst(tryParseGeneralRegister(target_str), __inst);

    const auto inst =
        unwrapOrThrowWithInst(assembly::CallInst::tryCreateReturning(
                                  target, std::move(fn_name), std::move(args)),
                              __inst);
    assembly_lines.push_back(inst.getAssembly());
  }
}

void AssemblyEmitter::visitReturnInst(llvm::ReturnInst &__inst) {
  const auto ret_value = __inst.getReturnValue();
  if (!ret_value) {
    const auto inst = assembly::ReturnInst::createVoid();
    assembly_lines.push_back(inst.getAssembly());
  } else {
    const auto value_str = unwrapOrThrowWithInst(tryGetName(ret_value), __inst);
    const auto value =
        unwrapOrThrowWithInst(tryParseScalarRegister(value_str), __inst);

    const auto inst = assembly::ReturnInst::create(std::move(value));
    assembly_lines.push_back(inst.getAssembly());
  }
}

void AssemblyEmitter::visitBranchInst(llvm::BranchInst &__inst) {
  if (__inst.isConditional()) {
    if (__inst.getNumSuccessors() == 2) {
      const auto cond_str =
          unwrapOrThrowWithInst(tryGetName(__inst.getCondition()), __inst);
      const auto cond =
          unwrapOrThrowWithInst(tryParseScalarRegister(cond_str), __inst);

      auto label_true =
          unwrapOrThrowWithInst(tryGetName(__inst.getSuccessor(0)), __inst);
      auto label_false =
          unwrapOrThrowWithInst(tryGetName(__inst.getSuccessor(1)), __inst);
      const auto inst = assembly::BranchInst::create(
          std::move(cond), std::move(label_true), std::move(label_false));
      assembly_lines.push_back(inst.getAssembly());
    } else {
      throw ErrorWithInstruction(
          IllFormedInstError("conditional branch must have two successors"s),
          __inst);
    }
  } else if (__inst.isUnconditional()) {
    if (__inst.getNumSuccessors() == 1) {
      auto label =
          unwrapOrThrowWithInst(tryGetName(__inst.getSuccessor(0)), __inst);
      const auto inst = assembly::JumpInst::create(std::move(label));
      assembly_lines.push_back(inst.getAssembly());
    } else {
      throw ErrorWithInstruction(
          IllFormedInstError(
              "unconditional branch must have only one successor"s),
          __inst);
    }
  } else {
    // Unreachable?
    throw ErrorWithInstruction(
        IllFormedInstError("invalid branch instruction"s), __inst);
  }
}

void AssemblyEmitter::visitSwitchInst(llvm::SwitchInst &__inst) {
  const auto cond_str =
      unwrapOrThrowWithInst(tryGetName(__inst.getCondition()), __inst);
  const auto cond =
      unwrapOrThrowWithInst(tryParseScalarRegister(cond_str), __inst);

  std::vector<IntTy> cases;
  cases.reserve(__inst.getNumCases());
  std::vector<std::string> labels;
  labels.reserve(__inst.getNumCases());
  for (auto &c : __inst.cases()) {
    auto succ_name =
        unwrapOrThrowWithInst(tryGetName(c.getCaseSuccessor()), __inst);
    cases.push_back(c.getCaseValue()->getZExtValue());
    labels.push_back(std::move(succ_name));
  }
  auto label_default = unwrapOrThrowWithInst(
      tryGetName(__inst.case_default()->getCaseSuccessor()), __inst);

  const auto inst =
      unwrapOrThrowWithInst(assembly::SwitchInst::tryCreate(
                                std::move(cond), std::move(cases),
                                std::move(labels), std::move(label_default)),
                            __inst);
  assembly_lines.push_back(inst.getAssembly());
}

void AssemblyEmitter::visitBinaryOperator(llvm::BinaryOperator &__op) {
  const auto target_str = unwrapOrThrowWithInst(tryGetName(&__op), __op);
  if (target_str.starts_with("r"s)) {
    assembly_lines.push_back(visitScalarBinaryOperator(__op));
  } else if (target_str.starts_with("v"s)) {
    assembly_lines.push_back(visitVectorBinaryOperator(__op));
  } else {
    throw ErrorWithInstruction(
        IllFormedInstError("invalid binary op instruction"s), __op);
  }
}

void AssemblyEmitter::visitUnreachableInst(llvm::UnreachableInst &__op) {
  const auto comment = assembly::CommentInst::create("unreachable"s);
  assembly_lines.push_back(comment.getAssembly());
  // UB at this point. Free to emit whichever assembly we'd like...
  const auto cst_inst =
      assembly::ConstantInst::create(GeneralRegister::R1, IntTy(0));
  assembly_lines.push_back(cst_inst.getAssembly());
  const auto inst =
      assembly::AssertEqInst::create(GeneralRegister::R1, IntTy(1));
  assembly_lines.push_back(inst.getAssembly());

  auto bb_label = __op.getParent()->getName().str();
  const auto term_placeholder = assembly::JumpInst::create(std::move(bb_label));
  assembly_lines.push_back(term_placeholder.getAssembly());
}

std::string AssemblyEmitter::getAssembly() noexcept {
  if (function_to_close.has_value()) {
    const auto end_fn = std::move(*function_to_close);
    assembly_lines.push_back(end_fn.getAssembly());
    function_to_close.reset();
  }

  return collectStrings(assembly_lines);
}
} // namespace sc::backend::emitter
