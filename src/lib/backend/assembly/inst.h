#ifndef SC_LIB_BACKEND_ASSEMBLY_INST_H
#define SC_LIB_BACKEND_ASSEMBLY_INST_H

#include <expected>
#include <map>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "../../../static_error.h"
#include "int_t.h"
#include "register_t.h"
#include "width_t.h"

namespace super = sc::backend::assembly;

namespace sc::backend::assembly::inst {
using AccessWidth = super::width_t::AccessWidth;
using BitWidth = super::width_t::BitWidth;
using VectorWidth = super::width_t::VectorWidth;

using IntTy = super::int_t::IntTy;
using GeneralRegister = super::register_t::GeneralRegister;
using ArgumentRegister = super::register_t::ArgumentRegister;
using VectorRegister = super::register_t::VectorRegister;
using ScalarRegisterTy = std::variant<GeneralRegister, ArgumentRegister>;
using ScalarValueTy = std::variant<IntTy, ScalarRegisterTy>;

enum class IcmpCondition { EQ, NE, UGT, UGE, ULT, ULE, SGT, SGE, SLT, SLE };

template <typename T> class AbstractInst {
private:
  AbstractInst() noexcept {}
  friend T;

public:
  AbstractInst(AbstractInst &&) = default;
  AbstractInst &operator=(AbstractInst &&) = default;

  std::string getAssembly() const noexcept {
    return static_cast<T *>(this)->getAssembly();
  }
};

class InvalidFunctionArgcError
    : public static_error::Error<InvalidFunctionArgcError> {
public:
  const char *what() const noexcept {
    return "Number of arguments must not exceed 16";
  }
};

class FunctionStartInst : public AbstractInst<FunctionStartInst> {
private:
  std::string name;
  IntTy argc;
  FunctionStartInst(std::string &&name, const IntTy argc) noexcept;

public:
  static std::expected<FunctionStartInst, InvalidFunctionArgcError>
  tryCreate(std::string &&name, const IntTy argc) noexcept;
  std::string getAssembly() const noexcept;
};

class FunctionEndInst : public AbstractInst<FunctionEndInst> {
private:
  std::string name;
  FunctionEndInst(std::string &&name) noexcept;

public:
  static FunctionEndInst create(std::string &&name) noexcept;
  std::string getAssembly() const noexcept;
};

class BasicBlockInst : public AbstractInst<BasicBlockInst> {
private:
  std::string name;
  BasicBlockInst(std::string &&name) noexcept;

public:
  static BasicBlockInst create(std::string &&name) noexcept;
  std::string getAssembly() const noexcept;
};

class CommentInst : public AbstractInst<CommentInst> {
private:
  std::string message;
  CommentInst(std::string &&message) noexcept;

public:
  static CommentInst create(std::string &&message) noexcept;
  std::string getAssembly() const noexcept;
};

class ReturnInst : public AbstractInst<ReturnInst> {
private:
  std::optional<ScalarValueTy> value;
  ReturnInst() noexcept;
  ReturnInst(const ScalarRegisterTy value) noexcept;

public:
  static ReturnInst createVoid() noexcept;
  static ReturnInst create(const ScalarRegisterTy value) noexcept;
  std::string getAssembly() const noexcept;
};

class JumpInst : public AbstractInst<JumpInst> {
private:
  std::string label;
  JumpInst(std::string &&message) noexcept;

public:
  static JumpInst create(std::string &&message) noexcept;
  std::string getAssembly() const noexcept;
};

class BranchInst : public AbstractInst<BranchInst> {
private:
  ScalarRegisterTy condition;
  std::string label_true;
  std::string label_false;
  BranchInst(const ScalarRegisterTy condition, std::string &&label_true,
             std::string &&label_false) noexcept;

public:
  static BranchInst create(const ScalarRegisterTy condition,
                           std::string &&label_true,
                           std::string &&label_false) noexcept;
  std::string getAssembly() const noexcept;
};

class SwitchInst : public AbstractInst<SwitchInst> {
private:
  ScalarRegisterTy condition;
  std::map<IntTy, std::string> cases;
  std::string label_default;
  SwitchInst(const ScalarRegisterTy condition,
             std::string &&label_default) noexcept;
  void addCase(const IntTy value, std::string &&label) noexcept;

public:
  class MismatchingCaseError
      : public static_error::Error<MismatchingCaseError> {
  public:
    const char *what() const noexcept {
      return "Number of cases and labels does not match";
    }
  };

  static std::expected<SwitchInst, MismatchingCaseError>
  tryCreate(const ScalarRegisterTy cond, std::vector<IntTy> &&cases,
            std::vector<std::string> &&labels,
            std::string &&label_default) noexcept;
  std::string getAssembly() const noexcept;
};

class MallocInst : public AbstractInst<MallocInst> {
private:
  GeneralRegister target;
  ScalarRegisterTy size;
  MallocInst(const GeneralRegister target,
             const ScalarRegisterTy size) noexcept;

public:
  static MallocInst create(const GeneralRegister target,
                           const ScalarRegisterTy size) noexcept;
  std::string getAssembly() const noexcept;
};

class FreeInst : public AbstractInst<FreeInst> {
private:
  ScalarRegisterTy ptr;
  FreeInst(const ScalarRegisterTy ptr) noexcept;

public:
  static FreeInst create(const ScalarRegisterTy ptr) noexcept;
  std::string getAssembly() const noexcept;
};

class LoadInst : public AbstractInst<LoadInst> {
private:
  GeneralRegister target;
  AccessWidth size;
  ScalarRegisterTy ptr;
  LoadInst(const GeneralRegister target, const AccessWidth size,
           const ScalarRegisterTy ptr) noexcept;

public:
  static LoadInst create(const GeneralRegister target, const AccessWidth size,
                         const ScalarRegisterTy ptr) noexcept;
  std::string getAssembly() const noexcept;
};

class StoreInst : public AbstractInst<StoreInst> {
private:
  AccessWidth size;
  ScalarRegisterTy value;
  ScalarRegisterTy ptr;
  StoreInst(const AccessWidth size, const ScalarRegisterTy value,
            const ScalarRegisterTy ptr) noexcept;

public:
  static StoreInst create(const AccessWidth size, const ScalarRegisterTy value,
                          const ScalarRegisterTy ptr) noexcept;
  std::string getAssembly() const noexcept;
};

template <typename T>
class IntBinaryOpInst : public AbstractInst<IntBinaryOpInst<T>> {
private:
  GeneralRegister target;
  BitWidth bw;
  ScalarRegisterTy lhs;
  ScalarRegisterTy rhs;

  IntBinaryOpInst(const GeneralRegister target, const BitWidth bw,
                  const ScalarRegisterTy lhs,
                  const ScalarRegisterTy rhs) noexcept {
    this->target = target;
    this->bw = bw;
    this->lhs = std::move(lhs);
    this->rhs = std::move(rhs);
  }
  friend T;

public:
  std::string getAssembly() const noexcept {
    return static_cast<T *>(this)->getAssembly();
  }
};

class IntAddInst : public IntBinaryOpInst<IntAddInst> {
private:
  IntAddInst(const GeneralRegister target, const BitWidth bw,
             const ScalarRegisterTy lhs, const ScalarRegisterTy rhs) noexcept;

public:
  static IntAddInst create(const GeneralRegister target, const BitWidth bw,
                           const ScalarRegisterTy lhs,
                           const ScalarRegisterTy rhs) noexcept;
  std::string getAssembly() const noexcept;
};

class IntSubInst : public IntBinaryOpInst<IntSubInst> {
private:
  IntSubInst(const GeneralRegister target, const BitWidth bw,
             const ScalarRegisterTy lhs, const ScalarRegisterTy rhs) noexcept;

public:
  static IntSubInst create(const GeneralRegister target, const BitWidth bw,
                           const ScalarRegisterTy lhs,
                           const ScalarRegisterTy rhs) noexcept;
  std::string getAssembly() const noexcept;
};

class IntMulInst : public IntBinaryOpInst<IntMulInst> {
private:
  IntMulInst(const GeneralRegister target, const BitWidth bw,
             const ScalarRegisterTy lhs, const ScalarRegisterTy rhs) noexcept;

public:
  static IntMulInst create(const GeneralRegister target, const BitWidth bw,
                           const ScalarRegisterTy lhs,
                           const ScalarRegisterTy rhs) noexcept;
  std::string getAssembly() const noexcept;
};

class IntUDivInst : public IntBinaryOpInst<IntUDivInst> {
private:
  IntUDivInst(const GeneralRegister target, const BitWidth bw,
              const ScalarRegisterTy lhs, const ScalarRegisterTy rhs) noexcept;

public:
  static IntUDivInst create(const GeneralRegister target, const BitWidth bw,
                            const ScalarRegisterTy lhs,
                            const ScalarRegisterTy rhs) noexcept;
  std::string getAssembly() const noexcept;
};

class IntSDivInst : public IntBinaryOpInst<IntSDivInst> {
private:
  IntSDivInst(const GeneralRegister target, const BitWidth bw,
              const ScalarRegisterTy lhs, const ScalarRegisterTy rhs) noexcept;

public:
  static IntSDivInst create(const GeneralRegister target, const BitWidth bw,
                            const ScalarRegisterTy lhs,
                            const ScalarRegisterTy rhs) noexcept;
  std::string getAssembly() const noexcept;
};

class IntURemInst : public IntBinaryOpInst<IntURemInst> {
private:
  IntURemInst(const GeneralRegister target, const BitWidth bw,
              const ScalarRegisterTy lhs, const ScalarRegisterTy rhs) noexcept;

public:
  static IntURemInst create(const GeneralRegister target, const BitWidth bw,
                            const ScalarRegisterTy lhs,
                            const ScalarRegisterTy rhs) noexcept;
  std::string getAssembly() const noexcept;
};

class IntSRemInst : public IntBinaryOpInst<IntSRemInst> {
private:
  IntSRemInst(const GeneralRegister target, const BitWidth bw,
              const ScalarRegisterTy lhs, const ScalarRegisterTy rhs) noexcept;

public:
  static IntSRemInst create(const GeneralRegister target, const BitWidth bw,
                            const ScalarRegisterTy lhs,
                            const ScalarRegisterTy rhs) noexcept;
  std::string getAssembly() const noexcept;
};

class IntAndInst : public IntBinaryOpInst<IntAndInst> {
private:
  IntAndInst(const GeneralRegister target, const BitWidth bw,
             const ScalarRegisterTy lhs, const ScalarRegisterTy rhs) noexcept;

public:
  static IntAndInst create(const GeneralRegister target, const BitWidth bw,
                           const ScalarRegisterTy lhs,
                           const ScalarRegisterTy rhs) noexcept;
  std::string getAssembly() const noexcept;
};

class IntOrInst : public IntBinaryOpInst<IntOrInst> {
private:
  IntOrInst(const GeneralRegister target, const BitWidth bw,
            const ScalarRegisterTy lhs, const ScalarRegisterTy rhs) noexcept;

public:
  static IntOrInst create(const GeneralRegister target, const BitWidth bw,
                          const ScalarRegisterTy lhs,
                          const ScalarRegisterTy rhs) noexcept;
  std::string getAssembly() const noexcept;
};

class IntXorInst : public IntBinaryOpInst<IntXorInst> {
private:
  IntXorInst(const GeneralRegister target, const BitWidth bw,
             const ScalarRegisterTy lhs, const ScalarRegisterTy rhs) noexcept;

public:
  static IntXorInst create(const GeneralRegister target, const BitWidth bw,
                           const ScalarRegisterTy lhs,
                           const ScalarRegisterTy rhs) noexcept;
  std::string getAssembly() const noexcept;
};

class IntShlInst : public IntBinaryOpInst<IntShlInst> {
private:
  IntShlInst(const GeneralRegister target, const BitWidth bw,
             const ScalarRegisterTy lhs, const ScalarRegisterTy rhs) noexcept;

public:
  static IntShlInst create(const GeneralRegister target, const BitWidth bw,
                           const ScalarRegisterTy lhs,
                           const ScalarRegisterTy rhs) noexcept;
  std::string getAssembly() const noexcept;
};

class IntLShrInst : public IntBinaryOpInst<IntLShrInst> {
private:
  IntLShrInst(const GeneralRegister target, const BitWidth bw,
              const ScalarRegisterTy lhs, const ScalarRegisterTy rhs) noexcept;

public:
  static IntLShrInst create(const GeneralRegister target, const BitWidth bw,
                            const ScalarRegisterTy lhs,
                            const ScalarRegisterTy rhs) noexcept;
  std::string getAssembly() const noexcept;
};

class IntAShrInst : public IntBinaryOpInst<IntAShrInst> {
private:
  IntAShrInst(const GeneralRegister target, const BitWidth bw,
              const ScalarRegisterTy lhs, const ScalarRegisterTy rhs) noexcept;

public:
  static IntAShrInst create(const GeneralRegister target, const BitWidth bw,
                            const ScalarRegisterTy lhs,
                            const ScalarRegisterTy rhs) noexcept;
  std::string getAssembly() const noexcept;
};

class IntCompInst : public IntBinaryOpInst<IntCompInst> {
private:
  IcmpCondition cond;
  IntCompInst(const GeneralRegister target, const IcmpCondition cond,
              const BitWidth bw, const ScalarRegisterTy lhs,
              const ScalarRegisterTy rhs) noexcept;

public:
  static IntCompInst create(const GeneralRegister target,
                            const IcmpCondition cond, const BitWidth bw,
                            const ScalarRegisterTy lhs,
                            const ScalarRegisterTy rhs) noexcept;
  std::string getAssembly() const noexcept;
};

class SelectInst : public AbstractInst<SelectInst> {
private:
  GeneralRegister target;
  ScalarRegisterTy cond;
  ScalarRegisterTy lhs;
  ScalarRegisterTy rhs;
  SelectInst(const GeneralRegister target, const ScalarRegisterTy cond,
             const ScalarRegisterTy lhs, const ScalarRegisterTy rhs) noexcept;

public:
  static SelectInst create(const GeneralRegister target,
                           const ScalarRegisterTy cond,
                           const ScalarRegisterTy lhs,
                           const ScalarRegisterTy rhs) noexcept;
  std::string getAssembly() const noexcept;
};

class CallInst : public AbstractInst<CallInst> {
private:
  std::optional<GeneralRegister> target;
  std::string name;
  std::vector<ScalarRegisterTy> args;
  CallInst(std::string &&name) noexcept;
  CallInst(const GeneralRegister target, std::string &&name) noexcept;
  void addArg(const ScalarRegisterTy arg) noexcept;

public:
  static std::expected<CallInst, InvalidFunctionArgcError>
  tryCreateDiscarding(std::string &&name,
                      std::vector<ScalarRegisterTy> &&args) noexcept;
  static std::expected<CallInst, InvalidFunctionArgcError>
  tryCreateReturning(const GeneralRegister target, std::string &&name,
                     std::vector<ScalarRegisterTy> &&args) noexcept;
  std::string getAssembly() const noexcept;
};

class RecursiveCallInst : public AbstractInst<RecursiveCallInst> {
private:
  std::optional<GeneralRegister> target;
  std::vector<ScalarRegisterTy> args;
  RecursiveCallInst() noexcept;
  RecursiveCallInst(const GeneralRegister target) noexcept;
  void addArg(const ScalarRegisterTy arg) noexcept;

public:
  static std::expected<RecursiveCallInst, InvalidFunctionArgcError>
  tryCreateDiscarding(std::vector<ScalarRegisterTy> &&args) noexcept;
  static std::expected<RecursiveCallInst, InvalidFunctionArgcError>
  tryCreateReturning(const GeneralRegister target,
                     std::vector<ScalarRegisterTy> &&args) noexcept;
  std::string getAssembly() const noexcept;
};

class AssertEqInst : public AbstractInst<AssertEqInst> {
private:
  ScalarRegisterTy lhs;
  ScalarValueTy rhs;
  AssertEqInst(const ScalarRegisterTy lhs, const ScalarValueTy rhs) noexcept;

public:
  static AssertEqInst create(const ScalarRegisterTy lhs,
                             const ScalarValueTy rhs) noexcept;
  std::string getAssembly() const noexcept;
};

class AsyncLoadInst : public AbstractInst<AsyncLoadInst> {
private:
  GeneralRegister target;
  AccessWidth size;
  ScalarRegisterTy ptr;
  AsyncLoadInst(const GeneralRegister target, const AccessWidth size,
                const ScalarRegisterTy ptr) noexcept;

public:
  static AsyncLoadInst create(const GeneralRegister target,
                              const AccessWidth size,
                              const ScalarRegisterTy ptr) noexcept;
  std::string getAssembly() const noexcept;
};

class InvalidNumOperandsError
    : public static_error::Error<InvalidNumOperandsError> {
private:
  std::string message;

public:
  InvalidNumOperandsError(const size_t expected, const size_t actual) noexcept;
  const char *what() const noexcept { return message.c_str(); }
};

class IntSumInst : public AbstractInst<IntSumInst> {
private:
  GeneralRegister target;
  ScalarRegisterTy v1;
  ScalarRegisterTy v2;
  ScalarRegisterTy v3;
  ScalarRegisterTy v4;
  ScalarRegisterTy v5;
  ScalarRegisterTy v6;
  ScalarRegisterTy v7;
  ScalarRegisterTy v8;
  BitWidth bw;
  IntSumInst(const GeneralRegister target, const ScalarRegisterTy v1,
             const ScalarRegisterTy v2, const ScalarRegisterTy v3,
             const ScalarRegisterTy v4, const ScalarRegisterTy v5,
             const ScalarRegisterTy v6, const ScalarRegisterTy v7,
             const ScalarRegisterTy v8, const BitWidth bw) noexcept;

public:
  static std::expected<IntSumInst, InvalidNumOperandsError>
  tryCreate(const GeneralRegister target,
            std::vector<ScalarRegisterTy> &&operands,
            const BitWidth bw) noexcept;
  std::string getAssembly() const noexcept;
};

class IntIncrInst : public AbstractInst<IntIncrInst> {
private:
  GeneralRegister target;
  ScalarRegisterTy arg;
  BitWidth bw;
  IntIncrInst(const GeneralRegister target, const ScalarRegisterTy arg,
              const BitWidth bw) noexcept;

public:
  static IntIncrInst create(const GeneralRegister target,
                            const ScalarRegisterTy arg,
                            const BitWidth bw) noexcept;
  std::string getAssembly() const noexcept;
};

class IntDecrInst : public AbstractInst<IntDecrInst> {
private:
  GeneralRegister target;
  ScalarRegisterTy arg;
  BitWidth bw;
  IntDecrInst(const GeneralRegister target, const ScalarRegisterTy arg,
              const BitWidth bw) noexcept;

public:
  static IntDecrInst create(const GeneralRegister target,
                            const ScalarRegisterTy arg,
                            const BitWidth bw) noexcept;
  std::string getAssembly() const noexcept;
};

class ConstantInst : public AbstractInst<ConstantInst> {
private:
  GeneralRegister target;
  IntTy value;
  ConstantInst(const GeneralRegister target, const IntTy value) noexcept;

public:
  static ConstantInst create(const GeneralRegister target,
                             const IntTy value) noexcept;
  std::string getAssembly() const noexcept;
};

class VectorLoadInst : public AbstractInst<VectorLoadInst> {
private:
  VectorRegister target;
  ScalarRegisterTy ptr;
  VectorLoadInst(const VectorRegister target,
                 const ScalarRegisterTy ptr) noexcept;

public:
  static VectorLoadInst create(const VectorRegister target,
                               const ScalarRegisterTy ptr) noexcept;
  std::string getAssembly() const noexcept;
};

class VectorStoreInst : public AbstractInst<VectorStoreInst> {
private:
  VectorRegister value;
  ScalarRegisterTy ptr;
  VectorStoreInst(const VectorRegister value,
                  const ScalarRegisterTy ptr) noexcept;

public:
  static VectorStoreInst create(const VectorRegister value,
                                const ScalarRegisterTy ptr) noexcept;
  std::string getAssembly() const noexcept;
};

// Vector Instruction Classes
template <typename T> class VectorInst : public AbstractInst<VectorInst<T>> {
private:
  VectorRegister target;
  VectorRegister vec1;
  VectorRegister vec2;
  VectorWidth bw;
  VectorInst(const VectorRegister target, const VectorRegister vec1,
             const VectorRegister vec2, const VectorWidth bw) noexcept {
    this->target = target;
    this->vec1 = vec1;
    this->vec2 = vec2;
    this->bw = bw;
  }
  friend T;

public:
  std::string getAssembly() const noexcept {
    return static_cast<T *>(this)->getAssembly();
  }
};

// Elementwise Arithmetics
class VectorAddInst : public VectorInst<VectorAddInst> {
private:
  VectorAddInst(const VectorRegister target, const VectorRegister vec1,
                const VectorRegister vec2, const VectorWidth bw) noexcept;

public:
  static VectorAddInst create(const VectorRegister target,
                              const VectorRegister vec1,
                              const VectorRegister vec2,
                              const VectorWidth bw) noexcept;
  std::string getAssembly() const noexcept;
};

class VectorSubInst : public VectorInst<VectorSubInst> {
private:
  VectorSubInst(const VectorRegister target, const VectorRegister vec1,
                const VectorRegister vec2, const VectorWidth bw) noexcept;

public:
  static VectorSubInst create(const VectorRegister target,
                              const VectorRegister vec1,
                              const VectorRegister vec2,
                              const VectorWidth bw) noexcept;
  std::string getAssembly() const noexcept;
};

class VectorMulInst : public VectorInst<VectorMulInst> {
private:
  VectorMulInst(const VectorRegister target, const VectorRegister vec1,
                const VectorRegister vec2, const VectorWidth bw) noexcept;

public:
  static VectorMulInst create(const VectorRegister target,
                              const VectorRegister vec1,
                              const VectorRegister vec2,
                              const VectorWidth bw) noexcept;
  std::string getAssembly() const noexcept;
};

class VectorUDivInst : public VectorInst<VectorUDivInst> {
private:
  VectorUDivInst(const VectorRegister target, const VectorRegister vec1,
                 const VectorRegister vec2, const VectorWidth bw) noexcept;

public:
  static VectorUDivInst create(const VectorRegister target,
                               const VectorRegister vec1,
                               const VectorRegister vec2,
                               const VectorWidth bw) noexcept;
  std::string getAssembly() const noexcept;
};

class VectorSDivInst : public VectorInst<VectorSDivInst> {
private:
  VectorSDivInst(const VectorRegister target, const VectorRegister vec1,
                 const VectorRegister vec2, const VectorWidth bw) noexcept;

public:
  static VectorSDivInst create(const VectorRegister target,
                               const VectorRegister vec1,
                               const VectorRegister vec2,
                               const VectorWidth bw) noexcept;
  std::string getAssembly() const noexcept;
};

class VectorURemInst : public VectorInst<VectorURemInst> {
private:
  VectorURemInst(const VectorRegister target, const VectorRegister vec1,
                 const VectorRegister vec2, const VectorWidth bw) noexcept;

public:
  static VectorURemInst create(const VectorRegister target,
                               const VectorRegister vec1,
                               const VectorRegister vec2,
                               const VectorWidth bw) noexcept;
  std::string getAssembly() const noexcept;
};

class VectorSRemInst : public VectorInst<VectorSRemInst> {
private:
  VectorSRemInst(const VectorRegister target, const VectorRegister vec1,
                 const VectorRegister vec2, const VectorWidth bw) noexcept;

public:
  static VectorSRemInst create(const VectorRegister target,
                               const VectorRegister vec1,
                               const VectorRegister vec2,
                               const VectorWidth bw) noexcept;
  std::string getAssembly() const noexcept;
};

class VectorAndInst : public VectorInst<VectorAndInst> {
private:
  VectorAndInst(const VectorRegister target, const VectorRegister vec1,
                const VectorRegister vec2, const VectorWidth bw) noexcept;

public:
  static VectorAndInst create(const VectorRegister target,
                              const VectorRegister vec1,
                              const VectorRegister vec2,
                              const VectorWidth bw) noexcept;
  std::string getAssembly() const noexcept;
};

class VectorOrInst : public VectorInst<VectorOrInst> {
private:
  VectorOrInst(const VectorRegister target, const VectorRegister vec1,
               const VectorRegister vec2, const VectorWidth bw) noexcept;

public:
  static VectorOrInst create(const VectorRegister target,
                             const VectorRegister vec1,
                             const VectorRegister vec2,
                             const VectorWidth bw) noexcept;
  std::string getAssembly() const noexcept;
};

class VectorXorInst : public VectorInst<VectorXorInst> {
private:
  VectorXorInst(const VectorRegister target, const VectorRegister vec1,
                const VectorRegister vec2, const VectorWidth bw) noexcept;

public:
  static VectorXorInst create(const VectorRegister target,
                              const VectorRegister vec1,
                              const VectorRegister vec2,
                              const VectorWidth bw) noexcept;
  std::string getAssembly() const noexcept;
};

class VectorShlInst : public VectorInst<VectorShlInst> {
private:
  VectorShlInst(const VectorRegister target, const VectorRegister vec1,
                const VectorRegister vec2, const VectorWidth bw) noexcept;

public:
  static VectorShlInst create(const VectorRegister target,
                              const VectorRegister vec1,
                              const VectorRegister vec2,
                              const VectorWidth bw) noexcept;
  std::string getAssembly() const noexcept;
};

class VectorLShrInst : public VectorInst<VectorLShrInst> {
private:
  VectorLShrInst(const VectorRegister target, const VectorRegister vec1,
                 const VectorRegister vec2, const VectorWidth bw) noexcept;

public:
  static VectorLShrInst create(const VectorRegister target,
                               const VectorRegister vec1,
                               const VectorRegister vec2,
                               const VectorWidth bw) noexcept;
  std::string getAssembly() const noexcept;
};

class VectorAShrInst : public VectorInst<VectorAShrInst> {
private:
  VectorAShrInst(const VectorRegister target, const VectorRegister vec1,
                 const VectorRegister vec2, const VectorWidth bw) noexcept;

public:
  static VectorAShrInst create(const VectorRegister target,
                               const VectorRegister vec1,
                               const VectorRegister vec2,
                               const VectorWidth bw) noexcept;
  std::string getAssembly() const noexcept;
};

class VectorIncrInst : public AbstractInst<VectorIncrInst> {
private:
  VectorRegister target;
  VectorRegister vec;
  VectorWidth bw;
  VectorIncrInst(const VectorRegister target, const VectorRegister vec,
                 const VectorWidth bw) noexcept;

public:
  static VectorIncrInst create(const VectorRegister target,
                               const VectorRegister vec,
                               const VectorWidth bw) noexcept;
  std::string getAssembly() const noexcept;
};

class VectorDecrInst : public AbstractInst<VectorDecrInst> {
private:
  VectorRegister target;
  VectorRegister vec;
  VectorWidth bw;
  VectorDecrInst(const VectorRegister target, const VectorRegister vec,
                 const VectorWidth bw) noexcept;

public:
  static VectorDecrInst create(const VectorRegister target,
                               const VectorRegister vec,
                               const VectorWidth bw) noexcept;
  std::string getAssembly() const noexcept;
};

class VectorCompInst : public VectorInst<VectorCompInst> {
private:
  IcmpCondition cond;
  VectorCompInst(const VectorRegister target, const IcmpCondition cond,
                 const VectorRegister vec1, const VectorRegister vec2,
                 const VectorWidth bw) noexcept;

public:
  static VectorCompInst create(const VectorRegister target,
                               const IcmpCondition cond,
                               const VectorRegister vec1,
                               const VectorRegister vec2,
                               const VectorWidth bw) noexcept;
  std::string getAssembly() const noexcept;
};

class VectorSelectInst : public VectorInst<VectorSelectInst> {
private:
  VectorRegister vec_cond;
  VectorSelectInst(const VectorRegister target, const VectorRegister vec_cond,
                   const VectorRegister vec_true,
                   const VectorRegister vec_false,
                   const VectorWidth bw) noexcept;

public:
  static VectorSelectInst create(const VectorRegister target,
                                 const VectorRegister vec_cond,
                                 const VectorRegister vec_true,
                                 const VectorRegister vec_false,
                                 const VectorWidth bw) noexcept;
  std::string getAssembly() const noexcept;
};

// Parallel Arithmetics
class VectorParallelAddInst : public VectorInst<VectorParallelAddInst> {
private:
  VectorParallelAddInst(const VectorRegister target, const VectorRegister vec1,
                        const VectorRegister vec2,
                        const VectorWidth bw) noexcept;

public:
  static VectorParallelAddInst create(const VectorRegister target,
                                      const VectorRegister vec1,
                                      const VectorRegister vec2,
                                      const VectorWidth bw) noexcept;
  std::string getAssembly() const noexcept;
};

class VectorParallelSubInst : public VectorInst<VectorParallelSubInst> {
private:
  VectorParallelSubInst(const VectorRegister target, const VectorRegister vec1,
                        const VectorRegister vec2,
                        const VectorWidth bw) noexcept;

public:
  static VectorParallelSubInst create(const VectorRegister target,
                                      const VectorRegister vec1,
                                      const VectorRegister vec2,
                                      const VectorWidth bw) noexcept;
  std::string getAssembly() const noexcept;
};

class VectorParallelMulInst : public VectorInst<VectorParallelMulInst> {
private:
  VectorParallelMulInst(const VectorRegister target, const VectorRegister vec1,
                        const VectorRegister vec2,
                        const VectorWidth bw) noexcept;

public:
  static VectorParallelMulInst create(const VectorRegister target,
                                      const VectorRegister vec1,
                                      const VectorRegister vec2,
                                      const VectorWidth bw) noexcept;
  std::string getAssembly() const noexcept;
};

class VectorParallelUDivInst : public VectorInst<VectorParallelUDivInst> {
private:
  VectorParallelUDivInst(const VectorRegister target, const VectorRegister vec1,
                         const VectorRegister vec2,
                         const VectorWidth bw) noexcept;

public:
  static VectorParallelUDivInst create(const VectorRegister target,
                                       const VectorRegister vec1,
                                       const VectorRegister vec2,
                                       const VectorWidth bw) noexcept;
  std::string getAssembly() const noexcept;
};

class VectorParallelSDivInst : public VectorInst<VectorParallelSDivInst> {
private:
  VectorParallelSDivInst(const VectorRegister target, const VectorRegister vec1,
                         const VectorRegister vec2,
                         const VectorWidth bw) noexcept;

public:
  static VectorParallelSDivInst create(const VectorRegister target,
                                       const VectorRegister vec1,
                                       const VectorRegister vec2,
                                       const VectorWidth bw) noexcept;
  std::string getAssembly() const noexcept;
};

class VectorParallelURemInst : public VectorInst<VectorParallelURemInst> {
private:
  VectorParallelURemInst(const VectorRegister target, const VectorRegister vec1,
                         const VectorRegister vec2,
                         const VectorWidth bw) noexcept;

public:
  static VectorParallelURemInst create(const VectorRegister target,
                                       const VectorRegister vec1,
                                       const VectorRegister vec2,
                                       const VectorWidth bw) noexcept;
  std::string getAssembly() const noexcept;
};

class VectorParallelSRemInst : public VectorInst<VectorParallelSRemInst> {
private:
  VectorParallelSRemInst(const VectorRegister target, const VectorRegister vec1,
                         const VectorRegister vec2,
                         const VectorWidth bw) noexcept;

public:
  static VectorParallelSRemInst create(const VectorRegister target,
                                       const VectorRegister vec1,
                                       const VectorRegister vec2,
                                       const VectorWidth bw) noexcept;
  std::string getAssembly() const noexcept;
};

class VectorParallelAndInst : public VectorInst<VectorParallelAndInst> {
private:
  VectorParallelAndInst(const VectorRegister target, const VectorRegister vec1,
                        const VectorRegister vec2,
                        const VectorWidth bw) noexcept;

public:
  static VectorParallelAndInst create(const VectorRegister target,
                                      const VectorRegister vec1,
                                      const VectorRegister vec2,
                                      const VectorWidth bw) noexcept;
  std::string getAssembly() const noexcept;
};

class VectorParallelOrInst : public VectorInst<VectorParallelOrInst> {
private:
  VectorParallelOrInst(const VectorRegister target, const VectorRegister vec1,
                       const VectorRegister vec2,
                       const VectorWidth bw) noexcept;

public:
  static VectorParallelOrInst create(const VectorRegister target,
                                     const VectorRegister vec1,
                                     const VectorRegister vec2,
                                     const VectorWidth bw) noexcept;
  std::string getAssembly() const noexcept;
};

class VectorParallelXorInst : public VectorInst<VectorParallelXorInst> {
private:
  VectorParallelXorInst(const VectorRegister target, const VectorRegister vec1,
                        const VectorRegister vec2,
                        const VectorWidth bw) noexcept;

public:
  static VectorParallelXorInst create(const VectorRegister target,
                                      const VectorRegister vec1,
                                      const VectorRegister vec2,
                                      const VectorWidth bw) noexcept;
  std::string getAssembly() const noexcept;
};

class VectorParallelShlInst : public VectorInst<VectorParallelShlInst> {
private:
  VectorParallelShlInst(const VectorRegister target, const VectorRegister vec1,
                        const VectorRegister vec2,
                        const VectorWidth bw) noexcept;

public:
  static VectorParallelShlInst create(const VectorRegister target,
                                      const VectorRegister vec1,
                                      const VectorRegister vec2,
                                      const VectorWidth bw) noexcept;
  std::string getAssembly() const noexcept;
};

class VectorParallelLShrInst : public VectorInst<VectorParallelLShrInst> {
private:
  VectorParallelLShrInst(const VectorRegister target, const VectorRegister vec1,
                         const VectorRegister vec2,
                         const VectorWidth bw) noexcept;

public:
  static VectorParallelLShrInst create(const VectorRegister target,
                                       const VectorRegister vec1,
                                       const VectorRegister vec2,
                                       const VectorWidth bw) noexcept;
  std::string getAssembly() const noexcept;
};

class VectorParallelAShrInst : public VectorInst<VectorParallelAShrInst> {
private:
  VectorParallelAShrInst(const VectorRegister target, const VectorRegister vec1,
                         const VectorRegister vec2,
                         const VectorWidth bw) noexcept;

public:
  static VectorParallelAShrInst create(const VectorRegister target,
                                       const VectorRegister vec1,
                                       const VectorRegister vec2,
                                       const VectorWidth bw) noexcept;
  std::string getAssembly() const noexcept;
};

class VectorParallelCompInst : public VectorInst<VectorParallelCompInst> {
private:
  IcmpCondition cond;
  VectorParallelCompInst(const VectorRegister target, const IcmpCondition cond,
                         const VectorRegister vec1, const VectorRegister vec2,
                         const VectorWidth bw) noexcept;

public:
  static VectorParallelCompInst create(const VectorRegister target,
                                       const IcmpCondition cond,
                                       const VectorRegister vec1,
                                       const VectorRegister vec2,
                                       const VectorWidth bw) noexcept;
  std::string getAssembly() const noexcept;
};

class VectorParallelSelectInst : public VectorInst<VectorParallelSelectInst> {
private:
  VectorRegister vec_cond;
  VectorParallelSelectInst(const VectorRegister target,
                           const VectorRegister vec_cond,
                           const VectorRegister vec1, const VectorRegister vec2,
                           const VectorWidth bw) noexcept;

public:
  static VectorParallelSelectInst create(const VectorRegister target,
                                         const VectorRegister vec_cond,
                                         const VectorRegister vec1,
                                         const VectorRegister vec2,
                                         const VectorWidth bw) noexcept;
  std::string getAssembly() const noexcept;
};

class VectorBroadcastInst : public AbstractInst<VectorBroadcastInst> {
private:
  VectorRegister target;
  ScalarRegisterTy val;
  VectorWidth bw;
  VectorBroadcastInst(const VectorRegister target, const ScalarRegisterTy val,
                      const VectorWidth bw) noexcept;

public:
  static VectorBroadcastInst create(const VectorRegister target,
                                    const ScalarRegisterTy val,
                                    const VectorWidth bw) noexcept;
  std::string getAssembly() const noexcept;
};

class VectorExtractInst : public AbstractInst<VectorExtractInst> {
private:
  GeneralRegister target;
  VectorRegister vec;
  ScalarRegisterTy idx;
  VectorWidth bw;
  VectorExtractInst(const GeneralRegister target, const VectorRegister vec,
                    const ScalarRegisterTy idx, const VectorWidth bw) noexcept;

public:
  static VectorExtractInst create(const GeneralRegister target,
                                  const VectorRegister vec,
                                  const ScalarRegisterTy idx,
                                  const VectorWidth bw) noexcept;
  std::string getAssembly() const noexcept;
};

class VectorUpdateInst : public AbstractInst<VectorUpdateInst> {
private:
  VectorRegister target;
  VectorRegister vec;
  ScalarRegisterTy val;
  ScalarRegisterTy idx;
  VectorWidth bw;
  VectorUpdateInst(const VectorRegister target, const VectorRegister vec,
                   const ScalarRegisterTy val, const ScalarRegisterTy idx,
                   const VectorWidth bw) noexcept;

public:
  static VectorUpdateInst create(const VectorRegister target,
                                 const VectorRegister vec,
                                 const ScalarRegisterTy val,
                                 const ScalarRegisterTy idx,
                                 const VectorWidth bw) noexcept;
  std::string getAssembly() const noexcept;
};
} // namespace sc::backend::assembly::inst
#endif // SC_LIB_BACKEND_ASSEMBLY_INST_H
