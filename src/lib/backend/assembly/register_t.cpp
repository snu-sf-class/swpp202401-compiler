#include "register_t.h"

using namespace std::string_literals;

namespace sc::backend::assembly::register_t {
std::string getToken(const GeneralRegister __reg) noexcept {
  switch (__reg) {
  case GeneralRegister::R1:
    return "r1"s;
  case GeneralRegister::R2:
    return "r2"s;
  case GeneralRegister::R3:
    return "r3"s;
  case GeneralRegister::R4:
    return "r4"s;
  case GeneralRegister::R5:
    return "r5"s;
  case GeneralRegister::R6:
    return "r6"s;
  case GeneralRegister::R7:
    return "r7"s;
  case GeneralRegister::R8:
    return "r8"s;
  case GeneralRegister::R9:
    return "r9"s;
  case GeneralRegister::R10:
    return "r10"s;
  case GeneralRegister::R11:
    return "r11"s;
  case GeneralRegister::R12:
    return "r12"s;
  case GeneralRegister::R13:
    return "r13"s;
  case GeneralRegister::R14:
    return "r14"s;
  case GeneralRegister::R15:
    return "r15"s;
  case GeneralRegister::R16:
    return "r16"s;
  case GeneralRegister::R17:
    return "r17"s;
  case GeneralRegister::R18:
    return "r18"s;
  case GeneralRegister::R19:
    return "r19"s;
  case GeneralRegister::R20:
    return "r20"s;
  case GeneralRegister::R21:
    return "r21"s;
  case GeneralRegister::R22:
    return "r22"s;
  case GeneralRegister::R23:
    return "r23"s;
  case GeneralRegister::R24:
    return "r24"s;
  case GeneralRegister::R25:
    return "r25"s;
  case GeneralRegister::R26:
    return "r26"s;
  case GeneralRegister::R27:
    return "r27"s;
  case GeneralRegister::R28:
    return "r28"s;
  case GeneralRegister::R29:
    return "r29"s;
  case GeneralRegister::R30:
    return "r30"s;
  case GeneralRegister::R31:
    return "r31"s;
  case GeneralRegister::R32:
    return "r32"s;
  case GeneralRegister::SP:
    return "sp"s;
  default: // unreachable
    return ";"s;
  }
}

std::string getToken(const ArgumentRegister __reg) noexcept {
  switch (__reg) {
  case ArgumentRegister::A1:
    return "arg1"s;
  case ArgumentRegister::A2:
    return "arg2"s;
  case ArgumentRegister::A3:
    return "arg3"s;
  case ArgumentRegister::A4:
    return "arg4"s;
  case ArgumentRegister::A5:
    return "arg5"s;
  case ArgumentRegister::A6:
    return "arg6"s;
  case ArgumentRegister::A7:
    return "arg7"s;
  case ArgumentRegister::A8:
    return "arg8"s;
  case ArgumentRegister::A9:
    return "arg9"s;
  case ArgumentRegister::A10:
    return "arg10"s;
  case ArgumentRegister::A11:
    return "arg11"s;
  case ArgumentRegister::A12:
    return "arg12"s;
  case ArgumentRegister::A13:
    return "arg13"s;
  case ArgumentRegister::A14:
    return "arg14"s;
  case ArgumentRegister::A15:
    return "arg15"s;
  case ArgumentRegister::A16:
    return "arg16"s;
  default: // unreachable
    return ";"s;
  }
}

// TODO: VectorRegister

std::string getToken(const VectorRegister __reg) noexcept {
  switch (__reg) {
  case VectorRegister::V1:
    return "v1"s;
  case VectorRegister::V2:
    return "v2"s;
  case VectorRegister::V3:
    return "v3"s;
  case VectorRegister::V4:
    return "v4"s;
  case VectorRegister::V5:
    return "v5"s;
  case VectorRegister::V6:
    return "v6"s;
  case VectorRegister::V7:
    return "v7"s;
  case VectorRegister::V8:
    return "v8"s;
  case VectorRegister::V9:
    return "v9"s;
  case VectorRegister::V10:
    return "v10"s;
  case VectorRegister::V11:
    return "v11"s;
  case VectorRegister::V12:
    return "v12"s;
  case VectorRegister::V13:
    return "v13"s;
  case VectorRegister::V14:
    return "v14"s;
  case VectorRegister::V15:
    return "v15"s;
  case VectorRegister::V16:
    return "v16"s;
  default: // unreachable
    return ";"s;
  }
}

} // namespace sc::backend::assembly::register_t
