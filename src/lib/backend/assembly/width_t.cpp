#include "width_t.h"

using namespace std::string_literals;

namespace sc::backend::assembly::width_t {
std::string getToken(const AccessWidth __width) noexcept {
  switch (__width) {
  case AccessWidth::BYTE:
    return "1"s;
  case AccessWidth::WORD:
    return "2"s;
  case AccessWidth::DWORD:
    return "4"s;
  case AccessWidth::QWORD:
    return "8"s;
  case AccessWidth::VECTOR:
    return "32"s;
  default: // unreachable
    return ";"s;
  }
}

std::string getToken(const BitWidth __width) noexcept {
  switch (__width) {
  case BitWidth::BIT:
    return "1"s;
  case BitWidth::BYTE:
    return "8"s;
  case BitWidth::WORD:
    return "16"s;
  case BitWidth::DWORD:
    return "32"s;
  case BitWidth::QWORD:
    return "64"s;
  default: // unreachable
    return ";"s;
  }
}

std::string getToken(const VectorWidth __width) noexcept {
  switch (__width) {
  case VectorWidth::DWORD:
    return "32"s;
  case VectorWidth::QWORD:
    return "64"s;
  default: // unreachable
    return ";"s;
  }
}

} // namespace sc::backend::assembly::width_t
