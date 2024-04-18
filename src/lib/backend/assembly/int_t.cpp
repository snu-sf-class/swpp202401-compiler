#include "int_t.h"

namespace sc::backend::assembly::int_t {
std::string getToken(const IntTy __value) noexcept {
  return std::to_string(__value);
}
} // namespace sc::backend::assembly::int_t
