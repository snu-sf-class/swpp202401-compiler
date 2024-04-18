#ifndef SC_LIB_BACKEND_ASSEMBLY_INT_T_H
#define SC_LIB_BACKEND_ASSEMBLY_INT_T_H

#include <string>

namespace sc::backend::assembly::int_t {
using IntTy = uint64_t;

std::string getToken(const IntTy value) noexcept;
} // namespace sc::backend::assembly::int_t

#endif // SC_LIB_BACKEND_ASSEMBLY_INT_T_H
