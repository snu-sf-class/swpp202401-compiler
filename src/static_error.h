#ifndef SC_STATIC_ERROR_H
#define SC_STATIC_ERROR_H

/**
 * @file static_error.h
 * @author SWPP TAs (swpp@sf.snu.ac.kr)
 * @brief Header-only module for statically typed exceptions
 * @version 2024.1.13
 * @date 2024-05-31
 * @copyright Copyright (c) 2022-2024 SWPP TAs
 */

#include <concepts>
#include <exception>

namespace static_error {
/**
 * @brief CRTP interface for statically typed exceptions
 *
 * This provides some boilerplate for static exceptions.
 * Derived exceptions can be thrown and caught just like ordinary exceptions.
 * But because they are statically typed, they can be read without RTTI overhead
 * @tparam E Derived exception
 */
template <typename E> class Error : public std::exception {
private:
  /**
   * @brief Construct a new Error object
   */
  Error() noexcept {}
  friend E;

public:
  /**
   * @brief Read the exception
   * @return Exception message in C-String format
   */
  const char *what() const noexcept override {
    return static_cast<const E *>(this)->what();
  }

  /**
   * @brief Copy-construct an Error object
   * @param other Error to copy from
   */
  Error(const Error &other) = default;

  /**
   * @brief Copy-assign an Error object
   * @param other Error to copy from
   */
  Error &operator=(const Error &other) = default;
};

template <typename E>
concept ErrorLike = std::copyable<E> && requires(const E &error) {
  { error.what() } noexcept -> std::same_as<const char *>;
};
} // namespace static_error
#endif // SC_STATIC_ERROR_H
