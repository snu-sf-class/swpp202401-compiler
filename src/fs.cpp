#include "fs.h"

#include <__expected/unexpected.h>
#include <filesystem>
#include <fstream>
#include <system_error>

namespace stdfs = std::filesystem;

namespace fs {
FilesystemError::FilesystemError(const std::string_view __message,
                                 const std::string_view __path) noexcept {
  using namespace std::string_literals;
  message = "filesystem error: "s;
  message.reserve(message.size() + __message.size() + __path.size() + 3);
  message.append(__message).append(" ["s).append(__path).append("]");
}

std::expected<std::string, FilesystemError>
readFile(const std::string_view __filepath) noexcept {
  using RetType = std::expected<std::string, FilesystemError>;

  const auto path = stdfs::absolute(stdfs::path(__filepath));
  std::error_code ec;
  const auto status = stdfs::status(path, ec);

  if (!stdfs::exists(status)) {
    return RetType::unexpected_type(
        FilesystemError("file does not exist", path.string()));
  } else if (!stdfs::is_regular_file(status)) {
    return RetType::unexpected_type(
        FilesystemError("tried to read nonregular file", path.string()));
  }

  std::ifstream ifstr(path);
  ifstr.seekg(0, std::ios::end);
  const auto size = ifstr.tellg();
  std::string content(size, '\0');
  ifstr.seekg(0, std::ios::beg);
  ifstr.read(content.data(), size);
  return RetType(std::move(content));
}

std::expected<size_t, FilesystemError>
writeFile(const std::string_view __filepath,
          const std::string_view __content) noexcept {
  using RetType = std::expected<size_t, FilesystemError>;
  const auto path = stdfs::absolute(stdfs::path(__filepath));
  std::error_code ec, parent_ec;
  const auto status = stdfs::status(path, ec);
  const auto parent_status = stdfs::status(path.parent_path(), parent_ec);

  if (!stdfs::is_directory(parent_status)) {
    return RetType::unexpected_type(FilesystemError(
        "parent path is not a directory", path.parent_path().string()));
  } else if (!stdfs::exists(status)) {
    ; // create new file. no problem!
  } else if (!stdfs::is_regular_file(status)) {
    return RetType::unexpected_type(
        FilesystemError("tried to write at nonregular file", path.string()));
  }

  std::ofstream ofstr(path);
  ofstr.write(__content.data(), __content.size());
  return RetType(__content.size());
}
} // namespace fs
