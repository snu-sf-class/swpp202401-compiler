cmake_minimum_required(VERSION 3.28.0)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_SCAN_FOR_MODULES OFF)

find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVM in: ${LLVM_BINARY_DIR}")

add_compile_options("-Wno-deprecated")
if(APPLE)
    link_directories(BEFORE ${LLVM_BINARY_DIR}/lib)
elseif(LINUX)
    add_compile_options(-stdlib=libc++)
    add_link_options(-stdlib=libc++)
else()
    message(FATAL "Unsupported system! Aborting CMake configuration...")
endif()

add_library(Fs fs.cpp)

add_library(SCArgs args.cpp)

set(PROJECT_LIB "SComp")
add_library(${PROJECT_LIB} lib.cpp)
target_include_directories(${PROJECT_LIB} PRIVATE ${LLVM_INCLUDE_DIRS})
llvm_map_components_to_libnames(comp_llvm_libs passes analysis)
target_link_libraries(${PROJECT_LIB} PRIVATE
                         Fs SCBackend SCPrintIR SCOpt SCParser ${comp_llvm_libs})

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})
add_executable(${PROJECT_NAME} main.cpp)
target_link_libraries(${PROJECT_NAME} PRIVATE ${PROJECT_LIB} SCArgs)

add_subdirectory(lib)
