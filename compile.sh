#!/bin/bash

# Edit two variables below to match your system configuration
CMAKE_DIR=~/cmake-3.28.3
LLVM_DIR=~/llvm-18.1.0
NINJA_DIR=~/ninja-1.11.1

CMAKE=$CMAKE_DIR/bin/cmake
CLANG=$LLVM_DIR/bin/clang
CLANGXX=$LLVM_DIR/bin/clang++
LLD=$LLVM_DIR/bin/ld.lld
NINJA=$NINJA_DIR/bin/ninja

$CMAKE -G Ninja -B build \
    -DCMAKE_C_COMPILER=$CLANG \
    -DCMAKE_CXX_COMPILER=$CLANGXX \
    -DCMAKE_EXE_LINKER_FLAGS="-fuse-ld=$LLD" \
    -DCMAKE_SHARED_LINKER_FLAGS="-fuse-ld=$LLD" \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_MAKE_PROGRAM=$NINJA \
    -DBUILD_SHARED_LIBS=ON \
    -DCMAKE_POSITION_INDEPENDENT_CODE=ON \
    -DLLVM_ROOT=$LLVM_DIR \

# Build compiler and documentation
# $CMAKE --build build

# Only build compiler
$CMAKE --build build --target swpp-compiler