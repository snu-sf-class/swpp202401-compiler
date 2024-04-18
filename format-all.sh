#!/bin/bash

find . -iname '*.h' -o -iname '*.cpp' | xargs clang-format -i -style=file:./.clang-format
