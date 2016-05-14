set -e

exe() { echo "\$ $@" ; "$@" ; }

vendor_bin="./vendor/llvm/build/bin"

exe clang++ -g -O3 \
    toy.cpp \
    `$vendor_bin/llvm-config --cxxflags --ldflags --system-libs --libs core mcjit native` \
    -o toy


