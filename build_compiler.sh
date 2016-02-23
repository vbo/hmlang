set -e

exe() { echo "\$ $@" ; "$@" ; }

# opt-3.7 -O3 example.ll -S -o example.ll.opt
# llc-3.7 example.ll.opt -filetype obj -relocation-model pic -march x86 -o build/example



CLANGXX="/usr/local/opt/llvm37/bin/clang++-3.7"
CLANG="/usr/local/opt/llvm37/bin/clang-3.7"
LLC="/usr/local/opt/llvm37/bin/llc-3.7"
LIBCXXINCLUDE="-nostdinc++ -I/usr/local/opt/llvm37/lib/llvm-3.7/include/c++/v1"
LIBCXXLIB="-L/usr/local/opt/llvm37/lib/llvm-3.7/lib"
LLVMCONFIG="/usr/local/opt/llvm37/bin/llvm-config-3.7"

exe mkdir -p build

echo "# Building scratch pads..."
$LLC -O0 -march=cpp scratch.ll -o build/scratch.cpp
$CLANG -c -emit-llvm -S scratch.c -o build/scratch.c.ll

BUILD_LLVM_MIDDLEWARE=""
if [ ! -f build/llvm_middleware.dylib ]; then
    echo "# Building LLVM middleware for the first time"
    BUILD_LLVM_MIDDLEWARE="yes"
else
    if [ llvm_middleware.cpp -nt build/llvm_middleware.dylib ] || [ middleware.h -nt build/llvm_middleware.dylib ]; then
        echo "# Building LLVM middleware for updated source"
        BUILD_LLVM_MIDDLEWARE="yes"
    fi
fi

if [[ -z "$BUILD_LLVM_MIDDLEWARE" ]]; then
    echo "# Skipping LLVM middleware build - no changes"
else
    exe $CLANGXX -g -dynamiclib \
        `$LLVMCONFIG --cxxflags --ldflags --system-libs --libs core mcjit native` \
        llvm_middleware.cpp \
        -o build/llvm_middleware.dylib
    if [ $? -eq 0 ]; then echo "# -> Success"; else exit 1; fi
    echo
fi

echo "# Building driver and frontend"
exe $CLANGXX -g -std=c++14 -stdlib=libc++ \
    $LIBCXXINCLUDE \
    $LIBCXXLIB \
    main.cpp build/llvm_middleware.dylib \
    -o build/compiler

if [ $? -eq 0 ]; then echo "# -> Success"; else exit 1; fi
echo
