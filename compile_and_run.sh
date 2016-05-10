set -e

exe() { echo "\$ $@" ; "$@" ; }

CLANGXX="/usr/local/opt/llvm37/bin/clang++-3.7"
LLC="/usr/local/opt/llvm37/bin/llc-3.7"
OPT="/usr/local/opt/llvm37/bin/opt-3.7"

echo "# Building .hm source to LLVM IR"
exe build/compiler $2 build/$2.out
if [ $? -eq 0 ]; then echo "# -> Success"; else exit 1; fi

if [ $1 != "-O0" ]; then
    echo "# Optimizing .ll"
    exe $OPT $1 build/$2.out -S -o build/$2.out.opt
    if [ $? -eq 0 ]; then echo "# -> Success"; else exit 1; fi
else
    echo "# Skipping .ll optimization"
    exe cp build/$2.out build/$2.out.opt
fi

echo "# Compiling .ll to obj"
exe $LLC build/$2.out.opt -filetype obj -relocation-model pic -o build/$2.o
if [ $? -eq 0 ]; then echo "# -> Success"; else exit 1; fi

echo "# Linking obj to produce a dylib"
exe ld build/$2.o \
    -dylib \
    -Z -dead_strip \
    -macosx_version_min 10.6 \
    -arch x86_64 \
    -o build/$2.dylib
if [ $? -eq 0 ]; then echo "# -> Success"; else exit 1; fi

echo "# Building runners..."
exe $CLANGXX -g runner.cpp build/$2.dylib -o build/runner
if [ $? -eq 0 ]; then echo "# -> Success"; else exit 1; fi

echo "# Running..."
exe build/runner
