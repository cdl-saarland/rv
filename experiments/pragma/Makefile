CXX=LV_DISABLE=1 clang++ -Xclang -load -Xclang libRV.so -emit-llvm -S -c -O3 -fno-vectorize -fno-slp-vectorize -mllvm -rv-loopvec 

all: build/test.ll build/test_2d.ll build/test_multi.ll

build/test.ll: test.cpp
	mkdir -p build
	RV_REPORT=1  $(CXX) $^ -o $@

build/test_2d.ll: test_2d.cpp
	mkdir -p build
	RV_REPORT=1 LV_DISABLE=1 $(CXX) $^ -o $@

build/test_multi.ll: test_multi.cpp
	mkdir -p build
	RV_REPORT=1 LV_DISABLE=1 $(CXX) $^ -o $@


