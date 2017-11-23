# The Region Vectorizer (LLVM 5.0 version)

Compiler Design Lab / Saarland University / Saarland Informatics Campus


The Region Vectorizer (RV) is a general-purpose vectorization framework for LLVM.
RV provides a unified interface to vectorize code regions, such as inner and outer loops, up to whole functions.

We had a poster on RV at the 2016 US LLVM Developers' Meeting (docs/poster_rv.pdf).
The initial version of RV was a fork of the Whole-Function Vectorizer by Ralf Karrenberg (http://www.cdl.uni-saarland.de/projects/wfv/).

For any questions, please get in touch with Simon Moll (moll@cs.uni-saarland.de).

## Buildling libRV

RV is an LLVM tool project and integrates into the LLVM build system.
Clone this repository into llvm/tools/rv where llvm is your LLVM source directory. RV will build along with LLVM.
To enable vectorized complex arithmetic through compiler-rt checkout compiler-rt in llvm/runtimes and configure RV with `RV_ENABLE_CRT=on`.

### Build prerequisites 

* LLVM 5.0 (cmake)
* Clang (for the vector math libraries)
* compiler-rt [optional] (for complex arithmetic functions)



## Testing

Install LLVM+RV, go to rv/test/ and run ./test_rv.py.

## RV's Outer-Loop Vectorizer

RV ships with an Outer-Loop Vectorizer that picks up on SIMD pragmas in your code.
RV is designed to deal with any control flow inside those loops. However, the annotated loops themselves need to be parallel counting loops.
RV supports a range of value reductions and recurrences, including conditional ones (e.g. `if (i % 3 == 0) a += A[i];` ).
Be aware that RV will exactly do as you annotated. Specifically, RV does not perform exhaustive legality checks nor is there cost modelling of any kind.
You'll get what you ordered.

### Usage

1. Annotate vectorizable loops with `#pragma clang loop vectorize(assume_safety) vectorize_width(W)` where W is the desired vectorization width.
2. Invoke clang with `-Xclang -load -Xclang libRV.so -mllvm -rv-loopvec`. We recommend to also disable loop unrolling `-fno-unroll-loops`.

## Getting started on the code

Users of RV should include its main header file include/rv/rv.h and supporting headers in include/rv.
The command line tester (tool/rvTool.cpp) is a good starting point to learn how to use RVs API.

## Source structure
* include/ - header files
* src/ - source files
* vecmath/ - SIMD library sources
* test/ - tests
* tool/ - sources of rvTool



## Advanced options

### environment variables

RV's diagnostic output can be configured through a couple of environment variables. These will be read by the Outer-Loop Vectorizer and rvTool.
To get a short diagnostic report from every transformation in RV, set the environment variable `RV_REPORT` to any value but `0`.
To also get a report from RV's Outer-Loop Vectorizer, set the environment variable `LV_DIAG` to a non-`0` value to get feedback.

### cmake options

* `RV_ENABLE_CRT:BOOL`
whether RV should inline and vectorize complex math functions. This makes use of the complex arithmetic in compiler-rt. Defaults to OFF.
* `RV_TARGETS_TO_BUILD:ListOfTargets`
List of LLVM targets, for which the SLEEF vector math library should be build. Same format as LLVM_TARGETS_TO_BUILD. RV uses SLEEF to vectorize math functions. Clang has to be able to (cross-)compile for all of these targets or the build will fail. Defaults to "Native", the host target.
* `RV_DEBUG:BOOL`
If enabled, RV will produce (very) verbose debug output and run additional consistency checks. Make sure you compile with assertions. Recommended for debugging only. Defaults to OFF.




The Region Vectorizer is distributed under the University of Illinois Open Source
License. See LICENSE.TXT for details.

