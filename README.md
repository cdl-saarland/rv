# The Region Vectorizer for SX-Aurora

NEC Deutschland


The Region Vectorizer (RV) is a general-purpose vectorization framework for LLVM.
RV provides a unified interface to vectorize code regions, such as inner and outer loops, up to whole functions.

## Features

* Support for tail-predicated OLV through [LLVM-VP](https://reviews.llvm.org/D57504).
* Support for OpenMP 4.5 `#pragma omp simd` **and** `#pragma omp declare simd` (pass `-fopenmp -fplugin=libRV.so -mllvm -rv` to Clang and you are set).
* Automatic outer-loop vectorization (preview feature) (pass `-mllvm -rv-autovec` to enable).
* Support for inter-procedural/recursive vectorization.
* Implements [*Partial Control-Flow Linearization*](http://compilers.cs.uni-saarland.de/papers/moll_parlin_pldi18.pdf), S. Moll and S. Hack (PLDI '18).
* Automatically uses [SLEEF](https://github.com/shibatch/sleef) vector math functions.
* Whole-Function vectorizer (`min -> min_avx2`).
* Outer-loop vectorizer.

## Buildling libRV

RV is an LLVM project and integrates into the LLVM build system.
Clone this repository into llvm-project/rv where llvm-project is your LLVM source directory.
To build RV along with LLVM, you need to tell cmake where to find RV.
This can be done by specifying `-DLLVM_EXTERNAL_PROJECTS="rv" -DLLVM_EXTERNAL_RV_SOURCE_DIR=llvm-project/rv` to cmake.
Run `git submodule update --init` to pull the SLEEF submodule.
To (optionally) enable vectorized complex arithmetic through compiler-rt checkout compiler-rt in llvm/runtimes and configure cmake with `-DRV_ENABLE_CRT=on`.

### Build prerequisites 

* LLVM trunk (as of latest commit on this branch)
* Clang (for the vector math libraries)
* compiler-rt [optional] (for complex arithmetic functions)


## Testing

Install LLVM+RV, go to rv/test/ and run ./test_rv.py.

## RV's Outer-Loop Vectorizer

RV ships with frontend passes for Outer-Loop and Whole-Function Vectorization.
The passes pick up on SIMD pragmas in your code to vectorize the region (loop or function) in question.
RV is designed to deal with any control flow inside those regions. However, in case of loop vectorization the annotated loops themselves need to be parallel counting loops.
RV supports a range of value reductions and recurrences, including conditional ones (e.g. `if (i % 3 == 0) a += A[i];` ).
Be aware that RV will exactly do as you annotated. Specifically, RV does not perform exhaustive legality checks nor is there cost modelling of any kind.
You'll get what you ordered.

### Usage

1. Annotate vectorizable loops with `#pragma clang loop vectorize(assume_safety) vectorize_width(W)` where W is the desired vectorization width.
2. Invoke clang with `-fplugin=libRV.so -mllvm -rv-loopvec`. We recommend to also disable loop unrolling `-fno-unroll-loops`.

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
To also get a report from RV's Outer-Loop Vectorizer, set the environment variable `LV_DIAG` to a non-`0` value.

### Optional cmake flags

* `RV_ENABLE_CRT:BOOL`
Whether RV should inline and vectorize complex math functions. This makes use of the complex arithmetic implementations in compiler-rt. Requires compiler-rt to live in llvm/projects. Defaults to OFF.
* `RV_TARGETS_TO_BUILD:ListOfTargets`
List of LLVM targets, for which the SLEEF vector math library should be built. Same format as `LLVM_TARGETS_TO_BUILD`. RV uses SLEEF to vectorize math functions. Clang has to be able to (cross-)compile for all of these targets or the build will fail. Defaults to "Native", the host target.
* `RV_DEBUG:BOOL`
If enabled, RV will produce (very) verbose debug output and run additional consistency checks. Make sure you compile with assertions. Recommended for debugging only. Defaults to OFF.
* `LLVM_RVPLUG_LINK_INTO_TOOLS:BOOL`
Enables the LLVM pass plugin mechanism to link RV into all LLVM tools (opt, clang, ..). Obviates the need to load libRV manually as a plugin on the command line.




The Region Vectorizer is distributed under the University of Illinois Open Source
License. See LICENSE.TXT for details.

