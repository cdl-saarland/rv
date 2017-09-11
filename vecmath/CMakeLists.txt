SET ( VECMATH_LIB_DIR ${RV_BINARY_DIR}/lib )
# Sleef lib
SET ( SLEEF_DIR "${RV_SOURCE_DIR}/vecmath/sleef" CACHE STRING "Path to libsleef sources" FORCE )
SET ( SLEEF_ARCH_DIR "${SLEEF_DIR}/src/arch" )
SET ( SLEEF_COMMON_DIR "${SLEEF_DIR}/src/common" )
SET ( RV_LIB_SLEEF_DIR "${SLEEF_DIR}/src/libm" )
SET ( RV_LIB_EXTRAS_DIR "${RV_SOURCE_DIR}/vecmath/extras" )

# compiler-rt
SET ( CRT_BC "${VECMATH_LIB_DIR}/crt.bc" )
SET ( CRT_GENBC "${VECMATH_LIB_DIR}/crt.gen.cpp" )
SET ( CRT_INC "${RV_SOURCE_DIR}/../../runtimes/compiler-rt/lib/builtins" )

# extra libs
SET ( SLEEF_BC_ADVSIMD_SP )
SET ( SLEEF_GENBC_ADVSIMD_SP )

# SLEEF bc build paths

IF (RV_ENABLE_X86)
Message("-- rv: SLEEF with x86 support.")
SET ( SLEEF_BC_AVX512_SP "${VECMATH_LIB_DIR}/avx512_sleef_sp.bc" )
SET ( SLEEF_GENBC_AVX512_SP "${VECMATH_LIB_DIR}/avx512_sleef_sp.gen.cpp" )

SET ( SLEEF_BC_AVX512_DP "${VECMATH_LIB_DIR}/avx512_sleef_dp.bc" )
SET ( SLEEF_GENBC_AVX512_DP "${VECMATH_LIB_DIR}/avx512_sleef_dp.gen.cpp" )

SET ( EXTRAS_AVX512_BC "${VECMATH_LIB_DIR}/avx512_extras.bc" )
SET ( EXTRAS_AVX512_GENBC "${VECMATH_LIB_DIR}/avx512_extras.gen.cpp" )

SET ( SLEEF_BC_AVX2_SP "${VECMATH_LIB_DIR}/avx2_sleef_sp.bc" )
SET ( SLEEF_GENBC_AVX2_SP "${VECMATH_LIB_DIR}/avx2_sleef_sp.gen.cpp" )

SET ( EXTRAS_AVX2_BC "${VECMATH_LIB_DIR}/avx2_extras.bc" )
SET ( EXTRAS_AVX2_GENBC "${VECMATH_LIB_DIR}/avx2_extras.gen.cpp" )

SET ( SLEEF_BC_AVX_SP "${VECMATH_LIB_DIR}/avx_sleef_sp.bc" )
SET ( SLEEF_GENBC_AVX_SP "${VECMATH_LIB_DIR}/avx_sleef_sp.gen.cpp" )

SET ( SLEEF_BC_SSE_SP "${VECMATH_LIB_DIR}/sse_sleef_sp.bc" )
SET ( SLEEF_GENBC_SSE_SP "${VECMATH_LIB_DIR}/sse_sleef_sp.gen.cpp" )

SET ( SLEEF_BC_AVX2_DP "${VECMATH_LIB_DIR}/avx2_sleef_dp.bc" )
SET ( SLEEF_GENBC_AVX2_DP "${VECMATH_LIB_DIR}/avx2_sleef_dp.gen.cpp" )

SET ( SLEEF_BC_AVX_DP "${VECMATH_LIB_DIR}/avx_sleef_dp.bc" )
SET ( SLEEF_GENBC_AVX_DP "${VECMATH_LIB_DIR}/avx_sleef_dp.gen.cpp" )

SET ( SLEEF_BC_SSE_DP "${VECMATH_LIB_DIR}/sse_sleef_dp.bc" )
SET ( SLEEF_GENBC_SSE_DP "${VECMATH_LIB_DIR}/sse_sleef_dp.gen.cpp" )

SET ( SLEEF_BC_LIBS ${SLEEF_BC_LIBS} ${SLEEF_BC_AVX2_SP} ${SLEEF_BC_AVX_SP} ${SLEEF_BC_SSE_SP} ${SLEEF_BC_AVX2_DP} ${SLEEF_BC_AVX_DP} ${SLEEF_BC_SSE_DP} ${EXTRAS_AVX2_BC} ${EXTRAS_AVX512_BC} )
SET ( RV_GENBC_SOURCES ${RV_GENBC_SOURCES} ${SLEEF_GENBC_AVX2_SP} ${SLEEF_GENBC_AVX_SP} ${SLEEF_GENBC_SSE_SP} ${SLEEF_GENBC_AVX2_DP} ${SLEEF_GENBC_AVX_DP} ${SLEEF_GENBC_SSE_DP}  ${SLEEF_GENBC_AVX512_SP} ${SLEEF_GENBC_AVX512_DP} ${EXTRAS_AVX2_GENBC} ${EXTRAS_AVX512_GENBC} )
ENDIF()

IF (RV_ENABLE_ADVSIMD)
  Message("-- rv: SLEEF with Advanced SIMD support.")

SET ( SLEEF_BC_ADVSIMD_DP "${VECMATH_LIB_DIR}/advsimd_sleef_dp.bc" )
SET ( SLEEF_GENBC_ADVSIMD_DP "${VECMATH_LIB_DIR}/advsimd_sleef_dp.gen.cpp" )

SET ( SLEEF_BC_ADVSIMD_SP "${VECMATH_LIB_DIR}/advsimd_sleef_sp.bc" )
SET ( SLEEF_GENBC_ADVSIMD_SP "${VECMATH_LIB_DIR}/advsimd_sleef_sp.gen.cpp" )

SET ( EXTRAS_ADVSIMD_BC "${VECMATH_LIB_DIR}/advsimd_extras.bc" )
SET ( EXTRAS_ADVSIMD_GENBC "${VECMATH_LIB_DIR}/advsimd_extras.gen.cpp" )

SET ( SLEEF_BC_LIBS ${SLEEF_BC_LIBS} ${SLEEF_BC_ADVSIMD_SP} ${SLEEF_BC_ADVSIMD_DP} ${EXTRAS_ADVSIMD_BC} )
SET ( RV_GENBC_SOURCES ${RV_GENBC_SOURCES} ${SLEEF_GENBC_ADVSIMD_SP} ${SLEEF_GENBC_ADVSIMD_DP} ${EXTRAS_ADVSIMD_GENBC} )
ENDIF()

SET( FLAGS_AVX2 -mavx2 )
SET( FLAGS_AVX512 -mavx512f -mavx512dq -mavx512cd -mavx512bw -mavx512vl -march=skylake )
SET( FLAGS_ADVSIMD -target aarch64-unknown-linux-gnu -mcpu=cortex-a53 )

IF (RV_ENABLE_CRT)
  SET ( RV_GENBC_SOURCES ${RV_GENBC_SOURCES} ${CRT_GENBC} )
ENDIF()

# conditionally provide extra sources
SET ( RV_LIB_SLEEF_OUT_DIR ${VECMATH_LIB_DIR} )

SET ( GENCPP_TOOL "${RV_SOURCE_DIR}/tools/gen_cpp.py" )

IF (${RV_ENABLE_SLEEF})
  SET(LLVM_TOOL_CLANG ${RV_BINARY_DIR}/../../bin/clang)
  # add SLEEF bc generators as extra source
  MESSAGE("-- rv: Building with SLEEF")

  # add SLEEF bc generators as extra source
  ADD_CUSTOM_TARGET ( libsleef_x64 DEPENDS ${RV_GENBC_SOURCES} )

  SET( RVLIB_BUILD_OPTS -O3 -I${RV_LIB_SLEEF_DIR}/../arch -I${RV_LIB_SLEEF_DIR}/../common)
  # extra libs
  # Sleef single precision
### ADVSIMD
IF (RV_ENABLE_ADVSIMD)
  ADD_CUSTOM_COMMAND (
    OUTPUT ${SLEEF_GENBC_ADVSIMD_DP}
    COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR}
    COMMAND ${LLVM_TOOL_CLANG} ${RV_LIB_SLEEF_DIR}/sleefsimddp.c -emit-llvm -c -Wall -Wno-unused ${RVLIB_BUILD_OPTS} ${FLAGS_ADVSIMD} -DENABLE_ADVSIMD=ON -ffp-contract=off -o ${SLEEF_BC_ADVSIMD_DP}
    COMMAND ${GENCPP_TOOL} ${SLEEF_GENBC_ADVSIMD_DP} \"advsimd_dp\" ${SLEEF_BC_ADVSIMD_DP}
    DEPENDS ${RV_LIB_SLEEF_DIR}/sleefsimddp.c ${LLVM_TOOL_CLANG}
  )
  ADD_CUSTOM_COMMAND (
    OUTPUT ${SLEEF_GENBC_ADVSIMD_SP}
    COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR}
    COMMAND ${LLVM_TOOL_CLANG} ${RV_LIB_SLEEF_DIR}/sleefsimdsp.c -emit-llvm -c -Wall -Wno-unused ${RVLIB_BUILD_OPTS} ${FLAGS_ADVSIMD} -DENABLE_ADVSIMD=ON -ffp-contract=off -o ${SLEEF_BC_ADVSIMD_SP}
    COMMAND ${GENCPP_TOOL} ${SLEEF_GENBC_ADVSIMD_SP} \"advsimd_sp\" ${SLEEF_BC_ADVSIMD_SP}
    DEPENDS ${RV_LIB_SLEEF_DIR}/sleefsimdsp.c ${LLVM_TOOL_CLANG}
  )
  ADD_CUSTOM_COMMAND (
    OUTPUT ${EXTRAS_ADVSIMD_GENBC}
    COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR}
    COMMAND ${LLVM_TOOL_CLANG} ${RV_LIB_EXTRAS_DIR}/vrand_advsimd.c -emit-llvm -c -Wall -Wno-unused ${RVLIB_BUILD_OPTS} ${FLAGS_ADVSIMD} -DENABLE_ADVSIMD=ON -ffp-contract=off -o ${EXTRAS_ADVSIMD_BC}
    COMMAND ${GENCPP_TOOL} ${EXTRAS_ADVSIMD_GENBC} \"advsimd_extras\" ${EXTRAS_ADVSIMD_BC}
    DEPENDS ${RV_LIB_EXTRAS_DIR}/vrand_advsimd.c ${LLVM_TOOL_CLANG}
  )
ENDIF()
### NEON32
  # NEON32 TODO
if (RV_ENABLE_X86)
### extra lib (avxmath)

### AVX512
  ADD_CUSTOM_COMMAND (
    OUTPUT ${SLEEF_GENBC_AVX512_SP}
    COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR}
    COMMAND ${LLVM_TOOL_CLANG} ${RV_LIB_SLEEF_DIR}/sleefsimdsp.c -emit-llvm -c -Wall -Wno-unused ${RVLIB_BUILD_OPTS} ${FLAGS_AVX512} -DENABLE_AVX512F=ON -ffp-contract=off -o ${SLEEF_BC_AVX512_SP}
    COMMAND ${GENCPP_TOOL} ${SLEEF_GENBC_AVX512_SP} \"avx512_sp\" ${SLEEF_BC_AVX512_SP}
    DEPENDS ${RV_LIB_SLEEF_DIR}/sleefsimddp.c ${LLVM_TOOL_CLANG}
  )
  ADD_CUSTOM_COMMAND (
    OUTPUT ${SLEEF_GENBC_AVX512_DP}
    COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR}
    COMMAND ${LLVM_TOOL_CLANG} ${RV_LIB_SLEEF_DIR}/sleefsimddp.c -emit-llvm -c -Wall -Wno-unused ${RVLIB_BUILD_OPTS} ${FLAGS_AVX512} -DENABLE_AVX512F=ON -ffp-contract=off -o ${SLEEF_BC_AVX512_DP}
    COMMAND ${GENCPP_TOOL} ${SLEEF_GENBC_AVX512_DP} \"avx512_dp\" ${SLEEF_BC_AVX512_DP}
    DEPENDS ${RV_LIB_SLEEF_DIR}/sleefsimddp.c ${LLVM_TOOL_CLANG}
  )
  ADD_CUSTOM_COMMAND (
    OUTPUT ${EXTRAS_AVX512_GENBC}
    COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR}
    COMMAND ${LLVM_TOOL_CLANG} ${RV_LIB_EXTRAS_DIR}/vrand_avx512.c -emit-llvm -c -Wall -Wno-unused ${RVLIB_BUILD_OPTS} ${FLAGS_AVX512} -DENABLE_AVX512F=ON -ffp-contract=off -o ${EXTRAS_AVX512_BC}
    COMMAND ${GENCPP_TOOL} ${EXTRAS_AVX512_GENBC} \"avx512_extras\" ${EXTRAS_AVX512_BC}
    DEPENDS ${RV_LIB_EXTRAS_DIR}/vrand_avx512.c ${LLVM_TOOL_CLANG}
  )
### AVX2
  ADD_CUSTOM_COMMAND (
    OUTPUT ${SLEEF_GENBC_AVX2_DP}
    COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR}
    COMMAND ${LLVM_TOOL_CLANG} ${RV_LIB_SLEEF_DIR}/sleefsimddp.c -emit-llvm -c -Wall -Wno-unused ${RVLIB_BUILD_OPTS} -mavx2 -mfma -DENABLE_AVX2=ON -ffp-contract=off -o ${SLEEF_BC_AVX2_DP}
    COMMAND ${GENCPP_TOOL} ${SLEEF_GENBC_AVX2_DP} \"avx2_dp\" ${SLEEF_BC_AVX2_DP}
    DEPENDS ${RV_LIB_SLEEF_DIR}/sleefsimddp.c ${LLVM_TOOL_CLANG}
  )
  ADD_CUSTOM_COMMAND (
    OUTPUT ${SLEEF_GENBC_AVX2_SP}
    COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR}
    COMMAND ${LLVM_TOOL_CLANG} ${RV_LIB_SLEEF_DIR}/sleefsimdsp.c -emit-llvm -c -Wall -Wno-unused ${RVLIB_BUILD_OPTS} -mavx2 -mfma -DENABLE_AVX2=ON -ffp-contract=off -o ${SLEEF_BC_AVX2_SP}
    COMMAND ${GENCPP_TOOL} ${SLEEF_GENBC_AVX2_SP} \"avx2_sp\" ${SLEEF_BC_AVX2_SP}
    DEPENDS ${RV_LIB_SLEEF_DIR}/sleefsimdsp.c ${LLVM_TOOL_CLANG}
  )
  ADD_CUSTOM_COMMAND (
    OUTPUT ${EXTRAS_AVX2_GENBC}
    COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR}
    COMMAND ${LLVM_TOOL_CLANG} ${RV_LIB_EXTRAS_DIR}/vrand_avx2.c -emit-llvm -c -Wall -Wno-unused ${RVLIB_BUILD_OPTS} -mavx2 -mfma -DENABLE_AVX2=ON -ffp-contract=off -o ${EXTRAS_AVX2_BC}
    COMMAND ${GENCPP_TOOL} ${EXTRAS_AVX2_GENBC} \"avx2_extras\" ${EXTRAS_AVX2_BC}
    DEPENDS ${RV_LIB_EXTRAS_DIR}/vrand_avx2.c ${LLVM_TOOL_CLANG}
  )
### AVX
  ADD_CUSTOM_COMMAND (
    OUTPUT ${SLEEF_GENBC_AVX_DP}
    COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR}
    COMMAND ${LLVM_TOOL_CLANG} ${RV_LIB_SLEEF_DIR}/sleefsimddp.c -emit-llvm -c -Wall -Wno-unused ${RVLIB_BUILD_OPTS} -DENABLE_AVX=ON -mavx -ffp-contract=off -o ${SLEEF_BC_AVX_DP}#.tmp
    COMMAND ${GENCPP_TOOL} ${SLEEF_GENBC_AVX_DP} \"avx_dp\" ${SLEEF_BC_AVX_DP}
    DEPENDS ${RV_LIB_SLEEF_DIR}/sleefsimddp.c ${LLVM_TOOL_CLANG}
  )
  ADD_CUSTOM_COMMAND (
    OUTPUT ${SLEEF_GENBC_AVX_SP}
    COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR}
    COMMAND ${LLVM_TOOL_CLANG} ${RV_LIB_SLEEF_DIR}/sleefsimdsp.c -emit-llvm -c -Wall -Wno-unused ${RVLIB_BUILD_OPTS} -mavx -DENABLE_AVX=ON -ffp-contract=off -o ${SLEEF_BC_AVX_SP}#.tmp
    COMMAND ${GENCPP_TOOL} ${SLEEF_GENBC_AVX_SP} \"avx_sp\" ${SLEEF_BC_AVX_SP}
    DEPENDS ${RV_LIB_SLEEF_DIR}/sleefsimdsp.c ${LLVM_TOOL_CLANG}
  )
### SSE
  ADD_CUSTOM_COMMAND (
    OUTPUT ${SLEEF_GENBC_SSE_SP}
    COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR}
    COMMAND ${LLVM_TOOL_CLANG} ${RV_LIB_SLEEF_DIR}/sleefsimdsp.c -emit-llvm -c -Wall -Wno-unused ${RVLIB_BUILD_OPTS} -m64 -msse2 -msse4.1 -DENABLE_SSE2=ON -ffp-contract=off -o ${SLEEF_BC_SSE_SP}#.tmp
    COMMAND ${GENCPP_TOOL} ${SLEEF_GENBC_SSE_SP} \"sse_sp\" ${SLEEF_BC_SSE_SP}
    DEPENDS ${RV_LIB_SLEEF_DIR}/sleefsimdsp.c ${LLVM_TOOL_CLANG}
  )
  ADD_CUSTOM_COMMAND (
    OUTPUT ${SLEEF_GENBC_SSE_DP}
    COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR}
    COMMAND ${LLVM_TOOL_CLANG} ${RV_LIB_SLEEF_DIR}/sleefsimddp.c -emit-llvm -c -Wall -Wno-unused ${RVLIB_BUILD_OPTS} -m64 -DENABLE_SSE2=ON -msse2 -msse4.1 -ffp-contract=off -o ${SLEEF_BC_SSE_DP}#.tmp
    COMMAND ${GENCPP_TOOL} ${SLEEF_GENBC_SSE_DP} \"sse_dp\" ${SLEEF_BC_SSE_DP}
    DEPENDS ${RV_LIB_SLEEF_DIR}/sleefsimddp.c ${LLVM_TOOL_CLANG}
  )
ENDIF()

  IF (RV_ENABLE_CRT) 
    # compiler-rt runtime library
    ADD_CUSTOM_COMMAND (
      OUTPUT ${CRT_GENBC}
      COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR}
      COMMAND ${LLVM_TOOL_CLANG} ${RV_SOURCE_DIR}/vecmath/crt.c -I${CRT_INC} -m64 -emit-llvm -c -Wall -Wno-unused ${RVLIB_BUILD_OPTS} -ffp-contract=off -o ${CRT_BC}
      COMMAND ${GENCPP_TOOL} ${CRT_GENBC} \"crt\" ${CRT_BC}
      DEPENDS ${RV_SOURCE_DIR}/vecmath/crt.c ${LLVM_TOOL_CLANG}
    )
    Message("-- rv: Building compiler-rt bc libs.")
  ELSE()
    Message("-- rv: Building without compiler-rt bc libs.")
  ENDIF()
  # generate static library
  add_rv_library(gensleef STATIC ${RV_GENBC_SOURCES})
ELSE()
    Message("-- rv: Buildling without SLEEF.")
ENDIF ()
