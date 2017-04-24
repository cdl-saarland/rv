SET ( VECMATH_LIB_DIR ${RV_BINARY_DIR}/lib )
# Sleef lib
SET ( SLEEF_DIR "${RV_SOURCE_DIR}/vecmath/sleef" CACHE STRING "Path to libsleef sources" FORCE )
SET ( RV_LIB_SLEEF_DIR "${SLEEF_DIR}/simd" )

# SLEEF bc build paths
SET ( SLEEF_BC_AVX2_SP "${VECMATH_LIB_DIR}/avx2_sleef_sp.bc" )
SET ( SLEEF_GENBC_AVX2_SP "${VECMATH_LIB_DIR}/avx2_sleef_sp.gen.cpp" )

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

SET ( SLEEF_BC_LIBS ${SLEEF_BC_AVX2_SP} ${SLEEF_BC_AVX_SP} ${SLEEF_BC_SSE_SP} ${SLEEF_BC_AVX2_DP} ${SLEEF_BC_AVX_DP} ${SLEEF_BC_SSE_DP} )
SET ( RV_GENBC_SOURCES ${SLEEF_GENBC_AVX2_SP} ${SLEEF_GENBC_AVX_SP} ${SLEEF_GENBC_SSE_SP} ${SLEEF_GENBC_AVX2_DP} ${SLEEF_GENBC_AVX_DP} ${SLEEF_GENBC_SSE_DP})

SET ( RV_LIB_SLEEF_OUT_DIR ${VECMATH_LIB_DIR} )

SET ( RV_LIB_EXTRA_DIR "${RV_SOURCE_DIR}/vecmath/extra")
SET ( EXTRA_BC_AVX_SP  "${VECMATH_LIB_DIR}/avx_extra_sp.bc" )
SET ( EXTRA_BC_SSE_SP  "${VECMATH_LIB_DIR}/sse_extra_sp.bc" )
SET ( EXTRA_BC_AVX_DP  "${VECMATH_LIB_DIR}/avx_extra_dp.bc" )
SET ( EXTRA_BC_SSE_DP  "${VECMATH_LIB_DIR}/sse_extra_dp.bc" )

SET ( GENCPP_TOOL "${RV_SOURCE_DIR}/tools/gen_cpp.py" )
OPTION(RV_ENABLE_SLEEF "Generate RV's builtin vector-math library" ON)
IF (${RV_ENABLE_SLEEF})
  SET(LLVM_TOOL_CLANG ${RV_BINARY_DIR}/../../bin/clang)
  SET(LLVM_TOOL_LINK ${RV_BINARY_DIR}/../../bin/llvm-link)
ENDIF()

# build SLEEF & others bc libs
IF (${RV_ENABLE_SLEEF})
  # add SLEEF bc generators as extra source
  MESSAGE("-- SLEEF sources found: ${SLEEF_DIR}")
  ADD_CUSTOM_TARGET ( libsleef_x64 DEPENDS ${RV_GENBC_SOURCES} )

  SET( RVLIB_BUILD_OPTS -O3 )
  # Sleef single precision
  ADD_CUSTOM_COMMAND (
    OUTPUT ${SLEEF_GENBC_AVX2_SP}
    COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR}
    COMMAND ${LLVM_TOOL_CLANG} ${RV_LIB_SLEEF_DIR}/sleefsimdsp.c -emit-llvm -c -Wall -Wno-unused-function ${RVLIB_BUILD_OPTS} -march=haswell -mavx2 -mfma -DENABLE_AVX2=ON -o ${SLEEF_BC_AVX2_SP}
    COMMAND ${GENCPP_TOOL} ${SLEEF_GENBC_AVX2_SP} \"avx2_sp\" ${SLEEF_BC_AVX2_SP}
    DEPENDS clang
  )
  ADD_CUSTOM_COMMAND (
    OUTPUT ${SLEEF_GENBC_AVX_SP}
    COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR}
    COMMAND ${LLVM_TOOL_CLANG} ${RV_LIB_SLEEF_DIR}/sleefsimdsp.c -emit-llvm -c -Wall -Wno-unused-function ${RVLIB_BUILD_OPTS} -mavx -DENABLE_AVX=ON -o ${SLEEF_BC_AVX_SP}.tmp
    COMMAND ${LLVM_TOOL_CLANG} ${RV_LIB_EXTRA_DIR}/extrasp.c -emit-llvm -c -Wall -Wno-unused-function ${RVLIB_BUILD_OPTS} -mavx -DENABLE_AVX=ON -o ${EXTRA_BC_AVX_SP}
    COMMAND ${LLVM_TOOL_LINK} ${EXTRA_BC_AVX_SP} ${SLEEF_BC_AVX_SP}.tmp -o ${SLEEF_BC_AVX_SP}
    COMMAND ${GENCPP_TOOL} ${SLEEF_GENBC_AVX_SP} \"avx_sp\" ${SLEEF_BC_AVX_SP}
    DEPENDS clang
  )
  ADD_CUSTOM_COMMAND (
    OUTPUT ${SLEEF_GENBC_SSE_SP}
    COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR}
    COMMAND ${LLVM_TOOL_CLANG} ${RV_LIB_SLEEF_DIR}/sleefsimdsp.c -emit-llvm -c -Wall -Wno-unused-function ${RVLIB_BUILD_OPTS} -m64 -msse4 -DENABLE_SSE2=ON -o ${SLEEF_BC_SSE_SP}.tmp
    COMMAND ${LLVM_TOOL_CLANG} ${RV_LIB_EXTRA_DIR}/extrasp.c -emit-llvm -c -Wall -Wno-unused-function ${RVLIB_BUILD_OPTS} -m64 -msse4 -DENABLE_SSE2=ON -o ${EXTRA_BC_SSE_SP}
    COMMAND ${LLVM_TOOL_LINK} ${EXTRA_BC_SSE_SP} ${SLEEF_BC_SSE_SP}.tmp -o ${SLEEF_BC_SSE_SP}
    COMMAND ${GENCPP_TOOL} ${SLEEF_GENBC_SSE_SP} \"sse_sp\" ${SLEEF_BC_SSE_SP}
    DEPENDS clang
  )
  # Sleef double precision
  ADD_CUSTOM_COMMAND (
    OUTPUT ${SLEEF_GENBC_AVX2_DP}
    COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR}
    COMMAND ${LLVM_TOOL_CLANG} ${RV_LIB_SLEEF_DIR}/sleefsimddp.c -emit-llvm -c -Wall -Wno-unused-function ${RVLIB_BUILD_OPTS} -march=haswell -mavx2 -mfma -DENABLE_AVX2=ON -o ${SLEEF_BC_AVX2_DP}
    COMMAND ${GENCPP_TOOL} ${SLEEF_GENBC_AVX2_DP} \"avx2_dp\" ${SLEEF_BC_AVX2_DP}
    DEPENDS clang
  )
  ADD_CUSTOM_COMMAND (
    OUTPUT ${SLEEF_GENBC_AVX_DP}
    COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR}
    COMMAND ${LLVM_TOOL_CLANG} ${RV_LIB_SLEEF_DIR}/sleefsimddp.c -emit-llvm -c -Wall -Wno-unused-function ${RVLIB_BUILD_OPTS} -mavx -DENABLE_AVX=ON -o ${SLEEF_BC_AVX_DP}.tmp
    COMMAND ${LLVM_TOOL_CLANG} ${RV_LIB_EXTRA_DIR}/extradp.c -emit-llvm -c -Wall -Wno-unused-function ${RVLIB_BUILD_OPTS} -mavx -DENABLE_AVX=ON -o ${EXTRA_BC_AVX_DP}
    COMMAND ${LLVM_TOOL_LINK} ${EXTRA_BC_AVX_DP} ${SLEEF_BC_AVX_DP}.tmp -o ${SLEEF_BC_AVX_DP}
    COMMAND ${GENCPP_TOOL} ${SLEEF_GENBC_AVX_DP} \"avx_dp\" ${SLEEF_BC_AVX_DP}
    DEPENDS clang
  )
  ADD_CUSTOM_COMMAND (
    OUTPUT ${SLEEF_GENBC_SSE_DP}
    COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR}
    COMMAND ${LLVM_TOOL_CLANG} ${RV_LIB_SLEEF_DIR}/sleefsimddp.c -emit-llvm -c -Wall -Wno-unused-function ${RVLIB_BUILD_OPTS} -m64 -msse4 -DENABLE_SSE2=ON -o ${SLEEF_BC_SSE_DP}.tmp
    COMMAND ${LLVM_TOOL_CLANG} ${RV_LIB_EXTRA_DIR}/extradp.c -emit-llvm -c -Wall -Wno-unused-function ${RVLIB_BUILD_OPTS} -m64 -msse4 -DENABLE_SSE2=ON -o ${EXTRA_BC_SSE_DP}
    COMMAND ${LLVM_TOOL_LINK} ${EXTRA_BC_SSE_DP} ${SLEEF_BC_SSE_DP}.tmp -o ${SLEEF_BC_SSE_DP}
    COMMAND ${GENCPP_TOOL} ${SLEEF_GENBC_SSE_DP} \"sse_dp\" ${SLEEF_BC_SSE_DP}
    DEPENDS clang
  )

  # generate static library
  ADD_LIBRARY(gensleef STATIC ${RV_GENBC_SOURCES})
ELSE()
  Message("-- Buildling without SLEEF.")
ENDIF ()

