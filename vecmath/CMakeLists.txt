SET ( VECMATH_LIB_DIR ${CMAKE_BINARY_DIR}/lib )
# Sleef lib
SET ( SLEEF_DIR "${PROJ_ROOT_DIR}/vecmath/sleef" CACHE STRING "Path to libsleef sources" FORCE )
SET ( RV_LIB_SLEEF_DIR "${SLEEF_DIR}/simd" )

# SLEEF bc build paths
SET ( SLEEF_BC_AVX2_SP "${VECMATH_LIB_DIR}/avx2_sleef_sp.bc" )
SET ( SLEEF_BC_AVX_SP "${VECMATH_LIB_DIR}/avx_sleef_sp.bc" )
SET ( SLEEF_BC_SSE_SP "${VECMATH_LIB_DIR}/sse_sleef_sp.bc" )

SET ( SLEEF_BC_AVX2_DP "${VECMATH_LIB_DIR}/avx2_sleef_dp.bc" )
SET ( SLEEF_BC_AVX_DP "${VECMATH_LIB_DIR}/avx_sleef_dp.bc" )
SET ( SLEEF_BC_SSE_DP "${VECMATH_LIB_DIR}/sse_sleef_dp.bc" )

SET ( SLEEF_BC_LIBS ${SLEEF_BC_AVX2_SP} ${SLEEF_BC_AVX_SP} ${SLEEF_BC_SSE_SP} ${SLEEF_BC_AVX2_DP} ${SLEEF_BC_AVX_DP} ${SLEEF_BC_SSE_DP} )

SET( RV_LIB_SLEEF_OUT_DIR "${CMAKE_BINARY_DIR}/lib" )

# build SLEEF bc libs
IF (ENABLE_SLEEF AND IS_DIRECTORY ${RV_LIB_SLEEF_DIR})
  MESSAGE("-- SLEEF sources found: ${SLEEF_DIR}")
  ADD_CUSTOM_TARGET ( libsleef_x64 DEPENDS ${SLEEF_BC_AVX2_SP} ${SLEEF_BC_AVX_SP} ${SLEEF_BC_SSE_SP} ${SLEEF_BC_AVX2_DP} ${SLEEF_BC_AVX_DP} ${SLEEF_BC_SSE_DP} )

  SET( RVLIB_BUILD_OPTS -O3 )
  ADD_CUSTOM_COMMAND (
          OUTPUT ${SLEEF_BC_AVX2_SP}
          COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR} && ${LLVM_TOOL_CLANG} ${RV_LIB_SLEEF_DIR}/sleefsimdsp.c -emit-llvm -c -Wall -Wno-unused-function ${RVLIB_BUILD_OPTS} -march=haswell -mavx2 -mfma -DENABLE_AVX2=ON -o ${SLEEF_BC_AVX2_SP}
  )
  ADD_CUSTOM_COMMAND (
          OUTPUT ${SLEEF_BC_AVX_SP}
          COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR} && ${LLVM_TOOL_CLANG} ${RV_LIB_SLEEF_DIR}/sleefsimdsp.c -emit-llvm -c -Wall -Wno-unused-function ${RVLIB_BUILD_OPTS} -mavx -DENABLE_AVX=ON -o ${SLEEF_BC_AVX_SP}
  )
  ADD_CUSTOM_COMMAND (
          OUTPUT ${SLEEF_BC_SSE_SP}
          COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR} && ${LLVM_TOOL_CLANG} ${RV_LIB_SLEEF_DIR}/sleefsimdsp.c -emit-llvm -c -Wall -Wno-unused-function ${RVLIB_BUILD_OPTS} -m64 -msse4 -DENABLE_SSE2=ON -o ${SLEEF_BC_SSE_SP}
  )
  ADD_CUSTOM_COMMAND (
          OUTPUT ${SLEEF_BC_AVX2_DP}
          COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR} && ${LLVM_TOOL_CLANG} ${RV_LIB_SLEEF_DIR}/sleefsimddp.c -emit-llvm -c -Wall -Wno-unused-function ${RVLIB_BUILD_OPTS} -march=haswell -mavx2 -mfma -DENABLE_AVX2=ON -o ${SLEEF_BC_AVX2_DP}
  )
  ADD_CUSTOM_COMMAND (
          OUTPUT ${SLEEF_BC_AVX_DP}
          COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR} && ${LLVM_TOOL_CLANG} ${RV_LIB_SLEEF_DIR}/sleefsimddp.c -emit-llvm -c -Wall -Wno-unused-function ${RVLIB_BUILD_OPTS} -mavx -DENABLE_AVX=ON -o ${SLEEF_BC_AVX_DP}
  )
  ADD_CUSTOM_COMMAND (
          OUTPUT ${SLEEF_BC_SSE_DP}
          COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR} && ${LLVM_TOOL_CLANG} ${RV_LIB_SLEEF_DIR}/sleefsimddp.c -emit-llvm -c -Wall -Wno-unused-function ${RVLIB_BUILD_OPTS} -m64 -msse4 -DENABLE_SSE2=ON -o ${SLEEF_BC_SSE_DP}
  )
  ADD_DEPENDENCIES ( ${LIBRARY_NAME} libsleef_x64 )

  INSTALL ( FILES ${SLEEF_BC_LIBS} DESTINATION lib COMPONENT sleef )
ELSE()
  Message("-- Buildling without SLEEF.")
ENDIF ()

