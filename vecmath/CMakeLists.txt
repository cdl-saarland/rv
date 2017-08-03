SET ( VECMATH_LIB_DIR ${RV_BINARY_DIR}/lib )
# Sleef lib
SET ( SLEEF_DIR "${RV_SOURCE_DIR}/vecmath/sleef" CACHE STRING "Path to libsleef sources" FORCE )
SET ( SLEEF_ARCH_DIR "${SLEEF_DIR}/src/arch" )
SET ( SLEEF_COMMON_DIR "${SLEEF_DIR}/src/common" )
SET ( RV_LIB_SLEEF_DIR "${SLEEF_DIR}/src/libm" )
SET ( RV_LIB_EXTRAS_DIR "${RV_SOURCE_DIR}/vecmath/extras" )

#SLEEF header build paths
SET ( SLEEF_R_AVX2 "${RV_LIB_SLEEF_DIR}/renameavx2.h" )
SET ( SLEEF_R_AVX "${RV_LIB_SLEEF_DIR}/renameavx.h" )
SET ( SLEEF_R_SSE2 "${RV_LIB_SLEEF_DIR}/renamesse2.h" )
SET ( SLEEF_H "${RV_LIB_SLEEF_DIR}/sleef.h" )

SET ( SLEEF_HEADERS ${SLEEF_R_AVX2} ${SLEEF_R_AVX} ${SLEEF_R_SSE2} ${SLEEF_H} )
SET ( SLEEF_MKRENAME "${RV_LIB_SLEEF_DIR}/mkrename" )

# compiler-rt
SET ( CRT_BC "${VECMATH_LIB_DIR}/crt.bc" )
SET ( CRT_GENBC "${VECMATH_LIB_DIR}/crt.gen.cpp" )
SET ( CRT_INC "${RV_SOURCE_DIR}/../../runtimes/compiler-rt/lib/builtins" )

# extra libs
SET ( EXTRAS_AVX_BC "${VECMATH_LIB_DIR}/avx_extras.bc" )
SET ( EXTRAS_AVX_GENBC "${VECMATH_LIB_DIR}/avx_extras.gen.cpp" )

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

SET ( SLEEF_BC_LIBS ${SLEEF_BC_AVX2_SP} ${SLEEF_BC_AVX_SP} ${SLEEF_BC_SSE_SP} ${SLEEF_BC_AVX2_DP} ${SLEEF_BC_AVX_DP} ${SLEEF_BC_SSE_DP} ${EXTRAS_AVX_BC} )


SET ( RV_GENBC_SOURCES ${SLEEF_GENBC_AVX2_SP} ${SLEEF_GENBC_AVX_SP} ${SLEEF_GENBC_SSE_SP} ${SLEEF_GENBC_AVX2_DP} ${SLEEF_GENBC_AVX_DP} ${SLEEF_GENBC_SSE_DP} ${EXTRAS_AVX_GENBC} )
IF (RV_ENABLE_CRT)
  SET ( RV_GENBC_SOURCES ${RV_GENBC_SOURCES} ${CRT_GENBC} )
ENDIF()

# conditionally provide extra sources
SET ( RV_LIB_SLEEF_OUT_DIR ${VECMATH_LIB_DIR} )

SET ( GENCPP_TOOL "${RV_SOURCE_DIR}/tools/gen_cpp.py" )

IF (${RV_ENABLE_SLEEF})
  SET(LLVM_TOOL_CLANG ${RV_BINARY_DIR}/../../bin/clang)
  SET(LLVM_TOOL_LINK ${RV_BINARY_DIR}/../../bin/llvm-link)
  # add SLEEF bc generators as extra source
  MESSAGE("-- rv: Building with SLEEF")

  # generate header files
  ADD_CUSTOM_COMMAND (
    OUTPUT ${SLEEF_HEADERS} ${SLEEF_MKRENAME}
    COMMAND cp ${RV_LIB_SLEEF_DIR}/sleeflibm.h.org ${RV_LIB_SLEEF_DIR}/sleef.h
    COMMAND ${LLVM_TOOL_CLANG} ${RV_LIB_SLEEF_DIR}/mkrename.c -O3 -Wall -Wno-unused -o ${RV_LIB_SLEEF_DIR}/mkrename
    COMMAND ${RV_LIB_SLEEF_DIR}/mkrename sse2 2 4 > ${RV_LIB_SLEEF_DIR}/renamesse2.h
    COMMAND ${RV_LIB_SLEEF_DIR}/mkrename avx 4 8 > ${RV_LIB_SLEEF_DIR}/renameavx.h
    COMMAND ${RV_LIB_SLEEF_DIR}/mkrename avx2 4 8 > ${RV_LIB_SLEEF_DIR}/renameavx2.h
    COMMAND ${RV_LIB_SLEEF_DIR}/mkrename sse2 2 4 __m128d __m128 __m128i __m128i __SSE2__ >> ${RV_LIB_SLEEF_DIR}/sleef.h
    COMMAND ${RV_LIB_SLEEF_DIR}/mkrename avx 4 8 __m256d __m256 __m128i 'struct { __m128i x, y; }' __AVX__ >> ${RV_LIB_SLEEF_DIR}/sleef.h
    COMMAND ${RV_LIB_SLEEF_DIR}/mkrename avx2 4 8 __m256d __m256 __m128i __m256i __AVX2__ >> ${RV_LIB_SLEEF_DIR}/sleef.h
    COMMAND echo '\#undef IMPORT' >> ${RV_LIB_SLEEF_DIR}/sleef.h
    COMMAND echo '\#endif' >> ${RV_LIB_SLEEF_DIR}/sleef.h
    DEPENDS clang
  )
  # add SLEEF bc generators as extra source
  ADD_CUSTOM_TARGET ( libsleef_x64 DEPENDS ${RV_GENBC_SOURCES} )

  SET( RVLIB_BUILD_OPTS -O3 )
  # extra libs
  ADD_CUSTOM_COMMAND (
    OUTPUT ${EXTRAS_AVX_GENBC}
    COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR}
    COMMAND ${LLVM_TOOL_CLANG} ${RV_LIB_EXTRAS_DIR}/vrand_avx.c -emit-llvm -c -Wall -Wno-unused ${RVLIB_BUILD_OPTS} -mavx2 -mfma -DENABLE_AVX2=ON -ffp-contract=off -o ${EXTRAS_AVX_BC}
    COMMAND ${GENCPP_TOOL} ${EXTRAS_AVX_GENBC} \"avx_extras\" ${EXTRAS_AVX_BC}
    DEPENDS ${RV_LIB_EXTRAS_DIR}/vrand_avx.c
  )

  # Sleef single precision
  ADD_CUSTOM_COMMAND (
    OUTPUT ${SLEEF_GENBC_AVX2_SP}
    COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR}
    COMMAND ${LLVM_TOOL_CLANG} ${RV_LIB_SLEEF_DIR}/sleefsimdsp.c -emit-llvm -c -Wall -Wno-unused ${RVLIB_BUILD_OPTS} -mavx2 -mfma -DENABLE_AVX2=ON -ffp-contract=off -o ${SLEEF_BC_AVX2_SP}
    COMMAND ${GENCPP_TOOL} ${SLEEF_GENBC_AVX2_SP} \"avx2_sp\" ${SLEEF_BC_AVX2_SP}
    DEPENDS ${SLEEF_HEADERS}
    DEPENDS ${RV_LIB_SLEEF_DIR}/sleefsimdsp.c
  )
  ADD_CUSTOM_COMMAND (
    OUTPUT ${SLEEF_GENBC_AVX_SP}
    COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR}
    COMMAND ${LLVM_TOOL_CLANG} ${RV_LIB_SLEEF_DIR}/sleefsimdsp.c -emit-llvm -c -Wall -Wno-unused ${RVLIB_BUILD_OPTS} -mavx -DENABLE_AVX=ON -ffp-contract=off -o ${SLEEF_BC_AVX_SP}#.tmp
    COMMAND ${GENCPP_TOOL} ${SLEEF_GENBC_AVX_SP} \"avx_sp\" ${SLEEF_BC_AVX_SP}
    DEPENDS ${SLEEF_HEADERS} ${RV_LIB_SLEEF_DIR}/sleefsimdsp.c
  )
  ADD_CUSTOM_COMMAND (
    OUTPUT ${SLEEF_GENBC_SSE_SP}
    COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR}
    COMMAND ${LLVM_TOOL_CLANG} ${RV_LIB_SLEEF_DIR}/sleefsimdsp.c -emit-llvm -c -Wall -Wno-unused ${RVLIB_BUILD_OPTS} -m64 -msse2 -msse4.1 -DENABLE_SSE2=ON -ffp-contract=off -o ${SLEEF_BC_SSE_SP}#.tmp
    COMMAND ${GENCPP_TOOL} ${SLEEF_GENBC_SSE_SP} \"sse_sp\" ${SLEEF_BC_SSE_SP}
    DEPENDS ${SLEEF_HEADERS} ${RV_LIB_SLEEF_DIR}/sleefsimdsp.c
  )
  # Sleef double precision
  ADD_CUSTOM_COMMAND (
    OUTPUT ${SLEEF_GENBC_AVX2_DP}
    COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR}
    COMMAND ${LLVM_TOOL_CLANG} ${RV_LIB_SLEEF_DIR}/sleefsimddp.c -emit-llvm -c -Wall -Wno-unused ${RVLIB_BUILD_OPTS} -mavx2 -mfma -DENABLE_AVX2=ON -ffp-contract=off -o ${SLEEF_BC_AVX2_DP}
    COMMAND ${GENCPP_TOOL} ${SLEEF_GENBC_AVX2_DP} \"avx2_dp\" ${SLEEF_BC_AVX2_DP}
    DEPENDS ${SLEEF_HEADERS} ${RV_LIB_SLEEF_DIR}/sleefsimddp.c

  )
  ADD_CUSTOM_COMMAND (
    OUTPUT ${SLEEF_GENBC_AVX_DP}
    COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR}
    COMMAND ${LLVM_TOOL_CLANG} ${RV_LIB_SLEEF_DIR}/sleefsimddp.c -emit-llvm -c -Wall -Wno-unused ${RVLIB_BUILD_OPTS} -DENABLE_AVX=ON -mavx -ffp-contract=off -o ${SLEEF_BC_AVX_DP}#.tmp
    COMMAND ${GENCPP_TOOL} ${SLEEF_GENBC_AVX_DP} \"avx_dp\" ${SLEEF_BC_AVX_DP}
    DEPENDS ${SLEEF_HEADERS} ${RV_LIB_SLEEF_DIR}/sleefsimddp.c
  )
  ADD_CUSTOM_COMMAND (
    OUTPUT ${SLEEF_GENBC_SSE_DP}
    COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR}
    COMMAND ${LLVM_TOOL_CLANG} ${RV_LIB_SLEEF_DIR}/sleefsimddp.c -emit-llvm -c -Wall -Wno-unused ${RVLIB_BUILD_OPTS} -m64 -DENABLE_SSE2=ON -msse2 -msse4.1 -ffp-contract=off -o ${SLEEF_BC_SSE_DP}#.tmp
    COMMAND ${GENCPP_TOOL} ${SLEEF_GENBC_SSE_DP} \"sse_dp\" ${SLEEF_BC_SSE_DP}
    DEPENDS ${SLEEF_HEADERS} ${RV_LIB_SLEEF_DIR}/sleefsimddp.c
  )
  IF (RV_ENABLE_CRT) 
    # compiler-rt runtime library
    ADD_CUSTOM_COMMAND (
      OUTPUT ${CRT_GENBC}
      COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR}
      COMMAND ${LLVM_TOOL_CLANG} ${RV_SOURCE_DIR}/vecmath/crt.c -I${CRT_INC} -m64 -emit-llvm -c -Wall -Wno-unused ${RVLIB_BUILD_OPTS} -ffp-contract=off -o ${CRT_BC}
      COMMAND ${GENCPP_TOOL} ${CRT_GENBC} \"crt\" ${CRT_BC}
      DEPENDS ${RV_SOURCE_DIR}/vecmath/crt.c
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
