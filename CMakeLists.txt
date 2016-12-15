PROJECT ( RV )

CMAKE_MINIMUM_REQUIRED ( VERSION 2.8.1 FATAL_ERROR )

### configure LLVM ###
FIND_PACKAGE(LLVM 3.8 REQUIRED)

# find out if this is a shared library build of LLVM
get_target_property(t LLVMPasses TYPE)
if("x${t}" STREQUAL "xSTATIC_LIBRARY")
  SET ( LLVM_SHARED_LIBS 0)
else()
  SET ( LLVM_SHARED_LIBS 1)
endif()


IF (LLVM_SHARED_LIBS) 
  MESSAGE("-- Found shared LLVM libraries." )
ELSE ()
  MESSAGE("-- Found static LLVM libraries.")
ENDIF()

SET ( LLVM_PLATFORM_TOOLS_PATH ${LLVM_TOOLS_BINARY_DIR})
# FIND_PATH ( LLVM_PLATFORM_TOOLS_PATH NAMES clang clang.exe PATHS ${LLVM_TOOLS_BINARY_DIR} ${LLVM_TOOLS_BINARY_DIR}/Debug ${LLVM_TOOLS_BINARY_DIR}/Release )
MESSAGE ( STATUS "LLVM Tools path found: ${LLVM_PLATFORM_TOOLS_PATH}" )
SET ( LLVM_TOOL_OPT "${LLVM_PLATFORM_TOOLS_PATH}/opt" )
SET ( LLVM_TOOL_LINK "${LLVM_PLATFORM_TOOLS_PATH}/llvm-link" )
SET ( LLVM_TOOL_LLVMAS "${LLVM_PLATFORM_TOOLS_PATH}/llvm-as" )
SET ( LLVM_TOOL_CLANG "${LLVM_PLATFORM_TOOLS_PATH}/clang" )
SET ( LLVM_TOOL_CLANGPP "${LLVM_PLATFORM_TOOLS_PATH}/clang++" )


### configure build ###
SET ( PACKAGE_VERSION "2.0" )
SET ( CMAKE_CONFIGURATION_TYPES "Debug;Release" CACHE STRING "limited config" FORCE )
SET ( VECTOR_ISA "avx" CACHE STRING "Select target vector ISA: avx, neon" )

IF ( NOT CMAKE_BUILD_TYPE )
    SET ( CMAKE_BUILD_TYPE Debug CACHE STRING "Debug or Release" FORCE )
ENDIF ()

SET ( PROJ_ROOT_DIR    ${CMAKE_CURRENT_SOURCE_DIR} )
SET ( PROJ_SOURCE_DIR  ${PROJ_ROOT_DIR}/src )

SET ( PROJ_BINARY_DIR  "bin" )
SET ( PROJ_LIBRARY_DIR "lib" )
SET ( PROJ_INCLUDE_DIR "include" )

SET ( PROJ_NAME  ${PROJECT_NAME} )
STRING ( TOUPPER ${PROJ_NAME} PROJ_PREFIX )

SET ( CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/${PROJ_BINARY_DIR} )
SET ( CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/${PROJ_LIBRARY_DIR} )
SET ( CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/${PROJ_LIBRARY_DIR} )

SET ( CMAKE_MODULE_PATH ${PROJ_ROOT_DIR} ${CMAKE_MODULE_PATH} )

# cmake module install
INSTALL ( FILES rv-config.cmake rv-shared.cmake DESTINATION share/rv/cmake )

# libRV
SET ( LIBRARY_NAME RV )
ADD_SUBDIRECTORY ( ${PROJ_SOURCE_DIR} )

# rvTool
SET ( RVTOOL_SOURCE_DIR  ${PROJ_ROOT_DIR}/tools )
ADD_SUBDIRECTORY( ${RVTOOL_SOURCE_DIR} )

# Sleef lib
SET ( SLEEF_DIR "${PROJ_ROOT_DIR}/sleef" CACHE STRING "Path to libsleef sources" FORCE )
SET ( RV_LIB_SLEEF_DIR "${SLEEF_DIR}/simd" )
SET ( RV_LIB_SLEEF_OUT_DIR "${PROJ_ROOT_DIR}/sleefsrc" )

SET ( SLEEF_BC_AVX2_SP "${PROJ_ROOT_DIR}/sleefsrc/avx2_sleef_sp.bc" )
SET ( SLEEF_BC_AVX_SP "${PROJ_ROOT_DIR}/sleefsrc/avx_sleef_sp.bc" )
SET ( SLEEF_BC_SSE_SP "${PROJ_ROOT_DIR}/sleefsrc/sse_sleef_sp.bc" )

SET ( SLEEF_BC_AVX2_DP "${PROJ_ROOT_DIR}/sleefsrc/avx2_sleef_dp.bc" )
SET ( SLEEF_BC_AVX_DP "${PROJ_ROOT_DIR}/sleefsrc/avx_sleef_dp.bc" )
SET ( SLEEF_BC_SSE_DP "${PROJ_ROOT_DIR}/sleefsrc/sse_sleef_dp.bc" )

SET ( RV_LIB_SLEEF_OUT_DIR "${PROJ_ROOT_DIR}/sleefsrc" )

IF (IS_DIRECTORY ${RV_LIB_SLEEF_DIR})
  MESSAGE("-- SLEEF sources found: ${SLEEF_DIR}")
  ADD_CUSTOM_TARGET ( libsleef_x64 DEPENDS ${SLEEF_BC_AVX2_SP} ${SLEEF_BC_AVX_SP} ${SLEEF_BC_SSE_SP} ${SLEEF_BC_AVX2_DP} ${SLEEF_BC_AVX_DP} ${SLEEF_BC_SSE_DP} )

  SET( RVLIB_BUILD_OPTS -O3 )
  ADD_CUSTOM_COMMAND (
          OUTPUT ${SLEEF_BC_AVX2_SP}
          COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR} && ${LLVM_TOOL_CLANG} ${RV_LIB_SLEEF_DIR}/sleefsimdsp.c -emit-llvm -c -Wall ${RVLIB_BUILD_OPTS} -march=haswell -mavx2 -mfma -DENABLE_AVX2=ON -o ${SLEEF_BC_AVX2_SP}
  )
  ADD_CUSTOM_COMMAND (
          OUTPUT ${SLEEF_BC_AVX_SP}
          COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR} && ${LLVM_TOOL_CLANG} ${RV_LIB_SLEEF_DIR}/sleefsimdsp.c -emit-llvm -c -Wall ${RVLIB_BUILD_OPTS} -mavx -DENABLE_AVX=ON -o ${SLEEF_BC_AVX_SP}
  )
  ADD_CUSTOM_COMMAND (
          OUTPUT ${SLEEF_BC_SSE_SP}
          COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR} && ${LLVM_TOOL_CLANG} ${RV_LIB_SLEEF_DIR}/sleefsimdsp.c -emit-llvm -c -Wall ${RVLIB_BUILD_OPTS} -m64 -msse4 -DENABLE_SSE2=ON -o ${SLEEF_BC_SSE_SP}
  )
  ADD_CUSTOM_COMMAND (
          OUTPUT ${SLEEF_BC_AVX2_DP}
          COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR} && ${LLVM_TOOL_CLANG} ${RV_LIB_SLEEF_DIR}/sleefsimddp.c -emit-llvm -c -Wall ${RVLIB_BUILD_OPTS} -march=haswell -mavx2 -mfma -DENABLE_AVX2=ON -o ${SLEEF_BC_AVX2_DP}
  )
  ADD_CUSTOM_COMMAND (
          OUTPUT ${SLEEF_BC_AVX_DP}
          COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR} && ${LLVM_TOOL_CLANG} ${RV_LIB_SLEEF_DIR}/sleefsimddp.c -emit-llvm -c -Wall ${RVLIB_BUILD_OPTS} -mavx -DENABLE_AVX=ON -o ${SLEEF_BC_AVX_DP}
  )
  ADD_CUSTOM_COMMAND (
          OUTPUT ${SLEEF_BC_SSE_DP}
          COMMAND mkdir -p ${RV_LIB_SLEEF_OUT_DIR} && ${LLVM_TOOL_CLANG} ${RV_LIB_SLEEF_DIR}/sleefsimddp.c -emit-llvm -c -Wall ${RVLIB_BUILD_OPTS} -m64 -msse4 -DENABLE_SSE2=ON -o ${SLEEF_BC_SSE_DP}
  )
  ADD_DEPENDENCIES ( ${LIBRARY_NAME} libsleef_x64 )
ELSE()
  Message(FATAL_ERROR "Did not find sleef sources")
ENDIF ()


