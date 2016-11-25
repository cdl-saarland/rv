SET ( RV_LIB_PATH "${CMAKE_BINARY_DIR}/rvlib.bc" CACHE FILEPATH "Wfv libary path" )
SET ( RV_LIB_DIR "${PROJ_ROOT_DIR}/rvlibsrc" )
SET ( RV_LIB_MATH_DIR "${RV_LIB_DIR}/mathfun" )

set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -D_DEBUG")

ADD_DEFINITIONS ( ${LLVM_DEFINITIONS} )

# setup additional lib definition
ADD_DEFINITIONS ( "-DRV_RVLIB_FILE=\"${RV_LIB_PATH}\"" )

# this defaults to no-rtti builds, if this flag is not available
IF (LLVM_ENABLE_RTTI)
	ADD_DEFINITIONS( "-frtti" )
ELSE ()
	ADD_DEFINITIONS( "-fno-rtti" )
ENDIF ()

ADD_DEFINITIONS ( "-std=c++11" )

# enable c++11
IF (NOT MSVC)
    SET(CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS} " -fPIC -fno-rtti" )
ELSE ()
    ADD_DEFINITIONS ( "-DRV_LIB" )
    ADD_DEFINITIONS ( "-DRV_STATIC_LIBS" )
    # suppress redundant warnings
    ADD_DEFINITIONS ( "-D_SCL_SECURE_NO_WARNINGS -D_CRT_SECURE_NO_WARNINGS" )
    ADD_DEFINITIONS ( "/wd4244 /wd4800")
ENDIF ()

SET ( RV_GLOBAL_INCLUDES "${PROJ_ROOT_DIR}/include" )
FILE ( GLOB RV_GLOBAL_INCLUDE_FILES "${RV_GLOBAL_INCLUDES}/rv/**/*.h" )

INCLUDE_DIRECTORIES ( ${PROJ_SOURCE_DIR} )
INCLUDE_DIRECTORIES ( ${RV_GLOBAL_INCLUDES} )
INCLUDE_DIRECTORIES ( ${LLVM_INCLUDE_DIRS} )

# get source files
FILE ( GLOB NATIVE_CPP          native/*.cpp )
FILE ( GLOB CPP_GLOBAL          *.cpp )
FILE ( GLOB CPP_UTILS           utils/*.cpp )
FILE ( GLOB STAGES              stages/*.cpp )
FILE ( GLOB CPP_ANALYSIS        analysis/*.cpp )
FILE ( GLOB RV                 rv/*.cpp )
FILE ( GLOB REGIONS             rv/Region/*.cpp )
FILE ( GLOB PDA                 pda/*.cpp )
FILE ( GLOB CPP_FUNCTIONPASSES  functionPasses/*.cpp )
FILE ( GLOB CPP_FUNCTIONPASSES2 functionPasses/functionVectorization/*.cpp )
FILE ( GLOB OPT_PASSES pass/*.cpp )

SET ( RV_SOURCE_FILES ${CPP_GLOBAL} ${CPP_UTILS} ${STAGES} ${CPP_ANALYSIS} ${CPP_FUNCTIONPASSES} ${CPP_FUNCTIONPASSES2} ${REGIONS} ${RV} ${PDA} ${NATIVE_CPP} ${OPT_PASSES} analysis/MetadataMaskAnalyzer.cpp)

# create libRV
INCLUDE ( rv-shared )
ADD_LIBRARY ( ${LIBRARY_NAME} SHARED ${RV_SOURCE_FILES} )

LINK_DIRECTORIES ( ${LLVM_LIBRARY_DIRS} )
get_rv_llvm_dependency_libs ( LLVM_LIBRARIES )

# link in shared LLVM libraries
IF (LLVM_SHARED_LIBS)
  TARGET_LINK_LIBRARIES ( ${LIBRARY_NAME} ${LLVM_LIBRARIES} )
ENDIF ()

# build additional lib
IF ( VECTOR_ISA  STREQUAL "avx")
    ADD_CUSTOM_COMMAND (
        OUTPUT ${RV_LIB_PATH}
        COMMAND ${LLVM_TOOL_LLVMAS} ${RV_LIB_DIR}/generated.ll -o ${CMAKE_BINARY_DIR}/generated.bc
        COMMAND ${LLVM_TOOL_CLANG} ${RV_LIB_MATH_DIR}/avx_mathfun.c -emit-llvm -c -Wall -mavx2 -o ${CMAKE_BINARY_DIR}/avx_mathfun.bc
        COMMAND ${LLVM_TOOL_CLANG} ${RV_LIB_MATH_DIR}/sse_mathfun.c -emit-llvm -c -Wall -msse4.2 -o ${CMAKE_BINARY_DIR}/sse_mathfun.bc
        COMMAND ${LLVM_TOOL_LINK} ${CMAKE_BINARY_DIR}/generated.bc
                ${CMAKE_BINARY_DIR}/avx_mathfun.bc
                ${CMAKE_BINARY_DIR}/sse_mathfun.bc
                -o ${CMAKE_BINARY_DIR}/rvlib_unopt.bc
        COMMAND ${LLVM_TOOL_OPT} ${CMAKE_BINARY_DIR}/rvlib_unopt.bc -O3 -o ${RV_LIB_PATH}
    )
ENDIF ()
IF ( VECTOR_ISA STREQUAL "neon")
    ADD_CUSTOM_COMMAND (
        OUTPUT ${RV_LIB_PATH}
        COMMAND ${LLVM_TOOL_LLVMAS} ${RV_LIB_DIR}/generated.ll -o ${CMAKE_BINARY_DIR}/generated.bc
        COMMAND ${LLVM_TOOL_CLANG} ${RV_LIB_MATH_DIR}/neon_mathfun.c -emit-llvm -c -Wall -o ${CMAKE_BINARY_DIR}/neon_mathfun.bc
        COMMAND ${LLVM_TOOL_LINK} ${CMAKE_BINARY_DIR}/generated.bc
                ${CMAKE_BINARY_DIR}/neon_mathfun.bc
                -o ${CMAKE_BINARY_DIR}/rvlib_unopt.bc
        COMMAND ${LLVM_TOOL_OPT} ${CMAKE_BINARY_DIR}/rvlib_unopt.bc -O3 -o ${RV_LIB_PATH}
    )
ENDIF ()

ADD_CUSTOM_TARGET ( RVLIB DEPENDS ${RV_LIB_PATH} )
ADD_DEPENDENCIES ( ${LIBRARY_NAME} RVLIB )

# make install
SET ( CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE )

# public headers
FILE ( GLOB RV_INCLUDES_RV "${RV_GLOBAL_INCLUDES}/rv/*.h" )
INSTALL ( FILES ${RV_INCLUDES_RV} DESTINATION ${PROJ_INCLUDE_DIR}/rv )
FILE ( GLOB RV_INCLUDES_RV_PDA "${RV_GLOBAL_INCLUDES}/rv/pda/*.h" )
INSTALL ( FILES ${RV_INCLUDES_RV_PDA} DESTINATION ${PROJ_INCLUDE_DIR}/rv/pda )
FILE ( GLOB RV_INCLUDES_RV_ANALYSIS "${RV_GLOBAL_INCLUDES}/rv/analysis/*.h" )
INSTALL ( FILES ${RV_INCLUDES_RV_ANALYSIS} DESTINATION ${PROJ_INCLUDE_DIR}/rv/analysis )
FILE ( GLOB RV_INCLUDES_RV_REGION "${RV_GLOBAL_INCLUDES}/rv/Region/*.h" )
INSTALL ( FILES ${RV_INCLUDES_RV_REGION} DESTINATION ${PROJ_INCLUDE_DIR}/rv/Region )
FILE ( GLOB RV_INCLUDES_RV_TRANSFORMS "${RV_GLOBAL_INCLUDES}/rv/transforms/*.h" )
INSTALL ( FILES ${RV_INCLUDES_RV_TRANSFORMS} DESTINATION ${PROJ_INCLUDE_DIR}/rv/transforms )
FILE ( GLOB RV_INCLUDES_RV_UTILS "${RV_GLOBAL_INCLUDES}/rv/utils/*.h" )
INSTALL ( FILES ${RV_INCLUDES_RV_UTILS} DESTINATION ${PROJ_INCLUDE_DIR}/rv/utils )
INSTALL ( FILES ${RV_LIB_PATH} DESTINATION ${PROJ_LIBRARY_DIR} )

# libRV
INSTALL ( TARGETS ${LIBRARY_NAME} LIBRARY DESTINATION lib)

# make rvTestSuite
SET ( RV_TEST_PATH "${PROJ_ROOT_DIR}/test" )
SET ( RV_TESTSUITE_PATH "${RV_TEST_PATH}/wfv1testsuite" )

SET ( RV_TESTSUITE_NAME "wfvTestSuite" )
SET ( TESTSUITE_SOURCE_FILES ${RV_TESTSUITE_PATH}/wfvTestSuite.cpp )

IF ( APPLE )
    SET ( STD_LIB "-std=c++11" "-stdlib=libstdc++" )
ENDIF ()

INCLUDE_DIRECTORIES ( ${RV_TEST_PATH}/include )
ADD_EXECUTABLE ( ${RV_TESTSUITE_NAME} ${TESTSUITE_SOURCE_FILES} )
TARGET_LINK_LIBRARIES ( ${RV_TESTSUITE_NAME} ${LLVM_LIBRARIES} ${LIBRARY_NAME} )

IF( VECTOR_ISA  STREQUAL "avx")
    SET ( TEST_VEC_FLAG "-msse4.2" )
    # AVX-only test cases
    SET ( TEST_SUITE_TESTS1 "${RV_TESTSUITE_PATH}/wfvTests.bc" )
    ADD_CUSTOM_COMMAND (
        OUTPUT ${TEST_SUITE_TESTS1}
        COMMAND ${LLVM_TOOL_CLANGPP} ${STD_LIB} ${RV_TESTSUITE_PATH}/wfvTests.cpp  -c -emit-llvm -Wall ${TEST_VEC_FLAG} -o ${RV_TESTSUITE_PATH}/wfvTests.bc
    )
    ADD_CUSTOM_TARGET ( wfvTest1 DEPENDS ${TEST_SUITE_TESTS1} )
    ADD_DEPENDENCIES ( ${RV_TESTSUITE_NAME} wfvTest1 )
ENDIF ()

#TODO: clion workaround for install target. remove before release
add_custom_target(install_${PROJECT_NAME}
        make install
        COMMENT "Installing ${PROJECT_NAME}")
