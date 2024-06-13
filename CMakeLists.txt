#set(RV_LIBRARY_NAME RV)
#set(RV_COMPONENT_BUILD Off)

project(RV)
cmake_minimum_required(VERSION 3.4.3)

find_package(LLVM REQUIRED)
if (LLVM_LINK_LLVM_DYLIB)
    set(RV_LLVM_LINK_SHARED "USE_SHARED")
else()
    if (BUILD_SHARED_LIBS)
        message(SEND_ERROR "Cannot build RV as a shared library with the current build of LLVM. Build LLVM with LLVM_LINK_LLVM_DYLIB or change BUILD_SHARED_LIBS to off.")
    endif()
endif()

# Where is LLVM installed?
set(LLVM_INSTALL_ROOT ${LLVM_INSTALL_PREFIX} CACHE PATH "Root of LLVM install.")

# Add the llvm header path.
#include_directories(${LLVM_INSTALL_ROOT}/include/)

# Get the system librarys that will link into LLVM.
#execute_process(COMMAND "${LLVM_INSTALL_ROOT}/bin/llvm-config" --system-libs
#              OUTPUT_VARIABLE LLVM_SYSTEM_LIBS
#              OUTPUT_STRIP_TRAILING_WHITESPACE)
#message(STATUS "System libs required by LLVM: ${LLVM_SYSTEM_LIBS}")

# Determine where LLVM stores its libraries.
#execute_process(COMMAND "${LLVM_INSTALL_ROOT}/bin/llvm-config" --libdir
#              OUTPUT_VARIABLE LLVM_LIBRARY_DIR
#              OUTPUT_STRIP_TRAILING_WHITESPACE)
#link_directories("${LLVM_LIBRARY_DIR}")

# Now set the header paths.
#execute_process(COMMAND "${LLVM_INSTALL_ROOT}/bin/llvm-config" --includedir
#              OUTPUT_VARIABLE LLVM_INCLUDE_DIR
#              OUTPUT_STRIP_TRAILING_WHITESPACE)
#include_directories( ${LLVM_INCLUDE_DIR} )
#
## Get the TARGET_TRIPLE
#execute_process(COMMAND "${LLVM_INSTALL_ROOT}/bin/llvm-config" --host-target
#              OUTPUT_VARIABLE TARGET_TRIPLE
#              OUTPUT_STRIP_TRAILING_WHITESPACE)
#
## And then set the cxx flags.
#execute_process(COMMAND "${LLVM_INSTALL_ROOT}/bin/llvm-config" --cxxflags
#              OUTPUT_VARIABLE LLVM_CXX_FLAGS
#              OUTPUT_STRIP_TRAILING_WHITESPACE)
#set(CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS} ${LLVM_CXX_FLAGS})

# Check LLVM_ENABLE_ASSERTIONS
#execute_process(COMMAND "${LLVM_INSTALL_ROOT}/bin/llvm-config" --assertion-mode
#              OUTPUT_VARIABLE LLVM_ENABLE_ASSERTIONS
#              OUTPUT_STRIP_TRAILING_WHITESPACE)
## Copied from LLVM's HandleLLVMOptions.cmake
#if( LLVM_ENABLE_ASSERTIONS )
## MSVC doesn't like _DEBUG on release builds. See PR 4379.
#if( NOT MSVC )
#  add_definitions( -D_DEBUG )
#endif()
## On non-Debug builds cmake automatically defines NDEBUG, so we
## explicitly undefine it:
#if( NOT uppercase_CMAKE_BUILD_TYPE STREQUAL "DEBUG" )
#  add_definitions( -UNDEBUG )
#  # Also remove /D NDEBUG to avoid MSVC warnings about conflicting defines.
#  foreach (flags_var_to_scrub
#      CMAKE_CXX_FLAGS_RELEASE
#      CMAKE_CXX_FLAGS_RELWITHDEBINFO
#      CMAKE_CXX_FLAGS_MINSIZEREL
#      CMAKE_C_FLAGS_RELEASE
#      CMAKE_C_FLAGS_RELWITHDEBINFO
#      CMAKE_C_FLAGS_MINSIZEREL)
#    string (REGEX REPLACE "(^| )[/-]D *NDEBUG($| )" " "
#      "${flags_var_to_scrub}" "${${flags_var_to_scrub}}")
#  endforeach()
#endif()
#endif()

# Make sure the isl c files are built as fPIC
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fPIC")
message("rv: Building RV as an external LLVM tool")

# Options
OPTION(RV_REBUILD_GENBC "Build the builtin BC library (SLEEF, SIMD random, ..) from source (requires the SLEEF submodule). Otw, use the pre-built BC buffers from vecmath/prebuilt_genbc" OFF)
OPTION(RV_ENABLE_SLEEF "Generate RV's builtin vector-math library" ON)
OPTION(RV_ENABLE_CRT "Implement complex math functions using BC-compiler compiler-rt" OFF)
OPTION(RV_DEBUG "Enable verbose debug output and expensive internal checks." OFF)

if (RV_REBUILD_GENBC AND LLVM_RVPLUG_LINK_INTO_TOOLS)
  message(FATAL_ERROR "Cannot set RV_REBUILD_GENBC and LLVM_RVPLUG_LINK_INTO_TOOLS at the same time. If you want to update the vecmath bitcode files and buffers, (temporarily) disable LLVM_RVPLUG_LINK_INTO_TOOLS, compile LLVM and copy the BUILD/tools/rv/lib/*.gen.cpp files to rv/vecmath/prebuilt_genbc/.")
endif()

OPTION(RV_ENABLE_VP "(experimental) Use LLVM-VP (Vector Predication) (https://reviews.llvm.org/D57504)." OFF)

set(RV_TARGETS_TO_BUILD "Native" CACHE STRING "Semicolon-separated list of targets to build, or 'Native'.")
string(REPLACE "Native" ${LLVM_NATIVE_ARCH} RV_TARGETS_TO_BUILD "${RV_TARGETS_TO_BUILD}")
list(REMOVE_DUPLICATES RV_TARGETS_TO_BUILD)

# check enabled targets
if(X86 IN_LIST RV_TARGETS_TO_BUILD)
  set(RV_ENABLE_X86 On)
else()
  set(RV_ENABLE_X86 Off)
endif()

if(AArch64 IN_LIST RV_TARGETS_TO_BUILD)
  set(RV_ENABLE_ADVSIMD On)
else()
  set(RV_ENABLE_ADVSIMD Off)
endif()


set(RV_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set(RV_BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR})

# Add path for custom modules
#set(CMAKE_MODULE_PATH
#  ${CMAKE_MODULE_PATH}
#  "${CMAKE_CURRENT_SOURCE_DIR}/cmake"
#  )

  #include("rv_macros")

# Add appropriate flags for GCC
if (CMAKE_COMPILER_IS_GNUCXX)
  # FIXME: Turn off exceptions, RTTI:
  # -fno-exceptions -fno-rtti
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-common -Woverloaded-virtual -Wno-long-long -Wall -W -Wno-unused-parameter -Wwrite-strings -fno-exceptions -fno-rtti")
elseif (MSVC)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /EHs-c-")
  add_definitions("-D_HAS_EXCEPTIONS=0")
else ()
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-exceptions -fno-rtti")
endif ()

# Add path for custom modules
#set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${RV_SOURCE_DIR}/cmake")

#SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

#include_directories(
#  BEFORE
#  ${CMAKE_CURRENT_SOURCE_DIR}/include
#  ${CMAKE_CURRENT_SOURCE_DIR}/src
#  ${CMAKE_CURRENT_BINARY_DIR}/include
#  )

#if (NOT LLVM_INSTALL_TOOLCHAIN_ONLY)
#  install(DIRECTORY include/
#    DESTINATION include
#    FILES_MATCHING
#    PATTERN "*.h"
#    PATTERN ".svn" EXCLUDE
#    )
#
#  install(DIRECTORY ${RV_BINARY_DIR}/include/
#    DESTINATION include
#    FILES_MATCHING
#    PATTERN "*.h"
#    PATTERN "CMakeFiles" EXCLUDE
#    PATTERN ".svn" EXCLUDE
#    )
#endif()

add_definitions( -D_GNU_SOURCE )

# populates $RV_SLEEF_OBJECTS with sleef BC buffer files (if RV_ENABLE_SLEEF is set).
if(RV_ENABLE_SLEEF)
  message(STATUS "-- rv: Building with SLEEF")
  add_subdirectory(vecmath)
  set(RV_SLEEF_OBJECTS $<TARGET_OBJECTS:vecmath>)
else()
  message(STATUS "-- rv: Buildling without SLEEF.")
  set(RV_SLEEF_OBJECTS)
endif()

# links RV_SLEEF_OBJECTS into libRV
add_subdirectory(src)

export(TARGETS RV FILE ${CMAKE_BINARY_DIR}/share/anydsl/cmake/rv-exports.cmake)
configure_file(cmake/rv-config.cmake.in ${CMAKE_BINARY_DIR}/share/anydsl/cmake/rv-config.cmake @ONLY)

# Add target to check formatting of RV files
#file( GLOB_RECURSE files *.h src/*.cpp src/*.c tools/*.cpp tools/*.c tools/*.h)
