# Check if this is a in tree build.
if (NOT DEFINED LLVM_MAIN_SRC_DIR)
  project(RV)
  cmake_minimum_required(VERSION 3.4.3)

  # Where is LLVM installed?
  set(LLVM_INSTALL_ROOT "" CACHE PATH "Root of LLVM install.")
  # Check if the LLVM_INSTALL_ROOT valid.
  if( NOT EXISTS ${LLVM_INSTALL_ROOT}/include/llvm )
    message(FATAL_ERROR "LLVM_INSTALL_ROOT (${LLVM_INSTALL_ROOT}) is not a valid LLVM installation.")
  endif(NOT EXISTS ${LLVM_INSTALL_ROOT}/include/llvm)
  # FileCheck, not and llvm-lit are not install by default, warn the user to copy them.
  if( NOT EXISTS ${LLVM_INSTALL_ROOT}/bin/FileCheck
      OR NOT EXISTS ${LLVM_INSTALL_ROOT}/bin/not
      OR NOT EXISTS ${LLVM_INSTALL_ROOT}/bin/llvm-lit )
    message(WARNING "'FileCheck', 'not' and 'llvm-lit' are required by running regress tests, "
                    "but they are not installed! Please copy them to "
                    "${LLVM_INSTALL_ROOT}/bin.")
  endif()
  # Add the llvm header path.
  include_directories(${LLVM_INSTALL_ROOT}/include/)

  # Get the system librarys that will link into LLVM.
  execute_process(COMMAND "${LLVM_INSTALL_ROOT}/bin/llvm-config" --system-libs
                  OUTPUT_VARIABLE LLVM_SYSTEM_LIBS
                  OUTPUT_STRIP_TRAILING_WHITESPACE)
  message(STATUS "System libs required by LLVM: ${LLVM_SYSTEM_LIBS}")

  # Determine where LLVM stores its libraries.
  execute_process(COMMAND "${LLVM_INSTALL_ROOT}/bin/llvm-config" --libdir
                  OUTPUT_VARIABLE LLVM_LIBRARY_DIR
                  OUTPUT_STRIP_TRAILING_WHITESPACE)
  link_directories("${LLVM_LIBRARY_DIR}")

  # Now set the header paths.
  execute_process(COMMAND "${LLVM_INSTALL_ROOT}/bin/llvm-config" --includedir
                  OUTPUT_VARIABLE LLVM_INCLUDE_DIR
                  OUTPUT_STRIP_TRAILING_WHITESPACE)
  include_directories( ${LLVM_INCLUDE_DIR} )

  # Get the TARGET_TRIPLE
  execute_process(COMMAND "${LLVM_INSTALL_ROOT}/bin/llvm-config" --host-target
                  OUTPUT_VARIABLE TARGET_TRIPLE
                  OUTPUT_STRIP_TRAILING_WHITESPACE)

  # And then set the cxx flags.
  execute_process(COMMAND "${LLVM_INSTALL_ROOT}/bin/llvm-config" --cxxflags
                  OUTPUT_VARIABLE LLVM_CXX_FLAGS
                  OUTPUT_STRIP_TRAILING_WHITESPACE)
  set(CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS} ${LLVM_CXX_FLAGS})

  # Check LLVM_ENABLE_ASSERTIONS
  execute_process(COMMAND "${LLVM_INSTALL_ROOT}/bin/llvm-config" --assertion-mode
                  OUTPUT_VARIABLE LLVM_ENABLE_ASSERTIONS
                  OUTPUT_STRIP_TRAILING_WHITESPACE)
  # Copied from LLVM's HandleLLVMOptions.cmake
  if( LLVM_ENABLE_ASSERTIONS )
    # MSVC doesn't like _DEBUG on release builds. See PR 4379.
    if( NOT MSVC )
      add_definitions( -D_DEBUG )
    endif()
    # On non-Debug builds cmake automatically defines NDEBUG, so we
    # explicitly undefine it:
    if( NOT uppercase_CMAKE_BUILD_TYPE STREQUAL "DEBUG" )
      add_definitions( -UNDEBUG )
      # Also remove /D NDEBUG to avoid MSVC warnings about conflicting defines.
      foreach (flags_var_to_scrub
          CMAKE_CXX_FLAGS_RELEASE
          CMAKE_CXX_FLAGS_RELWITHDEBINFO
          CMAKE_CXX_FLAGS_MINSIZEREL
          CMAKE_C_FLAGS_RELEASE
          CMAKE_C_FLAGS_RELWITHDEBINFO
          CMAKE_C_FLAGS_MINSIZEREL)
        string (REGEX REPLACE "(^| )[/-]D *NDEBUG($| )" " "
          "${flags_var_to_scrub}" "${${flags_var_to_scrub}}")
      endforeach()
    endif()
  endif()

  # Sources available, too?
  execute_process(COMMAND "${LLVM_INSTALL_ROOT}/bin/llvm-config" --src-root
                  OUTPUT_VARIABLE MAIN_SRC_DIR
                  OUTPUT_STRIP_TRAILING_WHITESPACE)
  set(LLVM_SOURCE_ROOT ${MAIN_SRC_DIR} CACHE PATH "Path to LLVM source tree")

  # Make sure the isl c files are built as fPIC
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fPIC")
else ()
  set(LLVM_SOURCE_ROOT "${LLVM_MAIN_SRC_DIR}")
endif ()

# Options
OPTION(RV_ENABLE_SLEEF "Generate RV's builtin vector-math library" ON)
OPTION(RV_ENABLE_CRT "Implement complex math functions using BC-compiler compiler-rt" OFF)
OPTION(RV_DEBUG "Enable verbose debug output and expensive internal checks." OFF)


# check enabled targets
IF (X86 IN_LIST LLVM_TARGETS_TO_BUILD)
  SET(RV_ENABLE_X86 On)
ELSE()
  SET(RV_ENABLE_X86 Off)
ENDIF()

IF (AArch64 IN_LIST LLVM_TARGETS_TO_BUILD)
  SET(RV_ENABLE_ADVSIMD On)
ELSE()
  SET(RV_ENABLE_ADVSIMD Off)
ENDIF()


set(RV_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set(RV_BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR})
set(RV_INCLUDE_DIR ${RV_SOURCE_DIR}/include PARENT_SCOPE)

# Add path for custom modules
set(CMAKE_MODULE_PATH
  ${CMAKE_MODULE_PATH}
  "${CMAKE_CURRENT_SOURCE_DIR}/cmake"
  )

include("rv_macros")

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
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${RV_SOURCE_DIR}/cmake")

SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

include_directories(
  BEFORE
  ${CMAKE_CURRENT_SOURCE_DIR}/include
  ${ISL_INCLUDE_DIRS}
  ${CMAKE_CURRENT_SOURCE_DIR}/lib/External/JSON/include
  ${CMAKE_CURRENT_SOURCE_DIR}/lib/External/pet/include
  ${CMAKE_CURRENT_SOURCE_DIR}/lib/External
  ${CMAKE_CURRENT_BINARY_DIR}/include
  )

if (NOT LLVM_INSTALL_TOOLCHAIN_ONLY)
  install(DIRECTORY include/
    DESTINATION include
    FILES_MATCHING
    PATTERN "*.h"
    PATTERN ".svn" EXCLUDE
    )

  install(DIRECTORY ${RV_BINARY_DIR}/include/
    DESTINATION include
    FILES_MATCHING
    PATTERN "*.h"
    PATTERN "CMakeFiles" EXCLUDE
    PATTERN ".svn" EXCLUDE
    )
endif()

add_definitions( -D_GNU_SOURCE )

add_subdirectory(vecmath)

add_subdirectory(src)

add_subdirectory(tools)

# Add target to check formatting of polly files
file( GLOB_RECURSE files *.h src/*.cpp src/*.c tools/*.cpp tools/*.c tools/*.h)
