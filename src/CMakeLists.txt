set(LLVM_NO_RTTI 1)

set(RV_HEADER_FILES)
if (MSVC_IDE OR XCODE)
  file(GLOB_RECURSE Rv_HEADER_FILES "${RV_SOURCE_DIR}/include/rv/**/*.h")
endif ()

if (RV_ENABLE_SLEEF) 
  add_definitions( "-DRV_ENABLE_BUITINS" )
endif()

add_rv_library(RV
  analysis/MandatoryAnalysis.cpp
  analysis/BranchDependenceAnalysis.cpp
  analysis/maskAnalysis.cpp
  analysis/reductionAnalysis.cpp
  analysis/DFG.cpp
  analysis/ABAAnalysis.cpp
  analysis/VectorizationAnalysis.cpp
  native/ShuffleBuilder.cpp
  native/Utils.cpp
  native/MemoryAccessGrouper.cpp
  native/NatBuilder.cpp
  native/nativeBackendPass.cpp
  rvConfig.cpp
  transform/loopExitCanonicalizer.cpp
  transform/maskGenerator.cpp
  transform/LoopVectorizer.cpp
  transform/Linearizer.cpp
  transform/srovTransform.cpp
  transform/structOpt.cpp
  utils/maskGraphUtils.cpp
  utils/rvTools.cpp
  VectorizationInfoProxyPass.cpp
  region/LoopRegion.cpp
  region/Region.cpp
  vectorMapping.cpp
  sleefLibrary.cpp
  PlatformInfo.cpp
  init.cpp
  vectorizationInfo.cpp
  report.cpp
  rv.cpp
  vectorShape.cpp
  ${RV_HEADER_FILES}
  ${RV_GENBC_SOURCES}
  )

# if (RV_ENABLE_SLEEF)
#   target_link_libraries(RV ${RV_GENBC_SOURCES})
# endif()

if (BUILD_SHARED_LIBS)
  target_link_libraries(RV
    LLVMSupport
    LLVMCore
    LLVMScalarOpts
    LLVMInstCombine
    LLVMTransformUtils
    LLVMAnalysis
    LLVMipo
    LLVMMC
    LLVMIRReader
# The libraries below are required for darwin: http://PR26392
    LLVMBitReader
    LLVMMCParser
    LLVMObject
    LLVMProfileData
    LLVMTarget
    LLVMVectorize
  )
  link_directories(
    ${LLVM_LIBRARY_DIR}
  )
elseif (LLVM_LINK_LLVM_DYLIB)
  target_link_libraries(RV
    LLVM
  )
  link_directories(
    ${LLVM_LIBRARY_DIR}
  )
endif()

# Build a monolithic Polly.a and a thin module LLVMPolly.moduleext that links to
# that static library.
if (MSVC)
  # Add dummy target, because loadable modules are not supported on Windows
  add_custom_target(LLVMRV)
  set_target_properties(LLVMRV PROPERTIES FOLDER "rv")
else ()
  add_rv_loadable_module(LLVMRV
    rv.cpp
  )

target_link_libraries(LLVMRV RV)

set_target_properties(LLVMRV
    PROPERTIES
    LINKER_LANGUAGE CXX
    PREFIX "")
endif ()

if (TARGET intrinsics_gen)
  # Check if we are building as part of an LLVM build
  add_dependencies(RV intrinsics_gen)
endif()

