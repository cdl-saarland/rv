set(LLVM_NO_RTTI 1)

set(RV_HEADER_FILES)
if (MSVC_IDE OR XCODE)
  file(GLOB_RECURSE RV_HEADER_FILES "${RV_SOURCE_DIR}/include/rv/**/*.h")
endif ()

if (RV_ENABLE_SLEEF) 
  add_definitions( "-DRV_ENABLE_BUILTINS" )
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
  rvConfig.cpp
  transform/loopExitCanonicalizer.cpp
  transform/maskGenerator.cpp
  transform/LoopVectorizer.cpp
  transform/Linearizer.cpp
  transform/remTransform.cpp
  transform/irPolisher.cpp
  transform/srovTransform.cpp
  transform/structOpt.cpp
  utils/maskGraphUtils.cpp
  utils/rvTools.cpp
  VectorizationInfoProxyPass.cpp
  region/LoopRegion.cpp
  region/Region.cpp
  PlatformInfo.cpp
  init.cpp
  report.cpp
  rv.cpp
  registerPasses.cpp
  sleefLibrary.cpp
  vectorMapping.cpp
  vectorShape.cpp
  vectorizationInfo.cpp
  ${RV_HEADER_FILES}
)

if (TARGET gensleef)
  target_link_libraries(RV PRIVATE gensleef)
ENDIF()

if (BUILD_SHARED_LIBS)
  target_link_libraries(RV PUBLIC
    LLVMSupport
    LLVMCore
    LLVMScalarOpts
    LLVMInstCombine
    LLVMTransformUtils
    LLVMAnalysis
    LLVMipo
    LLVMMC
    LLVMIRReader
    LLVMPasses
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

target_link_libraries(LLVMRV PRIVATE RV)

set_target_properties(LLVMRV
    PROPERTIES
    LINKER_LANGUAGE CXX
    PREFIX "")
endif ()

