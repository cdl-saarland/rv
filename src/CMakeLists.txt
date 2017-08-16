set(LLVM_NO_RTTI 1)

set(RV_HEADER_FILES)
if (MSVC_IDE OR XCODE)
  file(GLOB_RECURSE RV_HEADER_FILES "${RV_SOURCE_DIR}/include/rv/**/*.h")
endif ()

if (RV_ENABLE_X86)
  add_definitions( "-DRV_ENABLE_X86" )
ENDIF()

if (RV_ENABLE_ADVSIMD)
  add_definitions( "-DRV_ENABLE_ADVSIMD" )
endif()

if (RV_ENABLE_SLEEF) 
  add_definitions( "-DRV_ENABLE_BUILTINS" )
  if (RV_ENABLE_CRT)
    add_definitions( "-DRV_ENABLE_CRT" )
  endif()
endif()

if (RV_DEBUG) 
  add_definitions( "-DRV_DEBUG" )
endif()

add_rv_library(RV
  PlatformInfo.cpp
  VectorizationInfoProxyPass.cpp
  analysis/BranchDependenceAnalysis.cpp
  analysis/DFG.cpp
  analysis/VectorizationAnalysis.cpp
  analysis/reductionAnalysis.cpp
  analysis/DivPathDecider.cpp
  config.cpp
  init.cpp
  native/MemoryAccessGrouper.cpp
  native/NatBuilder.cpp
  native/ShuffleBuilder.cpp
  native/Utils.cpp
  region/LoopRegion.cpp
  region/Region.cpp
  registerPasses.cpp
  report.cpp
  rv.cpp
  rvConfig.cpp
  sleefLibrary.cpp
  transform/CNSPass.cpp
  transform/Linearizer.cpp
  transform/LoopVectorizer.cpp
  transform/bosccTransform.cpp
  transform/cns/BlockGraph.cpp
  transform/cns/CNS.cpp
  transform/cns/CNSScoring.cpp
  transform/cns/SplitTree.cpp
  transform/cns/llvmDomination.cpp
  transform/cns/llvmDuplication.cpp
  transform/divLoopTrans.cpp
  transform/irPolisher.cpp
  transform/loopExitCanonicalizer.cpp
  transform/maskExpander.cpp
  transform/redTools.cpp
  transform/redOpt.cpp
  transform/remTransform.cpp
  transform/srovTransform.cpp
  transform/structOpt.cpp
  utils/rvTools.cpp
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
