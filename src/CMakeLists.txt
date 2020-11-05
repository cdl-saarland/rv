set(LLVM_NO_RTTI 1)

set(RV_HEADER_FILES)
if (MSVC_IDE OR XCODE)
  file(GLOB_RECURSE RV_HEADER_FILES "${RV_SOURCE_DIR}/include/rv/**/*.h")
endif ()

if (RV_ENABLE_SLEEF)
  add_definitions( "-DRV_ENABLE_SLEEF" )
  if (RV_ENABLE_X86)
    add_definitions( "-DRV_ENABLE_X86" )
  endif()
  if (RV_ENABLE_ADVSIMD)
    add_definitions( "-DRV_ENABLE_ADVSIMD" )
  endif()
endif()

if (RV_ENABLE_CRT)
  add_definitions( "-DRV_ENABLE_CRT" )
endif()

if (RV_DEBUG)
  add_definitions( "-DRV_DEBUG" )
endif()

add_rv_library(RV
  ./PlatformInfo.cpp
  ./annotations.cpp
  ./config.cpp
  ./init.cpp
  ./intrinsics.cpp
  ./passes.cpp
  ./registerPasses.cpp
  ./report.cpp
  ./rv.cpp
  ./rvConfig.cpp
  ./rvDebug.cpp
  ./utils.cpp
  ./vectorMapping.cpp
  ./vectorizationInfo.cpp
  analysis/AllocaSSA.cpp
  analysis/BranchEstimate.cpp
  analysis/DFG.cpp
  analysis/UndeadMaskAnalysis.cpp
  analysis/VectorizationAnalysis.cpp
  analysis/costModel.cpp
  analysis/loopAnnotations.cpp
  analysis/predicateAnalysis.cpp
  analysis/reductionAnalysis.cpp
  analysis/reductions.cpp
  native/MemoryAccessGrouper.cpp
  native/NatBuilder.cpp
  native/ShuffleBuilder.cpp
  native/Utils.cpp
  region/FunctionRegion.cpp
  region/LoopRegion.cpp
  region/Region.cpp
  resolver/TLIResolver.cpp
  resolver/listResolver.cpp
  resolver/recResolver.cpp
  resolver/resolver.cpp
  resolver/sleefResolver.cpp
  shape/vectorShape.cpp
  shape/vectorShapeTransformer.cpp
  transform/CoherentIFTransform.cpp
  transform/Linearizer.cpp
  transform/LoopVectorizer.cpp
  transform/WFVPass.cpp
  transform/bosccTransform.cpp
  # transform/cns/llvmDomination.cpp
  utils/llvmDuplication.cpp
  transform/crtLowering.cpp
  transform/crtLowering.cpp
  transform/divLoopTrans.cpp
  transform/guardedDivLoopTrans.cpp
  transform/irPolisher.cpp
  transform/loopCloner.cpp
  transform/loopExitCanonicalizer.cpp
  transform/lowerDivergentSwitches.cpp
  transform/lowerRVIntrinsics.cpp
  transform/maskExpander.cpp
  transform/memCopyElision.cpp
  transform/redOpt.cpp
  transform/redTools.cpp
  transform/remTransform.cpp
  transform/singleReturnTrans.cpp
  transform/splitAllocas.cpp
  transform/srovTransform.cpp
  transform/structOpt.cpp
  utils/rvLinking.cpp
  utils/rvTools.cpp
  ${RV_HEADER_FILES}
  ${RV_SLEEF_OBJECTS}
)

if (BUILD_SHARED_LIBS)
  target_link_libraries(RV PUBLIC
    LLVMSupport
    LLVMCore
    LLVMScalarOpts
    LLVMAggressiveInstCombine
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
  target_link_libraries(RV PUBLIC LLVM)
  link_directories(
    ${LLVM_LIBRARY_DIR}
  )
endif()

# Build a monolithic libRV.a and a thin module LLVMRV.moduleext that links to
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
