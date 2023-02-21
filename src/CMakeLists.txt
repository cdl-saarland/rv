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

IF (RV_ENABLE_VP)
  Message("-- rv: Using the LLVM-VP extension (experimental)")
  add_definitions( "-DLLVM_HAVE_VP" )
ENDIF()

if (RV_ENABLE_CRT)
  add_definitions( "-DRV_ENABLE_CRT" )
endif()

if (RV_DEBUG)
  add_definitions( "-DRV_DEBUG" )
endif()

IF (RV_REBUILD_GENBC)
  set_source_files_properties(${RV_SLEEF_OBJECTS} PROPERTIES GENERATED On)
endif()

set (RV_LIBRARY_OBJECTS
  ./PlatformInfo.cpp
  ./annotations.cpp
  ./config.cpp
  ./intrinsics.cpp
  ./legacy/init.cpp
  ./legacy/passes.cpp
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
  passes/AutoMathPass.cpp
  passes/LoopVectorizer.cpp
  passes/WFVPass.cpp
  passes/irPolisher.cpp
  passes/loopExitCanonicalizer.cpp
  passes/lowerRVIntrinsics.cpp
  passes/PassManagerSession.cpp
  region/FunctionRegion.cpp
  region/LoopRegion.cpp
  region/Region.cpp
  resolver/listResolver.cpp
  resolver/recResolver.cpp
  resolver/resolver.cpp
  resolver/sleefResolver.cpp
  rv-c/passes.cpp
  shape/vectorShape.cpp
  shape/vectorShapeTransformer.cpp
  transform/CoherentIFTransform.cpp
  transform/Linearizer.cpp
  transform/bosccTransform.cpp
  transform/crtLowering.cpp
  transform/guardedDivLoopTrans.cpp
  transform/loopCloner.cpp
  transform/lowerDivergentSwitches.cpp
  transform/maskExpander.cpp
  transform/memCopyElision.cpp
  transform/redOpt.cpp
  transform/redTools.cpp
  transform/remTransform.cpp
  transform/singleReturnTrans.cpp
  transform/splitAllocas.cpp
  transform/srovTransform.cpp
  transform/structOpt.cpp
  utils/llvmDomination.cpp
  utils/llvmDuplication.cpp
  utils/rvLinking.cpp
  utils/rvTools.cpp
  ${RV_HEADER_FILES}
  ${RV_SLEEF_OBJECTS}
)

IF (RV_COMPONENT_BUILD)
  add_llvm_component_library(
    ${RV_LIBRARY_NAME} ${RV_LIBRARY_OBJECTS}

    DEPENDS
    intrinsics_gen

    LINK_COMPONENTS
    Analysis
    Core
    Support
    ScalarOpts
    TransformUtils
  )
ELSE()
  add_rv_library(${RV_LIBRARY_NAME} ${RV_LIBRARY_OBJECTS})
ENDIF()

# Make sure BC buffer cpp files are available before linking libRV
IF (RV_REBUILD_GENBC)
  add_dependencies(${RV_LIBRARY_NAME} sleef_genbc)
endif()

if (NOT RV_COMPONENT_BUILD)
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
endif()

# # Build a monolithic libRV.a and a thin module LLVMRV.moduleext that links to
# # that static library.
# if (MSVC)
#   # Add dummy target, because loadable modules are not supported on Windows
#   add_custom_target(LLVMRV)
#   set_target_properties(LLVMRV PROPERTIES FOLDER "rv")
# else ()
#   add_rv_loadable_module(LLVMRV
#     rv.cpp
#   )
# 
# target_link_libraries(LLVMRV PRIVATE RV)
# 
# set_target_properties(LLVMRV
#     PROPERTIES
#     LINKER_LANGUAGE CXX
#     PREFIX "")
# endif ()
