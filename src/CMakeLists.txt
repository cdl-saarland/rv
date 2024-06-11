#set(LLVM_NO_RTTI 1)

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

set (RV_SOURCES
  ./PlatformInfo.cpp
  ./config.cpp
  ./intrinsics.cpp
  ./report.cpp
  ./rv.cpp
  #./rvConfig.cpp
  ./rvDebug.cpp
  ./utils.cpp
  ./vectorMapping.cpp
  ./vectorizationInfo.cpp
  analysis/AllocaSSA.cpp
  analysis/UndeadMaskAnalysis.cpp
  analysis/VectorizationAnalysis.cpp
  analysis/predicateAnalysis.cpp
  analysis/reductionAnalysis.cpp
  analysis/reductions.cpp
  native/NatBuilder.cpp
  native/ShuffleBuilder.cpp
  native/Utils.cpp
  passes/irPolisher.cpp
  passes/loopExitCanonicalizer.cpp
  passes/PassManagerSession.cpp
  region/FunctionRegion.cpp
  region/Region.cpp
  resolver/listResolver.cpp
  resolver/resolver.cpp
  resolver/sleefResolver.cpp
  shape/vectorShape.cpp
  shape/vectorShapeTransformer.cpp
  transform/Linearizer.cpp
  transform/guardedDivLoopTrans.cpp
  transform/lowerDivergentSwitches.cpp
  transform/maskExpander.cpp
  transform/memCopyElision.cpp
  transform/redOpt.cpp
  transform/redTools.cpp
  transform/singleReturnTrans.cpp
  transform/splitAllocas.cpp
  transform/srovTransform.cpp
  transform/structOpt.cpp
  utils/llvmDuplication.cpp
  utils/rvLinking.cpp
  utils/rvTools.cpp

  native/NatBuilder.h
  native/ShuffleBuilder.h
  native/Utils.h
  report.h
  rvConfig.h
  utils/llvmDuplication.h
  utils/mathUtils.h
  utils/rvLinking.h
  utils/rvTools.h
)


#cmake_parse_arguments(ARG "" "" "" ${ARGN})
#set(srcs ${ARG_UNPARSED_ARGUMENTS})

#  if(MSVC_IDE OR XCODE)
#    file( GLOB_RECURSE headers *.h *.td *.def)
#    set(RV_SOURCES ${RV_SOURCES} ${headers})
#    string( REGEX MATCHALL "/[^/]+" split_path ${CMAKE_CURRENT_SOURCE_DIR})
#    list( GET split_path -1 dir)
#    file( GLOB_RECURSE headers ../../include/rv${dir}/*.h)
#    set(RV_SOURCES ${RV_SOURCES} ${headers})
#  endif(MSVC_IDE OR XCODE)

#  if (MODULE)
#    set(libkind MODULE)
#  elseif (SHARED_LIBRARY)
#    set(libkind SHARED)
#  else()
#    set(libkind)
#  endif()

add_library( RV ${RV_SOURCES} ${RV_HEADER_FILES} ${RV_SLEEF_OBJECTS} )
target_include_directories( RV SYSTEM PRIVATE ${LLVM_INCLUDE_DIRS} )
target_include_directories( RV PRIVATE ${RV_SOURCE_DIR}/include ${RV_SOURCE_DIR}/src )

llvm_config(RV core analysis)

# Make sure BC buffer cpp files are available before linking libRV
IF (RV_REBUILD_GENBC)
    add_dependencies(RV vecmath)
endif()
