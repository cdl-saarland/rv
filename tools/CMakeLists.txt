# get source files
SET ( RVTOOL_NAME rvTool )

SET ( RVTOOL_SOURCE_FILES rvTool.cpp rvTool.h )
INCLUDE_DIRECTORIES ( include/ )

ADD_EXECUTABLE ( ${RVTOOL_NAME} ${RVTOOL_SOURCE_FILES} )

# link with libRV in any case
target_link_libraries(${RVTOOL_NAME}
  RV
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

install (TARGETS ${RVTOOL_NAME} DESTINATION bin)

link_directories(
  ${LLVM_LIBRARY_DIR}
)
