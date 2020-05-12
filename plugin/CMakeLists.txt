if(LLVM_RVPLUG_LINK_INTO_TOOLS)
  message("-- rv: Linking RV into LLVM tools")
endif()

add_llvm_pass_plugin(RVPLUG
  plugin.cpp
  DEPENDS
  RV
  intrinsics_gen
  BUILDTREE_ONLY
 )
target_link_libraries(RVPLUG PRIVATE RV)
