function ( get_rv_llvm_dependency_libs OUT_VAR )
  llvm_map_components_to_libnames ( RV_LLVM_TEMP_LIBS all ) # scalaropts linker ipo irreader mcjit support core irreader
    SET ( ${OUT_VAR} ${RV_LLVM_TEMP_LIBS} PARENT_SCOPE )
endfunction( get_rv_llvm_dependency_libs )

