llvm_canonicalize_cmake_booleans(
  BUILD_SHARED_LIBS
  HAVE_LIBXAR
  HAVE_OCAMLOPT
  HAVE_OCAML_OUNIT
  LLVM_ENABLE_DIA_SDK
  LLVM_ENABLE_FFI
  LLVM_ENABLE_THREADS
  LLVM_ENABLE_ZLIB
  LLVM_ENABLE_LIBXML2
  LLVM_INCLUDE_GO_TESTS
  LLVM_LINK_LLVM_DYLIB
  LLVM_TOOL_LTO_BUILD
  LLVM_USE_INTEL_JITEVENTS
  LLVM_BUILD_EXAMPLES
  LLVM_ENABLE_PLUGINS
  LLVM_BYE_LINK_INTO_TOOLS
  LLVM_HAVE_TF_AOT
  LLVM_HAVE_TF_API
  LLVM_ENABLE_EXPENSIVE_CHECKS
  )

configure_lit_site_cfg(
  ${CMAKE_CURRENT_SOURCE_DIR}/lit.site.cfg.py.in
  ${CMAKE_CURRENT_BINARY_DIR}/lit.site.cfg.py
  MAIN_CONFIG
  ${CMAKE_CURRENT_SOURCE_DIR}/lit.cfg.py
  PATHS
  "RV_SOURCE_DIR"
  "LLVM_BINARY_DIR"
  "LLVM_TOOLS_DIR"
  "LLVM_LIBS_DIR"
  "SHLIBDIR"
  )

# Set the depends list as a variable so that it can grow conditionally.
# NOTE: Sync the substitutions in test/lit.cfg when adding to this list.
set(RV_TEST_DEPENDS
          BugpointPasses
          FileCheck
          LLVMHello
          UnitTests
          bugpoint
          count
          llc
          lli
          lli-child-target
          llvm-addr2line
          llvm-ar
          llvm-as
          llvm-bcanalyzer
          llvm-bitcode-strip
          llvm-c-test
          llvm-cat
          llvm-cfi-verify
          llvm-config
          llvm-cov
          llvm-cvtres
          llvm-cxxdump
          llvm-cxxfilt
          llvm-cxxmap
          llvm-diff
          llvm-dis
          llvm-dlltool
          dsymutil
          llvm-dwarfdump
          llvm-dwp
          llvm-elfabi
          llvm-exegesis
          llvm-extract
          llvm-gsymutil
          llvm-isel-fuzzer
          llvm-ifs
          llvm-install-name-tool
          llvm-jitlink
          llvm-lib
          llvm-libtool-darwin
          llvm-link
          llvm-lipo
          llvm-locstats
          llvm-lto2
          llvm-mc
          llvm-mca
          llvm-ml
          llvm-modextract
          llvm-mt
          llvm-nm
          llvm-objcopy
          llvm-objdump
          llvm-opt-fuzzer
          llvm-opt-report
          llvm-pdbutil
          llvm-profdata
          llvm-profgen
          llvm-ranlib
          llvm-rc
          llvm-readobj
          llvm-readelf
          llvm-reduce
          llvm-rtdyld
          llvm-size
          llvm-split
          llvm-strings
          llvm-strip
          llvm-symbolizer
          llvm-tblgen
          llvm-undname
          llvm-xray
          not
          obj2yaml
          opt
          sancov
          sanstats
          split-file
          verify-uselistorder
          yaml-bench
          yaml2obj
          rvTool
        )

add_custom_target(rv-test-depends DEPENDS ${RV_TEST_DEPENDS})
set_target_properties(rv-test-depends PROPERTIES FOLDER "Tests")

if(LLVM_BUILD_TOOLS)
  set(exclude_from_check_all "")
else()
  set(exclude_from_check_all "EXCLUDE_FROM_CHECK_ALL")
endif()

add_lit_testsuite(check-rv "Running the RV regression tests"
  ${CMAKE_CURRENT_BINARY_DIR}
  ${exclude_from_check_all}
  DEPENDS ${RV_TEST_DEPENDS}
  )
set_target_properties(check-rv PROPERTIES FOLDER "Tests")

add_lit_testsuites(RV ${CMAKE_CURRENT_SOURCE_DIR}
  ${exclude_from_check_all}
  DEPENDS ${RV_TEST_DEPENDS})
  
