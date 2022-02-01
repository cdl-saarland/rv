# add_subdirectory(lit)

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

# File-based tests.
configure_lit_site_cfg(
  ${CMAKE_CURRENT_SOURCE_DIR}/lit/lit.site.cfg.py.in
  ${CMAKE_CURRENT_BINARY_DIR}/lit/lit.site.cfg.py
  MAIN_CONFIG
  ${CMAKE_CURRENT_SOURCE_DIR}/lit/lit.cfg.py
  PATHS
  "RV_SOURCE_DIR"
  "LLVM_BINARY_DIR"
  "LLVM_TOOLS_DIR"
  "LLVM_LIBS_DIR"
  "SHLIBDIR"
  )

# Functional RV tests.
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
          ${LLVM_TEST_DEPENDS}
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
  
