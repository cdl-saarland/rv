
set(SLEEF_SOURCE_DIR  ${CMAKE_CURRENT_SOURCE_DIR}/sleef CACHE PATH "Path to libsleef sources" FORCE)
set(SLEEF_ARCH_DIR    ${SLEEF_SOURCE_DIR}/src/arch)
set(SLEEF_COMMON_DIR  ${SLEEF_SOURCE_DIR}/src/common)
set(RV_LIB_SLEEF_DIR  ${SLEEF_SOURCE_DIR}/src/libm)
set(RV_LIB_EXTRAS_DIR ${CMAKE_CURRENT_SOURCE_DIR}/extras)


if(RV_REBUILD_GENBC)
    message(STATUS "-- rv: Building the SLEEF BC library from source.")

    if(TARGET clang)
        set(LLVM_TOOL_CLANG clang)
    else()
        find_program(LLVM_TOOL_CLANG clang PATHS ${RV_BINARY_DIR}/../../bin/clang)
    endif()

    find_package(Python3 REQUIRED COMPONENTS Interpreter)
    set(RV_TOOL_GENCPP Python3::Interpreter ${RV_SOURCE_DIR}/tools/gen_cpp.py)

    file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
    set(RV_VECMATH_DIR ${CMAKE_CURRENT_BINARY_DIR})
else()
    message(STATUS "-- rv: Using pre-built SLEEF BC files.")
    set(RV_VECMATH_DIR ${CMAKE_CURRENT_SOURCE_DIR}/prebuilt_genbc)
endif()


set(RV_VECMATH_SOURCES)


set(SLEEF_SUPPORTED_SIMD_EXTENSIONS)
if (RV_ENABLE_X86)
    message(STATUS "-- rv: SLEEF with x86 support.")
    list(APPEND SLEEF_SUPPORTED_SIMD_EXTENSIONS avx512 avx2 avx sse)
endif()
if(RV_ENABLE_ADVSIMD)
    message(STATUS "-- rv: SLEEF with Advanced SIMD support.")
    list(APPEND SLEEF_SUPPORTED_SIMD_EXTENSIONS advsimd)
endif()
set(SLEEF_SIMD_EXTENSIONS_WITH_EXTRAS avx512 avx2 advsimd)


set(RV_VECMATH_FLAGS -Wall -Wno-unused -Wno-unknown-pragmas -DNDEBUG -O3 -fno-vectorize -ffp-contract=fast -I${SLEEF_ARCH_DIR} -I${SLEEF_COMMON_DIR})
set(FLAGS_sse -m64 -msse2 -msse4.1 -DENABLE_BUILTIN_MATH -DENABLE_SSE2=ON)
set(FLAGS_avx -mavx -mfma -DENABLE_BUILTIN_MATH -DENABLE_AVX=ON)
# TODO: why -march=skylake?
set(FLAGS_avx2 -march=skylake -mavx2 -mfma -DENABLE_BUILTIN_MATH -DENABLE_AVX2=ON)
set(FLAGS_avx512 -mfma -mavx512f -mavx512dq -mavx512cd -mavx512bw -mavx512vl -march=skylake -DENABLE_AVX512F=ON -DENABLE_BUILTIN_MATH)
if(APPLE)
    set(FLAGS_advsimd -target arm64-apple-darwin -mcpu=cyclone -DACCURATE_SQRT -DENABLE_ADVSIMD=ON -DENABLE_BUILTIN_MATH)
else()
    set(FLAGS_advsimd -mfma -target aarch64-unknown-linux-gnu -mcpu=cortex-a53 -DACCURATE_SQRT -DENABLE_ADVSIMD=ON -DENABLE_BUILTIN_MATH)
endif()
set(FLAGS_vla -DACCURATE_SQRT -DENABLE_BUILTIN_MATH -m64)


function(append_vecmath_bitcode _cpp _name _src)
    get_filename_component(_basename ${_cpp} NAME_WE)

    set(_cpp ${RV_VECMATH_DIR}/${_cpp})
    set(_bc  ${RV_VECMATH_DIR}/${_basename}.bc)

    if(RV_REBUILD_GENBC)
        add_custom_command(OUTPUT ${_cpp}
            COMMAND ${LLVM_TOOL_CLANG} ${_src} -emit-llvm -c ${RV_VECMATH_FLAGS} ${ARGN} -o ${_bc}
            COMMAND ${RV_TOOL_GENCPP} ${_cpp} ${_name} ${_bc}
            DEPENDS ${_src} ${LLVM_TOOL_CLANG}
            BYPRODUCTS ${_bc}
            VERBATIM COMMAND_EXPAND_LISTS
        )
    endif()

    set(RV_VECMATH_SOURCES ${RV_VECMATH_SOURCES} ${_cpp} PARENT_SCOPE)
endfunction()

# Constant table (rempitab)
append_vecmath_bitcode(rempitab.gen.cpp "rempitab" ${RV_LIB_SLEEF_DIR}/rempitab.c ${FLAGS_vla})

# vector-length agnostic
append_vecmath_bitcode(vla_sleef_sp.gen.cpp "vla_sp" ${RV_LIB_SLEEF_DIR}/sleefsp.c ${FLAGS_vla})
append_vecmath_bitcode(vla_sleef_dp.gen.cpp "vla_dp" ${RV_LIB_SLEEF_DIR}/sleefdp.c ${FLAGS_vla})


# arch specific simd extensions
foreach(_simd ${SLEEF_SUPPORTED_SIMD_EXTENSIONS})
    append_vecmath_bitcode(${_simd}_sleef_sp.gen.cpp "${_simd}_sp" ${RV_LIB_SLEEF_DIR}/sleefsimdsp.c ${FLAGS_${_simd}})
    append_vecmath_bitcode(${_simd}_sleef_dp.gen.cpp "${_simd}_dp" ${RV_LIB_SLEEF_DIR}/sleefsimddp.c ${FLAGS_${_simd}})

    if(_simd IN_LIST SLEEF_SIMD_EXTENSIONS_WITH_EXTRAS)
        append_vecmath_bitcode(${_simd}_extras.gen.cpp "${_simd}_extras" ${RV_LIB_EXTRAS_DIR}/vrand_${_simd}.c ${FLAGS_${_simd}})
    endif()
endforeach()


if(RV_ENABLE_CRT)
    # compiler-rt
    set(CRT_BC    ${RV_VECMATH_DIR}/crt.bc)
    set(CRT_GENBC ${RV_VECMATH_DIR}/crt.gen.cpp)

    if(RV_REBUILD_GENBC)
        # TODO: needs revision of finding CRT_SOURCES

        # look for compiler-rt source folder
        # try to find compiler-rt
        function(get_compiler_rt_path path)
            set(CRT_SEARCH_PATHS "${CMAKE_SOURCE_DIR}/runtimes/compiler-rt;${CMAKE_SOURCE_DIR}/projects/compiler-rt;${CMAKE_SOURCE_DIR}/../compiler-rt/")
            foreach(entry ${CRT_SEARCH_PATHS})
                if(IS_DIRECTORY ${entry})
                    set(${path} ${entry} PARENT_SCOPE)
                    return()
                endif()
            endforeach()
        endfunction()

        get_compiler_rt_path(CRT_SOURCES)

        message(STATUS "-- rv: Building compiler-rt BC libs using ${CRT_SOURCES}.")

        set(CRT_INC "${CRT_SOURCES}/lib/builtins")
        if(NOT (IS_DIRECTORY ${CRT_INC}))
            message(FATAL_ERROR "--rv: RV_ENABLE_CRT options requires compiler-rt sources to be available!")
        endif()

        # compiler-rt runtime library
        add_custom_command(OUTPUT ${CRT_GENBC}
            COMMAND ${LLVM_TOOL_CLANG} ${CMAKE_CURRENT_SOURCE_DIR}/crt.c -I${CRT_INC} -m64 -emit-llvm -c ${RV_VECMATH_FLAGS} -o ${CRT_BC}
            COMMAND ${RV_TOOL_GENCPP} ${CRT_GENBC} "crt" ${CRT_BC}
            DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/crt.c ${LLVM_TOOL_CLANG}
            BYPRODUCTS ${CRT_BC}
            VERBATIM COMMAND_EXPAND_LISTS
        )
    else()  # !RV_ENABLE_CRT
        message(STATUS "-- rv: Building without compiler-rt BC libs.")
    endif()

    list(APPEND RV_VECMATH_SOURCES ${CRT_GENBC})
endif()


add_library(vecmath OBJECT ${RV_VECMATH_SOURCES})
