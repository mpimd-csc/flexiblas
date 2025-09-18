IF (WIN32)
    cmake_minimum_required(VERSION 3.18.0)
ELSE()
    cmake_minimum_required(VERSION 3.10.0)
ENDIF()
INCLUDE(libcscutils/CMakeModules/CheckPolicy.cmake)
CheckPolicy(CMP0048 NEW)
CheckPolicy(CMP0054 NEW)
CheckPolicy(CMP0069 NEW)
CheckPolicy(CMP0075 NEW)

PROJECT(FlexiBLAS LANGUAGES C Fortran VERSION 3.4.82)


#
# FlexiBLAS version setup
#
SET(FLEXIBLAS_VERSION "${FlexiBLAS_VERSION}")
SET(FLEXIBLAS_VERSION_MAJOR  ${FlexiBLAS_VERSION_MAJOR})
SET(FLEXIBLAS_VERSION_MINOR  ${FlexiBLAS_VERSION_MINOR})
SET(FLEXIBLAS_VERSION_PATCH  ${FlexiBLAS_VERSION_PATCH})
IF ( NOT FLEXIBLAS_VERSION_PATCH )
    SET(FLEXIBLAS_VERSION_PATCH "0")
ENDIF()
SET(FLEXIBLAS_YEARS "2013-2025")
MESSAGE(STATUS "Configuring FlexiBLAS ${FlexiBLAS_VERSION_MAJOR}.${FlexiBLAS_VERSION_MINOR}.${FlexiBLAS_VERSION_PATCH}")


SET(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/libcscutils/CMakeModules ${PROJECT_SOURCE_DIR}/cmake ${CMAKE_MODULE_PATH})
SET(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib)
SET(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib)
SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin)
INCLUDE (CheckFeatureNeeded)
INCLUDE (PreventInSourceBuilds)
INCLUDE (PreventInBuildInstalls)
INCLUDE (CMakeHelpers)
INCLUDE (CheckIPOSupported)
#
# Options
#
INCLUDE(CMakeDependentOption)

OPTION(TESTS "Enable Tests" ON)
OPTION(CBLAS "Enable CBLAS Interface" ON )
OPTION(INTEGER8 "Use 8 Byte Integer" OFF)
OPTION(BUILD_SHARED_LIBS "Enable Shared Library" ON)
OPTION(LAPACK "Enable LAPACK Support" ON)
CMAKE_DEPENDENT_OPTION(LAPACKE "Enable LAPACKE Support" ON "LAPACK" OFF)

OPTION(DEV "Development Build" OFF)
OPTION(MKL_BUILDER "Build MKL support with Intel MKLs Custom library builder" OFF)
OPTION(EXAMPLES "Build Examples" ON)
OPTION(BLAS_AUTO_DETECT "Enabled the BLAS auto detection during configuration" ON)
OPTION(LINK_OPENMP "Link OpenMP to FlexiBLAS" OFF)
IF (APPLE)
	OPTION(LTO "Enable Link Time Optimization / IPO" OFF)
ELSE()
    OPTION(LTO "Enable Link Time Optimization / IPO" ON)
ENDIF()

OPTION(FLEXIBLAS_HOOK_API "Build the Hook API" ON)

IF (NOT DEFINED FLEXIBLAS_DEFAULT_BLAS)
    SET(FLEXIBLAS_DEFAULT_BLAS "NETLIB")
ELSE()
    STRING (TOUPPER "${FLEXIBLAS_DEFAULT_BLAS}" FLEXIBLAS_DEFAULT_BLAS)
ENDIF()

#
# build type
#
MESSAGE(STATUS "CMAKE_BUILD_TYPE = ${CMAKE_BUILD_TYPE}")
IF ( DEFINED DEBUG)
    IF (DEBUG STREQUAL ON)
        SET( CMAKE_BUILD_TYPE "Debug" )
        SET( DEBUG TRUE)
        SET( CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DDEBUG")
    ELSE()
	SET( CMAKE_BUILD_TYPE "Release")
	SET( DEBUG FALSE)
        SET( CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -UDEBUG")
    ENDIF()
ELSE()
    IF( NOT CMAKE_BUILD_TYPE )
        SET( CMAKE_BUILD_TYPE "Release" )
    ENDIF( NOT CMAKE_BUILD_TYPE )

    IF ( CMAKE_BUILD_TYPE STREQUAL "Debug" )
        SET ( DEBUG "ON" )
    ENDIF ( CMAKE_BUILD_TYPE STREQUAL "Debug" )
ENDIF()
#
# Paths
#
INCLUDE(GNUInstallDirs)

IF(DEFINED SYSCONFDIR)
    SET(CMAKE_INSTALL_FULL_SYSCONFDIR ${SYSCONFDIR})
ENDIF()

MESSAGE(STATUS "SYSCONFDIR: ${CMAKE_INSTALL_FULL_SYSCONFDIR}")
MESSAGE(STATUS "LIBDIR:     ${CMAKE_INSTALL_FULL_LIBDIR}")
MESSAGE(STATUS "BINDIR:     ${CMAKE_INSTALL_FULL_BINDIR}")
MESSAGE(STATUS "INCLUDEDIR: ${CMAKE_INSTALL_FULL_INCLUDEDIR}")
MESSAGE(STATUS "MANDIR:     ${CMAKE_INSTALL_FULL_MANDIR}")
MESSAGE(STATUS "Compiler_ID: ${CMAKE_C_COMPILER_ID}")
MESSAGE(STATUS "FCompiler_ID: ${CMAKE_Fortran_COMPILER_ID}")

#
# Compiler Settings
#
SET(CMAKE_POSITION_INDEPENDENT_CODE ON)
INCLUDE(CCompilerSettings)
INCLUDE(FortranCompilerSettings)


# Check C Compiler Version
IF ( CMAKE_C_COMPILER_ID STREQUAL "GNU" AND CMAKE_C_COMPILER_VERSION VERSION_LESS "5.0.0" )
    MESSAGE_COLOR(STATUS COLOR_BOLD_RED "Your gcc is older than version 5.0.0. LAPACK will be loaded using DEEPBIND")
    ADD_DEFINITIONS(-DLAPACK_DEEP_BIND)
ENDIF()

IF ( (CMAKE_C_COMPILER_ID STREQUAL "Intel" AND CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
    OR (CMAKE_C_COMPILER_ID STREQUAL "IntelLLVM" AND CMAKE_Fortran_COMPILER_ID STREQUAL "Intel"))
    MESSAGE_COLOR(FATAL_ERROR COLOR_BOLD_RED "Mixing Classic and LLVM based Intel compilers is not possible. Specify proper C and Fortran Compilers.")
ENDIF()


INCLUDE(ccache)




#SET(BUILD_SHARED_LIBS ON)
INCLUDE(CheckCCompilerFlag)
INCLUDE(CheckFortranCompilerFlag)


INCLUDE(EnablePedanticC)
INCLUDE(EnableStrictPrototypesC)
INCLUDE(EnableCastQual)

IF (LTO STREQUAL ON )
    check_ipo_supported(RESULT IPO_SUPPORT OUTPUT IPO_OUTPUT LANGUAGES C Fortran)
    IF (IPO_SUPPORT)
        MESSAGE(STATUS "LTO/IPO enabled.")
        SET(CMAKE_INTERPROCEDURAL_OPTIMIZATION TRUE)
    ELSE()
        MESSAGE(STATUS "IPO_SUPPORT not available: ${IPO_OUTPUT}")
    ENDIF()
ENDIF()

SET(CMAKE_C_STANDARD_REQUIRED 1)
SET(CMAKE_C_STANDARD 99)

IF ( ${CMAKE_C_COMPILER_ID} STREQUAL "GNU")
    check_c_compiler_flag("-fstack-protector-strong" C_STACK_PROTECTOR)
    check_c_compiler_flag("-fstack-clash-protection" C_STACK_CLASH)
    check_c_compiler_flag("-Wlto-type-mismatch" C_W_LTO_TYPE_MISMATCH)
    check_c_compiler_flag("-Wunused-parameter" C_W_UNUSED_PARAMETER)
    check_c_compiler_flag("-Wunused-but-set-parameter" C_W_UNUSED_BUT_SET_PARAMETER)

    check_fortran_compiler_flag("-fstack-protector-strong" FC_STACK_PROTECTOR)
    check_fortran_compiler_flag("-fstack-clash-protection" FC_STACK_CLASH)
    check_fortran_compiler_flag("-Wlto-type-mismatch" FC_W_LTO_TYPE_MISMATCH)
    check_fortran_compiler_flag("-Wunused-parameter" FC_W_UNUSED_PARAMETER)
    check_fortran_compiler_flag("-Wunused-but-set-parameter" FC_W_UNUSED_BUT_SET_PARAMETER)


    IF(C_STACK_PROTECTOR)
        SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fstack-protector-strong")
    ENDIF()
    IF (C_STACK_CLASH)
        SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fstack-clash-protection")
    ENDIF()
    IF (C_W_LTO_TYPE_MISMATCH)
        SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wlto-type-mismatch")
    ENDIF()
    IF (C_W_UNUSED_PARAMETER)
        SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wunused-parameter")
    ENDIF()
    IF (C_W_UNUSED_BUT_SET_PARAMETER)
        SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wunused-but-set-parameter")
    ENDIF()

    IF(FC_STACK_PROTECTOR)
        SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fstack-protector-strong")
    ENDIF()
    IF (FC_STACK_CLASH)
        SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fstack-clash-protection")
    ENDIF()
    IF (FC_W_LTO_TYPE_MISMATCH)
        SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wlto-type-mismatch")
    ENDIF()

    IF (FC_W_UNUSED_PARAMETER)
        SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wunused-parameter")
    ENDIF()
    IF (FC_W_UNUSED_BUT_SET_PARAMETER)
        SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wunused-but-set-parameter")
    ENDIF()

ENDIF()

#
# MacOS on ARM produces wrong results with to expensive optimizations
#
IF ( APPLE AND CMAKE_SYSTEM_PROCESSOR STREQUAL "arm64" AND CMAKE_Fortran_COMPILER_ID STREQUAL "GNU"
     AND CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL "14.0.0" AND CMAKE_Fortran_COMPILER_VERSION VERSION_LESS_EQUAL "16.0.0")
	message(WARNING "Your gfortran compiler is known to produce wrong code on MacOS/Apple Silicon. Turning off some optimizations. See https://github.com/Reference-LAPACK/lapack/issues/1073 for some details")
	check_c_compiler_flag("-fno-tree-loop-vectorize" C_F_NO_TREE_LOOP_VECTORIZE)
	check_fortran_compiler_flag("-fno-tree-loop-vectorize" FC_F_NO_TREE_LOOP_VECTORIZE)
	check_c_compiler_flag("-fno-expensive-optimizations" C_F_NO_EXPENSIVE_OPTIMIZATIONS)
	check_fortran_compiler_flag("-fno-expensive-optimizations" FC_F_NO_EXPENSIVE_OPTIMIZATIONS)
	if (C_F_NO_TREE_LOOP_VECTORIZE)
		set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fno-tree-loop-vectorize")
	endif()

	if (FC_F_NO_TREE_LOOP_VECTORIZE)
		set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fno-tree-loop-vectorize")
	endif()
	if (C_F_NO_EXPENSIVE_OPTIMIZATIONS)
		set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fno-expensive-optimizations")
	endif()

	if (FC_F_NO_EXPENSIVE_OPTIMIZATIONS)
		set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fno-expensive-optimizations")
	endif()
ENDIF()



FIND_PACKAGE(OpenMP)
if(NOT TARGET OpenMP::OpenMP_C AND OpenMP_C_FOUND)
    find_package(Threads REQUIRED)
    add_library(OpenMP::OpenMP_C IMPORTED INTERFACE)
    set_property(TARGET OpenMP::OpenMP_C
        PROPERTY INTERFACE_COMPILE_OPTIONS ${OpenMP_C_FLAGS})
    set_property(TARGET OpenMP_C::OpenMP_C INTERFACE_INCLUDE_DIRECTORIES ${OpenMP_C_INCLUDE_DIRS} )
    # Only works if the same flag is passed to the linker; use CMake 3.9+ otherwise (Intel, AppleClang)
    set_property(TARGET OpenMP::OpenMP_C
        PROPERTY INTERFACE_LINK_LIBRARIES ${OpenMP_C_FLAGS} Threads::Threads)
endif()

INCLUDE(cmake/libm.cmake)
IF (NEED_LINKING_AGAINST_LIBM)
    SET(LIBS ${LIBS} ${LIBM_NAME})
ENDIF()



IF (NOT DEFINED ABI)
    IF ( ${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel" OR ${CMAKE_Fortran_COMPILER_ID} STREQUAL "IntelLLVM")
        SET(ABI "Intel" CACHE STRING "ABI of the Fortran Interface")
    ELSEIF ( ${CMAKE_Fortran_COMPILER_ID} STREQUAL "PGI" )
        SET(ABI "Intel" CACHE STRING "ABI of the Fortran Interface")
    ELSEIF ( ${CMAKE_Fortran_COMPILER_ID} STREQUAL "NVHPC" AND CMAKE_SYSTEM_PROCESSOR MATCHES "aarch64" )
        SET(ABI "GNU" CACHE STRING "ABI of the Fortran Interface")
    ELSEIF ( ${CMAKE_Fortran_COMPILER_ID} STREQUAL "NVHPC" AND CMAKE_SYSTEM_PROCESSOR MATCHES "x86_64" )
        SET(ABI "Intel" CACHE STRING "ABI of the Fortran Interface")
    ELSEIF ( ${CMAKE_Fortran_COMPILER_ID} STREQUAL "XL" )
        SET(ABI "XL" CACHE STREQUAL "ABI of the Fortran Interface")
    ELSE()
        SET(USE_INTERFACE_INTEL FALSE)
        SET(ABI "GNU" CACHE STRING "ABI of the Fortran Interface")
    ENDIF()
ELSE()
    IF (NOT ( ABI STREQUAL "GNU" OR ABI STREQUAL "Intel" OR ABI STREQUAL "XL"))
        MESSAGE(FATAL_ERROR "ABI must be GNU or Intel")
    ENDIF()
ENDIF()

IF ( ABI STREQUAL "Intel")
    SET(FLEXIBLAS_ABI_INTEL TRUE)
    SET(USE_INTERFACE_INTEL TRUE)
    set(abi "intel")
ELSEIF ( ABI STREQUAL "XL" )
    SET(FLEXIBLAS_ABI_IBM TRUE)
    set(abi "intel")
ELSE ()
    SET (FLEXIBLAS_ABI_GNU TRUE)
    SET(USE_INTERFACE_INTEL FALSE)
    set(abi "gnu")
ENDIF()

# Name Mangling
include(FortranCInterface)
FortranCInterface_HEADER(${PROJECT_BINARY_DIR}/fc.h
    MACRO_NAMESPACE "FC_"
    SYMBOL_NAMESPACE "FC_")
FortranCInterface_VERIFY()

MESSAGE(STATUS "Fortran - C - Interface -- FortranCInterface_GLOBAL_SUFFIX = ${FortranCInterface_GLOBAL_SUFFIX}")
MESSAGE(STATUS "Fortran - C - Interface -- FortranCInterface_GLOBAL_CASE = ${FortranCInterface_GLOBAL_CASE}")

IF ( FortranCInterface_GLOBAL_SUFFIX STREQUAL "_" AND FortranCInterface_GLOBAL_CASE STREQUAL "LOWER")
    SET(FLEXIBLAS_MANGLE_DEFAULT 1)
    SET(FLEXIBLAS_MANGLE_NO_UNDERSCORE 0)
    SET(FLEXIBLAS_MANGLE_UPPERCASE 0)
    MESSAGE(STATUS "Use default Fortran name mangling.")
ELSEIF(FortranCInterface_GLOBAL_SUFFIX STREQUAL "" AND FortranCInterface_GLOBAL_CASE STREQUAL "LOWER")
    SET(FLEXIBLAS_MANGLE_DEFAULT 0)
    SET(FLEXIBLAS_MANGLE_NO_UNDERSCORE 1)
    SET(FLEXIBLAS_MANGLE_UPPERCASE 0)
    MESSAGE(STATUS "Use Windows-like Fortran name mangling.")
ELSEIF(FortranCInterface_GLOBAL_SUFFIX STREQUAL "" AND FortranCInterface_GLOBAL_CASE STREQUAL "UPPER")
    SET(FLEXIBLAS_MANGLE_DEFAULT 0)
    SET(FLEXIBLAS_MANGLE_NO_UNDERSCORE 0)
    SET(FLEXIBLAS_MANGLE_UPPERCASE 1)
    MESSAGE(STATUS "Use upper-case Fortran name mangling.")
ENDIF()




include(CheckThreadLocal)

IF (DEFINED FALLBACK)
    SET (FALLBACK_NAME "${FALLBACK}")
ELSE()
    SET (FALLBACK_NAME FALSE)
ENDIF()

SET ( INCLUDE_DIR ${INCLUDE_DIR} ${CMAKE_BINARY_DIR}/include ${CMAKE_SOURCE_DIR}/include ${CMAKE_BINARY_DIR} )




LIST(REMOVE_DUPLICATES INCLUDE_DIR)
IF (APPLE)
    SET ( CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -D_FILE_OFFSET_BITS=64 ")
    SET ( CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -rdynamic -Wl,-export_dynamic")
ELSE()
  IF (WIN32)
    # Export all symbols on Windows when building shared libraries
    SET(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS TRUE)

    SET ( CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -D_FILE_OFFSET_BITS=64 -D__WIN32__ -D_WIN32")
    SET ( CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wno-deprecated-declarations -Wno-unused-function -Wno-sometimes-uninitialized -Wno-self-assign")
    IF(NOT (MINGW OR MSYS))
        SET(INCLUDE_DIR ${INCLUDE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/contributed/win32ports)
    ENDIF()
  ELSE ()
    SET ( CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -D_FILE_OFFSET_BITS=64")
    IF  ( CMAKE_Fortran_COMPILER_ID STREQUAL "Intel" OR CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
            set (CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -rdynamic -Wl,--export-dynamic")
        ENDIF()
        # SET ( CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -D_FILE_OFFSET_BITS=64 ")

    ENDIF()
ENDIF()
SET ( CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -D_FILE_OFFSET_BITS=64 ")

SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)


#
# Check Some files
#
INCLUDE (CheckIncludeFiles)
IF (WIN32)
    MESSAGE(STATUS "Building Windows Version" )
    IF (MINGW)
        MESSAGE(STATUS "Using MingW Compiler")
    ENDIF()

    find_package(dlfcn-win32 REQUIRED)
    INCLUDE_DIRECTORIES(${dlfcn-win32_INCLUDEDIR})
    SET(DL_LIB dlfcn-win32::dl)
    SET(LIBS ${LIBS} dlfcn-win32::dl)
ELSE()
    # CHECK_INCLUDE_FILES (stdatomic.h HAVE_STDATOMIC_H)
    CHECK_INCLUDE_FILES (dlfcn.h	 HAVE_DLFCN_H)
    CHECK_INCLUDE_FILES (sys/sysmacro.h HAVE_SYSMACRO_H)
    CHECK_INCLUDE_FILES (sys/stat.h     HAVE_SYS_STAT_H)
    INCLUDE (CheckSymbolExists)
    CHECK_SYMBOL_EXISTS(RTLD_GLOBAL   "dlfcn.h" HAVE_RTLD_GLOBAL)
    CHECK_SYMBOL_EXISTS(RTLD_DEEPBIND "dlfcn.h" HAVE_RTLD_DEEPBIND)
    CHECK_SYMBOL_EXISTS(RTLD_NOW	  "dlfcn.h" HAVE_RTLD_NOW)
    CHECK_SYMBOL_EXISTS(RTLD_NODELETE "dlfcn.h" HAVE_RTLD_NODELETE)

    INCLUDE (CheckFunctionExists)
    CHECK_FUNCTION_EXISTS(dlopen DLOPEN_C)
    SET(__CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES})
    SET(CMAKE_REQUIRED_LIBRARIES dl)
    CHECK_FUNCTION_EXISTS(dlopen DLOPEN_DL)
    SET(CMAKE_REQUIRED_LIBRARIES ${__CMAKE_REQUIRED_LIBRARIES})

    IF (DLOPEN_DL)
        SET(LIBS ${LIBS} dl)
        SET(DL_LIB dl)
    ENDIF()
    IF (NOT ( DLOPEN_DL OR DLOPEN_C ) )
        MESSAGE(ERROR "No dlopen function found")
    ENDIF()

    IF ( NOT HAVE_DLFCN_H )
        MESSAGE(FATAL_ERROR "dlfcn.h not found, aborting")
    ENDIF()

    IF ( NOT HAVE_RTLD_NODELETE )
        MESSAGE(WARNING "RTLD_NODELETE not found in your C library. Consider setting -DLINK_OPENMP=ON")
    ENDIF()
ENDIF()

INCLUDE(CheckAttributeExists)
Check_Attribute_Exists( constructor HAVE_ATTR_CONSTRUCTOR)
Check_Attribute_Exists( "alias(\"foo\")" HAVE_ATTR_ALIAS)
Check_Attribute_Exists( "visibility(\"hidden\")" HAVE_ATTR_HIDDEN)
Check_Attribute_Exists( "visibility(\"default\")" HAVE_ATTR_DEFAULT)
Check_Attribute_Exists( unused HAVE_ATTR_UNUSED )
SET(EXTRA_BLAS "")


### libcscutils
SET(CSCUTILS_FEATURES "inifile;ds")
SET(CSCUTILS_OBJECT TRUE)
add_subdirectory(libcscutils)
SET(LIBS ${LIBS})
SET(INCLUDE_DIR ${INCLUDE_DIR} "${PROJECT_SOURCE_DIR}/libcscutils/include" "${PROJECT_BINARY_DIR}/libcscutils/include")

IF (CBLAS STREQUAL ON)
    SET(FLEXIBLAS_CBLAS TRUE)
ELSE()
    SET(FLEXIBLAS_CBLAS FALSE)
ENDIF()


### Setup Integer8 option
IF ( INTEGER8 STREQUAL ON )
    ADD_DEFINITIONS(-DINTEGER8)
    ADD_DEFINITIONS(-DLAPACK_ILP64)
    SET(FLEXIBLAS_INTEGER8 TRUE)
    SET(FLEXIBLAS_INCLUDE_PREFIX "flexiblas64")
    SET(FLEXIBLAS_LIBRARY_DIR "flexiblas64")
    SET(FLEXIBLAS_PKG_CONFIG "flexiblas64.pc")
    SET(FLEXIBLAS_API_PKG_CONFIG "flexiblas64_api.pc")
    SET(FLEXIBLAS_LIBRARY_NAME "flexiblas64")
    set(flexiblasname flexiblas64)
    SET (FLEXIBLASRC flexiblas64rc)
    SET (FLEXIBLAS_RC "flexiblas64rc")
    SET (FLEXIBLAS_RC_DIR "flexiblas64rc.d")
    SET (FLEXIBLAS_PREFIX "flexiblas64")

    IF ( ${CMAKE_C_COMPILER_ID} STREQUAL "GNU")
        MESSAGE(STATUS "Setting GNU Compiler Flags")
        SET(I8_FLAGS "-fdefault-integer-8 ")
    ELSEIF (${CMAKE_C_COMPILER_ID} STREQUAL "Intel" )
        SET ( I8_FLAGS "-i8 ")
    ELSEIF (${CMAKE_C_COMPILER_ID} STREQUAL "PGI" )
        SET ( I8_FLAGS "-i8 ")
    ENDIF()
    SET (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${I8_FLAGS}")
    SET (FLEXIBLAS_FC_I8FLAG "${I8_FLAGS}")
ELSE()
    SET(FLEXIBLAS_INTEGER8 FALSE)
    SET(FLEXIBLAS_INCLUDE_PREFIX "flexiblas")
    SET(FLEXIBLAS_PKG_CONFIG "flexiblas.pc")
    SET(FLEXIBLAS_API_PKG_CONFIG "flexiblas_api.pc")
    SET(FLEXIBLAS_LIBRARY_DIR "flexiblas")
    SET(FLEXIBLAS_LIBRARY_NAME "flexiblas")
    set(flexiblasname flexiblas)
    SET (FLEXIBLASRC flexiblasrc)
    SET (FLEXIBLAS_RC "flexiblasrc")
    SET (FLEXIBLAS_RC_DIR "flexiblasrc.d")
    SET (FLEXIBLAS_PREFIX "flexiblas")
ENDIF (INTEGER8 STREQUAL ON)
FILE(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/${FLEXIBLAS_RC_DIR}")

IF (DEV STREQUAL ON)
    SET ( CMAKE_INSTALL_FULL_SYSCONFDIR "${CMAKE_BINARY_DIR}")
ENDIF()



# Check for Serveral Functions
IF(WIN32)

    CHECK_FEATURE_NEEDED(_strdup string.h HAVE_STRDUP STRDUP_FEATURE)
    IF( HAVE_STRDUP )
        ADD_DEFINITIONS("${STRDUP_FEATURE}")
    ELSE ()
        MESSAGE(ERROR " _strdup not found")
    ENDIF()

    CHECK_FEATURE_NEEDED(GetFullPathName windows.h HAVE_GETFULLPATHNAME GETFULLPATHNAME_FEATURE)
    IF( HAVE_GETFULLPATHNAME )
        ADD_DEFINITIONS("${GETFULLPATHNAME_FEATURE}")
    ELSE ()
        MESSAGE(ERROR " GetFullPathName not found")
    ENDIF()

ELSE()

    CHECK_FEATURE_NEEDED(strdup string.h HAVE_STRDUP STRDUP_FEATURE)
    IF( HAVE_STRDUP )
        ADD_DEFINITIONS("${STRDUP_FEATURE}")
    ELSE ()
        MESSAGE(FATAL_ERROR "strdup not found")
    ENDIF()
    CHECK_FEATURE_NEEDED(realpath stdlib.h HAVE_REALPATH REALPATH_FEATURE)
    IF( HAVE_REALPATH  )
        ADD_DEFINITIONS("${REALPATH_FEATURE}")
    ELSE ()
        MESSAGE(FATAL_ERROR "realpath not found")
    ENDIF()

ENDIF()

### Add Source

INCLUDE(lapack)

INCLUDE_DIRECTORIES(${PROJECT_SOURCE_DIR}/src)

IF(WIN32)
# place all (link) libraries in the same folder as the dynamic libraries
set(CMAKE_INSTALL_FULL_LIBDIR "${CMAKE_INSTALL_FULL_BINDIR}")
# strip trailing slashes to avoid accidentally escaping final quotes
string(REGEX REPLACE "/$" "" WIN_CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")
string(REGEX REPLACE "/$" "" WIN_CMAKE_INSTALL_FULL_BINDIR "${CMAKE_INSTALL_FULL_BINDIR}")
string(REGEX REPLACE "/$" "" WIN_CMAKE_INSTALL_FULL_LIBDIR "${CMAKE_INSTALL_FULL_LIBDIR}")
string(REGEX REPLACE "/$" "" WIN_CMAKE_INSTALL_FULL_INCLUDEDIR "${CMAKE_INSTALL_FULL_INCLUDEDIR}")
string(REGEX REPLACE "/$" "" WIN_CMAKE_INSTALL_FULL_SYSCONFDIR "${CMAKE_INSTALL_FULL_SYSCONFDIR}")
# # convert linux/cmake style paths (with /) to Windows style paths (\) in the config file
FILE(TO_NATIVE_PATH "${WIN_CMAKE_INSTALL_PREFIX}" WIN_CMAKE_INSTALL_PREFIX)
FILE(TO_NATIVE_PATH "${WIN_CMAKE_INSTALL_FULL_BINDIR}" WIN_CMAKE_INSTALL_FULL_BINDIR)
FILE(TO_NATIVE_PATH "${WIN_CMAKE_INSTALL_FULL_LIBDIR}" WIN_CMAKE_INSTALL_FULL_LIBDIR)
FILE(TO_NATIVE_PATH "${WIN_CMAKE_INSTALL_FULL_INCLUDEDIR}" WIN_CMAKE_INSTALL_FULL_INCLUDEDIR)
FILE(TO_NATIVE_PATH "${WIN_CMAKE_INSTALL_FULL_SYSCONFDIR}" WIN_CMAKE_INSTALL_FULL_SYSCONFDIR)
# escape backslashes (yes this does \ -> \\)
string(REGEX REPLACE \\\\ \\\\\\\\ WIN_CMAKE_INSTALL_PREFIX "${WIN_CMAKE_INSTALL_PREFIX}")
string(REGEX REPLACE \\\\ \\\\\\\\ WIN_CMAKE_INSTALL_FULL_BINDIR "${WIN_CMAKE_INSTALL_FULL_BINDIR}")
string(REGEX REPLACE \\\\ \\\\\\\\ WIN_CMAKE_INSTALL_FULL_LIBDIR "${WIN_CMAKE_INSTALL_FULL_LIBDIR}")
string(REGEX REPLACE \\\\ \\\\\\\\ WIN_CMAKE_INSTALL_FULL_INCLUDEDIR "${WIN_CMAKE_INSTALL_FULL_INCLUDEDIR}")
string(REGEX REPLACE \\\\ \\\\\\\\ WIN_CMAKE_INSTALL_FULL_SYSCONFDIR "${WIN_CMAKE_INSTALL_FULL_SYSCONFDIR}")
ENDIF()

CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/config.h.in ${CMAKE_BINARY_DIR}/include/flexiblas_config.h)



add_subdirectory(src)

IF(EXAMPLES STREQUAL "ON")
    add_subdirectory(examples)
ENDIF()
IF(EXISTS tools/lapack_checks)
    add_subdirectory(tools/lapack_checks)
ENDIF()

### Install
INSTALL(FILES   ${CMAKE_CURRENT_BINARY_DIR}/${FLEXIBLASRC} DESTINATION ${CMAKE_INSTALL_FULL_SYSCONFDIR} PERMISSIONS OWNER_WRITE  OWNER_READ  GROUP_READ WORLD_READ )
INSTALL(FILES   ${CMAKE_SOURCE_DIR}/include/blas_gnu.h
    ${CMAKE_SOURCE_DIR}/include/blas_intel.h
    ${CMAKE_BINARY_DIR}/include/flexiblas_config.h
    ${CMAKE_BINARY_DIR}/include/flexiblas_api.h
    DESTINATION ${CMAKE_INSTALL_FULL_INCLUDEDIR}/${FLEXIBLAS_INCLUDE_PREFIX}/)
IF(LAPACK STREQUAL ON)
    INSTALL(FILES   ${CMAKE_BINARY_DIR}/include/lapack_f77.h
        ${CMAKE_BINARY_DIR}/include/flexiblas_real_lapack.h
        DESTINATION ${CMAKE_INSTALL_FULL_INCLUDEDIR}/${FLEXIBLAS_INCLUDE_PREFIX}/)

ENDIF()
INSTALL(FILES   ${CMAKE_SOURCE_DIR}/src/flexiblas_backend.h
    ${CMAKE_SOURCE_DIR}/src/flexiblas_fortran_mangle.h
    ${CMAKE_SOURCE_DIR}/src/flexiblas_hook_structure.h
    ${CMAKE_SOURCE_DIR}/src/flexiblas_real_calls.h
    ${CMAKE_SOURCE_DIR}/src/flexiblas_real_cblas_calls.h
    ${CMAKE_SOURCE_DIR}/src/flexiblas_dummy_fortran.h
    ${CMAKE_SOURCE_DIR}/src/flexiblas_dummy_cblas.h
    ${CMAKE_SOURCE_DIR}/src/flexiblas_mgmt.h
    ${CMAKE_SOURCE_DIR}/src/flexiblas_fortran_char_len.h
    DESTINATION ${CMAKE_INSTALL_FULL_INCLUDEDIR}/${FLEXIBLAS_INCLUDE_PREFIX}/)

IF(CBLAS STREQUAL ON)
    INSTALL(FILES	${CMAKE_SOURCE_DIR}/include/cblas.h DESTINATION ${CMAKE_INSTALL_FULL_INCLUDEDIR}/${FLEXIBLAS_INCLUDE_PREFIX}/)
ENDIF()

CONFIGURE_FILE(
    "${CMAKE_CURRENT_SOURCE_DIR}/cmake_uninstall.cmake.in"
    "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake"
    IMMEDIATE @ONLY)
ADD_CUSTOM_TARGET(uninstall "${CMAKE_COMMAND}" -P "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake")

### Setup PKG_CONFIG
set(prefix ${CMAKE_INSTALL_PREFIX})
set(libdir ${CMAKE_INSTALL_FULL_LIBDIR})
set(PKG_CONFIG_DIR ${CMAKE_INSTALL_FULL_LIBDIR}/pkgconfig)
set(includedir ${CMAKE_INSTALL_PREFIX}/include/${FLEXIBLAS_INCLUDE_PREFIX}/)
CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/flexiblas.pc.in ${CMAKE_CURRENT_BINARY_DIR}/${FLEXIBLAS_PKG_CONFIG})
CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/flexiblas_api.pc.in ${CMAKE_CURRENT_BINARY_DIR}/${FLEXIBLAS_API_PKG_CONFIG})

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${FLEXIBLAS_PKG_CONFIG}  DESTINATION ${PKG_CONFIG_DIR} )
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${FLEXIBLAS_API_PKG_CONFIG}  DESTINATION ${PKG_CONFIG_DIR} )


### Setup Man Pages
install(DIRECTORY doc/man/ DESTINATION ${CMAKE_INSTALL_FULL_MANDIR} FILES_MATCHING PATTERN "*.[1-9]")
IF (INTEGER8 STREQUAL ON)
    install(FILES doc/man/man1/flexiblas.1 DESTINATION ${CMAKE_INSTALL_FULL_MANDIR}/man1/ RENAME flexiblas64.1)
ENDIF()

IF ( TESTS )
    SET(PYTHON_IGNORE_LIBRARY ON)
    FIND_PACKAGE(PythonInterp)

    ENABLE_TESTING()
    add_subdirectory(test)
ENDIF (TESTS)

#
# Final Status
#
MESSAGE(STATUS "------------------------------------------------")
MESSAGE(STATUS "      Final Configuration Status ")
MESSAGE(STATUS "------------------------------------------------")
MESSAGE(STATUS "Build 8 byte integer: ${INTEGER8}")
MESSAGE(STATUS "Build CBLAS interface: ${CBLAS}")
MESSAGE(STATUS "Install path: ${CMAKE_INSTALL_PREFIX}")
MESSAGE(STATUS "Extra BLAS search path: ${FLEXIBLAS_DEFAULT_LIB_PATH}")
MESSAGE(STATUS "LAPACK Support: ${LAPACK}")
IF(LAPACK)
    MESSAGE(STATUS "LAPACK_API_VERSION: ${LAPACK_API_VERSION}")
ENDIF()
MESSAGE(STATUS "LAPACKE Support: ${LAPACKE}")
MESSAGE(STATUS "ABI: ${ABI}")
MESSAGE(STATUS "Interface to Extra BLAS Libraries:")
FOREACH(EBLAS ${EXTRA_BLAS})
    MESSAGE(STATUS " ${${EBLAS}_BLAS_LIBRARIES} - ${EBLAS} ")
ENDFOREACH()
MESSAGE(STATUS "Default BLAS: ${FLEXIBLAS_DEFAULT}")
MESSAGE(STATUS "")
MESSAGE(STATUS "Compiler Options:")
MESSAGE(STATUS " CMAKE_C_FLAGS               = ${CMAKE_C_FLAGS}")
MESSAGE(STATUS " CMAKE_C_FLAGS_RELEASE       = ${CMAKE_C_FLAGS_RELEASE}")
MESSAGE(STATUS " CMAKE_C_FLAGS_DEBUG         = ${CMAKE_C_FLAGS_DEBUG}")
MESSAGE(STATUS " CMAKE_Fortran_FLAGS         = ${CMAKE_Fortran_FLAGS}")
MESSAGE(STATUS " CMAKE_Fortran_FLAGS_RELEASE = ${CMAKE_Fortran_FLAGS_RELEASE}")
MESSAGE(STATUS " CMAKE_Fortran_FLAGS_DEBUG   = ${CMAKE_Fortran_FLAGS_DEBUG}")
MESSAGE(STATUS "------------------------------------------------")
IF ( DEV STREQUAL ON )
    MESSAGE (WARNING "- This is an development build and can not be install. Please set add -DDEV=OFF to cmake. -")
ENDIF()


