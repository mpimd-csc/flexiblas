# - Find BLAS64 library
# This module finds an installed fortran library that implements the BLAS64
# linear-algebra interface (see http://www.netlib.org/blas/).
# The list of libraries searched for is taken
# from the autoconf macro file, acx_blas.m4 (distributed at
# http://ac-archive.sourceforge.net/ac-archive/acx_blas.html).
#
# This module sets the following variables:
#  BLAS64_FOUND - set to true if a library implementing the BLAS64 interface
#    is found
#  BLAS64_LINKER_FLAGS - uncached list of required linker flags (excluding -l
#    and -L).
#  BLAS64_LIBRARIES - uncached list of libraries (using full path name) to
#    link against to use BLAS64
#  BLA_STATIC  if set on this determines what kind of linkage we do (static)
#  BLA64_VENDOR  if set checks only the specified vendor, if not set checks
#     all the possibilities
##########
# List of vendors (BLA64_VENDOR) valid in this module:
# - OpenBLAS64
# - OpenBLAS64-PThread
# - OpenBLAS64-OpenMP
# - Intel (search a default one)
# - Intel_64ilp   (Intel64, parallel, 4Byte Int, 8 Byte Long+Pointer)
# - Intel_64ilp_seq (Intel64, sequential)
# - ACML
# - ACML_MP
# - Generic
# C/CXX should be enabled to use Intel mkl
#########
# In order to use the ACML two additional CMAKE Variables can be set
#  - ACML_BASE sets the base directory where cmake should search for ACML
#              directories. Per default this will be "/opt"
#  - ACML_ROOT If you want to search for a special ACML version you can set
#              the absolute path to your ACML installation. If this variable is
#              set not other paths except of LD_LIBRARY_PATH will be searched.
#
#=============================================================================
# Copyright 2007-2009 Kitware, Inc.
# Copyright 2013 Martin Köhler, MPI Magdeburg
#
# Distributed under the OSI-approved BSD License (the "License");
# see accompanying file Copyright.txt for details.
#
# This software is distributed WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the License for more information.
#=============================================================================
# (To distribute this file outside of CMake, substitute the full
#  License text for the above reference.)

# lint_cmake: -package/consistency


include(CheckFunctionExists)
include(CheckFortranFunctionExists)

set(_blas_ORIG_CMAKE_FIND_LIBRARY_SUFFIXES ${CMAKE_FIND_LIBRARY_SUFFIXES})

# Check the language being used
get_property( _LANGUAGES_ GLOBAL PROPERTY ENABLED_LANGUAGES )
if(_LANGUAGES_ MATCHES Fortran)
    set( _CHECK_FORTRAN TRUE )
elseif((_LANGUAGES_ MATCHES C) OR (_LANGUAGES_ MATCHES CXX))
    set( _CHECK_FORTRAN FALSE )
else()
    if(BLAS64_FIND_REQUIRED)
        message(FATAL_ERROR "FindBLAS64 requires Fortran, C, or C++ to be enabled.")
    else()
        message(STATUS "Looking for BLAS64... - NOT found (Unsupported languages)")
        return()
    endif()
endif()

macro(Check_Fortran_Libraries LIBRARIES _prefix _name _flags _list _thread)
    # This macro checks for the existence of the combination of fortran libraries
    # given by _list.  If the combination is found, this macro checks (using the
    # Check_Fortran_Function_Exists macro) whether can link against that library
    # combination using the name of a routine given by _name using the linker
    # flags given by _flags.  If the combination of libraries is found and passes
    # the link test, LIBRARIES is set to the list of complete library paths that
    # have been found.  Otherwise, LIBRARIES is set to FALSE.

    # N.B. _prefix is the prefix applied to the names of all cached variables that
    # are generated internally and marked advanced by this macro.

    unset(_libdir)

    set(_libraries_work TRUE)
    set(${LIBRARIES})
    set(_combined_name)
    if(NOT _libdir)
        if(WIN32)
            set(_libdir ENV LIB ${ARGN})
        elseif(APPLE)
            set(_libdir ENV DYLD_LIBRARY_PATH ${ARGN})
        else()
            set(_libdir ENV LD_LIBRARY_PATH ${ARGN})
        endif()
    endif()
    foreach(_library ${_list})
        set(_combined_name ${_combined_name}_${_library})
        if(_libraries_work)
            if(BLA_STATIC)
                if(WIN32)
                    set(CMAKE_FIND_LIBRARY_SUFFIXES .lib ${CMAKE_FIND_LIBRARY_SUFFIXES})
                endif()
                if(APPLE)
                    set(CMAKE_FIND_LIBRARY_SUFFIXES .lib ${CMAKE_FIND_LIBRARY_SUFFIXES})
                else()
                    set(CMAKE_FIND_LIBRARY_SUFFIXES .a ${CMAKE_FIND_LIBRARY_SUFFIXES})
                endif()
            else()
                if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
                    # for ubuntu's libblas3gf and liblapack3gf packages
                    set(CMAKE_FIND_LIBRARY_SUFFIXES ${CMAKE_FIND_LIBRARY_SUFFIXES} .so.3gf .so.0)
                endif()
            endif()
            find_library(${_prefix}_${_library}_LIBRARY
                NAMES ${_library}
                HINTS ${_libdir}
                PATHS ${_libdir}
                NO_DEFAULT_PATH
                )
            find_library(${_prefix}_${_library}_LIBRARY
                NAMES ${_library}
                HINTS ${_libdir}
                PATHS ${_libdir}
                )
            mark_as_advanced(${_prefix}_${_library}_LIBRARY)
            set(${LIBRARIES} ${${LIBRARIES}} ${${_prefix}_${_library}_LIBRARY})
            set(_libraries_work ${${_prefix}_${_library}_LIBRARY})
        endif()
    endforeach()
    if(_libraries_work)
        # Test this combination of libraries.
        set(CMAKE_REQUIRED_LIBRARIES ${_flags} ${${LIBRARIES}} ${_thread})
        #  message("DEBUG: CMAKE_REQUIRED_LIBRARIES = ${CMAKE_REQUIRED_LIBRARIES}")
        if(_CHECK_FORTRAN)
            check_fortran_function_exists("${_name}" ${_prefix}${_combined_name}_WORKS)
        else()
            check_function_exists("${_name}_" ${_prefix}${_combined_name}_WORKS)
        endif()
        set(CMAKE_REQUIRED_LIBRARIES)
        mark_as_advanced(${_prefix}${_combined_name}_WORKS)
        set(_libraries_work ${${_prefix}${_combined_name}_WORKS})
    endif()
    if(NOT _libraries_work)
        set(${LIBRARIES} FALSE)
    endif()
    #message("DEBUG: ${LIBRARIES} = ${${LIBRARIES}}")
endmacro()


#
# PREPARATION
#
set(BLAS64_LINKER_FLAGS)
set(BLAS64_LIBRARIES)
if($ENV{BLA64_VENDOR} MATCHES ".+")
    set(BLA64_VENDOR $ENV{BLA64_VENDOR})
else()
    if(NOT BLA64_VENDOR)
        set(BLA64_VENDOR "All")
    endif()
endif()

# Check for a generic BLAS64 first. This enables us to use the system wide
# default setting on systems which provide the BLAS64 implementation via
# the updates-alternatives system or similar techniques.
if(BLA64_VENDOR STREQUAL "Generic" OR BLA64_VENDOR STREQUAL "All")
    if(NOT BLAS64_LIBRARIES)
        check_fortran_libraries(
            BLAS64_LIBRARIES
            BLAS64
            sgemm
            ""
            "blas64"
            ""
            )
    endif()
    if(BLAS64_LIBRARIES)
        set(BLA64_VENDOR "Generic")
    endif()
endif()

#
# Search for the FlexiBLAS64
#
if(BLA64_VENDOR STREQUAL "FlexiBLAS64" OR BLA64_VENDOR STREQUAL "All")
    if(NOT BLAS64_LIBRARIES)
        check_fortran_libraries(
            BLAS64_LIBRARIES
            BLAS64
            sgemm
            ""
            "flexiblas64"
            ""
            )
    endif()
    if(BLAS64_LIBRARIES)
        set(BLA64_VENDOR "FlexiBLAS64")
    endif()
endif()

# BLAS64 in IBM ESSL library? (requires generic BLAS64 lib, too)
if(BLA64_VENDOR STREQUAL "IBMESSL" OR BLA64_VENDOR STREQUAL "ESSL" OR BLA64_VENDOR STREQUAL "All")
    if(NOT BLAS64_LIBRARIES)
        set(BLAS64_SEARCH_LIBS "")

        list(APPEND BLAS64_SEARCH_LIBS "esslsmp6464")
        list(APPEND BLAS64_SEARCH_LIBS "essl6464")

        foreach(IT ${BLAS64_SEARCH_LIBS})
            string(REPLACE " " ";" SEARCH_LIBS ${IT})
            if(BLAS64_LIBRARIES)
            else()
                check_fortran_libraries(
                    BLAS64_LIBRARIES
                    BLAS64
                    sgemm
                    ""
                    "${SEARCH_LIBS}"
                    ""
                    )
            endif()
        endforeach()
        if(BLAS64_LIBRARIES)
            set(BLA64_VENDOR "IBMESSL")
            set(BLAS64_LIBRARIES "${BLAS64_LIBRARIES}")
        endif()
    endif()

    if(NOT BLAS64_LIBRARIES)
        check_fortran_libraries(
            BLAS64_LIBRARIES
            BLAS64
            sgemm
            ""
            "essl;blas"
            ""
            )
    endif()
    if(BLAS64_LIBRARIES)
        set(BLA64_VENDOR "IBMESSL")
    endif()

endif()

#
# Find OpenBLAS64
#
if(BLA64_VENDOR MATCHES "OpenBLAS64.*" OR BLA64_VENDOR STREQUAL "All")
    if(NOT BLAS64_LIBRARIES)
        set(BLAS64_SEARCH_LIBS "")
        find_package(OpenMP)
        find_package(Threads)
        if(BLA64_VENDOR STREQUAL "OpenBLAS64")
            list(APPEND BLAS64_SEARCH_LIBS "openblas64")
            set(XVENDOR "OpenBLAS64")
        endif()
        if(BLA64_VENDOR MATCHES "OpenBLAS64-P.*")
            list(APPEND BLAS64_SEARCH_LIBS "openblasp64")
            list(APPEND BLAS64_SEARCH_LIBS "openblas64p")
            set(XVENDOR "OpenBLAS64-Pthreads")
        endif()
        if(BLA64_VENDOR STREQUAL "OpenBLAS64-OpenMP" OR BLA64_VENDOR STREQUAL "OpenBLAS64-OpenMP")
            list(APPEND BLAS64_SEARCH_LIBS "openblaso64")
            list(APPEND BLAS64_SEARCH_LIBS "openblas64o")
            set(XVENDOR "OpenBLAS64-OpenMP")
        endif()

        foreach(IT ${BLAS64_SEARCH_LIBS})
            string(REPLACE " " ";" SEARCH_LIBS ${IT})
            if(NOT BLAS64_LIBRARIES)
                check_fortran_libraries(
                    BLAS64_LIBRARIES
                    BLAS64
                    sgemm
                    ""
                    "${SEARCH_LIBS}"
                    "${OpenMP_C_LIBRARIES};${CMAKE_THREAD_LIBS_INIT};${LM}"
                    )
            endif()
        endforeach()

        if(BLAS64_LIBRARIES)
            set(BLA64_VENDOR ${XVENDOR})
        endif()
    endif()
endif()


#BLAS64 in acml library?
if(BLA64_VENDOR MATCHES "ACML.*" OR BLA64_VENDOR STREQUAL "All")
    if(((BLA64_VENDOR STREQUAL "ACML") AND (NOT BLAS64_ACML_LIB_DIRS)) OR
        ((BLA64_VENDOR STREQUAL "ACML_MP") AND (NOT BLAS64_ACML_MP_LIB_DIRS)) OR
        ((BLA64_VENDOR STREQUAL "ACML_GPU") AND (NOT BLAS64_ACML_GPU_LIB_DIRS))
        )
    # try to find acml in "standard" paths
    if(NOT DEFINED ACML_ROOT)
        if(DEFINED ACML_BASE)
            set(_ACML_SEARCH_BASE ${ACML_BASE})
        else()
            set(_ACML_SEARCH_BASE "/opt")
        endif()
        if(WIN32)
            file(GLOB _ACML_ROOT "C:/AMD/acml*/ACML-EULA.txt")
        else()
            file(GLOB _ACML_ROOT "${_ACML_SEARCH_BASE}/acml*/ACML-EULA.txt")
        endif()
        message(STATUS "ACML Roots: ${_ACML_ROOT}")
        if(_ACML_ROOT)
            list(GET _ACML_ROOT 0 _ACML_ROOT)
            get_filename_component(_ACML_ROOT ${_ACML_ROOT} PATH)
        endif()
    else()
        set(_ACML_ROOT ${ACML_ROOT})
    endif()

    if(_ACML_ROOT)
        set( _ACML_PATH_SUFFIX "_int64" )

        if( CMAKE_Fortran_COMPILER_ID STREQUAL "Intel" )
            set( _ACML_COMPILER32 "ifort32" )
            set( _ACML_COMPILER64 "ifort64" )
        elseif( CMAKE_Fortran_COMPILER_ID STREQUAL "SunPro" )
            set( _ACML_COMPILER32 "sun32" )
            set( _ACML_COMPILER64 "sun64" )
        elseif( CMAKE_Fortran_COMPILER_ID STREQUAL "PGI" )
            set( _ACML_COMPILER32 "pgi32" )
            if( WIN32 )
                set( _ACML_COMPILER64 "win64" )
            else()
                set( _ACML_COMPILER64 "pgi64" )
            endif()
        elseif( CMAKE_Fortran_COMPILER_ID STREQUAL "Open64" )
            # 32 bit builds not supported on Open64 but for code simplicity
            # We'll just use the same directory twice
            set( _ACML_COMPILER32 "open64_64" )
            set( _ACML_COMPILER64 "open64_64" )
        elseif( CMAKE_Fortran_COMPILER_ID STREQUAL "NAG" )
            set( _ACML_COMPILER32 "nag32" )
            set( _ACML_COMPILER64 "nag64" )
        else()
            set( _ACML_COMPILER32 "gfortran32" )
            set( _ACML_COMPILER64 "gfortran64" )
        endif()

        if(BLA64_VENDOR STREQUAL "ACML_MP")
            set(_ACML_MP_LIB_DIRS
                "${_ACML_ROOT}/${_ACML_COMPILER32}_mp${_ACML_PATH_SUFFIX}/lib"
                "${_ACML_ROOT}/${_ACML_COMPILER64}_mp${_ACML_PATH_SUFFIX}/lib")
        else()
            set(_ACML_LIB_DIRS
                "${_ACML_ROOT}/${_ACML_COMPILER32}${_ACML_PATH_SUFFIX}/lib"
                "${_ACML_ROOT}/${_ACML_COMPILER64}${_ACML_PATH_SUFFIX}/lib")
        endif()
    endif()
elseif(BLAS64_${BLA64_VENDOR}_LIB_DIRS)
    set(_${BLA64_VENDOR}_LIB_DIRS ${BLAS64_${BLA64_VENDOR}_LIB_DIRS})
endif()

if( BLA64_VENDOR STREQUAL "ACML_MP" )
    foreach(BLAS64_ACML_MP_LIB_DIRS ${_ACML_MP_LIB_DIRS})
        set(__CMAKE_FIND_LIBRARY_SUFFIXES ${CMAKE_FIND_LIBRARY_SUFFIXES})
        set(CMAKE_FIND_LIBRARY_SUFFIXES ".a")

        check_fortran_libraries(
            BLAS64_LIBRARIES
            BLAS64
            sgemm
            ""
            "acml_mp"
            "-lgomp"
            ${BLAS64_ACML_MP_LIB_DIRS}
            )
        set(CMAKE_FIND_LIBRARY_SUFFIXES ${__CMAKE_FIND_LIBRARY_SUFFIXES})

        if( BLAS64_LIBRARIES )
            set(BLAS64_LIBRARIES "${BLAS64_LIBRARIES};-lgomp")
            break()
        endif()

        check_fortran_libraries(
            BLAS64_LIBRARIES
            BLAS64
            sgemm
            ""
            "acml_mp"
            "-lgomp"
            ${BLAS64_ACML_MP_LIB_DIRS}
            )

        if( BLAS64_LIBRARIES )
            set(BLAS64_LIBRARIES "${BLAS64_LIBRARIES};-lgomp")
            break()
        endif()

    endforeach()
else()
    foreach(BLAS64_ACML_LIB_DIRS ${_ACML_LIB_DIRS})
        set(__CMAKE_FIND_LIBRARY_SUFFIXES ${CMAKE_FIND_LIBRARY_SUFFIXES})
        set(CMAKE_FIND_LIBRARY_SUFFIXES ".a")
        check_fortran_libraries(
            BLAS64_LIBRARIES
            BLAS64
            sgemm
            ""
            "acml;acml_mv;"
            ""
            ${BLAS64_ACML_LIB_DIRS}
            )
        set(CMAKE_FIND_LIBRARY_SUFFIXES ${__CMAKE_FIND_LIBRARY_SUFFIXES})
        if(BLAS64_LIBRARIES)
            break()
        endif()

        check_fortran_libraries(
            BLAS64_LIBRARIES
            BLAS64
            sgemm
            ""
            "acml"
            ""
            ${BLAS64_ACML_LIB_DIRS}
            )
        if( BLAS64_LIBRARIES )
            break()
        endif()
    endforeach()
endif()

# Either acml or acml_mp should be in LD_LIBRARY_PATH but not both
if(NOT BLAS64_LIBRARIES)
    check_fortran_libraries(
        BLAS64_LIBRARIES
        BLAS64
        sgemm
        ""
        "acml_mp;acml_mv"
        ""
        )
endif()

if(NOT BLAS64_LIBRARIES)
    check_fortran_libraries(
        BLAS64_LIBRARIES
        BLAS64
        sgemm
        ""
        "acml;acml_mv"
        ""
        )
endif()
if(BLAS64_LIBRARIES)
    set(BLA64_VENDOR "ACML")
endif()
endif() # ACML

#
# BLAS64 in Intel MKL
#
if(BLA64_VENDOR MATCHES "Intel*" OR BLA64_VENDOR STREQUAL "All")
    if(NOT WIN32)
        set(LM "-lm")
    endif()
    if(NOT (_LANGUAGES_ MATCHES C OR _LANGUAGES_ MATCHES CXX))
        return()
    endif()

    find_package(Threads REQUIRED)
    find_package(OpenMP)

    set(BLAS64_SEARCH_LIBS "")

    set(BLAS64_mkl_SEARCH_SYMBOL sgemm)
    set(_LIBRARIES BLAS64_LIBRARIES)
    if(WIN32)
        list(APPEND BLAS64_SEARCH_LIBS "mkl_intel_c mkl_intel_thread mkl_core libguide40")
    else() #WIN32
        if(BLA64_VENDOR STREQUAL "Intel_64lp" OR BLA64_VENDOR STREQUAL "All")
            # old version
            list(APPEND BLAS64_SEARCH_LIBS "mkl_intel_ilp64 mkl_intel_thread mkl_core guide")
            # mkl >= 10.3
            if(CMAKE_C_COMPILER_ID MATCHES ".*GNU.*")
                list(APPEND BLAS64_SEARCH_LIBS "mkl_gf_ilp64 mkl_intel_thread mkl_core pthread iomp5 dl m")
                list(APPEND BLAS64_SEARCH_LIBS "mkl_gf_ilp64 mkl_gnu_thread mkl_core pthread gomp dl m")
            else()
                list(APPEND BLAS64_SEARCH_LIBS "mkl_intel_ilp64 mkl_intel_thread mkl_core iomp5 dl")
            endif()
        endif() #Intel64_lp


        if(BLA64_VENDOR STREQUAL "Intel_64lp_seq" OR BLA64_VENDOR STREQUAL "All")
            # mkl >= 10.3
            if(CMAKE_C_COMPILER_ID MATCHES ".*GNU.*")
                list(APPEND BLAS64_SEARCH_LIBS "mkl_gf_ilp64 mkl_sequential mkl_core pthread dl m")
            else()
                list(APPEND BLAS64_SEARCH_LIBS "mkl_intel_ilp64 mkl_sequential mkl_core dl m")
            endif()
        endif() # Intel64_lp_seq
    endif() # Win32

    foreach(IT ${BLAS64_SEARCH_LIBS})
        string(REPLACE " " ";" SEARCH_LIBS ${IT})
        if(${_LIBRARIES})
        else()
            check_fortran_libraries(
                ${_LIBRARIES}
                BLAS64
                ${BLAS64_mkl_SEARCH_SYMBOL}
                "${OpenMP_LD_FLAGS}"
                "${SEARCH_LIBS}"
                "${OpenMP_C_FLAGS};${CMAKE_THREAD_LIBS_INIT};${LM}"
                )
        endif()
    endforeach()
    if(BLAS64_LIBRARIES)
        set(BLA64_VENDOR "Intel")
        set(BLAS64_LIBRARIES "${BLAS64_LIBRARIES}" ${OPENMP_C_FLAGS} ${CMAKE_THREAD_LIBS_INIT} ${LM})
        set(BLAS64_LINKER_FLAGS "${OpenMP_LD_FLAGS}")
    endif()
endif()


### Check 64bit
if(BLAS64_LIBRARIES)
    if( BLA64_VENDOR MATCHES ".*ESSL.*" )
        set(BLAS64_INTERFACE64 TRUE)
    else()
        set(CHECK_FUNCTION_EXISTS_ADD_LIBRARIES "-DLINK_LIBRARIES:STRING=${BLAS64_LIBRARIES}")
        file(WRITE
            ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/testBLAS64.f90
            "
            program main
            integer*8 n,nn(3)
            real s,a(1),b(1),sdot
            a(1) = 1.0
            b(1) = 1.0
            ! Generate -2**33 + 1, if possible
            n = 2
            n = -4 * (n ** 30)
            write(*,*) n

            n = n + 1
            write(*,*) n
            if (n >= 0) stop 1
            ! This means we're on 64-bit integers. Check whether the BLAS64 is, too.
            s = sdot(n,a,1,b,1)
            write(*,*) 'S1 = ', s
            if (s .ne. 0.0) stop 1
            end
            "
            )
                try_run(RUN_RESULT COMPILE_RESULT
                    ${CMAKE_BINARY_DIR}
                    ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/testBLAS64.f90
                    CMAKE_FLAGS "${CHECK_FUNCTION_EXISTS_ADD_LIBRARIES}")
                message(STATUS "Check for BLAS64 interface: ${RUN_RESULT} ${COMPILE_RESULT}")
                if(RUN_RESULT EQUAL 0)
                    message(STATUS "BLAS64 Library ${BLA64_VENDOR} works with 64bit integers")
                    set(BLAS64_INTERFACE64 TRUE)
                else()
                    message(STATUS "BLAS64 Library ${BLA64_VENDOR} does not work with 64bit integers")
                    set(BLAS64_INTERFACE64 FALSE)
                endif()

            endif()
        else()
            return()
        endif()


        if(BLAS64_LIBRARIES AND BLAS64_INTERFACE64)
            set(BLAS64_FOUND TRUE)
            set(BLAS64_LIBRARIES ${BLAS64_LIBRARIES} CACHE STRING "BLAS64 LIBRARIES")
            set(BLAS64_LINKER_FLAGS ${BLAS64_LINKER_FLAGS} CACHE STRING "BLAS64 Linker Flags")
            set(BLAS64_FOUND     ${BLAS64_FOUND}     CACHE BOOL "Found BLAS64 Libary")
            set(BLA64_VENDOR     ${BLA64_VENDOR}     CACHE STRING "BLAS64 Vendor")
        else()
            set(BLAS64_FOUND FALSE)
        endif()

        if(NOT BLAS64_FIND_QUIETLY)
            if(BLAS64_FOUND)
                message(STATUS "Found BLAS64: ${BLA64_VENDOR} (${BLAS64_LIBRARIES})")
            else()
                if(BLAS64_FIND_REQUIRED)
                    message(FATAL_ERROR "A required library with BLAS64 API not found. Please specify library location.")
                else()
                    message(STATUS   "A library with BLAS64 API not found. Please specify library location.")
                endif()
            endif()
        endif()


        set(CMAKE_FIND_LIBRARY_SUFFIXES ${_blas_ORIG_CMAKE_FIND_LIBRARY_SUFFIXES})
