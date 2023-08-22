# - Find the UMFPACK includes and library
#
# This module defines
#  UMFPACK_INCLUDE_DIR, where to find umfpack.h, etc.
##  UMFPACK_LIBRARIES, the libraries to link against to use UMFPACK.
#  UMFPACK_FOUND, If false, do not try to use UMFPACK.
# also defined, but not for general use are
#  UMFPACK_LIBRARY, where to find the AMD library.
# None of the above will be defined unless AMD can be found.
# UMFPACK depends on AMD and UFConfig

#=============================================================================
# Copyright 2010, Martin Koehler
#
# Distributed under the OSI-approved BSD License (the "License");
# see accompanying file Copyright.txt for details.
#
# This software is distributed WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the License for more information.
#=============================================================================
# Changelog:
#    - Apr. 1, 2011   Martin Koehler


include(FindPackageHandleStandardArgs)

find_path(MESS_MESS_INCLUDE_DIR mess/mess.h
    /usr/include
    /usr/local/include
    ${MESS}/include   #LOCALSETUP
    )


if(MESS_FIND_QUIETLY)
    set(_FIND_UFCONFIG_ARG QUIET)
    set(_FIND_AMD_ARG QUIET)
    set(_FIND_CHOLMOD_ARG QUIET)
    set(_FIND_COLAMD_ARG QUIET)
    set(_FIND_UMPFACK_ARG QUIET)
    set(_FIND_CSPARSE_ARG QUIET)
    set(_FIND_RRQR_ARG QUIET)
    set(_FIND_BLAS_ARG QUIET)
    set(_FIND_LAPACK_ARG QUIET)
    set(_FIND_X11_ARG QUIET)
    set(_FIND_ZLIB__ARG QUIET)
    set(_FIND_BZ2_ARG QUIET)
    set(_FIND_LUA_ARG QUIET)
endif()

find_package(UFCONFIG   ${_FIND_UFCONFIG_ARG}   REQUIRED)
find_package(AMD        ${_FIND_AMD_ARG}     REQUIRED)
find_package(CHOLMOD    ${_FIND_CHOLMOD_ARG}   REQUIRED)
find_package(COLAMD     ${_FIND_COLAMD_ARG}     REQUIRED)
find_package(UMFPACK    ${_FIND_UMPFACK_ARG}   REQUIRED)
find_package(CSPARSE    ${_FIND_CSPARSE_ARG}   REQUIRED)
find_package(BLAS       ${_FIND_BLAS_ARG}     REQUIRED)
find_package(LAPACK     ${_FIND_LAPACK_ARG}   REQUIRED)
set(MESS_LIBRARIES      ${UMFPACK_LIBRARIES} ${AMD_LIBRARIES} ${COLAMD_LIBRARIES} ${CHOLMOD_LIBRARIES} ${CSPARSE_LIBRARIES})
set(MESS_INCLUDE_DIR    ${UMFPACK_INCLUDE_DIR} ${AMD_INCLUDE_DIR} ${COLAMD_INCLUDE_DIR} ${CHOLMOD_INCLUDE_DIR} ${CSPARSE_INCLUDE_DIR})

find_package(X11 ${_FIND_X11_ARG})
if(X11_FOUND)
    set(MESS_LIBRARIES ${MESS_LIBRARIES} ${X11_LIBRARIES})
    set(MESS_INCLUDE_DIR ${MESS_INCLUDE_DIR} ${X11_INCLUDE_DIR})
endif()

if(X11_Xpm_FOUND)
    set(MESS_LIBRARIES ${MESS_LIBRARIES} ${X11_Xpm_LIB})
    set(MESS_INCLUDE_DIR ${MESS_INCLUDE_DIR} ${X11_Xpm_INCLUDE_PATH})
endif()

find_package(ZLIB ${_FIND_ZLIB__ARG})
if(ZLIB_FOUND)
    set(MESS_LIBRARIES ${MESS_LIBRARIES} ${ZLIB_LIBRARIES})
    set(MESS_INCLUDE_DIR ${MESS_INCLUDE_DIR} ${ZLIB_INCLUDE_DIR})
endif()

find_package(BZip2 ${_FIND_BZ2_ARG})

if(BZIP2_FOUND)
    set(MESS_LIBRARIES ${MESS_LIBRARIES} ${BZIP2_LIBRARIES})
    set(MESS_INCLUDE_DIR ${MESS_INCLUDE_DIR} ${BZIP2_INCLUDE_DIR})
endif()

find_package(Threads REQUIRED)
set(MESS_LIBRARIES ${MESS_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})

set(MESS_LIBRARIES ${MESS_LIBRARIES} ${LAPACK_LIBRARIES}  ${BLAS_LIBRARIES})

find_package(OpenMP)
if(OPENMP_FOUND)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenMP_C_FLAGS}")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
    set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} ${OpenMP_LD_FLAGS}")
    set(OPENMP TRUE)
    set(HAVE_OPENMP TRUE)
endif()


set(MESS_NAMES ${MESS_NAMES} mess libmess)
set(MESS_PATH   /usr/local/lib64 /usr/local/lib /usr/lib64 /usr/lib ${MESS}/lib)
find_library(MESS_LIB NAMES ${MESS_NAMES} PATHS ${MESS_PATH} DOC "Search MESS library")


message(STATUS "MESS include: ${MESS_MESS_INCLUDE_DIR}")
message(STATUS "MESS library: ${MESS_LIB}")

if(MESS_LIB AND MESS_MESS_INCLUDE_DIR)
    set(MESS_INCLUDE_DIR ${MESS_MESS_INCLUDE_DIR} ${MESS_INCLUDE_DIR})
    set(MESS_LIBRARIES ${MESS_LIB} ${MESS_LIBRARIES})
endif()



find_package_handle_standard_args(MESS  DEFAULT_MSG  MESS_LIB MESS_MESS_INCLUDE_DIR)
mark_as_advanced(MESS_MESS_INCLUDE_DIR MESS_LIB)
