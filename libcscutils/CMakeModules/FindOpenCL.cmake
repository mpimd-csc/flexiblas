#.rst:
# FindOpenCL
# ----------
#
# Try to find OpenCL
#
# Once done this will define::
#
#   OPENCL_FOUND          - True if OpenCL was found
#   OPENCL_INCLUDE_DIRS   - include directories for OpenCL
#   OPENCL_LIBRARIES      - link against this library to use OpenCL
#   OPENCL_VERSION_STRING - Highest supported OpenCL version (eg. 1.2)
#   OPENCL_VERSION_MAJOR  - The major version of the OpenCL implementation
#   OPENCL_VERSION_MINOR  - The minor version of the OpenCL implementation
#
# The module will also define two cache variables::
#
#   OPENCL_INCLUDE_DIR    - the OpenCL include directory
#   OPENCL_LIBRARY        - the path to the OpenCL library
#

#=============================================================================
# Copyright 2014 Matthaeus G. Chajdas
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


include(FindPackageHandleStandardArgs)

function(_FIND_OPENCL_VERSION)
    include(CheckSymbolExists)
    include(CMakePushCheckState)
    set(CMAKE_REQUIRED_QUIET ${OpenCL_FIND_QUIETLY})

    cmake_push_check_state()
    foreach(VERSION "2_0" "1_2" "1_1" "1_0")
        set(CMAKE_REQUIRED_INCLUDES "${OpenCL_INCLUDE_DIR}")

        if(APPLE)
            check_symbol_exists(
                CL_VERSION_${VERSION}
                "${OpenCL_INCLUDE_DIR}/OpenCL/cl.h"
                OPENCL_VERSION_${VERSION})
        else()
            check_symbol_exists(
                CL_VERSION_${VERSION}
                "${OpenCL_INCLUDE_DIR}/CL/cl.h"
                OPENCL_VERSION_${VERSION})
        endif()

        if(OPENCL_VERSION_${VERSION})
            string(REPLACE "_" "." VERSION "${VERSION}")
            set(OpenCL_VERSION_STRING ${VERSION} PARENT_SCOPE)
            string(REGEX MATCHALL "[0-9]+" version_components "${VERSION}")
            list(GET version_components 0 major_version)
            list(GET version_components 1 minor_version)
            set(OpenCL_VERSION_MAJOR ${major_version} PARENT_SCOPE)
            set(OpenCL_VERSION_MINOR ${minor_version} PARENT_SCOPE)
            break()
        endif()
    endforeach()
    cmake_pop_check_state()
endfunction()

find_path(OpenCL_INCLUDE_DIR
    NAMES
    CL/cl.h OpenCL/cl.h
    PATHS
    ENV "PROGRAMFILES(X86)"
    ENV AMDAPPSDKROOT
    ENV INTELOCLSDKROOT
    ENV NVSDKCOMPUTE_ROOT
    ENV CUDA_PATH
    ENV ATISTREAMSDKROOT
    PATH_SUFFIXES
    include
    OpenCL/common/inc
    "AMD APP/include")

_find_opencl_version()

if(WIN32)
    if(CMAKE_SIZEOF_VOID_P EQUAL 4)
        find_library(OpenCL_LIBRARY
            NAMES OpenCL
            PATHS
            ENV "PROGRAMFILES(X86)"
            ENV AMDAPPSDKROOT
            ENV INTELOCLSDKROOT
            ENV CUDA_PATH
            ENV NVSDKCOMPUTE_ROOT
            ENV ATISTREAMSDKROOT
            PATH_SUFFIXES
            "AMD APP/lib/x86"
            lib/x86
            lib/Win32
            OpenCL/common/lib/Win32)
    elseif(CMAKE_SIZEOF_VOID_P EQUAL 8)
        find_library(OpenCL_LIBRARY
            NAMES OpenCL
            PATHS
            ENV "PROGRAMFILES(X86)"
            ENV AMDAPPSDKROOT
            ENV INTELOCLSDKROOT
            ENV CUDA_PATH
            ENV NVSDKCOMPUTE_ROOT
            ENV ATISTREAMSDKROOT
            PATH_SUFFIXES
            "AMD APP/lib/x86_64"
            lib/x86_64
            lib/x64
            OpenCL/common/lib/x64)
    endif()
else()
    find_library(OpenCL_LIBRARY
        NAMES OpenCL)
endif()

set(OpenCL_LIBRARIES ${OpenCL_LIBRARY})
set(OpenCL_INCLUDE_DIRS ${OpenCL_INCLUDE_DIR})

find_package_handle_standard_args(
    OPENCL
    FOUND_VAR OpenCL_FOUND
    REQUIRED_VARS OpenCL_LIBRARY OpenCL_INCLUDE_DIR
    VERSION_VAR OpenCL_VERSION_STRING)

mark_as_advanced(OpenCL_INCLUDE_DIR  OpenCL_LIBRARY)
