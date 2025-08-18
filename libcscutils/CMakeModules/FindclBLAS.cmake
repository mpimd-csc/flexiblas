#.rst:
# FindclBLAS
# ----------
#
# Try to find clBLAS
#
# Once done this will define::
#
#   clBLAS_FOUND          - True if clBLAS was found
#   clBLAS_INCLUDE_DIRS   - include directories for clBLAS
#   clBLAS_LIBRARIES      - link against this library to use clBLAS
#   clBLAS_VERSION_STRING - clBLAS version as string (e.g. "2.10.0")
#   clBLAS_VERSION_MAJOR  - The major version of the clBLAS implementation
#   clBLAS_VERSION_MINOR  - The minor version of the clBLAS implementation
#   clBLAS_VERSION_PATCH  - The patch version of the clBLAS implementation
#
# The module will also define two cache variables::
#
#   clBLAS_INCLUDE_DIR    - the clBLAS include directory
#   clBLAS_LIBRARY        - the path to the clBLAS library
#

#=============================================================================
# Copyright 2015 Martin Koehler
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

FIND_PACKAGE(OpenCL REQUIRED)

INCLUDE(CheckSymbolExists)
INCLUDE(CMakePushCheckState)
INCLUDE(FindPackageHandleStandardArgs)

FUNCTION(_FIND_CLBLAS_VERSION)

    # CMAKE_PUSH_CHECK_STATE()

    FILE(WRITE "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/clblas_version.c"
        "#include <stdio.h>
        #include <clBLAS.h>
        int main() {
        cl_uint major, minor,patch;
        clblasGetVersion(&major, &minor, &patch);
        printf(\"%u.%u.%u\", major, minor, patch);
        return 0; }\n")
        TRY_RUN(VERSION_EXITCODE VERSION_COMPILED
            ${CMAKE_BINARY_DIR}
            ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/clblas_version.c
            LINK_LIBRARIES ${clBLAS_LIBRARIES}
            CMAKE_FLAGS
            -DCMAKE_SKIP_RPATH:BOOL=${CMAKE_SKIP_RPATH}
            "-DINCLUDE_DIRECTORIES:STRING=${clBLAS_INCLUDE_DIRS}"
            COMPILE_OUTPUT_VARIABLE OUTPUT
            RUN_OUTPUT_VARIABLE RUN_OUTPUT)

        IF(VERSION_COMPILED)
            SET(clBLAS_VERSION_STRING "${RUN_OUTPUT}" PARENT_SCOPE)
            STRING(REGEX MATCHALL "[0-9]+" version_components "${RUN_OUTPUT}")
            LIST(GET version_components 0 major_version)
            LIST(GET version_components 1 minor_version)
            LIST(GET version_components 2 patch_version)
            SET(clBLAS_VERSION_MAJOR ${major_version} PARENT_SCOPE)
            SET(clBLAS_VERSION_MINOR ${minor_version} PARENT_SCOPE)
            SET(clBLAS_VERSION_PATCH ${patch_version} PARENT_SCOPE)
        ELSE()
            SET(clBLAS_VERSION_STRING "Unknown" PARENT_SCOPE)
            SET(clBLAS_VERSION_MAJOR 0 PARENT_SCOPE)
            SET(clBLAS_VERSION_MINOR 0 PARENT_SCOPE)
            SET(clBLAS_VERSION_PATCH 0 PARENT_SCOPE)
        ENDIF()

    ENDFUNCTION()

    IF(NOT _incdir)
        IF(WIN32)
            SET(_incdir ENV INCLUDE)
        ELSEIF(APPLE)
            SET(_incdir ENV INCLUDE CPATH)
        ELSE()
            SET(_incdir ENV INCLUDE CPATH)
        ENDIF()
    ENDIF()

    IF(NOT _libdir)
        IF(WIN32)
            SET(_libdir ENV LIB)
        ELSEIF(APPLE)
            SET(_libdir ENV DYLD_LIBRARY_PATH)
        ELSE()
            SET(_libdir ENV LD_LIBRARY_PATH)
        ENDIF()
    ENDIF()

    FIND_PATH(clBLAS_INCLUDE_DIR
        NAMES
        clBLAS.h
        PATHS
        /usr/include
        /usr/local/include
        /opt/local/include  #Macports
        )
    # _FIND_OPENCL_VERSION()

    FIND_LIBRARY(clBLAS_LIBRARY
        NAMES clBLAS
        PATHS
        /usr/lib
        /usr/lib32
        /usr/lib64
        )

    SET(clBLAS_LIBRARIES ${clBLAS_LIBRARY} ${OpenCL_LIBRARIES})
    SET(clBLAS_INCLUDE_DIRS ${clBLAS_INCLUDE_DIR} ${OpenCL_INCLUDE_DIR})

    _FIND_CLBLAS_VERSION()

    FIND_PACKAGE_HANDLE_STANDARD_ARGS(CLBLAS FOUND_VAR clBLAS_FOUND REQUIRED_VARS clBLAS_LIBRARY clBLAS_INCLUDE_DIR VERSION_VAR clBLAS_VERSION_STRING)

    MARK_AS_ADVANCED(clBLAS_INCLUDE_DIR clBLAS_LIBRARY)
