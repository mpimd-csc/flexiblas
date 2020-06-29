# - Find ZSTD

# This module defines
#  ZSTD_LIBRARIES, the libraries to link against to use libzstd.
#  ZSTD_FOUND, If false, do not try to use libzstd.
#  ZSTD_INCLUDE_DIR, include directories for libzstd.

#=============================================================================
# Copyright 2019, Martin Koehler
#
#
# Distributed under the OSI-approved BSD License (the "License");
# see accompanying file Copyright.txt for details.
#
# This software is distributed WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the License for more information.
#=============================================================================
INCLUDE(CheckFunctionExists)

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

FIND_PATH(ZSTD_INCLUDE_DIR NAMES zstd.h
    PATHS
    ${_incdir}
    /usr/include
    /usr/local/include
    /opt/local/include  #Macports
    )
SET(ZSTD_NAMES zstd libzstd)
FIND_LIBRARY(ZSTD_LIBRARIES
    NAMES ${ZSTD_NAMES}
    PATHS ${_libdir}
        /usr/lib
        /usr/lib32
        /usr/lib64)

IF(ZSTD_INCLUDE_DIR AND ZSTD_LIBRARIES)
    MESSAGE(STATUS "Found ZSTD header: ${ZSTD_INCLUDE_DIR}")
    MESSAGE(STATUS "Found ZSTD library: ${ZSTD_LIBRARIES}")
    SET(ZSTD_FOUND TRUE)
ELSE()
    SET(ZSTD_FOUND FALSE)
ENDIF()

# all listed variables are TRUE
INCLUDE(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(ZSTD DEFAULT_MSG ZSTD_LIBRARIES ZSTD_INCLUDE_DIR)
