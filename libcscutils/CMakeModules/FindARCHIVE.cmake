# - Find sqlite

# This module defines
#  ARCHIVE_LIBRARIES, the libraries to link against to use libsqlite3.
#  ARCHIVE_FOUND, If false, do not try to use libsqlite3.
#  ARCHIVE_INCLUDE_DIR, include directories for libsqlite3.

#=============================================================================
# Copyright 2014, Martin Koehler
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

FIND_PATH(ARCHIVE_INCLUDE_DIR NAMES archive.h
    PATHS
    ${_incdir}
    /usr/include
    /usr/local/include
    /opt/local/include  #Macports
    )
SET(ARCHIVE_NAMES archive libarchive)
FIND_LIBRARY(ARCHIVE_LIBRARIES NAMES ${ARCHIVE_NAMES} PATHS ${_libdir} /usr/lib /usr/lib32 /usr/lib64)

IF(ARCHIVE_INCLUDE_DIR AND ARCHIVE_LIBRARIES)
    MESSAGE(STATUS "Found ARCHIVE header: ${ARCHIVE_INCLUDE_DIR}")
    MESSAGE(STATUS "Found ARCHIVE library: ${ARCHIVE_LIBRARIES}")
    SET(ARCHIVE_FOUND TRUE)
ELSE()
    SET(ARCHIVE_FOUND FALSE)
ENDIF()

# all listed variables are TRUE
INCLUDE(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(ARCHIVE DEFAULT_MSG ARCHIVE_LIBRARIES ARCHIVE_INCLUDE_DIR)
