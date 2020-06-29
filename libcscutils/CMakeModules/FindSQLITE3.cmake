# Find sqlite.

# This module defines
#  SQLITE3_LIBRARIES    - the libraries to link against to use libsqlite3.
#  SQLITE3_FOUND        - If false, do not try to use libsqlite3.
#  SQLITE3_INCLUDE_DIR  - include directories for libsqlite3.

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

INCLUDE(FindPackageHandleStandardArgs)

FIND_PATH(SQLITE3_INCLUDE_DIR
    NAMES
    sqlite3.h
    PATHS
    /usr/
    /usr/local/
    /opt/
    /opt/local/
    PATH_SUFFIXES
    include
    include/suitesparse
    )


FIND_LIBRARY(SQLITE3_LIBRARIES
    NAMES
    sqlite3
    PATHS
    /usr/
    /usr/
    /usr/local/
    /opt/
    /opt/local/
    PATH_SUFFIXES
    lib/
    lib64
    )


FIND_PACKAGE_HANDLE_STANDARD_ARGS(SQLITE3 DEFAULT_MSG SQLITE3_LIBRARIES SQLITE3_INCLUDE_DIR)
MARK_AS_ADVANCED(SQLITE3_LIBRARIES SQLITE3_INCLUDE_DIR)



