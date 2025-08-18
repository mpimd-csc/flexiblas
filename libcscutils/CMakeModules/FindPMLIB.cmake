# - Find the PMLIB includes and library
#
# This module defines
#  PMLIB_INCLUDE_DIR    - where to find pmlib.h, etc.
#  PMLIB_LIBRARIES      - the libraries to link against to use PMLIB.
#  PMLIB_FOUND          - If false, do not try to use PMLIB.

#=============================================================================
# Copyright 2014, Martin Koehler
#
# Distributed under the OSI-approved BSD License (the "License");
# see accompanying file Copyright.txt for details.
#
# This software is distributed WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the License for more information.
#=============================================================================
# Changelog:


INCLUDE(FindPackageHandleStandardArgs)

# GET_INC_LIB_DIR MACRO
INCLUDE(CMakeHelpers)
GET_INC_LIB_DIR(_INCDIR _LIBDIR)


FIND_PATH(PMLIB_INCLUDE_DIR
    NAMES
    pmlib.h
    PATHS
    /usr/
    /usr/local/
    /opt/
    /opt/local/
    ${_INCDIR}
    PATH_SUFFIXES
    include
    )


FIND_LIBRARY(PMLIB_LIBRARIES
    NAMES
    libpmlib
    pmlib
    PATHS
    /usr/
    /usr/
    /usr/local/
    /opt/
    /opt/local/
    ${_LIBDIR}
    PATH_SUFFIXES
    lib/
    lib64
    )


FIND_PACKAGE_HANDLE_STANDARD_ARGS(PMLIB DEFAULT_MSG PMLIB_LIBRARIES PMLIB_INCLUDE_DIR)
MARK_AS_ADVANCED(PMLIB_LIBRARIES PMLIB_INCLUDE_DIR)
