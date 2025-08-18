# Find the AMD includes and library
#
# This module defines:
#   AMD_INCLUDE_DIR     -   where to find amd.h
#   AMD_LIBRARIES       -   libraries to link against to use AMD
#   AMD_FOUND           -   If false, do not try to use AMD.
#
#
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
#    - June 25, 2013   Martin Koehler, add _libdir and _incdir

# GET_INC_LIB_DIR MACRO
INCLUDE(CMakeHelpers)
GET_INC_LIB_DIR(_incdir _libdir)


# search in user given directories
FIND_PATH(AMD_INCLUDE_DIR   NAMES amd.h     PATHS ${SUITESPARSE} PATH_SUFFIXES include      NO_DEFAULT_PATH)
FIND_LIBRARY(AMD_LIBRARIES  NAMES amd       PATHS ${SUITESPARSE} PATH_SUFFIXES lib lib64    NO_DEFAULT_PATH)

# search in other directories
FIND_PATH(AMD_INCLUDE_DIR   NAMES amd.h     PATHS ${_incdir} /usr /opt PATH_SUFFIXES include local/include include/suitesparse local/include/suitesparse)
FIND_LIBRARY(AMD_LIBRARIES  NAMES amd       PATHS ${_libdir} /usr /opt PATH_SUFFIXES lib lib64 local/lib local/lib64)


INCLUDE(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(AMD DEFAULT_MSG AMD_LIBRARIES AMD_INCLUDE_DIR)
MARK_AS_ADVANCED(AMD_INCLUDE_DIR AMD_LIBRARIES)


