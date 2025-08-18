# Find the CXSPARSE includes and library
#
# This module defines:
#   CXSPARSE_INCLUDE_DIR     -   where to find amd.h
#   CXSPARSE_LIBRARIES       -   libraries to link against to use CXSPARSE
#   CXSPARSE_FOUND           -   If false, do not try to use CXSPARSE.
#
#
#=============================================================================
# Copyright 2010, Martin Koehler, Maximilian Behr
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

INCLUDE(FindPackageHandleStandardArgs)

# search in user given directories
FIND_PATH(CXSPARSE_INCLUDE_DIR   NAMES cs.h          PATHS ${SUITESPARSE} PATH_SUFFIXES include     NO_DEFAULT_PATH)
FIND_LIBRARY(CXSPARSE_LIBRARIES  NAMES cxsparse      PATHS ${SUITESPARSE} PATH_SUFFIXES lib lib64   NO_DEFAULT_PATH)

# search in other directories
FIND_PATH(CXSPARSE_INCLUDE_DIR   NAMES cs.h          PATHS ${_incdir} /usr /opt PATH_SUFFIXES include local/include include/suitesparse local/include/suitesparse)
FIND_LIBRARY(CXSPARSE_LIBRARIES  NAMES cxsparse      PATHS ${_libdir} /usr /opt PATH_SUFFIXES lib lib64 local/lib local/lib64)


FIND_PACKAGE_HANDLE_STANDARD_ARGS(CXSPARSE DEFAULT_MSG CXSPARSE_LIBRARIES CXSPARSE_INCLUDE_DIR)
MARK_AS_ADVANCED(CXSPARSE_INCLUDE_DIR CXSPARSE_LIBRARIES)


