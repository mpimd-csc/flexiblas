# Find the CSPARSE includes and library
#
# This module defines:
#   CSPARSE_INCLUDE_DIR     -   where to find amd.h
#   CSPARSE_LIBRARIES       -   libraries to link against to use CSPARSE
#   CSPARSE_FOUND           -   If false, do not try to use CSPARSE.
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
GET_INC_LIB_DIR(_INCDIR _LIBDIR)

INCLUDE(FindPackageHandleStandardArgs)

# search in user given directories
FIND_PATH(CSPARSE_INCLUDE_DIR   NAMES cs.h          PATHS ${SUITESPARSE} PATH_SUFFIXES include      NO_DEFAULT_PATH)
FIND_LIBRARY(CSPARSE_LIBRARIES  NAMES csparse       PATHS ${SUITESPARSE} PATH_SUFFIXES lib lib64    NO_DEFAULT_PATH)

# search in other directories
FIND_PATH(CSPARSE_INCLUDE_DIR   NAMES cs.h          PATHS ${_incdir} /usr /opt PATH_SUFFIXES include local/include include/suitesparse local/include/suitesparse)
FIND_LIBRARY(CSPARSE_LIBRARIES  NAMES csparse       PATHS ${_libdir} /usr /opt PATH_SUFFIXES lib lib64 local/lib local/lib64)


FIND_PACKAGE_HANDLE_STANDARD_ARGS(CSPARSE DEFAULT_MSG CSPARSE_LIBRARIES CSPARSE_INCLUDE_DIR)
MARK_AS_ADVANCED(CSPARSE_INCLUDE_DIR CSPARSE_LIBRARIES)


