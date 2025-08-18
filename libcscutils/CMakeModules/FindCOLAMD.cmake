# Find the COLAMD includes and library
#
# This module defines:
#   COLAMD_INCLUDE_DIR     -   where to find cholmod.h
#   COLAMD_LIBRARIES       -   libraries to link against to use COLAMD
#   COLAMD_FOUND           -   If false, do not try to use COLAMD.
#
#
#=============================================================================
# Copyright 2010, Maximilian Behr
#
# Distributed under the OSI-approved BSD License (the "License");
# see accompanying file Copyright.txt for details.
#
# This software is distributed WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the License for more information.
#=============================================================================
# Changelog:
#
#

# GET_INC_LIB_DIR MACRO
INCLUDE(CMakeHelpers)
INCLUDE(FindPackageHandleStandardArgs)
GET_INC_LIB_DIR(_INCDIR _LIBDIR)

# search in user given directories
FIND_PATH(COLAMD_INCLUDE_DIR   NAMES colamd.h     PATHS ${SUITESPARSE} PATH_SUFFIXES include   NO_DEFAULT_PATH)
FIND_LIBRARY(COLAMD_LIBRARIES  NAMES colamd       PATHS ${SUITESPARSE} PATH_SUFFIXES lib lib64 NO_DEFAULT_PATH)

# search in other directories
FIND_PATH(COLAMD_INCLUDE_DIR   NAMES colamd.h     PATHS ${_incdir} /usr /opt PATH_SUFFIXES include local/include include/suitesparse local/include/suitesparse)
FIND_LIBRARY(COLAMD_LIBRARIES  NAMES colamd       PATHS ${_libdir} /usr /opt PATH_SUFFIXES lib lib64 local/lib local/lib64)


FIND_PACKAGE_HANDLE_STANDARD_ARGS(COLAMD DEFAULT_MSG COLAMD_LIBRARIES COLAMD_INCLUDE_DIR)
MARK_AS_ADVANCED(COLAMD_INCLUDE_DIR COLAMD_LIBRARIES)


