# Find the UMFPACK includes and library
#
# This module defines:
#   UMFPACK_INCLUDE_DIR     -   where to find cholmod.h
#   UMFPACK_LIBRARIES       -   libraries to link against to use UMFPACK
#   UMFPACK_FOUND           -   If false, do not try to use UMFPACK.
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
INCLUDE(FindPackageHandleStandardArgs)

# GET_INC_LIB_DIR MACRO
INCLUDE(CMakeHelpers)
GET_INC_LIB_DIR(_incdir _libdir)

# search in user given directories
FIND_PATH(UMFPACK_INCLUDE_DIR   NAMES umfpack.h        PATHS ${SUITESPARSE} PATH_SUFFIXES include   NO_DEFAULT_PATH)
FIND_LIBRARY(UMFPACK_LIBRARIES  NAMES umfpack          PATHS ${SUITESPARSE} PATH_SUFFIXES lib lib64 NO_DEFAULT_PATH)

# search in other directories
FIND_PATH(UMFPACK_INCLUDE_DIR   NAMES umfpack.h        PATHS ${_incdir} /usr /opt PATH_SUFFIXES include local/include include/suitesparse local/include/suitesparse)
FIND_LIBRARY(UMFPACK_LIBRARIES  NAMES umfpack          PATHS ${_libdir} /usr /opt PATH_SUFFIXES lib lib64 local/lib local/lib64)


FIND_PACKAGE_HANDLE_STANDARD_ARGS(UMFPACK DEFAULT_MSG UMFPACK_LIBRARIES UMFPACK_INCLUDE_DIR)
MARK_AS_ADVANCED(UMFPACK_INCLUDE_DIR UMFPACK_LIBRARIES)

