# Find the CHOLMOD includes and library
#
# This module defines:
#   CHOLMOD_INCLUDE_DIR     -   where to find cholmod.h
#   CHOLMOD_LIBRARIES       -   libraries to link against to use CHOLMOD
#   CHOLMOD_FOUND           -   If false, do not try to use CHOLMOD.
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
GET_INC_LIB_DIR(_INCDIR _LIBDIR)


# search in user given directories
FIND_PATH(CHOLMOD_INCLUDE_DIR   NAMES cholmod.h     PATHS ${SUITESPARSE} PATH_SUFFIXES include      NO_DEFAULT_PATH)
FIND_LIBRARY(CHOLMOD_LIBRARIES  NAMES cholmod       PATHS ${SUITESPARSE} PATH_SUFFIXES lib lib64    NO_DEFAULT_PATH)

# search in other directories
FIND_PATH(CHOLMOD_INCLUDE_DIR   NAMES cholmod.h     PATHS ${_incdir} /usr /opt PATH_SUFFIXES include local/include include/suitesparse local/include/suitesparse)
FIND_LIBRARY(CHOLMOD_LIBRARIES  NAMES cholmod       PATHS ${_libdir} /usr /opt PATH_SUFFIXES lib lib64 local/lib local/lib64)


INCLUDE(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(CHOLMOD DEFAULT_MSG CHOLMOD_LIBRARIES CHOLMOD_INCLUDE_DIR)
MARK_AS_ADVANCED(CHOLMOD_INCLUDE_DIR CHOLMOD_LIBRARIES)























