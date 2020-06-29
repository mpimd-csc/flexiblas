# Find the UFconfig include
#
# This module defines
#
#   UFCONFIG_INCLUDE_DIR
#   UFCONFIG_FOUND
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
#   - Jan 28, 2011   Yoann Le Bars
#   - Feb 21, 2011  Martin Koehler
#       - June 25, 2013   Martin Koehler, add _libdir and _incdir

INCLUDE(FindPackageHandleStandardArgs)

# GET_INC_LIB_DIR MACRO
INCLUDE(CMakeHelpers)
GET_INC_LIB_DIR(_incdir _libdir)

# search in user given directories
FIND_PATH(UFCONFIG_INCLUDE_DIR   NAMES UFconfig.h    PATHS ${SUITESPARSE} PATH_SUFFIXES include   NO_DEFAULT_PATH)

# search in other directories
FIND_PATH(UFCONFIG_INCLUDE_DIR   NAMES UFconfig.h    PATHS ${_incdir} /usr /opt PATH_SUFFIXES include local/include include/suitesparse local/include/suitesparse)

FIND_PACKAGE_HANDLE_STANDARD_ARGS(UFCONFIG  DEFAULT_MSG  UFCONFIG_INCLUDE_DIR)
MARK_AS_ADVANCED(UFCONFIG_INCLUDE_DIR)

