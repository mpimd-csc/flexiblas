# Find the ARPACK includes and library
#
# This module defines:
#   ARPACK_LIBRARIES       -   libraries to link against to use ARPACK
#   ARPACK_FOUND           -   If false, do not try to use ARPACK.
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

FIND_LIBRARY(ARPACK_LIBRARIES
    NAMES
    arpack
    PATHS
    ${ARPACK}
    /usr/
    /usr/local/
    /opt/
    /opt/local/
    PATH_SUFFIXES
    lib
    lib64
    )


INCLUDE(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(ARPACK DEFAULT_MSG ARPACK_LIBRARIES)
MARK_AS_ADVANCED(ARPACK_LIBRARIES)

