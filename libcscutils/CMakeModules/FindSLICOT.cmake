# Find the SLICOT library.
#
# This module defines
#  SLICOT_FOUND         - If false, do not try to use SLICOT.
#  SLICOT_LIBRARIES     - the libraries to link against to use SLICOT.

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

INCLUDE(FindPackageHandleStandardArgs)

FIND_LIBRARY(SLICOT_LIBRARIES
    NAMES
    slicot
    PATHS
    /usr/
    /usr/
    /usr/local/
    /opt/
    /opt/local/
    ${_libdir}
    PATH_SUFFIXES
    lib/
    lib64
    )


FIND_PACKAGE_HANDLE_STANDARD_ARGS(SLICOT DEFAULT_MSG SLICOT_LIBRARIES)
MARK_AS_ADVANCED(SLICOT_LIBRARIES)


