# Find the RRQR library.
#
# This module defines
#  RRQR_FOUND       - If false, do not try to use RRQR.
#  RRQR_LIBRARIES   - the libraries to link against to use RRQR.

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


FIND_LIBRARY(RRQR_LIBRARIES
    NAMES
    librrqr
    rrqr
    PATHS
    /usr/
    /usr/
    /usr/local/
    /opt/
    /opt/local/
    PATH_SUFFIXES
    lib/
    lib64
    )


FIND_PACKAGE_HANDLE_STANDARD_ARGS(RRQR DEFAULT_MSG RRQR_LIBRARIES)
MARK_AS_ADVANCED(RRQR_LIBRARIES))



