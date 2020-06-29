# - Find libNUMA

# This module defines
#  NUMA_LIBRARIES, the libraries to link against to use libNUMA.
#  NUMA_FOUND, If false, do not try to use libNUMA.
#  NUMA_INCLUDE_DIR, include directories for libNUMA.

#=============================================================================
# Copyright 2013, Martin Koehler
#
#
# Distributed under the OSI-approved BSD License (the "License");
# see accompanying file Copyright.txt for details.
#
# This software is distributed WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the License for more information.
#=============================================================================

INCLUDE(FindPackageHandleStandardArgs)
INCLUDE(CmakeHelpers)
GET_INC_LIB_DIR(_INCDIR _LIBDIR)


FIND_PATH(NUMA_NUMA_INCLUDE_DIR NAMES numa.h
    PATHS
    ${_INCDIR}
    /usr/include
    /usr/local/include
    /opt/local/include
    )

FIND_LIBRARY(NUMA_LIBRARY
    NAMES numa
    PATHS
    ${_LIBDIR}
    )


FIND_PACKAGE_HANDLE_STANDARD_ARGS(NUMA DEFAULT_MSG NUMA_LIBRARY)
MARK_AS_ADVANCED(NUMA_LIBRARY NUMA_NUMA_INCLUDE_DIR)
