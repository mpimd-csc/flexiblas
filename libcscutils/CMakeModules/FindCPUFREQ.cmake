# - Find the CPUFREQ includes and library
#
# This module defines
#  CPUFREQ_INCLUDE_DIR, where to find pmlib.h, etc.
#  CPUFREQ_LIBRARIES, the libraries to link against to use CPUFREQ.
#  CPUFREQ_FOUND, If false, do not try to use CPUFREQ.

#=============================================================================
# Copyright 2014, Martin Koehler
#
# Distributed under the OSI-approved BSD License (the "License");
# see accompanying file Copyright.txt for details.
#
# This software is distributed WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the License for more information.
#=============================================================================
# Changelog:

INCLUDE(FindPackageHandleStandardArgs)

FIND_PATH(CPUFREQ_INC_DIR cpufreq.h)

SET(CPUFREQ_NAMES ${CPUFREQ_NAMES} libcpupower cpupower cpufreq libcpufreq)

IF(WIN32)
    SET(_libdir $ENV{LIB} ${ARGN})
ELSEIF(APPLE)
    SET(_libdir $ENV{DYLD_LIBRARY_PATH} ${ARGN})
ELSE()
    SET(_libdir $ENV{LD_LIBRARY_PATH} ${ARGN})
ENDIF()


SET(CPUFREQ_PATH
    ${_libdir}
    /usr/local/lib64
    /usr/local/lib
    /usr/lib64
    /usr/lib
    )
FIND_LIBRARY(CPUFREQ_LIBRARY NAMES ${CPUFREQ_NAMES} HINTS ${CPUFREQ_PATH} PATHS ${CPUFREQ_PATH})



IF(CPUFREQ_LIBRARY AND CPUFREQ_INC_DIR)
    SET(CPUFREQ_INCLUDE_DIR ${CPUFREQ_INC_DIR})
    SET(CPUFREQ_LIBRARIES  ${CPUFREQ_LIBRARY})
ENDIF()


FIND_PACKAGE_HANDLE_STANDARD_ARGS(CPUFREQ  DEFAULT_MSG CPUFREQ_LIBRARIES CPUFREQ_INCLUDE_DIR)
MARK_AS_ADVANCED(CPUFREQ_LIBRARY CPUFREQ_INC_DIR)
