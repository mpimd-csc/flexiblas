# Find the PYLINT search for the pylint programm
#
# This module defines:
#   PYLINT_FOUND            -   If false, do not try to use PYLINT
#   PYLINT                  -   Path to the programm pylint
#   PYLINT_VERSION_STRING   -   Python version found e.g. 1.6.5
#   PYLINT_VERSION_MAJOR    -   Python major version found e.g. 1
#   PYLINT_VERSION_MINOR    -   Python minor version found e.g. 6
#   PYLINT_VERSION_PATH     -   Python patvh version found e.g. 5
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


INCLUDE(FindPackageHandleStandardArgs)

FIND_PROGRAM(PYLINT
    NAMES
    pylint
    )



# determine python version string
IF(PYLINT)
    EXECUTE_PROCESS(COMMAND "${PYLINT}" --version OUTPUT_VARIABLE _VERSION ERROR_VARIABLE _VERSION ERROR_STRIP_TRAILING_WHITESPACE)
    STRING(REGEX MATCH      "pylint [0-9]+\\.[0-9]+\\.[0-9]+"       _VERSION                "${_VERSION}")
    STRING(REGEX MATCH      "[0-9]+\\.[0-9]+\\.[0-9]+"              PYLINT_VERSION_STRING   "${_VERSION}")
    STRING(REGEX REPLACE    "^([0-9]+)\\.[0-9]+\\.[0-9]+.*" "\\1"   PYLINT_VERSION_MAJOR    "${PYLINT_VERSION_STRING}")
    STRING(REGEX REPLACE    "^[0-9]+\\.([0-9])+\\.[0-9]+.*" "\\1"   PYLINT_VERSION_MINOR    "${PYLINT_VERSION_STRING}")
    STRING(REGEX REPLACE    "^[0-9]+\\.[0-9]+\\.([0-9]+).*" "\\1"   PYLINT_VERSION_PATCH    "${PYLINT_VERSION_STRING}")
ENDIF()

FIND_PACKAGE_HANDLE_STANDARD_ARGS(PYLINT DEFAULT_MSG PYLINT PYLINT_VERSION_STRING PYLINT_VERSION_MAJOR PYLINT_VERSION_MINOR)
MARK_AS_ADVANCED(PYLINT PYLINT_VERSION_STRING PYLINT_VERSION_MAJOR PYLINT_VERSION_MINOR PYLINT_VERSION_PATCH)


