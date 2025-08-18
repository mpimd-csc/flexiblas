# Find the cppcheck and cpp-checkhtmlreport program
#
# This module defines:
#   CPPCHECK_FOUND          -   If false, do not try to use CPPCHECK
#   CPPCHECK                -   cppcheck program
#   CPPCHECKHTMLREPORT      -   cppcheck-htmlreport generator
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

FIND_PROGRAM(CPPCHECK
            NAMES
            cppcheck
            )


FIND_PROGRAM(CPPCHECKHTMLREPORT
            NAMES
            cppcheck-htmlreport
            )


FIND_PACKAGE_HANDLE_STANDARD_ARGS(CPPCHECK DEFAULT_MSG CPPCHECK CPPCHECKHTMLREPORT)
MARK_AS_ADVANCED(CPPCHECK CPPCHECKHTMLREPORT)


