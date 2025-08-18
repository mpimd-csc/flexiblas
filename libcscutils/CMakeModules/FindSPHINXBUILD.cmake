# Find the SPHINXBUILD search for the sphinx-build programm
#
# This module defines:
#   SPHINXBUILD_FOUND       -   If false, do not try to use SPHINXBUILD
#   SPHINXBUILD             -   Path to the programm sphinx-build
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


FIND_PROGRAM(SPHINXBUILD
    NAMES
    sphinx-build
    )


FIND_PACKAGE_HANDLE_STANDARD_ARGS(SPHINXBUILD DEFAULT_MSG SPHINXBUILD)
MARK_AS_ADVANCED(SPHINXBUILD)


