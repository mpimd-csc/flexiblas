# - Find the GFortran library
#
# This module defines
#  GFORTRAN_LIBRARIES, the libraries to link against to use GFORTRAN.

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

IF(CMAKE_SYSTEM_NAME STREQUAL "FreeBSD")
    SET(GFORTRAN_NAMES ${GFORTRAN_NAMES}  gfortran)
    FIND_LIBRARY(GFORTRAN_LIBRARY NAMES gfortran libgfortran PATHS
        /usr/local/lib/gcc45/
        /usr/local/lib/gcc43/
        /usr/local/lib/gcc44/
        /usr/local/lib/
        /usr/lib/
        ${CMAKE_SYSTEM_LIBRARY_PATH}
        )
    IF(GFORTRAN_LIBRARY)
        SET(GFORTRAN_LIBRARIES ${GFORTRAN_LIBRARY})
    ENDIF()
ELSE()
    SET(GFORTRAN_LIBRARY "-lgfortran")
    SET(GFORTRAN_LIBRARIES "-lgfortran")
ENDIF()




# handle the QUIETLY and REQUIRED arguments and set AMD_FOUND to TRUE if
# all listed variables are TRUE
FIND_PACKAGE_HANDLE_STANDARD_ARGS(GFORTRAN  DEFAULT_MSG GFORTRAN_LIBRARY)

MARK_AS_ADVANCED(GFORTRAN_LIBRARY)
