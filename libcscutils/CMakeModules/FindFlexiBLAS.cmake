# - Find the FLEXIBLAS includes and library
#
# This module defines
#  FLEXIBLAS_INCLUDE_DIR, where to find flexiblas_info.h
#  FLEXIBLAS_FOUND, If false, do not try to use FLEXIBLAS.
# also defined, but not for general use are
#  FLEXIBLAS_LIBRARY, where to find the FLEXIBLAS library.
# None of the above will be defined unless UFconfig can be found.
# FLEXIBLAS depends on  UFConfig

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
#    - Apr. 1, 2011   Martin Koehler

INCLUDE(FindPackageHandleStandardArgs)

IF(NOT _INCDIR)
    IF(WIN32)
        SET(_INCDIR ENV INCLUDE)
    ELSEIF(APPLE)
        SET(_INCDIR ENV CPATH)
    ELSE()
        SET(_INCDIR ENV CPATH)
    ENDIF()
ENDIF()

FIND_PATH(FLEXIBLAS_INC_DIR
    flexiblas_info.h
    PATHS
    ${_INCDIR}
    /usr/include
    /usr/local/include
    /opt/local/include  #Macports
    )

IF(FLEXIBLAS_INC_DIR)
    SET(FLEXIBLAS_INCLUDE_DIR ${FLEXIBLAS_INC_DIR})
ENDIF()


# handle the QUIETLY and REQUIRED arguments and set AMD_FOUND to TRUE if
FIND_PACKAGE_HANDLE_STANDARD_ARGS(FLEXIBLAS  DEFAULT_MSG FLEXIBLAS_INC_DIR)
MARK_AS_ADVANCED(FLEXIBLAS_INC_DIR)
