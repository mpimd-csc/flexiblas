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
# http://www-user.tu-chemnitz.de/~komart/
#
# Distributed under the OSI-approved BSD License (the "License");
# see accompanying file Copyright.txt for details.
#
# This software is distributed WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the License for more information.
#=============================================================================
# Changelog:
#    - Apr. 1, 2011 	Martin Koehler

if (NOT _incdir)
  if (WIN32)
	  set(_incdir ENV INCLUDE)
  elseif (APPLE)
	  set(_incdir ENV CPATH)
  else ()
	  set(_incdir ENV CPATH)
  endif ()
endif ()

find_path(FLEXIBLAS_INC_DIR flexiblas_info.h PATHS
	${_incdir}
	/usr/include
	/usr/local/include
	/opt/local/include	#Macports
 )

if (FLEXIBLAS_INC_DIR)
	SET(FLEXIBLAS_INCLUDE_DIR ${FLEXIBLAS_INC_DIR} )
endif (FLEXIBLAS_INC_DIR)


# handle the QUIETLY and REQUIRED arguments and set AMD_FOUND to TRUE if
# all listed variables are TRUE
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(FLEXIBLAS  DEFAULT_MSG FLEXIBLAS_INC_DIR)
mark_as_advanced(FLEXIBLAS_INC_DIR )
