# - Find the AMD includes and library
#
# This module defines
#  AMD_INCLUDE_DIR, where to find umfpack.h, etc.
#  AMD_LIBRARIES, the libraries to link against to use AMD.
#  AMD_FOUND, If false, do not try to use AMD.
# also defined, but not for general use are
#  AMD_LIBRARY, where to find the AMD library.
# None of the above will be defined unless UFconfig can be found.
# AMD depends on  UFConfig

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

find_path(MAGMA_INC_DIR magma.h PATHS
	${_incdir}
	/usr/include
	/usr/local/include
	/opt/local/include	#Macports
 )
MESSAGE(STATUS "MAGMA_INC_DIR = ${MAGMA_INC_DIR}") 

set(MAGMA_NAMES ${MAGMA_NAMES} libmagma magma libmagma.a magma.a)

if (WIN32)
	  set(_libdir ENV LIB ${ARGN} )
elseif (APPLE)
  set(_libdir ENV DYLD_LIBRARY_PATH ${ARGN} )
else ()
	  set(_libdir ENV LD_LIBRARY_PATH ${ARGN})
endif ()
set(MAGMA_PATH
	${_libdir}
	/usr/local/lib64 
 	/usr/local/lib 
	/usr/lib64
	/usr/lib
)
find_library(MAGMA_LIBRARY NAMES ${MAGMA_NAMES} HINTS ${MAGMA_PATH} PATHS ${MAGMA_PATH} )
MESSAGE(STATUS "MAGMA_LIBRARY = ${MAGMA_LIBRARY}") 




if (MAGMA_LIBRARY AND MAGMA_INC_DIR)
      SET(MAGMA_INCLUDE_DIR ${MAGMA_INC_DIR} )
      SET(MAGMA_LIBRARIES  ${MAGMABLAS_LIBRARY} ${MAGMA_LIBRARY} ${MAGMABLAS_LIBRARY} )
endif (MAGMA_LIBRARY AND MAGMA_INC_DIR )


# handle the QUIETLY and REQUIRED arguments and set AMD_FOUND to TRUE if
# all listed variables are TRUE
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(MAGMA  DEFAULT_MSG MAGMA_LIBRARIES MAGMA_INC_DIR)

mark_as_advanced(MAGMABLAS_LIBRARY MAGMA_LIBRARY MAGMA_INC_DIR )
