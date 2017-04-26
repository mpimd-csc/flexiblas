# - Find the PMLIB includes and library
#
# This module defines
#  PMLIB_INCLUDE_DIR, where to find pmlib.h, etc.
#  PMLIB_LIBRARIES, the libraries to link against to use PMLIB.
#  AMD_FOUND, If false, do not try to use PMLIB.

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


find_path(PMLIB_INC_DIR pmlib.h  )

set(PMLIB_NAMES ${PMLIB_NAMES} libpmlib pmlib)

if (WIN32)
	  set(_libdir ENV LIB ${ARGN} )
elseif (APPLE)
  set(_libdir ENV DYLD_LIBRARY_PATH ${ARGN} )
else ()
	  set(_libdir ENV LD_LIBRARY_PATH ${ARGN})
endif ()
set(PMLIB_PATH
	${_libdir}
	/usr/local/lib64 
 	/usr/local/lib 
	/usr/lib64
	/usr/lib
)
find_library(PMLIB_LIBRARY NAMES ${PMLIB_NAMES} HINTS ${PMLIB_PATH} PATHS ${PMLIB_PATH} )


if (PMLIB_LIBRARY AND PMLIB_INC_DIR)
      SET(PMLIB_INCLUDE_DIR ${PMLIB_INC_DIR} )
      SET(PMLIB_LIBRARIES  ${PMLIB_LIBRARY} )
endif (PMLIB_LIBRARY AND PMLIB_INC_DIR )


# handle the QUIETLY and REQUIRED arguments and set AMD_FOUND to TRUE if
# all listed variables are TRUE
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(PMLIB  DEFAULT_MSG PMLIB_LIBRARIES PMLIB_INC_DIR)

mark_as_advanced(PMLIB_LIBRARY PMLIB_INC_DIR )
