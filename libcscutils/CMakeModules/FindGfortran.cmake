# - Find the GFortran library
#
# This module defines
#  GFORTRAN_LIBRARIES, the libraries to link against to use GFORTRAN.

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

IF (CMAKE_SYSTEM_NAME STREQUAL "FreeBSD")
set(GFORTRAN_NAMES ${GFORTRAN_NAMES}  gfortran)
find_library(GFORTRAN_LIBRARY NAMES gfortran libgfortran PATHS
	/usr/local/lib/gcc45/
	/usr/local/lib/gcc43/
	/usr/local/lib/gcc44/
	/usr/local/lib/
	/usr/lib/
	${CMAKE_SYSTEM_LIBRARY_PATH}
	)
if (GFORTRAN_LIBRARY)
      SET(GFORTRAN_LIBRARIES ${GFORTRAN_LIBRARY} )
endif (GFORTRAN_LIBRARY)
ELSE (CMAKE_SYSTEM_NAME STREQUAL "FreeBSD")
	SET(GFORTRAN_LIBRARY "-lgfortran")
	SET(GFORTRAN_LIBRARIES "-lgfortran")
ENDIF (CMAKE_SYSTEM_NAME STREQUAL "FreeBSD")




# handle the QUIETLY and REQUIRED arguments and set AMD_FOUND to TRUE if
# all listed variables are TRUE
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(GFORTRAN  DEFAULT_MSG GFORTRAN_LIBRARY)

mark_as_advanced(GFORTRAN_LIBRARY )
