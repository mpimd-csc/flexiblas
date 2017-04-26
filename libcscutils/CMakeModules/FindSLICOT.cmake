# - Find the SLICOT library
#
# This module defines
#  SLICOT_LIBRARIES, the libraries to link against to use SLICOT.
#  SLICOT_FOUND, If false, do not try to use SLICOT.
# also defined, but not for general use are
#  SLICOT_LIBRARY, where to find the SLICOT library.

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

if (WIN32)
	set(_libdir ENV LIB)
elseif (APPLE)
	set(_libdir ENV DYLD_LIBRARY_PATH)
else ()
	set(_libdir ENV LD_LIBRARY_PATH)
endif ()

set(SLICOT_NAMES slicot)
#set(SLICOT_PATH /usr/local/lib /usr/lib )
find_library(SLICOT_LIBRARY NAMES ${SLICOT_NAMES} PATHS ${_libdir} )

if (SLICOT_LIBRARY)
	SET(SLICOT_LIBRARIES ${SLICOT_LIBRARY} ${LAPACK_LIBRARIES})
	SET(SLICOT_FOUND true)
endif (SLICOT_LIBRARY)


# handle the QUIETLY and REQUIRED arguments and set SLICOT_FOUND to TRUE if
# all listed variables are TRUE
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(SLICOT  DEFAULT_MSG SLICOT_LIBRARY )

mark_as_advanced(SLICOT_LIBRARY )
