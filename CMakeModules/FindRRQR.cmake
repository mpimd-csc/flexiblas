# - Find the RRQR library
#
# This module defines
#  RRQR_LIBRARIES, the libraries to link against to use RRQR.
#  RRQR_FOUND, If false, do not try to use RRQR.

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

IF (NOT DEFINED RRQR)
set(RRQR_NAMES librrqr rrqr)
find_library(RRQR_LIBRARY NAMES ${RRQR_NAMES} PATHS
 	/usr/lib
	/usr/local/lib
	)

if (RRQR_LIBRARY)
      SET(RRQR_LIBRARIES ${RRQR_LIBRARY})
endif (RRQR_LIBRARY)
ELSE (NOT DEFINED RRQR)
	SET(RRQR_LIBRARIES ${RRQR})
	SET(RRQR_LIBRARY ${RRQR})
ENDIF (NOT DEFINED RRQR)


# handle the QUIETLY and REQUIRED arguments and set AMD_FOUND to TRUE if
# all listed variables are TRUE
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(RRQR  DEFAULT_MSG RRQR_LIBRARY)

