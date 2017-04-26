# - Find sqlite

# This module defines
#  ARCHIVE_LIBRARIES, the libraries to link against to use libsqlite3.
#  ARCHIVE_FOUND, If false, do not try to use libsqlite3.
#  ARCHIVE_INCLUDE_DIR, include directories for libsqlite3. 

#=============================================================================
# Copyright 2014, Martin Koehler
#
#
# Distributed under the OSI-approved BSD License (the "License");
# see accompanying file Copyright.txt for details.
#
# This software is distributed WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the License for more information.
#=============================================================================
INCLUDE(CheckFunctionExists) 

if (NOT _incdir)
  if (WIN32)
	  set(_incdir ENV INCLUDE)
  elseif (APPLE)
	  set(_incdir ENV INCLUDE CPATH)
  else ()
	  set(_incdir ENV INCLUDE CPATH)
  endif ()
endif ()

if (NOT _libdir)
  if (WIN32)
    set(_libdir ENV LIB)
  elseif (APPLE)
    set(_libdir ENV DYLD_LIBRARY_PATH)
  else ()
    set(_libdir ENV LD_LIBRARY_PATH)
  endif ()
endif ()

find_path(ARCHIVE_INCLUDE_DIR NAMES archive.h
	PATHS
	${_incdir}
	/usr/include
	/usr/local/include
	/opt/local/include	#Macports
  )
set(ARCHIVE_NAMES archive libarchive)
find_library(ARCHIVE_LIBRARIES NAMES ${ARCHIVE_NAMES} PATHS ${_libdir} /usr/lib /usr/lib32 /usr/lib64)

IF(ARCHIVE_INCLUDE_DIR AND ARCHIVE_LIBRARIES)
	MESSAGE(STATUS "Found ARCHIVE header: ${ARCHIVE_INCLUDE_DIR}")
	MESSAGE(STATUS "Found ARCHIVE library: ${ARCHIVE_LIBRARIES}")
	SET(ARCHIVE_FOUND TRUE) 
ELSE()
	SET(ARCHIVE_FOUND FALSE) 
ENDIF()

# all listed variables are TRUE
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(ARCHIVE DEFAULT_MSG ARCHIVE_LIBRARIES ARCHIVE_INCLUDE_DIR )
