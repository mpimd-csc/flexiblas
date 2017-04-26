# - Find sqlite

# This module defines
#  SQLITE3_LIBRARIES, the libraries to link against to use libsqlite3.
#  SQLITE3_FOUND, If false, do not try to use libsqlite3.
#  SQLITE3_INCLUDE_DIR, include directories for libsqlite3. 

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

find_path(SQLITE3_INCLUDE_DIR NAMES sqlite3.h
	PATHS
	${_incdir}
	/usr/include
	/usr/local/include
	/opt/local/include	#Macports
  )
set(SQLITE3_NAMES sqlite3)
find_library(SQLITE3_LIBRARIES NAMES ${SQLITE3_NAMES} PATHS ${_libdir} /usr/lib /usr/lib32 /usr/lib64)

IF(SQLITE3_INCLUDE_DIR AND SQLITE3_LIBRARIES)
	MESSAGE(STATUS "Found SQLITE3 header: ${SQLITE3_INCLUDE_DIR}")
	MESSAGE(STATUS "Found SQLITE3 library: ${SQLITE3_LIBRARIES}")
	SET(SQLITE3_FOUND TRUE) 
ELSE()
	SET(SQLITE3_FOUND FALSE) 
ENDIF()

# all listed variables are TRUE
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(SQLITE3 DEFAULT_MSG SQLITE3_LIBRARIES SQLITE3_INCLUDE_DIR )
