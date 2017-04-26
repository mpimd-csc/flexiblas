# - Find libmatio

# This module defines
#  MATIO_LIBRARIES, the libraries to link against to use libmatio.
#  MATIO_FOUND, If false, do not try to use libmatio.
#  MATIO_INCLUDE_DIR, include directories for libmatio. 

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
FIND_PACKAGE(ZLIB REQUIRED) 
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

find_path(MATIO_INCLUDE_DIR NAMES matio.h
	PATHS
	${_incdir}
	/usr/include
	/usr/local/include
	/opt/local/include	#Macports
  )
set(MATIO_NAMES matio)
find_library(MATIO_LIBRARY NAMES ${MATIO_NAMES} PATHS ${_libdir} /usr/lib /usr/lib32 /usr/lib64)

IF(MATIO_INCLUDE_DIR AND MATIO_LIBRARY)
	MESSAGE(STATUS "Found MATIO header: ${MATIO_INCLUDE_DIR}")
	MESSAGE(STATUS "Found MATIO library: ${MATIO_LIBRARY}")

	MESSAGE(STATUS "Check if MATIO needs HDF5")

	SET(__CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES})
	SET(CMAKE_REQUIRED_LIBRARIES ${MATIO_LIBRARY} ${ZLIB_LIBRARIES} )
	CHECK_FUNCTION_EXISTS(Mat_Open NEED_HDF5)
	IF ( NOT NEED_HDF5 ) 
		#try to find HDF5
		MESSAGE(STATUS "Search for HDF5 because Matio Reuqires it.")
		FIND_PACKAGE(HDF5)
		IF (HDF5_FOUND)
			MESSAGE(STATUS "HDF5 found.")		
			SET(MATIO_FOUND TRUE)
		ELSE (HDF5_FOUND)
			MESSAGE("MATIO could not enabled, due to missing HDF5." ) 
			SET(MATIO_FOUND FALSE)
		ENDIF (HDF5_FOUND)
	ELSE()
		SET (MATIO_FOUND TRUE)
	ENDIF()	
	SET(CMAKE_REQUIRED_LIBRARIES ${__CMAKE_REQUIRED_LIBRARIES})

ENDIF()

if (MATIO_FOUND AND MATIO_INCLUDE_DIR)
	SET(MATIO_LIBRARIES ${MATIO_LIBRARY} ${HDF5_LIBRARIES})
	SET(MATIO_INCLUDE_DIR ${MATIO_MATIO_INCLUDE_DIR} ${HDF5_INCLUDE_DIR})
endif ()


# handle the QUIETLY and REQUIRED arguments and set SLICOT_FOUND to TRUE if
# all listed variables are TRUE
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(MATIO DEFAULT_MSG MATIO_LIBRARY MATIO_INCLUDE_DIR )

mark_as_advanced(MATIO_LIBRARY)
