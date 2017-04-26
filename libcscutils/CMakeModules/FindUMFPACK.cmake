# - Find the UMFPACK includes and library
#
# This module defines
#  UMFPACK_INCLUDE_DIR, where to find umfpack.h, etc.
##  UMFPACK_LIBRARIES, the libraries to link against to use UMFPACK.
#  UMFPACK_FOUND, If false, do not try to use UMFPACK.
# also defined, but not for general use are
#  UMFPACK_LIBRARY, where to find the AMD library.
# None of the above will be defined unless AMD can be found.
# UMFPACK depends on AMD and UFConfig

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
#    - June 25, 2013 	Martin Koehler, add _libdir and _incdir 

  if (WIN32)
    set(_libdir ENV LIB)
    set(_liblist $ENV{LIB})
    set(_incdir) 
    foreach ( dir ${_liblist}) 
    	set(_incdir ${_incdir} "${dir}/../include")
    endforeach() 
    set(_incdir "${_incdir}" ENV INC ENV INCLUDE ENV CPATH)
  elseif (APPLE)
    set(_libdir ENV DYLD_LIBRARY_PATH)
    if (NOT $ENV{DYLD_LIBRARY_PATH}  STREQUAL "") 
	    string(REPLACE ":" ";" _liblist $ENV{DYLD_LIBRARY_PATH} "")
	    set(_incdir) 
	    foreach ( dir ${_liblist}) 
    		set(_incdir ${_incdir} "${dir}/../include")
	    endforeach() 
	    set(_incdir "${_incdir}" ENV INC ENV INCLUDE ENV CPATH)
    endif()

  else ()
    set(_libdir ENV LD_LIBRARY_PATH)
    if (NOT $ENV{DYLD_LIBRARY_PATH}  STREQUAL "") 
	    string(REPLACE ":" ";" _liblist $ENV{LD_LIBRARY_PATH} "" )
	    set(_incdir) 
	    foreach ( dir ${_liblist}) 
    		set(_incdir ${_incdir} "${dir}/../include")
	    endforeach() 
	    set(_incdir "${_incdir}" ENV INC ENV INCLUDE ENV CPATH)
    endif() 
    endif ()




if(UMFPACK_FIND_QUIETLY)
  set(_FIND_AMD_ARG QUIET)
  set(_FIND_CHOLMOD_ARG QUIET)
endif(UMFPACK_FIND_QUIETLY)

find_package(AMD ${_FIND_AMD_ARG})
find_package(CHOLMOD ${_FIND_CHOLMOD_ARG})

if (AMD_FOUND AND CHOLMOD_FOUND) 
find_path(UMFPACK_UMFPACK_INCLUDE_DIR umfpack.h
	${SUITESPARSE}/UMFPACK/Include  #Local Setup
	${SUITESPARSE}/include 
	${_incdir}
	/usr/include
	/usr/local/include
	/usr/local/include/suitesparse 	#FreeBSD
	/usr/include/suitesparse	#Debian
	/opt/local/include/ufsparse 	#Macports
	NO_DEFAULT_PATH
  )
find_path(UMFPACK_UMFPACK_INCLUDE_DIR umfpack.h) 

set(UMFPACK_NAMES ${UMFPACK_NAMES} umfpack libumfpack)
set(UMFPACK_PATH
	${SUITESPARSE}/UMFPACK/Lib 
	${SUITESPARSE}/lib
	${_libdir}
	/usr/local/lib64 
 	/usr/local/lib 
	/usr/lib64
	/usr/lib
 	/opt/local/lib	# Macports
	${SUITESPARSE}/UMFPACK/Lib 
	${SUITESPARSE}/lib
	)

find_library(UMFPACK_LIBRARY NAMES ${UMFPACK_NAMES} PATHS ${UMFPACK_PATH} NO_DEFAULT_PATH )
find_library(UMFPACK_LIBRARY NAMES ${UMFPACK_NAMES} PATHS ${UMFPACK_PATH} )




if (UMFPACK_LIBRARY AND UMFPACK_UMFPACK_INCLUDE_DIR)
      SET(UMFPACK_INCLUDE_DIR ${UMFPACK_UMFPACK_INCLUDE_DIR} ${CHOLMOD_INCLUDE_DIR} ${AMD_INCLUDE_DIR} )
      SET(UMFPACK_LIBRARIES ${UMFPACK_LIBRARY} ${AMD_LIBRARIES} ${CHOLMOD_LIBRARIES} )
endif (UMFPACK_LIBRARY AND UMFPACK_UMFPACK_INCLUDE_DIR)

endif (AMD_FOUND AND CHOLMOD_FOUND)

# handle the QUIETLY and REQUIRED arguments and set UMFPACK_FOUND to TRUE if
# all listed variables are TRUE
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(UMFPACK  DEFAULT_MSG  UMFPACK_LIBRARIES UMFPACK_INCLUDE_DIR)
mark_as_advanced(UMFPACK_UMFPACK_INCLUDE_DIR UMFPACK_LIBRARY )
