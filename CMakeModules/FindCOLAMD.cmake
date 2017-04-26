# - Find the COLAMD includes and library
#
# This module defines
#  COLAMD_INCLUDE_DIR, where to find umfpack.h, etc.
#  COLAMD_LIBRARIES, the libraries to link against to use COLAMD.
#  COLAMD_FOUND, If false, do not try to use COLAMD.
# also defined, but not for general use are
#  COLAMD_LIBRARY, where to find the COLAMD library.
# None of the above will be defined unless UFconfig can be found.
# COLAMD depends on  UFConfig

#=============================================================================
# Copyright 2011, Martin Koehler
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
    string(REPLACE ":" ";" _liblist $ENV{DYLD_LIBRARY_PATH} "" )
    set(_incdir) 
    foreach ( dir ${_liblist}) 
    	set(_incdir ${_incdir} "${dir}/../include")
    endforeach() 
    set(_incdir "${_incdir}" ENV INC ENV INCLUDE ENV CPATH)

else ()
    set(_libdir ENV LD_LIBRARY_PATH)
    string(REPLACE ":" ";" _liblist $ENV{LD_LIBRARY_PATH} "")
    set(_incdir) 
    foreach ( dir ${_liblist}) 
    	set(_incdir ${_incdir} "${dir}/../include")
    endforeach() 
    set(_incdir "${_incdir}" ENV INC ENV INCLUDE ENV CPATH)
  endif ()




find_path(COLAMD_COLAMD_INCLUDE_DIR colamd.h
	${SUITESPARSE}/COLAMD/Include  #Local Setup
	${SUITESPARSE}/include
	${_incdir}
	/usr/include
	/usr/local/include
	/usr/local/include/suitesparse 	#FreeBSD
	/usr/include/suitesparse	#Debian
	/opt/local/include/ufsparse 	#Macports 
	NO_DEFAULT_PATH
  )
find_path(COLAMD_COLAMD_INCLUDE_DIR colamd.h) 

set(COLAMD_NAMES ${COLAMD_NAMES} libcolamd colamd libcolamd.a colamd.a)
set(COLAMD_PATH
	${SUITESPARSE}/COLAMD/Lib 
	${SUITESPARSE}/lib
	${_libdir}
	/usr/local/lib64 
 	/usr/local/lib 
	/usr/lib64
	/usr/lib
 	/opt/local/lib	# Macports
	)
find_library(COLAMD_LIBRARY NAMES ${COLAMD_NAMES} PATHS ${COLAMD_PATH} NO_DEFAULT_PATH)
find_library(COLAMD_LIBRARY NAMES ${COLAMD_NAMES} PATHS ${COLAMD_PATH} )

if (COLAMD_LIBRARY AND COLAMD_COLAMD_INCLUDE_DIR)
      SET(COLAMD_INCLUDE_DIR ${COLAMD_COLAMD_INCLUDE_DIR}) 
      SET(COLAMD_LIBRARIES ${COLAMD_LIBRARY} )
endif (COLAMD_LIBRARY AND COLAMD_COLAMD_INCLUDE_DIR)


# handle the QUIETLY and REQUIRED arguments and set COLAMD_FOUND to TRUE if
# all listed variables are TRUE
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(COLAMD  DEFAULT_MSG COLAMD_LIBRARY COLAMD_COLAMD_INCLUDE_DIR)

mark_as_advanced(COLAMD_COLAMD_INCLUDE_DIR COLAMD_LIBRARY )
