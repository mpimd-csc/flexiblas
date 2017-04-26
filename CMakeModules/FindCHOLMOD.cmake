# - Find the CHOLMOD includes and library
#
# This module defines
#  CHOLMOD_INCLUDE_DIR, where to find umfpack.h, etc.
#  CHOLMOD_LIBRARIES, the libraries to link against to use UMFPACK.
#  CHOLMOD_FOUND, If false, do not try to use UMFPACK.
# also defined, but not for general use are
#  CHOLMOD_LIBRARY, where to find the AMD library.
# None of the above will be defined unless AMD can be found.
# UMFPACK depends on AMD and UFConfig

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
    string(REPLACE ":" ";" _liblist $ENV{LD_LIBRARY_PATH} "" )
    set(_incdir) 
    foreach ( dir ${_liblist}) 
    	set(_incdir ${_incdir} "${dir}/../include")
    endforeach() 
    set(_incdir "${_incdir}" ENV INC ENV INCLUDE ENV CPATH)
  endif ()



if(CHOLMOD_FIND_QUIETLY)
  set(_FIND_AMD_ARG QUIET)
  set(_FIND_COLAMD_ARG QUIET)
endif(CHOLMOD_FIND_QUIETLY)
find_package(AMD ${_FIND_AMD_ARG})
find_package(COLAMD ${_FIND_COLAMD_ARG})

if (AMD_FOUND AND COLAMD_FOUND) 
  find_path(CHOLMOD_CHOLMOD_INCLUDE_DIR cholmod.h
	${SUITESPARSE}/CHOLMOD/Include  #Local Setup
	${SUITESPARSE}/include
	 ${_incdir}
	/usr/include
	/usr/local/include
	/usr/local/include/suitesparse 	#FreeBSD
	/usr/include/suitesparse	#Debian
	/opt/local/include/ufsparse 	#Macports 
	NO_DEFAULT_PATH
  )
  find_path(CHOLMOD_CHOLMOD_INCLUDE_DIR cholmod.h) 

set(CHOLMOD_NAMES ${CHOLMOD_NAMES} cholmod libcholmod)
set(CHOLMOD_PATH
	${SUITESPARSE}/CHOLMOD/Lib 
	${SUITESPARSE}/lib
	${_libdir}
	/usr/local/lib64 
 	/usr/local/lib 
	/usr/lib64
	/usr/lib
 	/opt/local/lib	# Macports
	
	)

find_library(CHOLMOD_LIBRARY NAMES ${CHOLMOD_NAMES} PATHS ${CHOLMOD_PATH} NO_DEFAULT_PATH)
find_library(CHOLMOD_LIBRARY NAMES ${CHOLMOD_NAMES} PATHS ${CHOLMOD_PATH})




if (CHOLMOD_LIBRARY AND CHOLMOD_CHOLMOD_INCLUDE_DIR)
      SET(CHOLMOD_INCLUDE_DIR ${CHOLMOD_CHOLMOD_INCLUDE_DIR} ${COLAMD_INCLUDE_DIR} ${AMD_INCLUDE_DIR} )
      SET(CHOLMOD_LIBRARIES ${CHOLMOD_LIBRARY} ${COLAMD_LIBRARIES} ${AMD_LIBRARIES} )
endif (CHOLMOD_LIBRARY AND CHOLMOD_CHOLMOD_INCLUDE_DIR)

endif (AMD_FOUND AND COLAMD_FOUND)

# handle the QUIETLY and REQUIRED arguments and set CHOLMOD_FOUND to TRUE if
# all listed variables are TRUE
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(CHOLMOD  DEFAULT_MSG  CHOLMOD_LIBRARY CHOLMOD_CHOLMOD_INCLUDE_DIR)

mark_as_advanced(CHOLMOD_CHOLMOD_INCLUDE_DIR CHOLMOD_LIBRARY )
