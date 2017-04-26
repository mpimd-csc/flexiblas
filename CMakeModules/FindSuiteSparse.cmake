# - Find the SuiteSparse includes and library
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
#    - Sep. 11, 2012 	Martin Koehler
#    - June 25, 2013 	Martin Koehler, add _libdir and _incdir 

if (WIN32)
    set(_libdir ENV LIB)
    set(_liblist "$ENV{LIB}" "")
    set(_incdir) 
    foreach ( dir ${_liblist}) 
    	set(_incdir ${_incdir} "${dir}/../include")
    endforeach() 
    set(_incdir "${_incdir}" ENV INC ENV INCLUDE ENV CPATH)
elseif (APPLE)
    set(_libdir ENV DYLD_LIBRARY_PATH)
    string(REPLACE ":" ";" _liblist "$ENV{DYLD_LIBRARY_PATH}" "")
    set(_incdir) 
    foreach ( dir ${_liblist}) 
    	set(_incdir ${_incdir} "${dir}/../include")
    endforeach() 
    set(_incdir "${_incdir}" ENV INC ENV INCLUDE ENV CPATH)
else ()
    set(_libdir ENV LD_LIBRARY_PATH)
    string(REPLACE ":" ";" _liblist "$ENV{LD_LIBRARY_PATH}" "")
    set(_incdir) 
    foreach ( dir ${_liblist}) 
    	set(_incdir ${_incdir} "${dir}/../include")
    endforeach() 
    set(_incdir "${_incdir}" ENV INC ENV INCLUDE ENV CPATH)
endif ()

IF ( SuiteSparse_FIND_QUIETLY) 
  set(_FIND_AMD_ARG QUIET)
  set(_FIND_COLAMD_ARG QUIET)
  set(_FIND_UMFPACK_ARG QUIET)
  set(_FIND_CHOLMOD_ARG QUIET)
ENDIF (SuiteSparse_FIND_QUIETLY)


IF (APPLE) 
	find_path(SuiteSparse_SuiteSparse_INCLUDE_DIR SuiteSparse_config.h
		HINTS
		${SUITESPARSE}/SuiteSparse_config/  #Local Setup
		${SUITESPARSE}/include 
		${_incdir}
		/usr/include
		/usr/local/include
		/usr/local/include/suitesparse 	#FreeBSD
		/usr/include/suitesparse	#Debian
		/opt/local/include/ufsparse 	#Macports
		/opt/local/include
		NO_DEFAULT_PATH
  	)
	find_path(SuiteSparse_SuiteSparse_INCLUDE_DIR SuiteSparse_config.h)
	set(SuiteSparse_NAMES ${SuiteSparse_NAMES}  suitesparse suitesparse_config libsuitesparse SuiteSparse libSuiteSparse)
	set(Suitesparse_PATH
		${SUITESPARSE}/UMFPACK/Lib 
		${SUITESPARSE}/lib
		${_libdir}
		/usr/local/lib64 
		/usr/local/lib 
		/usr/lib64
		/usr/lib
		/opt/local/lib	# Macports
		${SUITESPARSE}/SuiteSparse_config/ 
		${SUITESPARSE}/lib
	)
	find_library(SuiteSparse_LIBRARY NAMES ${SuiteSparse_NAMES} PATHS ${Suitesparse_PATH} NO_DEFAULT_PATH)
	find_library(SuiteSparse_LIBRARY NAMES ${SuiteSparse_NAMES} PATHS ${Suitesparse_PATH})


	if (SuiteSparse_LIBRARY AND SuiteSparse_SuiteSparse_INCLUDE_DIR)
		SET(SuiteSparse_INCLUDE_DIR ${SuiteSparse_SuiteSparse_INCLUDE_DIR} )
		SET(SuiteSparse_LIBRARIES ${SuiteSparse_LIBRARY} )
		SET(SuiteSparse_FOUND TRUE) 
	endif (SuiteSparse_LIBRARY AND SuiteSparse_SuiteSparse_INCLUDE_DIR)


ELSE (APPLE)
	find_package(AMD      ${_FIND_AMD_ARG})
	find_package(COLAMD   ${_FIND_COLAMD_ARG})
	find_package(UMFPACK  ${_FIND_UMFPACK_ARG})
	find_package(CHOLMOD  ${_FIND_CHOLMOD_ARG})

	if(AMD_FOUND AND COLAMD_FOUND AND UMFPACK_FOUND AND CHOLMOD_FOUND) 
		find_path(SuiteSparse_SuiteSparse_INCLUDE_DIR SuiteSparse_config.h
			HINTS 
			${SUITESPARSE}/SuiteSparse_config/  #Local Setup
			${SUITESPARSE}/include 
			${_incdir}
			/usr/include
			/usr/local/include
			/usr/local/include/suitesparse 	#FreeBSD
			/usr/include/suitesparse	#Debian
			/opt/local/include/ufsparse 	#Macports
			/opt/local/include
  		)
		
		set(SuiteSparse_NAMES ${SuiteSparse_NAMES} suitesparseconfig libsuitesparseconfig suitesparse_config)
		set(SuiteSparse_PATH
			${SUITESPARSE}/UMFPACK/Lib 
			${SUITESPARSE}/lib
			${_libdir}
			/usr/local/lib64 
			/usr/local/lib 
			/usr/lib64
			/usr/lib
			/opt/local/lib	# Macports
			${SUITESPARSE}/SuiteSparse_config/ 
			${SUITESPARSE}/lib
		)
 		find_library(SuiteSparse_LIBRARY NAMES ${SuiteSparse_NAMES} HINTS ${SuiteSparse_PATH} PATHS ${SuiteSparse_PATH} )

		if (SuiteSparse_LIBRARY AND SuiteSparse_SuiteSparse_INCLUDE_DIR)
			SET(SuiteSparse_INCLUDE_DIR ${SuiteSparse_SuiteSparse_INCLUDE_DIR} ${UMFPACK_INCLUDE_DIR} ${CHOLMOD_INCLUDE_DIR} ${COLAMD_INCLUDE_DIR} ${AMD_INCLUDE_DIR} )
			SET(SuiteSparse_LIBRARIES ${SuiteSparse_LIBRARY} ${UMFPACK_LIBRARIES} ${COLAMD_LIBRARIES} ${AMD_LIBRARIES} ${CHOLMOD_LIBRARIES} ${SuiteSparse_LIBRARY})
			SET(SuiteSparse_FOUND TRUE)
		endif (SuiteSparse_LIBRARY AND SuiteSparse_SuiteSparse_INCLUDE_DIR)

		#		MESSAGE(STATUS "SuiteSparse_LIBRARY: ${SuiteSparse_LIBRARY}")
		#MESSAGE(STATUS "SuiteSparse_SuiteSparse_INCLUDE_DIR: ${SuiteSparse_SuiteSparse_INCLUDE_DIR}") 

		# handle the QUIETLY and REQUIRED arguments and set UMFPACK_FOUND to TRUE if
		# all listed variables are TRUE
	endif(AMD_FOUND AND COLAMD_FOUND AND UMFPACK_FOUND AND CHOLMOD_FOUND) 


ENDIF(APPLE)

include(FindPackageHandleStandardArgs)
mark_as_advanced(SuiteSparse_SuiteSparse_INCLUDE_DIR  SuiteSparse_LIBRARY) 
find_package_handle_standard_args(SuiteSparse  DEFAULT_MSG  SuiteSparse_LIBRARIES SuiteSparse_INCLUDE_DIR)


