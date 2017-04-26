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

find_path(MESS_MESS_INCLUDE_DIR mess/mess.h
	/usr/include
	/usr/local/include
	${MESS}/include 	#LOCALSETUP
  )


if(MESS_FIND_QUIETLY)
  set(_FIND_UFCONFIG_ARG QUIET)
  set(_FIND_AMD_ARG QUIET)
  set(_FIND_CHOLMOD_ARG QUIET)
  set(_FIND_COLAMD_ARG QUIET)
  set(_FIND_UMPFACK_ARG QUIET)
  set(_FIND_CSPARSE_ARG QUIET)
  set(_FIND_RRQR_ARG QUIET)
  set(_FIND_BLAS_ARG QUIET)
  set(_FIND_LAPACK_ARG QUIET)
  set(_FIND_X11_ARG QUIET) 
  set(_FIND_ZLIB__ARG QUIET) 
  set(_FIND_BZ2_ARG QUIET)
  set(_FIND_LUA_ARG QUIET) 
endif(MESS_FIND_QUIETLY)

find_package(UFCONFIG ${_FIND_UFCONFIG_ARG} 	REQUIRED)
find_package(AMD ${_FIND_AMD_ARG} 		REQUIRED)
find_package(CHOLMOD ${_FIND_CHOLMOD_ARG} 	REQUIRED)
find_package(COLAMD ${_FIND_COLAMD_ARG}   	REQUIRED) 
find_package(UMFPACK ${_FIND_UMPFACK_ARG} 	REQUIRED)
find_package(CSPARSE ${_FIND_CSPARSE_ARG} 	REQUIRED)
find_package(BLAS ${_FIND_BLAS_ARG} 		REQUIRED)
find_package(LAPACK ${_FIND_LAPACK_ARG} 	REQUIRED)
SET (MESS_LIBRARIES ${UMFPACK_LIBRARIES} ${AMD_LIBRARIES} ${COLAMD_LIBRARIES} ${CHOLMOD_LIBRARIES} ${CSPARSE_LIBRARIES})
SET (MESS_INCLUDE_DIR ${UMFPACK_INCLUDE_DIR} ${AMD_INCLUDE_DIR} ${COLAMD_INCLUDE_DIR} ${CHOLMOD_INCLUDE_DIR} ${CSPARSE_INCLUDE_DIR})

find_package(RRQR ${_FIND_RRQR_ARG})
IF ( RRQR_FOUND ) 
	SET (MESS_LIBRARIES ${MESS_LIBRARIES} ${RRQR_LIBRARIES})
ENDIF ( RRQR_FOUND )

find_package(X11 ${_FIND_X11_ARG})
IF ( X11_FOUND ) 
	SET (MESS_LIBRARIES ${MESS_LIBRARIES} ${X11_LIBRARIES})
	SET (MESS_INCLUDE_DIR ${MESS_INCLUDE_DIR} ${X11_INCLUDE_DIR})
ENDIF ( X11_FOUND )

IF (X11_Xpm_FOUND)
	SET (MESS_LIBRARIES ${MESS_LIBRARIES} ${X11_Xpm_LIB})
	SET (MESS_INCLUDE_DIR ${MESS_INCLUDE_DIR} ${X11_Xpm_INCLUDE_PATH} )
ENDIF(X11_Xpm_FOUND)

find_package(ZLIB ${_FIND_ZLIB__ARG})
IF ( ZLIB_FOUND ) 
	SET (MESS_LIBRARIES ${MESS_LIBRARIES} ${ZLIB_LIBRARIES})
	SET (MESS_INCLUDE_DIR ${MESS_INCLUDE_DIR} ${ZLIB_INCLUDE_DIR}) 
ENDIF ( ZLIB_FOUND)

find_package(BZip2 ${_FIND_BZ2_ARG})

IF ( BZIP2_FOUND)
	SET (MESS_LIBRARIES ${MESS_LIBRARIES} ${BZIP2_LIBRARIES})
	SET (MESS_INCLUDE_DIR ${MESS_INCLUDE_DIR} ${BZIP2_INCLUDE_DIR})
ENDIF ( BZIP2_FOUND )

#find_package(Lua51 ${_FIND_LUA_ARG}) 
#IF ( LUA51_FOUND ) 
#	SET ( MESS_LIBRARIES ${MESS_LIBRARIES} ${LUA_LIBRARIES})
#	SET ( MESS_INCLUDE_DIR ${MESS_INCLUDE_DIR} ${LUA_INCLUDE_DIR})
#ENDIF ( LUA51_FOUND) 

FIND_PACKAGE(Threads REQUIRED)
SET(MESS_LIBRARIES ${MESS_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})

SET (MESS_LIBRARIES ${MESS_LIBRARIES} ${LAPACK_LIBRARIES}  ${BLAS_LIBRARIES} )

FIND_PACKAGE(OpenMP)
	IF ( OPENMP_FOUND) 
		SET ( CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}" ) 
		SET ( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenMP_C_FLAGS}")
		SET( CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
		SET ( CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} ${OpenMP_LD_FLAGS}" )
		SET ( OPENMP TRUE) 
		SET ( HAVE_OPENMP TRUE) 
	ENDIF (OPENMP_FOUND )  


set(MESS_NAMES ${MESS_NAMES} mess libmess)
set(MESS_PATH
	/usr/local/lib64 
 	/usr/local/lib 
	/usr/lib64
	/usr/lib
	${MESS}/lib ) 
find_library(MESS_LIB NAMES ${MESS_NAMES} PATHS ${MESS_PATH} DOC "Search MESS library" )


MESSAGE (STATUS "MESS include: ${MESS_MESS_INCLUDE_DIR}") 
MESSAGE (STATUS "MESS library: ${MESS_LIB}") 

if (MESS_LIB AND MESS_MESS_INCLUDE_DIR)
      SET(MESS_INCLUDE_DIR ${MESS_MESS_INCLUDE_DIR} ${MESS_INCLUDE_DIR} )
      SET(MESS_LIBRARIES ${MESS_LIB} ${MESS_LIBRARIES} )
endif (MESS_LIB AND MESS_MESS_INCLUDE_DIR)

# handle the QUIETLY and REQUIRED arguments and set UMFPACK_FOUND to TRUE if
# all listed variables are TRUE
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(MESS  DEFAULT_MSG  MESS_LIB MESS_MESS_INCLUDE_DIR)

mark_as_advanced(MESS_MESS_INCLUDE_DIR MESS_LIB )
