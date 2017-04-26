# - Find the CXSPARSE includes and library
#
# This module defines
#  CSPARSE_INCLUDE_DIR, where to find umfpack.h, etc.
#  CSPARSE_LIBRARIES, the libraries to link against to use CSPARSE.
#  CSPARSE_FOUND, If false, do not try to use CSPARSE.
# also defined, but not for general use are
#  CSPARSE_LIBRARY, where to find the CSPARSE library.
#
#
# A system wide installed CSPARSE is mapped to CXSPARSE on nearly all systems
# (tested on FreeBSD, Debian GNU/Linux, Gentoo)

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
# 	- Jan. 28, 2011  - Yoann Le Bars
# 	- Feb. 21, 2011  - Martin Koehler
#	- Apr. 1, 2011 - Martin Koehler
#       - June 25, 2013 	Martin Koehler, add _libdir and _incdir 

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
    string(REPLACE ":" ";" _liblist $ENV{DYLD_LIBRARY_PATH} "")
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




find_path(CSPARSE_CSPARSE_INCLUDE_DIR cs.h
	${SUITESPARSE}/CXSparse/Include  # Local Setup
	${SUITESPARSE}/CSparse/Include  # Local Setup
	${SUITESPARSE}/include		# user defined setups
	${_incdir}
	/usr/include
	/usr/local/include
	/opt/local/include/ufsparse	# MacOS X - MacPorts
	/usr/local/include/suitesparse 	# FreeBSD
	/usr/include/suitesparse	# Debian
	NO_DEFAULT_PATH
  )
find_path(CSPARSE_CSPARSE_INCLUDE_DIR cs.h) 
# set(CSPARSE_NAMES ${CSPARSE_NAMES} cxsparse libcxsparse)
set(CSPARSE_NAMES cxsparse libcxsparse csparse libcsparse)
find_library(CSPARSE_LIBRARY NAMES ${CSPARSE_NAMES} PATHS
	${SUITESPARSE}/lib
	${SUITESPARSE}/CXSparse/Lib  #Local Setup
	${SUITESPARSE}/CSparse/Lib  #Local Setup
	${SUITESPARSE}/lib
	${SUITESPARSE}/lib64
	${_libdir}
	/opt/local/lib			# MacOS X - MacPorts
	/usr/local/lib64
	/usr/local/lib
	/usr/lib64
	/usr/lib
	NO_DEFAULT_PATH
	)
find_library(CSPARSE_LIBRARY NAMES ${CSPARSE_NAMES}) 
if (CSPARSE_LIBRARY AND CSPARSE_CSPARSE_INCLUDE_DIR)
      SET(CSPARSE_INCLUDE_DIR ${CSPARSE_CSPARSE_INCLUDE_DIR} )
      SET(CSPARSE_LIBRARIES ${CSPARSE_LIBRARY} )
endif (CSPARSE_LIBRARY AND CSPARSE_CSPARSE_INCLUDE_DIR)

# handle the QUIETLY and REQUIRED arguments and set AMD_FOUND to TRUE if
# all listed variables are TRUE
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(CSPARSE DEFAULT_MSG CSPARSE_LIBRARY CSPARSE_CSPARSE_INCLUDE_DIR)

mark_as_advanced(CSPARSE_CSPARSE_INCLUDE_DIR CSPARSE_LIBRARY)
