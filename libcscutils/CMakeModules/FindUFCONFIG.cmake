# - Find the UFconfig include
#
# This module defines
#  UFCONFIG_INCLUDE_DIR, where to find UFconfig.h, etc.
#  UFCONFIG_FOUND, If false, do not try to use UFconfig.

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
# 	- Jan 28, 2011 	Yoann Le Bars
# 	- Feb 21, 2011  Martin Koehler
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




find_path(UFCONFIG_UFCONFIG_INCLUDE_DIR UFconfig.h
	${SUITESPARSE}/UFconfig  	# Local Setup
	${SUITESPARSE}/include		# 
	${_incdir}
	/usr/include
	/usr/local/include
	/opt/local/include/ufsparse	# MacOS X - MacPorts
	/usr/local/include/suitesparse 	# FreeBSD
	/usr/include/suitesparse	# Debian
	NO_DEFAULT_PATH
  )
find_path(UFCONFIG_UFCONFIG_INCLUDE_DIR UFConfig.h) 

if (UFCONFIG_UFCONFIG_INCLUDE_DIR )
      SET(UFCONFIG_INCLUDE_DIR ${UFCONFIG_UFCONFIG_INCLUDE_DIR} )
endif (UFCONFIG_UFCONFIG_INCLUDE_DIR)


# handle the QUIETLY and REQUIRED arguments and set UFconfig_FOUND to TRUE if
# all listed variables are TRUE
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(UFCONFIG  DEFAULT_MSG  UFCONFIG_UFCONFIG_INCLUDE_DIR)

mark_as_advanced(UFCONFIG_UFCONFIG_INCLUDE_DIR )
