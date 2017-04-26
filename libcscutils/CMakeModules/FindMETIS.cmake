# - Find the METIS includes and library
#
# This module defines
#  METIS_INCLUDE_DIR, where to find metis.h, etc.
#  METIS_LIBRARIES, the libraries to link against to use METIS.
#  METIS_FOUND, If false, do not try to use METIS.
 
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

  
find_path(METIS_METIS_INCLUDE_DIR metis.h
	${SUITESPARSE}/metis-4.0/Lib  #Local Setup
	${SUITESPARSE}/metis/Lib  #Local Setup
	${_incdir}
	/usr/include
	/usr/local/include
	/usr/local/include/suitesparse 	#FreeBSD
	/usr/include/suitesparse	#Debian
	/opt/local/include/ufsparse 	#Macports 
	NO_DEFAULT_PATH
  )
find_path(METIS_METIS_INCLUDE_DIR metis.h) 

set(METIS_NAMES ${METIS_NAMES} libmetis metis libmetis.a metis.a)
set(METIS_PATH
	${SUITESPARSE}/metis-4.0/
	${SUITESPARSE}/metis/
	${SUITESPARSE}/lib
	${_libdir}
	/usr/local/lib64 
 	/usr/local/lib 
	/usr/lib64
	/usr/lib
 	/opt/local/lib	# Macports
	)
find_library(METIS_LIBRARY NAMES ${METIS_NAMES} PATHS ${METIS_PATH} NO_DEFAULT_PATH)
find_library(METIS_LIBRARY NAMES ${METIS_NAMES} PATHS ${METIS_PATH} )

if (METIS_LIBRARY AND METIS_METIS_INCLUDE_DIR)
      SET(METIS_INCLUDE_DIR ${METIS_METIS_INCLUDE_DIR}) 
      SET(METIS_LIBRARIES ${METIS_LIBRARY} )
endif (METIS_LIBRARY AND METIS_METIS_INCLUDE_DIR)


# handle the QUIETLY and REQUIRED arguments and set METIS_FOUND to TRUE if
# all listed variables are TRUE
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(METIS  DEFAULT_MSG METIS_LIBRARY METIS_METIS_INCLUDE_DIR)

mark_as_advanced(METIS_METIS_INCLUDE_DIR METIS_LIBRARY )
