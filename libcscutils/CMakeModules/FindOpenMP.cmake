#.rst:
# FindOpenMP
# ----------
#
# Finds OpenMP support
#
# This module can be used to detect OpenMP support in a compiler.  If
# the compiler supports OpenMP, the flags required to compile with
# OpenMP support are returned in variables for the different languages.
# The variables may be empty if the compiler does not need a special
# flag to support OpenMP.
#
# The following variables are set:
#
# ::
#
#    OpenMP_C_FLAGS - flags to add to the C compiler for OpenMP support
#    OpenMP_CXX_FLAGS - flags to add to the CXX compiler for OpenMP support
#    OpenMP_Fortran_FLAGS - flags to add to the Fortran compiler for OpenMP support
#    OPENMP_FOUND - true if openmp is detected
#
#
#
# Supported compilers can be found at
# http://openmp.org/wp/openmp-compilers/

#=============================================================================
# Copyright 2009 Kitware, Inc.
# Copyright 2008-2009 André Rigland Brodtkorb <Andre.Brodtkorb@ifi.uio.no>
# Copyright 2012 Rolf Eike Beer <eike@sf-mail.de>
# Copyright 2014 Nicolas Bock <nicolasbock@gmail.com>
#
# Distributed under the OSI-approved BSD License (the "License");
# see accompanying file Copyright.txt for details.
#
# This software is distributed WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the License for more information.
#=============================================================================
# (To distribute this file outside of CMake, substitute the full
#  License text for the above reference.)

set(_OPENMP_REQUIRED_VARS)
set(CMAKE_REQUIRED_QUIET_SAVE ${CMAKE_REQUIRED_QUIET})
set(CMAKE_REQUIRED_QUIET ${OpenMP_FIND_QUIETLY})

function(_OPENMP_FLAG_CANDIDATES LANG)
  set(OpenMP_FLAG_CANDIDATES
    #Empty, if compiler automatically accepts openmp
    " "
    #GNU
    "-fopenmp"
    #Clang
    "-fopenmp=libomp"
    #Microsoft Visual Studio
    "/openmp"
    #Intel windows
    "-Qopenmp"
    "-qopenmp" 
    #PathScale, Intel
    "-openmp"
    #Sun
    "-xopenmp"
    #HP
    "+Oopenmp"
    #IBM XL C/c++
    "-qsmp=omp"
    #Portland Group, MIPSpro
    "-mp"
  )

  set(OMP_FLAG_GNU "-fopenmp")
  set(OMP_FLAG_Clang "-fopenmp=libomp")
  set(OMP_FLAG_HP "+Oopenmp")
  if(WIN32)
    set(OMP_FLAG_Intel "-Qopenmp")
  elseif(CMAKE_${LANG}_COMPILER_ID STREQUAL "Intel" AND
         "${CMAKE_${LANG}_COMPILER_VERSION}" VERSION_LESS "15.0.0.20140528")
    set(OMP_FLAG_Intel "-openmp")
  else()
    set(OMP_FLAG_Intel "-qopenmp")
  endif()
  set(OMP_FLAG_MIPSpro "-mp")
  set(OMP_FLAG_MSVC "/openmp")
  set(OMP_FLAG_PathScale "-openmp")
  set(OMP_FLAG_PGI "-mp")
  set(OMP_FLAG_SunPro "-xopenmp")
  set(OMP_FLAG_XL "-qsmp=omp")
  set(OMP_FLAG_Cray " ")

  # Move the flag that matches the compiler to the head of the list,
  # this is faster and doesn't clutter the output that much. If that
  # flag doesn't work we will still try all.
  if(OMP_FLAG_${CMAKE_${LANG}_COMPILER_ID})
    list(REMOVE_ITEM OpenMP_FLAG_CANDIDATES "${OMP_FLAG_${CMAKE_${LANG}_COMPILER_ID}}")
    list(INSERT OpenMP_FLAG_CANDIDATES 0 "${OMP_FLAG_${CMAKE_${LANG}_COMPILER_ID}}")
  endif()

  set(OpenMP_${LANG}_FLAG_CANDIDATES "${OpenMP_FLAG_CANDIDATES}" PARENT_SCOPE)
endfunction()

# sample openmp source code to test
set(OpenMP_C_TEST_SOURCE
"
#include <omp.h>
int main() {
#ifdef _OPENMP
  return 0;
#else
  breaks_on_purpose
#endif
}
")

# same in Fortran
set(OpenMP_Fortran_TEST_SOURCE
  "
      program test
      use omp_lib
      integer :: n
      n = omp_get_num_threads()
      end program test
  "
  )

# check c compiler
if(CMAKE_C_COMPILER_LOADED)
  # if these are set then do not try to find them again,
  # by avoiding any try_compiles for the flags
  if(OpenMP_C_FLAGS)
    unset(OpenMP_C_FLAG_CANDIDATES)
  else()
    _OPENMP_FLAG_CANDIDATES("C")
    include(CheckCSourceCompiles)
  endif()

  foreach(FLAG IN LISTS OpenMP_C_FLAG_CANDIDATES)
    set(SAFE_CMAKE_REQUIRED_FLAGS "${CMAKE_REQUIRED_FLAGS}")
    set(CMAKE_REQUIRED_FLAGS "${FLAG}")
    unset(OpenMP_FLAG_DETECTED CACHE)
    if(NOT CMAKE_REQUIRED_QUIET)
      message(STATUS "Try OpenMP C flag = [${FLAG}]")
    endif()
    check_c_source_compiles("${OpenMP_C_TEST_SOURCE}" OpenMP_FLAG_DETECTED)
    set(CMAKE_REQUIRED_FLAGS "${SAFE_CMAKE_REQUIRED_FLAGS}")
    if(OpenMP_FLAG_DETECTED)
      set(OpenMP_C_FLAGS_INTERNAL "${FLAG}")
      break()
    endif()
  endforeach()

  set(OpenMP_C_FLAGS "${OpenMP_C_FLAGS_INTERNAL}"
    CACHE STRING "C compiler flags for OpenMP parallization")

  list(APPEND _OPENMP_REQUIRED_VARS OpenMP_C_FLAGS)
  unset(OpenMP_C_FLAG_CANDIDATES)
endif()

# check cxx compiler
if(CMAKE_CXX_COMPILER_LOADED)
  # if these are set then do not try to find them again,
  # by avoiding any try_compiles for the flags
  if(OpenMP_CXX_FLAGS)
    unset(OpenMP_CXX_FLAG_CANDIDATES)
  else()
    _OPENMP_FLAG_CANDIDATES("CXX")
    include(CheckCXXSourceCompiles)

    # use the same source for CXX as C for now
    set(OpenMP_CXX_TEST_SOURCE ${OpenMP_C_TEST_SOURCE})
  endif()

  foreach(FLAG IN LISTS OpenMP_CXX_FLAG_CANDIDATES)
    set(SAFE_CMAKE_REQUIRED_FLAGS "${CMAKE_REQUIRED_FLAGS}")
    set(CMAKE_REQUIRED_FLAGS "${FLAG}")
    unset(OpenMP_FLAG_DETECTED CACHE)
    if(NOT CMAKE_REQUIRED_QUIET)
      message(STATUS "Try OpenMP CXX flag = [${FLAG}]")
    endif()
    check_cxx_source_compiles("${OpenMP_CXX_TEST_SOURCE}" OpenMP_FLAG_DETECTED)
    set(CMAKE_REQUIRED_FLAGS "${SAFE_CMAKE_REQUIRED_FLAGS}")
    if(OpenMP_FLAG_DETECTED)
      set(OpenMP_CXX_FLAGS_INTERNAL "${FLAG}")
      break()
    endif()
  endforeach()

  set(OpenMP_CXX_FLAGS "${OpenMP_CXX_FLAGS_INTERNAL}"
    CACHE STRING "C++ compiler flags for OpenMP parallization")

  list(APPEND _OPENMP_REQUIRED_VARS OpenMP_CXX_FLAGS)
  unset(OpenMP_CXX_FLAG_CANDIDATES)
  unset(OpenMP_CXX_TEST_SOURCE)
endif()

# check Fortran compiler
if(CMAKE_Fortran_COMPILER_LOADED)
  # if these are set then do not try to find them again,
  # by avoiding any try_compiles for the flags
  if(OpenMP_Fortran_FLAGS)
    unset(OpenMP_Fortran_FLAG_CANDIDATES)
  else()
    _OPENMP_FLAG_CANDIDATES("Fortran")
    include(CheckFortranSourceCompiles)
  endif()

  foreach(FLAG IN LISTS OpenMP_Fortran_FLAG_CANDIDATES)
    set(SAFE_CMAKE_REQUIRED_FLAGS "${CMAKE_REQUIRED_FLAGS}")
    set(CMAKE_REQUIRED_FLAGS "${FLAG}")
    unset(OpenMP_FLAG_DETECTED CACHE)
    if(NOT CMAKE_REQUIRED_QUIET)
      message(STATUS "Try OpenMP Fortran flag = [${FLAG}]")
    endif()
    check_fortran_source_compiles("${OpenMP_Fortran_TEST_SOURCE}" OpenMP_FLAG_DETECTED)
    set(CMAKE_REQUIRED_FLAGS "${SAFE_CMAKE_REQUIRED_FLAGS}")
    if(OpenMP_FLAG_DETECTED)
      set(OpenMP_Fortran_FLAGS_INTERNAL "${FLAG}")
      break()
    endif()
  endforeach()

  set(OpenMP_Fortran_FLAGS "${OpenMP_Fortran_FLAGS_INTERNAL}"
    CACHE STRING "Fortran compiler flags for OpenMP parallization")

  list(APPEND _OPENMP_REQUIRED_VARS OpenMP_Fortran_FLAGS)
  unset(OpenMP_Fortran_FLAG_CANDIDATES)
  unset(OpenMP_Fortran_TEST_SOURCE)
endif()

set(CMAKE_REQUIRED_QUIET ${CMAKE_REQUIRED_QUIET_SAVE})

if(_OPENMP_REQUIRED_VARS)
  include(FindPackageHandleStandardArgs)

  find_package_handle_standard_args(OpenMP
                                    REQUIRED_VARS ${_OPENMP_REQUIRED_VARS})

  mark_as_advanced(${_OPENMP_REQUIRED_VARS})

  unset(_OPENMP_REQUIRED_VARS)
else()
  message(SEND_ERROR "FindOpenMP requires C or CXX language to be enabled")
endif()




if (OpenMP_C_FLAGS STREQUAL "-fopenmp")
	set (OpenMP_LD_FLAGS "${OpenMP_C_FLAGS}" )
else (OpenMP_C_FLAGS STREQUAL "-fopenmp")
	set (OpenMP_LD_FLAGS "" )
endif (OpenMP_C_FLAGS STREQUAL "-fopenmp" )


set(OpenMP_Fortran_TEST_SOURCE 
"
PROGRAM OMPTEST
    USE omp_lib
    IF (_OPENMP .LT.  200505 ) THEN 
        WRITE (*,*) \"2.0\" 
    ELSE IF ( _OPENMP .EQ. 200505 ) THEN 
        WRITE (*,*) \"2.5\"
    ELSE IF (_OPENMP .EQ. 200805 )  THEN
        WRITE (*,*) \"3.0\"
    ELSE IF (_OPENMP .EQ. 201107 )  THEN
        WRITE (*,*) \"3.1\" 
    ELSE IF (_OPENMP .EQ. 201307 )  THEN
         WRITE (*,*) \"4.0\" 
    ELSE IF (_OPENMP .EQ. 201511 )  THEN
        WRITE (*,*) \"4.5\" 
    ELSE
        WRITE (*,*) \"0.0\" 
    END IF 
END PROGRAM 
") 


SET (OpenMP_VERSION_SOURCE 
"
#include <stdio.h>

int main(int argc, char **argv)
{
#ifdef _OPENMP
    if ( _OPENMP < 200505 ) 
        printf(\"2.0\")\;
    else if (_OPENMP == 200505 ) 
        printf(\"2.5\")\;
    else if (_OPENMP == 200805 ) 
        printf(\"3.0\")\; 
    else if (_OPENMP == 201107 ) 
        printf(\"3.1\")\; 
    else if (_OPENMP == 201307 ) 
        printf(\"4.0\")\; 
    else if (_OPENMP == 201511 ) 
        printf(\"4.5\")\; 
    else 
        printf(\"0.0\")\; 
#else 
    #error OpenMP not supported
#endif 
    return 0\;
}
") 

IF ( OPENMP_FOUND ) 
	FILE(WRITE ${CMAKE_BINARY_DIR}/openmp_version_test.c ${OpenMP_VERSION_SOURCE}) 
	FILE(WRITE ${CMAKE_BINARY_DIR}/openmp_version_test.cxx ${OpenMP_VERSION_SOURCE}) 

	TRY_RUN(OMP_RUN_STATE_C OMP_COMPILE_STATE_C ${CMAKE_BINARY_DIR} ${CMAKE_BINARY_DIR}/openmp_version_test.c COMPILE_DEFINITIONS ${OpenMP_C_FLAGS} LINK_LIBRARIES ${OpenMP_C_FLAGS} 
		COMPILE_OUTPUT_VARIABLE OUTPUT RUN_OUTPUT_VARIABLE OpenMP_C_VERSION) 
	#		MESSAGE(STATUS "OUTPUT: ${OUTPUT}") 
	#		MESSAGE(STATUS "VERSION: ${OpenMP_VERSION}") 
	#		MESSAGE(STATUS "OMP_RUN_STATE_C  ${OMP_RUN_STATE_C}") 
	#		MESSAGE(STATUS "OMP_COMPILE_STATE_C ${OMP_COMPILE_STATE_C}") 

	TRY_RUN(OMP_RUN_STATE_CXX OMP_COMPILE_STATE_CXX ${CMAKE_BINARY_DIR} ${CMAKE_BINARY_DIR}/openmp_version_test.cxx COMPILE_DEFINITIONS ${OpenMP_CXX_FLAGS}  LINK_LIBRARIES ${OpenMP_CXX_FLAGS}
		COMPILE_OUTPUT_VARIABLE OUTPUT RUN_OUTPUT_VARIABLE OpenMP_CXX_VERSION)

	FILE(REMOVE  ${CMAKE_BINARY_DIR}/openmp_version_test.c ) 
	FILE(REMOVE  ${CMAKE_BINARY_DIR}/openmp_version_test.cxx ) 

	IF (OMP_COMPILE_STATE_C AND "${OMP_RUN_STATE_C}" EQUAL 0 ) 
		MESSAGE(STATUS "C Compiler supports OpenMP ${OpenMP_C_VERSION}")
		SET(OpenMP_C_VERSION ${OpenMP_C_VERSION} CACHE  STRING "Detected OpenMP Version supported by the C Compiler") 
	ELSE() 
		MESSAGE(STATUS "Failed to determine OpenMP Version of the C Compiler") 
		SET (OpenMP_C_VERSION "0.0") 
	ENDIF() 
	IF (OMP_COMPILE_STATE_CXX AND "${OMP_RUN_STATE_CXX}" EQUAL 0 ) 
		MESSAGE(STATUS "C++ Compiler supports OpenMP ${OpenMP_CXX_VERSION}")
		SET(OpenMP_CXX_VERSION ${OpenMP_CXX_VERSION} CACHE  STRING "Detected OpenMP Version supported by the C++ Compiler") 

	ELSE() 
		MESSAGE(STATUS "Failed to determine OpenMP Version of the C++ Compiler") 
		SET (OpenMP_CXX_VERSION "0.0") 
	ENDIF() 

	if(CMAKE_Fortran_COMPILER_LOADED)
		FILE(WRITE ${CMAKE_BINARY_DIR}/openmp_version_test.F90 ${OpenMP_Fortran_TEST_SOURCE}) 
		TRY_RUN(OMP_RUN_STATE_Fortran OMP_COMPILE_STATE_Fortran ${CMAKE_BINARY_DIR} ${CMAKE_BINARY_DIR}/openmp_version_test.F90 COMPILE_DEFINITIONS ${OpenMP_Fortran_FLAGS}  LINK_LIBRARIES ${OpenMP_Fortran_FLAGS}
			COMPILE_OUTPUT_VARIABLE OUTPUT RUN_OUTPUT_VARIABLE OpenMP_Fortran_VERSION)
			# MESSAGE(STATUS "OUTPUT: ${OUTPUT}") 
			# MESSAGE(STATUS "VERSION: ${OpenMP_Fortran_VERSION}") 
			# MESSAGE(STATUS "OMP_RUN_STATE_C  ${OMP_RUN_STATE_Fortran}") 
			# MESSAGE(STATUS "OMP_COMPILE_STATE_C ${OMP_COMPILE_STATE_Fortran}") 

		IF (OMP_COMPILE_STATE_Fortran AND "${OMP_RUN_STATE_Fortran}" EQUAL 0 ) 
			MESSAGE(STATUS "Fortran Compiler supports OpenMP ${OpenMP_Fortran_VERSION}")
			SET(OpenMP_Fortran_VERSION ${OpenMP_Fortran_VERSION} CACHE  STRING "Detected OpenMP Version supported by the Fortran Compiler") 

		ELSE() 
			MESSAGE(STATUS "Failed to determine OpenMP Version of the Fortran Compiler") 
			SET (OpenMP_Fortran_VERSION "0.0") 
		ENDIF() 
		FILE(REMOVE  ${CMAKE_BINARY_DIR}/openmp_version_test.F90) 
	ENDIF() 
ENDIF()

if(CMAKE_Fortran_COMPILER_LOADED)
	mark_as_advanced(
	  OpenMP_C_VERSION 
	  OpenMP_CXX_VERSION
	  OpenMP_Fortran_VERSION
	)

ELSE () 
	mark_as_advanced(
	  OpenMP_C_VERSION 
	  OpenMP_CXX_VERSION
	)
ENDIF()

