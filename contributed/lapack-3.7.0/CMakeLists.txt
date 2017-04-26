cmake_minimum_required(VERSION 2.8)
project(LAPACK Fortran)

set(LAPACK_MAJOR_VERSION 3)
set(LAPACK_MINOR_VERSION 7)
set(LAPACK_PATCH_VERSION 0)
set(
  LAPACK_VERSION
  ${LAPACK_MAJOR_VERSION}.${LAPACK_MINOR_VERSION}.${LAPACK_PATCH_VERSION}
  )

# Add the CMake directory for custon CMake modules
set(CMAKE_MODULE_PATH "${LAPACK_SOURCE_DIR}/CMAKE" ${CMAKE_MODULE_PATH})

if (UNIX)
   if ( "${CMAKE_Fortran_COMPILER}" MATCHES "ifort" )
  set( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict" )
   endif ()
   if ( "${CMAKE_Fortran_COMPILER}" MATCHES "xlf" )
  set( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -qnosave -qstrict=none" )
   endif ()
# Delete libmtsk in linking sequence for Sun/Oracle Fortran Compiler.
# This library is not present in the Sun package SolarisStudio12.3-linux-x86-bin
   STRING(REPLACE \;mtsk\; \; CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES "${CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES}")
endif ()
STRING(REPLACE \;mtsk\; \; CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES "${CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES}")



# Disable Warings for BLAS 
if ( CMAKE_Fortran_COMPILER_ID STREQUAL "GNU" ) 
	SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wno-conversion -Wno-maybe-uninitialized -Wno-unused-dummy-argument -Wno-unused-variable -Wno-intrinsic-shadow -Wno-surprising -Wno-character-truncation") 
endif()
# --------------------------------------------------
# Check for any necessary platform specific compiler flags
include( CheckLAPACKCompilerFlags )
CheckLAPACKCompilerFlags()

# --------------------------------------------------
# Check second function

include(CheckTimeFunction)
set(TIME_FUNC NONE ${TIME_FUNC})
CHECK_TIME_FUNCTION(NONE TIME_FUNC)
CHECK_TIME_FUNCTION(INT_CPU_TIME TIME_FUNC)
CHECK_TIME_FUNCTION(EXT_ETIME TIME_FUNC)
CHECK_TIME_FUNCTION(EXT_ETIME_ TIME_FUNC)
CHECK_TIME_FUNCTION(INT_ETIME TIME_FUNC)
message(STATUS "--> Will use second_${TIME_FUNC}.f and dsecnd_${TIME_FUNC}.f as timing function.")

set(SECOND_SRC  ${LAPACK_SOURCE_DIR}/INSTALL/second_${TIME_FUNC}.f)
set(DSECOND_SRC  ${LAPACK_SOURCE_DIR}/INSTALL/dsecnd_${TIME_FUNC}.f)

IF ( INTEGER8 STREQUAL ON ) 
	SET (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${I8_FLAGS}")
	ADD_DEFINITIONS(-DINTEGER8)
ENDIF()

# --------------------------------------------------
# Precision to build
# By default all precisions are generated
add_subdirectory(SRC)




