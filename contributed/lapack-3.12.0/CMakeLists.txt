cmake_minimum_required(VERSION 3.9)

project(LAPACK Fortran C)

set(LAPACK_MAJOR_VERSION 3)
set(LAPACK_MINOR_VERSION 12)
set(LAPACK_PATCH_VERSION 0)
set(
  LAPACK_VERSION
  ${LAPACK_MAJOR_VERSION}.${LAPACK_MINOR_VERSION}.${LAPACK_PATCH_VERSION}
  )

# Add the CMake directory for custom CMake modules
set(CMAKE_MODULE_PATH "${LAPACK_SOURCE_DIR}/CMAKE" ${CMAKE_MODULE_PATH})

# Export all symbols on Windows when building shared libraries
SET(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS TRUE)

# Set a default build type if none was specified
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
  # Set the possible values of build type for cmake-gui
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo" "Coverage")
endif()

include(PreventInSourceBuilds)
include(PreventInBuildInstalls)

# Check if recursive flag exists
include(CheckFortranCompilerFlag)
if(CMAKE_Fortran_COMPILER_ID STREQUAL Flang)
  check_fortran_compiler_flag("-Mrecursive" _MrecursiveFlag)
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
  check_fortran_compiler_flag("-frecursive" _frecursiveFlag)
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
  check_fortran_compiler_flag("-recursive" _recursiveFlag)
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL XL)
  check_fortran_compiler_flag("-qrecur" _qrecurFlag)
else()
  message(WARNING "Fortran local arrays should be allocated on the stack."
    " Please use a compiler which guarantees that feature."
    " See https://github.com/Reference-LAPACK/lapack/pull/188 and references therein.")
endif()

# Add recursive flag
if(_MrecursiveFlag)
  string(REGEX MATCH "-Mrecursive" output_test <string> "${CMAKE_Fortran_FLAGS}")
  if(NOT output_test)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Mrecursive"
      CACHE STRING "Recursive flag must be set" FORCE)
  endif()
elseif(_frecursiveFlag)
  string(REGEX MATCH "-frecursive" output_test <string> "${CMAKE_Fortran_FLAGS}")
  if(NOT output_test)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -frecursive"
      CACHE STRING "Recursive flag must be set" FORCE)
  endif()
elseif(_recursiveFlag)
  string(REGEX MATCH "-recursive" output_test <string> "${CMAKE_Fortran_FLAGS}")
  if(NOT output_test)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -recursive"
      CACHE STRING "Recursive flag must be set" FORCE)
  endif()
elseif(_qrecurFlag)
  string(REGEX MATCH "-qrecur" output_test <string> "${CMAKE_Fortran_FLAGS}")
  if(NOT output_test)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -qrecur"
      CACHE STRING "Recursive flag must be set" FORCE)
  endif()
endif()

if(UNIX)
  if(CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict")
  endif()
  if(CMAKE_Fortran_COMPILER_ID STREQUAL XL)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -qnosave -qstrict")
  endif()
# Delete libmtsk in linking sequence for Sun/Oracle Fortran Compiler.
# This library is not present in the Sun package SolarisStudio12.3-linux-x86-bin
  string(REPLACE \;mtsk\; \; CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES "${CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES}")
endif()

if(CMAKE_Fortran_COMPILER_ID STREQUAL Compaq)
  if(WIN32)
    if(CMAKE_GENERATOR STREQUAL "NMake Makefiles")
      get_filename_component(CMAKE_Fortran_COMPILER_CMDNAM ${CMAKE_Fortran_COMPILER} NAME_WE)
      message(STATUS "Using Compaq Fortran compiler with command name ${CMAKE_Fortran_COMPILER_CMDNAM}")
      set(cmd ${CMAKE_Fortran_COMPILER_CMDNAM})
      string(TOLOWER "${cmd}" cmdlc)
      if(cmdlc STREQUAL "df")
        message(STATUS "Assume the Compaq Visual Fortran Compiler is being used")
        set(CMAKE_Fortran_USE_RESPONSE_FILE_FOR_OBJECTS 1)
        set(CMAKE_Fortran_USE_RESPONSE_FILE_FOR_INCLUDES 1)
        #This is a workaround that is needed to avoid forward-slashes in the
        #filenames listed in response files from incorrectly being interpreted as
        #introducing compiler command options
        if(${BUILD_SHARED_LIBS})
          message(FATAL_ERROR "Making of shared libraries with CVF has not been tested.")
        endif()
        set(str "NMake version 9 or later should be used. NMake version 6.0 which is\n")
        set(str "${str}   included with the CVF distribution fails to build Lapack because\n")
        set(str "${str}   the number of source files exceeds the limit for NMake v6.0\n")
        message(STATUS ${str})
        set(CMAKE_Fortran_LINK_EXECUTABLE "LINK /out:<TARGET> <LINK_FLAGS> <LINK_LIBRARIES> <OBJECTS>")
      endif()
    endif()
  endif()
endif()

# Disable Warings for BLAS
if ( CMAKE_Fortran_COMPILER_ID STREQUAL "GNU" )
	SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffloat-store -Wno-conversion -Wno-maybe-uninitialized -Wno-unused-dummy-argument -Wno-unused-variable -Wno-intrinsic-shadow -Wno-surprising -Wno-character-truncation")
endif()

IF ( INTEGER8 STREQUAL ON )
	SET (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${I8_FLAGS}")
	ADD_DEFINITIONS(-DINTEGER8)
ENDIF()



# --------------------------------------------------
# Check for any necessary platform specific compiler flags
include(CheckLAPACKCompilerFlags)
CheckLAPACKCompilerFlags()

# --------------------------------------------------
# Check second function

include(CheckTimeFunction)
set(NONE ${TIME_FUNC})
CHECK_TIME_FUNCTION(NONE TIME_FUNC)
CHECK_TIME_FUNCTION(INT_CPU_TIME TIME_FUNC)
CHECK_TIME_FUNCTION(EXT_ETIME TIME_FUNC)
CHECK_TIME_FUNCTION(EXT_ETIME_ TIME_FUNC)
CHECK_TIME_FUNCTION(INT_ETIME TIME_FUNC)
message(STATUS "--> Will use second_${TIME_FUNC}.f and dsecnd_${TIME_FUNC}.f as timing function.")

set(SECOND_SRC ${LAPACK_SOURCE_DIR}/INSTALL/second_${TIME_FUNC}.f)
set(DSECOND_SRC ${LAPACK_SOURCE_DIR}/INSTALL/dsecnd_${TIME_FUNC}.f)

add_subdirectory(SRC)
