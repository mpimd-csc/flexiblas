#[=======================================================================[.rst:
FindTATLAS
---------

Find the Threaded ATLAS implementation of BLAS

#]=======================================================================]

include(CheckFunctionExists)
include(CheckFortranFunctionExists)
include(CMakePushCheckState)
include(FindPackageHandleStandardArgs)
cmake_push_check_state()
set(CMAKE_REQUIRED_QUIET ${BLAS_FIND_QUIETLY})

set(_blas_ORIG_CMAKE_FIND_LIBRARY_SUFFIXES ${CMAKE_FIND_LIBRARY_SUFFIXES})

macro(Check_Fortran_Libraries LIBRARIES _prefix _name _flags _list _thread)
  # This macro checks for the existence of the combination of fortran libraries
  # given by _list.  If the combination is found, this macro checks (using the
  # Check_Fortran_Function_Exists macro) whether can link against that library
  # combination using the name of a routine given by _name using the linker
  # flags given by _flags.  If the combination of libraries is found and passes
  # the link test, LIBRARIES is set to the list of complete library paths that
  # have been found.  Otherwise, LIBRARIES is set to FALSE.

  # N.B. _prefix is the prefix applied to the names of all cached variables that
  # are generated internally and marked advanced by this macro.

  set(_libraries_work TRUE)
  set(${LIBRARIES})
  set(_combined_name)
  if (NOT _libdir)
    if (WIN32)
      set(_libdir ENV LIB)
    elseif (APPLE)
      set(_libdir ENV DYLD_LIBRARY_PATH)
    else ()
      set(_libdir ENV LD_LIBRARY_PATH)
    endif ()
  endif ()

  # if (DEFINED ARGN)
    list(APPEND _libdir ${ARGN})
  # endif()
  list(APPEND _libdir "${CMAKE_C_IMPLICIT_LINK_DIRECTORIES}")
  message(STATUS "Search dirs for ATLAS(Threaed) ${_libdir}")

  foreach(_library ${_list})
    set(_combined_name ${_combined_name}_${_library})
    if(NOT "${_thread}" STREQUAL "")
      set(_combined_name ${_combined_name}_thread)
    endif()
    if(_libraries_work)
      if (BLA_STATIC)
        if (WIN32)
          set(CMAKE_FIND_LIBRARY_SUFFIXES .lib ${CMAKE_FIND_LIBRARY_SUFFIXES})
        endif ()
        if (APPLE)
          set(CMAKE_FIND_LIBRARY_SUFFIXES .lib ${CMAKE_FIND_LIBRARY_SUFFIXES})
        else ()
          set(CMAKE_FIND_LIBRARY_SUFFIXES .a ${CMAKE_FIND_LIBRARY_SUFFIXES})
        endif ()
      else ()
        if (CMAKE_SYSTEM_NAME STREQUAL "Linux")
          # for ubuntu's libblas3gf and liblapack3gf packages
          set(CMAKE_FIND_LIBRARY_SUFFIXES ${CMAKE_FIND_LIBRARY_SUFFIXES} .so.3gf)
        endif ()
      endif ()
      find_library(${_prefix}_${_library}_LIBRARY
        NAMES ${_library}
        PATHS ${_libdir}
        )
      mark_as_advanced(${_prefix}_${_library}_LIBRARY)
      set(${LIBRARIES} ${${LIBRARIES}} ${${_prefix}_${_library}_LIBRARY})
      set(_libraries_work ${${_prefix}_${_library}_LIBRARY})
    endif()
  endforeach()
  if(_libraries_work)
    # Test this combination of libraries.
    set(CMAKE_REQUIRED_LIBRARIES ${_flags} ${${LIBRARIES}} ${_thread})
    #  message("DEBUG: CMAKE_REQUIRED_LIBRARIES = ${CMAKE_REQUIRED_LIBRARIES}")
    if (CMAKE_Fortran_COMPILER_LOADED)
      check_fortran_function_exists("${_name}" ${_prefix}${_combined_name}_WORKS)
    else()
      check_function_exists("${_name}_" ${_prefix}${_combined_name}_WORKS)
    endif()
    set(CMAKE_REQUIRED_LIBRARIES)
    set(_libraries_work ${${_prefix}${_combined_name}_WORKS})
  endif()
  if(_libraries_work)
    if("${_list}" STREQUAL "")
      set(${LIBRARIES} "${LIBRARIES}-PLACEHOLDER-FOR-EMPTY-LIBRARIES")
    else()
      set(${LIBRARIES} ${${LIBRARIES}} ${_thread})  # for static link
    endif()
  else()
    set(${LIBRARIES} FALSE)
  endif()
  #message("DEBUG: ${LIBRARIES} = ${${LIBRARIES}}")
endmacro()

SET(CAND_1 "ptf77blas;tatlas")
SET(CAND_2 "tatlas")

SET(CANDIDATES
    CAND_1 CAND_2)
FOREACH(CANDIDATE IN LISTS CANDIDATES)
    MESSAGE(STATUS "Checking ATLAS (threaded) in ${${CANDIDATE}}")
    check_fortran_libraries(
        TATLAS_LIBRARIES
        TATLAS
          dgemm
          ""
          "${${CANDIDATE}}"
          ""
          "/usr/lib/atlas"
          "/usr/lib64/atlas"
    )
    IF (TATLAS_LIBRARIES)
        SET(TATLAS_FOUND TRUE)
        BREAK()
    ENDIF()
ENDFOREACH()

IF(TATLAS_LIBRARIES)
    SET(TATLAS_LIBRARY ${TATLAS_LIBRARIES})
ENDIF()

IF (TATLAS_FOUND AND NOT TARGET TATLAS::ATLAS)
    add_library(TATLAS::TATLAS IMPORTED INTERFACE)
    set_property(TARGET TATLAS::TATLAS
        PROPERTY INTERFACE_LINK_LIBRARIES ${TATLAS_LIBRARIES})
endif()

find_package_handle_standard_args(TATLAS REQUIRED_VARS TATLAS_LIBRARIES)

cmake_pop_check_state()
set(CMAKE_FIND_LIBRARY_SUFFIXES ${_blas_ORIG_CMAKE_FIND_LIBRARY_SUFFIXES})
