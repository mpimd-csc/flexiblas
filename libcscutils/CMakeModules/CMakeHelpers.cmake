# This module defines some useful macros/functions for
# CMake.
#
# Changelog:
#    - Aug 18, 2017   Maximilian Behr
#    - Nov 22, 2019   Martin Koehler


## ------- FUNCTION CheckHasModule -----

# include the module CheckCCompilerFlag
INCLUDE(CheckCCompilerFlag)
INCLUDE(CheckCXXCompilerFlag)
INCLUDE(CheckFortranCompilerFlag_meta)


# ------- FUNCTION ADD --------
#
#   Add a value to a variable.
#
# Usage:
#   Instead of:
#        SET(MYVAR ${MYVAR} VALUE1 VALUE2 ...)
#   Use:
#       ADD(MYVAR VALUE1 VALUE2 VALUE3)
#
#
FUNCTION(ADD VAR)
    SET(ARGS "${ARGN}")
    FOREACH(ARG IN LISTS ARGS)
        #update variable with new values
        SET(${VAR}  ${${VAR}} ${ARG})
    ENDFOREACH()
    # make variable available in parent scope
    SET(${VAR} ${${VAR}} PARENT_SCOPE)
ENDFUNCTION()


## ------- FUNCTION SHOW_ALL_VARIABLES --------
#   Show all variables.
#
#   Usage:
#       SHOW_ALL_VARIABLES()
#
FUNCTION(SHOW_ALL_VARIABLES)
    GET_CMAKE_PROPERTY(varnames VARIABLES)
    FOREACH(varname ${varnames})
        MESSAGE(STATUS "${varname}=${${varname}}")
    ENDFOREACH()
ENDFUNCTION()


## ------- FUNCTION SHOW_VARIABLE --------
#   Use MESSAGE to print the value of a variable.
#
#   Usage:
#       SHOW_VARIABLE(VAR1 VAR2 VAR3)
#
#
FUNCTION(SHOW_VARIABLE)
    SET(ARGLIST "${ARGN}")
    FOREACH(ARG IN LISTS ARGLIST)
        MESSAGE(STATUS "${ARG}=${${ARG}}")
    ENDFOREACH()
    #MESSAGE(STATUS "${VAR}=${${VAR}}")
ENDFUNCTION()




## ------- FUNCTION SHOW_PROJECT_INFO --------
#   Show various information about your project.
#
#   Usage:
#       SHOW_PROJECT_INFO()
#
FUNCTION(SHOW_PROJECT_INFO)
    MESSAGE("Project:           ${PROJECT_NAME} (${PROJECT_VERSION})")
    MESSAGE("CMake:             ${CMAKE_VERSION} ")

    # C compiler information
    IF(CMAKE_C_COMPILER_VERSION)
        MESSAGE("C-Compiler:        ${CMAKE_C_COMPILER} (${CMAKE_C_COMPILER_ID}, ${CMAKE_C_COMPILER_VERSION})")
    ELSE()
        MESSAGE("C-Compiler:        ${CMAKE_C_COMPILER} (${CMAKE_C_COMPILER_ID})")
    ENDIF()

    # CXX compiler information
    IF(CMAKE_CXX_COMPILER_VERSION)
        MESSAGE("C++-Compiler:      ${CMAKE_CXX_COMPILER} (${CMAKE_CXX_COMPILER_ID}, ${CMAKE_CXX_COMPILER_VERSION})")
    ELSE()
        MESSAGE("C++-Compiler:      ${CMAKE_CXX_COMPILER} (${CMAKE_CXX_COMPILER_ID})")
    ENDIF()


    # fortran compiler information
    IF(CMAKE_Fortran_COMPILER_VERSION)
        MESSAGE("Fortran-Compiler:  ${CMAKE_Fortran_COMPILER} (${CMAKE_Fortran_COMPILER_ID}, ${CMAKE_Fortran_COMPILER_VERSION})")
    ELSE()
        MESSAGE("Fortran-Compiler:  ${CMAKE_Fortran_COMPILER} (${CMAKE_Fortran_COMPILER_ID})")
    ENDIF()


    MESSAGE("Source Dir:        ${PROJECT_SOURCE_DIR}")
    MESSAGE("Binary Dir:        ${PROJECT_BINARY_DIR}")
    MESSAGE("System:            ${CMAKE_HOST_SYSTEM} ${CMAKE_HOST_SYSTEM_PROCESSOR}")

    MESSAGE("Compiler Options:")
    IF(CMAKE_C_COMPILER)
        MESSAGE(" CMAKE_C_FLAGS               = ${CMAKE_C_FLAGS}")
        MESSAGE(" CMAKE_C_FLAGS_RELEASE       = ${CMAKE_C_FLAGS_RELEASE}")
        MESSAGE(" CMAKE_C_FLAGS_DEBUG         = ${CMAKE_C_FLAGS_DEBUG}")
    ENDIF()
    IF(CMAKE_CXX_COMPILER)
        MESSAGE(" CMAKE_CXX_FLAGS             = ${CMAKE_CXX_FLAGS}")
        MESSAGE(" CMAKE_CXX_FLAGS_RELEASE     = ${CMAKE_CXX_FLAGS_RELEASE}")
        MESSAGE(" CMAKE_CXX_FLAGS_DEBUG       = ${CMAKE_CXX_FLAGS_DEBUG}")
    ENDIF()

    IF(CMAKE_Fortran_COMPILER)
        MESSAGE(" CMAKE_Fortran_FLAGS         = ${CMAKE_Fortran_FLAGS}")
        MESSAGE(" CMAKE_Fortran_FLAGS_RELEASE = ${CMAKE_Fortran_FLAGS_RELEASE}")
        MESSAGE(" CMAKE_Fortran_FLAGS_DEBUG   = ${CMAKE_Fortran_FLAGS_DEBUG}")
    ENDIF()
ENDFUNCTION()



## ------- FUNCTION SET_IFNDEF --------
#   SET a variable to a value if it is not defined.
#
#   Usage:
#       SET_IFNDEF(VAR 1)
#
FUNCTION(SET_IFNDEF VAR VAL)
    IF(NOT DEFINED ${VAR})
        SET(${VAR} ${VAL} PARENT_SCOPE)
    ENDIF()
ENDFUNCTION()




## ------- FUNCTION CHECK_ADD_FLAG -----
#
#   Check if a flag works and add them.
#
#   Usage:
#       CHECK_ADD_C_FLAG("-Wall" SUCCESS)
#       CHECK_ADD_CXX_FLAG("-Wall" SUCCESS)
#       CHECK_ADD_FORTRAN_FLAG("-Wall" SUCCESS)
#
MACRO(CHECK_ADD_C_FLAG _FLAG OUTPUT)
    CHECK_C_COMPILER_FLAG(${_FLAG} _TEST_${_FLAG})
    SET(${OUTPUT} FALSE)
    IF(_TEST_${_FLAG})
        MESSAGE(STATUS "ADD C_FLAGS: ${_FLAG}")
        SET(CMAKE_C_FLAGS          "${CMAKE_C_FLAGS} ${_FLAG}")
        SET(CMAKE_C_FLAGS_DEBUG    "${CMAKE_C_FLAGS_DEBUG} ${_FLAG}")
        SET(${OUTPUT} TRUE)
    ENDIF()
ENDMACRO()


MACRO(CHECK_ADD_CXX_FLAG _FLAG OUTPUT)
    CHECK_CXX_COMPILER_FLAG(${_FLAG} _TEST_${_FLAG})
    SET(${OUTPUT} FALSE)
    IF(_TEST_${_FLAG})
        MESSAGE(STATUS "ADD CXX_FLAGS: ${_FLAG}")
        SET(CMAKE_CXX_FLAGS          "${CMAKE_CXX_FLAGS} ${_FLAG}")
        SET(CMAKE_CXX_FLAGS_DEBUG    "${CMAKE_CXX_FLAGS_DEBUG} ${_FLAG}")
        SET(${OUTPUT} TRUE)
    ENDIF()
ENDMACRO()


MACRO(CHECK_ADD_FORTRAN_FLAG _FLAG OUTPUT)
    CHECK_FORTRAN_COMPILER_FLAG(${_FLAG} _TEST_${_FLAG})
    SET(${OUTPUT} FALSE)
    IF(_TEST_${_FLAG})
        MESSAGE(STATUS "ADD FORTRAN_FLAGS: ${_FLAG}")
        SET(CMAKE_Fortran_FLAGS          "${CMAKE_Fortran_FLAGS} ${_FLAG}")
        SET(CMAKE_Fortran_FLAGS_DEBUG    "${CMAKE_Fortran_FLAGS_DEBUG} ${_FLAG}")
        SET(${OUTPUT} TRUE)
    ENDIF()
ENDMACRO()



## ------- COLOR FOR OUTPUT -----
#
#   Extendend Message Function with COLOR Support.
#
#   Usage:
#       MESSAGE_COLOR("64 Bit integer support is activated but experimental.")
#       MESSAGE_COLOR(COLOR_BOLD_CYAN "64 Bit integer support is activated but experimental.")
#       MESSAGE_COLOR(STATUS "64 Bit integer support is activated but experimental.")
#       MESSAGE_COLOR(STATUS COLOR_BOLD_CYAN "64 Bit integer support is activated but experimental.")
#
#
IF(NOT WIN32)
    STRING(ASCII 27 COLOR_ESC)
    SET(COLOR_RESET         "${COLOR_ESC}[m")
    SET(COLOR_BOLD          "${COLOR_ESC}[1m")
    SET(COLOR_RED           "${COLOR_ESC}[31m")
    SET(COLOR_GREEN         "${COLOR_ESC}[32m")
    SET(COLOR_YELLOW        "${COLOR_ESC}[33m")
    SET(COLOR_BLUE          "${COLOR_ESC}[34m")
    SET(COLOR_MAGENTA       "${COLOR_ESC}[35m")
    SET(COLOR_CYAN          "${COLOR_ESC}[36m")
    SET(COLOR_WHITE         "${COLOR_ESC}[37m")
    SET(COLOR_BOLD_RED      "${COLOR_ESC}[1;31m")
    SET(COLOR_BOLD_GREEN    "${COLOR_ESC}[1;32m")
    SET(COLOR_BOLD_YELLOW   "${COLOR_ESC}[1;33m")
    SET(COLOR_BOLD_BLUE     "${COLOR_ESC}[1;34m")
    SET(COLOR_BOLD_MAGENTA  "${COLOR_ESC}[1;35m")
    SET(COLOR_BOLD_CYAN     "${COLOR_ESC}[1;36m")
    SET(COLOR_BOLD_WHITE    "${COLOR_ESC}[1;37m")
ENDIF()

FUNCTION(MESSAGE_COLOR)
    IF(${ARGC} EQUAL 1)
        # COLOR NO,  STATUS NO
        MESSAGE("${ARGV0}")
    ELSEIF(${ARGC} EQUAL 2)

        # check first argumnent if it is an option from MESSAGE
        IF(";STATUS;WARNING;AUTHOR_WARNING;SEND_ERROR;FATAL_ERROR;DEPRECATION" MATCHES ";${ARGV0};")
            # COLOR NO, STATUS YES
            MESSAGE(${ARGV0} "${ARGV1}")
        ELSE()
            # COLOR YES, STATUS NO
            MESSAGE("${${ARGV0}}${ARGV1}${COLOR_RESET}")
        ENDIF()
    ELSEIF(${ARGC} EQUAL 3)
        # COLOR YES, STATUS YES
        MESSAGE(${ARGV0} "${${ARGV1}}${ARGV2}${COLOR_RESET}")
    ELSE()
        MESSAGE(AUTHOR_WARNING "UNKOWN CALLING SEQUENCE OF MESSAGE_COLOR. PRINT STANDARD MESSAGE")
        MESSAGE("${ARGV}")
    ENDIF()

ENDFUNCTION()

## ------- MACRO GET_INC_LIB_DIR -----
#
#   Get include/library directories from environment variable.
#
#   Usage:
#       GET_INC_LIB_DIR(INCDIR LIBDIR)
#
MACRO(GET_INC_LIB_DIR INCDIR LIBDIR)
    STRING(REPLACE ":" ";" _libdir "$ENV{LIB}" "$ENV{DYLD_LIBRARY_PATH}"  "$ENV{LD_LIBRARY_PATH}")
    SET(_incdir "${_incdir}" $ENV{INC} $ENV{INCLUDE} $ENV{CPATH})
    SET(${INCDIR} ${_incdir})
    SET(${LIBDIR} ${_libdir})
ENDMACRO()



## ------- FUNCTION FIND_PYTHON_MODULE -----
#
#   Find a Python Module.
#
# Found at:
#       http://www.cmake.org/pipermail/cmake/2011-January/041666.html
#       https://github.com/ivansafrin/Polycode/blob/master/CMake/FindPythonModule.cmake
#
#   USAGE:
#       FIND_PYTHON_MODULE(${PYTHON_EXECUTABLE} numpydoc)
#
FUNCTION(FIND_PYTHON_MODULE PYTHON_EXECUTABLE MODULE)
    STRING(TOUPPER ${MODULE} MODULE_UPPER)
    IF(NOT PY_${MODULE_UPPER})
        IF(ARGC GREATER 2 AND ARGV2 STREQUAL "REQUIRED")
            SET(${MODULE}_FIND_REQUIRED TRUE)
        ENDIF()

        # A module's location is usually a directory, but for binary modules
        # it's a .so file.
        EXECUTE_PROCESS(COMMAND "${PYTHON_EXECUTABLE}" "-c" "import re, ${MODULE}; print(re.compile('/__init__.py.*').sub('',${MODULE}.__file__))"
            RESULT_VARIABLE _${MODULE}_STATUS
            OUTPUT_VARIABLE _${MODULE}_LOCATION
            ERROR_QUIET
            OUTPUT_STRIP_TRAILING_WHITESPACE)
        IF(NOT _${MODULE}_STATUS)
            SET(PY_${MODULE_UPPER}  ${_${MODULE}_LOCATION} CACHE STRING "Location of Python module ${MODULE}")
        ENDIF()
    ENDIF()

    INCLUDE(FindPackageHandleStandardArgs)
    FIND_PACKAGE_HANDLE_STANDARD_ARGS(PY_${MODULE} DEFAULT_MSG PY_${MODULE_UPPER})
    MARK_AS_ADVANCED(PY_${MODULE_UPPER})
ENDFUNCTION()
