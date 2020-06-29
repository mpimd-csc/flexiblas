# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file Copyright.txt or https://cmake.org/licensing for details.

#.rst:
# CheckFortranSourceCompiles
# --------------------------
#
# Check if given Fortran source compiles and links into an executable::
#
#   CHECK_Fortran_SOURCE_COMPILES(<code> <var> [FAIL_REGEX <fail-regex>]
#                                 [SRC_EXT <ext>])
#
# The arguments are:
#
# ``<code>``
#   Source code to try to compile.  It must define a PROGRAM entry point.
# ``<var>``
#   Variable to store whether the source code compiled.
#   Will be created as an internal cache variable.
# ``FAIL_REGEX <fail-regex>``
#   Fail if test output matches this regex.
# ``SRC_EXT <ext>``
#   Use source extension ``.<ext>`` instead of the default ``.F``.
#
# The following variables may be set before calling this macro to modify
# the way the check is run::
#
#   CMAKE_REQUIRED_FLAGS = string of compile command line flags
#   CMAKE_REQUIRED_DEFINITIONS = list of macros to define (-DFOO=bar)
#   CMAKE_REQUIRED_INCLUDES = list of include directories
#   CMAKE_REQUIRED_LIBRARIES = list of libraries to link
#   CMAKE_REQUIRED_QUIET = execute quietly without messages
#
#
# --------------------------------------------------------
# THIS IS A REPLACEMENT IF THE CMAKE VERSION IS TOO OLD
# WE USE THIS MODULE.
# --------------------------------------------------------


MACRO(CHECK_Fortran_SOURCE_COMPILES SOURCE VAR)
    IF(NOT DEFINED "${VAR}")
        SET(_FAIL_REGEX)
        SET(_SRC_EXT)
        SET(_key)
        FOREACH(arg ${ARGN})
            IF("${arg}" MATCHES "^(FAIL_REGEX|SRC_EXT)$")
                SET(_key "${arg}")
            ELSEIF(_key)
                LIST(APPEND _${_key} "${arg}")
            ELSE()
                MESSAGE(FATAL_ERROR "Unknown argument:\n  ${arg}\n")
            ENDIF()
        ENDFOREACH()
        IF(NOT _SRC_EXT)
            SET(_SRC_EXT F)
        ENDIF()
        SET(MACRO_CHECK_FUNCTION_DEFINITIONS
            "-D${VAR} ${CMAKE_REQUIRED_FLAGS}")
        IF(CMAKE_REQUIRED_LIBRARIES)
            SET(CHECK_Fortran_SOURCE_COMPILES_ADD_LIBRARIES
                LINK_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES})
        ELSE()
            SET(CHECK_Fortran_SOURCE_COMPILES_ADD_LIBRARIES)
        ENDIF()
        IF(CMAKE_REQUIRED_INCLUDES)
            SET(CHECK_Fortran_SOURCE_COMPILES_ADD_INCLUDES
                "-DINCLUDE_DIRECTORIES:STRING=${CMAKE_REQUIRED_INCLUDES}")
        ELSE()
            SET(CHECK_Fortran_SOURCE_COMPILES_ADD_INCLUDES)
        ENDIF()
        FILE(WRITE "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/src.${_SRC_EXT}"
            "${SOURCE}\n")

        IF(NOT CMAKE_REQUIRED_QUIET)
            MESSAGE(STATUS "Performing Test ${VAR}")
        ENDIF()
        TRY_COMPILE(${VAR}
            ${CMAKE_BINARY_DIR}
            ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/src.${_SRC_EXT}
            COMPILE_DEFINITIONS ${CMAKE_REQUIRED_DEFINITIONS}
            ${CHECK_Fortran_SOURCE_COMPILES_ADD_LIBRARIES}
            CMAKE_FLAGS -DCOMPILE_DEFINITIONS:STRING=${MACRO_CHECK_FUNCTION_DEFINITIONS}
            "${CHECK_Fortran_SOURCE_COMPILES_ADD_INCLUDES}"
            OUTPUT_VARIABLE OUTPUT)

        FOREACH(_regex ${_FAIL_REGEX})
            IF("${OUTPUT}" MATCHES "${_regex}")
                SET(${VAR} 0)
            ENDIF()
        ENDFOREACH()

        IF(${VAR})
            SET(${VAR} 1 CACHE INTERNAL "Test ${VAR}")
            IF(NOT CMAKE_REQUIRED_QUIET)
                MESSAGE(STATUS "Performing Test ${VAR} - Success")
            ENDIF()
            FILE(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeOutput.log
                "Performing Fortran SOURCE FILE Test ${VAR} succeeded with the following output:\n"
                "${OUTPUT}\n"
                "Source file was:\n${SOURCE}\n")
        ELSE()
            IF(NOT CMAKE_REQUIRED_QUIET)
                MESSAGE(STATUS "Performing Test ${VAR} - Failed")
            ENDIF()
            SET(${VAR} "" CACHE INTERNAL "Test ${VAR}")
            FILE(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
                "Performing Fortran SOURCE FILE Test ${VAR} failed with the following output:\n"
                "${OUTPUT}\n"
                "Source file was:\n${SOURCE}\n")
        ENDIF()
    ENDIF()
ENDMACRO()
