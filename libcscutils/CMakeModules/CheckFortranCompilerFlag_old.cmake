# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file Copyright.txt or https://cmake.org/licensing for details.

#.rst:
# CheckFortranCompilerFlag
# ------------------------
#
# Check whether the Fortran compiler supports a given flag.
#
# CHECK_Fortran_COMPILER_FLAG(<flag> <var>)
#
# ::
#
#   <flag> - the compiler flag
#   <var>  - variable to store the result
#            Will be created as an internal cache variable.
#
# This internally calls the check_fortran_source_compiles macro and
# sets CMAKE_REQUIRED_DEFINITIONS to <flag>.  See help for
# CheckFortranSourceCompiles for a listing of variables that can
# otherwise modify the build.  The result only tells that the compiler
# does not give an error message when it encounters the flag.  If the
# flag has any effect or even a specific one is beyond the scope of
# this module.
#
# --------------------------------------------------------
# THIS IS A REPLACEMENT IF THE CMAKE VERSION IS TOO OLD
# WE USE THIS MODULE.
# --------------------------------------------------------

INCLUDE(CheckFortranSourceCompiles_old)
INCLUDE(CMakeCheckCompilerFlagCommonPatterns_old)

MACRO(CHECK_Fortran_COMPILER_FLAG _FLAG _RESULT)
    SET(SAFE_CMAKE_REQUIRED_DEFINITIONS "${CMAKE_REQUIRED_DEFINITIONS}")
    SET(CMAKE_REQUIRED_DEFINITIONS "${_FLAG}")

    # Normalize locale during test compilation.
    SET(_CheckFortranCompilerFlag_LOCALE_VARS LC_ALL LC_MESSAGES LANG)
    FOREACH(v ${_CheckFortranCompilerFlag_LOCALE_VARS})
        SET(_CheckFortranCompilerFlag_SAVED_${v} "$ENV{${v}}")
        SET(ENV{${v}} C)
    ENDFOREACH()
    CHECK_COMPILER_FLAG_COMMON_PATTERNS(_CheckFortranCompilerFlag_COMMON_PATTERNS)
    CHECK_FORTRAN_SOURCE_COMPILES("       program test\n       stop\n       end program" ${_RESULT}
        # Some compilers do not fail with a bad flag
        FAIL_REGEX "command line option .* is valid for .* but not for Fortran" # GNU
        ${_CheckFortranCompilerFlag_COMMON_PATTERNS}
        )
    FOREACH(v ${_CheckFortranCompilerFlag_LOCALE_VARS})
        SET(ENV{${v}} ${_CheckFortranCompilerFlag_SAVED_${v}})
        UNSET(_CheckFortranCompilerFlag_SAVED_${v})
    ENDFOREACH()
    UNSET(_CheckFortranCompilerFlag_LOCALE_VARS)
    UNSET(_CheckFortranCompilerFlag_COMMON_PATTERNS)

    SET(CMAKE_REQUIRED_DEFINITIONS "${SAFE_CMAKE_REQUIRED_DEFINITIONS}")
ENDMACRO()
