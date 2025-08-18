# - this module looks for Matlab
# Defines:
#  MATLAB_INCLUDE_DIR: include path for mex.h, engine.h
#  MATLAB_LIBRARIES:   required libraries: libmex, etc
#  MATLAB_MEX_LIBRARY: path to libmex.lib
#  MATLAB_MX_LIBRARY:  path to libmx.lib
#  MATLAB_ENG_LIBRARY: path to libeng.lib
#  MATLAB_ROOT: path to Matlab's root directory
#  MATLAB_ARCH_DIR:    MATLAB ARCH DIR
#  MATALB_EXT

# This file is part of Gerardus
#
# This is a derivative work of file FindMatlab.cmake released with
# CMake v2.8, because the original seems to be a bit outdated and
# doesn't work with my Windows XP and Visual Studio 10 install
#
# (Note that the original file does work for Ubuntu Natty)
#
# Author: Ramon Casero <rcasero@gmail.com>, Tom Doel
# Version: 0.2.3
# $Rev$
# $Date$
#
# The original file was copied from an Ubuntu Linux install
# /usr/share/cmake-2.8/Modules/FindMatlab.cmake

#=============================================================================
# Copyright 2005-2009 Kitware, Inc.
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







INCLUDE(FindPackageHandleStandardArgs)

SET(MATLAB_FOUND 0)
IF(WIN32)
    # Search for a version of Matlab available, starting from the most modern one to older versions
    FOREACH(MATVER "7.11" "7.10" "7.9" "7.8" "7.7" "7.6" "7.5" "7.4")
        IF((NOT DEFINED MATLAB_ROOT) OR ("${MATLAB_ROOT}" STREQUAL "") OR("${MATLAB_ROOT}" STREQUAL "/registry"))
            GET_FILENAME_COMPONENT(MATLAB_ROOT
                "[HKEY_LOCAL_MACHINE\\SOFTWARE\\MathWorks\\MATLAB\\${MATVER};MATLABROOT]"
                ABSOLUTE)
            SET(MATLAB_VERSION ${MATVER})
        ENDIF()
    ENDFOREACH()

    # Directory name depending on whether the Windows architecture is 32
    # bit or 64 bit
    IF(CMAKE_SIZEOF_VOID_P MATCHES "4")
        SET(WINDIR "win32")
    ELSEIF(CMAKE_SIZEOF_VOID_P MATCHES "8")
        SET(WINDIR "win64")
    ELSE()
        MESSAGE(FATAL_ERROR "CMAKE_SIZEOF_VOID_P (${CMAKE_SIZEOF_VOID_P}) doesn't indicate a valid platform")
    ENDIF()

    # Folder where the MEX libraries are, depending of the Windows compiler
    IF(${CMAKE_GENERATOR} MATCHES "Visual Studio 6")
        SET(MATLAB_LIBRARIES_DIR "${MATLAB_ROOT}/extern/lib/${WINDIR}/microsoft/msvc60")
    ELSEIF(${CMAKE_GENERATOR} MATCHES "Visual Studio 7")
        # Assume people are generally using Visual Studio 7.1,
        # if using 7.0 need to link to: ../extern/lib/${WINDIR}/microsoft/msvc70
        SET(MATLAB_LIBRARIES_DIR "${MATLAB_ROOT}/extern/lib/${WINDIR}/microsoft/msvc71")
        # SET(MATLAB_LIBRARIES_DIR "${MATLAB_ROOT}/extern/lib/${WINDIR}/microsoft/msvc70")
    ELSEIF(${CMAKE_GENERATOR} MATCHES "Borland")
        # Assume people are generally using Borland 5.4,
        # if using 7.0 need to link to: ../extern/lib/${WINDIR}/microsoft/msvc70
        SET(MATLAB_LIBRARIES_DIR "${MATLAB_ROOT}/extern/lib/${WINDIR}/microsoft/bcc54")
        # SET(MATLAB_LIBRARIES_DIR "${MATLAB_ROOT}/extern/lib/${WINDIR}/microsoft/bcc50")
        # SET(MATLAB_LIBRARIES_DIR "${MATLAB_ROOT}/extern/lib/${WINDIR}/microsoft/bcc51")
    ELSEIF(${CMAKE_GENERATOR} MATCHES "Visual Studio*")
        # If the compiler is Visual Studio, but not any of the specific
        # versions above, we try our luck with the microsoft directory
        SET(MATLAB_LIBRARIES_DIR "${MATLAB_ROOT}/extern/lib/${WINDIR}/microsoft/")
    ELSE()
        MESSAGE(FATAL_ERROR "Generator not compatible: ${CMAKE_GENERATOR}")
    ENDIF()

    # Get paths to the Matlab MEX libraries
    FIND_LIBRARY(MATLAB_MEX_LIBRARY
        libmex
        ${MATLAB_LIBRARIES_DIR}
        )
    FIND_LIBRARY(MATLAB_MX_LIBRARY
        libmx
        ${MATLAB_LIBRARIES_DIR}
        )
    FIND_LIBRARY(MATLAB_ENG_LIBRARY
        libeng
        ${MATLAB_LIBRARIES_DIR}
        )

    # Get path to the include directory
    FIND_PATH(MATLAB_INCLUDE_DIR
        "mex.h"
        "${MATLAB_ROOT}/extern/include"
        )

ELSE()

    IF((NOT DEFINED MATLAB_ROOT) OR("${MATLAB_ROOT}" STREQUAL ""))
        # get path to the Matlab root directory
        EXECUTE_PROCESS(
            COMMAND which matlab
            COMMAND xargs readlink
            COMMAND xargs dirname
            COMMAND xargs dirname
            COMMAND xargs echo -n
            OUTPUT_VARIABLE MATLAB_ROOT
            )
    ENDIF()
    MESSAGE(STATUS "MATLAB_ROOT = ${MATLAB_ROOT}")
    # Check if this is a Mac
    IF(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")

        SET(LIBRARY_EXTENSION .dylib)

        # If this is a Mac and the attempts to find MATLAB_ROOT have so far failed,
        # we look in the applications folder
        IF((NOT DEFINED MATLAB_ROOT) OR ("${MATLAB_ROOT}" STREQUAL ""))

            # Search for a version of Matlab available, starting from the most modern one to older versions
            FOREACH(MATVER "R2013b" "R2013a" "R2012b" "R2012a" "R2011b" "R2011a" "R2010b" "R2010a" "R2009b" "R2009a" "R2008b")
                IF((NOT DEFINED MATLAB_ROOT) OR ("${MATLAB_ROOT}" STREQUAL ""))
                    IF(EXISTS /Applications/MATLAB_${MATVER}.app)
                        SET(MATLAB_ROOT /Applications/MATLAB_${MATVER}.app)

                    ENDIF()
                ENDIF()
            ENDFOREACH()

        ENDIF()

    ELSE()
        SET(LIBRARY_EXTENSION .so)

    ENDIF()

    # Get path to the MEX libraries
    IF(APPLE)
        IF(CMAKE_OSX_ARCHITECTURES MATCHES i386)
            SET(MATLAB_EXT ".mexmaci")
        ELSE()
            SET(MATLAB_EXT ".mexmac")
        ENDIF()
    ELSE()
        IF(CMAKE_SIZEOF_VOID_P MATCHES "4")
            SET(MATLAB_EXT ".mexglx")
            SET(MATLAB_ARCH_DIR "glnx86")
        ELSEIF(CMAKE_SIZEOF_VOID_P MATCHES "8")
            SET(MATLAB_EXT ".mexa64")
            SET(MATLAB_ARCH_DIR "glnxa64")
        ELSE()
            MESSAGE(FATAL_ERROR "CMAKE_SIZEOF_VOID_P (${CMAKE_SIZEOF_VOID_P}) doesn't indicate a valid platform")
        ENDIF()
    ENDIF()

    MESSAGE(STATUS "MATLAB_EXT = ${MATLAB_EXT}")
    SET(MATLAB_ARCH_DIR "${MATLAB_ROOT}/bin/${MATLAB_ARCH_DIR}/")
    IF(EXISTS ${MATLAB_ARCH_DIR})
        EXECUTE_PROCESS(
            COMMAND find "${MATLAB_ARCH_DIR}" -name libmex${LIBRARY_EXTENSION}
            COMMAND xargs echo -n
            OUTPUT_VARIABLE MATLAB_MEX_LIBRARY
            )
        EXECUTE_PROCESS(
            COMMAND find "${MATLAB_ARCH_DIR}" -name libmx${LIBRARY_EXTENSION}
            COMMAND xargs echo -n
            OUTPUT_VARIABLE MATLAB_MX_LIBRARY
            )
        EXECUTE_PROCESS(
            COMMAND find "${MATLAB_ARCH_DIR}" -name libeng${LIBRARY_EXTENSION}
            COMMAND xargs echo -n
            OUTPUT_VARIABLE MATLAB_ENG_LIBRARY
            )
    ENDIF()

    # Get path to the include directory
    FIND_PATH(MATLAB_INCLUDE_DIR
        "mex.h"
        PATHS "${MATLAB_ROOT}/extern/include"
        )
    SET(MATLAB_MEX ${MATLAB_ROOT}/bin/mex)
    MESSAGE(STATUS "MATLAB_MEX = ${MATLAB_MEX}")



    # determine if matlab version is 9.5 or higher
    # https://de.mathworks.com/help/matlab/matlab_external/do-i-need-to-upgrade-my-mex-files-to-use-interleaved-complex.html
    EXECUTE_PROCESS(
        COMMAND "${MATLAB_ROOT}/bin/matlab" -nosplash -nojvm -nodesktop -nodisplay -r "exit(~verLessThan('matlab','9.4'))"
        RESULT_VARIABLE  MATLAB_MEX_INTERLEAVED_COMPLEX_API
        TIMEOUT 120
        )

ENDIF()

# This is common to UNIX and Win32:
SET(MATLAB_LIBRARIES
    ${MATLAB_MEX_LIBRARY}
    ${MATLAB_MX_LIBRARY}
    ${MATLAB_ENG_LIBRARY}
    )

IF(MATLAB_INCLUDE_DIR AND MATLAB_LIBRARIES)
    SET(MATLAB_FOUND 1)
ENDIF()

MARK_AS_ADVANCED(
    MATLAB_LIBRARIES
    MATLAB_MEX_LIBRARY
    MATLAB_MX_LIBRARY
    MATLAB_ENG_LIBRARY
    MATLAB_INCLUDE_DIR
    MATLAB_FOUND
    MATLAB_ROOT
    )

FIND_PACKAGE_HANDLE_STANDARD_ARGS(MATLAB  DEFAULT_MSG  MATLAB_LIBRARIES MATLAB_INCLUDE_DIR)

