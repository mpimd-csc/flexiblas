# - Find python interpreter
# This module finds if Python interpreter is installed and determines where the
# executables are. This code sets the following variables:
#
#  PYTHONINTERP_FOUND       - Was the Python executable found
#  PYTHON_EXECUTABLE        - path to the Python interpreter
#
#  PYTHON_VERSION_STRING    - Python version found e.g. 2.5.2
#  PYTHON_VERSION_MAJOR     - Python major version found e.g. 2
#  PYTHON_VERSION_MINOR     - Python minor version found e.g. 5
#  PYTHON_VERSION_PATCH     - Python patch version found e.g. 2
#  PYTHON_LIBRARIES         - Python libraries, e.g. libpython2.7.so
#  PYTHON_INCLUDE_DIR       - Python include directories,  where to find Python.h
#
#  Input:
#  Python_ADDITIONAL_VERSIONS - list of additional Python versions to search for

#=============================================================================
# Copyright 2005-2010 Kitware, Inc.
# Copyright 2011 Bjoern Ricks <bjoern.ricks@gmail.com>
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


include(FindPackageHandleStandardArgs)





# Set up the versions we know about, in the order we will search. Always add
# the user supplied additional versions to the front.
set(_Python_VERSIONS ${Python_ADDITIONAL_VERSIONS})


# Search for newest python version if python executable isn't found
if(NOT PYTHON_EXECUTABLE)
    foreach(_CURRENT_VERSION ${_Python_VERSIONS})
        set(_Python_NAMES python${_CURRENT_VERSION})
        if(WIN32)
            list(APPEND _Python_NAMES python)
        endif()
        find_program(PYTHON_EXECUTABLE
            NAMES ${_Python_NAMES}
            PATHS [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\${_CURRENT_VERSION}\\InstallPath]
            )
    endforeach()
endif()

if(NOT PYTHON_EXECUTABLE)
    # Search for the current active python version first
    find_program(PYTHON_EXECUTABLE NAMES python)
endif()

# determine library, include path and version from python exectuable
if(PYTHON_EXECUTABLE)

    # get version string
    execute_process(COMMAND "${PYTHON_EXECUTABLE}" -c   "import sys; print(sys.version_info[0])"
                    OUTPUT_VARIABLE PYTHON_VERSION_MAJOR OUTPUT_STRIP_TRAILING_WHITESPACE)
    execute_process(COMMAND "${PYTHON_EXECUTABLE}" -c   "import sys; print(sys.version_info[1])"
                    OUTPUT_VARIABLE PYTHON_VERSION_MINOR OUTPUT_STRIP_TRAILING_WHITESPACE)
    execute_process(COMMAND "${PYTHON_EXECUTABLE}" -c   "import sys; print(sys.version_info[2])"
                    OUTPUT_VARIABLE PYTHON_VERSION_PATCH OUTPUT_STRIP_TRAILING_WHITESPACE)
    set(PYTHON_VERSION_STRING "${PYTHON_VERSION_MAJOR}.${PYTHON_VERSION_MINOR}.${PYTHON_VERSION_PATCH}")


    # get libpython file
    execute_process(COMMAND "${PYTHON_EXECUTABLE}" -c "from sysconfig import get_config_var; print(get_config_var('LDLIBRARY'))"
                    OUTPUT_VARIABLE _PYTHON_LD_LIB OUTPUT_STRIP_TRAILING_WHITESPACE)


    # get libpython path
    execute_process(COMMAND "${PYTHON_EXECUTABLE}" -c "from sysconfig import get_config_var; print(get_config_var('LIBPL'))"
                    OUTPUT_VARIABLE _PYTHON_LD_LIB_PATH OUTPUT_STRIP_TRAILING_WHITESPACE)
    execute_process(COMMAND "${PYTHON_EXECUTABLE}" -c "from sysconfig import get_config_var; print(get_config_var('LIBDIR'))"
                    OUTPUT_VARIABLE _PYTHON_LD_LIB_PATH2 OUTPUT_STRIP_TRAILING_WHITESPACE)


    find_library(PYTHON_LIBRARIES
                NAMES
                ${_PYTHON_LD_LIB}
                PATHS
                ${_PYTHON_LD_LIB_PATH}
                ${_PYTHON_LD_LIB_PATH2}
                NO_DEFAULT_PATH)


    # get include path
    execute_process(COMMAND "${PYTHON_EXECUTABLE}" -c "from sysconfig import get_config_var; print(get_config_var('INCLUDEPY'))"
                    OUTPUT_VARIABLE PYTHON_INCLUDE_DIR OUTPUT_STRIP_TRAILING_WHITESPACE)


endif()

if(PYTHON_IGNORE_LIBRARY STREQUAL ON)
    find_package_handle_standard_args(PYTHONINTERP REQUIRED_VARS
                                    PYTHON_EXECUTABLE
                                    PYTHON_VERSION_STRING
                                    PYTHON_INCLUDE_DIR)

    find_package_message(PYTHININTERP
        "Found Python (${PYTHON_VERSION_STRING}): ${PYTHON_INCLUDE_DIR}"
        "${PYTHON_INCLUDE_DIR}")
else()
    find_package_handle_standard_args(PYTHONINTERP REQUIRED_VARS
                                    PYTHON_EXECUTABLE
                                    PYTHON_VERSION_STRING PYTHON_LIBRARIES
                                    PYTHON_INCLUDE_DIR)

    find_package_message(PYTHININTERP
        "Found Python (${PYTHON_VERSION_STRING}): ${PYTHON_INCLUDE_DIR} - ${PYTHON_LIBRARIES}"
        "${PYTHON_INCLUDE_DIR}${PYTHON_LIBRARIES}")


endif()


mark_as_advanced(PYTHON_EXECUTABLE)


