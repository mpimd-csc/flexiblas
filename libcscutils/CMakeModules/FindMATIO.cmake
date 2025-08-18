# - Find libmatio

# This module defines
#  MATIO_LIBRARIES, the libraries to link against to use libmatio.
#  MATIO_FOUND, If false, do not try to use libmatio.
#  MATIO_INCLUDE_DIR, include directories for libmatio.

#=============================================================================
# Copyright 2014, Martin Koehler
#
#
# Distributed under the OSI-approved BSD License (the "License");
# see accompanying file Copyright.txt for details.
#
# This software is distributed WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the License for more information.
#=============================================================================
include(CheckFunctionExists)
include(FindPackageHandleStandardArgs)

find_package(ZLIB REQUIRED)

if(NOT _incdir)
    if(WIN32)
        set(_incdir ENV INCLUDE)
    elseif(APPLE)
        set(_incdir ENV INCLUDE CPATH)
    else()
        set(_incdir ENV INCLUDE CPATH)
    endif()
endif()

if(NOT _libdir)
    if(WIN32)
        set(_libdir ENV LIB)
    elseif(APPLE)
        set(_libdir ENV DYLD_LIBRARY_PATH)
    else()
        set(_libdir ENV LD_LIBRARY_PATH)
    endif()
endif()

find_path(MATIO_INCLUDE_DIR NAMES matio.h
    PATHS
    ${_incdir}
    /usr/include
    /usr/local/include
    /opt/local/include  #Macports
    )
set(MATIO_NAMES matio)
find_library(MATIO_LIBRARY NAMES ${MATIO_NAMES} PATHS ${_libdir} /usr/lib /usr/lib32 /usr/lib64)

if(MATIO_INCLUDE_DIR AND MATIO_LIBRARY)
    message(STATUS "Found MATIO header: ${MATIO_INCLUDE_DIR}")
    message(STATUS "Found MATIO library: ${MATIO_LIBRARY}")

    message(STATUS "Check if MATIO needs HDF5")

    set(__CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES})
    set(CMAKE_REQUIRED_LIBRARIES ${MATIO_LIBRARY} ${ZLIB_LIBRARIES})
    check_function_exists(Mat_Open NEED_HDF5)
    if( NOT NEED_HDF5 )
        #try to find HDF5
        message(STATUS "Search for HDF5 because Matio Reuqires it.")
        find_package(HDF5)
        if(HDF5_FOUND)
            message(STATUS "HDF5 found.")
            set(MATIO_FOUND TRUE)
        else()
            message("MATIO could not enabled, due to missing HDF5.")
            set(MATIO_FOUND FALSE)
        endif()
    else()
        set(MATIO_FOUND TRUE)
    endif()
    set(CMAKE_REQUIRED_LIBRARIES ${__CMAKE_REQUIRED_LIBRARIES})

endif()

if(MATIO_FOUND AND MATIO_INCLUDE_DIR)
    set(MATIO_LIBRARIES ${MATIO_LIBRARY} ${HDF5_LIBRARIES})
    set(MATIO_INCLUDE_DIR ${MATIO_MATIO_INCLUDE_DIR} ${HDF5_INCLUDE_DIR})
endif()


# handle the QUIETLY and REQUIRED arguments and set MATIO_FOUND to TRUE if
# all listed variables are TRUE
find_package_handle_standard_args(MATIO DEFAULT_MSG MATIO_LIBRARY MATIO_INCLUDE_DIR)

mark_as_advanced(MATIO_LIBRARY)
