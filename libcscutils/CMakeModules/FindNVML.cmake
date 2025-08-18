# - Find NVML

# This module defines
#  NVML_LIBRARIES, the libraries to link against to use libnvidia-ml
#  NVML_FOUND, If false, do not try to use libnvml
#  NVML_INCLUDE_DIR, include directories for libnvml. 

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

if (NOT _incdir)
  if (WIN32)
	  set(_incdir ENV INCLUDE)
  elseif (APPLE)
	  set(_incdir ENV INCLUDE)
  else ()
	  set(_incdir ENV INCLUDE)
  endif ()
endif ()

if (NOT _libdir)
  if (WIN32)
    set(_libdir ENV LIB)
  elseif (APPLE)
    set(_libdir ENV DYLD_LIBRARY_PATH)
  else ()
    set(_libdir ENV LD_LIBRARY_PATH)
  endif ()
endif ()

find_path(NVML_NVML_INCLUDE_DIR NAMES nvml.h
	PATHS
	${_incdir}
	/usr/local/cuda-8.0/targets/ppc64le-linux/include/
	/usr/local/cuda-8.0/targets/x86_64-linux/include/
	/usr/include/nvidia/gdk/
	/usr/include
	/usr/local/include
	/opt/local/include	#Macports
  )

set(NVML_NAMES nvidia-ml)
find_library(NVML_LIBRARY NAMES ${NVML_NAMES} PATHS 
		${_libdir} 
		/usr/lib64/nvidia/
		/usr/lib/nvidia/)

if (NVML_LIBRARY AND NVML_NVML_INCLUDE_DIR)
	SET(NVML_LIBRARIES ${NVML_LIBRARY})
	SET(NVML_INCLUDE_DIR ${NVML_NVML_INCLUDE_DIR})
	SET(NVML_FOUND TRUE)
else (NVML_LIBRARY AND NVML_NVML_INCLUDE_DIR) 
	SET(NVML_FOUND_FOUND FALSE) 
endif (NVML_LIBRARY AND NVML_NVML_INCLUDE_DIR)


# handle the QUIETLY and REQUIRED arguments 
# all listed variables are TRUE
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(NVML DEFAULT_MSG NVML_LIBRARY NVML_INCLUDE_DIR)
mark_as_advanced(NVML_LIBRARY NVML_NVML_INCLUDE_DIR)
