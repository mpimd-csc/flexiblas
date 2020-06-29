# Function to set policies only if they exists.
#
#=============================================================================
# Copyright 2020, Martin Koehler
#
# Distributed under the OSI-approved BSD License (the "License");
# see accompanying file Copyright.txt for details.
#
# This software is distributed WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the License for more information.
#=============================================================================
#
FUNCTION(SET_POLICY POL VAL)
    IF ( POLICY ${POL} )
        CMAKE_POLICY(SET ${POL} ${VAL})
    ELSE()
        MESSAGE(STATUS "Policy ${POL} does not exist in this CMake version.")
    ENDIF()
ENDFUNCTION()
