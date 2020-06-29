# - Find the SUPERLU includes and library
#
# This module defines
#
#   SUPERLU_INCLUDE_DIR,
#   SUPERLU_LIBRARIES
#   SUPERLU_FOUND
#   SUPERLU_REFINEMENT_BUG_FIXED        -   Boolean, iterative refinement bug in SuperLU was fixed
#   SUPERLU_MIN_VERSION_43              -   Boolean, SuperLU Version >= 4.3
#   SUPERLU_MIN_VERSION_50              -   Boolean, SuperLU Version >= 5.0
#   SUPERLU_MT_MIN_VERSION_20           -   Boolean, SuperLU MT Version >= 2.0
#   SUPERLU_MT_MIN_VERSION_30           -   Boolean, SuperLU MT Version >= 3.0
#
#
#=============================================================================
# Copyright 2011, Martin Koehler
#
# Distributed under the OSI-approved BSD License (the "License");
# see accompanying file Copyright.txt for details.
#
# This software is distributed WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the License for more information.
#=============================================================================
# Changelog:
#    - Nov. 3, 2015   Maximilian Behr



INCLUDE(FindPackageHandleStandardArgs)

# GET_INC_LIB_DIR MACRO
INCLUDE(CMakeHelpers)
GET_INC_LIB_DIR(_incdir _libdir)



#look for header in user given directories
FIND_PATH(SUPERLU_INCLUDE_DIR
    NAMES supermatrix.h
    PATHS ${SUPERLU_ROOT}
    PATH_SUFFIXES "superlu" "SuperLU" "include/superlu" "include" "SRC"
    NO_DEFAULT_PATH
    )

#look for header in standard paths
FIND_PATH(SUPERLU_INCLUDE_DIR
    NAMES supermatrix.h
    PATHS ${_incdir} /usr/include /usr/local/include /opt/local/include
    PATH_SUFFIXES "superlu" "SuperLU"
    )

#find library
SET(SUPERLU_NAMES   "superlu"
    "superlu_2.0"
    "superlu_3.0"
    "superlu_3.1"
    "superlu_4.0"
    "superlu_4.1"
    "superlu_4.2"
    "superlu_4.3"
    "superlu_5.0"
    "superlu_5.1"
    "superlu_5.1.1"
    "superlu_5.2"
    "superlu_mt"
    "superlu_mt_2.0"
    "superlu_mt_2.1"
    "superlu_mt_2.2"
    "superlu_mt_2.3"
    "superlu_mt_2.4"
    "superlu_mt_3.0")


#look for libraries in user given path
FIND_LIBRARY(SUPERLU_LIBRARIES
    NAMES ${SUPERLU_NAMES}
    PATHS ${SUPERLU_ROOT}
    PATH_SUFFIXES "lib" "lib32" "lib64"
    NO_DEFAULT_PATH
    )


#look for libraries in standard path
FIND_LIBRARY(SUPERLU_LIBRARIES
    NAMES ${SUPERLU_NAMES}
    PATHS ${_libdir} /usr/lib /usr/local/lib /opt/local/lib
    PATH_SUFFIXES "superlu" "SuperLU"
    )


IF(SUPERLU_LIBRARIES AND SUPERLU_INCLUDE_DIR)

    #set includes and libraries for tests
    SET(CMAKE_REQUIRED_LIBRARIES "${SUPERLU_LIBRARIES}" "${BLAS_LIBRARIES}" "-lm")
    SET(CMAKE_REQUIRED_INCLUDES ${SUPERLU_INCLUDE_DIR})
    SET(CMAKE_REQUIRED_FLAGS "")
    SET(CMAKE_REQUIRED_DEFINITIONS "")

    #check for iterative refinement bug superlu version < 4.3
    FILE(READ ${CMAKE_CURRENT_SOURCE_DIR}/CMakeModules/superlu_refinement_bug.c SUPERLU_SRC_REFINEMENT_BUG_FIXED)
    CHECK_C_SOURCE_RUNS("${SUPERLU_SRC_REFINEMENT_BUG_FIXED}" SUPERLU_REFINEMENT_BUG_FIXED)

    #check version greater than 4.3
    CHECK_C_SOURCE_COMPILES("#include <slu_ddefs.h> \n int main(void){return SLU_DOUBLE;}" SUPERLU_MIN_VERSION_43)
    IF(SUPERLU_MIN_VERSION_43)
        MESSAGE(STATUS "SuperLU Version >= 4.3")
        #check for iterative refinement bug
        FILE(READ ${CMAKE_CURRENT_SOURCE_DIR}/CMakeModules/superlu_refinement_bug_43.c SUPERLU_SRC_REFINEMENT_BUG_FIXED_43)
        CHECK_C_SOURCE_RUNS("${SUPERLU_SRC_REFINEMENT_BUG_FIXED_43}" SUPERLU_REFINEMENT_BUG_FIXED_43)
    ENDIF()

    #check if version greater than 5.0
    CHECK_C_SOURCE_COMPILES("#include <slu_ddefs.h>
    int main(void){dgssvx(NULL, NULL, NULL, NULL,NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL, NULL, NULL, NULL);return 0;}" SUPERLU_MIN_VERSION_50)
    IF(SUPERLU_MIN_VERSION_50)
        MESSAGE(STATUS "SuperLU Version >= 5.0")
        #check for iterative refinement bug
        FILE(READ ${CMAKE_CURRENT_SOURCE_DIR}/CMakeModules/superlu_refinement_bug_50.c SUPERLU_SRC_REFINEMENT_BUG_FIXED_50)
        CHECK_C_SOURCE_RUNS("${SUPERLU_SRC_REFINEMENT_BUG_FIXED_50}" SUPERLU_REFINEMENT_BUG_FIXED_50)
    ENDIF()

    #check if it is superlu mt version >=3.0
    CHECK_C_SOURCE_COMPILES("#include <slu_mt_ddefs.h>
    int main(void){return 0;}" SUPERLU_MT_MIN_VERSION_30)
    IF(SUPERLU_MT_MIN_VERSION_30)
        MESSAGE(STATUS "SuperLU is MT and Version >= 3.0")
    ENDIF()

    #check if refinement bug was fixed
    IF(SUPERLU_REFINEMENT_BUG_FIXED OR SUPERLU_REFINEMENT_BUG_FIXED_43 OR SUPERLU_REFINEMENT_BUG_FIXED_50)
        SET(SUPERLU_REFINEMENT_BUG_FIXED TRUE)
    ENDIF()

    ##message about superlu refinement bug
    IF(SUPERLU_REFINEMENT_BUG_FIXED)
        MESSAGE(STATUS "SuperLU refinement bug fixed.")
    ELSE()
        MESSAGE(STATUS "SuperLU refinement bug not fixed.")
    ENDIF()


ENDIF()



# handle the QUIETLY and REQUIRED arguments and set SUPERLU_FOUND to TRUE if
# all listed variables are TRUE
FIND_PACKAGE_HANDLE_STANDARD_ARGS(SUPERLU DEFAULT_MSG SUPERLU_LIBRARIES SUPERLU_INCLUDE_DIR)

MARK_AS_ADVANCED(SUPERLU_LIBRARIES SUPERLU_INCLUDE_DIR SUPERLU_REFINEMENT_BUG_FIXED
    SUPERLU_REFINEMENT_BUG_FIXED
    SUPERLU_MIN_VERSION_43
    SUPERLU_MIN_VERSION_50
    SUPERLU_MT_MIN_VERSION_20
    SUPERLU_MT_MIN_VERSION_30
    )





