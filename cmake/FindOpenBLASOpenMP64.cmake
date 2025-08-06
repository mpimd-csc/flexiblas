SET(OPENBLAS_PREFIX "OpenBLAS-OpenMP64: ")
SET(OPENBLAS_LIB_NAME OpenMP64)
SET(OPENBLAS_GET_PARALLEL 2)

IF ( EXISTS /etc/debian_version )
    # We are on debian
    MESSAGE(STATUS "${OPENBLAS_PREFIX} ${CMAKE_LIBRARY_ARCHITECTURE} ")
    SET(SEARCH_PATH /usr/lib/${CMAKE_LIBRARY_ARCHITECTURE}/openblas64-openmp)
ELSE()
    SET(SEARCH_PATH /usr/lib/ /usr/lib64 /opt/homebrew/opt/openblas/lib )
ENDIF()

SET(SEARCH_NAME openblaso64 openblas64)

FIND_LIBRARY(OpenBLAS${OPENBLAS_LIB_NAME}_LIBRARY NAMES ${SEARCH_NAME} HINTS ${SEARCH_PATH}   DOC "OpenBLAS-OpenMP64 Location")

IF (OpenBLAS${OPENBLAS_LIB_NAME}_LIBRARY)
    MESSAGE(STATUS "${OPENBLAS_PREFIX} Candidate: ${OpenBLAS${OPENBLAS_LIB_NAME}_LIBRARY}")

    SET(TEST_CODE "
    #ifdef NDEBUG
    #undef NDEBUG
    #endif

    #include<assert.h>
    #include<string.h>
    #include<stdlib.h>
    extern int openblas_get_parallel()\;
    extern char *openblas_get_config()\;
    int main() { assert(openblas_get_parallel() == ${OPENBLAS_GET_PARALLEL})\;
    assert(strstr(openblas_get_config(), \"USE64BITINT\") != NULL)\; return 0\;}
    ")

    FILE(WRITE ${CMAKE_BINARY_DIR}/otest.c ${TEST_CODE})
    TRY_RUN(_RUN_RES _COMPILE_RES ${CMAKE_BINARY_DIR} ${CMAKE_BINARY_DIR}/otest.c LINK_LIBRARIES ${OpenBLAS${OPENBLAS_LIB_NAME}_LIBRARY}
        COMPILE_OUTPUT_VARIABLE COMPILE_OUT
        RUN_OUTPUT_VARIABLE RUN_OUT)
    IF(FIND_DEBUG)
        MESSAGE(STATUS "_RUN_RES : ${_RUN_RES}")
        MESSAGE(STATUS "_COMPILE_RES: ${_COMPILE_RES}")
        MESSAGE(STATUS "COMPILE_OUT: ${COMPILE_OUT}")
        MESSAGE(STATUS "RUN_OUT: ${RUN_OUT}")
    ENDIF()
    FILE(REMOVE ${CMAKE_BINARY_DIR}/otest.c)

    IF ( NOT ( _RUN_RES EQUAL 0 AND _COMPILE_RES ) )
        MESSAGE(STATUS "${OPENBLAS_PREFIX} ${OpenBLAS${OPENBLAS_LIB_NAME}_LIBRARY} does not contain the ${OPENBLAS_LIB_NAME} version of OpenBLAS.")
        SET(OpenBLAS${OPENBLAS_LIB_NAME}_LIBRARY FALSE)
    ENDIF()
    UNSET(_RUN_RES)
    UNSET(_COMPILE_RES)
    UNSET(COMPILE_OUT)
    UNSET(RUN_OUT)
ELSE()
    SET(OpenBLAS${OPENBLAS_LIB_NAME}_FOUND FALSE)
ENDIF()

include(FindPackageHandleStandardArgs)

# INCLUDE(find_package_handle_standard_args)
find_package_handle_standard_args(OpenBLAS${OPENBLAS_LIB_NAME} REQUIRED_VARS OpenBLAS${OPENBLAS_LIB_NAME}_LIBRARY)

UNSET(TEST_CODE)
UNSET(SEARCH_NAME)
UNSET(SEARCH_PATH)
UNSET(OPENBLAS_PREFIX)
UNSET(OPENBLAS_LIB_NAME)
UNSET(OPENBLAS_GET_PARALLEL)
