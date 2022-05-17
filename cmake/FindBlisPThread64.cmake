SET(BLIS_PREFIX "Blis-PThread64: ")
SET(BLIS_LIB_NAME PThread64)

IF ( EXISTS /etc/debian_version )
    # We are on debian
    MESSAGE(STATUS "${BLIS_PREFIX} ${CMAKE_LIBRARY_ARCHITECTURE} ")
    SET(SEARCH_PATH /usr/lib/${CMAKE_LIBRARY_ARCHITECTURE}/blis64-pthread /usr/lib/ /usr/lib64 /usr/local/lib/ /usr/local/lib64)
    SET(SEARCH_NAME blis64 blis)
ELSE()
    SET(SEARCH_PATH /usr/lib/ /usr/lib64 /usr/local/lib/ /usr/local/lib64)
    SET(SEARCH_NAME blisp64 blis64 blisp blis)
ENDIF()

FIND_LIBRARY(Blis${BLIS_LIB_NAME}_LIBRARY NAMES ${SEARCH_NAME} HINTS ${SEARCH_PATH}   DOC "Blis-Serial Location")

IF (Blis${BLIS_LIB_NAME}_LIBRARY)
    MESSAGE(STATUS "${BLIS_PREFIX} Candidate: ${Blis${BLIS_LIB_NAME}_LIBRARY}")

    IF(INTEGER8 STREQUAL ON)
        SET(CF "-DINTEGER8")
    ELSE()
        SET(CF "")
    ENDIF()

    SET(TEST_CODE "
    #ifdef NDEBUG
    #undef NDEBUG
    #endif
    #include<assert.h>
    #include<stdint.h>
    extern int64_t bli_info_get_enable_blas()\;
    extern int64_t bli_info_get_enable_openmp()\;
    extern int64_t bli_info_get_enable_pthreads()\;
    extern int64_t bli_info_get_int_type_size()\;
    int main() { assert(bli_info_get_int_type_size() == 64 && bli_info_get_enable_pthreads() == 1 && bli_info_get_enable_openmp()==0)\; return 0\;}
    ")

    FILE(WRITE ${CMAKE_BINARY_DIR}/otest.c ${TEST_CODE})
    TRY_RUN(_RUN_RES _COMPILE_RES ${CMAKE_BINARY_DIR} ${CMAKE_BINARY_DIR}/otest.c LINK_LIBRARIES ${Blis${BLIS_LIB_NAME}_LIBRARY}
        COMPILE_DEFINITIONS ${CF}
        COMPILE_OUTPUT_VARIABLE COMPILE_OUT
        RUN_OUTPUT_VARIABLE RUN_OUT)
    IF(FIND_DEBUG)
        MESSAGE(STATUS "_RUN_RES : ${_RUN_RES}")
        MESSAGE(STATUS "_COMPILE_RES: ${_COMPILE_RES}")
        MESSAGE(STATUS "COMPILE_OUT: ${COMPILE_OUT}")
        MESSAGE(STATUS "RUN_OUT: ${RUN_OUT}")
    ENDIF()
    FILE(REMOVE ${CMAKE_BINARY_DIR}/otest.c)

    IF ( NOT (_RUN_RES EQUAL 0 AND _COMPILE_RES) )
        MESSAGE(STATUS "${BLIS_PREFIX} ${Blis${BLIS_LIB_NAME}_LIBRARY} does not contain the ${BLIS_LIB_NAME} version of Blis.")
        SET(Blis${BLIS_LIB_NAME}_LIBRARY FALSE)
    ENDIF()
    UNSET(_RUN_RES)
    UNSET(_COMPILE_RES)
    UNSET(COMPILE_OUT)
    UNSET(RUN_OUT)
ELSE()
    SET(Blis${BLIS_LIB_NAME}_FOUND FALSE)
ENDIF()

include(FindPackageHandleStandardArgs)

# INCLUDE(find_package_handle_standard_args)
find_package_handle_standard_args(Blis${BLIS_LIB_NAME} REQUIRED_VARS Blis${BLIS_LIB_NAME}_LIBRARY)

UNSET(CF)
UNSET(TEST_CODE)
UNSET(SEARCH_NAME)
UNSET(SEARCH_PATH)
UNSET(BLIS_PREFIX)
UNSET(BLIS_LIB_NAME)
UNSET(BLIS_GET_PARALLEL)
