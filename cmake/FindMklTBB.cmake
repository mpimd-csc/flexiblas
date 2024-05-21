IF(DEFINED ENV{MKLROOT})
    IF(CMAKE_SIZEOF_VOID_P EQUAL 8)
        SET(SEARCH_PATH $ENV{MKLROOT}/lib/intel64 $ENV{MKLROOT}/lib/intel64_lin)
    ELSE()
        SET(SEARCH_PATH $ENV{MKLROOT}/lib/ia32)
    ENDIF()
ELSE()
    SET(SEARCH_PATH /usr/local/lib64 /usr/local/lib32/ /usr/local/lib/ /usr/lib64 /usr/lib32 /usr/lib)
ENDIF()


FIND_PACKAGE(TBB)
IF(NOT TBB_FOUND)
    MESSAGE(STATUS "MKL_TBB depends on TBB. Not Found.")
    RETURN()
ENDIF()

IF(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    SET(MKL_FRONTEND mkl_gf_lp64)
    SET(MKL_MIDDLE   mkl_tbb_thread)
    SET(MKL_CORE mkl_core )
    SET(MKL_ADD_LIBS ${TBB_LIBRARIES} stdc++ pthread m dl)
ELSEIF(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel" OR CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
    SET(MKL_FRONTEND mkl_intel_lp64)
    SET(MKL_MIDDLE   mkl_tbb_thread)
    SET(MKL_CORE mkl_core )
    SET(MKL_ADD_LIBS ${TBB_LIBRARIES} stdc++ pthread m dl)
ELSEIF(CMAKE_Fortran_COMPILER_ID STREQUAL "PGI")
    MESSAGE(STATUS "MKL_TBB is not supported with the PGI compiler.")
    RETURN()
ENDIF()

UNSET(MKL_TBB_FRONTEND_LIBRARY CACHE)
UNSET(MKL_TBB_MIDDLE_LIBRARY CACHE)
UNSET(MKL_TBB_CORE_LIBRARY CACHE)

MESSAGE(STATUS "Search for MKL: ${MKL_FRONTEND} ${MKL_MIDDLE} ${MKL_CORE}")
FIND_LIBRARY(MKL_TBB_FRONTEND_LIBRARY NAMES ${MKL_FRONTEND} HINTS ${SEARCH_PATH}   )
FIND_LIBRARY(MKL_TBB_MIDDLE_LIBRARY   NAMES ${MKL_MIDDLE} HINTS ${SEARCH_PATH}   )
FIND_LIBRARY(MKL_TBB_CORE_LIBRARY     NAMES ${MKL_CORE} HINTS ${SEARCH_PATH}   )

IF (NOT( MKL_TBB_FRONTEND_LIBRARY AND MKL_TBB_MIDDLE_LIBRARY AND MKL_TBB_CORE_LIBRARY ))
    RETURN()
ENDIF()

SET(MKL_VERSION_CODE
"#include <stdio.h>
#include <stdlib.h>

struct version_t {int major\; int minor\; int patch\; void* helper[32]\;}\;
extern void MKL_Get_Version(struct version_t * v)\;

int main (void) {
 struct version_t v\;
 MKL_Get_Version(&v)\;
 printf(\"%d.%d.%d\",v.major,v.minor,v.patch)\;
}
")

FILE(WRITE ${CMAKE_BINARY_DIR}/mkltest.c ${MKL_VERSION_CODE})
TRY_RUN(_RUN_RES _COMPILE_RES ${CMAKE_BINARY_DIR} ${CMAKE_BINARY_DIR}/mkltest.c
    LINK_LIBRARIES ${MKL_TBB_FRONTEND_LIBRARY} ${MKL_TBB_MIDDLE_LIBRARY} ${MKL_TBB_CORE_LIBRARY} ${MKL_ADD_LIBS}
    COMPILE_OUTPUT_VARIABLE COMPILE_OUT
    RUN_OUTPUT_VARIABLE RUN_OUT)
IF(FIND_DEBUG)
    MESSAGE(STATUS "MKL_TBB_FRONTEND_LIBRARY: ${MKL_TBB_FRONTEND_LIBRARY}")
    MESSAGE(STATUS "MKL_TBB_MIDDLE_LIBRARY:   ${MKL_TBB_MIDDLE_LIBRARY}")
    MESSAGE(STATUS "MKL_TBB_CORE_LIBRARY:     ${MKL_TBB_CORE_LIBRARY}")
    MESSAGE(STATUS "_RUN_RES : ${_RUN_RES}")
    MESSAGE(STATUS "_COMPILE_RES: ${_COMPILE_RES}")
    MESSAGE(STATUS "COMPILE_OUT: ${COMPILE_OUT}")
    MESSAGE(STATUS "RUN_OUT: ${RUN_OUT}")
ENDIF()
FILE(REMOVE ${CMAKE_BINARY_DIR}/mkltest.c)

IF (_RUN_RES AND NOT _COMPILE_RES)
    MESSAGE(STATUS "MKL_TBB OpenMP with ${MKL_TBB_FRONTEND_LIBRARY} ${MKL_TBB_MIDDLE_LIBRARY} ${MKL_TBB_CORE_LIBRARY} does not compile an example program.")
ELSE()
    SET(MklTBB_LIBRARY ${MKL_TBB_FRONTEND_LIBRARY} ${MKL_TBB_MIDDLE_LIBRARY} ${MKL_TBB_CORE_LIBRARY} ${MKL_ADD_LIBS})
    SET(MklTBB_VERSION ${RUN_OUT})
ENDIF()

#
# Call ILAVER to obtain LAPACK compatability level
#
SET(LAPACK_VERSION_CODE
    "PROGRAM LAPACKV
    INTEGER :: MA,MI,PA
    EXTERNAL ILAVER
    CALL ILAVER(MA, MI, PA)
    WRITE(*,'(I2,\".\",I2,\".\",I2)', advance=\"no\")  MA, MI, PA
    END PROGRAM"
)

    file(WRITE ${PROJECT_BINARY_DIR}/mkl_lapack_version_test.f90 ${LAPACK_VERSION_CODE})
    try_run(LV_RUN_STATE_Fortran LV_COMPILE_STATE_Fortran
        ${PROJECT_BINARY_DIR} ${PROJECT_BINARY_DIR}/mkl_lapack_version_test.f90
        LINK_LIBRARIES ${MKL_TBB_FRONTEND_LIBRARY} ${MKL_TBB_MIDDLE_LIBRARY} ${MKL_TBB_CORE_LIBRARY} ${MKL_ADD_LIBS}
        RUN_OUTPUT_VARIABLE MKL_LAPACK_VERSION
        COMPILE_OUTPUT_VARIABLE LAPACK_TRY_OUTPUT)
FILE(REMOVE ${CMAKE_BINARY_DIR}/mkl_lapack_version_test.f90)
STRING(REPLACE " " "" MklTBB_LAPACK_VERSION "${MKL_LAPACK_VERSION}")

MESSAGE(STATUS "MKL_TBB: ${MKL_TBB_FRONTEND_LIBRARY} ${MKL_TBB_MIDDLE_LIBRARY} ${MKL_TBB_CORE_LIBRARY} contains LAPACK ${MklTBB_LAPACK_VERSION}")

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(MklTBB REQUIRED_VARS MklTBB_LIBRARY)

UNSET(SEARCH_PATH)
UNSET(MKL_TBB_FRONTEND_LIBRARY)
UNSET(MKL_TBB_FRONTEND)
UNSET(MKL_TBB_MIDDLE_LIBRARY)
UNSET(MKL_CORE_LIBRARY)
UNSET(MKL_MIDDLE)
UNSET(MKL_CORE)
UNSET(MKL_ADD_LIBS)
