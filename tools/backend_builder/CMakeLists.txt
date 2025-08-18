cmake_minimum_required(VERSION 3.5)
PROJECT(FBBACKEND LANGUAGES C Fortran)
SET(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake ${CMAKE_MODULE_PATH})

INCLUDE(CheckCSourceRuns)
INCLUDE(CheckFortranFunctionExists)
INCLUDE(CheckFunctionExists)


MACRO(CheckBlasZdotcMKL BLAS_LIBS BLAS_LD_FLAGS INT8)
    SET(__CMAKE_REQUIRED_FLAGS ${CMAKE_REQUIRED_FLAGS})
    SET(__CMAKE_REQUIRED_DEFINITIONS ${CMAKE_REQUIRED_DEFINITIONS})
    SET(__CMAKE_REQUIRED_INCLUDES ${CMAKE_REQUIRED_INCLUDES})
    SET(__CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES})
    UNSET(ZDOTC_MKL CACHE)
    IF(INT8)
        SET(CMAKE_REQUIRED_DEFINITIONS "-DInt=int64_t")
    ELSE()
        SET(CMAKE_REQUIRED_DEFINITIONS "-DInt=int")
    ENDIF()
    SET(CMAKE_REQUIRED_LIBRARIES ${BLAS_LIBS})
    SET(CMAKE_REQUIRED_FLAGS ${BLAS_LD_FLAGS})
    CHECK_C_SOURCE_RUNS(
        "
        #include <stdio.h>
        #include <stdlib.h>
        #include <complex.h>
        #include <stdint.h>
        double complex zdotc_(Int *n, double complex *X, Int *incx, double complex *Y, Int *INCY , Int *dummy);
        int main ( ) {
        Int n = 4;
        Int incx = 1, incy = 1;
        Int dummy= 1;
        double complex x[4] = {1,1,3,4};
        double complex y[4] = {2,2,1,1};
        double complex ret;
        ret = zdotc_(&n,x,&incx,y,&incy,&dummy);
        printf(\"n = %d ret = %lg\", n, creal(ret));
        if (n!=4) return 0;
            return -1;
            abort();
            }"
            ZDOTC_MKL)
    SET(CMAKE_REQUIRED_FLAGS ${__CMAKE_REQUIRED_FLAGS})
    SET(CMAKE_REQUIRED_DEFINITIONS ${__CMAKE_REQUIRED_DEFINITIONS})
    SET(CMAKE_REQUIRED_INCLUDES ${__CMAKE_REQUIRED_INCLUDES})
    SET(CMAKE_REQUIRED_LIBRARIES ${__CMAKE_REQUIRED_LIBRARIES})

ENDMACRO()

MACRO(BuildBackendBlas VENDOR)
    string(TOLOWER ${VENDOR} ivendor)


    MESSAGE(STATUS "Build BLAS interface: ${VENDOR}")
    MESSAGE(STATUS "-- Libs: ${${VENDOR}_LIBRARY}")
    MESSAGE(STATUS "-- Linker Flags: ${${VENDOR}_LINKER_FLAGS}")
    MESSAGE(STATUS "-- INTEGER8 Flag: ${FLEXIBLAS_INTEGER8_FLAG}")
    MESSAGE(STATUS "-- CONFIG FILE: ${FLEXIBLAS_RC_DIR}/${VENDOR}.conf")
    MESSAGE(STATUS "-- Backend install dir: ${FLEXIBLAS_LIBRARY_DIR}")

    #
    # Configure the found backend
    #
    CheckBlasZdotcMKL("${${VENDOR}_LIBRARY}" "" ${INTEGER8})
    SET(__CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES})
    SET(CMAKE_REQUIRED_LIBRARIES ${${VENDOR}_LIBRARY})

    UNSET(ISFLEXIBLAS CACHE)
    UNSET(DAXPY CACHE)
    UNSET(MKL_OMATCOPY CACHE)
    UNSET(CBLAS_DAXPY CACHE)
    UNSET(_MKL_VERSION CACHE)
    Check_Function_Exists(flexiblas_exit ISFLEXIBLAS)
    Check_Fortran_Function_Exists(DAXPY  DAXPY)
    Check_Function_Exists(cblas_daxpy  CBLAS_DAXPY)
    Check_Function_Exists(MKL_Get_Version _MKL_VERSION)

    # Use the MKL Backend Code if it is MKL
    IF(NOT BACKEND_CODE)
        IF(_MKL_VERSION)
            SET(BACKEND_CODE src/intel_mkl/backend.c)
        ELSE()
            SET(BACKEND_CODE src/generic/backend.c)
        ENDIF()
    ENDIF()

    SET(CMAKE_REQUIRED_LIBRARIES ${__CMAKE_REQUIRED_LIBRARIES})

    IF(NOT DAXPY)
        MESSAGE(FATAL_ERROR "${VENDOR} library does not contain a BLAS implementation or does not link.")
    ENDIF()

    IF(ISFLEXIBLAS)
        MESSAGE(FATAL_ERROR "${VENDOR} (${${VENDOR}_LIBRARY}) contains FlexiBLAS. Disable the use with -D${VENDOR}=OFF")
    ENDIF()

    UNSET(EXTRA_FLAGS CACHE)

    #
    # Create the wrapper
    #
    ADD_LIBRARY(flexiblas_${ivendor} SHARED ${BACKEND_CODE})
    SET(EXTRA_FLAGS "${FLEXIBLAS_INTEGER8_FLAG}")

    IF (ZDOTC_MKL)
        MESSAGE(STATUS "${VENDOR} uses Intel ZDOTC")
        SET(EXTRA_FLAGS "${EXTRA_FLAGS} -DZDOTC_MKL")
    ENDIF (ZDOTC_MKL)

    IF(CBLAS_DAXPY AND CBLAS STREQUAL ON)
        MESSAGE(STATUS "Add CBLAS in dummy library")
        SET(EXTRA_FLAGS "${EXTRA_FLAGS} -DCBLAS_INTERFACE")
    ENDIF()

	SET_TARGET_PROPERTIES(flexiblas_${ivendor} PROPERTIES COMPILE_FLAGS "${EXTRA_FLAGS}")
    IF ( ${VENDOR}_LINKER_FLAGS)
        SET_TARGET_PROPERTIES(flexiblas_${ivendor} PROPERTIES LINK_FLAGS "${${VENDOR}_LINKER_FLAGS}")
    ENDIF()

    IF (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
        SET(BLAS_LIBRARIES ${BLAS_LIBRARIES} gfortran)
    ENDIF()

    TARGET_LINK_LIBRARIES(flexiblas_${ivendor} ${${VENDOR}_LIBRARY})
    INSTALL(TARGETS flexiblas_${ivendor} LIBRARY DESTINATION ${FLEXIBLAS_BACKEND_DIR}/${FLEXIBLAS_LIBRARY_DIR})

    LIST(APPEND EXTRA_BLAS "${VENDOR}")

    UNSET(LIBNAME CACHE)
    SET(LIBNAME "${CMAKE_SHARED_LIBRARY_PREFIX}flexiblas_${ivendor}${CMAKE_SHARED_LIBRARY_SUFFIX} ")
    FILE(WRITE "${CMAKE_BINARY_DIR}/${VENDOR}.conf" "[${VENDOR}]\nlibrary = ${FLEXIBLAS_BACKEND_DIR}/${LIBNAME}\n\n")
    INSTALL(FILES "${CMAKE_BINARY_DIR}/${VENDOR}.conf" DESTINATION "${FLEXIBLAS_RC_DIR}")
    # Check for default setting
    STRING (TOLOWER  "${FLEXIBLAS_DEFAULT}" FDB_LOW)
    IF ( FDB_LOW STREQUAL ivendor)
        SET(DEFAULT_FOUND TRUE)
    ENDIF()
ENDMACRO()

MACRO(BuildFindPackage VENDOR BACKEND_CODE)
    FIND_PACKAGE(${VENDOR})
    IF ( ${VENDOR}_FOUND AND NOT "${${VENDOR}}" STREQUAL "OFF" )
    	MESSAGE(STATUS "Build BLAS interface: ${VENDOR}")
        MESSAGE(STATUS "Libs: ${${VENDOR}_LIBRARY}")
        BuildBackendBlas(${VENDOR} ${BACKEND_CODE})
    ELSEIF(NOT ${VENDOR}_FOUND)
        MESSAGE(STATUS "${VENDOR} not found.")
    ELSE()
        MESSAGE(STATUS "Found ${VENDOR} but deactivated manually.")
    ENDIF()
ENDMACRO()




OPTION(INTEGER8 "Use 8 Byte Integer" OFF)
OPTION(BUILD_SHARED_LIBS "Enable Shared Library" ON)

IF(INTEGER8 STREQUAL ON)
    SET(FLEXIBLASCONFIG_NAME "flexiblas64-config")
ELSE()
    SET(FLEXIBLASCONFIG_NAME "flexiblas-config")
ENDIF()

IF(NOT FLEXIBLASCONFIG)
    FIND_PROGRAM(FLEXIBLASCONFIG ${FLEXIBLASCONFIG_NAME})
ENDIF()

IF (NOT FLEXIBLASCONFIG)
    MESSAGE(FATAL_ERROR "flexiblas-config or flexiblas64-config not found. Please specify it using -DFLEXIBLASCONFIG=PATH")
ELSE()
    MESSAGE(STATUS "flexiblas-config found: ${FLEXIBLASCONFIG}")
ENDIF()


execute_process( COMMAND ${FLEXIBLASCONFIG} --incdir OUTPUT_VARIABLE FLEXIBLAS_INCLUDE_DIR OUTPUT_STRIP_TRAILING_WHITESPACE)
MESSAGE(STATUS "flexiblas include directory: ${FLEXIBLAS_INCLUDE_DIR}")
INCLUDE_DIRECTORIES(${FLEXIBLAS_INCLUDE_DIR})
execute_process( COMMAND ${FLEXIBLASCONFIG} --backenddir OUTPUT_VARIABLE FLEXIBLAS_BACKEND_DIR OUTPUT_STRIP_TRAILING_WHITESPACE)
MESSAGE(STATUS "flexiblas backend directory: ${FLEXIBLAS_BACKEND_DIR}")
execute_process( COMMAND ${FLEXIBLASCONFIG} --rcdir OUTPUT_VARIABLE FLEXIBLAS_RC_DIR OUTPUT_STRIP_TRAILING_WHITESPACE)
MESSAGE(STATUS "flexiblasrc directory: ${FLEXIBLAS_RC_DIR}")
execute_process( COMMAND ${FLEXIBLASCONFIG} --i8cflag OUTPUT_VARIABLE FLEXIBLAS_INTEGER8_FLAG OUTPUT_STRIP_TRAILING_WHITESPACE)
MESSAGE(STATUS "flexiblas integer-8 flag: ${FLEXIBLAS_INTEGER8_FLAG}")



SET(VENDOR ${BLAS_NAME})
SET(${VENDOR}_LIBRARY ${BLAS_LIBRARY})
SET(${VENDOR}_LINKER_FLAGS ${BLAS_LINKER_FLAGS})


IF (NOT VENDOR)
    MESSAGE(FATAL_ERROR "BLAS_NAME not set. Please specify the name of the BLAS library using -DBLAS_NAME=<NAME>")
ENDIF()

IF (NOT ${VENDOR}_LIBRARY)
    MESSAGE(FATAL_ERROR "BLAS_LIBRARY not set. Please specify the required libraries using -DBLAS_LIBRARY=<>")
ENDIF()

BuildBackendBlas(${VENDOR})

