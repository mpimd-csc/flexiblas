IF (CMAKE_Fortran_COMPILER_LOADED)

OPTION(FORTRAN_BOUND_CHECK "Enable the Fortran bound checker" OFF)
OPTION(FORTRAN_SANITIZE    "Enable the Fortran sanitizer" OFF)

IF(NOT "${HOSTOPT}" STREQUAL "")
    IF(NOT (HOSTOPT STREQUAL OFF OR HOSTOPT STREQUAL ON))
        STRING(SUBSTRING "${HOSTOPT}" 0 1 FIRST_CHAR)
        IF( "${FIRST_CHAR}" STREQUAL "/" )
            MESSAGE(STATUS "Load user supplied Host Optimizations for Fortran -- ${HOSTOPT}")
            INCLUDE(${HOSTOPT})
        ELSE()
            MESSAGE(STATUS  "Load user supplied Host Optimizations for Fortran -- ${CMAKE_BINARY_DIR}/${HOSTOPT}")
            INCLUDE(${CMAKE_BINARY_DIR}/${HOSTOPT})
        ENDIF()
        SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${HOSTOPT_Fortran}")
    ENDIF()
ENDIF()

INCLUDE(CheckFortranCompilerFlag_meta)
# INCLUDE(CheckFortranCompilerFlag)


FUNCTION(ADD_FORTRAN_COMPILER_FLAG VAR FLAGNAME )
    IF(DEFINED CACHE{FORTRAN_${FLAGNAME}_WORK})
        RETURN()
    ENDIF()
    SET(_SAVE ${CMAKE_REQUIRED_QUIET})
    UNSET(_WORKS CACHE)
    SET(CMAKE_REQUIRED_QUIET TRUE)
    CHECK_FORTRAN_COMPILER_FLAG("${FLAGNAME}" _WORKS)
    SET(FORTRAN_${FLAGNAME}_WORK ${_WORKS} CACHE INTERNAL "Fortran Compiler supports ${FLAGNAME}")
    IF ( _WORKS)
        SET(${VAR} "${${VAR}} ${FLAGNAME}" PARENT_SCOPE)
        MESSAGE(STATUS "Fortran compiler supports ${FLAGNAME}")
    ELSE()
        MESSAGE(STATUS "Fortran compiler does not support ${FLAGNAME}")
    ENDIF()
    SET(CMAKE_REQUIRED_QUIET ${_SAVE})
ENDFUNCTION()

IF(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    # GNU
    ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-frecursive")
    ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-fPIC")

    ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_RELEASE "-O3")

    ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_DEBUG "-Wimplicit-procedure")
    ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_DEBUG "-Wall")
    ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_DEBUG "-Wunused")
    ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_DEBUG "-Warray-temporaries")
    ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_DEBUG "-fbacktrace")
    ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_DEBUG "-Wshadow")

    IF(DEBUGOPT STREQUAL ON)
        ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_DEBUG "-O3")
    ELSE()
        ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_DEBUG "-O0")
    ENDIF()

    IF ( FORTRAN_BOUND_CHECK STREQUAL ON )
        ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-fcheck=bounds")
    ENDIF()

    IF ( FORTRAN_SANITIZE STREQUAL ON OR SANITIZE STREQUAL ON)
	    ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-fcheck=all")
	    ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-fsanitize=undefined")
	    ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-fsanitize=address")
	    ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-fsanitize=leak")
    ENDIF()

    IF(INTEGER8 STREQUAL ON)
        SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fdefault-integer-8")
    ENDIF()

    IF(HOSTOPT STREQUAL ON)
        ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-march=native")
        ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-mtune-native")
    ENDIF()

    LIST(APPEND LIBRARIES "gfortran")

    SET(I8FLAG "-fdefault-integer-8")

ELSEIF(CMAKE_Fortran_COMPILER_ID STREQUAL "Flang")
    SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fPIC -Mrecursive")
    SET(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -O3")
    SET(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -Wimplicit-procedure -Wall -Wunused -Warray-temporaries -fbacktrace -Wshadow")

    IF(DEBUGOPT STREQUAL ON)
        SET(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -O3")
    ELSE()
        SET(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -O0")
    ENDIF()

    IF(INTEGER8 STREQUAL ON)
        SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fdefault-integer-8")
    ENDIF()

    IF(HOSTOPT STREQUAL ON)
        SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -march=native -mtune-native")
    ENDIF()

    SET(I8FLAG "-fdefault-integer-8")

ELSEIF(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
    # Intel
    IF (WIN32)
        SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /recursive /heap-arrays 64")
        SET(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} /O3 /Qunroll")
        SET(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} /warn all /Zi /warn nointerfaces /traceback /debug all")

        IF(DEBUGOPT STREQUAL ON)
            ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_DEBUG "/O3")
        ELSE()
            ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_DEBUG "/O0")
        ENDIF()

        IF ( FORTRAN_BOUND_CHECK STREQUAL ON )
            SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /check bounds")
        ENDIF()

        IF(HOSTOPT STREQUAL ON)
            SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /QxHost")
        ENDIF()

        IF(INTEGER8 STREQUAL ON)
            SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /4I8")
        ENDIF()

        LIST(APPEND LIBRARIES "ifcore")

        SET(I8FLAG "/4I8")
    ELSE()
        SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -recursive -fpic -heap-arrays 64")
        SET(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -O3 -unroll")
        SET(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -warn all -g -warn nointerfaces -traceback -debug all")

        IF(DEBUGOPT STREQUAL ON)
            ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_DEBUG "-O3")
        ELSE()
            ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_DEBUG "-O0")
        ENDIF()

        IF ( FORTRAN_BOUND_CHECK STREQUAL ON )
            SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -check bounds")
        ENDIF()

        IF(INTEGER8 STREQUAL ON)
            SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -i8")
        ENDIF()

        IF(HOSTOPT STREQUAL ON)
            SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -xHost")
        ENDIF()

        LIST(APPEND LIBRARIES "ifcore")

        SET(I8FLAG "-i8")
    ENDIF()

ELSEIF(CMAKE_Fortran_COMPILER_ID STREQUAL "NVHPC" OR CMAKE_Fortran_COMPILER_ID STREQUAL "PGI")
    # Nvidia HPC SDK (nvfortran) or PGI
    SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fpic -Mrecursive -Mnoipa")
    SET(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -O3")
    SET(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -Minfo=all")

    IF(DEBUGOPT STREQUAL ON)
        SET(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -gopt -O3")
    ELSE()
        SET(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -g -O0")
    ENDIF()

    IF ( FORTRAN_BOUND_CHECK STREQUAL ON )
        SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Mbounds")
    ENDIF()

    IF(INTEGER8 STREQUAL ON)
        SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -i8")
    ENDIF()

    IF(HOSTOPT STREQUAL ON)
        SET(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -fast -tp=native")
        IF (DEBUGOPT STREQUAL ON)
            SET(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -fast -tp=native")
        ENDIF()
    ENDIF()

    SET(I8FLAG "-i8")

ELSEIF(CMAKE_Fortran_COMPILER_ID STREQUAL "XL")
    # IBM XL
    SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -qpic -qstrict=ieeefp -qnosave -qxlf77=nopersistent -qalias=std -qnoipa -qmaxmem=32768")
    SET(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -qessl -qhot=level=2 -qreport -qlistfmt=html=all")
    SET(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -qsigtrap -g9")

    IF(HOSTOPT STREQUAL ON)
        SET(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -O5 -qtune=auto -qarch=auto")
    ELSE()
        SET(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -O3")
    ENDIF()

    IF ( FORTRAN_BOUND_CHECK STREQUAL ON )
        SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -qcheck=all")
    ENDIF()

    IF(DEBUGOPT STREQUAL ON)
        SET(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -O2")
    ELSE()
        SET(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -O0")
    ENDIF()

    STRING(REPLACE "-qhalt=e" "" CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}")

    IF(INTEGER8 STREQUAL ON)
        SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -qintsize=8")
    ENDIF()

    IF(OPENMP_FOUND)
        LIST(REMOVE_ITEM CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES "xlomp_ser")
        SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}")
    ENDIF()
    SET(I8FLAG "-qintsize=8")
ENDIF()

ENDIF() # Compiler loaded
