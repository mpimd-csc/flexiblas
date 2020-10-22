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

    # Standard Flags
    ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-frecursive")
    ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-fPIC")

    # Debug Flags
    ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_DEBUG "-Wimplicit-procedure")
    ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_DEBUG "-Wall")
    ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_DEBUG "-Wunused")
    ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_DEBUG "-Warray-temporaries")
    ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_DEBUG "-fbacktrace")
    ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS_DEBUG "-Wshadow")

    IF ( FORTRAN_BOUND_CHECK STREQUAL ON )
        ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-fcheck=bounds")
    ENDIF()

    IF ( FORTRAN_SANITIZE STREQUAL ON OR SANITIZE STREQUAL ON)
	    ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-fcheck=all")
	    ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-fsanitize=undefined")
	    ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-fsanitize=address")
	    ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-fsanitize=leak")
    ENDIF()

    # Integer 8 Support
    IF(INTEGER8 STREQUAL ON)
        SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fdefault-integer-8")
    ENDIF()

    # Host optimizations
    IF(HOSTOPT STREQUAL ON)
        ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-O3")
        ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-march=native")
        ADD_FORTRAN_COMPILER_FLAG(CMAKE_Fortran_FLAGS "-mtune-native")
    ENDIF()

    LIST(APPEND LIBRARIES "gfortran")

    SET(I8FLAG "-fdefault-integer-8")
ELSEIF(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
    # Intel
    IF ( FORTRAN_BOUND_CHECK STREQUAL ON )
        SET (_BC "-check bounds")
    ELSE( )
        SET (_BC "")
    ENDIF()

    SET(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -warn -g -warn nointerfaces  ${_BC} -traceback -debug all")
    SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -recursive -fpic -fPIC -heap-arrays 64 ")
    SET(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -O3 -funroll-loops")

    IF(HOSTOPT STREQUAL ON)
        SET(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -xHost ")
        SET(CMAKE_Fortran_FLAGS_DEBUG   "${CMAKE_Fortran_FLAGS_DEBUG} -xHost")
    ENDIF()

    IF(INTEGER8 STREQUAL ON)
        SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -i8")
    ENDIF()
    LIST(APPEND LIBRARIES "ifcore")

    SET(I8FLAG "-i8")
ELSEIF(CMAKE_Fortran_COMPILER_ID STREQUAL "PGI")
    # PGI
    SET(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -O4 ")
    SET(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -g -Minfo=all")
    SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fPIC -fpic -Mnoipa")
    IF(INTEGER8 STREQUAL ON)
        SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -i8")
    ENDIF()

    STRING(REPLACE "-Mbounds" "" CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG}")
    STRING(REPLACE "-Mipa=fast" "" CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}")
    STRING(REPLACE "-Mipa=fast" "" CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE}")

    IF ( CMAKE_Fortran_FLAGS_DEBUG MATCHES "-Mbounds" )
	    STRING(REPLACE "-Mbounds" "" CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG}")
    ENDIF()
    SET(I8FLAG "-i8")
ELSEIF(CMAKE_Fortran_COMPILER_ID STREQUAL "XL")
    # IBM XL
    IF(HOSTOPT STREQUAL ON)
        SET(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -O5 -qessl -qhot -qtune=auto -qarch=auto -qnoipa")
    ELSE()
        SET(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -O3 -qessl -qhot -qnoipa")
    ENDIF()
    SET(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -qstrict=ieeefp -qreport -qlistfmt=html=all -qnosave -qxlf77=nopersistent")
    SET(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -qstrict=ieeefp -qnosave -qxlf77=nopersistent -qcheck  -qsigtrap")
    SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -qpic -qnosave -qxlf77=nopersistent -qalias=std  -qnoipa -qmaxmem=32768")
    STRING(REPLACE "-qhalt=e" "" CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}")

    IF(INTEGER8 STREQUAL ON)
        SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -qintsize=8")
    ENDIF()

    IF(OPENMP_FOUND)
        LIST(REMOVE_ITEM CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES "xlomp_ser")
    ENDIF()
    SET(I8FLAG "-qintsize=8")
ENDIF()

ENDIF() # Compiler loaded
