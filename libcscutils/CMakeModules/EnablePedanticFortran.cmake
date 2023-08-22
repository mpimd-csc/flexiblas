IF ( CMAKE_Fortran_COMPILER_LOADED )
    INCLUDE(CheckFortranCompilerFlag)
    IF ( CMAKE_Fortran_FLAGS MATCHES "-Wpedantic")
        STRING(REPLACE "-Wpedantic" "" CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}")
        SET (Fortran_W_PEDANTIC 1)
    ENDIF()
    IF ( CMAKE_Fortran_FLAGS MATCHES "-pedantic")
        STRING(REPLACE "-pedantic" "" CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}")
        SET (Fortran_PEDANTIC 1)
    ENDIF()


    IF (NOT Fortran_W_PEDANTIC)
        check_fortran_compiler_flag("-Wpedantic" Fortran_W_PEDANTIC)
    ENDIF()

    IF (NOT Fortran_PEDANTIC)
        check_fortran_compiler_flag("-pedantic" Fortran_PEDANTIC)
    ENDIF()

    IF (Fortran_W_PEDANTIC)
        SET(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -Wpedantic")
        SET(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -Wpedantic")
        SET(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "${CMAKE_Fortran_FLAGS_RELWITHDEBINFO} -Wpedantic")
        SET(CMAKE_Fortran_FLAGS_MINSIZEREL "${CMAKE_Fortran_FLAGS_MINSIZEREL} -Wpedantic")
    ENDIF()

    IF (Fortran_PEDANTIC)
        SET(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -pedantic")
        SET(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -pedantic")
        SET(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "${CMAKE_Fortran_FLAGS_RELWITHDEBINFO} -pedantic")
        SET(CMAKE_Fortran_FLAGS_MINSIZEREL "${CMAKE_Fortran_FLAGS_MINSIZEREL} -pedantic")
    ENDIF()
ENDIF() # CMAKE_Fortran
