IF ( CMAKE_Fortran_COMPILER_LOADED )
    IF(NOT DEFINED CACHE{Fortran_W_CONVERSION_LOADED})
        INCLUDE(CheckFortranCompilerFlag)

        IF ( CMAKE_Fortran_FLAGS MATCHES "-Wconversion")
            STRING(REPLACE "-Wconversion" "" CMAKE_Fortran_FLAGS_X "${CMAKE_Fortran_FLAGS}")
            SET(CMAKE_Fortran_FLAGS "{CMAKE_Fortran_FLAGS_X}" CACHE INTERNAL "")
            SET (Fortran_W_CONVERSION 1)
        ENDIF()

        IF (NOT Fortran_W_CONVERSION )
            check_fortran_compiler_flag("-Wconversion" Fortran_W_CONVERSION)
        ENDIF()

        IF ( Fortran_W_CONVERSION)
            SET (CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -Wconversion" CACHE INTERNAL "")
            SET (CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -Wconversion" CACHE INTERNAL "")
            SET (CMAKE_Fortran_FLAGS_RELWITHDEBINFO "${CMAKE_Fortran_FLAGS_RELWITHDEBINFO} -Wconversion" CACHE INTERNAL "")
            SET (CMAKE_Fortran_FLAGS_MINSIZEREL "${CMAKE_Fortran_FLAGS_MINSIZEREL} -Wconversion" CACHE INTERNAL "")
        ENDIF()
        SET(Fortran_W_CONVERSION_LOADED TRUE CACHE INTERNAL "Fortran Flags: -Wconversion")
    ENDIF()
ENDIF() # CMAKE_Fortran_COMPILER_LOADED


