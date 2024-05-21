IF ( CMAKE_C_COMPILER_LOADED )
    IF (NOT DEFINED CACHE{C_W_MAY_BE_UNINITIALIZED_LOADED})
        INCLUDE(CheckCCompilerFlag)

        IF ( CMAKE_C_FLAGS MATCHES "-Wmaybe-uninitialized")
            STRING(REPLACE "-Wmaybe-uninitialized" "" CMAKE_C_FLAGS_X "${CMAKE_C_FLAGS}")
            SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS_X}" CACHE INTERNAL "")

            SET (C_W_MAY_BE_UNINITIALIZED 1)
        ENDIF()

        IF (NOT C_W_MAY_BE_UNINITIALIZED )
            check_c_compiler_flag("-Wmaybe-uninitialized" C_W_MAY_BE_UNINITIALIZED)
        ENDIF()

        IF ( C_W_MAY_BE_UNINITIALIZED)
            SET (CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -Wmaybe-uninitialized" CACHE INTERNAL "")
            SET (CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -Wmaybe-uninitialized" CACHE INTERNAL "")
            SET (CMAKE_C_FLAGS_RELWITHDEBINFO "${CMAKE_C_FLAGS_RELWITHDEBINFO} -Wmaybe-uninitialized" CACHE INTERNAL "")
            SET (CMAKE_C_FLAGS_MINSIZEREL "${CMAKE_C_FLAGS_MINSIZEREL} -Wmaybe-uninitialized" CACHE INTERNAL "")
        ENDIF()
        SET(C_W_MAY_BE_UNINITIALIZED_LOADED TRUE CACHE INTERNAL "C Flags: -Wmaybe-uninitialized")
    ENDIF()
ENDIF() # CMAKE_C_COMPILER_LOADED

IF ( CMAKE_CXX_COMPILER_LOADED )
    IF(NOT DEFINED CACHE{CXX_W_MAY_BE_UNINITIALIZED_LOADED})
        INCLUDE(CheckCXXCompilerFlag)

        IF ( CMAKE_CXX_FLAGS MATCHES "-Wmaybe-uninitialized")
            STRING(REPLACE "-Wmaybe-uninitialized" "" CMAKE_CXX_FLAGS_X "${CMAKE_CXX_FLAGS}")
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS_X}" CACHE INTERNAL "")

            SET (CXX_W_MAY_BE_UNINITIALIZED 1)
        ENDIF()

        IF (NOT CXX_W_MAY_BE_UNINITIALIZED )
            check_cxx_compiler_flag("-Wmaybe-uninitialized" CXX_W_MAY_BE_UNINITIALIZED)
        ENDIF()

        IF ( CXX_W_MAY_BE_UNINITIALIZED)
            SET (CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -Wmaybe-uninitialized" CACHE INTERNAL "")
            SET (CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -Wmaybe-uninitialized" CACHE INTERNAL "")
            SET (CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} -Wmaybe-uninitialized" CACHE INTERNAL "")
            SET (CMAKE_CXX_FLAGS_MINSIZEREL "${CMAKE_CXX_FLAGS_MINSIZEREL} -Wmaybe-uninitialized" CACHE INTERNAL "")
        ENDIF()
        SET(CXX_W_MAY_BE_UNINITIALIZED_LOADED TRUE CACHE INTERNAL "CXX Flags: -Wmaybe-uninitialized")
    ENDIF()
ENDIF() # CMAKE_CXX_COMPILER_LOADED


IF ( CMAKE_Fortran_COMPILER_LOADED )
    IF(NOT DEFINED CACHE{Fortran_W_MAY_BE_UNINITIALIZED_LOADED})
        INCLUDE(CheckFortranCompilerFlag)

        IF ( CMAKE_Fortran_FLAGS MATCHES "-Wmaybe-uninitialized")
            STRING(REPLACE "-Wmaybe-uninitialized" "" CMAKE_Fortran_FLAGS_X "${CMAKE_Fortran_FLAGS}")
            SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS_X}")
            SET (Fortran_W_MAY_BE_UNINITIALIZED 1)
        ENDIF()

        IF (NOT Fortran_W_MAY_BE_UNINITIALIZED )
            check_fortran_compiler_flag("-Wmaybe-uninitialized" Fortran_W_MAY_BE_UNINITIALIZED)
        ENDIF()

        IF ( Fortran_W_MAY_BE_UNINITIALIZED)
            SET (CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -Wmaybe-uninitialized" CACHE INTERNAL "")
            SET (CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -Wmaybe-uninitialized" CACHE INTERNAL "")
            SET (CMAKE_Fortran_FLAGS_RELWITHDEBINFO "${CMAKE_Fortran_FLAGS_RELWITHDEBINFO} -Wmaybe-uninitialized" CACHE INTERNAL "")
            SET (CMAKE_Fortran_FLAGS_MINSIZEREL "${CMAKE_Fortran_FLAGS_MINSIZEREL} -Wmaybe-uninitialized" CACHE INTERNAL "")
        ENDIF()
        SET(Fortran_W_MAY_BE_UNINITIALIZED_LOADED TRUE CACHE INTERNAL "Fortran Flags: -Wmaybe-uninitialized")
    ENDIF()
ENDIF() # CMAKE_Fortran_COMPILER_LOADED




