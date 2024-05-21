IF ( CMAKE_C_COMPILER_LOADED )
    IF(NOT DEFINED CACHE{C_W_CONVERSION_LOADED})
        INCLUDE(CheckCCompilerFlag)

        IF ( CMAKE_C_FLAGS MATCHES "-Wconversion")
            STRING(REPLACE "-Wconversion" "" CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
            SET (C_W_CONVERSION 1)
        ENDIF()

        IF (NOT C_W_CONVERSION )
            check_c_compiler_flag("-Wconversion" C_W_CONVERSION)
        ENDIF()

        IF ( C_W_CONVERSION)
            SET (CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -Wconversion")
            SET (CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -Wconversion")
            SET (CMAKE_C_FLAGS_RELWITHDEBINFO "${CMAKE_C_FLAGS_RELWITHDEBINFO} -Wconversion")
            SET (CMAKE_C_FLAGS_MINSIZEREL "${CMAKE_C_FLAGS_MINSIZEREL} -Wconversion")
        ENDIF()
        SET(C_W_CONVERSION_LOADED TRUE CACHE INTENAL "C Flags: -Wconversion")
    ENDIF()
ENDIF() # CMAKE_C_COMPILER_LOADED


