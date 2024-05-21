IF ( CMAKE_C_COMPILER_LOADED )
    IF(NOT DEFINED CACHE{C_W_CAST_QUAL_LOADED})
        INCLUDE(CheckCCompilerFlag)

        IF ( CMAKE_C_FLAGS MATCHES "-Wcast-qual")
            STRING(REPLACE "-Wcast-qual" "" CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
            SET (C_W_CAST_QUAL 1)
        ENDIF()

        IF (NOT C_W_CAST_QUAL )
            check_c_compiler_flag("-Wcast-qual" C_W_CAST_QUAL)
        ENDIF()

        IF ( C_W_CAST_QUAL)
            SET (CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -Wcast-qual")
            SET (CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -Wcast-qual")
            SET (CMAKE_C_FLAGS_RELWITHDEBINFO "${CMAKE_C_FLAGS_RELWITHDEBINFO} -Wcast-qual")
            SET (CMAKE_C_FLAGS_MINSIZEREL "${CMAKE_C_FLAGS_MINSIZEREL} -Wcast-qual")
        ENDIF()
        SET(C_W_CAST_QUAL_LOADED  TRUE CACHE INTERNAL "C Flags: -Wcast-qual")
    ENDIF()
ENDIF() # CMAKE_C_COMPILER_LOADED


