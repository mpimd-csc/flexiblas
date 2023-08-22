IF ( CMAKE_C_COMPILER_LOADED )
    INCLUDE(CheckCCompilerFlag)

    IF ( CMAKE_C_FLAGS MATCHES "-Wstrict-prototypes")
        STRING(REPLACE "-Wstrict-prototypes" "" CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
        SET (C_W_STRICT_PROTOTYPES 1)
    ENDIF()

    IF (NOT C_W_STRICT_PROTOTYPES )
        check_c_compiler_flag("-Wstrict-prototypes" C_W_STRICT_PROTOTYPES)
    ENDIF()

    IF ( C_W_STRICT_PROTOTYPES)
        SET (CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -Wstrict-prototypes")
        SET (CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -Wstrict-prototypes")
        SET (CMAKE_C_FLAGS_RELWITHDEBINFO "${CMAKE_C_FLAGS_RELWITHDEBINFO} -Wstrict-prototypes")
        SET (CMAKE_C_FLAGS_MINSIZEREL "${CMAKE_C_FLAGS_MINSIZEREL} -Wstrict-prototypes")
    ENDIF()
ENDIF() # CMAKE_C_COMPILER_LOADED


