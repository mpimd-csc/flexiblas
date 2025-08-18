IF ( CMAKE_C_COMPILER_LOADED )
    IF (NOT DEFINED CACHE{C_W_PEDANTIC_LOADED})
        INCLUDE(CheckCCompilerFlag)
        IF ( CMAKE_C_FLAGS MATCHES "-Wpedantic")
            STRING(REPLACE "-Wpedantic" "" CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
            SET (C_W_PEDANTIC 1)
        ENDIF()

        IF (NOT C_W_PEDANTIC)
            check_c_compiler_flag("-Wpedantic" C_W_PEDANTIC)
        ENDIF()

        IF (C_W_PEDANTIC)
            SET(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -Wpedantic")
            SET(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -Wpedantic")
            SET(CMAKE_C_FLAGS_RELWITHDEBINFO "${CMAKE_C_FLAGS_RELWITHDEBINFO} -Wpedantic")
            SET(CMAKE_C_FLAGS_MINSIZEREL "${CMAKE_C_FLAGS_MINSIZEREL} -Wpedantic")
        ENDIF()
        SET(C_W_PEDANTIC_LÖADED TRUE CACHE INTERNAL "C Flags: -Wpedantic")
    ENDIF()
ENDIF() # CMAKE_C_COMPILER_LOADED


