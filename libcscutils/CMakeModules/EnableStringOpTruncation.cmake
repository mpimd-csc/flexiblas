IF ( CMAKE_C_COMPILER_LOADED )
    IF (NOT DEFINED CACHE{C_W_STRING_OP_TRUNCATION_LOADED})
        INCLUDE(CheckCCompilerFlag)

        IF ( CMAKE_C_FLAGS MATCHES "-Wstringop-truncation")
            STRING(REPLACE "-Wstringop-truncation" "" CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
            SET (C_W_STRING_OP_TRUNCATION 1)
        ENDIF()

        IF (NOT C_W_STRING_OP_TRUNCATION )
            check_c_compiler_flag("-Wstringop-truncation" C_W_STRING_OP_TRUNCATION)
        ENDIF()

        IF ( C_W_STRING_OP_TRUNCATION)
            SET (CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -Wstringop-truncation")
            SET (CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -Wstringop-truncation")
            SET (CMAKE_C_FLAGS_RELWITHDEBINFO "${CMAKE_C_FLAGS_RELWITHDEBINFO} -Wstringop-truncation")
            SET (CMAKE_C_FLAGS_MINSIZEREL "${CMAKE_C_FLAGS_MINSIZEREL} -Wstringop-truncation")
        ENDIF()
        SET(C_W_STRING_OP_TRUNCATION_LOADED TRUE CACHE INTERNAL "C Flags: -Wstringop-truncation")
    ENDIF()
ENDIF() # CMAKE_C_COMPILER_LOADED

IF ( CMAKE_CXX_COMPILER_LOADED )
    IF(NOT DEFINED CACHE{CXX_W_STRING_OP_TRUNCATION_LOADED})
        INCLUDE(CheckCXXCompilerFlag)

        IF ( CMAKE_CXX_FLAGS MATCHES "-Wstringop-truncation")
            STRING(REPLACE "-Wstringop-truncation" "" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
            SET (CXX_W_STRING_OP_TRUNCATION 1)
        ENDIF()

        IF (NOT CXX_W_STRING_OP_TRUNCATION )
            check_cxx_compiler_flag("-Wstringop-truncation" CXX_W_STRING_OP_TRUNCATION)
        ENDIF()

        IF ( CXX_W_STRING_OP_TRUNCATION)
            SET (CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -Wstringop-truncation")
            SET (CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -Wstringop-truncation")
            SET (CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} -Wstringop-truncation")
            SET (CMAKE_CXX_FLAGS_MINSIZEREL "${CMAKE_CXX_FLAGS_MINSIZEREL} -Wstringop-truncation")
        ENDIF()
        SET(CXX_W_STRING_OP_TRUNCATION_LOADED TRUE CACHE INTERNAL "CXX Flags: -Wstringop-truncation")
    ENDIF()
ENDIF() # CMAKE_CXX_COMPILER_LOADED


