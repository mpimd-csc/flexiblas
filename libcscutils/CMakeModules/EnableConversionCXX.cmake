IF ( CMAKE_CXX_COMPILER_LOADED )
    IF (NOT DEFINED CACHE{CXX_W_CONVERSION_LOADED})
        INCLUDE(CheckCXXCompilerFlag)

        IF ( CMAKE_CXX_FLAGS MATCHES "-Wconversion")
            STRING(REPLACE "-Wconversion" "" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
            SET (CXX_W_CONVERSION 1)
        ENDIF()

        IF (NOT CXX_W_CONVERSION )
            check_cxx_compiler_flag("-Wconversion" CXX_W_CONVERSION)
        ENDIF()

        IF ( CXX_W_CONVERSION)
            SET (CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -Wconversion")
            SET (CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -Wconversion")
            SET (CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} -Wconversion")
            SET (CMAKE_CXX_FLAGS_MINSIZEREL "${CMAKE_CXX_FLAGS_MINSIZEREL} -Wconversion")
        ENDIF()
        SET(CXX_W_CONVERSION_LOADED TRUE CACHE INTERNAL "CXX Flags: -Wconversion")
    ENDIF()
ENDIF() # CMAKE_CXX_COMPILER_LOADED

