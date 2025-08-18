# Requires CMP0069 set to new.

include(CheckIPOSupported)
check_ipo_supported(RESULT supported OUTPUT error)
if ( supported )
    message(STATUS "IPO / LTO enabled")
    IF ( CMAKE_C_FLAGS MATCHES "-flto=auto")
        STRING(REPLACE "-flto=auto" "" CMAKE_C_FLAGS_X "${CMAKE_C_FLAGS}")
        SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS_X}")
    ENDIF()

    IF ( CMAKE_CXX_FLAGS MATCHES "-flto=auto")
        STRING(REPLACE "-flto=auto" "" CMAKE_CXX_FLAGS_X "${CMAKE_CXX_FLAGS}")
        SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS_X}")
    ENDIF()

    IF ( CMAKE_Fortran_FLAGS MATCHES "-flto=auto")
        STRING(REPLACE "-flto=auto" "" CMAKE_Fortran_FLAGS_X "${CMAKE_Fortran_FLAGS}")
        SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS_X}")
    ENDIF()

    set(CMAKE_INTERPROCEDURAL_OPTIMIZATION TRUE)
else()
    message(STATUS "IPO / LTO not supported: <${error}>")
    set(CMAKE_INTERPROCEDURAL_OPTIMIZATION FALSE)
endif()

