INCLUDE(CheckFortranFunctionExists)

if (NEED_LINKING_AGAINST_LIBM)
    SET(LIBM ${LIBM_NAME})
else()
    SET(LIBM)
endif()


SET (SRC
    info.c
    dummy.c
    )

if( (CMAKE_Fortran_COMPILER_ID STREQUAL "VisualAge" ) OR  # CMake 2.6
    (CMAKE_Fortran_COMPILER_ID STREQUAL "XL" ) )          # CMake 2.8
    # 	if( "${CMAKE_Fortran_FLAGS}" MATCHES "-qflttrap=[a-zA-Z:]:enable" )
    # 		set( FPE_EXIT TRUE )
    # 	endif()

    if( NOT ("${CMAKE_Fortran_FLAGS}" MATCHES "-qfixed") )
        message( STATUS "BLAS -- Enabling fixed format F90/F95 with -qfixed" )
        set( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -qfixed")
        #         CACHE STRING "Flags for Fortran compiler." FORCE )
    endif()
endif()

if (SYS_BLAS_LIBRARY)
    message(STATUS "Use system provided NETLIB BLAS for fallback generation. SYS_BLAS_LIBRARY = ${SYS_BLAS_LIBRARY}")
    add_library(flexiblas_netlib_import STATIC IMPORTED)
    set_property(TARGET flexiblas_netlib_import PROPERTY IMPORTED_LOCATION ${SYS_BLAS_LIBRARY})

    SET(_CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES})
    SET(CMAKE_REQUIRED_LIBRARIES flexiblas_netlib_import ${LIBM})

    MACRO(add_blas_function check)
        CHECK_FORTRAN_FUNCTION_EXISTS(${check} ${check}_EXISTS)
        if(NOT ${check}_EXISTS)
            SET(SRC ${SRC} ${ARGN})
        endif()
    ENDMACRO()

    add_blas_function(dgemmtr   dgemmtr.f sgemmtr.f cgemmtr.f zgemmtr.f)
    add_blas_function(daxpby    daxpby.f90 saxpby.f90 caxpby.f90 zaxpby.f90)
    add_blas_function(dgeadd    dgeadd.f90 sgeadd.f90 cgeadd.f90 zgeadd.f90)
    add_blas_function(domatcopy domatcopy.c somatcopy.c comatcopy.c zomatcopy.c )
    add_blas_function(dimatcopy dimatcopy.c simatcopy.c cimatcopy.c zimatcopy.c )

    MESSAGE(STATUS "BLAS sources to add: ${SRC}")
else()
    SET(SRC ${SRC} dgemmtr.f sgemmtr.f cgemmtr.f zgemmtr.f)
    SET(SRC ${SRC} daxpby.f90 saxpby.f90 caxpby.f90 zaxpby.f90)
    SET(SRC ${SRC} dgeadd.f90 sgeadd.f90 cgeadd.f90 zgeadd.f90)
    SET(SRC ${SRC} domatcopy.c somatcopy.c comatcopy.c zomatcopy.c )
    SET(SRC ${SRC} dimatcopy.c simatcopy.c cimatcopy.c zimatcopy.c )
endif()

IF (FLEXIBLAS_CBLAS)
    SET(SRC ${SRC}
        cblas_daxpby.c cblas_saxpby.c cblas_caxpby.c cblas_zaxpby.c
        cblas_dgeadd.c cblas_sgeadd.c cblas_cgeadd.c cblas_zgeadd.c
        cblas_domatcopy.c cblas_somatcopy.c cblas_comatcopy.c cblas_zomatcopy.c
        cblas_dimatcopy.c cblas_simatcopy.c cblas_cimatcopy.c cblas_zimatcopy.c
    )

ENDIF()

SET(CMAKE_REQUIRED_LIBRARIES ${_CMAKE_REQUIRED_LIBRARIES})


if ( SYS_BLAS_LIBRARY )
    if (FLEXIBLAS_CBLAS)
        add_library(flexiblas_netlib SHARED ${SRC} $<TARGET_OBJECTS:cblas_obj>)
    else()
        add_library(flexiblas_netlib SHARED ${SRC})
    endif()
    target_link_libraries(flexiblas_netlib PUBLIC flexiblas_netlib_import PRIVATE ${LIBM})
else()
    if (FLEXIBLAS_CBLAS)
        add_library(flexiblas_netlib SHARED ${SRC} $<TARGET_OBJECTS:flexiblas_netlib_objs> $<TARGET_OBJECTS:cblas_obj>)
    else()
        add_library(flexiblas_netlib SHARED ${SRC} $<TARGET_OBJECTS:flexiblas_netlib_objs> )
    endif()
    target_link_libraries(flexiblas_netlib PRIVATE ${LIBM})
endif()


target_include_directories(flexiblas_netlib PRIVATE
    ${PROJECT_BINARY_DIR}/include
    ${PROJECT_BINARY_DIR}/
    ${PROJECT_SOURCE_DIR}/include
    ${PROJECT_SOURCE_DIR}/src
    $<$<BOOL:${FLEXIBLAS_CBLAS}>:${PROJECT_SOURCE_DIR}/contributed/cblas/>
)

set_target_properties(flexiblas_netlib PROPERTIES LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib/${FLEXIBLAS_LIBRARY_DIR}/)

if (APPLE)
    set_target_properties(flexiblas_netlib PROPERTIES LINK_FLAGS "-undefined dynamic_lookup -flat_namespace")
endif()

if(INTEGER8)
    target_compile_definitions(flexiblas_netlib PRIVATE F77_INT=int64_t )
endif()


if (WIN32)
    install(TARGETS flexiblas_netlib
        EXPORT flexiblas-targets
        LIBRARY DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR}/${FLEXIBLAS_LIBRARY_DIR}
        RUNTIME DESTINATION ${CMAKE_INSTALL_FULL_BINDIR}
        ARCHIVE DESTINATION ${CMAKE_INSTALL_FULL_BINDIR})
else ()
    SET(LIBNAME "${CMAKE_SHARED_LIBRARY_PREFIX}flexiblas_netlib${CMAKE_SHARED_LIBRARY_SUFFIX} ")
    FILE(WRITE "${CMAKE_BINARY_DIR}/${FLEXIBLAS_RC_DIR}/NETLIB.conf" "[NETLIB]\nlibrary = ${LIBNAME}\n\n")
    INSTALL(FILES "${CMAKE_BINARY_DIR}/${FLEXIBLAS_RC_DIR}/NETLIB.conf" DESTINATION "${CMAKE_INSTALL_FULL_SYSCONFDIR}/${FLEXIBLAS_RC_DIR}/")

    install(TARGETS flexiblas_netlib LIBRARY DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR}/${FLEXIBLAS_LIBRARY_DIR})
endif()
