

SET (SRC
    caxpby.f90
    cgeadd.f90
    cimatcopy.c
    comatcopy.c
    daxpby.f90
    dgeadd.f90
    dimatcopy.c
    domatcopy.c
    info.c
    dummy.c
    saxpby.f90
    sgeadd.f90
    simatcopy.c
    somatcopy.c
    zaxpby.f90
    zgeadd.f90
    zimatcopy.c
    zomatcopy.c
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

if ( SYS_BLAS_LIBRARY )
    message(STATUS "Use system provided NETLIB BLAS for fallback generation. SYS_BLAS_LIBRARY = ${SYS_BLAS_LIBRARY}")
    add_library(flexiblas_netlib_import STATIC IMPORTED)
    set_property(TARGET flexiblas_netlib_import PROPERTY IMPORTED_LOCATION ${SYS_BLAS_LIBRARY})
    add_library(flexiblas_netlib SHARED ${SRC})
    target_link_libraries(flexiblas_netlib PUBLIC flexiblas_netlib_import)
else()
    add_library(flexiblas_netlib SHARED ${SRC} $<TARGET_OBJECTS:flexiblas_netlib_objs>)
endif()


target_include_directories(flexiblas_netlib PRIVATE ${PROJECT_BINARY_DIR}/include)

if (APPLE)
    set_target_properties(flexiblas_netlib PROPERTIES LINK_FLAGS "-undefined dynamic_lookup -flat_namespace")
endif()


if (WIN32)
    install(TARGETS flexiblas_netlib EXPORT flexiblas-targets LIBRARY DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR}/${FLEXIBLAS_LIBRARY_DIR} RUNTIME DESTINATION ${CMAKE_INSTALL_FULL_BINDIR})
else ()
    SET(LIBNAME "${CMAKE_SHARED_LIBRARY_PREFIX}flexiblas_netlib${CMAKE_SHARED_LIBRARY_SUFFIX} ")
    FILE(WRITE "${CMAKE_BINARY_DIR}/${FLEXIBLAS_RC_DIR}/NETLIB.conf" "[NETLIB]\nlibrary = ${LIBNAME}\n\n")
    INSTALL(FILES "${CMAKE_BINARY_DIR}/${FLEXIBLAS_RC_DIR}/NETLIB.conf" DESTINATION "${CMAKE_INSTALL_FULL_SYSCONFDIR}/${FLEXIBLAS_RC_DIR}/")

    install(TARGETS flexiblas_netlib LIBRARY DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR}/${FLEXIBLAS_LIBRARY_DIR})
endif()
SET_TARGET_PROPERTIES(flexiblas_netlib  PROPERTIES LIBRARY_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/lib")
