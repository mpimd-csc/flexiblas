IF ( LAPACK )
    ADD_LIBRARY(flexiblas_hook_profile SHARED
        profile_hook.c
        profile_blas.c
        profile_lapack_${_LAPACK_FILE_SUFFIX}.c  )
ELSE()
    ADD_LIBRARY(flexiblas_hook_profile SHARED
        profile_hook.c
        profile_blas.c
        )
ENDIF()
IF (FLEXIBLAS_INTEGER8)
    SET(EXTRA_FLAGS "-DBACKEND_INTEGER8")
ELSE ()
    SET(EXTRA_FLAGS "")
ENDIF()



SET_TARGET_PROPERTIES(flexiblas_hook_profile PROPERTIES COMPILE_FLAGS "${EXTRA_FLAGS}")
TARGET_LINK_LIBRARIES(flexiblas_hook_profile flexiblas cscutils $<$<BOOL:${OpenMP_C_FOUND}>:OpenMP::OpenMP_C>)
set_target_properties(flexiblas_hook_profile PROPERTIES LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib/${FLEXIBLAS_LIBRARY_DIR}/)

if (APPLE)
    set_target_properties(flexiblas_hook_profile PROPERTIES LINK_FLAGS "-undefined dynamic_lookup -flat_namespace")
endif()

IF (WIN32)
    INSTALL(TARGETS flexiblas_hook_profile
            LIBRARY DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR}/${FLEXIBLAS_LIBRARY_DIR}
            RUNTIME DESTINATION ${CMAKE_INSTALL_FULL_BINDIR}
            ARCHIVE DESTINATION ${CMAKE_INSTALL_FULL_BINDIR})
ELSE()
    INSTALL(TARGETS flexiblas_hook_profile LIBRARY DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR}/${FLEXIBLAS_LIBRARY_DIR})
ENDIF()
