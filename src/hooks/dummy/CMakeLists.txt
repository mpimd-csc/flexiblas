IF (DEV STREQUAL ON)
    SET(DUMMY_DEFAULT ON)
ELSE()
    SET(DUMMY_DEFAULT OFF)
ENDIF()
OPTION(DUMMY_HOOK "Build the dummy hook" ${DUMMY_DEFAULT})

IF ( NOT DUMMY_HOOK)
    ADD_LIBRARY(flexiblas_hook_dummy SHARED
        dummy_hook.c
        )

    IF (FLEXIBLAS_INTEGER8)
        SET(EXTRA_FLAGS "-DBACKEND_INTEGER8")
    ELSE ()
        SET(EXTRA_FLAGS "")
    ENDIF()

    SET_TARGET_PROPERTIES(flexiblas_hook_dummy PROPERTIES COMPILE_FLAGS "${EXTRA_FLAGS}")
    TARGET_LINK_LIBRARIES(flexiblas_hook_dummy flexiblas)
    set_target_properties(flexiblas_hook_dummy PROPERTIES LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib/${FLEXIBLAS_LIBRARY_DIR}/)

    if (APPLE)
       set_target_properties(flexiblas_hook_dummy PROPERTIES LINK_FLAGS "-undefined dynamic_lookup -flat_namespace")
    endif()

    IF (WIN32)
        INSTALL(TARGETS flexiblas_hook_dummy
                LIBRARY DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR}/${FLEXIBLAS_LIBRARY_DIR}
                RUNTIME DESTINATION ${CMAKE_INSTALL_FULL_BINDIR}
                ARCHIVE DESTINATION ${CMAKE_INSTALL_FULL_BINDIR})
    ELSE()
        INSTALL(TARGETS flexiblas_hook_dummy LIBRARY DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR}/${FLEXIBLAS_LIBRARY_DIR})
    ENDIF()
ENDIF()
