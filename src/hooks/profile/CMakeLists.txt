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
IF (FLEXIBLAS_INTEGER8 STREQUAL ON)
  SET(EXTRA_FLAGS "-DBACKEND_INTEGER8")
ELSE ()
  SET(EXTRA_FLAGS "")
ENDIF()

SET_TARGET_PROPERTIES(flexiblas_hook_profile PROPERTIES COMPILE_FLAGS "${EXTRA_FLAGS}")
TARGET_LINK_LIBRARIES(flexiblas_hook_profile flexiblas cscutils)

INSTALL(TARGETS flexiblas_hook_profile LIBRARY DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR}/${FLEXIBLAS_LIBRARY_DIR})

