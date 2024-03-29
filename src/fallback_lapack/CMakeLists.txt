IF(LAPACK_API_VERSION STREQUAL "3.12.0")
	SET(_LAPACK_FILE_SUFFIX 3_12_0)
ELSEIF (LAPACK_API_VERSION STREQUAL "3.12.0-wodprc")
	SET(_LAPACK_FILE_SUFFIX 3_12_0-wodprc) #
ELSEIF(LAPACK_API_VERSION STREQUAL "3.11.0")
	SET(_LAPACK_FILE_SUFFIX 3_11_0)
ELSEIF (LAPACK_API_VERSION STREQUAL "3.11.0-wodprc")
	SET(_LAPACK_FILE_SUFFIX 3_11_0-wodprc) #
ELSEIF(LAPACK_API_VERSION STREQUAL "3.10.1")
	SET(_LAPACK_FILE_SUFFIX 3_10_1)
ELSEIF (LAPACK_API_VERSION STREQUAL "3.10.1-wodprc")
	SET(_LAPACK_FILE_SUFFIX 3_10_1-wodprc) #
ELSEIF(LAPACK_API_VERSION STREQUAL "3.10.0")
	SET(_LAPACK_FILE_SUFFIX 3_10_0)
ELSEIF (LAPACK_API_VERSION STREQUAL "3.10.0-wodprc")
	SET(_LAPACK_FILE_SUFFIX 3_10_0-wodprc) #
ELSEIF(LAPACK_API_VERSION STREQUAL "3.9.1")
	SET(_LAPACK_FILE_SUFFIX 3_9_1)
ELSEIF (LAPACK_API_VERSION STREQUAL "3.9.1-wodprc")
	SET(_LAPACK_FILE_SUFFIX 3_9_1-wodprc) #
ELSEIF(LAPACK_API_VERSION STREQUAL "3.9.0")
	SET(_LAPACK_FILE_SUFFIX 3_9_0)
ELSEIF (LAPACK_API_VERSION STREQUAL "3.9.0-wodprc")
	SET(_LAPACK_FILE_SUFFIX 3_9_0-wodprc) #
ELSEIF(LAPACK_API_VERSION STREQUAL "3.8.0")
	SET(_LAPACK_FILE_SUFFIX 3_8_0)
ELSEIF (LAPACK_API_VERSION STREQUAL "3.8.0-wodprc")
	SET(_LAPACK_FILE_SUFFIX 3_8_0-wodprc) #
ELSEIF(LAPACK_API_VERSION STREQUAL "3.7.1")
	SET(_LAPACK_FILE_SUFFIX 3_7_0)
ELSEIF (LAPACK_API_VERSION STREQUAL "3.7.1-wodprc")
	SET(_LAPACK_FILE_SUFFIX 3_7_0-wodprc) #
ELSEIF(LAPACK_API_VERSION STREQUAL "3.7.0")
	SET(_LAPACK_FILE_SUFFIX 3_7_0)
ELSEIF (LAPACK_API_VERSION STREQUAL "3.7.0-wodprc")
	SET(_LAPACK_FILE_SUFFIX 3_7_0-wodprc) #
ELSEIF(LAPACK_API_VERSION STREQUAL "3.6.1")
	SET(_LAPACK_FILE_SUFFIX 3_6_1)
ELSEIF (LAPACK_API_VERSION STREQUAL "3.6.1-wodprc")
	SET(_LAPACK_FILE_SUFFIX 3_6_1-wodprc) #
ELSEIF (LAPACK_API_VERSION STREQUAL "3.6.0")
	SET(_LAPACK_FILE_SUFFIX 3_6_0) #
ELSEIF (LAPACK_API_VERSION STREQUAL "3.6.0-wodprc")
	SET(_LAPACK_FILE_SUFFIX 3_6_0-wodprc) #
ELSEIF (LAPACK_API_VERSION STREQUAL "3.5.0")
	SET(_LAPACK_FILE_SUFFIX 3_5_0) #
ELSEIF (LAPACK_API_VERSION STREQUAL "3.4.2")
	SET(_LAPACK_FILE_SUFFIX 3_4_2) #
ELSEIF (LAPACK_API_VERSION STREQUAL "3.4.1")
	SET(_LAPACK_FILE_SUFFIX 3_4_1) #
ELSEIF (LAPACK_API_VERSION STREQUAL "3.4.0")
	SET(_LAPACK_FILE_SUFFIX 3_4_0) #
ELSEIF (LAPACK_API_VERSION STREQUAL "3.3.1")
	SET(_LAPACK_FILE_SUFFIX 3_3_1) #
ELSEIF (LAPACK_API_VERSION STREQUAL "3.3.0")
	SET(_LAPACK_FILE_SUFFIX 3_3_0) #
ENDIF()

SET (SRC
    dummy_${_LAPACK_FILE_SUFFIX}.c
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

if ( SYS_LAPACK_LIBRARY )
    message(STATUS "Use system provided LAPACK for fallback generation. SYS_LAPACK_LIBRARY = ${SYS_LAPACK_LIBRARY}")
    add_library(flexiblas_lapack_import STATIC IMPORTED)
    set_property(TARGET flexiblas_lapack_import PROPERTY IMPORTED_LOCATION ${SYS_LAPACK_LIBRARY})
    add_library(flexiblas_fallback_lapack SHARED ${SRC})
    target_link_libraries(flexiblas_fallback_lapack PUBLIC flexiblas_lapack_import)
else()
    add_library(flexiblas_fallback_lapack SHARED ${SRC} $<TARGET_OBJECTS:flexiblas_fallback_lapack_objs>)
endif()

if (APPLE)
   set_target_properties(flexiblas_fallback_lapack PROPERTIES LINK_FLAGS "-undefined dynamic_lookup ")
endif()

target_include_directories(flexiblas_fallback_lapack PRIVATE ${PROJECT_BINARY_DIR}/include)

install(TARGETS flexiblas_fallback_lapack LIBRARY DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR}/${FLEXIBLAS_LIBRARY_DIR})
SET_TARGET_PROPERTIES(flexiblas_fallback_lapack  PROPERTIES LIBRARY_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/lib")

