macro(add_blas_test name src)
	get_filename_component(baseNAME ${src} NAME_WE)
	set(TEST_INPUT "${CMAKE_CURRENT_SOURCE_DIR}/${baseNAME}.in")
    set(TEST_OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/${baseNAME}.out")


	MESSAGE(STATUS "${TEST_INPUT}")
	add_executable(${name} ${src})
	target_link_libraries(${name} flexiblas)
	if(EXISTS "${TEST_INPUT}")
		add_test(NAME BLAS-${name} COMMAND "${CMAKE_COMMAND}"
			-DTEST=$<TARGET_FILE:${name}>
			-DINPUT=${TEST_INPUT}
            -DOUTPUT=${TEST_OUTPUT}
			-DINTDIR=${CMAKE_CFG_INTDIR}
            -DPROJECT_BINARY_DIR=${PROJECT_BINARY_DIR}
			-P "${CMAKE_CURRENT_SOURCE_DIR}/runtest.cmake")
	else()
		add_test(NAME BLAS-${name} COMMAND "${CMAKE_COMMAND}"
			-DTEST=$<TARGET_FILE:${name}>
			-DINTDIR=${CMAKE_CFG_INTDIR}
            -DPROJECT_BINARY_DIR=${PROJECT_BINARY_DIR}
			-P "${CMAKE_CURRENT_SOURCE_DIR}/runtest.cmake")
	endif()

    if ( INTEGER8 )
        set_property(TEST BLAS-${name} APPEND PROPERTY ENVIRONMENT  FLEXIBLAS64_LIBRARY_PATH="${PROJECT_BINARY_DIR}/lib")
        set_property(TEST BLAS-${name} APPEND PROPERTY ENVIRONMENT  FLEXIBLAS64RC_DIR="${PROJECT_BINARY_DIR}/flexiblas64rc.d")
        set_property(TEST BLAS-${name} APPEND PROPERTY ENVIRONMENT  FLEXIBLAS64=NETLIB)
        set_property(TEST BLAS-${name} APPEND PROPERTY ENVIRONMENT  FLEXIBLAS64_CONFIG=${PROJECT_BINARY_DIR}/${FLEXIBLAS64_RC})
    else()
        set_property(TEST BLAS-${name} APPEND PROPERTY ENVIRONMENT  FLEXIBLAS_LIBRARY_PATH="${PROJECT_BINARY_DIR}/lib")
        set_property(TEST BLAS-${name} APPEND PROPERTY ENVIRONMENT  FLEXIBLASRC_DIR="${PROJECT_BINARY_DIR}/flexiblasrc.d")
        set_property(TEST BLAS-${name} APPEND PROPERTY ENVIRONMENT  FLEXIBLAS=NETLIB)
        set_property(TEST BLAS-${name} APPEND PROPERTY ENVIRONMENT  FLEXIBLAS_CONFIG=${PROJECT_BINARY_DIR}/${FLEXIBLAS_RC})
    endif()
    if (WIN32)
	    add_custom_command(TARGET ${name} POST_BUILD
 		 COMMAND ${CMAKE_COMMAND} -E copy_if_different $<TARGET_RUNTIME_DLLS:${name}> $<TARGET_FILE_DIR:${name}>
		 COMMAND_EXPAND_LISTS
  		)
	    #	    set(LISTE $<TARGET_RUNTIME_DLL_DIRS:${name}>)
	    #foreach(DIR IN LISTS LISTE)
	    #	    message(STATUS "DIR: ${DIR}")
	    #endforeach()
	    # set_property(TEST BLAS-${name} PROPERTY ENVIRONMENT_MODIFICATION PATH=path_list_append:$<TARGET_RUNTIME_DLL_DIRS:${name}>)
    endif()

endmacro(add_blas_test)

IF ( INTEGER8 STREQUAL ON )
	IF ( ${CMAKE_C_COMPILER_ID} STREQUAL "GNU")
		MESSAGE(STATUS "Setting GNU Compiler Flags")
		SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fdefault-integer-8 -frecursive")
	ELSEIF (${CMAKE_C_COMPILER_ID} STREQUAL "Intel" )
		SET ( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -i8 -recursive")
	ENDIF()
ENDIF (INTEGER8 STREQUAL ON)

if( (CMAKE_Fortran_COMPILER_ID STREQUAL "VisualAge" ) OR  # CMake 2.6
	(CMAKE_Fortran_COMPILER_ID STREQUAL "XL" ) )          # CMake 2.8
	if( NOT ("${CMAKE_Fortran_FLAGS}" MATCHES "-qfixed") )
		message( STATUS "BLAS -- Enabling fixed format F90/F95 with -qfixed" )
		set( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -qfixed")
	endif()
endif()

if ( CMAKE_Fortran_COMPILER_ID STREQUAL "GNU" )
	SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wno-conversion -Wno-maybe-uninitialized -Wno-unused-dummy-argument -Wno-unused-variable -Wno-intrinsic-shadow -Wno-surprising")
endif()
if ( CMAKE_C_COMPILER_ID STREQUAL "GNU")
	SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wno-unused-variable -Wno-unused-parameter")
endif()


add_blas_test(xblat1s sblat1.f)
add_blas_test(xblat2s sblat2.f)
add_blas_test(xblat3s sblat3.f)

add_blas_test(xblat1d dblat1.f)
add_blas_test(xblat2d dblat2.f)
add_blas_test(xblat3d dblat3.f)

add_blas_test(xblat1c cblat1.f)
add_blas_test(xblat2c cblat2.f)
add_blas_test(xblat3c cblat3.f)

add_blas_test(xblat1z zblat1.f)
add_blas_test(xblat2z zblat2.f)
add_blas_test(xblat3z zblat3.f)

if (CBLAS STREQUAL ON)
	add_subdirectory(cblas)
endif()
