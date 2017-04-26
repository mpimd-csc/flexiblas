macro(add_blas_test name src)
  get_filename_component(baseNAME ${src} NAME_WE)
  set(TEST_INPUT "${CMAKE_CURRENT_SOURCE_DIR}/${baseNAME}.in")

  MESSAGE(STATUS "${TEST_INPUT}")
  add_executable(${name} ${src})
  get_target_property(TEST_LOC ${name} LOCATION)
  target_link_libraries(${name} flexiblas)
  if(EXISTS "${TEST_INPUT}")
    add_test(BLAS-${name} "${CMAKE_COMMAND}"
      -DTEST=${TEST_LOC}
      -DINPUT=${TEST_INPUT}
      -P "${CMAKE_CURRENT_SOURCE_DIR}/runtest.cmake")
    else()
      add_test(BLAS-${name} "${CMAKE_COMMAND}" 
        -DTEST=${TEST_LOC}
	-P "${CMAKE_CURRENT_SOURCE_DIR}/runtest.cmake")
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
