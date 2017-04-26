SET (SRC  	demo_1
		demo_2
		benchmark_dgemm
		benchmark_daxpy
		benchmark_dgemv
		test_zdotc_gnu
		test_zdotc_intel
		demo_xerbla
			)
IF (EXTBLAS) 
	SET (SRC ${SRC} demo_axpby
			demo_omatcopy
			demo_imatcopy
		) 
ENDIF()
INCLUDE_DIRECTORIES(${INCLUDE_DIR})
foreach (srcfile ${SRC})
	add_executable (${srcfile} ${srcfile}.c)
	target_link_libraries (${srcfile} ${LIBS} flexiblas ${LIBS})	
endforeach (srcfile)
			
add_executable(test_zdotc_fortran test_zdotc_fortran.f90) 
target_link_libraries(test_zdotc_fortran ${LIBS} flexiblas ${LIBS}) 
