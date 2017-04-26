SET (SRC  	demo_1
		demo_2
		benchmark_dgemm
		benchmark_daxpy
		benchmark_dgemv
	
			)
INCLUDE_DIRECTORIES(${INCLUDE_DIR})
foreach (srcfile ${SRC})
	add_executable (${srcfile} ${srcfile}.c)
	target_link_libraries (${srcfile} ${LIBS} flexiblas ${LIBS})	
endforeach (srcfile)
			

