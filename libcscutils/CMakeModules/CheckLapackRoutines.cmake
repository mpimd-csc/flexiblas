include(CheckFortranFunctionExists)

macro(CheckLapackRoutine ROUTINE VAR_TO_SET) 
	set (_X ${CMAKE_REQUIRED_LIBRARIES})
	set (CMAKE_REQUIRED_LIBRARIES ${LAPACK_LIBRARIES} ${BLAS_LIBRARIES}) 
	
	check_fortran_function_exists(${ROUTINE} ${VAR_TO_SET})
	set(CMAKE_REQUIRED_LIBRARIES ${_X})	
endmacro()

