
IF ( CMAKE_Fortran_COMPILER_ID STREQUAL "GNU") 
	# GNU
	SET(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -Wall -Wunused -Wimplicit-procedure") 
	SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -frecursive -fPIC") 

	IF (INTEGER8 STREQUAL ON ) 
		SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fdefault-integer-8") 
	ENDIF()
	IF(HOSTOPT STREQUAL ON ) 
		SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -march=native -mtune=native") 

	ENDIF()

	LIST(APPEND LIBRARIES "gfortran") 

ELSEIF ( CMAKE_Fortran_COMPILER_ID STREQUAL "Intel") 
	# Intel 
	SET(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -warn -g") 
	SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -recursive -fpic -fPIC") 
	IF ( HOSTOPT STREQUAL ON) 
		SET(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -O3 -xHost ") 
		SET(CMAKE_Fortran_FLAGS_DEBUG   "${CMAKE_Fortran_FLAGS_DEBUG} -xHost") 
	ENDIF()

	IF (INTEGER8 STREQUAL ON ) 
		SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -i8") 
	ENDIF()
	LIST(APPEND LIBRARIES "ifcore") 


ELSEIF ( CMAKE_Fortran_COMPILER_ID STREQUAL "PGI" ) 
	# PGI 
	SET(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -O4 ") 
	SET(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -g -Minfo=all") 
	SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fPIC -fpic")
	IF (INTEGER8 STREQUAL ON ) 
		SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -i8") 
	ENDIF()

ELSEIF ( CMAKE_Fortran_COMPILER_ID STREQUAL "XL")
	# IBM XL 
	IF (HOSTOPT STREQUAL ON ) 
		SET (CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -O5 -qessl -qtune=auto -qarch=auto")
	ELSE()
		SET (CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -O3 -qessl ")
	ENDIF()
	SET(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -qstrict=ieeefp -qmaxmem=524288")
	SET(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_RELEASE} -qstrict=ieeefp")
	SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -qalias=aryovrlp:pteovrlp:std:intptr -qpic ") 

	IF (INTEGER8 STREQUAL ON ) 
		SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -qintsize=8") 
	ENDIF()

	IF( OPENMP_FOUND) 
		LIST(REMOVE_ITEM CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES "xlomp_ser")
	ENDIF()
ENDIF()

