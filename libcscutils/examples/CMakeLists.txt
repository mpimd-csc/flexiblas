SET (SRC  
	demo_inifile
	demo_io
	demo_table
	)
IF (HDF5_ENABLE) 
	INCLUDE_DIRECTORIES(${HDF5_INCLUDE_DIRS}) 
	SET ( SRC ${SRC} demo_hdf) 
ENDIF()
IF (HARDWARE_ENABLE) 
	SET (SRC ${SRC} demo_hardware) 
ENDIF()

IF (DS_ENABLE) 
	SET (SRC ${SRC} demo_hashtable
		demo_slist ) 

ENDIF()
IF(THREADING_ENABLE) 
	SET (SRC ${SRC}
		demo_background_worker) 
ENDIF()


INCLUDE_DIRECTORIES(${PROJECT_SOURCE_DIR}/include)
foreach (srcfile ${SRC})
	add_executable (${srcfile} ${srcfile}.c)
	target_link_libraries (${srcfile} cscutils ${LIBS})	
endforeach (srcfile)
			

