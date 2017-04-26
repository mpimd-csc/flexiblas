SET (COMMON_SRC 
	common/error_message.c
	common/backtrace.c
	common/counter.c
	common/string.c
	common/table.c
	common/table_comment.c
	common/table_latex.c
	) 

IF(IO_ENABLE) 
	SET (IO_SRC 
		file/compress_handler.c
		file/compress_io_bzip.c
		file/compress_io_gzip.c
		file/compress_io_uncompressed.c
		file/compress_io_xz.c 
		file/io.c 
		file/dir.c
		)
ENDIF()

IF(INIFILE_ENABLE)
	SET (INI_SRC
		ini/inifile.c)
ENDIF()

IF(THREADING_ENABLE) 
	SET (THREAD_SRC
		threading/worker_queue.c 
		threading/bgthread.c) 
ENDIF()

IF (HDF5_ENABLE) 
	SET(HDF5_SRC 
		hdf5/common.c
		hdf5/vector.c
		hdf5/matrix.c
		hdf5/datatypes.c
		hdf5/sparse_matrix.c
	) 
ENDIF()

IF ( IMAGE_ENABLE) 
	SET (IMAGE_SRC 
		image/bmp.c)
ENDIF()

IF ( HARDWARE_ENABLE) 
	SET (HARDWARE_SRC 
		hardware/meminfo.c)
ENDIF()

IF ( DS_ENABLE ) 
	SET (DS_SRC 
		ds/ds.c
		ds/hashtable.c 
		ds/slist.c
		)
ENDIF()

SET(SRC ${COMMON_SRC} ${FILE_SRC} ${IO_SRC} ${INI_SRC} ${THREAD_SRC} ${HDF5_SRC} ${IMAGE_SRC} ${HARDWARE_SRC} ${DS_SRC}) 

INCLUDE_DIRECTORIES(${INCLUDE_DIR})
INCLUDE_DIRECTORIES(${PROJECT_BINARY_DIR}/include)
INCLUDE_DIRECTORIES(${PROJECT_SOURCE_DIR}/src)
INCLUDE_DIRECTORIES(${PROJECT_SOURCE_DIR}/include)


add_library (cscutils ${SRC})

TARGET_LINK_LIBRARIES(cscutils  ${LIBS})
set(GENERIC_LIB_VERSION "1.0")
set(GENERIC_LIB_SOVERSION "1")
set_target_properties(cscutils PROPERTIES VERSION ${GENERIC_LIB_VERSION} SOVERSION ${GENERIC_LIB_SOVERSION} OUTPUT_NAME "cscutils")


IF ( NOT INPROJECT) 
	install(TARGETS cscutils 
		RUNTIME DESTINATION bin COMPONENT bin
		LIBRARY DESTINATION lib COMPONENT shlib
		ARCHIVE DESTINATION lib COMPONENT stlib
		PUBLIC_HEADER DESTINATION include COMPONENT dev)

	install(DIRECTORY ${PROJECT_SOURCE_DIR}/include/cscutils/ DESTINATION include/cscutils 
		FILES_MATCHING PATTERN "*.h")
ENDIF()
