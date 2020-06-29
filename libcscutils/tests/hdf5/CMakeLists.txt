SET( TEST_SRC
    sparse_read_write
)

INCLUDE_DIRECTORIES(${HDF5_INCLUDE_DIRS})

INCLUDE_DIRECTORIES(${PROJECT_BINARY_DIR}/include)
INCLUDE_DIRECTORIES(${PROJECT_SOURCE_DIR}/include)
FOREACH(srcfile ${TEST_SRC})
    ADD_EXECUTABLE(${srcfile} ${srcfile}.c)
    TARGET_LINK_LIBRARIES(${srcfile} cscutils ${LIBS})
ENDFOREACH()


ADD_TEST(NAME hdf5_sparse_read_write        COMMAND ${CMAKE_CURRENT_BINARY_DIR}/sparse_read_write 0 0 )
ADD_TEST(NAME hdf5_sparse_read_write_matlab COMMAND ${CMAKE_CURRENT_BINARY_DIR}/sparse_read_write 1 0 )
ADD_TEST(NAME hdf5_sparse_read_write_octave COMMAND ${CMAKE_CURRENT_BINARY_DIR}/sparse_read_write 0 1 )

