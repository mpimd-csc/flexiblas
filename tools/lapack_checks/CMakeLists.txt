INCLUDE_DIRECTORIES(${INCLUDE_DIR})
ADD_EXECUTABLE(lapack_check lapack-check.c) 
TARGET_LINK_LIBRARIES(lapack_check dl cscutils) 

