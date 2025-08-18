message(STATUS "LAPACKE enable")
enable_language(C)

set(LAPACK_INSTALL_EXPORT_NAME lapacke-targets)

if (WIN32 AND NOT UNIX)
 ADD_DEFINITIONS(-DHAVE_LAPACK_CONFIG_H -DLAPACK_COMPLEX_STRUCTURE)
 MESSAGE (STATUS "Windows BUILD")
endif (WIN32 AND NOT UNIX)

get_directory_property( DirDefs COMPILE_DEFINITIONS )

include_directories( include )
add_subdirectory(src)
add_subdirectory(utils)

macro(append_subdir_files variable dirname)
get_directory_property(holder DIRECTORY ${dirname} DEFINITION ${variable})
foreach(depfile ${holder})
  list(APPEND ${variable} "${dirname}/${depfile}")
endforeach()
endmacro()

append_subdir_files(LAPACKE_INCLUDE "include")
append_subdir_files(SRC_OBJ "src")
append_subdir_files(SRCX_OBJ "src")
append_subdir_files(MATGEN_OBJ "src")
append_subdir_files(UTILS_OBJ "utils")


add_library(lapacke OBJECT ${SRC_OBJ} ${UTILS_OBJ})
set_target_properties(
  lapacke PROPERTIES
  LINKER_LANGUAGE C
  VERSION ${LAPACK_VERSION}
  SOVERSION ${LAPACK_MAJOR_VERSION}
  )

target_include_directories(${LAPACKELIB} PUBLIC
    $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/include>
    $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/include>
    $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/src>
    $<INSTALL_INTERFACE:include>
)



