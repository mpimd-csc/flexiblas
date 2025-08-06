cmake_minimum_required(VERSION 3.13)

project(LAPACK C)

set(LAPACK_MAJOR_VERSION 3)
set(LAPACK_MINOR_VERSION 12)
set(LAPACK_PATCH_VERSION 1)
set(
  LAPACK_VERSION
  ${LAPACK_MAJOR_VERSION}.${LAPACK_MINOR_VERSION}.${LAPACK_PATCH_VERSION}
  )

# Allow setting a prefix for the library names
set(CMAKE_STATIC_LIBRARY_PREFIX "lib${LIBRARY_PREFIX}")
set(CMAKE_SHARED_LIBRARY_PREFIX "lib${LIBRARY_PREFIX}")

# Add the CMake directory for custom CMake modules
set(CMAKE_MODULE_PATH "${LAPACK_SOURCE_DIR}/CMAKE" ${CMAKE_MODULE_PATH})

# Export all symbols on Windows when building shared libraries
SET(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS TRUE)

# Set a default build type if none was specified
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
  # Set the possible values of build type for cmake-gui
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo" "Coverage")
endif()

# Coverage
set(_is_coverage_build 0)
set(_msg "Checking if build type is 'Coverage'")
message(STATUS "${_msg}")
if(NOT CMAKE_CONFIGURATION_TYPES)
  string(TOLOWER ${CMAKE_BUILD_TYPE} _build_type_lc)
  if(${_build_type_lc} STREQUAL "coverage")
    set(_is_coverage_build 1)
  endif()
endif()
message(STATUS "${_msg}: ${_is_coverage_build}")

if(_is_coverage_build)
  message(STATUS "Adding coverage")
  find_package(codecov)
endif()

# By default static library
option(BUILD_SHARED_LIBS "Build shared libraries" OFF)

# By default build index32 library
option(BUILD_INDEX64 "Build Index-64 API libraries" OFF)
if(BUILD_INDEX64)
  set(BLASLIB "blas64")
  set(CBLASLIB "cblas64")
  set(LAPACKLIB "lapack64")
  set(LAPACKELIB "lapacke64")
  set(TMGLIB "tmglib64")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DWeirdNEC -DLAPACK_ILP64 -DHAVE_LAPACK_CONFIG_H")
  set(FORTRAN_ILP TRUE)
else()
  set(BLASLIB "blas")
  set(CBLASLIB "cblas")
  set(LAPACKLIB "lapack")
  set(LAPACKELIB "lapacke")
  set(TMGLIB "tmglib")
endif()

set(BUILD_INDEX64_EXT_API OFF)

include(PreventInSourceBuilds)
include(PreventInBuildInstalls)

# Add option to enable flat namespace for symbol resolution on macOS
if(APPLE)
  option(USE_FLAT_NAMESPACE "Use flat namespaces for symbol resolution during build and runtime." OFF)

  if(USE_FLAT_NAMESPACE)
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,-flat_namespace")
    set(CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} -Wl,-flat_namespace")
    set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -Wl,-flat_namespace")
  else()
    if(BUILD_SHARED_LIBS AND BUILD_TESTING)
      message(WARNING
        "LAPACK test suite might fail with shared libraries and the default two-level namespace. "
        "Disable shared libraries or enable flat namespace for symbol resolution via -DUSE_FLAT_NAMESPACE=ON.")
    endif()
  endif()
endif()

# --------------------------------------------------
set(LAPACK_INSTALL_EXPORT_NAME ${LAPACKLIB}-targets)

set(LAPACK_BINARY_PATH_SUFFIX "" CACHE STRING "Path suffix appended to the install path of binaries")

if(NOT "${LAPACK_BINARY_PATH_SUFFIX}" STREQUAL "" AND NOT "${LAPACK_BINARY_PATH_SUFFIX}" MATCHES "^/")
  set(LAPACK_BINARY_PATH_SUFFIX "/${LAPACK_BINARY_PATH_SUFFIX}")
endif()

# --------------------------------------------------
# Organize output files.  On Windows this also keeps .dll files next
# to the .exe files that need them, making tests easy to run.
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${LAPACK_BINARY_DIR}/bin)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${LAPACK_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${LAPACK_BINARY_DIR}/lib)

# deprecated LAPACK and LAPACKE routines
option(BUILD_DEPRECATED "Build deprecated routines" ON)
message(STATUS "Build deprecated routines: ${BUILD_DEPRECATED}")

# --------------------------------------------------
# Precision to build
# By default all precisions are generated
option(BUILD_SINGLE "Build single precision real" ON)
option(BUILD_DOUBLE "Build double precision real" ON)
option(BUILD_COMPLEX "Build single precision complex" ON)
option(BUILD_COMPLEX16 "Build double precision complex" ON)
message(STATUS "Build single precision real: ${BUILD_SINGLE}")
message(STATUS "Build double precision real: ${BUILD_DOUBLE}")
message(STATUS "Build single precision complex: ${BUILD_COMPLEX}")
message(STATUS "Build double precision complex: ${BUILD_COMPLEX16}")

if(NOT (BUILD_SINGLE OR BUILD_DOUBLE OR BUILD_COMPLEX OR BUILD_COMPLEX16))
  message(FATAL_ERROR "Nothing to build, no precision selected.
  Please enable at least one of these:
  BUILD_SINGLE, BUILD_DOUBLE, BUILD_COMPLEX, BUILD_COMPLEX16.")
endif()


enable_language(Fortran)

# Check for any necessary platform specific compiler flags
include(CheckLAPACKCompilerFlags)
CheckLAPACKCompilerFlags()

# Check second function
include(CheckTimeFunction)
set(TIME_FUNC NONE)
CHECK_TIME_FUNCTION(NONE TIME_FUNC)
CHECK_TIME_FUNCTION(INT_CPU_TIME TIME_FUNC)
CHECK_TIME_FUNCTION(EXT_ETIME TIME_FUNC)
CHECK_TIME_FUNCTION(EXT_ETIME_ TIME_FUNC)
CHECK_TIME_FUNCTION(INT_ETIME TIME_FUNC)

# Set second function
message(STATUS "--> Will use second_${TIME_FUNC}.f and dsecnd_${TIME_FUNC}.f as timing function.")
set(SECOND_SRC ${LAPACK_SOURCE_DIR}/INSTALL/second_${TIME_FUNC}.f)
set(DSECOND_SRC ${LAPACK_SOURCE_DIR}/INSTALL/dsecnd_${TIME_FUNC}.f)

add_subdirectory(SRC)
