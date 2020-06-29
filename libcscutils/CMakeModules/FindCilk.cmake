# - Finds OpenMP support
# This module can be used to detect OpenMP support in a compiler.
# If the compiler supports OpenMP, the flags required to compile with
# openmp support are set.
#
# The following variables are set:
#   OpenMP_C_FLAGS - flags to add to the C compiler for OpenMP support
#   OpenMP_CXX_FLAGS - flags to add to the CXX compiler for OpenMP support
#   OpenMP_LD_FLAGS - flags for the linker
#   OPENMP_FOUND - true if openmp is detected
#
# Supported compilers can be found at http://openmp.org/wp/openmp-compilers/

#=============================================================================
# Copyright 2009 Kitware, Inc.
# Copyright 2008-2009 André Rigland Brodtkorb <Andre.Brodtkorb@ifi.uio.no>
#
# Distributed under the OSI-approved BSD License (the "License");
# see accompanying file Copyright.txt for details.
#
# This software is distributed WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the License for more information.
#=============================================================================
# (To distribute this file outside of CMake, substitute the full
#  License text for the above reference.)

INCLUDE(checkCSourceCompiles)
INCLUDE(CheckCXXSourceCompiles)
INCLUDE(FindPackageHandleStandardArgs)

SET(CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES} cilkrts)
# sample openmp source code to test
SET(CILK_C_TEST_SOURCE
    "
    #include <cilk/cilk.h>
    int f() {
    return 0;
    }
    int main() {
    cilk_spawn f();
    cilk_sync;
    return 0;
    }
    "
    )
# use the same source for CXX as C for now
SET(CILK_CXX_TEST_SOURCE ${CILK_C_TEST_SOURCE})

CHECK_C_SOURCE_COMPILES("${CILK_C_TEST_SOURCE}" CILK_C_DETECTED)

CHECK_CXX_SOURCE_COMPILES("${CILK_CXX_TEST_SOURCE}" CILK_CXX_DETECTED)

FIND_PACKAGE_HANDLE_STANDARD_ARGS(CILK DEFAULT_MSG CILK_CXX_DETECTED CILK_C_DETECTED)

MARK_AS_ADVANCED(CILK_C_DETECTED CILK_CXX_DETECTED)




