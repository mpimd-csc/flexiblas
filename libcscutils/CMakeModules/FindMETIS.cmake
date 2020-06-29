# Find the METIS includes and library
#
# This module defines
#  METIS_INCLUDE_DIR        -    where to find metis.h
#  METIS_LIBRARIES          -    libraries to link against to use METIS.
#  METIS_FOUND              -    METIS library was found

INCLUDE(FindPackageHandleStandardArgs)
INCLUDE(CMakeHelpers)
GET_INC_LIB_DIR(_INCDIR _LIBDIR)


FIND_PATH(METIS_INCLUDE_DIR
    NAMES
    metis.h
    PATHS
    ${SUITESPARSE}/metis-4.0
    ${SUITESPARSE}/metis
    ${SUITESPARSE}
    /usr/
    /usr/local/
    /opt/
    /opt/local/
    ${_INCDIR}
    PATH_SUFFIXES
    include
    include/suitesparse
    )


FIND_LIBRARY(METIS_LIBRARIES
    NAMES
    libmetis metis
    PATHS
    ${SUITESPARSE}/metis-4.0
    ${SUITESPARSE}/metis
    ${SUITESPARSE}
    /usr/
    /usr/local/
    /opt/
    /opt/local/
    ${_LIBDIR}
    ${METIS_PATH}
    PATH_SUFFIXES
    lib
    lib64
    )


FIND_PACKAGE_HANDLE_STANDARD_ARGS(METIS DEFAULT_MSG METIS_INCLUDE_DIR METIS_LIBRARIES)
MARK_AS_ADVANCED(METIS_LIBRARIES METIS_INCLUDE_DIR)
