FIND_PACKAGE(OpenMP)
SET(SRC)
IF ( OpenMP_FOUND)
    SET (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
    MESSAGE(STATUS "Enable Multithread Demo")
    SET( SRC  demo_multithread)
ENDIF()


SET (SRC ${SRC}  	demo_1
    demo_1s
    demo_2
    demo_api
    demo_set_threads
    benchmark
    test_zdotc_gnu
    test_zdotc_intel
    demo_xerbla
    demo_axpby
    demo_omatcopy
    demo_imatcopy
    )

IF ( LAPACK )
    SET (SRC ${SRC} benchmark_dgetrf)
ENDIF ()

IF (LAPACKE)
    SET (SRC ${SRC} demo_lapacke)
ENDIF()

IF (CMAKE_SYSTEM_PROCESSOR STREQUAL "x86_64")
    SET (SRC ${SRC} benchmark_overhead_quickreturn)
ENDIF()

IF ( CMAKE_C_COMPILER_ID STREQUAL "GNU")
    SET (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wno-unused-parameter")
ENDIF()


INCLUDE_DIRECTORIES(${CMAKE_SOURCE_DIR}/src ${CMAKE_SOURCE_DIR}/include ${CMAKE_SOURCE_DIR}/libcscutils/include ${PROJECT_BINARY_DIR}/include/ ${PROJECT_BINARY_DIR}/libcscutils/include)
foreach (srcfile ${SRC})
    add_executable (${srcfile} ${srcfile}.c)
    target_link_libraries (${srcfile} flexiblas cscutils ${LIBS})
endforeach (srcfile)

add_executable(test_zdotc_fortran test_zdotc_fortran.f90)
target_link_libraries(test_zdotc_fortran flexiblas cscutils ${LIBS})

add_executable(demo_api_standalone_all demo_api_standalone.c)
add_executable(demo_api_standalone_allinv demo_api_standalone.c)

add_executable(demo_api_standalone_fb demo_api_standalone.c)
add_executable(demo_api_standalone_api demo_api_standalone.c)

target_link_libraries(demo_api_standalone_all flexiblas flexiblas_api)
target_link_libraries(demo_api_standalone_allinv flexiblas_api flexiblas)
target_link_libraries(demo_api_standalone_fb  flexiblas )
target_link_libraries(demo_api_standalone_api flexiblas_api)

SET_TARGET_PROPERTIES (demo_api_standalone_allinv PROPERTIES COMPILE_FLAGS "-DLINK_BLAS")
SET_TARGET_PROPERTIES (demo_api_standalone_all PROPERTIES COMPILE_FLAGS "-DLINK_BLAS")
SET_TARGET_PROPERTIES (demo_api_standalone_fb PROPERTIES COMPILE_FLAGS "-DLINK_BLAS")




#
# Netlib Benchmark
#
add_executable (benchmark.NETLIB benchmark.c)
target_link_libraries (benchmark.NETLIB flexiblas_netlib cscutils ${LIBS})
SET_TARGET_PROPERTIES (benchmark.NETLIB PROPERTIES COMPILE_FLAGS "${EXTRA_FLAGS} -DSTANDALONE -DBACKEND=\\\"NETLIB\\\"")


IF (CMAKE_SYSTEM_PROCESSOR STREQUAL "x86_64")
    add_executable (benchmark_overhead_quickreturn.NETLIB benchmark_overhead_quickreturn.c)
    target_link_libraries (benchmark_overhead_quickreturn.NETLIB flexiblas_netlib cscutils ${LIBS})
    SET_TARGET_PROPERTIES (benchmark_overhead_quickreturn.NETLIB PROPERTIES COMPILE_FLAGS "${EXTRA_FLAGS} -DSTANDALONE -DBACKEND=\\\"NETLIB\\\"")
ENDIF()

MESSAGE(STATUS "Build Extra Benchmarks")
FOREACH(EBLAS ${EXTRA_BLAS})
    UNSET(BLAS_LIBS)
    UNSET(EXTRA_BLAS)
    UNSET(LINKER_FLAGS)
    SET(BLAS_LIBS ${${EBLAS}_BLAS_LIBRARIES})
    SET(EXTRA_FLAGS ${${EBLAS}_BLAS_EXTRA_FLAGS})
    SET(LINKER_FLAGS ${${EBLAS}_BLAS_LINKER_FLAGS})

    MESSAGE(STATUS "${EBLAS} - ${BLAS_LIBS} - ${LINKER_FLAGS}")
    add_executable (benchmark.${EBLAS} benchmark.c)

    target_link_libraries (benchmark.${EBLAS} ${BLAS_LIBS} cscutils ${LIBS})
    SET_TARGET_PROPERTIES (benchmark.${EBLAS} PROPERTIES COMPILE_FLAGS "${EXTRA_FLAGS} -DSTANDALONE -DBACKEND=\\\"${EBLAS}\\\"" LINK_FLAGS "${LINKER_FLAGS}")
    # MESSAGE(STATUS " - ${EBLAS}")


    IF (CMAKE_SYSTEM_PROCESSOR STREQUAL "x86_64")
        add_executable (benchmark_overhead_quickreturn.${EBLAS} benchmark_overhead_quickreturn.c)
        target_link_libraries (benchmark_overhead_quickreturn.${EBLAS} ${BLAS_LIBS} cscutils ${LIBS})
        SET_TARGET_PROPERTIES (benchmark_overhead_quickreturn.${EBLAS} PROPERTIES COMPILE_FLAGS "${EXTRA_FLAGS} -DSTANDALONE -DBACKEND=\\\"${EBLAS}\\\"" LINK_FLAGS "${LINKER_FLAGS}")
    ENDIF()

ENDFOREACH()

