macro(add_blas_test name src)
  get_filename_component(baseNAME ${name} NAME_WE)
  set(TEST_INPUT "${CMAKE_CURRENT_SOURCE_DIR}/${baseNAME}.in")

  MESSAGE(STATUS "${TEST_INPUT}")
  add_executable(${name} ${src} ${ARGN})
  get_target_property(TEST_LOC ${name} LOCATION)
  INCLUDE_DIRECTORIES(${CMAKE_SOURCE_DIR}/include ${CMAKE_BINARY_DIR}/include)
  target_link_libraries(${name} flexiblas)
  
  if(EXISTS "${TEST_INPUT}")
    add_test(BLAS-${name} "${CMAKE_COMMAND}"
      -DTEST=${TEST_LOC}
      -DINPUT=${TEST_INPUT}
      -P "${CMAKE_CURRENT_SOURCE_DIR}/runtest.cmake")
    else()
      add_test(BLAS-${name} "${CMAKE_COMMAND}" 
        -DTEST=${TEST_LOC}
	-P "${CMAKE_CURRENT_SOURCE_DIR}/runtest.cmake")
    endif()
endmacro(add_blas_test)

##add_blas_test(xblat1s sblat1.f)
#add_blas_test(xblat2s sblat2.f)
#add_blas_test(xblat3s sblat3.f)
#
#add_blas_test(xblat1d dblat1.f)
#add_blas_test(xblat2d dblat2.f)
#add_blas_test(xblat3d dblat3.f)
#
#add_blas_test(xblat1c cblat1.f)
#add_blas_test(xblat2c cblat2.f)
#add_blas_test(xblat3c cblat3.f)
#
#add_blas_test(xblat1z zblat1.f)
#add_blas_test(xblat2z zblat2.f)
#add_blas_test(xblat3z zblat3.f)

add_blas_test(xscblas1 c_sblas1.c c_sblat1.f)
add_blas_test(xdcblas1 c_dblas1.c c_dblat1.f)
add_blas_test(xccblas1 c_cblas1.c c_cblat1.f)
add_blas_test(xzcblas1 c_zblas1.c c_zblat1.f)

add_blas_test(xccblas2 c_cblas2.c c_cblat2.f c_c2chke.c auxiliary.c c_xerbla.c)
add_blas_test(xdcblas2 c_dblas2.c c_dblat2.f c_d2chke.c auxiliary.c c_xerbla.c)
add_blas_test(xscblas2 c_sblas2.c c_sblat2.f c_s2chke.c auxiliary.c c_xerbla.c)
add_blas_test(xzcblas2 c_zblas2.c c_zblat2.f c_z2chke.c auxiliary.c c_xerbla.c)

add_blas_test(xccblas3 c_cblas3.c c_cblat3.f c_c3chke.c auxiliary.c c_xerbla.c)
add_blas_test(xdcblas3 c_dblas3.c c_dblat3.f c_d3chke.c auxiliary.c c_xerbla.c)
add_blas_test(xscblas3 c_sblas3.c c_sblat3.f c_s3chke.c auxiliary.c c_xerbla.c)
add_blas_test(xzcblas3 c_zblas3.c c_zblat3.f c_z3chke.c auxiliary.c c_xerbla.c)
