SET(LAPACK_TEST_VERSION "3.12.0")

if(MSVC_VERSION)
    #  string(REPLACE "/STACK:10000000" "/STACK:900000000000000000"
    #    CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS}")
    string(REGEX REPLACE "(.*)/STACK:(.*) (.*)" "\\1/STACK:900000000000000000 \\3"
        CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS}")
endif()

if( (CMAKE_Fortran_COMPILER_ID STREQUAL "VisualAge" ) OR  # CMake 2.6
    (CMAKE_Fortran_COMPILER_ID STREQUAL "XL" ) )          # CMake 2.8
    if( NOT ("${CMAKE_Fortran_FLAGS}" MATCHES "-qfixed") )
        message( STATUS "BLAS -- Enabling fixed format F90/F95 with -qfixed" )
        set( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -qfixed")
    endif()
endif()

if ( CMAKE_Fortran_COMPILER_ID STREQUAL "GNU" )
    SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wno-conversion -Wno-maybe-uninitialized -Wno-unused-dummy-argument -Wno-unused-variable -Wno-intrinsic-shadow -Wno-surprising -Wno-tabs")
endif()




add_subdirectory(MATGEN)
add_subdirectory(LIN)
add_subdirectory(EIG)

# Only run this test if python 3 is found
if(PYTHON_EXECUTABLE)
    message(STATUS "Running Summary")
    file(COPY ${PROJECT_SOURCE_DIR}/test/lapack-${LAPACK_TEST_VERSION}/lapack_testing.py DESTINATION ${PROJECT_BINARY_DIR}/test/lapack-${LAPACK_TEST_VERSION}/)
    add_test(
        NAME LAPACK_Test_Summary
        WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/test/lapack-${LAPACK_TEST_VERSION}"
        COMMAND ${PYTHON_EXECUTABLE} "lapack_testing.py")
endif()


macro(add_lapack_test output input target)
    set(TEST_INPUT  "${PROJECT_SOURCE_DIR}/test/lapack-${LAPACK_TEST_VERSION}/${input}")
    set(TEST_OUTPUT "${PROJECT_BINARY_DIR}/test/lapack-${LAPACK_TEST_VERSION}/${output}")
    string(REPLACE "." "_" input_name ${input})
    set(testName "${target}_${input_name}")
    if(EXISTS "${TEST_INPUT}")
        add_test(NAME LAPACK-${testName} COMMAND "${CMAKE_COMMAND}"
            -DTEST=$<TARGET_FILE:${target}>
            -DINPUT=${TEST_INPUT}
            -DOUTPUT=${TEST_OUTPUT}
            -DINTDIR=${CMAKE_CFG_INTDIR}
            -P "${PROJECT_SOURCE_DIR}/test/lapack-${LAPACK_TEST_VERSION}/runtest.cmake")

        if(PYTHON_EXECUTABLE)
            set_property(
                TEST LAPACK_Test_Summary
                APPEND PROPERTY DEPENDS LAPACK-${testName}
                )
        endif()


        if ( INTEGER8 )
            set_property(TEST LAPACK-${testName} APPEND PROPERTY ENVIRONMENT  FLEXIBLAS64_LIBRARY_PATH="${PROJECT_BINARY_DIR}/lib")
            set_property(TEST LAPACK-${testName} APPEND PROPERTY ENVIRONMENT  FLEXIBLAS64=NETLIB)
            set_property(TEST LAPACK-${testName} APPEND PROPERTY ENVIRONMENT  FLEXIBLAS64_CONFIG=${PROJECT_BINARY_DIR}/${FLEXIBLAS64_RC})
        else()
            set_property(TEST LAPACK-${testName} APPEND PROPERTY ENVIRONMENT  FLEXIBLAS_LIBRARY_PATH="${PROJECT_BINARY_DIR}/lib")
            set_property(TEST LAPACK-${testName} APPEND PROPERTY ENVIRONMENT  FLEXIBLAS=NETLIB)
            set_property(TEST LAPACK-${testName} APPEND PROPERTY ENVIRONMENT  FLEXIBLAS_CONFIG=${PROJECT_BINARY_DIR}/${FLEXIBLAS_RC})
        endif()


    endif()
endmacro()


add_lapack_test(stest.out stest.in xlintsts)
#
# ======== SINGLE RFP LIN TESTS ========================
add_lapack_test(stest_rfp.out stest_rfp.in xlintstrfs)
#
#
# ======== SINGLE EIG TESTS ===========================

add_lapack_test(snep.out nep.in xeigtsts)
add_lapack_test(ssep.out sep.in xeigtsts)
add_lapack_test(sse2.out se2.in xeigtsts)
add_lapack_test(ssvd.out svd.in xeigtsts)
add_lapack_test(sec.out sec.in xeigtsts)
add_lapack_test(sed.out sed.in xeigtsts)
add_lapack_test(sgg.out sgg.in xeigtsts)
add_lapack_test(sgd.out sgd.in xeigtsts)
add_lapack_test(ssb.out ssb.in xeigtsts)
add_lapack_test(ssg.out ssg.in xeigtsts)
add_lapack_test(sbal.out sbal.in xeigtsts)
add_lapack_test(sbak.out sbak.in xeigtsts)
add_lapack_test(sgbal.out sgbal.in xeigtsts)
add_lapack_test(sgbak.out sgbak.in xeigtsts)
add_lapack_test(sbb.out sbb.in xeigtsts)
add_lapack_test(sglm.out glm.in xeigtsts)
add_lapack_test(sgqr.out gqr.in xeigtsts)
add_lapack_test(sgsv.out gsv.in xeigtsts)
add_lapack_test(scsd.out csd.in xeigtsts)
add_lapack_test(slse.out lse.in xeigtsts)

#
# ======== SINGLE DMD EIG TESTS ===========================
add_lapack_test(sdmd.out sdmd.in xdmdeigtsts)

#
# ======== DOUBLE LIN TESTS ===========================
add_lapack_test(dtest.out dtest.in xlintstd)
#
# ======== DOUBLE RFP LIN TESTS ========================
add_lapack_test(dtest_rfp.out dtest_rfp.in xlintstrfd)
#
# ======== DOUBLE EIG TESTS ===========================
add_lapack_test(dnep.out nep.in xeigtstd)
add_lapack_test(dsep.out sep.in xeigtstd)
add_lapack_test(dse2.out se2.in xeigtstd)
add_lapack_test(dsvd.out svd.in xeigtstd)
add_lapack_test(dec.out dec.in xeigtstd)
add_lapack_test(ded.out ded.in xeigtstd)
add_lapack_test(dgg.out dgg.in xeigtstd)
add_lapack_test(dgd.out dgd.in xeigtstd)
add_lapack_test(dsb.out dsb.in xeigtstd)
add_lapack_test(dsg.out dsg.in xeigtstd)
add_lapack_test(dbal.out dbal.in xeigtstd)
add_lapack_test(dbak.out dbak.in xeigtstd)
add_lapack_test(dgbal.out dgbal.in xeigtstd)
add_lapack_test(dgbak.out dgbak.in xeigtstd)
add_lapack_test(dbb.out dbb.in xeigtstd)
add_lapack_test(dglm.out glm.in xeigtstd)
add_lapack_test(dgqr.out gqr.in xeigtstd)
add_lapack_test(dgsv.out gsv.in xeigtstd)
add_lapack_test(dcsd.out csd.in xeigtstd)
add_lapack_test(dlse.out lse.in xeigtstd)

#
# ======== DOUBLE DMD EIG TESTS ===========================
add_lapack_test(ddmd.out ddmd.in xdmdeigtstd)

add_lapack_test(ctest.out ctest.in xlintstc)
#
# ======== COMPLEX RFP LIN TESTS ========================
add_lapack_test(ctest_rfp.out ctest_rfp.in xlintstrfc)
#
# ======== COMPLEX EIG TESTS ===========================
add_lapack_test(cnep.out nep.in xeigtstc)
add_lapack_test(csep.out sep.in xeigtstc)
add_lapack_test(cse2.out se2.in xeigtstc)
add_lapack_test(csvd.out svd.in xeigtstc)
add_lapack_test(cec.out cec.in xeigtstc)
add_lapack_test(ced.out ced.in xeigtstc)
add_lapack_test(cgg.out cgg.in xeigtstc)
add_lapack_test(cgd.out cgd.in xeigtstc)
add_lapack_test(csb.out csb.in xeigtstc)
add_lapack_test(csg.out csg.in xeigtstc)
add_lapack_test(cbal.out cbal.in xeigtstc)
add_lapack_test(cbak.out cbak.in xeigtstc)
add_lapack_test(cgbal.out cgbal.in xeigtstc)
add_lapack_test(cgbak.out cgbak.in xeigtstc)
add_lapack_test(cbb.out cbb.in xeigtstc)
add_lapack_test(cglm.out glm.in xeigtstc)
add_lapack_test(cgqr.out gqr.in xeigtstc)
add_lapack_test(cgsv.out gsv.in xeigtstc)
add_lapack_test(ccsd.out csd.in xeigtstc)
add_lapack_test(clse.out lse.in xeigtstc)

#
# ======== COMPLEX DMD EIG TESTS ===========================
add_lapack_test(cdmd.out cdmd.in xdmdeigtstc)

#
# ======== COMPLEX16 LIN TESTS ========================
add_lapack_test(ztest.out ztest.in xlintstz)
#
# ======== COMPLEX16 RFP LIN TESTS ========================
add_lapack_test(ztest_rfp.out ztest_rfp.in xlintstrfz)
#
# ======== COMPLEX16 EIG TESTS ===========================
add_lapack_test(znep.out nep.in xeigtstz)
add_lapack_test(zsep.out sep.in xeigtstz)
add_lapack_test(zse2.out se2.in xeigtstz)
add_lapack_test(zsvd.out svd.in xeigtstz)
add_lapack_test(zec.out zec.in xeigtstz)
add_lapack_test(zed.out zed.in xeigtstz)
add_lapack_test(zgg.out zgg.in xeigtstz)
add_lapack_test(zgd.out zgd.in xeigtstz)
add_lapack_test(zsb.out zsb.in xeigtstz)
add_lapack_test(zsg.out zsg.in xeigtstz)
add_lapack_test(zbal.out zbal.in xeigtstz)
add_lapack_test(zbak.out zbak.in xeigtstz)
add_lapack_test(zgbal.out zgbal.in xeigtstz)
add_lapack_test(zgbak.out zgbak.in xeigtstz)
add_lapack_test(zbb.out zbb.in xeigtstz)
add_lapack_test(zglm.out glm.in xeigtstz)
add_lapack_test(zgqr.out gqr.in xeigtstz)
add_lapack_test(zgsv.out gsv.in xeigtstz)
add_lapack_test(zcsd.out csd.in xeigtstz)
add_lapack_test(zlse.out lse.in xeigtstz)

#
# ======== COMPLEX16 DMD EIG TESTS ===========================
add_lapack_test(zdmd.out zdmd.in xdmdeigtstz)


#
# ======== SINGLE-DOUBLE PROTO LIN TESTS ==============
add_lapack_test(dstest.out dstest.in xlintstds)


#
# ======== COMPLEX-COMPLEX16 LIN TESTS ========================
add_lapack_test(zctest.out zctest.in xlintstzc)




