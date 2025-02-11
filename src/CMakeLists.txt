
CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/flexiblas_api.h.in ${CMAKE_BINARY_DIR}/include/flexiblas_api.h)


ADD_SUBDIRECTORY(fallback_blas)

IF(LAPACK)
    ADD_SUBDIRECTORY(fallback_lapack)
ENDIF()

SET (TOOL_SRC
    paths.c
    hooks.c
    utils.c
    sh_utils.c
    common.c
    )

SET (COMMON_SRC
    mgmt/hooks.c
    mgmt.c
    flexiblas_printf.c
    ${TOOL_SRC}
)

SET ( SRC
    flexiblas.c
    flexiblas_api.c
    loader.c
    hook_loader.c
    xerbla.c
    lsame.c
    set_num_threads.c
    wrap_cblas.c
    cabs.c
    ${COMMON_SRC}
    )

SET (FLEXIBLAS_MGMT_SRC
    ${COMMON_SRC}
    )

SET (FLEXIBLAS_API_SRC
    flexiblas_api_standalone.c
    )

IF ( FLEXIBLAS_ABI_INTEL)
    SET (SRC ${SRC} wrapper_blas_intel.c)
ELSE ()
    SET (SRC ${SRC} wrapper_blas_gnu.c)
ENDIF()


SET ( CBLAS_SRC
    # Globlas
    cblas_interface/cblas_globals.c
    cblas_interface/cblas_xerbla.c
    # Double
    cblas_interface/cblas_dasum.c
    cblas_interface/cblas_daxpy.c
    cblas_interface/cblas_dcopy.c
    cblas_interface/cblas_ddot.c
    cblas_interface/cblas_dgbmv.c
    cblas_interface/cblas_dgemm.c
    cblas_interface/cblas_dgemv.c
    cblas_interface/cblas_dger.c
    cblas_interface/cblas_dnrm2.c
    cblas_interface/cblas_drot.c
    cblas_interface/cblas_drotg.c
    cblas_interface/cblas_drotm.c
    cblas_interface/cblas_drotmg.c
    cblas_interface/cblas_dsbmv.c
    cblas_interface/cblas_dscal.c
    cblas_interface/cblas_dsdot.c
    cblas_interface/cblas_dspmv.c
    cblas_interface/cblas_dspr.c
    cblas_interface/cblas_dspr2.c
    cblas_interface/cblas_dswap.c
    cblas_interface/cblas_dsymm.c
    cblas_interface/cblas_dsymv.c
    cblas_interface/cblas_dsyr.c
    cblas_interface/cblas_dsyr2.c
    cblas_interface/cblas_dsyr2k.c
    cblas_interface/cblas_dsyrk.c
    cblas_interface/cblas_dtbmv.c
    cblas_interface/cblas_dtbsv.c
    cblas_interface/cblas_dtpmv.c
    cblas_interface/cblas_dtpsv.c
    cblas_interface/cblas_dtrmm.c
    cblas_interface/cblas_dtrmv.c
    cblas_interface/cblas_dtrsm.c
    cblas_interface/cblas_dtrsv.c
    cblas_interface/cblas_dzasum.c
    cblas_interface/cblas_dznrm2.c
    cblas_interface/cblas_idamax.c
    # Single
    cblas_interface/cblas_sasum.c
    cblas_interface/cblas_saxpy.c
    cblas_interface/cblas_scasum.c
    cblas_interface/cblas_scnrm2.c
    cblas_interface/cblas_scopy.c
    cblas_interface/cblas_sdot.c
    cblas_interface/cblas_sdsdot.c
    cblas_interface/cblas_sgbmv.c
    cblas_interface/cblas_sgemm.c
    cblas_interface/cblas_sgemv.c
    cblas_interface/cblas_sger.c
    cblas_interface/cblas_snrm2.c
    cblas_interface/cblas_srot.c
    cblas_interface/cblas_srotg.c
    cblas_interface/cblas_srotm.c
    cblas_interface/cblas_srotmg.c
    cblas_interface/cblas_ssbmv.c
    cblas_interface/cblas_sscal.c
    cblas_interface/cblas_sspmv.c
    cblas_interface/cblas_sspr2.c
    cblas_interface/cblas_sspr.c
    cblas_interface/cblas_sswap.c
    cblas_interface/cblas_ssymm.c
    cblas_interface/cblas_ssymv.c
    cblas_interface/cblas_ssyr2.c
    cblas_interface/cblas_ssyr2k.c
    cblas_interface/cblas_ssyr.c
    cblas_interface/cblas_ssyrk.c
    cblas_interface/cblas_stbmv.c
    cblas_interface/cblas_stbsv.c
    cblas_interface/cblas_stpmv.c
    cblas_interface/cblas_stpsv.c
    cblas_interface/cblas_strmm.c
    cblas_interface/cblas_strmv.c
    cblas_interface/cblas_strsm.c
    cblas_interface/cblas_strsv.c
    cblas_interface/cblas_isamax.c
    # Complex Single
    cblas_interface/cblas_caxpy.c
    cblas_interface/cblas_ccopy.c
    cblas_interface/cblas_cdotc_sub.c
    cblas_interface/cblas_cdotu_sub.c
    cblas_interface/cblas_cgbmv.c
    cblas_interface/cblas_cgemm.c
    cblas_interface/cblas_cgemv.c
    cblas_interface/cblas_cgerc.c
    cblas_interface/cblas_cgeru.c
    cblas_interface/cblas_chbmv.c
    cblas_interface/cblas_chemm.c
    cblas_interface/cblas_chemv.c
    cblas_interface/cblas_cher2.c
    cblas_interface/cblas_cher2k.c
    cblas_interface/cblas_cher.c
    cblas_interface/cblas_cherk.c
    cblas_interface/cblas_chpmv.c
    cblas_interface/cblas_chpr2.c
    cblas_interface/cblas_chpr.c
    cblas_interface/cblas_crotg.c
    cblas_interface/cblas_csrot.c
    cblas_interface/cblas_cscal.c
    cblas_interface/cblas_csscal.c
    cblas_interface/cblas_cswap.c
    cblas_interface/cblas_csymm.c
    cblas_interface/cblas_csyr2k.c
    cblas_interface/cblas_csyrk.c
    cblas_interface/cblas_ctbmv.c
    cblas_interface/cblas_ctbsv.c
    cblas_interface/cblas_ctpmv.c
    cblas_interface/cblas_ctpsv.c
    cblas_interface/cblas_ctrmm.c
    cblas_interface/cblas_ctrmv.c
    cblas_interface/cblas_ctrsm.c
    cblas_interface/cblas_ctrsv.c
    cblas_interface/cblas_icamax.c
    # Complex Double
    cblas_interface/cblas_zaxpy.c
    cblas_interface/cblas_zcopy.c
    cblas_interface/cblas_zdotc_sub.c
    cblas_interface/cblas_zdotu_sub.c
    cblas_interface/cblas_zgbmv.c
    cblas_interface/cblas_zgemm.c
    cblas_interface/cblas_zgemv.c
    cblas_interface/cblas_zgerc.c
    cblas_interface/cblas_zgeru.c
    cblas_interface/cblas_zhbmv.c
    cblas_interface/cblas_zhemm.c
    cblas_interface/cblas_zhemv.c
    cblas_interface/cblas_zher2.c
    cblas_interface/cblas_zher2k.c
    cblas_interface/cblas_zher.c
    cblas_interface/cblas_zherk.c
    cblas_interface/cblas_zhpmv.c
    cblas_interface/cblas_zhpr2.c
    cblas_interface/cblas_zhpr.c
    cblas_interface/cblas_zrotg.c
    cblas_interface/cblas_zdrot.c
    cblas_interface/cblas_zscal.c
    cblas_interface/cblas_zdscal.c
    cblas_interface/cblas_zswap.c
    cblas_interface/cblas_zsymm.c
    cblas_interface/cblas_zsyr2k.c
    cblas_interface/cblas_zsyrk.c
    cblas_interface/cblas_ztbmv.c
    cblas_interface/cblas_ztbsv.c
    cblas_interface/cblas_ztpmv.c
    cblas_interface/cblas_ztpsv.c
    cblas_interface/cblas_ztrmm.c
    cblas_interface/cblas_ztrmv.c
    cblas_interface/cblas_ztrsm.c
    cblas_interface/cblas_ztrsv.c
    cblas_interface/cblas_izamax.c
    cblas_interface/cblas_daxpby.c
    cblas_interface/cblas_saxpby.c
    cblas_interface/cblas_caxpby.c
    cblas_interface/cblas_zaxpby.c
    cblas_interface/cblas_simatcopy.c
    cblas_interface/cblas_dimatcopy.c
    cblas_interface/cblas_cimatcopy.c
    cblas_interface/cblas_zimatcopy.c
    cblas_interface/cblas_somatcopy.c
    cblas_interface/cblas_domatcopy.c
    cblas_interface/cblas_comatcopy.c
    cblas_interface/cblas_zomatcopy.c
    cblas_interface/cblas_dgeadd.c
    cblas_interface/cblas_cgeadd.c
    cblas_interface/cblas_zgeadd.c
    cblas_interface/cblas_sgeadd.c
    cblas_interface/cblas_dgemmt.c
    cblas_interface/cblas_cgemmt.c
    cblas_interface/cblas_zgemmt.c
    cblas_interface/cblas_sgemmt.c
    cblas_interface/cblas_dgemmtr.c
    cblas_interface/cblas_cgemmtr.c
    cblas_interface/cblas_zgemmtr.c
    cblas_interface/cblas_sgemmtr.c


    )
IF (CBLAS STREQUAL ON)
    SET (SRC ${SRC} ${CBLAS_SRC})
ENDIF()

#
# LAPACK
#
IF ( LAPACK )
    # 3.7.1 and 3.7.0 share the same api/abi
    IF(LAPACK_API_VERSION STREQUAL "3.12.0")
        SET(_LAPACK_FILE_SUFFIX 3_12_0)
    ELSEIF (LAPACK_API_VERSION STREQUAL "3.12.0-wodprc")
        SET(_LAPACK_FILE_SUFFIX 3_12_0-wodprc) #
    ELSEIF(LAPACK_API_VERSION STREQUAL "3.11.0")
        SET(_LAPACK_FILE_SUFFIX 3_11_0)
    ELSEIF (LAPACK_API_VERSION STREQUAL "3.11.0-wodprc")
        SET(_LAPACK_FILE_SUFFIX 3_11_0-wodprc) #
    ELSEIF(LAPACK_API_VERSION STREQUAL "3.10.1")
        SET(_LAPACK_FILE_SUFFIX 3_10_1)
    ELSEIF (LAPACK_API_VERSION STREQUAL "3.10.1-wodprc")
        SET(_LAPACK_FILE_SUFFIX 3_10_1-wodprc) #
    ELSEIF(LAPACK_API_VERSION STREQUAL "3.10.0")
        SET(_LAPACK_FILE_SUFFIX 3_10_0)
    ELSEIF (LAPACK_API_VERSION STREQUAL "3.10.0-wodprc")
        SET(_LAPACK_FILE_SUFFIX 3_10_0-wodprc) #
    ELSEIF(LAPACK_API_VERSION STREQUAL "3.9.1")
        SET(_LAPACK_FILE_SUFFIX 3_9_1)
    ELSEIF (LAPACK_API_VERSION STREQUAL "3.9.1-wodprc")
        SET(_LAPACK_FILE_SUFFIX 3_9_1-wodprc) #
    ELSEIF(LAPACK_API_VERSION STREQUAL "3.9.0")
        SET(_LAPACK_FILE_SUFFIX 3_9_0)
    ELSEIF (LAPACK_API_VERSION STREQUAL "3.9.0-wodprc")
        SET(_LAPACK_FILE_SUFFIX 3_9_0-wodprc) #
    ELSEIF(LAPACK_API_VERSION STREQUAL "3.8.0")
        SET(_LAPACK_FILE_SUFFIX 3_8_0)
    ELSEIF (LAPACK_API_VERSION STREQUAL "3.8.0-wodprc")
        SET(_LAPACK_FILE_SUFFIX 3_8_0-wodprc) #
    ELSEIF(LAPACK_API_VERSION STREQUAL "3.7.1")
        SET(_LAPACK_FILE_SUFFIX 3_7_0)
    ELSEIF (LAPACK_API_VERSION STREQUAL "3.7.1-wodprc")
        SET(_LAPACK_FILE_SUFFIX 3_7_0-wodprc) #
    ELSEIF(LAPACK_API_VERSION STREQUAL "3.7.0")
        SET(_LAPACK_FILE_SUFFIX 3_7_0)
    ELSEIF (LAPACK_API_VERSION STREQUAL "3.7.0-wodprc")
        SET(_LAPACK_FILE_SUFFIX 3_7_0-wodprc) #
    ELSEIF(LAPACK_API_VERSION STREQUAL "3.6.1")
        SET(_LAPACK_FILE_SUFFIX 3_6_1)
    ELSEIF (LAPACK_API_VERSION STREQUAL "3.6.1-wodprc")
        SET(_LAPACK_FILE_SUFFIX 3_6_1-wodprc) #
    ELSEIF (LAPACK_API_VERSION STREQUAL "3.6.0")
        SET(_LAPACK_FILE_SUFFIX 3_6_0) #
    ELSEIF (LAPACK_API_VERSION STREQUAL "3.6.0-wodprc")
        SET(_LAPACK_FILE_SUFFIX 3_6_0-wodprc) #
    ELSEIF (LAPACK_API_VERSION STREQUAL "3.5.0")
        SET(_LAPACK_FILE_SUFFIX 3_5_0) #
    ELSEIF (LAPACK_API_VERSION STREQUAL "3.4.2")
        SET(_LAPACK_FILE_SUFFIX 3_4_2) #
    ELSEIF (LAPACK_API_VERSION STREQUAL "3.4.1")
        SET(_LAPACK_FILE_SUFFIX 3_4_1) #
    ELSEIF (LAPACK_API_VERSION STREQUAL "3.4.0")
        SET(_LAPACK_FILE_SUFFIX 3_4_0) #
    ELSEIF (LAPACK_API_VERSION STREQUAL "3.3.1")
        SET(_LAPACK_FILE_SUFFIX 3_3_1) #
    ELSEIF (LAPACK_API_VERSION STREQUAL "3.3.0")
        SET(_LAPACK_FILE_SUFFIX 3_3_0) #
    ENDIF()
    INCLUDE(lapack_interface/lapack_${_LAPACK_FILE_SUFFIX}.cmake)

    SET (SRC ${SRC} ${LAPACK_SRC} lapack_interface/ilaver.c
        lapack_interface/load_lapack_${_LAPACK_FILE_SUFFIX}.c
        lapack_interface/load_lapack_${_LAPACK_FILE_SUFFIX}_fallback.c
        )

    FILE(COPY lapack_interface/lapack_${_LAPACK_FILE_SUFFIX}.h DESTINATION ${PROJECT_BINARY_DIR}/include/)
    FILE(RENAME  ${PROJECT_BINARY_DIR}/include/lapack_${_LAPACK_FILE_SUFFIX}.h ${PROJECT_BINARY_DIR}/include/lapack.h)
    FILE(COPY lapack_interface/flexiblas_real_lapack_${_LAPACK_FILE_SUFFIX}.h DESTINATION ${PROJECT_BINARY_DIR}/include/)
    FILE(RENAME  ${PROJECT_BINARY_DIR}/include/flexiblas_real_lapack_${_LAPACK_FILE_SUFFIX}.h ${PROJECT_BINARY_DIR}/include/flexiblas_real_lapack.h)

ENDIF()

FIND_PACKAGE(OpenMP)

INCLUDE_DIRECTORIES(${INCLUDE_DIR} ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/lapack_interface)
add_library(flexiblas SHARED ${SRC}) #  $<TARGET_OBJECTS:cscutils_src>)
add_executable(flexiblas-tool flexiblas_tool.c tool/hookhandling.c tool/blashandling.c ${TOOL_SRC} ) # $<TARGET_OBJECTS:cscutils_src>)

IF (LINK_OPENMP)
    TARGET_LINK_LIBRARIES(flexiblas ${LIBS} cscutils OpenMP::OpenMP_C)
    TARGET_LINK_LIBRARIES(flexiblas-tool PRIVATE flexiblas_mgmt ${LIBS} cscutils OpenMP::OpenMP_C)
ELSE()
    TARGET_LINK_LIBRARIES(flexiblas ${LIBS} cscutils) # OpenMP::OpenMP_C)
    TARGET_LINK_LIBRARIES(flexiblas-tool PRIVATE flexiblas_mgmt ${LIBS} cscutils) # OpenMP::OpenMP_C)
ENDIF()


add_library(flexiblas_mgmt SHARED ${FLEXIBLAS_MGMT_SRC}) # $<TARGET_OBJECTS:cscutils_src>)
TARGET_LINK_LIBRARIES(flexiblas_mgmt ${LIBS} cscutils )

add_library(flexiblas_api SHARED ${FLEXIBLAS_API_SRC} ) # $<TARGET_OBJECTS:cscutils_src>)
IF (WIN32)
TARGET_LINK_LIBRARIES(flexiblas_api ${DL_LIB} cscutils)
ELSE()
TARGET_LINK_LIBRARIES(flexiblas_api ${DL_LIB} cscutils c)
ENDIF()

set(GENERIC_LIB_VERSION "${FLEXIBLAS_VERSION_MAJOR}.${FLEXIBLAS_VERSION_MINOR}")
set(GENERIC_LIB_SOVERSION "${FLEXIBLAS_VERSION_MAJOR}")


add_executable(flexiblas-config flexiblas-config.c flexiblas_printf.c mgmt.c utils.c paths.c hooks.c common.c sh_utils.c)
TARGET_LINK_LIBRARIES(flexiblas-config PRIVATE ${DL_LIB} cscutils)

IF ( C_W_PEDANTIC )
    target_compile_options(flexiblas PRIVATE "-Wpedantic")
    target_compile_options(flexiblas-tool PRIVATE "-Wpedantic")
    target_compile_options(flexiblas_mgmt PRIVATE "-Wpedantic")
    target_compile_options(flexiblas_api PRIVATE "-Wpedantic")
    target_compile_options(flexiblas-config PRIVATE "-Wpedantic")
ENDIF()






IF( NOT WIN32 )
    IF ( NOT ${CMAKE_C_COMPILER_ID} MATCHES "XL")
        SET_TARGET_PROPERTIES(flexiblas PROPERTIES VERSION ${GENERIC_LIB_VERSION} SOVERSION ${GENERIC_LIB_SOVERSION} LINK_FLAGS "-Bsymbolic" )
        SET_TARGET_PROPERTIES(flexiblas_mgmt PROPERTIES VERSION ${GENERIC_LIB_VERSION} SOVERSION ${GENERIC_LIB_SOVERSION} LINK_FLAGS "-Bsymbolic" )
        SET_TARGET_PROPERTIES(flexiblas_api PROPERTIES VERSION ${GENERIC_LIB_VERSION} SOVERSION ${GENERIC_LIB_SOVERSION} LINK_FLAGS "-Bsymbolic" )
    ENDIF()
ENDIF()



IF (FLEXIBLAS_ABI_INTEL)
    SET_TARGET_PROPERTIES(flexiblas PROPERTIES OUTPUT_NAME flexiblas_intel)
ENDIF()

IF (FLEXIBLAS_ABI_IBM)
    SET_TARGET_PROPERTIES(flexiblas PROPERTIES OUTPUT_NAME flexiblas_xl)
ENDIF()


IF(FLEXIBLAS_INTEGER8)
    SET_TARGET_PROPERTIES(flexiblas      PROPERTIES OUTPUT_NAME ${flexiblasname})
    SET_TARGET_PROPERTIES(flexiblas_api  PROPERTIES OUTPUT_NAME ${flexiblasname}_api)
    SET_TARGET_PROPERTIES(flexiblas_mgmt PROPERTIES OUTPUT_NAME ${flexiblasname}_mgmt)
    IF (WIN32)
        SET_TARGET_PROPERTIES(flexiblas-tool PROPERTIES OUTPUT_NAME ${flexiblasname}-tool)
    ELSE()
        SET_TARGET_PROPERTIES(flexiblas-tool PROPERTIES OUTPUT_NAME ${flexiblasname})
    ENDIF()
    SET_TARGET_PROPERTIES(flexiblas-config PROPERTIES OUTPUT_NAME ${flexiblasname}-config)

    IF (USE_INTERFACE_INTEL)
        SET_TARGET_PROPERTIES(flexiblas PROPERTIES OUTPUT_NAME ${flexiblasname}_intel)
    ENDIF()
ELSE ()
    SET_TARGET_PROPERTIES(flexiblas      PROPERTIES OUTPUT_NAME ${flexiblasname})
    IF (WIN32)
        SET_TARGET_PROPERTIES(flexiblas-tool PROPERTIES OUTPUT_NAME flexiblas-tool)
    ELSE()
        SET_TARGET_PROPERTIES(flexiblas-tool PROPERTIES OUTPUT_NAME flexiblas)
    ENDIF()
ENDIF()


If (WIN32)
install(TARGETS flexiblas LIBRARY DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR} ARCHIVE  DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR} RUNTIME DESTINATION ${CMAKE_INSTALL_FULL_BINDIR})
install(TARGETS flexiblas_mgmt LIBRARY DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR} ARCHIVE  DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR} RUNTIME DESTINATION ${CMAKE_INSTALL_FULL_BINDIR})
install(TARGETS flexiblas_api LIBRARY DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR} ARCHIVE  DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR} RUNTIME DESTINATION ${CMAKE_INSTALL_FULL_BINDIR})
ELSE()
install(TARGETS flexiblas LIBRARY DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR} ARCHIVE  DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR})
install(TARGETS flexiblas_mgmt LIBRARY DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR} ARCHIVE  DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR})
install(TARGETS flexiblas_api LIBRARY DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR} ARCHIVE  DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR})
ENDIF()

install(TARGETS flexiblas-tool RUNTIME DESTINATION ${CMAKE_INSTALL_FULL_BINDIR})
install(TARGETS flexiblas-config RUNTIME DESTINATION ${CMAKE_INSTALL_FULL_BINDIR})

INCLUDE(CheckBlasZdotcMKL)
INCLUDE(CheckFortranFunctionExists)
INCLUDE(CheckFunctionExists)

# SET(BLA_STATIC TRUE)

MACRO(BuildNVPL)
    SET(BACKEND_CODE backends/nvpl/backend.c )
    FIND_PACKAGE(nvpl COMPONENTS blas lapack QUIET)

    IF (nvpl_FOUND )
        IF (FLEXIBLAS_INTEGER8)
            SET(LP "ilp64")
        ELSE()
            SET(LP "lp64")
        ENDIF()
        #
        # Create the wrapper
        #
        MESSAGE(STATUS "NVPL_THREADING TYPES: ${nvpl_blas_THREADING_TYPES}")

        FOREACH(THR "seq" "omp")
            MESSAGE(STATUS "Build NVPL Backend for ${THR} / ${LP}")
            ADD_LIBRARY(flexiblas_nvpl_${LP}_${THR} SHARED ${BACKEND_CODE})
            TARGET_LINK_LIBRARIES(flexiblas_nvpl_${LP}_${THR} PUBLIC nvpl::blas_${LP}_${THR}  nvpl::lapack_${LP}_${THR})
            IF (FLEXIBLAS_INTEGER8 OR BLAS_INTERFACE64)
                SET(EXTRA_FLAGS "-DBACKEND_INTEGER8")
            ELSE ()
                SET(EXTRA_FLAGS "")
            ENDIF()
            IF(CBLAS STREQUAL ON)
                MESSAGE(STATUS "Add CBLAS in dummy library")
                SET(EXTRA_FLAGS "${EXTRA_FLAGS} -DCBLAS_INTERFACE")
            ENDIF()
            SET_TARGET_PROPERTIES(flexiblas_nvpl_${LP}_${THR} PROPERTIES COMPILE_FLAGS "${EXTRA_FLAGS}")

            if(THR_TYPE STREQUAL omp)
                target_link_libraries(${name} PRIVATE OpenMP::OpenMP_C)
            endif()

            INSTALL(TARGETS flexiblas_nvpl_${LP}_${THR} LIBRARY DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR}/${FLEXIBLAS_LIBRARY_DIR})

            SET(VENDOR "NVPL_${THR}")
            LIST(APPEND EXTRA_BLAS "NVPL_${THR}")
            SET(${VENDOR}_BLAS_LIBRARIES "nvpl::blas_${LP}_${THR};nvpl::lapack_${LP}_${THR}" PARENT_SCOPE)
            IF (NOT EXTRA_FLAGS)
                SET(${VENDOR}_BLAS_EXTRA_FLAGS "")
            ELSE()
                SET(${VENDOR}_BLAS_EXTRA_FLAGS "${EXTRA_FLAGS}" PARENT_SCOPE)
            ENDIF()

            UNSET(LIBNAME CACHE)
            SET(LIBNAME "${CMAKE_SHARED_LIBRARY_PREFIX}flexiblas_nvpl_${LP}_${THR}${CMAKE_SHARED_LIBRARY_SUFFIX}")
            FILE(WRITE "${CMAKE_BINARY_DIR}/${FLEXIBLAS_RC_DIR}/${VENDOR}.conf" "[${VENDOR}]\nlibrary = ${LIBNAME}\n\n")
            INSTALL(FILES "${CMAKE_BINARY_DIR}/${FLEXIBLAS_RC_DIR}/${VENDOR}.conf" DESTINATION "${CMAKE_INSTALL_FULL_SYSCONFDIR}/${FLEXIBLAS_RC_DIR}")
            # Check for default setting
            STRING (TOLOWER  "${FLEXIBLAS_DEFAULT}" FDB_LOW)
            IF ( FDB_LOW STREQUAL ivendor)
                SET(DEFAULT_FOUND TRUE)
            ENDIF()
        ENDFOREACH()

    ENDIF()
ENDMACRO()

MACRO(BuildBackendBlas VENDOR BACKEND_CODE)
    MESSAGE(STATUS "Build additional BLAS interface: ${VENDOR}")
    MESSAGE(STATUS "-- Libs: ${${VENDOR}_LIBRARY}")
    MESSAGE(STATUS "-- Linker Flags: ${${VENDOR}_LINKER_FLAGS}")

    #
    # Configure the found backend
    #
    CheckBlasZdotcMKL("${${VENDOR}_LIBRARY}" "" ${FLEXIBLAS_INTEGER8})
    SET(__CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES})
    SET(CMAKE_REQUIRED_LIBRARIES ${${VENDOR}_LIBRARY})

    UNSET(ISFLEXIBLAS CACHE)
    UNSET(DAXPY CACHE)
    UNSET(MKL_OMATCOPY CACHE)
    UNSET(CBLAS_DAXPY CACHE)
    UNSET(_MKL_VERSION CACHE)
    UNSET(_BUILD CACHE)
    Check_Function_Exists(flexiblas_exit ISFLEXIBLAS)
    Check_Fortran_Function_Exists(DAXPY  DAXPY)
    Check_Function_Exists(cblas_daxpy  CBLAS_DAXPY)
    Check_Function_Exists(MKL_Get_Version _MKL_VERSION)

    # Use the MKL Backend Code if it is MKL
    IF(_MKL_VERSION)
        SET(BACKEND_CODE backends/intel_mkl/backend.c)
    ENDIF()

    SET(CMAKE_REQUIRED_LIBRARIES ${__CMAKE_REQUIRED_LIBRARIES})

    SET(_BUILD 1)
    IF(NOT DAXPY)
        MESSAGE(WARNING "${VENDOR} library does not contain a BLAS implementation or does not link. If TBB is involved please check if the C++ ABI is compatible TBB")
        SET(_BUILD 0)
    ENDIF()

    IF(ISFLEXIBLAS)
        MESSAGE(WARNING "${VENDOR} (${${VENDOR}_LIBRARY}) contains FlexiBLAS. Disable the use with -D${VENDOR}=OFF")
        SET(_BUILD 0)
    ENDIF()

    IF (_BUILD)
        UNSET(EXTRA_FLAGS CACHE)
        string(TOLOWER ${VENDOR} ivendor)


        #
        # Create the wrapper
        #
        ADD_LIBRARY(flexiblas_${ivendor} SHARED ${BACKEND_CODE})
        IF (FLEXIBLAS_INTEGER8 OR BLAS_INTERFACE64)
            SET(EXTRA_FLAGS "-DBACKEND_INTEGER8")
        ELSE ()
            SET(EXTRA_FLAGS "")
        ENDIF()
        IF (ZDOTC_MKL)
            MESSAGE(STATUS "${VENDOR} uses Intel ZDOTC")
            SET(EXTRA_FLAGS "${EXTRA_FLAGS} -DZDOTC_MKL")
        ENDIF (ZDOTC_MKL)

        IF(CBLAS_DAXPY AND CBLAS STREQUAL ON)
            MESSAGE(STATUS "Add CBLAS in dummy library")
            SET(EXTRA_FLAGS "${EXTRA_FLAGS} -DCBLAS_INTERFACE")
        ENDIF()

        SET_TARGET_PROPERTIES(flexiblas_${ivendor} PROPERTIES COMPILE_FLAGS "${EXTRA_FLAGS}")
        IF ( ${VENDOR}_LINKER_FLAGS)
            SET_TARGET_PROPERTIES(flexiblas_${ivendor} PROPERTIES LINK_FLAGS "${${VENDOR}_LINKER_FLAGS}")
            SET(${VENDOR}_BLAS_LINKER_FLAGS "${${VENDOR}_LINKER_FLAGS}" PARENT_SCOPE)
        ENDIF()

        IF (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
            SET(BLAS_LIBRARIES ${BLAS_LIBRARIES} gfortran)
        ENDIF()

        TARGET_LINK_LIBRARIES(flexiblas_${ivendor} ${${VENDOR}_LIBRARY})
        INSTALL(TARGETS flexiblas_${ivendor} LIBRARY DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR}/${FLEXIBLAS_LIBRARY_DIR})

        LIST(APPEND EXTRA_BLAS "${VENDOR}")
        SET(${VENDOR}_BLAS_LIBRARIES "${${VENDOR}_LIBRARY}" PARENT_SCOPE)
        IF (NOT EXTRA_FLAGS)
            SET(${VENDOR}_BLAS_EXTRA_FLAGS "")
        ELSE()
            SET(${VENDOR}_BLAS_EXTRA_FLAGS "${EXTRA_FLAGS}" PARENT_SCOPE)
        ENDIF()

        UNSET(LIBNAME CACHE)
        SET(LIBNAME "${CMAKE_SHARED_LIBRARY_PREFIX}flexiblas_${ivendor}${CMAKE_SHARED_LIBRARY_SUFFIX} ")
        FILE(WRITE "${CMAKE_BINARY_DIR}/${FLEXIBLAS_RC_DIR}/${VENDOR}.conf" "[${VENDOR}]\nlibrary = ${LIBNAME}\n\n")
        INSTALL(FILES "${CMAKE_BINARY_DIR}/${FLEXIBLAS_RC_DIR}/${VENDOR}.conf" DESTINATION "${CMAKE_INSTALL_FULL_SYSCONFDIR}/${FLEXIBLAS_RC_DIR}")
        # Check for default setting
        STRING (TOLOWER  "${FLEXIBLAS_DEFAULT}" FDB_LOW)
        IF ( FDB_LOW STREQUAL ivendor)
            SET(DEFAULT_FOUND TRUE)
        ENDIF()
    ENDIF()
ENDMACRO()

MACRO(BuildFindPackage VENDOR BACKEND_CODE)
    FIND_PACKAGE(${VENDOR})
    IF ( ${VENDOR}_FOUND AND NOT "${${VENDOR}}" STREQUAL "OFF" )
        MESSAGE(STATUS "Build BLAS interface: ${VENDOR}")
        MESSAGE(STATUS "Libs: ${${VENDOR}_LIBRARY}")
        BuildBackendBlas(${VENDOR} ${BACKEND_CODE})
    ELSEIF(NOT ${VENDOR}_FOUND)
        MESSAGE(STATUS "${VENDOR} not found.")
    ELSE()
        MESSAGE(STATUS "Found ${VENDOR} but deactivated manually.")
    ENDIF()
ENDMACRO()



MACRO(Build_Extra_BLAS VENDOR BACKEND_CODE)
    MESSAGE(STATUS "")
    UNSET(BLAS_FOUND CACHE)
    SET(BLAS_FOUND FALSE )
    UNSET(BLAS_LIBRARIES CACHE)
    UNSET(BLAS_LINKER_FLAGS CACHE)
    SET(BLA_VENDOR ${VENDOR})
    STRING(TOUPPER ${VENDOR} uvendor)
    IF( "${uvendor}" STREQUAL "APPLE")
        SET (uvendor APPLEACCELERATE)
    ENDIF()
    IF (DEFINED ${uvendor})
        IF (${uvendor} STREQUAL OFF)
            SET(BLAS_FOUND OFF)
            MESSAGE(STATUS "Disable BLAS backend - ${VENDOR}")
        ELSE()
            MESSAGE(STATUS "Use user defined settings for ${VENDOR}")
            SET(BLAS_FOUND TRUE)
            SET(BLAS_LIBRARIES ${${uvendor}})
            IF ( DEFINED ${uvendor}_LDFLAGS )
                SET(BLAS_LINKER_FLAGS ${${uvendor}_LDFLAGS})
            ENDIF()
        ENDIF()
    ELSE ()
        MESSAGE(STATUS "Search for ${VENDOR}")
        IF (FLEXIBLAS_INTEGER8)
            FIND_PACKAGE(BLAS64)
        ELSE()
            FIND_PACKAGE(BLAS)
        ENDIF()
    ENDIF()
    IF (BLAS_FOUND)
        MESSAGE(STATUS "Build extra BLAS interface: ${VENDOR}")
        MESSAGE(STATUS "Libs: ${BLAS_LIBRARIES}")

        CheckBlasZdotcMKL("${BLAS_LIBRARIES}" "${BLAS_LINKER_FLAGS}" ${FLEXIBLAS_INTEGER8})
        SET(__CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES})
        SET(CMAKE_REQUIRED_LIBRARIES ${BLAS_LIBRARIES} ${BLAS_LINKER_FLAGS})
        #UNSET(SCABS1_EXIST CACHE)
        UNSET(ISFLEXIBLAS CACHE)
        UNSET(DAXPY CACHE)
        UNSET(MKL_OMATCOPY CACHE)
        UNSET(CBLAS_DAXPY CACHE)
        Check_Fortran_Function_Exists(SCABS1 SCABS1_EXIST)
        Check_Function_Exists(flexiblas_exit ISFLEXIBLAS)
        Check_Fortran_Function_Exists(DAXPY  DAXPY)
        Check_Fortran_Function_Exists(MKL_DOMATCOPY MKL_OMATCOPY)
        Check_Function_Exists(cblas_daxpy  CBLAS_DAXPY)
        SET(CMAKE_REQUIRED_LIBRARIES ${__CMAKE_REQUIRED_LIBRARIES})
        IF(NOT DAXPY)
            MESSAGE(FATAL_ERROR "Provided BLAS library does not contain a BLAS implementation or does not link.")
        ENDIF()

        IF (NOT ISFLEXIBLAS)
            UNSET(EXTRA_FLAGS CACHE)
            string(TOLOWER ${VENDOR} ivendor)
            string(TOUPPER ${VENDOR} uvendor)
            ADD_LIBRARY(flexiblas_${ivendor} SHARED ${BACKEND_CODE})
            IF (FLEIXBLAS_INTEGER8 OR BLAS_INTERFACE64)
                SET(EXTRA_FLAGS "-DBACKEND_INTEGER8")
            ELSE ()
                SET(EXTRA_FLAGS "")
            ENDIF()
            IF (ZDOTC_MKL)
                MESSAGE(STATUS "${VENDOR} uses Intel ZDOTC")
                SET(EXTRA_FLAGS "${EXTRA_FLAGS} -DZDOTC_MKL")
            ENDIF (ZDOTC_MKL)
            IF (NOT SCABS1_EXIST)
                MESSAGE(STATUS "${VENDOR} does not provide a scabs1 function")
                SET(EXTRA_FLAGS "${EXTRA_FLAGS} -DSCABS_MISSING")
            ENDIF(NOT SCABS1_EXIST)
            IF(CBLAS_DAXPY)
                IF (CBLAS STREQUAL ON)
                    MESSAGE(STATUS "Add CBLAS in dummy library")
                    SET(EXTRA_FLAGS "${EXTRA_FLAGS} -DCBLAS_INTERFACE")
                ENDIF()
            ENDIF()

            #IF ( MKL_OMATCOPY )
            #	MESSAGE(STATUS "BLAS ${ivendor}$ seem to be an Intel MKL")
            #		SET_TARGET_PROPERTIES(blas_${ivendor} PROPERTIES COMPILE_DEFINITIONS "MKL")
            #ENDIF()
            MESSAGE(STATUS "Extra Linker Flags: ${BLAS_LINKER_FLAGS}")
            SET_TARGET_PROPERTIES(flexiblas_${ivendor} PROPERTIES COMPILE_FLAGS "${EXTRA_FLAGS}")
            # SET_TARGET_PROPERTIES(blas_${ivendor} PROPERTIES COMPILE_FLAGS "${EXTRA_FLAGS}" LINK_FLAGS "${BLAS_LINKER_FLAGS}")

            IF (CMAKE_C_COMPILER_ID STREQUAL "GNU")
                SET(BLAS_LIBRARIES ${BLAS_LIBRARIES} gfortran)
            ENDIF()
            TARGET_LINK_LIBRARIES(flexiblas_${ivendor} ${BLAS_LIBRARIES})
            INSTALL(TARGETS flexiblas_${ivendor} LIBRARY DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR}/${FLEXIBLAS_LIBRARY_DIR})

            LIST(APPEND EXTRA_BLAS "${uvendor}")
            SET(${uvendor}_BLAS_LIBRARIES "${BLAS_LIBRARIES}" PARENT_SCOPE)
            IF (NOT EXTRA_FLAGS)
                SET(${uvendor}_BLAS_EXTRA_FLAGS "")
            ELSE()
                SET(${uvendor}_BLAS_EXTRA_FLAGS "${EXTRA_FLAGS}" PARENT_SCOPE)
            ENDIF()
            SET(${uvendor}_BLAS_LINKER_FLAGS "${BLAS_LINKER_FLAGS}" PARENT_SCOPE)

            UNSET(LIBNAME CACHE)
            SET(LIBNAME "${CMAKE_SHARED_LIBRARY_PREFIX}flexiblas_${ivendor}${CMAKE_SHARED_LIBRARY_SUFFIX} ")
            FILE(WRITE "${CMAKE_BINARY_DIR}/${FLEXIBLAS_RC_DIR}/${VENDOR}.conf" "[${VENDOR}]\nlibrary = ${LIBNAME}\n\n")
            INSTALL(FILES "${CMAKE_BINARY_DIR}/${FLEXIBLAS_RC_DIR}/${VENDOR}.conf" DESTINATION "${CMAKE_INSTALL_FULL_SYSCONFDIR}/${FLEXIBLAS_RC_DIR}")
            # Check for default setting
            STRING (TOLOWER  "${FLEXIBLAS_DEFAULT}" FDB_LOW)
            IF ( FDB_LOW STREQUAL ivendor)
                SET(DEFAULT_FOUND TRUE)
            ENDIF()
        ELSE ()
            MESSAGE(STATUS "Skip BLAS ${ivendor} because it seems to contain FlexiBLAS")
        ENDIF()
    ENDIF()
ENDMACRO()

#
# Build BLAS interfaces
#
FILE(WRITE "${CMAKE_BINARY_DIR}/${FLEXIBLASRC}" "verbose = 0\n\n[NETLIB]\nlibrary = libflexiblas_netlib${CMAKE_SHARED_LIBRARY_SUFFIX}\n\n")

#
# Build Additional BLAS interfaces
#
#Build_Extra_BLAS(Generic)

SET(DEFAULT_FOUND FALSE)
IF (FLEXIBLAS_DEFAULT STREQUAL "NETLIB")
    SET(DEFAULT_FOUND TRUE)
ENDIF()

IF( BLAS_AUTO_DETECT STREQUAL ON)
    IF (NOT FLEXIBLAS_INTEGER8)
        BuildFindPackage(OpenBLASSerial backends/generic/backend.c)
        BuildFindPackage(OpenBLASPThread backends/generic/backend.c)
        BuildFindPackage(OpenBLASOpenMP backends/generic/backend.c)

        BuildFindPackage(BlisSerial backends/generic/backend.c)
        BuildFindPackage(BlisPThread backends/generic/backend.c)
        BuildFindPackage(BlisOpenMP backends/generic/backend.c)

        BuildFindPackage(MklSerial backends/intel_mkl/backend.c)
        BuildFindPackage(MklOpenMP backends/intel_mkl/backend.c)
        BuildFindPackage(MklTBB backends/intel_mkl/backend.c)

        BuildNVPL()



    ELSE()
        BuildFindPackage(OpenBLASSerial64 backends/generic/backend.c)
        BuildFindPackage(OpenBLASPThread64 backends/generic/backend.c)
        BuildFindPackage(OpenBLASOpenMP64 backends/generic/backend.c)

        BuildFindPackage(BlisSerial64 backends/generic/backend.c)
        BuildFindPackage(BlisPThread64 backends/generic/backend.c)
        BuildFindPackage(BlisOpenMP64 backends/generic/backend.c)

        BuildFindPackage(MklSerial64 backends/intel_mkl/backend.c)
        BuildFindPackage(MklOpenMP64 backends/intel_mkl/backend.c)
        BuildFindPackage(MklTBB64 backends/intel_mkl/backend.c)

        BuildNVPL()



    ENDIF()

    Build_Extra_BLAS(ATLAS  	backends/generic/backend.c)
    Build_Extra_BLAS(Apple          backends/generic/backend.c)
ENDIF()

FOREACH(ADDITIONAL_BLAS ${EXTRA})
    MESSAGE(STATUS "Build additional BLAS backend with ${ADDITIONAL_BLAS}")
    IF (${ADDITIONAL_BLAS}_LIBRARY)
        BuildBackendBlas(${ADDITIONAL_BLAS} backends/generic/backend.c)
    ELSE()
        MESSAGE(FATAL_ERROR "-- ${ADDITIONAL_BLAS}_LIBRARY is not set.")
    ENDIF()
ENDFOREACH()


IF ( DEFAULT_FOUND )
    FILE(WRITE "${CMAKE_BINARY_DIR}/${FLEXIBLASRC}" "default = ${FLEXIBLAS_DEFAULT}\n\n")
ELSE()
    MESSAGE(STATUS "Default BLAS ${FLEXIBLAS_DEFAULT} not found. Use NETLIB instead.")
    SET(FLEXIBLAS_DEFAULT "NETLIB" PARENT_SCOPE)
    FILE(WRITE "${CMAKE_BINARY_DIR}/${FLEXIBLASRC}" "default = NETLIB\n\n")
ENDIF()

#
# MKL Custom
#
IF ( MKL_BUILDER STREQUAL ON)
    MESSAGE(STATUS "Build custom MKL from MKLROOT = $ENV{MKLROOT}")
    IF(NOT DEFINED ENV{MKLROOT})
        MESSAGE(FATAL_ERROR "MKLROOT must be set when building MKL backends.")
    ENDIF()
    EXECUTE_PROCESS( COMMAND uname -m COMMAND tr -d '\n' OUTPUT_VARIABLE ARCHITECTURE )
    IF(${ARCHITECTURE} STREQUAL "x86_64")
        IF (FLEXIBLAS_INTEGER8)
            SET(MKL_INTERFACE ilp64)
        ELSE()
            SET(MKL_INTERFACE lp64)
        ENDIF()
        SET(MKL_TARGET libintel64)
    ELSE()
        IF (FLEXIBLAS_INTEGER8)
            SET(MKL_INTERFACE ilp64)
        ELSE()
            SET(MKL_INTERFACE lp64)
        ENDIF()
        SET(MKL_TARGET libia32)
    ENDIF()
    SET (MKL_LIST mkl_2018.3.list)
    ADD_CUSTOM_TARGET(mkl_parallel COMMAND make -C "$ENV{MKLROOT}/tools/builder" ${MKL_TARGET} export=${CMAKE_CURRENT_SOURCE_DIR}/backends/intel_mkl/${MKL_LIST} name="${CMAKE_BINARY_DIR}/libflexiblas_mkl_custom_parallel" threading=parallel interface=${MKL_INTERFACE})
    ADD_CUSTOM_TARGET(mkl_sequential COMMAND make -C "$ENV{MKLROOT}/tools/builder" ${MKL_TARGET} export=${CMAKE_CURRENT_SOURCE_DIR}/backends/intel_mkl/${MKL_LIST} name="${CMAKE_BINARY_DIR}/libflexiblas_mkl_custom_sequential" threading=sequential interface=${MKL_INTERFACE})
    SET(MKL_PARALLEL ${CMAKE_BINARY_DIR}/libflexiblas_mkl_custom_parallel.so)
    SET(MKL_SEQUENTIAL ${CMAKE_BINARY_DIR}/libflexiblas_mkl_custom_sequential.so)

    ADD_LIBRARY(flexiblas_intel_mkl SHARED backends/intel_mkl/backend.c)
    ADD_LIBRARY(flexiblas_intel_mkl_seq SHARED backends/intel_mkl/backend.c)

    IF (FLEXIBLAS_INTEGER8 OR BLAS_INTERFACE64)
        SET(EXTRA_FLAGS "-DMKL_BUILDER -DBACKEND_INTEGER8")
    ELSE ()
        SET(EXTRA_FLAGS "-DMKL_BUILDER")
    ENDIF()
    SET(EXTRA_FLAGS "${EXTRA_FLAGS} -DZDOTC_MKL")
    SET_TARGET_PROPERTIES(flexiblas_intel_mkl PROPERTIES COMPILE_FLAGS "${EXTRA_FLAGS}")
    SET_TARGET_PROPERTIES(flexiblas_intel_mkl_seq PROPERTIES COMPILE_FLAGS "${EXTRA_FLAGS}")
    TARGET_LINK_LIBRARIES(flexiblas_intel_mkl ${MKL_PARALLEL})
    TARGET_LINK_LIBRARIES(flexiblas_intel_mkl_seq ${MKL_SEQUENTIAL})
    ADD_DEPENDENCIES(flexiblas_intel_mkl mkl_parallel)
    ADD_DEPENDENCIES(flexiblas_intel_mkl_seq mkl_sequential)

    LIST(APPEND EXTRA_BLAS "INTEL_MKL")
    LIST(APPEND EXTRA_BLAS "INTEL_MKL_SEQ")

    SET(INTEL_MKL_BLAS_LIBRARIES "flexiblas_intel_mkl" PARENT_SCOPE)
    SET(INTEL_MKL_SEQ_BLAS_LIBRARIES "flexiblas_intel_mkl_seq" PARENT_SCOPE)



    SET(LIBNAME "${CMAKE_SHARED_LIBRARY_PREFIX}flexiblas_intel_mkl${CMAKE_SHARED_LIBRARY_SUFFIX} ")
    FILE(APPEND "${CMAKE_BINARY_DIR}/${FLEXIBLASRC}" "[INTEL_MKL]\nlibrary = ${LIBNAME}\n\n")
    SET(LIBNAME "${CMAKE_SHARED_LIBRARY_PREFIX}flexiblas_intel_mkl_seq${CMAKE_SHARED_LIBRARY_SUFFIX} ")
    FILE(APPEND "${CMAKE_BINARY_DIR}/${FLEXIBLASRC}" "[INTEL_MKL_SEQ]\nlibrary = ${LIBNAME}\n\n")

    INSTALL(TARGETS flexiblas_intel_mkl LIBRARY DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR}/${FLEXIBLAS_LIBRARY_DIR})
    INSTALL(TARGETS flexiblas_intel_mkl_seq LIBRARY DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR}/${FLEXIBLAS_LIBRARY_DIR})
    INSTALL(FILES ${MKL_PARALLEL} ${MKL_SEQUENTIAL} DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR})


ENDIF()

# Compile Hooks
IF( EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/hooks" )
    MESSAGE(STATUS "Found hook directory.")
    ADD_SUBDIRECTORY(hooks)
ENDIF()

#Build_Extra_BLAS(Generic)
SET(EXTRA_BLAS "${EXTRA_BLAS}" PARENT_SCOPE)
#SET(EXTRA_BLAS_VENDOR "${EXTRA_BLAS_VENDOR}" PARENT_SCOPE)



