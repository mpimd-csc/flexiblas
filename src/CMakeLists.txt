SET ( SRC 
	flexiblas.c 
	integer.c
	double.c 
	single.c
	complex.c
	complex16.c
	hashtable.c
	lsame.c
	) 
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
	)
IF (CBLAS STREQUAL ON) 
	SET (SRC ${SRC} ${CBLAS_SRC}) 
ENDIF()

INCLUDE_DIRECTORIES(${INCLUDE_DIR})
add_library(flexiblas ${SRC})
TARGET_LINK_LIBRARIES(flexiblas ${LIBS})
set(GENERIC_LIB_VERSION "0.1.0")
set(GENERIC_LIB_SOVERSION "1")

add_executable(flexiblas-tool flexiblas_tool.c hashtable.c) 


IF(PROFILE STREQUAL ON)
	SET_TARGET_PROPERTIES(flexiblas PROPERTIES VERSION ${GENERIC_LIB_VERSION} SOVERSION ${GENERIC_LIB_SOVERSION} OUTPUT_NAME flexiblas-profile  LINK_FLAGS "-Bsymbolic")
	SET_TARGET_PROPERTIES(flexiblas-tool PROPERTIES OUTPUT_NAME flexiblas-profile) 

ELSE() 
	SET_TARGET_PROPERTIES(flexiblas PROPERTIES VERSION ${GENERIC_LIB_VERSION} SOVERSION ${GENERIC_LIB_SOVERSION} LINK_FLAGS "-Bsymbolic" )
	SET_TARGET_PROPERTIES(flexiblas-tool PROPERTIES OUTPUT_NAME flexiblas) 
ENDIF()


install(TARGETS flexiblas LIBRARY DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR} ARCHIVE  DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR})
install(TARGETS flexiblas-tool RUNTIME DESTINATION ${CMAKE_INSTALL_FULL_BINDIR}) 

INCLUDE(CheckBlasZdotcMKL)
INCLUDE(CheckFortranFunctionExists)
INCLUDE(CheckFunctionExists) 

IF ( PROFILE STREQUAL ON ) 
 SET (FLEXIBLASRC "flexiblasrc-profile") 
ELSE () 
 SET (FLEXIBLASRC "flexiblasrc")
ENDIF ()


MACRO(Build_Extra_BLAS VENDOR)
	MESSAGE(STATUS "")
	UNSET(BLAS_FOUND CACHE) 
	SET(BLAS_FOUND FALSE )
	UNSET(BLAS_LIBRARIES CACHE) 
	UNSET(BLAS_LINKER_FLAGS CACHE)
	SET(BLA_VENDOR ${VENDOR}) 
	STRING(TOUPPER ${VENDOR} uvendor)
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
		FIND_PACKAGE(BLAS)
	ENDIF()
	IF (BLAS_FOUND) 
		MESSAGE(STATUS "Build extra BLAS interface: ${VENDOR}")
		MESSAGE(STATUS "Libs: ${BLAS_LIBRARIES}")
		#		IF ( "${VENDOR}" MATCHES "Intel.*") 
		#			STRING(FIND "${BLAS_LIBRARIES}" "gf" GFIN)
		#			MESSAGE(STATUS "GFIN: ${GFIN}")
		#			IF ( ${GFIN} EQUAL -1 )
		#				SET(ZDOTC_MKL TRUE)
		#			ELSE ()
		#				SET(ZDOTC_MKL FALSE)
		#			ENDIF()
		#		ENDIF () 
		CheckBlasZdotcMKL("${BLAS_LIBRARIES}" "${BLAS_LINKER_FLAGS}" ${INTEGER8})
		SET(__CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES})
		SET(CMAKE_REQUIRED_LIBRARIES ${BLAS_LIBRARIES} ${BLAS_LINKER_FLAGS})
		UNSET(SCABS1_EXIST CACHE)
		UNSET(ISFLEXIBLAS CACHE)
		UNSET(DAXPY CACHE) 
		UNSET(CBLAS_DAXPY CACHE) 
		Check_Fortran_Function_Exists(SCABS1 SCABS1_EXIST)
		Check_Function_Exists(flexiblas_exit ISFLEXIBLAS) 
		Check_Fortran_Function_Exists(DAXPY  DAXPY)
		Check_Function_Exists(cblas_daxpy  CBLAS_DAXPY)
		SET(CMAKE_REQUIRED_LIBRARIES ${__CMAKE_REQUIRED_LIBRARIES})
		IF(NOT DAXPY) 
			MESSAGE(FATAL_ERROR "Provided BLAS library does not contain a BLAS implementation or does not link.")
		ENDIF()

		IF (NOT ISFLEXIBLAS) 
			UNSET(EXTRA_FLAGS CACHE)	
			string(TOLOWER ${VENDOR} ivendor)
			ADD_LIBRARY(blas_${ivendor} SHARED dummy_lib.c)
			SET(EXTRA_FLAGS "")
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

			SET_TARGET_PROPERTIES(blas_${ivendor} PROPERTIES COMPILE_FLAGS "${EXTRA_FLAGS}" LINK_FLAGS "${BLAS_LINKER_FLAGS}") 

			IF (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")  
				SET(BLAS_LIBRARIES ${BLAS_LIBRARIES} gfortran)
			ENDIF() 
			TARGET_LINK_LIBRARIES(blas_${ivendor} ${BLAS_LIBRARIES})
			IF ( PROFILE STREQUAL ON ) 
				INSTALL(TARGETS blas_${ivendor} LIBRARY DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR}/flexiblas-profile)
			ELSE ()
				INSTALL(TARGETS blas_${ivendor} LIBRARY DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR}/flexiblas)
			ENDIF()
			LIST(APPEND EXTRA_BLAS "${ivendor}")
			UNSET(LIBNAME CACHE)
			SET(LIBNAME "${CMAKE_SHARED_LIBRARY_PREFIX}blas_${ivendor}${CMAKE_SHARED_LIBRARY_SUFFIX} ") 
			FILE(APPEND "${CMAKE_BINARY_DIR}/${FLEXIBLASRC}" "${VENDOR} | ${LIBNAME}\n")
		ELSE ()
			MESSAGE(STATUS "Skip BLAS ${ivendor} because it seems to contain FlexiBLAS")
		ENDIF()
	ENDIF()
ENDMACRO()

#
# Build BLAS interfaces 
#
FILE(REMOVE "${CMAKE_BINARY_DIR}/${FLEXIBLASRC}") 
FILE(APPEND "${CMAKE_BINARY_DIR}/${FLEXIBLASRC}" "Netlib | libblas_netlib${CMAKE_SHARED_LIBRARY_SUFFIX}\n") 
#
# Build Additional BLAS interfaces 
#
#Build_Extra_BLAS(Generic) 
Build_Extra_BLAS(Goto)
Build_Extra_BLAS(OpenBLAS)
Build_Extra_BLAS(ATLAS)
#Build_Extra_BLAS(SunPerf)
#Build_Extra_BLAS(SCSL)
#Build_Extra_BLAS(SGIMATH)
#Build_Extra_BLAS(IBMESSL)
Build_Extra_BLAS(Intel_32)
Build_Extra_BLAS(Intel_32_seq)
Build_Extra_BLAS(Intel_64lp)
Build_Extra_BLAS(Intel_64lp_seq)
Build_Extra_BLAS(ACML)
Build_Extra_BLAS(ACML_MP)
Build_Extra_BLAS(Apple)
#Build_Extra_BLAS(Generic)
SET(EXTRA_BLAS "${EXTRA_BLAS}" PARENT_SCOPE)
INSTALL(FILES "${CMAKE_BINARY_DIR}/${FLEXIBLASRC}" DESTINATION "${CMAKE_INSTALL_FULL_SYSCONFDIR}" PERMISSIONS OWNER_WRITE  OWNER_READ  GROUP_READ WORLD_READ) 

