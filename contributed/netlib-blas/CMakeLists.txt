IF ( LAPACK_API_VERSION VERSION_GREATER 3.9.1 )
    MESSAGE(STATUS "Netlib BLAS uses the scaling/overflow/underflow improvements from LAPACK 3.10")
    SET( ROTG
        drotg.f90
        srotg.f90
        crotg.f90
        zrotg.f90
        )
    SET (NRM
        dznrm2.f90
        scnrm2.f90
        dnrm2.f90
        snrm2.f90

        )

ELSE ()
    MESSAGE(STATUS "Netlib BLAS uses the pre LAPACK 3.10.0 codes")
    SET( ROTG
        drotg.f
        srotg.f
        crotg.f
        zrotg.f
        )
    SET( NRM
        dznrm2.f
        scnrm2.f
        dnrm2.f
        snrm2.f
        )
ENDIF()


SET (NETLIBSRC
    ${ROTG}
    ${NRM}
caxpy.f
ccopy.f
cdotc.f
cdotu.f
cgbmv.f
cgemm.f
cgemv.f
cgerc.f
cgeru.f
chbmv.f
chemm.f
chemv.f
cher2.f
cher2k.f
cher.f
cherk.f
chpmv.f
chpr2.f
chpr.f
cscal.f
csrot.f
csscal.f
cswap.f
csymm.f
csyr2k.f
csyrk.f
ctbmv.f
ctbsv.f
ctpmv.f
ctpsv.f
ctrmm.f
ctrmv.f
ctrsm.f
ctrsv.f
dasum.f
daxpy.f
dcabs1.f
dcopy.f
ddot.f
dgbmv.f
dgemm.f
dgemv.f
dger.f
drot.f
drotm.f
drotmg.f
dsbmv.f
dscal.f
dsdot.f
dspmv.f
dspr2.f
dspr.f
dswap.f
dsymm.f
dsymv.f
dsyr2.f
dsyr2k.f
dsyr.f
dsyrk.f
dtbmv.f
dtbsv.f
dtpmv.f
dtpsv.f
dtrmm.f
dtrmv.f
dtrsm.f
dtrsv.f
dzasum.f
icamax.f
idamax.f
isamax.f
izamax.f
lsame.f
sasum.f
saxpy.f
scabs1.f
scasum.f
scopy.f
sdot.f
sdsdot.f
sgbmv.f
sgemm.f
sgemv.f
sger.f
srot.f
srotm.f
srotmg.f
ssbmv.f
sscal.f
sspmv.f
sspr2.f
sspr.f
sswap.f
ssymm.f
ssymv.f
ssyr2.f
ssyr2k.f
ssyr.f
ssyrk.f
stbmv.f
stbsv.f
stpmv.f
stpsv.f
strmm.f
strmv.f
strsm.f
strsv.f
xerbla_array.f
xerbla.f
zaxpy.f
zcopy.f
zdotc.f
zdotu.f
zdrot.f
zdscal.f
zgbmv.f
zgemm.f
zgemv.f
zgerc.f
zgeru.f
zhbmv.f
zhemm.f
zhemv.f
zher2.f
zher2k.f
zher.f
zherk.f
zhpmv.f
zhpr2.f
zhpr.f
zscal.f
zswap.f
zsymm.f
zsyr2k.f
zsyrk.f
ztbmv.f
ztbsv.f
ztpmv.f
ztpsv.f
ztrmm.f
ztrmv.f
ztrsm.f
ztrsv.f
    )


if( (CMAKE_Fortran_COMPILER_ID STREQUAL "VisualAge" ) OR  # CMake 2.6
	(CMAKE_Fortran_COMPILER_ID STREQUAL "XL" ) )          # CMake 2.8
	# 	if( "${CMAKE_Fortran_FLAGS}" MATCHES "-qflttrap=[a-zA-Z:]:enable" )
	# 		set( FPE_EXIT TRUE )
	# 	endif()

	if( NOT ("${CMAKE_Fortran_FLAGS}" MATCHES "-qfixed") )
		message( STATUS "BLAS -- Enabling fixed format F90/F95 with -qfixed" )
		set( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -qfixed")
		#         CACHE STRING "Flags for Fortran compiler." FORCE )
	endif()
endif()


# Disable Warings for BLAS
if ( CMAKE_Fortran_COMPILER_ID STREQUAL "GNU" )
	SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wno-conversion -Wno-maybe-uninitialized")
endif()

INCLUDE_DIRECTORIES(${PROJECT_BINARY_DIR}/include)
add_library (flexiblas_netlib_objs OBJECT ${NETLIBSRC})


