SET (NETLIBSRC
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
crotg.f
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
dnrm2.f
drot.f
drotg.f
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
dznrm2.f
ffiles
icamax.f
idamax.f
isamax.f
izamax.f
make.inc
sasum.f
saxpy.f
scabs1.f
scasum.f
scnrm2.f
scopy.f
sdot.f
sdsdot.f
sgbmv.f
sgemm.f
sgemv.f
sger.f
snrm2.f
srot.f
srotg.f
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
zrotg.f
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
lsame.f
info.c
	)

IF ( ${CMAKE_C_COMPILER_ID} STREQUAL "GNU") 
	MESSAGE(STATUS "Setting GNU Compiler Flags")
	SET(I8_FLAGS "-fdefault-integer-8 -frecursive")
ELSEIF (${CMAKE_C_COMPILER_ID} STREQUAL "Intel" ) 
	SET ( I8_FLAGS "-i8 -recursive") 
ENDIF()

IF ( INTEGER8 STREQUAL ON ) 
	SET (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${I8_FLAGS}")
	ADD_DEFINITIONS(-DINTEGER8)
ENDIF()


add_library (blas_netlib SHARED ${NETLIBSRC})

IF(INTEGER8 STREQUAL OFF AND NETLIB64 STREQUAL ON) 
	add_library (blas_netlib64 SHARED ${NETLIBSRC}) 
	set_target_properties(blas_netlib64 PROPERTIES COMPILE_FLAGS "${I8_FLAGS} -DINTEGER8") 
	install(TARGETS blas_netlib64 LIBRARY DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR}/flexiblas)
ENDIF()

if (WIN32) 
	install(TARGETS blas_netlib EXPORT flexiblas-targets LIBRARY DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR}/${FLEXIBLAS_LIBRARY_DIR} RUNTIME DESTINATION ${CMAKE_INSTALL_FULL_BINDIR})
else ()
	install(TARGETS blas_netlib LIBRARY DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR}/${FLEXIBLAS_LIBRARY_DIR})
endif()

