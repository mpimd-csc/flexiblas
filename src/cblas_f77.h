/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) Martin Koehler, 2013-2020
 */

#ifndef CBLAS_F77_H
#define CBLAS_F77_H
#include <stdlib.h> 
#include <complex.h> 
#include "fortran_mangle.h"
#include "flexiblas.h"

#ifdef INTEGER8 
	#include <stdint.h>
	#define F77_INT int64_t
#endif

#ifdef  F77_CHAR
   #define FCHAR F77_CHAR
#else
   #define FCHAR char *
#endif

#ifdef F77_INT
   #define FINT const F77_INT *
   #define FINT2 F77_INT *
#else
   #define FINT const int *
   #define FINT2 int *
#endif


#define COPY_CONST_PTR(a,b) memcpy(&a, &b, sizeof(void*))

#ifdef __cplusplus
extern "C" {
#endif

   extern void FC_GLOBAL(xerbla,XERBLA)(FCHAR, void *);
/*
 * Level 1 Fortran Prototypes
 */

/* Single Precision */

   void FC_GLOBAL(srot,SROT)(FINT, float *, FINT, float *, FINT, const float *, const float *);
   void FC_GLOBAL(srotg,SROTG)(float *,float *,float *,float *);    
   void FC_GLOBAL(srotm,SROTM)( FINT, float *, FINT, float *, FINT, const float *);
   void FC_GLOBAL(srotmg,SROTMG)(float *,float *,float *,const float *, float *);
   void FC_GLOBAL(sswap,SSWAP)( FINT, float *, FINT, float *, FINT);
   void FC_GLOBAL(scopy,SCOPY)( FINT, const float *, FINT, float *, FINT);
   void FC_GLOBAL(saxpy,SAXPY)( FINT, const float *, const float *, FINT, float *, FINT);
   float FC_GLOBAL(sdot,SDOT)(FINT, const float *, FINT, const float *, FINT);
   float FC_GLOBAL(sdsdot,SDSDOT)( FINT, const float *, const float *, FINT, const float *, FINT);
   void FC_GLOBAL(sscal,SSCAL)( FINT, const float *, float *, FINT);
   float FC_GLOBAL(snrm2,SNRM2)( FINT, const float *, FINT);
   float FC_GLOBAL(sasum,SASUM)( FINT, const float *, FINT);

/* Double Precision */

   void FC_GLOBAL(drot,DROT)(FINT, double *, FINT, double *, FINT, const double *, const double *);
   void FC_GLOBAL(drotg,DROTG)(double *,double *,double *,double *);    
   void FC_GLOBAL(drotm,DROTM)( FINT, double *, FINT, double *, FINT, const double *);
   void FC_GLOBAL(drotmg,DROTMG)(double *,double *,double *,const double *, double *);
   void FC_GLOBAL(dswap,DSWAP)( FINT, double *, FINT, double *, FINT);
   void FC_GLOBAL(dcopy,DCOPY)( FINT, const double *, FINT, double *, FINT);
   void FC_GLOBAL(daxpy,DAXPY)( FINT, const double *, const double *, FINT, double *, FINT);
   double FC_GLOBAL(dsdot,DSDOT)(FINT, const float *, FINT, const float *, FINT);
   double FC_GLOBAL(ddot,DDOT)( FINT, const double *, FINT, const double *, FINT);
   void FC_GLOBAL(dscal,DSCAL)( FINT, const double *, double *, FINT);
   double FC_GLOBAL(dnrm2,DNRM2)( FINT, const double *, FINT);
   double FC_GLOBAL(dasum,DASUM)( FINT, const double *, FINT);


/* Single Complex Precision */

   void FC_GLOBAL(cswap,CSWAP)( FINT, void *, FINT, void *, FINT);
   void FC_GLOBAL(ccopy,CCOPY)( FINT, const void *, FINT, void *, FINT);
   void FC_GLOBAL(caxpy,CAXPY)( FINT, const void *, const void *, FINT, void *, FINT);
#ifdef FLEXIBLAS_ABI_INTEL
   void  FC_GLOBAL(cdotc,CDOTC)( void *, FINT, const void *, FINT, const void *, FINT);
   void  FC_GLOBAL(cdotu,CDOTU)( void *, FINT, const void *, FINT, const void *, FINT);
#else 
   float complex FC_GLOBAL(cdotc,CDOTC)( FINT, const void *, FINT, const void *, FINT);
   float complex FC_GLOBAL(cdotu,CDOTU)( FINT, const void *, FINT, const void *, FINT);
#endif
   void FC_GLOBAL(cscal,CSCAL)( FINT, const void *, void *, FINT);
   void FC_GLOBAL(csscal,CSSCAL)( FINT, const float *, void *, FINT);
   float FC_GLOBAL(scnrm2,SCNRM2)( FINT, const void *, FINT);
   float FC_GLOBAL(scasum,SCASUM)( FINT, const void *, FINT);

/* Double Complex Precision */

   void FC_GLOBAL(zswap,ZSWAP)( FINT, void *, FINT, void *, FINT);
   void FC_GLOBAL(zcopy,ZCOPY)( FINT, const void *, FINT, void *, FINT);
   void FC_GLOBAL(zaxpy,ZAXPY)( FINT, const void *, const void *, FINT, void *, FINT);
   
#ifdef FLEXIBLAS_ABI_INTEL
   void  FC_GLOBAL(zdotc,ZDOTC)(void * RET, FINT, const void *, FINT, const void *, FINT);
   void  FC_GLOBAL(zdotu,ZDOTU)(void * RET, FINT, const void *, FINT, const void *, FINT);
#else 
   double complex  FC_GLOBAL(zdotc,ZDOTC)( FINT, const void *, FINT, const void *, FINT);
   double complex  FC_GLOBAL(zdotu,ZDOTU)( FINT, const void *, FINT, const void *, FINT);
#endif 
   
   void FC_GLOBAL(zdscal,ZDSCAL)( FINT, const double *, void *, FINT);
   void FC_GLOBAL(zscal,ZSCAL)( FINT, const void *, void *, FINT);
   double FC_GLOBAL(dznrm2,DZNRM2)( FINT, const void *, FINT);
   double FC_GLOBAL(dzasum,DZASUM)( FINT, const void *, FINT);


   int FC_GLOBAL(isamax,ISAMAX)( FINT, const float * , FINT);
   int FC_GLOBAL(idamax,IDAMAX)( FINT, const double * , FINT);
   int FC_GLOBAL(icamax,ICAMAX)( FINT, const void *, FINT);
   int FC_GLOBAL(izamax,IZAMAX)( FINT, const void *, FINT);

/*
 * Level 2 Fortran Prototypes
 */

/* Single Precision */

   void FC_GLOBAL(sgemv,SGEMV)(FCHAR, FINT, FINT, const float *, const float *, FINT, const float *, FINT, const float *, float *, FINT);
   void FC_GLOBAL(sgbmv,SGBMV)(FCHAR, FINT, FINT, FINT, FINT, const float *,  const float *, FINT, const float *, FINT, const float *, float *, FINT);
   void FC_GLOBAL(ssymv,SSYMV)(FCHAR, FINT, const float *, const float *, FINT, const float *,  FINT, const float *, float *, FINT);
   void FC_GLOBAL(ssbmv,SSBMV)(FCHAR, FINT, FINT, const float *, const float *, FINT, const float *, FINT, const float *, float *, FINT);
   void FC_GLOBAL(sspmv,SSPMV)(FCHAR, FINT, const float *, const float *, const float *, FINT, const float *, float *, FINT);
   void FC_GLOBAL(strmv,STRMV)( FCHAR, FCHAR, FCHAR, FINT, const float *, FINT, float *, FINT);
   void FC_GLOBAL(stbmv,STBMV)( FCHAR, FCHAR, FCHAR, FINT, FINT, const float *, FINT, float *, FINT);
   void FC_GLOBAL(strsv,STRSV)( FCHAR, FCHAR, FCHAR, FINT, const float *, FINT, float *, FINT);
   void FC_GLOBAL(stbsv,STBSV)( FCHAR, FCHAR, FCHAR, FINT, FINT, const float *, FINT, float *, FINT);
   void FC_GLOBAL(stpmv,STPMV)( FCHAR, FCHAR, FCHAR, FINT, const float *, float *, FINT);
   void FC_GLOBAL(stpsv,STPSV)( FCHAR, FCHAR, FCHAR, FINT, const float *, float *, FINT);
   void FC_GLOBAL(sger,SGER)( FINT, FINT, const float *, const float *, FINT, const float *, FINT, float *, FINT);
   void FC_GLOBAL(ssyr,SSYR)(FCHAR, FINT, const float *, const float *, FINT, float *, FINT);
   void FC_GLOBAL(sspr,SSPR)(FCHAR, FINT, const float *, const float *, FINT, float *); 
   void FC_GLOBAL(sspr2,SSPR2)(FCHAR, FINT, const float *, const float *, FINT, const float *, FINT,  float *); 
   void FC_GLOBAL(ssyr2,SSYR2)(FCHAR, FINT, const float *, const float *, FINT, const float *, FINT,  float *, FINT);

/* Double Precision */

   void FC_GLOBAL(dgemv,DGEMV)(FCHAR, FINT, FINT, const double *, const double *, FINT, const double *, FINT, const double *, double *, FINT);
   void FC_GLOBAL(dgbmv,DGBMV)(FCHAR, FINT, FINT, FINT, FINT, const double *,  const double *, FINT, const double *, FINT, const double *, double *, FINT);
   void FC_GLOBAL(dsymv,DSYMV)(FCHAR, FINT, const double *, const double *, FINT, const double *,  FINT, const double *, double *, FINT);
   void FC_GLOBAL(dsbmv,DSBMV)(FCHAR, FINT, FINT, const double *, const double *, FINT, const double *, FINT, const double *, double *, FINT);
   void FC_GLOBAL(dspmv,DSPMV)(FCHAR, FINT, const double *, const double *, const double *, FINT, const double *, double *, FINT);
   void FC_GLOBAL(dtrmv,DTRMV)( FCHAR, FCHAR, FCHAR, FINT, const double *, FINT, double *, FINT);
   void FC_GLOBAL(dtbmv,DTBMV)( FCHAR, FCHAR, FCHAR, FINT, FINT, const double *, FINT, double *, FINT);
   void FC_GLOBAL(dtrsv,DTRSV)( FCHAR, FCHAR, FCHAR, FINT, const double *, FINT, double *, FINT);
   void FC_GLOBAL(dtbsv,DTBSV)( FCHAR, FCHAR, FCHAR, FINT, FINT, const double *, FINT, double *, FINT);
   void FC_GLOBAL(dtpmv,DTPMV)( FCHAR, FCHAR, FCHAR, FINT, const double *, double *, FINT);
   void FC_GLOBAL(dtpsv,DTPSV)( FCHAR, FCHAR, FCHAR, FINT, const double *, double *, FINT);
   void FC_GLOBAL(dger,DGER)( FINT, FINT, const double *, const double *, FINT, const double *, FINT, double *, FINT);
   void FC_GLOBAL(dsyr,DSYR)(FCHAR, FINT, const double *, const double *, FINT, double *, FINT);
   void FC_GLOBAL(dspr,DSPR)(FCHAR, FINT, const double *, const double *, FINT, double *); 
   void FC_GLOBAL(dspr2,DSPR2)(FCHAR, FINT, const double *, const double *, FINT, const double *, FINT,  double *); 
   void FC_GLOBAL(dsyr2,DSYR2)(FCHAR, FINT, const double *, const double *, FINT, const double *, FINT,  double *, FINT);

/* Single Complex Precision */

   void FC_GLOBAL(cgemv,CGEMV)(FCHAR, FINT, FINT, const void *, const void *, FINT, const void *, FINT, const void *, void *, FINT);
   void FC_GLOBAL(cgbmv,CGBMV)(FCHAR, FINT, FINT, FINT, FINT, const void *,  const void *, FINT, const void *, FINT, const void *, void *, FINT);
   void FC_GLOBAL(chemv,CHEMV)(FCHAR, FINT, const void *, const void *, FINT, const void *, FINT, const void *, void *, FINT);
   void FC_GLOBAL(chbmv,CHBMV)(FCHAR, FINT, FINT, const void *, const void *, FINT, const void *, FINT, const void *, void *, FINT);
   void FC_GLOBAL(chpmv,CHPMV)(FCHAR, FINT, const void *, const void *, const void *, FINT, const void *, void *, FINT);
   void FC_GLOBAL(ctrmv,CTRMV)( FCHAR, FCHAR, FCHAR, FINT, const void *, FINT, void *, FINT);
   void FC_GLOBAL(ctbmv,CTBMV)( FCHAR, FCHAR, FCHAR, FINT, FINT, const void *, FINT, void *, FINT);
   void FC_GLOBAL(ctpmv,CTPMV)( FCHAR, FCHAR, FCHAR, FINT, const void *, void *, FINT);
   void FC_GLOBAL(ctrsv,CTRSV)( FCHAR, FCHAR, FCHAR, FINT, const void *, FINT, void *, FINT);
   void FC_GLOBAL(ctbsv,CTBSV)( FCHAR, FCHAR, FCHAR, FINT, FINT, const void *, FINT, void *, FINT);
   void FC_GLOBAL(ctpsv,CTPSV)( FCHAR, FCHAR, FCHAR, FINT, const void *, void *,FINT);
   void FC_GLOBAL(cgerc,CGERC)( FINT, FINT, const void *, const void *, FINT, const void *, FINT, void *, FINT);
   void FC_GLOBAL(cgeru,CGERU)( FINT, FINT, const void *, const void *, FINT, const void *, FINT, void *,  FINT);
   void FC_GLOBAL(cher,CHER)(FCHAR, FINT, const float *, const void *, FINT, void *, FINT);
   void FC_GLOBAL(cher2,CHER2)(FCHAR, FINT, const void *, const void *, FINT, const void *, FINT, void *, FINT);
   void FC_GLOBAL(chpr,CHPR)(FCHAR, FINT, const float *, const void *, FINT, void *);
   void FC_GLOBAL(chpr2,CHPR2)(FCHAR, FINT, const float *, const void *, FINT, const void *, FINT, void *);

/* Double Complex Precision */

   void FC_GLOBAL(zgemv,ZGEMV)(FCHAR, FINT, FINT, const void *, const void *, FINT, const void *, FINT, const void *, void *, FINT);
   void FC_GLOBAL(zgbmv,ZGBMV)(FCHAR, FINT, FINT, FINT, FINT, const void *,  const void *, FINT, const void *, FINT, const void *, void *, FINT);
   void FC_GLOBAL(zhemv,ZHEMV)(FCHAR, FINT, const void *, const void *, FINT, const void *, FINT, const void *, void *, FINT);
   void FC_GLOBAL(zhbmv,ZHBMV)(FCHAR, FINT, FINT, const void *, const void *, FINT, const void *, FINT, const void *, void *, FINT);
   void FC_GLOBAL(zhpmv,ZHPMV)(FCHAR, FINT, const void *, const void *, const void *, FINT, const void *, void *, FINT);
   void FC_GLOBAL(ztrmv,ZTRMV)( FCHAR, FCHAR, FCHAR, FINT, const void *, FINT, void *, FINT);
   void FC_GLOBAL(ztbmv,ZTBMV)( FCHAR, FCHAR, FCHAR, FINT, FINT, const void *, FINT, void *, FINT);
   void FC_GLOBAL(ztpmv,ZTPMV)( FCHAR, FCHAR, FCHAR, FINT, const void *, void *, FINT);
   void FC_GLOBAL(ztrsv,ZTRSV)( FCHAR, FCHAR, FCHAR, FINT, const void *, FINT, void *, FINT);
   void FC_GLOBAL(ztbsv,ZTBSV)( FCHAR, FCHAR, FCHAR, FINT, FINT, const void *, FINT, void *, FINT);
   void FC_GLOBAL(ztpsv,ZTPSV)( FCHAR, FCHAR, FCHAR, FINT, const void *, void *,FINT);
   void FC_GLOBAL(zgerc,ZGERC)( FINT, FINT, const void *, const void *, FINT, const void *, FINT, void *, FINT);
   void FC_GLOBAL(zgeru,ZGERU)( FINT, FINT, const void *, const void *, FINT, const void *, FINT, void *,  FINT);
   void FC_GLOBAL(zher,ZHER)(FCHAR, FINT, const double *, const void *, FINT, void *, FINT);
   void FC_GLOBAL(zher2,ZHER2)(FCHAR, FINT, const void *, const void *, FINT, const void *, FINT, void *, FINT);
   void FC_GLOBAL(zhpr,ZHPR)(FCHAR, FINT, const double *, const void *, FINT, void *);
   void FC_GLOBAL(zhpr2,ZHPR2)(FCHAR, FINT, const double *, const void *, FINT, const void *, FINT, void *);

/*
 * Level 3 Fortran Prototypes
 */

/* Single Precision */

   void FC_GLOBAL(sgemm,SGEMM)(FCHAR, FCHAR, FINT, FINT, FINT, const float *, const float *, FINT, const float *, FINT, const float *, float *, FINT);
   void FC_GLOBAL(ssymm,SSYMM)(FCHAR, FCHAR, FINT, FINT, const float *, const float *, FINT, const float *, FINT, const float *, float *, FINT);
   void FC_GLOBAL(ssyrk,SSYRK)(FCHAR, FCHAR, FINT, FINT, const float *, const float *, FINT, const float *, float *, FINT);
   void FC_GLOBAL(ssyr2k,SSYR2K)(FCHAR, FCHAR, FINT, FINT, const float *, const float *, FINT, const float *, FINT, const float *, float *, FINT);
   void FC_GLOBAL(strmm,STRMM)(FCHAR, FCHAR, FCHAR, FCHAR, FINT, FINT, const float *, const float *, FINT, float *, FINT);
   void FC_GLOBAL(strsm,STRSM)(FCHAR, FCHAR, FCHAR, FCHAR, FINT, FINT, const float *, const float *, FINT, float *, FINT);

/* Double Precision */

   void FC_GLOBAL(dgemm,DGEMM)(FCHAR, FCHAR, FINT, FINT, FINT, const double *, const double *, FINT, const double *, FINT, const double *, double *, FINT);
   void FC_GLOBAL(dsymm,DSYMM)(FCHAR, FCHAR, FINT, FINT, const double *, const double *, FINT, const double *, FINT, const double *, double *, FINT);
   void FC_GLOBAL(dsyrk,DSYRK)(FCHAR, FCHAR, FINT, FINT, const double *, const double *, FINT, const double *, double *, FINT);
   void FC_GLOBAL(dsyr2k,DSYR2K)(FCHAR, FCHAR, FINT, FINT, const double *, const double *, FINT, const double *, FINT, const double *, double *, FINT);
   void FC_GLOBAL(dtrmm,DTRMM)(FCHAR, FCHAR, FCHAR, FCHAR, FINT, FINT, const double *, const double *, FINT, double *, FINT);
   void FC_GLOBAL(dtrsm,DTRSM)(FCHAR, FCHAR, FCHAR, FCHAR, FINT, FINT, const double *, const double *, FINT, double *, FINT);

/* Single Complex Precision */

   void FC_GLOBAL(cgemm,CGEMM)(FCHAR, FCHAR, FINT, FINT, FINT, const float *, const float *, FINT, const float *, FINT, const float *, float *, FINT);
   void FC_GLOBAL(csymm,CSYMM)(FCHAR, FCHAR, FINT, FINT, const float *, const float *, FINT, const float *, FINT, const float *, float *, FINT);
   void FC_GLOBAL(chemm,CHEMM)(FCHAR, FCHAR, FINT, FINT, const float *, const float *, FINT, const float *, FINT, const float *, float *, FINT);
   void FC_GLOBAL(csyrk,CSYRK)(FCHAR, FCHAR, FINT, FINT, const float *, const float *, FINT, const float *, float *, FINT);
   void FC_GLOBAL(cherk,CHERK)(FCHAR, FCHAR, FINT, FINT, const float *, const float *, FINT, const float *, float *, FINT);
   void FC_GLOBAL(csyr2k,CSYR2K)(FCHAR, FCHAR, FINT, FINT, const float *, const float *, FINT, const float *, FINT, const float *, float *, FINT);
   void FC_GLOBAL(cher2k,CHER2K)(FCHAR, FCHAR, FINT, FINT, const float *, const float *, FINT, const float *, FINT, const float *, float *, FINT);
   void FC_GLOBAL(ctrmm,CTRMM)(FCHAR, FCHAR, FCHAR, FCHAR, FINT, FINT, const float *, const float *, FINT, float *, FINT);
   void FC_GLOBAL(ctrsm,CTRSM)(FCHAR, FCHAR, FCHAR, FCHAR, FINT, FINT, const float *, const float *, FINT, float *, FINT);

/* Double Complex Precision */

   void FC_GLOBAL(zgemm,ZGEMM)(FCHAR, FCHAR, FINT, FINT, FINT, const double *, const double *, FINT, const double *, FINT, const double *, double *, FINT);
   void FC_GLOBAL(zsymm,ZSYMM)(FCHAR, FCHAR, FINT, FINT, const double *, const double *, FINT, const double *, FINT, const double *, double *, FINT);
   void FC_GLOBAL(zhemm,ZHEMM)(FCHAR, FCHAR, FINT, FINT, const double *, const double *, FINT, const double *, FINT, const double *, double *, FINT);
   void FC_GLOBAL(zsyrk,ZSYRK)(FCHAR, FCHAR, FINT, FINT, const double *, const double *, FINT, const double *, double *, FINT);
   void FC_GLOBAL(zherk,ZHERK)(FCHAR, FCHAR, FINT, FINT, const double *, const double *, FINT, const double *, double *, FINT);
   void FC_GLOBAL(zsyr2k,ZSYR2K)(FCHAR, FCHAR, FINT, FINT, const double *, const double *, FINT, const double *, FINT, const double *, double *, FINT);
   void FC_GLOBAL(zher2k,ZHER2K)(FCHAR, FCHAR, FINT, FINT, const double *, const double *, FINT, const double *, FINT, const double *, double *, FINT);
   void FC_GLOBAL(ztrmm,ZTRMM)(FCHAR, FCHAR, FCHAR, FCHAR, FINT, FINT, const double *, const double *, FINT, double *, FINT);
   void FC_GLOBAL(ztrsm,ZTRSM)(FCHAR, FCHAR, FCHAR, FCHAR, FINT, FINT, const double *, const double *, FINT, double *, FINT);


/* BLAS Extensions  */
   void FC_GLOBAL(saxpby,SAXPBY)( FINT, const float *, const float *,  FINT, const float *, float *, FINT);
   void FC_GLOBAL(daxpby,DAXPBY)( FINT, const double*, const double *, FINT, const double *,double*, FINT);
   void FC_GLOBAL(caxpby,CAXPBY)( FINT, const float *, const void *, FINT, const void *, void*, FINT);
   void FC_GLOBAL(zaxpby,ZAXPBY)( FINT, const float *, const void *, FINT, const void *, void*, FINT);

   void FC_GLOBAL(somatcopy,SOMATCOPY)(FCHAR, FCHAR, FINT, FINT, const float * , const float*, FINT, float *, FINT); 
   void FC_GLOBAL(domatcopy,DOMATCOPY)(FCHAR, FCHAR, FINT, FINT, const double * , const double*, FINT, double *, FINT); 
   void FC_GLOBAL(comatcopy,COMATCOPY)(FCHAR, FCHAR, FINT, FINT, const void * , const void*, FINT, void *, FINT); 
   void FC_GLOBAL(zomatcopy,ZOMATCOPY)(FCHAR, FCHAR, FINT, FINT, const void * , const void*, FINT, void *, FINT); 

   void FC_GLOBAL(simatcopy,SIMATCOPY)(FCHAR, FCHAR, FINT, FINT, const float * , float*, FINT, FINT); 
   void FC_GLOBAL(dimatcopy,DIMATCOPY)(FCHAR, FCHAR, FINT, FINT, const double * ,double*, FINT, FINT); 
   void FC_GLOBAL(cimatcopy,CIMATCOPY)(FCHAR, FCHAR, FINT, FINT, const void * , void*, FINT, FINT); 
   void FC_GLOBAL(zimatcopy,ZIMATCOPY)(FCHAR, FCHAR, FINT, FINT, const void * , void*, FINT, FINT); 

   void FC_GLOBAL(sgeadd,SGEADD)(FINT, FINT, const float *, float *, FINT, const float *, float *, FINT); 
   void FC_GLOBAL(dgeadd,DGEADD)(FINT, FINT, const double *, double *, FINT, const double *, double *, FINT); 
   void FC_GLOBAL(cgeadd,CGEADD)(FINT, FINT, const float complex *, float complex *, FINT, const float complex *, float complex *, FINT); 
   void FC_GLOBAL(zgeadd,ZGEADD)(FINT, FINT, const double complex *, double complex *, FINT, const double complex *, double complex *, FINT); 




#ifdef __cplusplus
}
#endif

#endif /*  CBLAS_F77_H */
