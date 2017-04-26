/* $Id$ */
/* 
 Copyright (C) 2013  Martin Köhler, koehlerm@mpi-magdeburg.mpg.de

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/
#ifndef HOOKS_H 
#define HOOKS_H

#include "flexiblas_config.h"
#include <complex.h> 
#ifndef INTEGER8 
#define Int 	int
#else 
#include <stdint.h>
#define Int 	int64_t
#endif

#define COLOR_RED "\033[1;2;31m"
#define COLOR_RESET "\033[0m"


#define PRINT_PREFIX "<flexiblas> "

/*-----------------------------------------------------------------------------
 *  Global Handle for BLAS Libary 
 *-----------------------------------------------------------------------------*/
struct flexiblas_blasfn {
	void *call_fblas;
	void *call_cblas; 
};

struct flexiblas_info {
	int zdotc_is_intel;
	int zdotu_is_intel; 
	int cdotc_is_intel;
	int cdotu_is_intel; 
	int scabs1_missing; 
};

extern  struct flexiblas_info __flexiblas_current_blas; 
typedef void (*flexiblas_info_function)(struct flexiblas_info *); 
extern int __flexiblas_verbose; 

#define HOOK_INIT {NULL,NULL} 

/*-----------------------------------------------------------------------------
 *  double precission code
zR *-----------------------------------------------------------------------------*/
struct flexiblas_blasfn flexiblas_dasum;
struct flexiblas_blasfn flexiblas_dcabs1; 
struct flexiblas_blasfn flexiblas_dgbmv;
struct flexiblas_blasfn flexiblas_drot;
struct flexiblas_blasfn flexiblas_dscal;
struct flexiblas_blasfn flexiblas_dswap;
struct flexiblas_blasfn flexiblas_dsyr;
struct flexiblas_blasfn flexiblas_dtpsv;
struct flexiblas_blasfn flexiblas_dzasum;
struct flexiblas_blasfn flexiblas_daxpy;
struct flexiblas_blasfn flexiblas_dgemm;
struct flexiblas_blasfn flexiblas_drotg;
struct flexiblas_blasfn flexiblas_dsdot;
struct flexiblas_blasfn flexiblas_dsymm;
struct flexiblas_blasfn flexiblas_dsyrk;
struct flexiblas_blasfn flexiblas_dtrmm;
struct flexiblas_blasfn flexiblas_dzrnm2;
struct flexiblas_blasfn flexiblas_dgemv;
struct flexiblas_blasfn flexiblas_drotm;
struct flexiblas_blasfn flexiblas_dspmv;
struct flexiblas_blasfn flexiblas_dsymv;
struct flexiblas_blasfn flexiblas_dtbmv;
struct flexiblas_blasfn flexiblas_dtrmv;
struct flexiblas_blasfn flexiblas_dcopy;
struct flexiblas_blasfn flexiblas_dger;
struct flexiblas_blasfn flexiblas_drotmg;
struct flexiblas_blasfn flexiblas_dspr2;
struct flexiblas_blasfn flexiblas_dsyr2;
struct flexiblas_blasfn flexiblas_dtbsv;
struct flexiblas_blasfn flexiblas_dtrsm;
struct flexiblas_blasfn flexiblas_ddot;
struct flexiblas_blasfn flexiblas_dnrm2;
struct flexiblas_blasfn flexiblas_dsbmv;
struct flexiblas_blasfn flexiblas_dspr;
struct flexiblas_blasfn flexiblas_dsyr2k;
struct flexiblas_blasfn flexiblas_dtpmv;
struct flexiblas_blasfn flexiblas_dtrsv;
struct flexiblas_blasfn flexiblas_dznrm2;

/*-----------------------------------------------------------------------------
 *  	single precision 
 *-----------------------------------------------------------------------------*/
struct flexiblas_blasfn flexiblas_sasum;
struct flexiblas_blasfn flexiblas_saxpy;
struct flexiblas_blasfn flexiblas_scabs1;
struct flexiblas_blasfn flexiblas_scasum;
struct flexiblas_blasfn flexiblas_scnrm2;
struct flexiblas_blasfn flexiblas_scopy;
struct flexiblas_blasfn flexiblas_sdot;
struct flexiblas_blasfn flexiblas_sdsdot;
struct flexiblas_blasfn flexiblas_sgbmv;
struct flexiblas_blasfn flexiblas_sgemm;
struct flexiblas_blasfn flexiblas_sgemv;
struct flexiblas_blasfn flexiblas_sger;
struct flexiblas_blasfn flexiblas_snrm2;
struct flexiblas_blasfn flexiblas_srot;
struct flexiblas_blasfn flexiblas_srotg;
struct flexiblas_blasfn flexiblas_srotm;
struct flexiblas_blasfn flexiblas_srotmg;
struct flexiblas_blasfn flexiblas_ssbmv;
struct flexiblas_blasfn flexiblas_sscal;
struct flexiblas_blasfn flexiblas_sspmv;
struct flexiblas_blasfn flexiblas_sspr2;
struct flexiblas_blasfn flexiblas_sspr;
struct flexiblas_blasfn flexiblas_sswap;
struct flexiblas_blasfn flexiblas_ssymm;
struct flexiblas_blasfn flexiblas_ssymv;
struct flexiblas_blasfn flexiblas_ssyr2;
struct flexiblas_blasfn flexiblas_ssyr2k;
struct flexiblas_blasfn flexiblas_ssyr;
struct flexiblas_blasfn flexiblas_ssyrk;
struct flexiblas_blasfn flexiblas_stbmv;
struct flexiblas_blasfn flexiblas_stbsv;
struct flexiblas_blasfn flexiblas_stpmv;
struct flexiblas_blasfn flexiblas_stpsv;
struct flexiblas_blasfn flexiblas_strmm;
struct flexiblas_blasfn flexiblas_strmv;
struct flexiblas_blasfn flexiblas_strsm;
struct flexiblas_blasfn flexiblas_strsv;

/*-----------------------------------------------------------------------------
 *  Complex * 8
 *-----------------------------------------------------------------------------*/
struct flexiblas_blasfn flexiblas_caxpy;
struct flexiblas_blasfn flexiblas_ccopy;
struct flexiblas_blasfn flexiblas_cdotc;
struct flexiblas_blasfn flexiblas_cdotu;
struct flexiblas_blasfn flexiblas_cgbmv;
struct flexiblas_blasfn flexiblas_cgemm;
struct flexiblas_blasfn flexiblas_cgemv;
struct flexiblas_blasfn flexiblas_cgerc;
struct flexiblas_blasfn flexiblas_cgeru;
struct flexiblas_blasfn flexiblas_chbmv;
struct flexiblas_blasfn flexiblas_chemm;
struct flexiblas_blasfn flexiblas_chemv;
struct flexiblas_blasfn flexiblas_cher2;
struct flexiblas_blasfn flexiblas_cher2k;
struct flexiblas_blasfn flexiblas_cher;
struct flexiblas_blasfn flexiblas_cherk;
struct flexiblas_blasfn flexiblas_chpmv;
struct flexiblas_blasfn flexiblas_chpr2;
struct flexiblas_blasfn flexiblas_chpr;
struct flexiblas_blasfn flexiblas_crotg;
struct flexiblas_blasfn flexiblas_cscal;
struct flexiblas_blasfn flexiblas_csrot;
struct flexiblas_blasfn flexiblas_csscal;
struct flexiblas_blasfn flexiblas_cswap;
struct flexiblas_blasfn flexiblas_csymm;
struct flexiblas_blasfn flexiblas_csyr2k;
struct flexiblas_blasfn flexiblas_csyrk;
struct flexiblas_blasfn flexiblas_ctbmv;
struct flexiblas_blasfn flexiblas_ctbsv;
struct flexiblas_blasfn flexiblas_ctpmv;
struct flexiblas_blasfn flexiblas_ctpsv;
struct flexiblas_blasfn flexiblas_ctrmm;
struct flexiblas_blasfn flexiblas_ctrmv;
struct flexiblas_blasfn flexiblas_ctrsm;
struct flexiblas_blasfn flexiblas_ctrsv;

/*-----------------------------------------------------------------------------
 *  Complex * 16
 *-----------------------------------------------------------------------------*/
struct flexiblas_blasfn flexiblas_zaxpy;
struct flexiblas_blasfn flexiblas_zcopy;
struct flexiblas_blasfn flexiblas_zdotc;
struct flexiblas_blasfn flexiblas_zdotu;
struct flexiblas_blasfn flexiblas_zdrot;
struct flexiblas_blasfn flexiblas_zdscal;
struct flexiblas_blasfn flexiblas_zgbmv;
struct flexiblas_blasfn flexiblas_zgemm;
struct flexiblas_blasfn flexiblas_zgemv;
struct flexiblas_blasfn flexiblas_zgerc;
struct flexiblas_blasfn flexiblas_zgeru;
struct flexiblas_blasfn flexiblas_zhbmv;
struct flexiblas_blasfn flexiblas_zhemm;
struct flexiblas_blasfn flexiblas_zhemv;
struct flexiblas_blasfn flexiblas_zher2;
struct flexiblas_blasfn flexiblas_zher2k;
struct flexiblas_blasfn flexiblas_zher;
struct flexiblas_blasfn flexiblas_zherk;
struct flexiblas_blasfn flexiblas_zhpmv;
struct flexiblas_blasfn flexiblas_zhpr2;
struct flexiblas_blasfn flexiblas_zhpr;
struct flexiblas_blasfn flexiblas_zrotg;
struct flexiblas_blasfn flexiblas_zscal;
struct flexiblas_blasfn flexiblas_zswap;
struct flexiblas_blasfn flexiblas_zsymm;
struct flexiblas_blasfn flexiblas_zsyr2k;
struct flexiblas_blasfn flexiblas_zsyrk;
struct flexiblas_blasfn flexiblas_ztbmv;
struct flexiblas_blasfn flexiblas_ztbsv;
struct flexiblas_blasfn flexiblas_ztpmv;
struct flexiblas_blasfn flexiblas_ztpsv;
struct flexiblas_blasfn flexiblas_ztrmm;
struct flexiblas_blasfn flexiblas_ztrmv;
struct flexiblas_blasfn flexiblas_ztrsm;
struct flexiblas_blasfn flexiblas_ztrsv;

/*-----------------------------------------------------------------------------
 *	Integer code 
 *-----------------------------------------------------------------------------*/
struct flexiblas_blasfn flexiblas_icamax;
struct flexiblas_blasfn flexiblas_idamax;
struct flexiblas_blasfn flexiblas_isamax;
struct flexiblas_blasfn flexiblas_izamax;
struct flexiblas_blasfn flexiblas_xerbla; 

/*-----------------------------------------------------------------------------
 *  Hook functions 
 *-----------------------------------------------------------------------------*/
int __flexiblas_hook_double(void *handle); 
int __flexiblas_hook_single(void * handle); 
int __flexiblas_hook_complex(void *handle); 
int __flexiblas_hook_complex16(void *handle); 
int __flexiblas_hook_integer(void *handle); 

/*-----------------------------------------------------------------------------
 *  other functions
 *-----------------------------------------------------------------------------*/
int __flexiblas_loadhook(void *handle, const char *symbol, const char *csymbol, struct flexiblas_blasfn * fn); 


#ifdef FLEXIBLAS_PROFILE
/*-----------------------------------------------------------------------------
 *  Variables for Profiling 
 *-----------------------------------------------------------------------------*/
// Complex Single Precission 
#define POS_FBLAS 0
#define POS_CBLAS 1
double flexiblas_time_caxpy [2];
double flexiblas_time_ccopy [2];
double flexiblas_time_cdotc [2];
double flexiblas_time_cdotu [2];
double flexiblas_time_cgbmv [2];
double flexiblas_time_cgemm [2];
double flexiblas_time_cgemv [2];
double flexiblas_time_cgerc [2];
double flexiblas_time_cgeru [2];
double flexiblas_time_chbmv [2];
double flexiblas_time_chemm [2];
double flexiblas_time_chemv [2];
double flexiblas_time_cher2 [2];
double flexiblas_time_cher2k[2];
double flexiblas_time_cher  [2];
double flexiblas_time_cherk [2];
double flexiblas_time_chpmv [2];
double flexiblas_time_chpr2 [2];
double flexiblas_time_chpr  [2];
double flexiblas_time_crotg [2];
double flexiblas_time_cscal [2];
double flexiblas_time_csrot [2];
double flexiblas_time_csscal[2];
double flexiblas_time_cswap [2];
double flexiblas_time_csymm [2];
double flexiblas_time_csyr2k[2];
double flexiblas_time_csyrk [2];
double flexiblas_time_ctbmv [2];
double flexiblas_time_ctbsv [2];
double flexiblas_time_ctpmv [2];
double flexiblas_time_ctpsv [2];
double flexiblas_time_ctrmm [2];
double flexiblas_time_ctrmv [2];
double flexiblas_time_ctrsm [2];
double flexiblas_time_ctrsv [2];
unsigned long  flexiblas_call_caxpy [2];
unsigned long  flexiblas_call_ccopy [2];
unsigned long  flexiblas_call_cdotc [2];
unsigned long  flexiblas_call_cdotu [2];
unsigned long  flexiblas_call_cgbmv [2];
unsigned long  flexiblas_call_cgemm [2];
unsigned long  flexiblas_call_cgemv [2];
unsigned long  flexiblas_call_cgerc [2];
unsigned long  flexiblas_call_cgeru [2];
unsigned long  flexiblas_call_chbmv [2];
unsigned long  flexiblas_call_chemm [2];
unsigned long  flexiblas_call_chemv [2];
unsigned long  flexiblas_call_cher2 [2];
unsigned long  flexiblas_call_cher2k[2];
unsigned long  flexiblas_call_cher  [2];
unsigned long  flexiblas_call_cherk [2];
unsigned long  flexiblas_call_chpmv [2];
unsigned long  flexiblas_call_chpr2 [2];
unsigned long  flexiblas_call_chpr  [2];
unsigned long  flexiblas_call_crotg [2];
unsigned long  flexiblas_call_cscal [2];
unsigned long  flexiblas_call_csrot [2];
unsigned long  flexiblas_call_csscal[2];
unsigned long  flexiblas_call_cswap [2];
unsigned long  flexiblas_call_csymm [2];
unsigned long  flexiblas_call_csyr2k[2];
unsigned long  flexiblas_call_csyrk [2];
unsigned long  flexiblas_call_ctbmv [2];
unsigned long  flexiblas_call_ctbsv [2];
unsigned long  flexiblas_call_ctpmv [2];
unsigned long  flexiblas_call_ctpsv [2];
unsigned long  flexiblas_call_ctrmm [2];
unsigned long  flexiblas_call_ctrmv [2];
unsigned long  flexiblas_call_ctrsm [2];
unsigned long  flexiblas_call_ctrsv [2];

// Complex Double Precission
double  flexiblas_time_zaxpy [2];
double  flexiblas_time_zcopy [2];
double  flexiblas_time_zdotc [2];
double  flexiblas_time_zdotu [2];
double  flexiblas_time_zdrot [2];
double  flexiblas_time_zdscal[2];
double  flexiblas_time_zgbmv [2];
double  flexiblas_time_zgemm [2];
double  flexiblas_time_zgemv [2];
double  flexiblas_time_zgerc [2];
double  flexiblas_time_zgeru [2];
double  flexiblas_time_zhbmv [2];
double  flexiblas_time_zhemm [2];
double  flexiblas_time_zhemv [2];
double  flexiblas_time_zher2 [2];
double  flexiblas_time_zher2k[2];
double  flexiblas_time_zher  [2];
double  flexiblas_time_zherk [2];
double  flexiblas_time_zhpmv [2];
double  flexiblas_time_zhpr2 [2];
double  flexiblas_time_zhpr  [2];
double  flexiblas_time_zrotg [2];
double  flexiblas_time_zscal [2];
double  flexiblas_time_zswap [2];
double  flexiblas_time_zsymm [2];
double  flexiblas_time_zsyr2k[2];
double  flexiblas_time_zsyrk [2];
double  flexiblas_time_ztbmv [2];
double  flexiblas_time_ztbsv [2];
double  flexiblas_time_ztpmv [2];
double  flexiblas_time_ztpsv [2];
double  flexiblas_time_ztrmm [2];
double  flexiblas_time_ztrmv [2];
double  flexiblas_time_ztrsm [2];
double  flexiblas_time_ztrsv [2];
unsigned long flexiblas_call_zaxpy [2];
unsigned long flexiblas_call_zcopy [2];
unsigned long flexiblas_call_zdotc [2];
unsigned long flexiblas_call_zdotu [2];
unsigned long flexiblas_call_zdrot [2];
unsigned long flexiblas_call_zdscal[2];
unsigned long flexiblas_call_zgbmv [2];
unsigned long flexiblas_call_zgemm [2];
unsigned long flexiblas_call_zgemv [2];
unsigned long flexiblas_call_zgerc [2];
unsigned long flexiblas_call_zgeru [2];
unsigned long flexiblas_call_zhbmv [2];
unsigned long flexiblas_call_zhemm [2];
unsigned long flexiblas_call_zhemv [2];
unsigned long flexiblas_call_zher2 [2];
unsigned long flexiblas_call_zher2k[2];
unsigned long flexiblas_call_zher  [2];
unsigned long flexiblas_call_zherk [2];
unsigned long flexiblas_call_zhpmv [2];
unsigned long flexiblas_call_zhpr2 [2];
unsigned long flexiblas_call_zhpr  [2];
unsigned long flexiblas_call_zrotg [2];
unsigned long flexiblas_call_zscal [2];
unsigned long flexiblas_call_zswap [2];
unsigned long flexiblas_call_zsymm [2];
unsigned long flexiblas_call_zsyr2k[2];
unsigned long flexiblas_call_zsyrk [2];
unsigned long flexiblas_call_ztbmv [2];
unsigned long flexiblas_call_ztbsv [2];
unsigned long flexiblas_call_ztpmv [2];
unsigned long flexiblas_call_ztpsv [2];
unsigned long flexiblas_call_ztrmm [2];
unsigned long flexiblas_call_ztrmv [2];
unsigned long flexiblas_call_ztrsm [2];
unsigned long flexiblas_call_ztrsv [2];

// real double precision
double flexiblas_time_dasum   [2];
double flexiblas_time_dgbmv   [2];
double flexiblas_time_drot    [2];
double flexiblas_time_dscal   [2];
double flexiblas_time_dswap   [2];
double flexiblas_time_dsyr    [2];
double flexiblas_time_dtpsv   [2];
double flexiblas_time_dzasum  [2];
double flexiblas_time_daxpy   [2];
double flexiblas_time_dgemm   [2];
double flexiblas_time_drotg   [2];
double flexiblas_time_dsdot   [2];
double flexiblas_time_dsymm   [2];
double flexiblas_time_dsyrk   [2];
double flexiblas_time_dtrmm   [2];
double flexiblas_time_dgemv   [2];
double flexiblas_time_drotm   [2];
double flexiblas_time_dspmv   [2];
double flexiblas_time_dsymv   [2];
double flexiblas_time_dtbmv   [2];
double flexiblas_time_dtrmv   [2];
double flexiblas_time_dcopy   [2];
double flexiblas_time_dger    [2];
double flexiblas_time_drotmg  [2];
double flexiblas_time_dspr2   [2];
double flexiblas_time_dsyr2   [2];
double flexiblas_time_dtbsv   [2];
double flexiblas_time_dtrsm   [2];
double flexiblas_time_ddot    [2];
double flexiblas_time_dnrm2   [2];
double flexiblas_time_dsbmv   [2];
double flexiblas_time_dspr    [2];
double flexiblas_time_dsyr2k  [2];
double flexiblas_time_dtpmv   [2];
double flexiblas_time_dtrsv   [2];
double flexiblas_time_dznrm2  [2]; 
double flexiblas_time_dcabs1  [2]; 
unsigned long flexiblas_call_dasum   [2];
unsigned long flexiblas_call_dgbmv   [2];
unsigned long flexiblas_call_drot    [2];
unsigned long flexiblas_call_dscal   [2];
unsigned long flexiblas_call_dswap   [2];
unsigned long flexiblas_call_dsyr    [2];
unsigned long flexiblas_call_dtpsv   [2];
unsigned long flexiblas_call_dzasum  [2];
unsigned long flexiblas_call_daxpy   [2];
unsigned long flexiblas_call_dgemm   [2];
unsigned long flexiblas_call_drotg   [2];
unsigned long flexiblas_call_dsdot   [2];
unsigned long flexiblas_call_dsymm   [2];
unsigned long flexiblas_call_dsyrk   [2];
unsigned long flexiblas_call_dtrmm   [2];
unsigned long flexiblas_call_dznrm2  [2];
unsigned long flexiblas_call_dgemv   [2];
unsigned long flexiblas_call_drotm   [2];
unsigned long flexiblas_call_dspmv   [2];
unsigned long flexiblas_call_dsymv   [2];
unsigned long flexiblas_call_dtbmv   [2];
unsigned long flexiblas_call_dtrmv   [2];
unsigned long flexiblas_call_dcopy   [2];
unsigned long flexiblas_call_dger    [2];
unsigned long flexiblas_call_drotmg  [2];
unsigned long flexiblas_call_dspr2   [2];
unsigned long flexiblas_call_dsyr2   [2];
unsigned long flexiblas_call_dtbsv   [2];
unsigned long flexiblas_call_dtrsm   [2];
unsigned long flexiblas_call_ddot    [2];
unsigned long flexiblas_call_dnrm2   [2];
unsigned long flexiblas_call_dsbmv   [2];
unsigned long flexiblas_call_dspr    [2];
unsigned long flexiblas_call_dsyr2k  [2];
unsigned long flexiblas_call_dtpmv   [2];
unsigned long flexiblas_call_dtrsv   [2];
unsigned long flexiblas_call_dcabs1  [2];  

// real single precision
double  flexiblas_time_sasum  [2];
double  flexiblas_time_saxpy  [2];
double  flexiblas_time_scabs1 [2];
double  flexiblas_time_scasum [2];
double  flexiblas_time_scnrm2 [2];
double  flexiblas_time_scopy  [2];
double  flexiblas_time_sdot   [2];
double  flexiblas_time_sdsdot [2];
double  flexiblas_time_sgbmv  [2];
double  flexiblas_time_sgemm  [2];
double  flexiblas_time_sgemv  [2];
double  flexiblas_time_sger   [2];
double  flexiblas_time_snrm2  [2];
double  flexiblas_time_srot   [2];
double  flexiblas_time_srotg  [2];
double  flexiblas_time_srotm  [2];
double  flexiblas_time_srotmg [2];
double  flexiblas_time_ssbmv  [2];
double  flexiblas_time_sscal  [2];
double  flexiblas_time_sspmv  [2];
double  flexiblas_time_sspr2  [2];
double  flexiblas_time_sspr   [2];
double  flexiblas_time_sswap  [2];
double  flexiblas_time_ssymm  [2];
double  flexiblas_time_ssymv  [2];
double  flexiblas_time_ssyr2  [2];
double  flexiblas_time_ssyr2k [2];
double  flexiblas_time_ssyr   [2];
double  flexiblas_time_ssyrk  [2];
double  flexiblas_time_stbmv  [2];
double  flexiblas_time_stbsv  [2];
double  flexiblas_time_stpmv  [2];
double  flexiblas_time_stpsv  [2];
double  flexiblas_time_strmm  [2];
double  flexiblas_time_strmv  [2];
double  flexiblas_time_strsm  [2];
double  flexiblas_time_strsv  [2];
unsigned long  flexiblas_call_sasum  [2];
unsigned long  flexiblas_call_saxpy  [2];
unsigned long  flexiblas_call_scabs1 [2];
unsigned long  flexiblas_call_scasum [2];
unsigned long  flexiblas_call_scnrm2 [2];
unsigned long  flexiblas_call_scopy  [2];
unsigned long  flexiblas_call_sdot   [2];
unsigned long  flexiblas_call_sdsdot [2];
unsigned long  flexiblas_call_sgbmv  [2];
unsigned long  flexiblas_call_sgemm  [2];
unsigned long  flexiblas_call_sgemv  [2];
unsigned long  flexiblas_call_sger   [2];
unsigned long  flexiblas_call_snrm2  [2];
unsigned long  flexiblas_call_srot   [2];
unsigned long  flexiblas_call_srotg  [2];
unsigned long  flexiblas_call_srotm  [2];
unsigned long  flexiblas_call_srotmg [2];
unsigned long  flexiblas_call_ssbmv  [2];
unsigned long  flexiblas_call_sscal  [2];
unsigned long  flexiblas_call_sspmv  [2];
unsigned long  flexiblas_call_sspr2  [2];
unsigned long  flexiblas_call_sspr   [2];
unsigned long  flexiblas_call_sswap  [2];
unsigned long  flexiblas_call_ssymm  [2];
unsigned long  flexiblas_call_ssymv  [2];
unsigned long  flexiblas_call_ssyr2  [2];
unsigned long  flexiblas_call_ssyr2k [2];
unsigned long  flexiblas_call_ssyr   [2];
unsigned long  flexiblas_call_ssyrk  [2];
unsigned long  flexiblas_call_stbmv  [2];
unsigned long  flexiblas_call_stbsv  [2];
unsigned long  flexiblas_call_stpmv  [2];
unsigned long  flexiblas_call_stpsv  [2];
unsigned long  flexiblas_call_strmm  [2];
unsigned long  flexiblas_call_strmv  [2];
unsigned long  flexiblas_call_strsm  [2];
unsigned long  flexiblas_call_strsv  [2];
// integer function
double  flexiblas_time_icamax[2];
double  flexiblas_time_idamax[2];
double  flexiblas_time_isamax[2];
double  flexiblas_time_izamax[2];
double  flexiblas_time_xerbla[2];

unsigned long  flexiblas_call_icamax[2];
unsigned long  flexiblas_call_idamax[2];
unsigned long  flexiblas_call_isamax[2];
unsigned long  flexiblas_call_izamax[2];
unsigned long  flexiblas_call_xerbla [2];



#endif 
/*-----------------------------------------------------------------------------
 *  Preprocessor macros
 *-----------------------------------------------------------------------------*/
#define BLAS_FN(rettype,name,args,call) \
	BLAS_FN_(rettype,name,args,call) \
	BLAS_FN_NO_(rettype,name,args,call) 
#define BLAS_NONVOID_FN(rettype,name,args,call) \
	BLAS_NONVOID_FN_(rettype,name,args,call) \
	BLAS_NONVOID_FN_NO_(rettype,name,args,call) 


#ifndef FLEXIBLAS_PROFILE 
#define BLAS_FN_(rettype, name, args, callseq) \
	rettype name##_ args { \
	rettype (*fn) args ;  \
	fn = flexiblas_##name .call_fblas; \
	if ( fn == NULL ) { \
		fprintf(stderr, PRINT_PREFIX #name"_not hooked, abort\n"); \
		abort(); \
	}\
	return fn callseq; \
}

#define BLAS_FN_NO_(rettype, name, args, callseq) \
	rettype name args { \
	rettype (*fn) args ;  \
	fn = flexiblas_##name.call_fblas; \
	if ( fn == NULL ) { \
		fprintf(stderr, PRINT_PREFIX #name"_not hooked, abort\n"); \
		abort(); \
	}\
	return fn callseq; \
}

#define BLAS_NONVOID_FN_(rettype, name, args, callseq) \
	rettype name##_ args { \
	rettype (*fn) args ;  \
	rettype erg; \
	fn = flexiblas_##name .call_fblas; \
	if ( fn == NULL ) { \
		fprintf(stderr, PRINT_PREFIX #name"_not hooked, abort\n"); \
		abort(); \
	}\
	erg = fn callseq; \
	return erg;\
}

#define BLAS_NONVOID_FN_NO_(rettype, name, args, callseq) \
	rettype name args { \
	rettype (*fn) args ;  \
	rettype erg; \
	fn = flexiblas_##name.call_fblas; \
	if ( fn == NULL ) { \
		fprintf(stderr, PRINT_PREFIX #name"_not hooked, abort\n"); \
		abort(); \
	}\
	erg = fn callseq; \
	return erg;\
}



#else 
double flexiblas_wtime();
#define BLAS_FN_(rettype, name, args, callseq) \
	rettype name##_ args { \
	double ts;\
	rettype (*fn) args ;  \
	fn = flexiblas_##name .call_fblas; \
	if ( fn == NULL ) { \
		fprintf(stderr, PRINT_PREFIX #name"_not hooked, abort\n"); \
		abort(); \
	}\
	ts = flexiblas_wtime(); \
	fn callseq;\
	flexiblas_time_##name [0] += (flexiblas_wtime() -ts);\
	flexiblas_call_##name [0]++;\
	return; \
}

#define BLAS_FN_NO_(rettype, name, args, callseq) \
	rettype name args { \
	rettype (*fn) args ;  \
	double ts; \
	fn = flexiblas_##name.call_fblas; \
	if ( fn == NULL ) { \
		fprintf(stderr, PRINT_PREFIX #name"_not hooked, abort\n"); \
		abort(); \
	}\
	ts = flexiblas_wtime(); \
	fn callseq; \
	( flexiblas_time_## name [0]) += (flexiblas_wtime()-ts);\
	flexiblas_call_##name [0] ++;\
	return; \
}

#define BLAS_NONVOID_FN_(rettype, name, args, callseq) \
	rettype name##_ args { \
	rettype (*fn) args ;  \
	rettype erg; double ts; \
	fn = flexiblas_##name .call_fblas; \
	if ( fn == NULL ) { \
		fprintf(stderr, PRINT_PREFIX #name"_not hooked, abort\n"); \
		abort(); \
	}\
	ts = flexiblas_wtime();\
	erg = fn callseq; \
	flexiblas_time_##name [0] += (flexiblas_wtime() - ts);\
	flexiblas_call_##name [0]++;\
	return erg;\
}

#define BLAS_NONVOID_FN_NO_(rettype, name, args, callseq) \
	rettype name args { \
	rettype (*fn) args ;  \
	rettype erg; double ts;\
	fn = flexiblas_##name.call_fblas; \
	if ( fn == NULL ) { \
		fprintf(stderr, PRINT_PREFIX #name"_not hooked, abort\n"); \
		abort(); \
	}\
	ts = flexiblas_wtime();\
	erg = fn callseq; \
	flexiblas_time_##name [0]+= (flexiblas_wtime()-ts);\
	flexiblas_call_##name [0]++;\
	return erg;\
}



#endif


#define LOAD_HOOK(x) if ( LOAD_HOOK_INTERN(x) != 0) {\
			fprintf(stderr, COLOR_RED PRINT_PREFIX "Can not load " #x "\n" COLOR_RESET );\
			abort(); \
		     }
#define LOAD_HOOK2(x,c) if ( LOAD_HOOK_INTERN2(x,c) != 0) {\
			fprintf(stderr, COLOR_RED PRINT_PREFIX "Can not load " #x "\n" COLOR_RESET);\
			abort(); \
		     }

#define LOAD_HOOK_INTERN(x) HOOK(handle, #x ,NULL, &(flexiblas_##x))
#define LOAD_HOOK_INTERN2(x,c) HOOK(handle, #x ,#c, &(flexiblas_##x))

#define HOOK(h,s,c,v) __flexiblas_loadhook(h,s,c,v)

#endif 
