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
 * Copyright (C) Martin Koehler, 2016
 */




#ifndef CBLAS_TEST_H
#define CBLAS_TEST_H
#include "cblas.h"
#include "fortran_mangle.h"

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

#define  TRUE           1
#define  PASSED         1
#define  TEST_ROW_MJR	1

#define  FALSE          0
#define  FAILED         0
#define  TEST_COL_MJR	0

#define  INVALID       -1
#define  UNDEFINED     -1

typedef struct { float real; float imag; } CBLAS_TEST_COMPLEX;
typedef struct { double real; double imag; } CBLAS_TEST_ZOMPLEX;

// #if defined(ADD_)
   #define F77_xerbla  FC_GLOBAL(xerbla,XERBLA)
/*
 * Level 1 BLAS
 */
   #define F77_srotg       FC_GLOBAL(srotgtest,SROTGTEST)
   #define F77_srotmg      FC_GLOBAL(srotmgtest,SROTMGTEST)
   #define F77_srot        FC_GLOBAL(srottest,SROTTEST)
   #define F77_srotm       FC_GLOBAL(srotmtest,SROTMTEST)
   #define F77_drotg       FC_GLOBAL(drotgtest,DROTGTEST)
   #define F77_drotmg      FC_GLOBAL(drotmgtest,DROTMGTEST)
   #define F77_drot        FC_GLOBAL(drottest,DROTTEST)
   #define F77_drotm       FC_GLOBAL(drotmtest,DROTMTEST)
   #define F77_sswap       FC_GLOBAL(sswaptest,SSWAPTEST)
   #define F77_scopy       FC_GLOBAL(scopytest,SCOPYTEST)
   #define F77_saxpy       FC_GLOBAL(saxpytest,SAXPYTEST)
   #define F77_isamax      FC_GLOBAL(isamaxtest,ISAMAXTEST)
   #define F77_dswap       FC_GLOBAL(dswaptest,DSWAPTEST)
   #define F77_dcopy       FC_GLOBAL(dcopytest,DCOPYTEST)
   #define F77_daxpy       FC_GLOBAL(daxpytest,DAXPYTEST)
   #define F77_idamax      FC_GLOBAL(idamaxtest,IDAMAXTEST)
   #define F77_cswap       FC_GLOBAL(cswaptest,CSWAPTEST)
   #define F77_ccopy       FC_GLOBAL(ccopytest,CCOPYTEST)
   #define F77_caxpy       FC_GLOBAL(caxpytest,CAXPYTEST)
   #define F77_icamax      FC_GLOBAL(icamaxtest,ICAMAXTEST)
   #define F77_zswap       FC_GLOBAL(zswaptest,ZSWAPTEST)
   #define F77_zcopy       FC_GLOBAL(zcopytest,ZCOPYTEST)
   #define F77_zaxpy       FC_GLOBAL(zaxpytest,ZAXPYTEST)
   #define F77_izamax      FC_GLOBAL(izamaxtest,IZAMAXTEST)
   #define F77_sdot        FC_GLOBAL(sdottest,SDOTTEST)
   #define F77_ddot        FC_GLOBAL(ddottest,DDOTTEST)
   #define F77_dsdot       FC_GLOBAL(dsdottest,DSDOTTEST)
   #define F77_sscal       FC_GLOBAL(sscaltest,SSCALTEST)
   #define F77_dscal       FC_GLOBAL(dscaltest,DSCALTEST)
   #define F77_cscal       FC_GLOBAL(cscaltest,CSCALTEST)
   #define F77_zscal       FC_GLOBAL(zscaltest,ZSCALTEST)
   #define F77_csscal      FC_GLOBAL(csscaltest,CSSCALTEST)
   #define F77_zdscal       FC_GLOBAL(zdscaltest,ZDSCALTEST)
   #define F77_cdotu       FC_GLOBAL(cdotutest,CDOTUTEST)
   #define F77_cdotc       FC_GLOBAL(cdotctest,CDOTCTEST)
   #define F77_zdotu       FC_GLOBAL(zdotutest,ZDOTUTEST)
   #define F77_zdotc       FC_GLOBAL(zdotctest,ZDOTCTEST)
   #define F77_snrm2       FC_GLOBAL(snrm2test,SNRM2TEST)
   #define F77_sasum       FC_GLOBAL(sasumtest,SASUMTEST)
   #define F77_dnrm2       FC_GLOBAL(dnrm2test,DNRM2TEST)
   #define F77_dasum       FC_GLOBAL(dasumtest,DASUMTEST)
   #define F77_scnrm2      FC_GLOBAL(scnrm2test,SCNRM2TEST)
   #define F77_scasum      FC_GLOBAL(scasumtest,SCASUMTEST)
   #define F77_dznrm2      FC_GLOBAL(dznrm2test,DZNRM2TEST)
   #define F77_dzasum      FC_GLOBAL(dzasumtest,DZASUMTEST)
   #define F77_sdsdot      FC_GLOBAL(sdsdottest,SDSDOTTEST)
/*
 * Level 2 BLAS
 */
   #define F77_s2chke      FC_GLOBAL(cs2chke,CS2CHKE)
   #define F77_d2chke      FC_GLOBAL(cd2chke,CD2CHKE)
   #define F77_c2chke      FC_GLOBAL(cc2chke,CC2CHKE)
   #define F77_z2chke      FC_GLOBAL(cz2chke,CZ2CHKE)
   #define F77_ssymv       FC_GLOBAL(cssymv,CSSYMV)
   #define F77_ssbmv       FC_GLOBAL(cssbmv,CSSBMV)
   #define F77_sspmv       FC_GLOBAL(csspmv,CSSPMV)
   #define F77_sger        FC_GLOBAL(csger,CSGER)
   #define F77_ssyr        FC_GLOBAL(cssyr,CSSYR)
   #define F77_sspr        FC_GLOBAL(csspr,CSSPR)
   #define F77_ssyr2       FC_GLOBAL(cssyr2,CSSYR2)
   #define F77_sspr2       FC_GLOBAL(csspr2,CSSPR2)
   #define F77_dsymv       FC_GLOBAL(cdsymv,CDSYMV)
   #define F77_dsbmv       FC_GLOBAL(cdsbmv,CDSBMV)
   #define F77_dspmv       FC_GLOBAL(cdspmv,CDSPMV)
   #define F77_dger        FC_GLOBAL(cdger,CDGER)
   #define F77_dsyr        FC_GLOBAL(cdsyr,CDSYR)
   #define F77_dspr        FC_GLOBAL(cdspr,CDSPR)
   #define F77_dsyr2       FC_GLOBAL(cdsyr2,CDSYR2)
   #define F77_dspr2       FC_GLOBAL(cdspr2,CDSPR2)
   #define F77_chemv       FC_GLOBAL(cchemv,CCHEMV)
   #define F77_chbmv       FC_GLOBAL(cchbmv,CCHBMV)
   #define F77_chpmv       FC_GLOBAL(cchpmv,CCHPMV)
   #define F77_cgeru       FC_GLOBAL(ccgeru,CCGERU)
   #define F77_cgerc       FC_GLOBAL(ccgerc,CCGERC)
   #define F77_cher        FC_GLOBAL(ccher,CCHER)
   #define F77_chpr        FC_GLOBAL(cchpr,CCHPR)
   #define F77_cher2       FC_GLOBAL(ccher2,CCHER2)
   #define F77_chpr2       FC_GLOBAL(cchpr2,CCHPR2)
   #define F77_zhemv       FC_GLOBAL(czhemv,CZHEMV)
   #define F77_zhbmv       FC_GLOBAL(czhbmv,CZHBMV)
   #define F77_zhpmv       FC_GLOBAL(czhpmv,CZHPMV)
   #define F77_zgeru       FC_GLOBAL(czgeru,CZGERU)
   #define F77_zgerc       FC_GLOBAL(czgerc,CZGERC)
   #define F77_zher        FC_GLOBAL(czher,CZHER)
   #define F77_zhpr        FC_GLOBAL(czhpr,CZHPR)
   #define F77_zher2       FC_GLOBAL(czher2,CZHER2)
   #define F77_zhpr2       FC_GLOBAL(czhpr2,CZHPR2)
   #define F77_sgemv       FC_GLOBAL(csgemv,CSGEMV)
   #define F77_sgbmv       FC_GLOBAL(csgbmv,CSGBMV)
   #define F77_strmv       FC_GLOBAL(cstrmv,CSTRMV)
   #define F77_stbmv       FC_GLOBAL(cstbmv,CSTBMV)
   #define F77_stpmv       FC_GLOBAL(cstpmv,CSTPMV)
   #define F77_strsv       FC_GLOBAL(cstrsv,CSTRSV)
   #define F77_stbsv       FC_GLOBAL(cstbsv,CSTBSV)
   #define F77_stpsv       FC_GLOBAL(cstpsv,CSTPSV)
   #define F77_dgemv       FC_GLOBAL(cdgemv,CDGEMV)
   #define F77_dgbmv       FC_GLOBAL(cdgbmv,CDGBMV)
   #define F77_dtrmv       FC_GLOBAL(cdtrmv,CDTRMV)
   #define F77_dtbmv       FC_GLOBAL(cdtbmv,CDTBMV)
   #define F77_dtpmv       FC_GLOBAL(cdtpmv,CDTPMV)
   #define F77_dtrsv       FC_GLOBAL(cdtrsv,CDTRSV)
   #define F77_dtbsv       FC_GLOBAL(cdtbsv,CDTBSV)
   #define F77_dtpsv       FC_GLOBAL(cdtpsv,CDTPSV)
   #define F77_cgemv       FC_GLOBAL(ccgemv,CCGEMV)
   #define F77_cgbmv       FC_GLOBAL(ccgbmv,CCGBMV)
   #define F77_ctrmv       FC_GLOBAL(cctrmv,CCTRMV)
   #define F77_ctbmv       FC_GLOBAL(cctbmv,CCTBMV)
   #define F77_ctpmv       FC_GLOBAL(cctpmv,CCTPMV)
   #define F77_ctrsv       FC_GLOBAL(cctrsv,CCTRSV)
   #define F77_ctbsv       FC_GLOBAL(cctbsv,CCTBSV)
   #define F77_ctpsv       FC_GLOBAL(cctpsv,CCTPSV)
   #define F77_zgemv       FC_GLOBAL(czgemv,CZGEMV)
   #define F77_zgbmv       FC_GLOBAL(czgbmv,CZGBMV)
   #define F77_ztrmv       FC_GLOBAL(cztrmv,CZTRMV)
   #define F77_ztbmv       FC_GLOBAL(cztbmv,CZTBMV)
   #define F77_ztpmv       FC_GLOBAL(cztpmv,CZTPMV)
   #define F77_ztrsv       FC_GLOBAL(cztrsv,CZTRSV)
   #define F77_ztbsv       FC_GLOBAL(cztbsv,CZTBSV)
   #define F77_ztpsv       FC_GLOBAL(cztpsv,CZTPSV)
/*
 * Level 3 BLAS
 */
   #define F77_s3chke      FC_GLOBAL(cs3chke,CS3CHKE)
   #define F77_d3chke      FC_GLOBAL(cd3chke,CD3CHKE)
   #define F77_c3chke      FC_GLOBAL(cc3chke,CC3CHKE)
   #define F77_z3chke      FC_GLOBAL(cz3chke,CZ3CHKE)
   #define F77_chemm       FC_GLOBAL(cchemm,CCHEMM)
   #define F77_cherk       FC_GLOBAL(ccherk,CCHERK)
   #define F77_cher2k      FC_GLOBAL(ccher2k,CCHER2K)
   #define F77_zhemm       FC_GLOBAL(czhemm,CZHEMM)
   #define F77_zherk       FC_GLOBAL(czherk,CZHERK)
   #define F77_zher2k      FC_GLOBAL(czher2k,CZHER2K)
   #define F77_sgemm       FC_GLOBAL(csgemm,CSGEMM)
   #define F77_ssymm       FC_GLOBAL(cssymm,CSSYMM)
   #define F77_ssyrk       FC_GLOBAL(cssyrk,CSSYRK)
   #define F77_ssyr2k      FC_GLOBAL(cssyr2k,CSSYR2K)
   #define F77_strmm       FC_GLOBAL(cstrmm,CSTRMM)
   #define F77_strsm       FC_GLOBAL(cstrsm,CSTRSM)
   #define F77_dgemm       FC_GLOBAL(cdgemm,CDGEMM)
   #define F77_dsymm       FC_GLOBAL(cdsymm,CDSYMM)
   #define F77_dsyrk       FC_GLOBAL(cdsyrk,CDSYRK)
   #define F77_dsyr2k      FC_GLOBAL(cdsyr2k,CDSYR2K)
   #define F77_dtrmm       FC_GLOBAL(cdtrmm,CDTRMM)
   #define F77_dtrsm       FC_GLOBAL(cdtrsm,CDTRSM)
   #define F77_cgemm       FC_GLOBAL(ccgemm,CCGEMM)
   #define F77_csymm       FC_GLOBAL(ccsymm,CCSYMM)
   #define F77_csyrk       FC_GLOBAL(ccsyrk,CCSYRK)
   #define F77_csyr2k      FC_GLOBAL(ccsyr2k,CCSYR2K)
   #define F77_ctrmm       FC_GLOBAL(cctrmm,CCTRMM)
   #define F77_ctrsm       FC_GLOBAL(cctrsm,CCTRSM)
   #define F77_zgemm       FC_GLOBAL(czgemm,CZGEMM)
   #define F77_zsymm       FC_GLOBAL(czsymm,CZSYMM)
   #define F77_zsyrk       FC_GLOBAL(czsyrk,CZSYRK)
   #define F77_zsyr2k      FC_GLOBAL(czsyr2k,CZSYR2K)
   #define F77_ztrmm       FC_GLOBAL(cztrmm,CZTRMM)
   #define F77_ztrsm       FC_GLOBAL(cztrsm,CZTRSM)
void get_transpose_type(char *type, CBLAS_TRANSPOSE *trans);
void get_uplo_type(char *type, CBLAS_UPLO *uplo);
void get_diag_type(char *type, CBLAS_DIAG *diag);
void get_side_type(char *type, CBLAS_SIDE *side);

#endif /* CBLAS_TEST_H */
