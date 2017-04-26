/*
 * cblas_test.h
 * Written by Keita Teranishi
 */
#ifndef CBLAS_TEST_H
#define CBLAS_TEST_H
#include "cblas.h"

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
   #define F77_xerbla xerbla_
/*
 * Level 1 BLAS
 */
   #define F77_srotg      srotgtest_
   #define F77_srotmg     srotmgtest_
   #define F77_srot       srottest_
   #define F77_srotm      srotmtest_
   #define F77_drotg      drotgtest_
   #define F77_drotmg     drotmgtest_
   #define F77_drot       drottest_
   #define F77_drotm      drotmtest_
   #define F77_sswap      sswaptest_
   #define F77_scopy      scopytest_
   #define F77_saxpy      saxpytest_
   #define F77_isamax     isamaxtest_
   #define F77_dswap      dswaptest_
   #define F77_dcopy      dcopytest_
   #define F77_daxpy      daxpytest_
   #define F77_idamax     idamaxtest_
   #define F77_cswap      cswaptest_
   #define F77_ccopy      ccopytest_
   #define F77_caxpy      caxpytest_
   #define F77_icamax     icamaxtest_
   #define F77_zswap      zswaptest_
   #define F77_zcopy      zcopytest_
   #define F77_zaxpy      zaxpytest_
   #define F77_izamax     izamaxtest_
   #define F77_sdot       sdottest_
   #define F77_ddot       ddottest_
   #define F77_dsdot      dsdottest_
   #define F77_sscal      sscaltest_
   #define F77_dscal      dscaltest_
   #define F77_cscal      cscaltest_
   #define F77_zscal      zscaltest_
   #define F77_csscal     csscaltest_
   #define F77_zdscal      zdscaltest_
   #define F77_cdotu      cdotutest_
   #define F77_cdotc      cdotctest_
   #define F77_zdotu      zdotutest_
   #define F77_zdotc      zdotctest_
   #define F77_snrm2      snrm2test_
   #define F77_sasum      sasumtest_
   #define F77_dnrm2      dnrm2test_
   #define F77_dasum      dasumtest_
   #define F77_scnrm2     scnrm2test_
   #define F77_scasum     scasumtest_
   #define F77_dznrm2     dznrm2test_
   #define F77_dzasum     dzasumtest_
   #define F77_sdsdot     sdsdottest_
/*
 * Level 2 BLAS
 */
   #define F77_s2chke     cs2chke_
   #define F77_d2chke     cd2chke_
   #define F77_c2chke     cc2chke_
   #define F77_z2chke     cz2chke_
   #define F77_ssymv      cssymv_
   #define F77_ssbmv      cssbmv_
   #define F77_sspmv      csspmv_
   #define F77_sger       csger_
   #define F77_ssyr       cssyr_
   #define F77_sspr       csspr_
   #define F77_ssyr2      cssyr2_
   #define F77_sspr2      csspr2_
   #define F77_dsymv      cdsymv_
   #define F77_dsbmv      cdsbmv_
   #define F77_dspmv      cdspmv_
   #define F77_dger       cdger_
   #define F77_dsyr       cdsyr_
   #define F77_dspr       cdspr_
   #define F77_dsyr2      cdsyr2_
   #define F77_dspr2      cdspr2_
   #define F77_chemv      cchemv_
   #define F77_chbmv      cchbmv_
   #define F77_chpmv      cchpmv_
   #define F77_cgeru      ccgeru_
   #define F77_cgerc      ccgerc_
   #define F77_cher       ccher_
   #define F77_chpr       cchpr_
   #define F77_cher2      ccher2_
   #define F77_chpr2      cchpr2_
   #define F77_zhemv      czhemv_
   #define F77_zhbmv      czhbmv_
   #define F77_zhpmv      czhpmv_
   #define F77_zgeru      czgeru_
   #define F77_zgerc      czgerc_
   #define F77_zher       czher_
   #define F77_zhpr       czhpr_
   #define F77_zher2      czher2_
   #define F77_zhpr2      czhpr2_
   #define F77_sgemv      csgemv_
   #define F77_sgbmv      csgbmv_
   #define F77_strmv      cstrmv_
   #define F77_stbmv      cstbmv_
   #define F77_stpmv      cstpmv_
   #define F77_strsv      cstrsv_
   #define F77_stbsv      cstbsv_
   #define F77_stpsv      cstpsv_
   #define F77_dgemv      cdgemv_
   #define F77_dgbmv      cdgbmv_
   #define F77_dtrmv      cdtrmv_
   #define F77_dtbmv      cdtbmv_
   #define F77_dtpmv      cdtpmv_
   #define F77_dtrsv      cdtrsv_
   #define F77_dtbsv      cdtbsv_
   #define F77_dtpsv      cdtpsv_
   #define F77_cgemv      ccgemv_
   #define F77_cgbmv      ccgbmv_
   #define F77_ctrmv      cctrmv_
   #define F77_ctbmv      cctbmv_
   #define F77_ctpmv      cctpmv_
   #define F77_ctrsv      cctrsv_
   #define F77_ctbsv      cctbsv_
   #define F77_ctpsv      cctpsv_
   #define F77_zgemv      czgemv_
   #define F77_zgbmv      czgbmv_
   #define F77_ztrmv      cztrmv_
   #define F77_ztbmv      cztbmv_
   #define F77_ztpmv      cztpmv_
   #define F77_ztrsv      cztrsv_
   #define F77_ztbsv      cztbsv_
   #define F77_ztpsv      cztpsv_
/*
 * Level 3 BLAS
 */
   #define F77_s3chke     cs3chke_
   #define F77_d3chke     cd3chke_
   #define F77_c3chke     cc3chke_
   #define F77_z3chke     cz3chke_
   #define F77_chemm      cchemm_
   #define F77_cherk      ccherk_
   #define F77_cher2k     ccher2k_
   #define F77_zhemm      czhemm_
   #define F77_zherk      czherk_
   #define F77_zher2k     czher2k_
   #define F77_sgemm      csgemm_
   #define F77_ssymm      cssymm_
   #define F77_ssyrk      cssyrk_
   #define F77_ssyr2k     cssyr2k_
   #define F77_strmm      cstrmm_
   #define F77_strsm      cstrsm_
   #define F77_dgemm      cdgemm_
   #define F77_dsymm      cdsymm_
   #define F77_dsyrk      cdsyrk_
   #define F77_dsyr2k     cdsyr2k_
   #define F77_dtrmm      cdtrmm_
   #define F77_dtrsm      cdtrsm_
   #define F77_cgemm      ccgemm_
   #define F77_csymm      ccsymm_
   #define F77_csyrk      ccsyrk_
   #define F77_csyr2k     ccsyr2k_
   #define F77_ctrmm      cctrmm_
   #define F77_ctrsm      cctrsm_
   #define F77_zgemm      czgemm_
   #define F77_zsymm      czsymm_
   #define F77_zsyrk      czsyrk_
   #define F77_zsyr2k     czsyr2k_
   #define F77_ztrmm      cztrmm_
   #define F77_ztrsm      cztrsm_
void get_transpose_type(char *type, enum CBLAS_TRANSPOSE *trans);
void get_uplo_type(char *type, enum CBLAS_UPLO *uplo);
void get_diag_type(char *type, enum CBLAS_DIAG *diag);
void get_side_type(char *type, enum CBLAS_SIDE *side);

#endif /* CBLAS_TEST_H */
