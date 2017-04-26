/*
 *     Written by D.P. Manley, Digital Equipment Corporation.
 *     Prefixed "C_" to BLAS routines and their declarations.
 *
 *     Modified by T. H. Do, 4/08/98, SGI/CRAY Research.
 */
#include <stdlib.h>
#include "cblas.h"
#include "cblas_test.h"

#ifdef  INTEGER8 
	#include <stdint.h>
	#define Int int64_t 
#else 
	#define Int int 
#endif 

void F77_cgemv(Int *order, char *transp, Int *m, Int *n, 
          const void *alpha,
          CBLAS_TEST_COMPLEX *a, Int *lda, const void *x, Int *incx, 
          const void *beta, void *y, Int *incy) {

  CBLAS_TEST_COMPLEX *A;
  Int i,j,LDA;
  enum CBLAS_TRANSPOSE trans;

  get_transpose_type(transp, &trans);
  if (*order == TEST_ROW_MJR) {
     LDA = *n+1;
     A  = (CBLAS_TEST_COMPLEX *)malloc( (*m)*LDA*sizeof( CBLAS_TEST_COMPLEX) );
     for( i=0; i<*m; i++ )
        for( j=0; j<*n; j++ ){
           A[ LDA*i+j ].real=a[ (*lda)*j+i ].real;
           A[ LDA*i+j ].imag=a[ (*lda)*j+i ].imag;
        }
     cblas_cgemv( CblasRowMajor, trans, *m, *n, alpha, A, LDA, x, *incx,
	    beta, y, *incy );
     free(A);
  }
  else if (*order == TEST_COL_MJR)
     cblas_cgemv( CblasColMajor, trans,
                  *m, *n, alpha, a, *lda, x, *incx, beta, y, *incy );
  else
     cblas_cgemv( UNDEFINED, trans,
                  *m, *n, alpha, a, *lda, x, *incx, beta, y, *incy );
}

void F77_cgbmv(Int *order, char *transp, Int *m, Int *n, Int *kl, Int *ku, 
	      CBLAS_TEST_COMPLEX *alpha, CBLAS_TEST_COMPLEX *a, Int *lda, 
	      CBLAS_TEST_COMPLEX *x, Int *incx, 
	      CBLAS_TEST_COMPLEX *beta, CBLAS_TEST_COMPLEX *y, Int *incy) {

  CBLAS_TEST_COMPLEX *A;
  Int i,j,irow,jcol,LDA;
  enum CBLAS_TRANSPOSE trans;

  get_transpose_type(transp, &trans);
  if (*order == TEST_ROW_MJR) {
     LDA = *ku+*kl+2;
     A=( CBLAS_TEST_COMPLEX* )malloc((*n+*kl)*LDA*sizeof(CBLAS_TEST_COMPLEX));
     for( i=0; i<*ku; i++ ){
        irow=*ku+*kl-i;
        jcol=(*ku)-i;
        for( j=jcol; j<*n; j++ ){
           A[ LDA*(j-jcol)+irow ].real=a[ (*lda)*j+i ].real;
           A[ LDA*(j-jcol)+irow ].imag=a[ (*lda)*j+i ].imag;
        }
     }
     i=*ku;
     irow=*ku+*kl-i;
     for( j=0; j<*n; j++ ){
        A[ LDA*j+irow ].real=a[ (*lda)*j+i ].real;
        A[ LDA*j+irow ].imag=a[ (*lda)*j+i ].imag;
     }
     for( i=*ku+1; i<*ku+*kl+1; i++ ){
        irow=*ku+*kl-i;
        jcol=i-(*ku);
        for( j=jcol; j<(*n+*kl); j++ ){
           A[ LDA*j+irow ].real=a[ (*lda)*(j-jcol)+i ].real;
           A[ LDA*j+irow ].imag=a[ (*lda)*(j-jcol)+i ].imag;
        }
     }
     cblas_cgbmv( CblasRowMajor, trans, *m, *n, *kl, *ku, alpha, A, LDA, x,
		  *incx, beta, y, *incy );
     free(A);
  }
  else if (*order == TEST_COL_MJR)
     cblas_cgbmv( CblasColMajor, trans, *m, *n, *kl, *ku, alpha, a, *lda, x,
		  *incx, beta, y, *incy );
  else
     cblas_cgbmv( UNDEFINED, trans, *m, *n, *kl, *ku, alpha, a, *lda, x,
		  *incx, beta, y, *incy );
}

void F77_cgeru(Int *order, Int *m, Int *n, CBLAS_TEST_COMPLEX *alpha, 
	 CBLAS_TEST_COMPLEX *x, Int *incx, CBLAS_TEST_COMPLEX *y, Int *incy, 
         CBLAS_TEST_COMPLEX *a, Int *lda){

  CBLAS_TEST_COMPLEX *A;
  Int i,j,LDA;

  if (*order == TEST_ROW_MJR) {
     LDA = *n+1;
     A=(CBLAS_TEST_COMPLEX*)malloc((*m)*LDA*sizeof(CBLAS_TEST_COMPLEX));
     for( i=0; i<*m; i++ )
        for( j=0; j<*n; j++ ){
           A[ LDA*i+j ].real=a[ (*lda)*j+i ].real;
           A[ LDA*i+j ].imag=a[ (*lda)*j+i ].imag;
     }
     cblas_cgeru( CblasRowMajor, *m, *n, alpha, x, *incx, y, *incy, A, LDA );
     for( i=0; i<*m; i++ )
        for( j=0; j<*n; j++ ){
           a[ (*lda)*j+i ].real=A[ LDA*i+j ].real;
           a[ (*lda)*j+i ].imag=A[ LDA*i+j ].imag;
        }
     free(A);
  }
  else if (*order == TEST_COL_MJR)
     cblas_cgeru( CblasColMajor, *m, *n, alpha, x, *incx, y, *incy, a, *lda );
  else
     cblas_cgeru( UNDEFINED, *m, *n, alpha, x, *incx, y, *incy, a, *lda );
}

void F77_cgerc(Int *order, Int *m, Int *n, CBLAS_TEST_COMPLEX *alpha, 
	 CBLAS_TEST_COMPLEX *x, Int *incx, CBLAS_TEST_COMPLEX *y, Int *incy, 
         CBLAS_TEST_COMPLEX *a, Int *lda) {
  CBLAS_TEST_COMPLEX *A;
  Int i,j,LDA;

  if (*order == TEST_ROW_MJR) {
     LDA = *n+1;
     A=(CBLAS_TEST_COMPLEX* )malloc((*m)*LDA*sizeof(CBLAS_TEST_COMPLEX ) );
     for( i=0; i<*m; i++ )
        for( j=0; j<*n; j++ ){
           A[ LDA*i+j ].real=a[ (*lda)*j+i ].real;
           A[ LDA*i+j ].imag=a[ (*lda)*j+i ].imag;
        }
     cblas_cgerc( CblasRowMajor, *m, *n, alpha, x, *incx, y, *incy, A, LDA );
     for( i=0; i<*m; i++ )
        for( j=0; j<*n; j++ ){
           a[ (*lda)*j+i ].real=A[ LDA*i+j ].real;
           a[ (*lda)*j+i ].imag=A[ LDA*i+j ].imag;
        }
     free(A);
  }
  else if (*order == TEST_COL_MJR)
     cblas_cgerc( CblasColMajor, *m, *n, alpha, x, *incx, y, *incy, a, *lda );
  else
     cblas_cgerc( UNDEFINED, *m, *n, alpha, x, *incx, y, *incy, a, *lda );
}

void F77_chemv(Int *order, char *uplow, Int *n, CBLAS_TEST_COMPLEX *alpha,
      CBLAS_TEST_COMPLEX *a, Int *lda, CBLAS_TEST_COMPLEX *x,
      Int *incx, CBLAS_TEST_COMPLEX *beta, CBLAS_TEST_COMPLEX *y, Int *incy){

  CBLAS_TEST_COMPLEX *A;
  Int i,j,LDA;
  enum CBLAS_UPLO uplo;

  get_uplo_type(uplow,&uplo);

  if (*order == TEST_ROW_MJR) {
     LDA = *n+1;
     A = (CBLAS_TEST_COMPLEX *)malloc((*n)*LDA*sizeof(CBLAS_TEST_COMPLEX));
     for( i=0; i<*n; i++ )
        for( j=0; j<*n; j++ ){
           A[ LDA*i+j ].real=a[ (*lda)*j+i ].real;
           A[ LDA*i+j ].imag=a[ (*lda)*j+i ].imag;
     }
     cblas_chemv( CblasRowMajor, uplo, *n, alpha, A, LDA, x, *incx,
	    beta, y, *incy );
     free(A);
  }
  else if (*order == TEST_COL_MJR)
     cblas_chemv( CblasColMajor, uplo, *n, alpha, a, *lda, x, *incx, 
	   beta, y, *incy );
  else
     cblas_chemv( UNDEFINED, uplo, *n, alpha, a, *lda, x, *incx,
	   beta, y, *incy );
}

void F77_chbmv(Int *order, char *uplow, Int *n, Int *k,
     CBLAS_TEST_COMPLEX *alpha, CBLAS_TEST_COMPLEX *a, Int *lda, 
     CBLAS_TEST_COMPLEX *x, Int *incx, CBLAS_TEST_COMPLEX *beta,
     CBLAS_TEST_COMPLEX *y, Int *incy){

CBLAS_TEST_COMPLEX *A;
Int i,irow,j,jcol,LDA;

  enum CBLAS_UPLO uplo;

  get_uplo_type(uplow,&uplo);

  if (*order == TEST_ROW_MJR) {
     if (uplo != CblasUpper && uplo != CblasLower )
        cblas_chbmv(CblasRowMajor, UNDEFINED, *n, *k, alpha, a, *lda, x, 
		 *incx, beta, y, *incy );
     else {
        LDA = *k+2;
        A =(CBLAS_TEST_COMPLEX*)malloc((*n+*k)*LDA*sizeof(CBLAS_TEST_COMPLEX));
        if (uplo == CblasUpper) {
           for( i=0; i<*k; i++ ){
              irow=*k-i;
              jcol=(*k)-i;
              for( j=jcol; j<*n; j++ ) {
                 A[ LDA*(j-jcol)+irow ].real=a[ (*lda)*j+i ].real;
                 A[ LDA*(j-jcol)+irow ].imag=a[ (*lda)*j+i ].imag;
              }
           }
           i=*k;
           irow=*k-i;
           for( j=0; j<*n; j++ ) {
              A[ LDA*j+irow ].real=a[ (*lda)*j+i ].real;
              A[ LDA*j+irow ].imag=a[ (*lda)*j+i ].imag;
           }
        }
        else {
           i=0;
           irow=*k-i;
           for( j=0; j<*n; j++ ) {
              A[ LDA*j+irow ].real=a[ (*lda)*j+i ].real;
              A[ LDA*j+irow ].imag=a[ (*lda)*j+i ].imag;
           }
           for( i=1; i<*k+1; i++ ){
              irow=*k-i;
              jcol=i;
              for( j=jcol; j<(*n+*k); j++ ) {
                 A[ LDA*j+irow ].real=a[ (*lda)*(j-jcol)+i ].real;
                 A[ LDA*j+irow ].imag=a[ (*lda)*(j-jcol)+i ].imag;
              }
           }
        }
        cblas_chbmv( CblasRowMajor, uplo, *n, *k, alpha, A, LDA, x, *incx,
       		     beta, y, *incy );
        free(A);
      }
   }
   else if (*order == TEST_COL_MJR)
     cblas_chbmv(CblasColMajor, uplo, *n, *k, alpha, a, *lda, x, *incx,
                 beta, y, *incy );
   else
     cblas_chbmv(UNDEFINED, uplo, *n, *k, alpha, a, *lda, x, *incx,
                 beta, y, *incy );
}

void F77_chpmv(Int *order, char *uplow, Int *n, CBLAS_TEST_COMPLEX *alpha,
     CBLAS_TEST_COMPLEX *ap, CBLAS_TEST_COMPLEX *x, Int *incx, 
     CBLAS_TEST_COMPLEX *beta, CBLAS_TEST_COMPLEX *y, Int *incy){

  CBLAS_TEST_COMPLEX *A, *AP;
  Int i,j,k,LDA;
  enum CBLAS_UPLO uplo;

  get_uplo_type(uplow,&uplo);
  if (*order == TEST_ROW_MJR) {
     if (uplo != CblasUpper && uplo != CblasLower )
        cblas_chpmv(CblasRowMajor, UNDEFINED, *n, alpha, ap, x, *incx, 
	         beta, y, *incy);
     else {
        LDA = *n;
        A = (CBLAS_TEST_COMPLEX* )malloc(LDA*LDA*sizeof(CBLAS_TEST_COMPLEX ));
        AP = (CBLAS_TEST_COMPLEX* )malloc( (((LDA+1)*LDA)/2)*
	        sizeof( CBLAS_TEST_COMPLEX ));
        if (uplo == CblasUpper) {
           for( j=0, k=0; j<*n; j++ )
              for( i=0; i<j+1; i++, k++ ) {
                 A[ LDA*i+j ].real=ap[ k ].real;
                 A[ LDA*i+j ].imag=ap[ k ].imag;
              }
           for( i=0, k=0; i<*n; i++ )
              for( j=i; j<*n; j++, k++ ) {
                 AP[ k ].real=A[ LDA*i+j ].real;
                 AP[ k ].imag=A[ LDA*i+j ].imag;
              }
        }
        else {
           for( j=0, k=0; j<*n; j++ )
              for( i=j; i<*n; i++, k++ ) {
                 A[ LDA*i+j ].real=ap[ k ].real;
                 A[ LDA*i+j ].imag=ap[ k ].imag;
              }
           for( i=0, k=0; i<*n; i++ )
              for( j=0; j<i+1; j++, k++ ) {
	         AP[ k ].real=A[ LDA*i+j ].real;
	         AP[ k ].imag=A[ LDA*i+j ].imag;
              }
        }
        cblas_chpmv( CblasRowMajor, uplo, *n, alpha, AP, x, *incx, beta, y,
                     *incy );
        free(A);
        free(AP);
     }
  }
  else if (*order == TEST_COL_MJR)
     cblas_chpmv( CblasColMajor, uplo, *n, alpha, ap, x, *incx, beta, y,
                  *incy );
  else
     cblas_chpmv( UNDEFINED, uplo, *n, alpha, ap, x, *incx, beta, y,
                  *incy );
}

void F77_ctbmv(Int *order, char *uplow, char *transp, char *diagn,
     Int *n, Int *k, CBLAS_TEST_COMPLEX *a, Int *lda, CBLAS_TEST_COMPLEX *x,
     Int *incx) {
  CBLAS_TEST_COMPLEX *A;
  Int irow, jcol, i, j, LDA;
  enum CBLAS_TRANSPOSE trans;
  enum CBLAS_UPLO uplo;
  enum CBLAS_DIAG diag;

  get_transpose_type(transp,&trans);
  get_uplo_type(uplow,&uplo);
  get_diag_type(diagn,&diag);

  if (*order == TEST_ROW_MJR) {
     if (uplo != CblasUpper && uplo != CblasLower )
        cblas_ctbmv(CblasRowMajor, UNDEFINED, trans, diag, *n, *k, a, *lda,
	x, *incx);
     else {
        LDA = *k+2;
        A=(CBLAS_TEST_COMPLEX *)malloc((*n+*k)*LDA*sizeof(CBLAS_TEST_COMPLEX));
        if (uplo == CblasUpper) {
           for( i=0; i<*k; i++ ){
              irow=*k-i;
              jcol=(*k)-i;
              for( j=jcol; j<*n; j++ ) {
                 A[ LDA*(j-jcol)+irow ].real=a[ (*lda)*j+i ].real;
                 A[ LDA*(j-jcol)+irow ].imag=a[ (*lda)*j+i ].imag;
              }
           }
           i=*k;
           irow=*k-i;
           for( j=0; j<*n; j++ ) {
              A[ LDA*j+irow ].real=a[ (*lda)*j+i ].real;
              A[ LDA*j+irow ].imag=a[ (*lda)*j+i ].imag;
           }
        }
        else {
          i=0;
          irow=*k-i;
          for( j=0; j<*n; j++ ) {
             A[ LDA*j+irow ].real=a[ (*lda)*j+i ].real;
             A[ LDA*j+irow ].imag=a[ (*lda)*j+i ].imag;
          }
          for( i=1; i<*k+1; i++ ){
             irow=*k-i;
             jcol=i;
             for( j=jcol; j<(*n+*k); j++ ) {
                A[ LDA*j+irow ].real=a[ (*lda)*(j-jcol)+i ].real;
                A[ LDA*j+irow ].imag=a[ (*lda)*(j-jcol)+i ].imag;
             }
          }
        }
        cblas_ctbmv(CblasRowMajor, uplo, trans, diag, *n, *k, A, LDA, x, 
		    *incx);
        free(A);
     }
   }
   else if (*order == TEST_COL_MJR)
     cblas_ctbmv(CblasColMajor, uplo, trans, diag, *n, *k, a, *lda, x, *incx);
   else
     cblas_ctbmv(UNDEFINED, uplo, trans, diag, *n, *k, a, *lda, x, *incx);
}

void F77_ctbsv(Int *order, char *uplow, char *transp, char *diagn,
      Int *n, Int *k, CBLAS_TEST_COMPLEX *a, Int *lda, CBLAS_TEST_COMPLEX *x,
      Int *incx) {

  CBLAS_TEST_COMPLEX *A;
  Int irow, jcol, i, j, LDA;
  enum CBLAS_TRANSPOSE trans;
  enum CBLAS_UPLO uplo;
  enum CBLAS_DIAG diag;

  get_transpose_type(transp,&trans);
  get_uplo_type(uplow,&uplo);
  get_diag_type(diagn,&diag);

  if (*order == TEST_ROW_MJR) {
     if (uplo != CblasUpper && uplo != CblasLower )
        cblas_ctbsv(CblasRowMajor, UNDEFINED, trans, diag, *n, *k, a, *lda, x, 
	         *incx);
     else {
        LDA = *k+2;
        A=(CBLAS_TEST_COMPLEX*)malloc((*n+*k)*LDA*sizeof(CBLAS_TEST_COMPLEX ));
        if (uplo == CblasUpper) {
           for( i=0; i<*k; i++ ){
              irow=*k-i;
              jcol=(*k)-i;
              for( j=jcol; j<*n; j++ ) {
                 A[ LDA*(j-jcol)+irow ].real=a[ (*lda)*j+i ].real;
                 A[ LDA*(j-jcol)+irow ].imag=a[ (*lda)*j+i ].imag;
              }
           }
           i=*k;
           irow=*k-i;
           for( j=0; j<*n; j++ ) {
              A[ LDA*j+irow ].real=a[ (*lda)*j+i ].real;
              A[ LDA*j+irow ].imag=a[ (*lda)*j+i ].imag;
           }
        }
        else {
           i=0;
           irow=*k-i;
           for( j=0; j<*n; j++ ) {
             A[ LDA*j+irow ].real=a[ (*lda)*j+i ].real;
             A[ LDA*j+irow ].imag=a[ (*lda)*j+i ].imag;
           }
           for( i=1; i<*k+1; i++ ){
              irow=*k-i;
              jcol=i;
              for( j=jcol; j<(*n+*k); j++ ) {
	         A[ LDA*j+irow ].real=a[ (*lda)*(j-jcol)+i ].real;
                 A[ LDA*j+irow ].imag=a[ (*lda)*(j-jcol)+i ].imag;
              }
           }
        }
        cblas_ctbsv(CblasRowMajor, uplo, trans, diag, *n, *k, A, LDA, 
		    x, *incx);
        free(A);
     }
  }
  else if (*order == TEST_COL_MJR)
     cblas_ctbsv(CblasColMajor, uplo, trans, diag, *n, *k, a, *lda, x, *incx);
  else
     cblas_ctbsv(UNDEFINED, uplo, trans, diag, *n, *k, a, *lda, x, *incx);
}

void F77_ctpmv(Int *order, char *uplow, char *transp, char *diagn,
      Int *n, CBLAS_TEST_COMPLEX *ap, CBLAS_TEST_COMPLEX *x, Int *incx) {
  CBLAS_TEST_COMPLEX *A, *AP;
  Int i, j, k, LDA;
  enum CBLAS_TRANSPOSE trans;
  enum CBLAS_UPLO uplo;
  enum CBLAS_DIAG diag;

  get_transpose_type(transp,&trans);
  get_uplo_type(uplow,&uplo);
  get_diag_type(diagn,&diag);

  if (*order == TEST_ROW_MJR) {
     if (uplo != CblasUpper && uplo != CblasLower )
        cblas_ctpmv( CblasRowMajor, UNDEFINED, trans, diag, *n, ap, x, *incx );
     else {
        LDA = *n;
        A=(CBLAS_TEST_COMPLEX*)malloc(LDA*LDA*sizeof(CBLAS_TEST_COMPLEX));
        AP=(CBLAS_TEST_COMPLEX*)malloc((((LDA+1)*LDA)/2)*
	 	sizeof(CBLAS_TEST_COMPLEX));
        if (uplo == CblasUpper) {
           for( j=0, k=0; j<*n; j++ )
              for( i=0; i<j+1; i++, k++ ) {
                 A[ LDA*i+j ].real=ap[ k ].real;
                 A[ LDA*i+j ].imag=ap[ k ].imag;
              }
           for( i=0, k=0; i<*n; i++ )
              for( j=i; j<*n; j++, k++ ) {
                 AP[ k ].real=A[ LDA*i+j ].real;
                 AP[ k ].imag=A[ LDA*i+j ].imag;
              }
        }
        else {
           for( j=0, k=0; j<*n; j++ )
              for( i=j; i<*n; i++, k++ ) {
                 A[ LDA*i+j ].real=ap[ k ].real;
	         A[ LDA*i+j ].imag=ap[ k ].imag;
              }
           for( i=0, k=0; i<*n; i++ )
              for( j=0; j<i+1; j++, k++ ) {
                 AP[ k ].real=A[ LDA*i+j ].real;
	         AP[ k ].imag=A[ LDA*i+j ].imag;
              }
        }
        cblas_ctpmv( CblasRowMajor, uplo, trans, diag, *n, AP, x, *incx );
        free(A);
        free(AP);
     }
  }
  else if (*order == TEST_COL_MJR)
     cblas_ctpmv( CblasColMajor, uplo, trans, diag, *n, ap, x, *incx );
  else
     cblas_ctpmv( UNDEFINED, uplo, trans, diag, *n, ap, x, *incx );
}

void F77_ctpsv(Int *order, char *uplow, char *transp, char *diagn,
     Int *n, CBLAS_TEST_COMPLEX *ap, CBLAS_TEST_COMPLEX *x, Int *incx) {
  CBLAS_TEST_COMPLEX *A, *AP;
  Int i, j, k, LDA;
  enum CBLAS_TRANSPOSE trans;
  enum CBLAS_UPLO uplo;
  enum CBLAS_DIAG diag;

  get_transpose_type(transp,&trans);
  get_uplo_type(uplow,&uplo);
  get_diag_type(diagn,&diag);

  if (*order == TEST_ROW_MJR) {
     if (uplo != CblasUpper && uplo != CblasLower )
        cblas_ctpsv( CblasRowMajor, UNDEFINED, trans, diag, *n, ap, x, *incx );
     else {
        LDA = *n;
        A=(CBLAS_TEST_COMPLEX*)malloc(LDA*LDA*sizeof(CBLAS_TEST_COMPLEX));
        AP=(CBLAS_TEST_COMPLEX*)malloc((((LDA+1)*LDA)/2)*
		sizeof(CBLAS_TEST_COMPLEX));
     	if (uplo == CblasUpper) {
           for( j=0, k=0; j<*n; j++ )
              for( i=0; i<j+1; i++, k++ ) {
                 A[ LDA*i+j ].real=ap[ k ].real;
       	         A[ LDA*i+j ].imag=ap[ k ].imag;
              }
           for( i=0, k=0; i<*n; i++ )
              for( j=i; j<*n; j++, k++ ) {
                 AP[ k ].real=A[ LDA*i+j ].real;
	         AP[ k ].imag=A[ LDA*i+j ].imag;
              }
        }
        else {
           for( j=0, k=0; j<*n; j++ )
              for( i=j; i<*n; i++, k++ ) {
                 A[ LDA*i+j ].real=ap[ k ].real;
                 A[ LDA*i+j ].imag=ap[ k ].imag;
              }
           for( i=0, k=0; i<*n; i++ )
              for( j=0; j<i+1; j++, k++ ) {
                 AP[ k ].real=A[ LDA*i+j ].real;
	         AP[ k ].imag=A[ LDA*i+j ].imag;
              }
        }
        cblas_ctpsv( CblasRowMajor, uplo, trans, diag, *n, AP, x, *incx );
        free(A);
        free(AP);
     }
  }
  else if (*order == TEST_COL_MJR)
     cblas_ctpsv( CblasColMajor, uplo, trans, diag, *n, ap, x, *incx );
  else
     cblas_ctpsv( UNDEFINED, uplo, trans, diag, *n, ap, x, *incx );
}

void F77_ctrmv(Int *order, char *uplow, char *transp, char *diagn,
     Int *n, CBLAS_TEST_COMPLEX *a, Int *lda, CBLAS_TEST_COMPLEX *x,
      Int *incx) {
  CBLAS_TEST_COMPLEX *A;
  Int i,j,LDA;
  enum CBLAS_TRANSPOSE trans;
  enum CBLAS_UPLO uplo;
  enum CBLAS_DIAG diag;

  get_transpose_type(transp,&trans);
  get_uplo_type(uplow,&uplo);
  get_diag_type(diagn,&diag);

  if (*order == TEST_ROW_MJR) {
     LDA=*n+1;
     A=(CBLAS_TEST_COMPLEX*)malloc((*n)*LDA*sizeof(CBLAS_TEST_COMPLEX));
     for( i=0; i<*n; i++ )
       for( j=0; j<*n; j++ ) {
	  A[ LDA*i+j ].real=a[ (*lda)*j+i ].real;
          A[ LDA*i+j ].imag=a[ (*lda)*j+i ].imag;
       }
     cblas_ctrmv(CblasRowMajor, uplo, trans, diag, *n, A, LDA, x, *incx);
     free(A);
  }
  else if (*order == TEST_COL_MJR)
     cblas_ctrmv(CblasColMajor, uplo, trans, diag, *n, a, *lda, x, *incx);
  else
     cblas_ctrmv(UNDEFINED, uplo, trans, diag, *n, a, *lda, x, *incx);
}
void F77_ctrsv(Int *order, char *uplow, char *transp, char *diagn,
       Int *n, CBLAS_TEST_COMPLEX *a, Int *lda, CBLAS_TEST_COMPLEX *x,
              Int *incx) {
  CBLAS_TEST_COMPLEX *A;
  Int i,j,LDA;
  enum CBLAS_TRANSPOSE trans;
  enum CBLAS_UPLO uplo;
  enum CBLAS_DIAG diag;

  get_transpose_type(transp,&trans);
  get_uplo_type(uplow,&uplo);
  get_diag_type(diagn,&diag);

  if (*order == TEST_ROW_MJR) {
     LDA = *n+1;
     A =(CBLAS_TEST_COMPLEX* )malloc((*n)*LDA*sizeof(CBLAS_TEST_COMPLEX ) );
     for( i=0; i<*n; i++ )
        for( j=0; j<*n; j++ ) {
           A[ LDA*i+j ].real=a[ (*lda)*j+i ].real;
	   A[ LDA*i+j ].imag=a[ (*lda)*j+i ].imag;
	}
     cblas_ctrsv(CblasRowMajor, uplo, trans, diag, *n, A, LDA, x, *incx );
     free(A);
   }
   else if (*order == TEST_COL_MJR)
     cblas_ctrsv(CblasColMajor, uplo, trans, diag, *n, a, *lda, x, *incx );
   else
     cblas_ctrsv(UNDEFINED, uplo, trans, diag, *n, a, *lda, x, *incx );
}

void F77_chpr(Int *order, char *uplow, Int *n, float *alpha,
	     CBLAS_TEST_COMPLEX *x, Int *incx, CBLAS_TEST_COMPLEX *ap) {
  CBLAS_TEST_COMPLEX *A, *AP;
  Int i,j,k,LDA;
  enum CBLAS_UPLO uplo;

  get_uplo_type(uplow,&uplo);

  if (*order == TEST_ROW_MJR) {
     if (uplo != CblasUpper && uplo != CblasLower )
        cblas_chpr(CblasRowMajor, UNDEFINED, *n, *alpha, x, *incx, ap );
     else {
        LDA = *n;
        A = (CBLAS_TEST_COMPLEX* )malloc(LDA*LDA*sizeof(CBLAS_TEST_COMPLEX ) );
        AP = ( CBLAS_TEST_COMPLEX* )malloc( (((LDA+1)*LDA)/2)*
		sizeof( CBLAS_TEST_COMPLEX ));
        if (uplo == CblasUpper) {
           for( j=0, k=0; j<*n; j++ )
              for( i=0; i<j+1; i++, k++ ){
                 A[ LDA*i+j ].real=ap[ k ].real;
                 A[ LDA*i+j ].imag=ap[ k ].imag;
              }
           for( i=0, k=0; i<*n; i++ )
              for( j=i; j<*n; j++, k++ ){
                 AP[ k ].real=A[ LDA*i+j ].real;
                 AP[ k ].imag=A[ LDA*i+j ].imag;
              }
        }
        else {
           for( j=0, k=0; j<*n; j++ )
              for( i=j; i<*n; i++, k++ ){
                 A[ LDA*i+j ].real=ap[ k ].real;
       	         A[ LDA*i+j ].imag=ap[ k ].imag;
              }
           for( i=0, k=0; i<*n; i++ )
              for( j=0; j<i+1; j++, k++ ){
                 AP[ k ].real=A[ LDA*i+j ].real;
                 AP[ k ].imag=A[ LDA*i+j ].imag;
              }
        }
        cblas_chpr(CblasRowMajor, uplo, *n, *alpha, x, *incx, AP );
        if (uplo == CblasUpper) {
           for( i=0, k=0; i<*n; i++ )
              for( j=i; j<*n; j++, k++ ){
                 A[ LDA*i+j ].real=AP[ k ].real;
                 A[ LDA*i+j ].imag=AP[ k ].imag;
              }
           for( j=0, k=0; j<*n; j++ )
              for( i=0; i<j+1; i++, k++ ){
                 ap[ k ].real=A[ LDA*i+j ].real;
                 ap[ k ].imag=A[ LDA*i+j ].imag;
              }
        }
        else {
           for( i=0, k=0; i<*n; i++ )
              for( j=0; j<i+1; j++, k++ ){
                 A[ LDA*i+j ].real=AP[ k ].real;
                 A[ LDA*i+j ].imag=AP[ k ].imag;
              }
           for( j=0, k=0; j<*n; j++ )
              for( i=j; i<*n; i++, k++ ){
                 ap[ k ].real=A[ LDA*i+j ].real;
                 ap[ k ].imag=A[ LDA*i+j ].imag;
              }
        }
        free(A);
        free(AP);
     }
  }
  else if (*order == TEST_COL_MJR)
     cblas_chpr(CblasColMajor, uplo, *n, *alpha, x, *incx, ap );
  else
     cblas_chpr(UNDEFINED, uplo, *n, *alpha, x, *incx, ap );
}

void F77_chpr2(Int *order, char *uplow, Int *n, CBLAS_TEST_COMPLEX *alpha,
       CBLAS_TEST_COMPLEX *x, Int *incx, CBLAS_TEST_COMPLEX *y, Int *incy,
       CBLAS_TEST_COMPLEX *ap) {
  CBLAS_TEST_COMPLEX *A, *AP;
  Int i,j,k,LDA;
  enum CBLAS_UPLO uplo;

  get_uplo_type(uplow,&uplo);

  if (*order == TEST_ROW_MJR) {
     if (uplo != CblasUpper && uplo != CblasLower )
        cblas_chpr2( CblasRowMajor, UNDEFINED, *n, alpha, x, *incx, y, 
		     *incy, ap );
     else {
        LDA = *n;
        A=(CBLAS_TEST_COMPLEX*)malloc( LDA*LDA*sizeof(CBLAS_TEST_COMPLEX ) );
        AP=(CBLAS_TEST_COMPLEX*)malloc( (((LDA+1)*LDA)/2)*
	sizeof( CBLAS_TEST_COMPLEX ));
        if (uplo == CblasUpper) {
           for( j=0, k=0; j<*n; j++ )
              for( i=0; i<j+1; i++, k++ ) {
                 A[ LDA*i+j ].real=ap[ k ].real;
	         A[ LDA*i+j ].imag=ap[ k ].imag;
	      }
           for( i=0, k=0; i<*n; i++ )
              for( j=i; j<*n; j++, k++ ) {
                 AP[ k ].real=A[ LDA*i+j ].real;
	         AP[ k ].imag=A[ LDA*i+j ].imag;
	      }
        }
        else {
           for( j=0, k=0; j<*n; j++ )
              for( i=j; i<*n; i++, k++ ) {
	         A[ LDA*i+j ].real=ap[ k ].real;
	         A[ LDA*i+j ].imag=ap[ k ].imag;
	      }
           for( i=0, k=0; i<*n; i++ )
              for( j=0; j<i+1; j++, k++ ) {
                 AP[ k ].real=A[ LDA*i+j ].real;
	         AP[ k ].imag=A[ LDA*i+j ].imag;
	      }
        }
        cblas_chpr2( CblasRowMajor, uplo, *n, alpha, x, *incx, y, *incy, AP );
        if (uplo == CblasUpper) {
           for( i=0, k=0; i<*n; i++ )
              for( j=i; j<*n; j++, k++ ) {
                 A[ LDA*i+j ].real=AP[ k ].real;
                 A[ LDA*i+j ].imag=AP[ k ].imag;
              }
           for( j=0, k=0; j<*n; j++ )
              for( i=0; i<j+1; i++, k++ ) {
                 ap[ k ].real=A[ LDA*i+j ].real;
	         ap[ k ].imag=A[ LDA*i+j ].imag;
              }
        }
        else {
           for( i=0, k=0; i<*n; i++ )
              for( j=0; j<i+1; j++, k++ ) {
                 A[ LDA*i+j ].real=AP[ k ].real;
	         A[ LDA*i+j ].imag=AP[ k ].imag;
              }
           for( j=0, k=0; j<*n; j++ )
              for( i=j; i<*n; i++, k++ ) {
                 ap[ k ].real=A[ LDA*i+j ].real;
	         ap[ k ].imag=A[ LDA*i+j ].imag;
              }
        }
        free(A);
        free(AP);
     }
  }
  else if (*order == TEST_COL_MJR)
     cblas_chpr2( CblasColMajor, uplo, *n, alpha, x, *incx, y, *incy, ap );
  else
     cblas_chpr2( UNDEFINED, uplo, *n, alpha, x, *incx, y, *incy, ap );
}

void F77_cher(Int *order, char *uplow, Int *n, float *alpha,
  CBLAS_TEST_COMPLEX *x, Int *incx, CBLAS_TEST_COMPLEX *a, Int *lda) {
  CBLAS_TEST_COMPLEX *A;
  Int i,j,LDA;
  enum CBLAS_UPLO uplo;

  get_uplo_type(uplow,&uplo);

  if (*order == TEST_ROW_MJR) {
     LDA = *n+1;
     A=(CBLAS_TEST_COMPLEX*)malloc((*n)*LDA*sizeof( CBLAS_TEST_COMPLEX ));

     for( i=0; i<*n; i++ ) 
       for( j=0; j<*n; j++ ) {
	  A[ LDA*i+j ].real=a[ (*lda)*j+i ].real;
          A[ LDA*i+j ].imag=a[ (*lda)*j+i ].imag;
       }

     cblas_cher(CblasRowMajor, uplo, *n, *alpha, x, *incx, A, LDA );
     for( i=0; i<*n; i++ )
       for( j=0; j<*n; j++ ) {
	  a[ (*lda)*j+i ].real=A[ LDA*i+j ].real;
          a[ (*lda)*j+i ].imag=A[ LDA*i+j ].imag;
       }
     free(A);
  }
  else if (*order == TEST_COL_MJR)
     cblas_cher( CblasColMajor, uplo, *n, *alpha, x, *incx, a, *lda );
  else
     cblas_cher( UNDEFINED, uplo, *n, *alpha, x, *incx, a, *lda );
}

void F77_cher2(Int *order, char *uplow, Int *n, CBLAS_TEST_COMPLEX *alpha,
          CBLAS_TEST_COMPLEX *x, Int *incx, CBLAS_TEST_COMPLEX *y, Int *incy,
	  CBLAS_TEST_COMPLEX *a, Int *lda) {

  CBLAS_TEST_COMPLEX *A;
  Int i,j,LDA;
  enum CBLAS_UPLO uplo;

  get_uplo_type(uplow,&uplo);

  if (*order == TEST_ROW_MJR) {
     LDA = *n+1;
     A= ( CBLAS_TEST_COMPLEX* )malloc((*n)*LDA*sizeof(CBLAS_TEST_COMPLEX ) );

     for( i=0; i<*n; i++ ) 
       for( j=0; j<*n; j++ ) {
	  A[ LDA*i+j ].real=a[ (*lda)*j+i ].real;
          A[ LDA*i+j ].imag=a[ (*lda)*j+i ].imag;
       }

     cblas_cher2(CblasRowMajor, uplo, *n, alpha, x, *incx, y, *incy, A, LDA );
     for( i=0; i<*n; i++ )
       for( j=0; j<*n; j++ ) {
	  a[ (*lda)*j+i ].real=A[ LDA*i+j ].real;
          a[ (*lda)*j+i ].imag=A[ LDA*i+j ].imag;
       }
     free(A);
  }
  else if (*order == TEST_COL_MJR)
     cblas_cher2( CblasColMajor, uplo, *n, alpha, x, *incx, y, *incy, a, *lda);
  else
     cblas_cher2( UNDEFINED, uplo, *n, alpha, x, *incx, y, *incy, a, *lda);
}
