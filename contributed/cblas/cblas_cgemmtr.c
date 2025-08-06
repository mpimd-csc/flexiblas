/*
 *
 * cblas_cgemmtr.c
 * This program is a C interface to cgemmtr.
 * Written by Martin Koehler, MPI Magdeburg
 * 06/24/2024
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_cgemmtr)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo, const CBLAS_TRANSPOSE TransA,
        const CBLAS_TRANSPOSE TransB, const CBLAS_INT N,
        const CBLAS_INT K, const void *alpha, const void  *A,
        const CBLAS_INT lda, const void  *B, const CBLAS_INT ldb,
        const void *beta, void  *C, const CBLAS_INT ldc)
{
    char TA, TB;
    char UL;
#ifdef F77_CHAR
    F77_CHAR F77_TA, F77_TB, F77_UL;
#else
#define F77_TA &TA
#define F77_TB &TB
#define F77_UL &UL
#endif

#ifdef F77_INT
    F77_INT F77_N=N, F77_K=K, F77_lda=lda, F77_ldb=ldb;
    F77_INT F77_ldc=ldc;
#else
#define F77_N N
#define F77_K K
#define F77_lda lda
#define F77_ldb ldb
#define F77_ldc ldc
#endif

    extern int CBLAS_CallFromC;
    extern int RowMajorStrg;
    RowMajorStrg = 0;
    CBLAS_CallFromC = 1;


    if( layout == CblasColMajor )
    {
        if ( Uplo == CblasUpper ) UL = 'U';
        else if (Uplo == CblasLower) UL= 'L';
        else {
            API_SUFFIX(cblas_xerbla)(2, "cblas_cgemmtr", "Illegal Uplo setting, %d\n", Uplo);
            CBLAS_CallFromC = 0;
            RowMajorStrg = 0;
            return;
        }

        if(TransA == CblasTrans) TA='T';
        else if ( TransA == CblasConjTrans ) TA='C';
        else if ( TransA == CblasNoTrans )   TA='N';
        else
        {
            API_SUFFIX(cblas_xerbla)(3, "cblas_cgemmtr", "Illegal TransA setting, %d\n", TransA);
            CBLAS_CallFromC = 0;
            RowMajorStrg = 0;
            return;
        }

        if(TransB == CblasTrans) TB='T';
        else if ( TransB == CblasConjTrans ) TB='C';
        else if ( TransB == CblasNoTrans )   TB='N';
        else
        {
            API_SUFFIX(cblas_xerbla)(4, "cblas_cgemmtr", "Illegal TransB setting, %d\n", TransB);
            CBLAS_CallFromC = 0;
            RowMajorStrg = 0;
            return;
        }

#ifdef F77_CHAR
        F77_TA = C2F_CHAR(&TA);
        F77_TB = C2F_CHAR(&TB);
        F77_UL = C2F_CHAR(&UL);
#endif

        F77_cgemmtr(F77_UL, F77_TA, F77_TB, &F77_N, &F77_K, alpha, A,
                &F77_lda, B, &F77_ldb, beta, C, &F77_ldc);
    }
    else if (layout == CblasRowMajor)
    {
        RowMajorStrg = 1;

        if ( Uplo == CblasUpper ) UL = 'L';
        else if (Uplo == CblasLower) UL= 'U';
        else {
            API_SUFFIX(cblas_xerbla)(2, "cblas_cgemmtr", "Illegal Uplo setting, %d\n", Uplo);
            CBLAS_CallFromC = 0;
            RowMajorStrg = 0;
            return;
        }

        if(TransA == CblasTrans) TB='T';
        else if ( TransA == CblasConjTrans ) TB='C';
        else if ( TransA == CblasNoTrans )   TB='N';
        else
        {
            API_SUFFIX(cblas_xerbla)(3, "cblas_cgemmtr", "Illegal TransA setting, %d\n", TransA);
            CBLAS_CallFromC = 0;
            RowMajorStrg = 0;
            return;
        }
        if(TransB == CblasTrans) TA='T';
        else if ( TransB == CblasConjTrans ) TA='C';
        else if ( TransB == CblasNoTrans )   TA='N';
        else
        {
            API_SUFFIX(cblas_xerbla)(4, "cblas_cgemmtr", "Illegal TransB setting, %d\n", TransB);
            CBLAS_CallFromC = 0;
            RowMajorStrg = 0;
            return;
        }
#ifdef F77_CHAR
        F77_TA = C2F_CHAR(&TA);
        F77_TB = C2F_CHAR(&TB);
        F77_UL = C2F_CHAR(&UL);

#endif

        F77_cgemmtr(F77_UL, F77_TA, F77_TB, &F77_N, &F77_K, alpha, B,
                &F77_ldb, A, &F77_lda, beta, C, &F77_ldc);
    }
    else API_SUFFIX(cblas_xerbla)(1, "cblas_cgemmtr", "Illegal layout setting, %d\n", layout);
    CBLAS_CallFromC = 0;
    RowMajorStrg = 0;
    return;
}
