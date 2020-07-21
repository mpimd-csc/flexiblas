/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * Linking FlexiBLAS statically or dynamically with other modules is making a
 * combined work based on FlexiBLAS. Thus, the terms and conditions of the GNU
 * General Public License cover the whole combination.
 *
 * As a special exception, the copyright holders of FlexiBLAS give you permission
 * to combine FlexiBLAS program with free software programs or libraries that are
 * released under the GNU LGPL and with independent modules that communicate with
 * FlexiBLAS solely through the BLAS/LAPACK interface as provided by the
 * BLAS/LAPACK reference implementation. You may copy and distribute such a system
 * following the terms of the GNU GPL for FlexiBLAS and the licenses of the other
 * code concerned, provided that you include the source code of that other code
 * when and as the GNU GPL requires distribution of source code and provided that
 * you do not modify the BLAS/LAPACK interface.
 *
 * Note that people who make modified versions of FlexiBLAS are not obligated to
 * grant this special exception for their modified versions; it is their choice
 * whether to do so. The GNU General Public License gives permission to release a
 * modified version without this exception; this exception also makes it possible
 * to release a modified version which carries forward this exception. If you
 * modify the BLAS/LAPACK interface, this exception does not apply to your
 * modified version of FlexiBLAS, and you must remove this exception when you
 * distribute your modified version.
 *
 * This exception is an additional permission under section 7 of the GNU General
 * Public License, version 3 (“GPLv3”)
 *
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) Martin Koehler, 2013-2020
 */



#include "cblas.h"
#include "cblas_f77.h"
#include "../flexiblas.h"
#include "cblas_flexiblas.h"

static TLS_STORE uint8_t hook_cblas_ztrmm_pos = 0;
void cblas_ztrmm(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
        const CBLAS_UPLO Uplo, const  CBLAS_TRANSPOSE TransA,
        const CBLAS_DIAG Diag, const int M, const int N,
        const void *alpha, const void  *A, const int lda,
        void  *B, const int ldb)
{
    void (*fn)
        (const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
         const CBLAS_UPLO Uplo, const  CBLAS_TRANSPOSE TransA,
         const CBLAS_DIAG Diag, const int M, const int N,
         const void *alpha, const void  *A, const int lda,
         void  *B, const int ldb);
    CBLAS_BACKEND_INIT();
    CBLAS_HOOK_SELECT(ztrmm);

    fn(layout,Side,Uplo,TransA,Diag,M,N,alpha,A,lda,B,ldb);

}

void flexiblas_chain_cblas_ztrmm(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
        const CBLAS_UPLO Uplo, const  CBLAS_TRANSPOSE TransA,
        const CBLAS_DIAG Diag, const int M, const int N,
        const void *alpha, const void  *A, const int lda,
        void  *B, const int ldb)
{
    void (*fn)
        (const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
         const CBLAS_UPLO Uplo, const  CBLAS_TRANSPOSE TransA,
         const CBLAS_DIAG Diag, const int M, const int N,
         const void *alpha, const void  *A, const int lda,
         void  *B, const int ldb);
    CBLAS_HOOK_ADVANCE(ztrmm);

    fn(layout,Side,Uplo,TransA,Diag,M,N,alpha,A,lda,B,ldb);

}

void flexiblas_real_cblas_ztrmm(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
        const CBLAS_UPLO Uplo, const  CBLAS_TRANSPOSE TransA,
        const CBLAS_DIAG Diag, const int M, const int N,
        const void *alpha, const void  *A, const int lda,
        void  *B, const int ldb)
{
    char UL, TA, SD, DI;
#define F77_TA &TA
#define F77_UL &UL
#define F77_SD &SD
#define F77_DI &DI

#ifdef F77_INT
    F77_INT F77_M=M, F77_N=N, F77_lda=lda, F77_ldb=ldb;
#else
#define F77_M M
#define F77_N N
#define F77_lda lda
#define F77_ldb ldb
#endif

    if ( current_backend->blas.ztrmm.cblas_function != NULL ) {

        void (*fn)
            (const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
             const CBLAS_UPLO Uplo, const  CBLAS_TRANSPOSE TransA,
             const CBLAS_DIAG Diag, const int M, const int N,
             const void *alpha, const void  *A, const int lda,
             void  *B, const int ldb)
            = current_backend->blas.ztrmm.cblas_function;
        fn(layout,Side,Uplo,TransA,Diag,M,N,alpha,A,lda,B,ldb);
    } else {
        extern int CBLAS_CallFromC;
        extern int RowMajorStrg;
        RowMajorStrg = 0;
        CBLAS_CallFromC = 1;

        if( layout == CblasColMajor )
        {
            if( Side == CblasRight ) SD='R';
            else if ( Side == CblasLeft ) SD='L';
            else
            {
                cblas_xerbla(2, "cblas_ztrmm", "Illegal Side setting, %d\n", Side);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }
            if( Uplo == CblasUpper ) UL='U';
            else if ( Uplo == CblasLower ) UL='L';
            else
            {
                cblas_xerbla(3, "cblas_ztrmm", "Illegal Uplo setting, %d\n", Uplo);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }

            if( TransA == CblasTrans ) TA ='T';
            else if ( TransA == CblasConjTrans ) TA='C';
            else if ( TransA == CblasNoTrans )   TA='N';
            else
            {
                cblas_xerbla(4, "cblas_ztrmm", "Illegal Trans setting, %d\n", TransA);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }

            if( Diag == CblasUnit ) DI='U';
            else if ( Diag == CblasNonUnit ) DI='N';
            else cblas_xerbla(5, "cblas_ztrmm",
                    "Illegal Diag setting, %d\n", Diag);

#ifdef F77_CHAR
            F77_UL = C2F_CHAR(&UL);
            F77_TA = C2F_CHAR(&TA);
            F77_SD = C2F_CHAR(&SD);
            F77_DI = C2F_CHAR(&DI);
#endif

            FC_GLOBAL(ztrmm,ZTRMM)(F77_SD, F77_UL, F77_TA, F77_DI, &F77_M, &F77_N, alpha, A, &F77_lda, B, &F77_ldb);
        } else if (layout == CblasRowMajor)
        {
            RowMajorStrg = 1;
            if( Side == CblasRight ) SD='L';
            else if ( Side == CblasLeft ) SD='R';
            else
            {
                cblas_xerbla(2, "cblas_ztrmm", "Illegal Side setting, %d\n", Side);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }

            if( Uplo == CblasUpper ) UL='L';
            else if ( Uplo == CblasLower ) UL='U';
            else
            {
                cblas_xerbla(3, "cblas_ztrmm", "Illegal Uplo setting, %d\n", Uplo);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }

            if( TransA == CblasTrans ) TA ='T';
            else if ( TransA == CblasConjTrans ) TA='C';
            else if ( TransA == CblasNoTrans )   TA='N';
            else
            {
                cblas_xerbla(4, "cblas_ztrmm", "Illegal Trans setting, %d\n", TransA);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }

            if( Diag == CblasUnit ) DI='U';
            else if ( Diag == CblasNonUnit ) DI='N';
            else
            {
                cblas_xerbla(5, "cblas_ztrmm", "Illegal Diag setting, %d\n", Diag);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }

#ifdef F77_CHAR
            F77_UL = C2F_CHAR(&UL);
            F77_TA = C2F_CHAR(&TA);
            F77_SD = C2F_CHAR(&SD);
            F77_DI = C2F_CHAR(&DI);
#endif

            FC_GLOBAL(ztrmm,ZTRMM)(F77_SD, F77_UL, F77_TA, F77_DI, &F77_N, &F77_M, alpha, A, &F77_lda, B, &F77_ldb);
        }
        else  cblas_xerbla(1, "cblas_ztrmm", "Illegal layout setting, %d\n", layout);
        CBLAS_CallFromC = 0;
        RowMajorStrg = 0;

    }
    return;
}
