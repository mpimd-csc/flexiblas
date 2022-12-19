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
 * Copyright (C) Martin Koehler, 2013-2022
 */





#include "cblas.h"
#include "cblas_f77.h"
#include "../flexiblas.h"
#include "cblas_flexiblas.h"

static TLS_STORE uint8_t hook_cblas_csyrk_pos = 0;

void cblas_csyrk(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K,
        const void *alpha, const void  *A, const CBLAS_INT lda,
        const void *beta, void  *C, const CBLAS_INT ldc)
{
        void (*fn)
            (const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
             const CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K,
             const void *alpha, const void  *A, const CBLAS_INT lda,
             const void *beta, void  *C, const CBLAS_INT ldc);
        CBLAS_BACKEND_INIT();
        CBLAS_HOOK_SELECT(csyrk);

        fn(layout,Uplo,Trans,N,K,alpha,A,lda,beta,C,ldc);

}

void flexiblas_chain_cblas_csyrk(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K,
        const void *alpha, const void  *A, const CBLAS_INT lda,
        const void *beta, void  *C, const CBLAS_INT ldc)
{
        void (*fn)
            (const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
             const CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K,
             const void *alpha, const void  *A, const CBLAS_INT lda,
             const void *beta, void  *C, const CBLAS_INT ldc);
        CBLAS_BACKEND_INIT();
        CBLAS_HOOK_ADVANCE(csyrk);

        fn(layout,Uplo,Trans,N,K,alpha,A,lda,beta,C,ldc);


}

void flexiblas_real_cblas_csyrk(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
        const CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K,
        const void *alpha, const void  *A, const CBLAS_INT lda,
        const void *beta, void  *C, const CBLAS_INT ldc)
{
    char UL, TR;
#define F77_TR &TR
#define F77_UL &UL

#ifdef F77_INT
    F77_INT F77_N=N, F77_K=K, F77_lda=lda;
    F77_INT F77_ldc=ldc;
#else
#define F77_N N
#define F77_K K
#define F77_lda lda
#define F77_ldc ldc
#endif

    if ( current_backend->blas.csyrk.cblas_function != NULL ) {
        void (*fn)
            (const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
             const CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K,
             const void *alpha, const void  *A, const CBLAS_INT lda,
             const void *beta, void  *C, const CBLAS_INT ldc);
        *(void **) & fn = current_backend->blas.csyrk.cblas_function;
        fn(layout,Uplo,Trans,N,K,alpha,A,lda,beta,C,ldc);
    } else {
        extern int CBLAS_CallFromC;
        extern int RowMajorStrg;
        RowMajorStrg = 0;
        CBLAS_CallFromC = 1;

        if( layout == CblasColMajor )
        {

            if( Uplo == CblasUpper) UL='U';
            else if ( Uplo == CblasLower ) UL='L';
            else
            {
                cblas_xerbla(2, "cblas_csyrk", "Illegal Uplo setting, %d\n", Uplo);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }

            if( Trans == CblasTrans) TR ='T';
            else if ( Trans == CblasConjTrans ) TR='C';
            else if ( Trans == CblasNoTrans )   TR='N';
            else
            {
                cblas_xerbla(3, "cblas_csyrk", "Illegal Trans setting, %d\n", Trans);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }


#ifdef F77_CHAR
            F77_UL = C2F_CHAR(&UL);
            F77_TR = C2F_CHAR(&TR);
#endif

            FC_GLOBAL(csyrk,CSYRK)(F77_UL, F77_TR, &F77_N, &F77_K, alpha, A, &F77_lda,
                    beta, C, &F77_ldc);
        } else if (layout == CblasRowMajor)
        {
            RowMajorStrg = 1;
            if( Uplo == CblasUpper) UL='L';
            else if ( Uplo == CblasLower ) UL='U';
            else
            {
                cblas_xerbla(3, "cblas_csyrk", "Illegal Uplo setting, %d\n", Uplo);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }
            if( Trans == CblasTrans) TR ='N';
            else if ( Trans == CblasConjTrans ) TR='N';
            else if ( Trans == CblasNoTrans )   TR='T';
            else
            {
                cblas_xerbla(3, "cblas_csyrk", "Illegal Trans setting, %d\n", Trans);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }

#ifdef F77_CHAR
            F77_UL = C2F_CHAR(&UL);
            F77_TR = C2F_CHAR(&TR);
#endif

            FC_GLOBAL(csyrk,CSYRK)(F77_UL, F77_TR, &F77_N, &F77_K, alpha, A, &F77_lda,
                    beta, C, &F77_ldc);
        }
        else cblas_xerbla(1, "cblas_csyrk", "Illegal layout setting, %d\n", layout);
        CBLAS_CallFromC = 0;
        RowMajorStrg = 0;

    }
    return;
}

