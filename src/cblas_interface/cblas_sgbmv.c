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
 * Copyright (C) Martin Koehler, 2013-2023
 */





#include "cblas.h"
#include "cblas_f77.h"
#include "../flexiblas.h"
#include "cblas_flexiblas.h"

static TLS_STORE uint8_t hook_cblas_sgbmv_pos = 0;

void cblas_sgbmv(const CBLAS_LAYOUT layout,
        const CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N,
        const CBLAS_INT KL, const CBLAS_INT KU,
        const float alpha, const float  *A, const CBLAS_INT lda,
        const float  *X, const CBLAS_INT incX, const float beta,
        float  *Y, const CBLAS_INT incY)
{
    void (*fn)(const CBLAS_LAYOUT layout,
            const CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N,
            const CBLAS_INT KL, const CBLAS_INT KU,
            const float alpha, const float  *A, const CBLAS_INT lda,
            const float  *X, const CBLAS_INT incX, const float beta,
            float  *Y, const CBLAS_INT incY);
    CBLAS_BACKEND_INIT();
    CBLAS_HOOK_SELECT(sgbmv);
    fn(layout, TransA, M,N,KL,KU,alpha,A,lda,X,incX,beta, Y, incY);

}

void flexiblas_chain_cblas_sgbmv(const CBLAS_LAYOUT layout,
        const CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N,
        const CBLAS_INT KL, const CBLAS_INT KU,
        const float alpha, const float  *A, const CBLAS_INT lda,
        const float  *X, const CBLAS_INT incX, const float beta,
        float  *Y, const CBLAS_INT incY)
{

    void (*fn)(const CBLAS_LAYOUT layout,
            const CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N,
            const CBLAS_INT KL, const CBLAS_INT KU,
            const float alpha, const float  *A, const CBLAS_INT lda,
            const float  *X, const CBLAS_INT incX, const float beta,
            float  *Y, const CBLAS_INT incY);
    CBLAS_HOOK_ADVANCE(sgbmv);
    fn(layout, TransA, M,N,KL,KU,alpha,A,lda,X,incX,beta, Y, incY);


}

void flexiblas_real_cblas_sgbmv(const CBLAS_LAYOUT layout,
        const CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N,
        const CBLAS_INT KL, const CBLAS_INT KU,
        const float alpha, const float  *A, const CBLAS_INT lda,
        const float  *X, const CBLAS_INT incX, const float beta,
        float  *Y, const CBLAS_INT incY)
{
    char TA;
#define F77_TA &TA
#ifdef F77_INT
    F77_INT F77_M=M, F77_N=N, F77_lda=lda, F77_incX=incX, F77_incY=incY;
    F77_INT F77_KL=KL,F77_KU=KU;
#else
#define F77_M M
#define F77_N N
#define F77_lda lda
#define F77_KL KL
#define F77_KU KU
#define F77_incX incX
#define F77_incY incY
#endif
    if ( current_backend->blas.sgbmv.cblas_function != NULL ) {
        void (*fn)(const CBLAS_LAYOUT layout,
                const CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N,
                const CBLAS_INT KL, const CBLAS_INT KU,
                const float alpha, const float  *A, const CBLAS_INT lda,
                const float  *X, const CBLAS_INT incX, const float beta,
                float  *Y, const CBLAS_INT incY);
        *(void **) &fn = current_backend->blas.sgbmv.cblas_function;
        fn(layout, TransA, M,N,KL,KU,alpha,A,lda,X,incX,beta, Y, incY);
    } else {
        extern int CBLAS_CallFromC;
        extern int RowMajorStrg;
        RowMajorStrg = 0;

        CBLAS_CallFromC = 1;
        if (layout == CblasColMajor)
        {
            if (TransA == CblasNoTrans) TA = 'N';
            else if (TransA == CblasTrans) TA = 'T';
            else if (TransA == CblasConjTrans) TA = 'C';
            else
            {
                cblas_xerbla(2, "cblas_sgbmv","Illegal TransA setting, %d\n", TransA);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }
#ifdef F77_CHAR
            F77_TA = C2F_CHAR(&TA);
#endif
            FC_GLOBAL(sgbmv,SGBMV)(F77_TA, &F77_M, &F77_N, &F77_KL, &F77_KU, &alpha,
                    A, &F77_lda, X, &F77_incX, &beta, Y, &F77_incY);
        }
        else if (layout == CblasRowMajor)
        {
            RowMajorStrg = 1;
            if (TransA == CblasNoTrans) TA = 'T';
            else if (TransA == CblasTrans) TA = 'N';
            else if (TransA == CblasConjTrans) TA = 'N';
            else
            {
                cblas_xerbla(2, "cblas_sgbmv","Illegal TransA setting, %d\n", TransA);
                CBLAS_CallFromC = 0;
                RowMajorStrg = 0;
                return;
            }
#ifdef F77_CHAR
            F77_TA = C2F_CHAR(&TA);
#endif
            FC_GLOBAL(sgbmv,SGBMV)(F77_TA, &F77_N, &F77_M, &F77_KU, &F77_KL, &alpha,
                    A ,&F77_lda, X,&F77_incX, &beta, Y, &F77_incY);
        }
        else cblas_xerbla(1, "cblas_sgbmv", "Illegal layout setting, %d\n", layout);
        CBLAS_CallFromC = 0;
        RowMajorStrg = 0;
    }
    return;
}
