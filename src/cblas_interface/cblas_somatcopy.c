//    SPDX-License-Identifier: LGPL-3.0-or-later
/*
    This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
    Copyright (C) 2013-2024 Martin Koehler

    This program is free software: you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by the Free
    Software Foundation, either version 3 of the License, or (at your option)
    any later version.

    This program is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
    more details.

    You should have received a copy of the GNU General Public License along
    with this program. If not, see <https://www.gnu.org/licenses/>.
 */





#include "cblas.h"
#include "cblas_f77.h"
#include "../flexiblas.h"
#include "cblas_flexiblas.h"

static TLS_STORE uint8_t hook_cblas_somatcopy_pos = 0;

void cblas_somatcopy(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS,
        const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, const float *a, const CBLAS_INT clda,
        float *b, const CBLAS_INT cldb)
{
    void (*fn)(const CBLAS_ORDER, const CBLAS_TRANSPOSE, const CBLAS_INT, const CBLAS_INT, const float, const float *, const CBLAS_INT, float *, const CBLAS_INT) ;
    CBLAS_BACKEND_INIT();
    CBLAS_HOOK_SELECT(somatcopy);

    fn(CORDER, CTRANS, crows, ccols, calpha, a, clda, b, cldb);

}

void flexiblas_chain_cblas_somatcopy(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS,
        const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, const float *a, const CBLAS_INT clda,
        float *b, const CBLAS_INT cldb)
{
    void (*fn)(const CBLAS_ORDER, const CBLAS_TRANSPOSE, const CBLAS_INT, const CBLAS_INT, const float, const float *, const CBLAS_INT, float *, const CBLAS_INT);
    CBLAS_HOOK_ADVANCE(somatcopy);
    fn(CORDER, CTRANS, crows, ccols, calpha, a, clda, b, cldb);

}

void flexiblas_real_cblas_somatcopy(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS,
        const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, const float *a, const CBLAS_INT clda,
        float *b, const CBLAS_INT cldb)
{
#ifdef F77_INT
    F77_INT F77_ROWS=crows;
    F77_INT F77_COLS=ccols;
    F77_INT F77_LDA =clda;
    F77_INT F77_LDB =cldb;
#else
#define F77_ROWS crows
#define F77_COLS ccols
#define F77_LDA  clda
#define F77_LDB  cldb
#endif

    if ( current_backend->blas.somatcopy.cblas_function != NULL ) {
        void (*fn)(const CBLAS_ORDER, const CBLAS_TRANSPOSE, const CBLAS_INT, const CBLAS_INT, const float, const float *, const CBLAS_INT, float *, const CBLAS_INT) ;
        *(void **) &fn = current_backend->blas.somatcopy.cblas_function;
        fn(CORDER, CTRANS, crows, ccols, calpha, a, clda, b, cldb);
    } else {
        char ORDER[2]=" ";
        char TRANS[2]=" ";
        switch(CORDER){
            case CblasColMajor:
                ORDER[0]='C';
                break;
            case CblasRowMajor:
                ORDER[0]='R';
                break;
            default:
                ORDER[0]='X';
        }
        switch(CTRANS){
            case CblasNoTrans:
            case CblasConjNoTrans:
                TRANS[0]='N';
                break;
            case CblasTrans:
            case CblasConjTrans:
                TRANS[0]='T';
                break;
            default:
                TRANS[0]='X';
        }
        FC_GLOBAL(somatcopy,SOMATCOPY)( ORDER, TRANS, (blasint *)(uintptr_t)&F77_ROWS, (blasint *)(uintptr_t)&F77_COLS, (float *)(uintptr_t) &calpha, (float *)(uintptr_t) a, (blasint *)(uintptr_t)&F77_LDA, b, (blasint *)(uintptr_t)&F77_LDB, 1, 1);
    }
}

