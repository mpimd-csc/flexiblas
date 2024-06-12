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

static TLS_STORE uint8_t hook_cblas_zgeadd_pos = 0;


void cblas_zgeadd(const CBLAS_ORDER CORDER,
        const CBLAS_INT crows, const CBLAS_INT ccols, const void *alpha, void *ca, const CBLAS_INT clda,
        const void *beta, void *cb, const CBLAS_INT cldb)
{
    void (*fn)(const CBLAS_ORDER, const CBLAS_INT, const CBLAS_INT, const void *, void *, const CBLAS_INT, const void *, void *, const CBLAS_INT);
    CBLAS_BACKEND_INIT();
    CBLAS_HOOK_SELECT(zgeadd);

    fn(CORDER, crows, ccols, alpha, ca, clda, beta, cb, cldb);

}

void flexiblas_chain_cblas_zgeadd(const CBLAS_ORDER CORDER,
        const CBLAS_INT crows, const CBLAS_INT ccols, const void *alpha, void *ca, const CBLAS_INT clda,
        const void *beta, void *cb, const CBLAS_INT cldb)
{
    void (*fn)(const CBLAS_ORDER, const CBLAS_INT, const CBLAS_INT, const void *, void *, const CBLAS_INT, const void *, void *, const CBLAS_INT);
    CBLAS_HOOK_ADVANCE(zgeadd);

    fn(CORDER, crows, ccols, alpha, ca, clda, beta, cb, cldb);

}

void flexiblas_real_cblas_zgeadd(const CBLAS_ORDER CORDER,
        const CBLAS_INT crows, const CBLAS_INT ccols, const void *alpha, void *ca, const CBLAS_INT clda,
        const void *beta, void *cb, const CBLAS_INT cldb)
{
    const double complex *calpha = ((const double complex*) alpha);
    const double complex *cbeta  = ((const double complex*) beta);
    double complex * a = (double complex *) ca;
    double complex * b = (double complex *) cb;


#ifdef F77_INT
    F77_INT F77_LDA =clda;
    F77_INT F77_LDB =cldb;
#else
#define F77_LDA  clda
#define F77_LDB  cldb
#endif

    if ( current_backend->blas.zgeadd.cblas_function != NULL ) {
        void (*fn)(const CBLAS_ORDER, const CBLAS_INT, const CBLAS_INT, const double complex*, double complex *, const CBLAS_INT, const double complex*, double complex *, const CBLAS_INT);
        *(void **) & fn = current_backend->blas.zgeadd.cblas_function;
        fn(CORDER, crows, ccols, calpha, a, clda, cbeta, b, cldb);
    } else {
#ifdef F77_INT
        F77_INT t = 0;
        F77_INT rows = crows;
        F77_INT cols = ccols;
#else
        CBLAS_INT t = 0;
        CBLAS_INT rows = crows;
        CBLAS_INT cols = ccols;
#endif

        if ( CORDER == CblasRowMajor ) {
            t = rows;
            rows = cols;
            cols = t;
        }
        FC_GLOBAL(zgeadd,ZGEADD)( &rows, &cols, (void *)(uintptr_t) calpha, (void *)(uintptr_t) a, (blasint *)(uintptr_t)&F77_LDA, (void *)(uintptr_t) cbeta, b, (blasint *)(uintptr_t)&F77_LDB);
    }
}


