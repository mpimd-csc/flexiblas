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

static TLS_STORE uint8_t hook_cblas_srotm_pos = 0;

void cblas_srotm( const CBLAS_INT N, float *X, const CBLAS_INT incX, float *Y,
        const CBLAS_INT incY, const float *P)
{
    void (*fn)( const CBLAS_INT N, float *X, const CBLAS_INT incX, float *Y,
            const CBLAS_INT incY, const float *P);
    CBLAS_BACKEND_INIT();
    CBLAS_HOOK_SELECT(srotm);
    fn(N,X,incX,Y,incY,P);

}

void flexiblas_chain_cblas_srotm( const CBLAS_INT N, float *X, const CBLAS_INT incX, float *Y,
        const CBLAS_INT incY, const float *P)
{
    void (*fn)( const CBLAS_INT N, float *X, const CBLAS_INT incX, float *Y,
            const CBLAS_INT incY, const float *P);

    CBLAS_HOOK_ADVANCE(srotm);
    fn(N,X,incX,Y,incY,P);


}

void flexiblas_real_cblas_srotm( const CBLAS_INT N, float *X, const CBLAS_INT incX, float *Y,
        const CBLAS_INT incY, const float *P)
{
#ifdef F77_INT
    F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else
#define F77_N N
#define F77_incX incX
#define F77_incY incY
#endif

    if ( current_backend->blas.srotm.cblas_function != NULL ) {
        void (*fn)( const CBLAS_INT N, float *X, const CBLAS_INT incX, float *Y,
                const CBLAS_INT incY, const float *P);
        *(void**) &fn = current_backend->blas.srotm.cblas_function;
        fn(N,X,incX,Y,incY,P);
    } else {
        FC_GLOBAL(srotm,SROTM)( (blasint *)(uintptr_t)&F77_N, X, (blasint *)(uintptr_t)&F77_incX, Y, (blasint *)(uintptr_t)&F77_incY, (float *)(uintptr_t) P);
    }
    return;
}
