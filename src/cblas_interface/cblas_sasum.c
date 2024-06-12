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


static TLS_STORE uint8_t hook_cblas_sasum_pos = 0;

float cblas_sasum( const CBLAS_INT N, const float *X, const CBLAS_INT incX)
{
    float asum;
    float (*fn)(const CBLAS_INT , const float *, const CBLAS_INT );

    CBLAS_BACKEND_INIT();
    CBLAS_HOOK_SELECT(sasum);
    asum = fn(N,X,incX);

    return asum;
}

float flexiblas_chain_cblas_sasum( const CBLAS_INT N, const float *X, const CBLAS_INT incX)
{
    float asum;
    float (*fn)(const CBLAS_INT , const float *, const CBLAS_INT );

    CBLAS_HOOK_ADVANCE(sasum);
    asum = fn(N,X,incX);
    return asum;
}


float flexiblas_real_cblas_sasum( const CBLAS_INT N, const float *X, const CBLAS_INT incX)
{
    float asum;
#ifdef F77_INT
    F77_INT F77_N=N, F77_incX=incX;
#else
#define F77_N N
#define F77_incX incX
#endif
    if ( current_backend->blas.sasum.cblas_function != NULL ) {

        float (*fn)(const CBLAS_INT , const float *, const CBLAS_INT );
        *(void **) & fn = current_backend->blas.sasum.cblas_function;
        asum = fn(N,X,incX);

    } else {
        asum = FC_GLOBAL(sasum,SASUM)( (blasint *)(uintptr_t)&F77_N, (float *)(uintptr_t) X, (blasint *)(uintptr_t)&F77_incX);
    }

    return asum;
}

