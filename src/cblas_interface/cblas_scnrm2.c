//    SPDX-License-Identifier: LGPL-3.0-or-later
/*
    This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
    Copyright (C) 2013-2025 Martin Koehler

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program; if not, write to the Free Software Foundation,
    Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/







#include "cblas.h"
#include "cblas_f77.h"
#include "../flexiblas.h"
#include "cblas_flexiblas.h"

static TLS_STORE uint8_t hook_cblas_scnrm2_pos = 0;


float cblas_scnrm2( const CBLAS_INT N, const void *X, const CBLAS_INT incX)
{
    float nrm2;
    float (*fn)
        ( const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    CBLAS_BACKEND_INIT();
    CBLAS_HOOK_SELECT(scnrm2);
    nrm2 = fn(N,X,incX);
    return nrm2;
}

float flexiblas_chain_cblas_scnrm2( const CBLAS_INT N, const void *X, const CBLAS_INT incX)
{
    float nrm2;
    float (*fn)
        ( const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    CBLAS_HOOK_ADVANCE(scnrm2);
    nrm2 = fn(N,X,incX);
    return nrm2;
}

float flexiblas_real_cblas_scnrm2( const CBLAS_INT N, const void *X, const CBLAS_INT incX)
{
    float nrm2;
#ifdef F77_INT
    F77_INT F77_N=N, F77_incX=incX;
#else
#define F77_N N
#define F77_incX incX
#endif
    if ( current_backend->blas.scnrm2.cblas_function != NULL ) {
        float (*fn)
            ( const CBLAS_INT N, const void *X, const CBLAS_INT incX);
        *(void **) &fn = current_backend->blas.scnrm2.cblas_function;
        nrm2 = fn(N,X,incX);
    } else {
        nrm2 =  FC_GLOBAL(scnrm2,SCNRM2)( (blasint *)(uintptr_t)&F77_N, (float complex*)(uintptr_t) X, (blasint *)(uintptr_t)&F77_incX);
    }
    return nrm2;
}

