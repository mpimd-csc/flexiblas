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






#include "flexiblas_config.h"
#include "cblas.h"
#include "cblas_f77.h"
#include "../flexiblas.h"
#include "cblas_flexiblas.h"

static TLS_STORE uint8_t hook_cblas_dsdot_pos = 0;


double cblas_dsdot( const CBLAS_INT N, const float *X, const CBLAS_INT incX, const float *Y, const CBLAS_INT incY)
{
    double d;
    double (*fn)  ( const CBLAS_INT N, const float *X, const CBLAS_INT incX, const float *Y, const CBLAS_INT incY);
    CBLAS_BACKEND_INIT();
    CBLAS_HOOK_SELECT(dsdot);

    d = fn(N,X,incX,Y,incY);

    return d;
}

double flexiblas_real_cblas_dsdot( const CBLAS_INT N, const float *X, const CBLAS_INT incX, const float *Y, const CBLAS_INT incY)
{
    double d;
    if ( current_backend->blas.dsdot.cblas_function != NULL ) {
        double (*fn)  ( const CBLAS_INT N, const float *X, const CBLAS_INT incX, const float *Y, const CBLAS_INT incY);
        *(void **) &fn = current_backend->blas.dsdot.cblas_function;
        d = fn(N,X,incX,Y,incY);
    } else {
        Int F77_N=N, F77_incX=incX, F77_incY=incY;
        d = FC_GLOBAL(dsdot,DSDOT)( (blasint *)(uintptr_t)&F77_N, (float *)(uintptr_t)X, (blasint *)(uintptr_t)&F77_incX,(float *)(uintptr_t) Y, (blasint *)(uintptr_t)&F77_incY);
    }
    return d;

}

double flexiblas_chain_cblas_dsdot( const CBLAS_INT N, const float *X, const CBLAS_INT incX, const float *Y, const CBLAS_INT incY)
{
    double (*fn)  ( const CBLAS_INT N, const float *X, const CBLAS_INT incX, const float *Y, const CBLAS_INT incY);
    double d;
    CBLAS_HOOK_ADVANCE(dsdot);


    d = fn(N,X,incX,Y,incY);

    return d;
}


