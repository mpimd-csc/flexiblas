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
#include "cblas_flexiblas.h"
#include "../flexiblas.h"
#include <complex.h>

static TLS_STORE uint8_t hook_cblas_zdotc_sub_pos = 0;


void cblas_zdotc_sub( const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY,void *dotc)
{
    void (*fn)  ( const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY,void *dotc);

    CBLAS_BACKEND_INIT();
    CBLAS_HOOK_SELECT(zdotc_sub);
    fn(N,X,incX,Y,incY,dotc);
    return;
}

void flexiblas_real_cblas_zdotc_sub( const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY,void *dotc)
{
    void (*fn)  ( const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY,void *dotc);

    if ( current_backend->blas.zdotc_sub.cblas_function != NULL ) {
        *(void **) &fn = current_backend->blas.zdotc_sub.cblas_function;
        fn(N,X,incX,Y,incY,dotc);
    } else {
        double complex d;
        Int F77_N=N, F77_incX=incX, F77_incY=incY;
#ifdef FLEXIBLAS_ABI_INTEL
        FC_GLOBAL(zdotc,ZDOTC)( &d, (blasint *)(uintptr_t)&F77_N, (void *)(uintptr_t) X, (blasint *)(uintptr_t)&F77_incX, (void *)(uintptr_t) Y, (blasint *)(uintptr_t)&F77_incY);
#else
        d = FC_GLOBAL(zdotc,ZDOTC)( (blasint *)(uintptr_t)&F77_N, (void *)(uintptr_t) X, (blasint *)(uintptr_t)&F77_incX, (void *)(uintptr_t) Y, (blasint *)(uintptr_t)&F77_incY);
#endif
        *((double complex *) dotc) = d;
    }
    return;
}

void flexiblas_chain_cblas_zdotc_sub( const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY,void *dotc)
{
    void (*fn)  ( const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY,void *dotc);
    CBLAS_HOOK_ADVANCE(zdotc_sub);
    fn(N,X,incX,Y,incY,dotc);
}


