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

static TLS_STORE uint8_t hook_cblas_saxpy_pos = 0;

void cblas_saxpy( const CBLAS_INT N, const float alpha, const float *X,
        const CBLAS_INT incX, float *Y, const CBLAS_INT incY)
{

    void (*fn)  ( const CBLAS_INT N, const float alpha, const float *X, const CBLAS_INT incX, float *Y, const CBLAS_INT incY);
    CBLAS_BACKEND_INIT();
    CBLAS_HOOK_SELECT(saxpy);

    fn(N,alpha,X,incX,Y,incY);
}


void flexiblas_real_cblas_saxpy( const CBLAS_INT N, const float alpha, const float *X, const CBLAS_INT incX, float *Y, const CBLAS_INT incY)
{
    if ( current_backend->blas.saxpy.cblas_function != NULL ) {
        void (*fn)  ( const CBLAS_INT N, const float alpha, const float *X, const CBLAS_INT incX, float *Y, const CBLAS_INT incY);
        *(void **) &fn = current_backend->blas.saxpy.cblas_function;
        fn(N,alpha,X,incX,Y,incY);
    } else {
        Int F77_N=N, F77_incX=incX, F77_incY=incY;
        FC_GLOBAL(saxpy,SAXPY)( (blasint *)(uintptr_t)&F77_N, (float *)(uintptr_t) &alpha, (float *)(uintptr_t) X, (blasint *)(uintptr_t)&F77_incX, Y, (blasint *)(uintptr_t)&F77_incY);
    }
}

void flexiblas_chain_cblas_saxpy( const CBLAS_INT N, const float alpha, const float *X, const CBLAS_INT incX, float *Y, const CBLAS_INT incY)
{
    void (*fn)  ( const CBLAS_INT N, const float alpha, const float *X, const CBLAS_INT incX, float *Y, const CBLAS_INT incY);
    CBLAS_HOOK_ADVANCE(saxpy);
    fn(N,alpha,X,incX,Y,incY);
}



