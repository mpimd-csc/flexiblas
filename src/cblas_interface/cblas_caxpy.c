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
#include "cblas_flexiblas.h"

#include "../flexiblas.h"


static TLS_STORE uint8_t hook_cblas_caxpy_pos = 0;


void flexiblas_real_cblas_caxpy( const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY)
{
    void (*fn)  ( const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    if ( current_backend->blas.caxpy.cblas_function != NULL ) {
        *(void **) &fn = current_backend->blas.caxpy.cblas_function;
        fn(N,alpha,X,incX,Y,incY);
    } else {
        Int F77_N=N, F77_incX=incX, F77_incY=incY;
        FC_GLOBAL(caxpy,CAXPY)( (blasint *)(uintptr_t)&F77_N, (void *)(uintptr_t) alpha, (void *)(uintptr_t) X, (blasint *)(uintptr_t)&F77_incX, Y, (blasint *)(uintptr_t)&F77_incY);
    }
    return;
}

void flexiblas_chain_cblas_caxpy( const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY)
{
    void (*fn)  ( const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    hook_cblas_caxpy_pos ++;
    if ( hook_cblas_caxpy_pos < __flexiblas_hooks->caxpy.cblas_nhook) {
        *(void **) &fn = __flexiblas_hooks->caxpy.cblas_hook_function[hook_cblas_caxpy_pos];
    } else {
        hook_cblas_caxpy_pos = 0;
        fn = flexiblas_chain_cblas_caxpy;
    }
    fn(N,alpha,X,incX,Y,incY);
}

void cblas_caxpy( const CBLAS_INT N, const void *alpha, const void *X,
        const CBLAS_INT incX, void *Y, const CBLAS_INT incY)
{

    void (*fn)  ( const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);

    CBLAS_BACKEND_INIT();
    CBLAS_HOOK_SELECT(caxpy);

    fn(N,alpha,X,incX,Y,incY);

    return;


}



