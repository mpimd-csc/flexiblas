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

static TLS_STORE uint8_t hook_cblas_zcopy_pos = 0;


void cblas_zcopy( const CBLAS_INT N, const void *X,const CBLAS_INT incX, void *Y, const CBLAS_INT incY)
{
    void (*fn)  ( const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);

    CBLAS_BACKEND_INIT();
    CBLAS_HOOK_SELECT(zcopy);
    fn(N,X,incX,Y,incY);
    return;
}

void flexiblas_real_cblas_zcopy( const CBLAS_INT N, const void *X,const CBLAS_INT incX, void *Y, const CBLAS_INT incY)
{
    if ( current_backend->blas.zcopy.cblas_function != NULL ) {
        void (*fn)  ( const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
        *(void **) &fn = current_backend->blas.zcopy.cblas_function;
        fn(N,X,incX,Y,incY);
    } else {
        Int F77_N=N, F77_incX=incX, F77_incY=incY;
        FC_GLOBAL(zcopy,ZCOPY)( (blasint *)(uintptr_t)&F77_N, (void *)(uintptr_t) X, (blasint *)(uintptr_t)&F77_incX, Y, (blasint *)(uintptr_t)&F77_incY);
    }
    return;
}

void flexiblas_chain_cblas_zcopy( const CBLAS_INT N, const void *X,const CBLAS_INT incX, void *Y, const CBLAS_INT incY)
{

    void (*fn)  ( const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    CBLAS_HOOK_ADVANCE(zcopy);
    fn(N,X,incX,Y,incY);
    return;
}


