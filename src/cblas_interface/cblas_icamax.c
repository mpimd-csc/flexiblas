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

static TLS_STORE uint8_t hook_cblas_icamax_pos = 0;



CBLAS_INDEX cblas_icamax( const CBLAS_INT N, const void *X, const CBLAS_INT incX)
{
    CBLAS_INDEX iamax;

    CBLAS_INDEX (*fn) ( const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    CBLAS_BACKEND_INIT();
    CBLAS_HOOK_SELECT(icamax);
    iamax = fn(N,X,incX);

    return iamax;
}

CBLAS_INDEX flexiblas_chain_cblas_icamax( const CBLAS_INT N, const void *X, const CBLAS_INT incX)
{
    CBLAS_INDEX iamax;

    CBLAS_INDEX (*fn) ( const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    CBLAS_HOOK_ADVANCE(icamax);
    iamax = fn(N,X,incX);
    return iamax;

}

CBLAS_INDEX flexiblas_real_cblas_icamax( const CBLAS_INT N, const void *X, const CBLAS_INT incX)
{
    CBLAS_INDEX iamax;
#ifdef F77_INT
    F77_INT F77_N=N, F77_incX=incX;
#else
#define F77_N N
#define F77_incX incX
#endif

    if ( current_backend->blas.icamax.cblas_function != NULL ) {
        CBLAS_INDEX (*fn) ( const CBLAS_INT N, const void *X, const CBLAS_INT incX);
        *(void **) &fn = current_backend->blas.icamax.cblas_function;
        iamax = fn(N,X,incX);

    } else {
        iamax = FC_GLOBAL(icamax,ICAMAX)( &F77_N, X, &F77_incX);
        iamax = iamax ? iamax-1 : 0;
    }
    return iamax;
}
