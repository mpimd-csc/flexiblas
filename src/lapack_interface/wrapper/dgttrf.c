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

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <complex.h>

#include "flexiblas_config.h"

#include "flexiblas_fortran_mangle.h"

#include "flexiblas.h"

#include "flexiblas_fortran_char_len.h"


static TLS_STORE uint8_t hook_pos_dgttrf = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dgttrf,DGTTRF)(blasint* n, double* dl, double* d, double* du, double* du2, blasint* ipiv, blasint* info)
#else
void FC_GLOBAL(dgttrf,DGTTRF)(blasint* n, double* dl, double* d, double* du, double* du2, blasint* ipiv, blasint* info)
#endif
{
    void (*fn) (void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* info);
    void (*fn_hook) (void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dgttrf.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dgttrf.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) n, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) info);
        return;
    } else {
        hook_pos_dgttrf = 0;
        fn_hook((void*) n, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) info);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(dgttrf,DGTTRF)(blasint* n, double* dl, double* d, double* du, double* du2, blasint* ipiv, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dgttrf,DGTTRF)))));
void FC_GLOBAL3(dgttrf,DGTTRF)(blasint* n, double* dl, double* d, double* du, double* du2, blasint* ipiv, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dgttrf,DGTTRF)))));
#else
void FC_GLOBAL2(dgttrf,DGTTRF)(blasint* n, double* dl, double* d, double* du, double* du2, blasint* ipiv, blasint* info){ FC_GLOBAL(dgttrf,DGTTRF)((void*) n, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) info); }
void FC_GLOBAL3(dgttrf,DGTTRF)(blasint* n, double* dl, double* d, double* du, double* du2, blasint* ipiv, blasint* info){ FC_GLOBAL(dgttrf,DGTTRF)((void*) n, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) info); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dgttrf_(void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* info)
{
    void (*fn) (void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* info);

    *(void **) & fn = current_backend->lapack.dgttrf.f77_blas_function;

    fn((void*) n, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) info);

    return;
}
#ifndef __APPLE__
void flexiblas_real_dgttrf(void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* info) __attribute__((alias("flexiblas_real_dgttrf_")));
#else
void flexiblas_real_dgttrf(void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* info){flexiblas_real_dgttrf_((void*) n, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dgttrf_(void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* info)
{
    void (*fn) (void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* info);
    void (*fn_hook) (void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* info);

    *(void **) &fn      = current_backend->lapack.dgttrf.f77_blas_function;

    hook_pos_dgttrf ++;
    if( hook_pos_dgttrf < __flexiblas_hooks->dgttrf.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dgttrf.f77_hook_function[hook_pos_dgttrf];
        fn_hook((void*) n, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) info);
    } else {
        hook_pos_dgttrf = 0;
        fn((void*) n, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) info);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_dgttrf(void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* info) __attribute__((alias("flexiblas_chain_dgttrf_")));
#else
void flexiblas_chain_dgttrf(void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* info){flexiblas_chain_dgttrf_((void*) n, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) info);}
#endif



