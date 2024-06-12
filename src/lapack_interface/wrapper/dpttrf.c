//  SPDX-License-Identifier: LGPL-3.0-or-later
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

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <complex.h>

#include "flexiblas_fortran_mangle.h"

#include "flexiblas.h"

#include "flexiblas_fortran_char_len.h"


static TLS_STORE uint8_t hook_pos_dpttrf = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dpttrf,DPTTRF)(blasint* n, double* d, double* e, blasint* info)
#else
void FC_GLOBAL(dpttrf,DPTTRF)(blasint* n, double* d, double* e, blasint* info)
#endif
{
    void (*fn) (void* n, void* d, void* e, void* info);
    void (*fn_hook) (void* n, void* d, void* e, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dpttrf.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dpttrf.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) n, (void*) d, (void*) e, (void*) info);
        return;
    } else {
        hook_pos_dpttrf = 0;
        fn_hook((void*) n, (void*) d, (void*) e, (void*) info);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void dpttrf_(blasint* n, double* d, double* e, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dpttrf,DPTTRF)))));
#else
#ifndef __APPLE__
void dpttrf(blasint* n, double* d, double* e, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dpttrf,DPTTRF)))));
#else
void dpttrf(blasint* n, double* d, double* e, blasint* info){ FC_GLOBAL(dpttrf,DPTTRF)((void*) n, (void*) d, (void*) e, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dpttrf_(void* n, void* d, void* e, void* info)
{
    void (*fn) (void* n, void* d, void* e, void* info);

    *(void **) & fn = current_backend->lapack.dpttrf.f77_blas_function;

    fn((void*) n, (void*) d, (void*) e, (void*) info);

    return;
}
#ifndef __APPLE__
void flexiblas_real_dpttrf(void* n, void* d, void* e, void* info) __attribute__((alias("flexiblas_real_dpttrf_")));
#else
void flexiblas_real_dpttrf(void* n, void* d, void* e, void* info){flexiblas_real_dpttrf_((void*) n, (void*) d, (void*) e, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dpttrf_(void* n, void* d, void* e, void* info)
{
    void (*fn) (void* n, void* d, void* e, void* info);
    void (*fn_hook) (void* n, void* d, void* e, void* info);

    *(void **) &fn      = current_backend->lapack.dpttrf.f77_blas_function;

    hook_pos_dpttrf ++;
    if( hook_pos_dpttrf < __flexiblas_hooks->dpttrf.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dpttrf.f77_hook_function[hook_pos_dpttrf];
        fn_hook((void*) n, (void*) d, (void*) e, (void*) info);
    } else {
        hook_pos_dpttrf = 0;
        fn((void*) n, (void*) d, (void*) e, (void*) info);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_dpttrf(void* n, void* d, void* e, void* info) __attribute__((alias("flexiblas_chain_dpttrf_")));
#else
void flexiblas_chain_dpttrf(void* n, void* d, void* e, void* info){flexiblas_chain_dpttrf_((void*) n, (void*) d, (void*) e, (void*) info);}
#endif



