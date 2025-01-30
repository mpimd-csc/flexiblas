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


static TLS_STORE uint8_t hook_pos_spttrf = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(spttrf,SPTTRF)(blasint* n, float* d, float* e, blasint* info)
#else
void FC_GLOBAL(spttrf,SPTTRF)(blasint* n, float* d, float* e, blasint* info)
#endif
{
    void (*fn) (void* n, void* d, void* e, void* info);
    void (*fn_hook) (void* n, void* d, void* e, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.spttrf.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->spttrf.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) n, (void*) d, (void*) e, (void*) info);
        return;
    } else {
        hook_pos_spttrf = 0;
        fn_hook((void*) n, (void*) d, (void*) e, (void*) info);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(spttrf,SPTTRF)(blasint* n, float* d, float* e, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(spttrf,SPTTRF)))));
void FC_GLOBAL3(spttrf,SPTTRF)(blasint* n, float* d, float* e, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(spttrf,SPTTRF)))));
#else
void FC_GLOBAL2(spttrf,SPTTRF)(blasint* n, float* d, float* e, blasint* info){ FC_GLOBAL(spttrf,SPTTRF)((void*) n, (void*) d, (void*) e, (void*) info); }
void FC_GLOBAL3(spttrf,SPTTRF)(blasint* n, float* d, float* e, blasint* info){ FC_GLOBAL(spttrf,SPTTRF)((void*) n, (void*) d, (void*) e, (void*) info); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_spttrf_(void* n, void* d, void* e, void* info)
{
    void (*fn) (void* n, void* d, void* e, void* info);

    *(void **) & fn = current_backend->lapack.spttrf.f77_blas_function;

    fn((void*) n, (void*) d, (void*) e, (void*) info);

    return;
}
#ifndef __APPLE__
void flexiblas_real_spttrf(void* n, void* d, void* e, void* info) __attribute__((alias("flexiblas_real_spttrf_")));
#else
void flexiblas_real_spttrf(void* n, void* d, void* e, void* info){flexiblas_real_spttrf_((void*) n, (void*) d, (void*) e, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_spttrf_(void* n, void* d, void* e, void* info)
{
    void (*fn) (void* n, void* d, void* e, void* info);
    void (*fn_hook) (void* n, void* d, void* e, void* info);

    *(void **) &fn      = current_backend->lapack.spttrf.f77_blas_function;

    hook_pos_spttrf ++;
    if( hook_pos_spttrf < __flexiblas_hooks->spttrf.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->spttrf.f77_hook_function[hook_pos_spttrf];
        fn_hook((void*) n, (void*) d, (void*) e, (void*) info);
    } else {
        hook_pos_spttrf = 0;
        fn((void*) n, (void*) d, (void*) e, (void*) info);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_spttrf(void* n, void* d, void* e, void* info) __attribute__((alias("flexiblas_chain_spttrf_")));
#else
void flexiblas_chain_spttrf(void* n, void* d, void* e, void* info){flexiblas_chain_spttrf_((void*) n, (void*) d, (void*) e, (void*) info);}
#endif



