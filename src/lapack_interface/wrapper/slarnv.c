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


static TLS_STORE uint8_t hook_pos_slarnv = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slarnv,SLARNV)(blasint* idist, blasint* iseed, blasint* n, float* x)
#else
void FC_GLOBAL(slarnv,SLARNV)(blasint* idist, blasint* iseed, blasint* n, float* x)
#endif
{
    void (*fn) (void* idist, void* iseed, void* n, void* x);
    void (*fn_hook) (void* idist, void* iseed, void* n, void* x);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.slarnv.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->slarnv.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) idist, (void*) iseed, (void*) n, (void*) x);
        return;
    } else {
        hook_pos_slarnv = 0;
        fn_hook((void*) idist, (void*) iseed, (void*) n, (void*) x);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(slarnv,SLARNV)(blasint* idist, blasint* iseed, blasint* n, float* x) __attribute__((alias(MTS(FC_GLOBAL(slarnv,SLARNV)))));
void FC_GLOBAL3(slarnv,SLARNV)(blasint* idist, blasint* iseed, blasint* n, float* x) __attribute__((alias(MTS(FC_GLOBAL(slarnv,SLARNV)))));
#else
void FC_GLOBAL2(slarnv,SLARNV)(blasint* idist, blasint* iseed, blasint* n, float* x){ FC_GLOBAL(slarnv,SLARNV)((void*) idist, (void*) iseed, (void*) n, (void*) x); }
void FC_GLOBAL3(slarnv,SLARNV)(blasint* idist, blasint* iseed, blasint* n, float* x){ FC_GLOBAL(slarnv,SLARNV)((void*) idist, (void*) iseed, (void*) n, (void*) x); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slarnv_(void* idist, void* iseed, void* n, void* x)
{
    void (*fn) (void* idist, void* iseed, void* n, void* x);

    *(void **) & fn = current_backend->lapack.slarnv.f77_blas_function;

    fn((void*) idist, (void*) iseed, (void*) n, (void*) x);

    return;
}
#ifndef __APPLE__
void flexiblas_real_slarnv(void* idist, void* iseed, void* n, void* x) __attribute__((alias("flexiblas_real_slarnv_")));
#else
void flexiblas_real_slarnv(void* idist, void* iseed, void* n, void* x){flexiblas_real_slarnv_((void*) idist, (void*) iseed, (void*) n, (void*) x);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_slarnv_(void* idist, void* iseed, void* n, void* x)
{
    void (*fn) (void* idist, void* iseed, void* n, void* x);
    void (*fn_hook) (void* idist, void* iseed, void* n, void* x);

    *(void **) &fn      = current_backend->lapack.slarnv.f77_blas_function;

    hook_pos_slarnv ++;
    if( hook_pos_slarnv < __flexiblas_hooks->slarnv.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->slarnv.f77_hook_function[hook_pos_slarnv];
        fn_hook((void*) idist, (void*) iseed, (void*) n, (void*) x);
    } else {
        hook_pos_slarnv = 0;
        fn((void*) idist, (void*) iseed, (void*) n, (void*) x);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_slarnv(void* idist, void* iseed, void* n, void* x) __attribute__((alias("flexiblas_chain_slarnv_")));
#else
void flexiblas_chain_slarnv(void* idist, void* iseed, void* n, void* x){flexiblas_chain_slarnv_((void*) idist, (void*) iseed, (void*) n, (void*) x);}
#endif



