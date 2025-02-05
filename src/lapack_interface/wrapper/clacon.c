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


static TLS_STORE uint8_t hook_pos_clacon = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(clacon,CLACON)(blasint* n, float complex* v, float complex* x, float* est, blasint* kase)
#else
void FC_GLOBAL(clacon,CLACON)(blasint* n, float complex* v, float complex* x, float* est, blasint* kase)
#endif
{
    void (*fn) (void* n, void* v, void* x, void* est, void* kase);
    void (*fn_hook) (void* n, void* v, void* x, void* est, void* kase);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.clacon.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->clacon.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) n, (void*) v, (void*) x, (void*) est, (void*) kase);
        return;
    } else {
        hook_pos_clacon = 0;
        fn_hook((void*) n, (void*) v, (void*) x, (void*) est, (void*) kase);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(clacon,CLACON)(blasint* n, float complex* v, float complex* x, float* est, blasint* kase) __attribute__((alias(MTS(FC_GLOBAL(clacon,CLACON)))));
void FC_GLOBAL3(clacon,CLACON)(blasint* n, float complex* v, float complex* x, float* est, blasint* kase) __attribute__((alias(MTS(FC_GLOBAL(clacon,CLACON)))));
#else
void FC_GLOBAL2(clacon,CLACON)(blasint* n, float complex* v, float complex* x, float* est, blasint* kase){ FC_GLOBAL(clacon,CLACON)((void*) n, (void*) v, (void*) x, (void*) est, (void*) kase); }
void FC_GLOBAL3(clacon,CLACON)(blasint* n, float complex* v, float complex* x, float* est, blasint* kase){ FC_GLOBAL(clacon,CLACON)((void*) n, (void*) v, (void*) x, (void*) est, (void*) kase); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_clacon_(void* n, void* v, void* x, void* est, void* kase)
{
    void (*fn) (void* n, void* v, void* x, void* est, void* kase);

    *(void **) & fn = current_backend->lapack.clacon.f77_blas_function;

    fn((void*) n, (void*) v, (void*) x, (void*) est, (void*) kase);

    return;
}
#ifndef __APPLE__
void flexiblas_real_clacon(void* n, void* v, void* x, void* est, void* kase) __attribute__((alias("flexiblas_real_clacon_")));
#else
void flexiblas_real_clacon(void* n, void* v, void* x, void* est, void* kase){flexiblas_real_clacon_((void*) n, (void*) v, (void*) x, (void*) est, (void*) kase);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_clacon_(void* n, void* v, void* x, void* est, void* kase)
{
    void (*fn) (void* n, void* v, void* x, void* est, void* kase);
    void (*fn_hook) (void* n, void* v, void* x, void* est, void* kase);

    *(void **) &fn      = current_backend->lapack.clacon.f77_blas_function;

    hook_pos_clacon ++;
    if( hook_pos_clacon < __flexiblas_hooks->clacon.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->clacon.f77_hook_function[hook_pos_clacon];
        fn_hook((void*) n, (void*) v, (void*) x, (void*) est, (void*) kase);
    } else {
        hook_pos_clacon = 0;
        fn((void*) n, (void*) v, (void*) x, (void*) est, (void*) kase);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_clacon(void* n, void* v, void* x, void* est, void* kase) __attribute__((alias("flexiblas_chain_clacon_")));
#else
void flexiblas_chain_clacon(void* n, void* v, void* x, void* est, void* kase){flexiblas_chain_clacon_((void*) n, (void*) v, (void*) x, (void*) est, (void*) kase);}
#endif



