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


static TLS_STORE uint8_t hook_pos_crscl = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(crscl,CRSCL)(blasint* n, float complex* a, float complex* x, blasint* incx)
#else
void FC_GLOBAL(crscl,CRSCL)(blasint* n, float complex* a, float complex* x, blasint* incx)
#endif
{
    void (*fn) (void* n, void* a, void* x, void* incx);
    void (*fn_hook) (void* n, void* a, void* x, void* incx);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.crscl.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->crscl.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) n, (void*) a, (void*) x, (void*) incx);
        return;
    } else {
        hook_pos_crscl = 0;
        fn_hook((void*) n, (void*) a, (void*) x, (void*) incx);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void crscl_(blasint* n, float complex* a, float complex* x, blasint* incx) __attribute__((alias(MTS(FC_GLOBAL(crscl,CRSCL)))));
#else
#ifndef __APPLE__
void crscl(blasint* n, float complex* a, float complex* x, blasint* incx) __attribute__((alias(MTS(FC_GLOBAL(crscl,CRSCL)))));
#else
void crscl(blasint* n, float complex* a, float complex* x, blasint* incx){ FC_GLOBAL(crscl,CRSCL)((void*) n, (void*) a, (void*) x, (void*) incx); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_crscl_(void* n, void* a, void* x, void* incx)
{
    void (*fn) (void* n, void* a, void* x, void* incx);

    *(void **) & fn = current_backend->lapack.crscl.f77_blas_function;

    fn((void*) n, (void*) a, (void*) x, (void*) incx);

    return;
}
#ifndef __APPLE__
void flexiblas_real_crscl(void* n, void* a, void* x, void* incx) __attribute__((alias("flexiblas_real_crscl_")));
#else
void flexiblas_real_crscl(void* n, void* a, void* x, void* incx){flexiblas_real_crscl_((void*) n, (void*) a, (void*) x, (void*) incx);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_crscl_(void* n, void* a, void* x, void* incx)
{
    void (*fn) (void* n, void* a, void* x, void* incx);
    void (*fn_hook) (void* n, void* a, void* x, void* incx);

    *(void **) &fn      = current_backend->lapack.crscl.f77_blas_function;

    hook_pos_crscl ++;
    if( hook_pos_crscl < __flexiblas_hooks->crscl.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->crscl.f77_hook_function[hook_pos_crscl];
        fn_hook((void*) n, (void*) a, (void*) x, (void*) incx);
    } else {
        hook_pos_crscl = 0;
        fn((void*) n, (void*) a, (void*) x, (void*) incx);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_crscl(void* n, void* a, void* x, void* incx) __attribute__((alias("flexiblas_chain_crscl_")));
#else
void flexiblas_chain_crscl(void* n, void* a, void* x, void* incx){flexiblas_chain_crscl_((void*) n, (void*) a, (void*) x, (void*) incx);}
#endif



