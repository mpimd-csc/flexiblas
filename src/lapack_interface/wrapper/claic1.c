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


static TLS_STORE uint8_t hook_pos_claic1 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(claic1,CLAIC1)(blasint* job, blasint* j, float complex* x, float* sest, float complex* w, float complex* gamma, float* sestpr, float complex* s, float complex* c)
#else
void FC_GLOBAL(claic1,CLAIC1)(blasint* job, blasint* j, float complex* x, float* sest, float complex* w, float complex* gamma, float* sestpr, float complex* s, float complex* c)
#endif
{
    void (*fn) (void* job, void* j, void* x, void* sest, void* w, void* gamma, void* sestpr, void* s, void* c);
    void (*fn_hook) (void* job, void* j, void* x, void* sest, void* w, void* gamma, void* sestpr, void* s, void* c);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.claic1.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->claic1.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) job, (void*) j, (void*) x, (void*) sest, (void*) w, (void*) gamma, (void*) sestpr, (void*) s, (void*) c);
        return;
    } else {
        hook_pos_claic1 = 0;
        fn_hook((void*) job, (void*) j, (void*) x, (void*) sest, (void*) w, (void*) gamma, (void*) sestpr, (void*) s, (void*) c);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void claic1_(blasint* job, blasint* j, float complex* x, float* sest, float complex* w, float complex* gamma, float* sestpr, float complex* s, float complex* c) __attribute__((alias(MTS(FC_GLOBAL(claic1,CLAIC1)))));
#else
#ifndef __APPLE__
void claic1(blasint* job, blasint* j, float complex* x, float* sest, float complex* w, float complex* gamma, float* sestpr, float complex* s, float complex* c) __attribute__((alias(MTS(FC_GLOBAL(claic1,CLAIC1)))));
#else
void claic1(blasint* job, blasint* j, float complex* x, float* sest, float complex* w, float complex* gamma, float* sestpr, float complex* s, float complex* c){ FC_GLOBAL(claic1,CLAIC1)((void*) job, (void*) j, (void*) x, (void*) sest, (void*) w, (void*) gamma, (void*) sestpr, (void*) s, (void*) c); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_claic1_(void* job, void* j, void* x, void* sest, void* w, void* gamma, void* sestpr, void* s, void* c)
{
    void (*fn) (void* job, void* j, void* x, void* sest, void* w, void* gamma, void* sestpr, void* s, void* c);

    *(void **) & fn = current_backend->lapack.claic1.f77_blas_function;

    fn((void*) job, (void*) j, (void*) x, (void*) sest, (void*) w, (void*) gamma, (void*) sestpr, (void*) s, (void*) c);

    return;
}
#ifndef __APPLE__
void flexiblas_real_claic1(void* job, void* j, void* x, void* sest, void* w, void* gamma, void* sestpr, void* s, void* c) __attribute__((alias("flexiblas_real_claic1_")));
#else
void flexiblas_real_claic1(void* job, void* j, void* x, void* sest, void* w, void* gamma, void* sestpr, void* s, void* c){flexiblas_real_claic1_((void*) job, (void*) j, (void*) x, (void*) sest, (void*) w, (void*) gamma, (void*) sestpr, (void*) s, (void*) c);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_claic1_(void* job, void* j, void* x, void* sest, void* w, void* gamma, void* sestpr, void* s, void* c)
{
    void (*fn) (void* job, void* j, void* x, void* sest, void* w, void* gamma, void* sestpr, void* s, void* c);
    void (*fn_hook) (void* job, void* j, void* x, void* sest, void* w, void* gamma, void* sestpr, void* s, void* c);

    *(void **) &fn      = current_backend->lapack.claic1.f77_blas_function;

    hook_pos_claic1 ++;
    if( hook_pos_claic1 < __flexiblas_hooks->claic1.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->claic1.f77_hook_function[hook_pos_claic1];
        fn_hook((void*) job, (void*) j, (void*) x, (void*) sest, (void*) w, (void*) gamma, (void*) sestpr, (void*) s, (void*) c);
    } else {
        hook_pos_claic1 = 0;
        fn((void*) job, (void*) j, (void*) x, (void*) sest, (void*) w, (void*) gamma, (void*) sestpr, (void*) s, (void*) c);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_claic1(void* job, void* j, void* x, void* sest, void* w, void* gamma, void* sestpr, void* s, void* c) __attribute__((alias("flexiblas_chain_claic1_")));
#else
void flexiblas_chain_claic1(void* job, void* j, void* x, void* sest, void* w, void* gamma, void* sestpr, void* s, void* c){flexiblas_chain_claic1_((void*) job, (void*) j, (void*) x, (void*) sest, (void*) w, (void*) gamma, (void*) sestpr, (void*) s, (void*) c);}
#endif



