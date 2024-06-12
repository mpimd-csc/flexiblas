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


static TLS_STORE uint8_t hook_pos_zlartg = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zlartg,ZLARTG)(float complex* f, float complex* g, float* c, float complex* s, float complex* r)
#else
void FC_GLOBAL(zlartg,ZLARTG)(float complex* f, float complex* g, float* c, float complex* s, float complex* r)
#endif
{
    void (*fn) (void* f, void* g, void* c, void* s, void* r);
    void (*fn_hook) (void* f, void* g, void* c, void* s, void* r);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.zlartg.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->zlartg.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) f, (void*) g, (void*) c, (void*) s, (void*) r);
        return;
    } else {
        hook_pos_zlartg = 0;
        fn_hook((void*) f, (void*) g, (void*) c, (void*) s, (void*) r);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void zlartg_(float complex* f, float complex* g, float* c, float complex* s, float complex* r) __attribute__((alias(MTS(FC_GLOBAL(zlartg,ZLARTG)))));
#else
#ifndef __APPLE__
void zlartg(float complex* f, float complex* g, float* c, float complex* s, float complex* r) __attribute__((alias(MTS(FC_GLOBAL(zlartg,ZLARTG)))));
#else
void zlartg(float complex* f, float complex* g, float* c, float complex* s, float complex* r){ FC_GLOBAL(zlartg,ZLARTG)((void*) f, (void*) g, (void*) c, (void*) s, (void*) r); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zlartg_(void* f, void* g, void* c, void* s, void* r)
{
    void (*fn) (void* f, void* g, void* c, void* s, void* r);

    *(void **) & fn = current_backend->lapack.zlartg.f77_blas_function;

    fn((void*) f, (void*) g, (void*) c, (void*) s, (void*) r);

    return;
}
#ifndef __APPLE__
void flexiblas_real_zlartg(void* f, void* g, void* c, void* s, void* r) __attribute__((alias("flexiblas_real_zlartg_")));
#else
void flexiblas_real_zlartg(void* f, void* g, void* c, void* s, void* r){flexiblas_real_zlartg_((void*) f, (void*) g, (void*) c, (void*) s, (void*) r);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zlartg_(void* f, void* g, void* c, void* s, void* r)
{
    void (*fn) (void* f, void* g, void* c, void* s, void* r);
    void (*fn_hook) (void* f, void* g, void* c, void* s, void* r);

    *(void **) &fn      = current_backend->lapack.zlartg.f77_blas_function;

    hook_pos_zlartg ++;
    if( hook_pos_zlartg < __flexiblas_hooks->zlartg.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zlartg.f77_hook_function[hook_pos_zlartg];
        fn_hook((void*) f, (void*) g, (void*) c, (void*) s, (void*) r);
    } else {
        hook_pos_zlartg = 0;
        fn((void*) f, (void*) g, (void*) c, (void*) s, (void*) r);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_zlartg(void* f, void* g, void* c, void* s, void* r) __attribute__((alias("flexiblas_chain_zlartg_")));
#else
void flexiblas_chain_zlartg(void* f, void* g, void* c, void* s, void* r){flexiblas_chain_zlartg_((void*) f, (void*) g, (void*) c, (void*) s, (void*) r);}
#endif



