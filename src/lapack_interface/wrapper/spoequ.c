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


static TLS_STORE uint8_t hook_pos_spoequ = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(spoequ,SPOEQU)(blasint* n, float* a, blasint* lda, float* s, float* scond, float* amax, blasint* info)
#else
void FC_GLOBAL(spoequ,SPOEQU)(blasint* n, float* a, blasint* lda, float* s, float* scond, float* amax, blasint* info)
#endif
{
    void (*fn) (void* n, void* a, void* lda, void* s, void* scond, void* amax, void* info);
    void (*fn_hook) (void* n, void* a, void* lda, void* s, void* scond, void* amax, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.spoequ.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->spoequ.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) n, (void*) a, (void*) lda, (void*) s, (void*) scond, (void*) amax, (void*) info);
        return;
    } else {
        hook_pos_spoequ = 0;
        fn_hook((void*) n, (void*) a, (void*) lda, (void*) s, (void*) scond, (void*) amax, (void*) info);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void spoequ_(blasint* n, float* a, blasint* lda, float* s, float* scond, float* amax, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(spoequ,SPOEQU)))));
#else
#ifndef __APPLE__
void spoequ(blasint* n, float* a, blasint* lda, float* s, float* scond, float* amax, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(spoequ,SPOEQU)))));
#else
void spoequ(blasint* n, float* a, blasint* lda, float* s, float* scond, float* amax, blasint* info){ FC_GLOBAL(spoequ,SPOEQU)((void*) n, (void*) a, (void*) lda, (void*) s, (void*) scond, (void*) amax, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_spoequ_(void* n, void* a, void* lda, void* s, void* scond, void* amax, void* info)
{
    void (*fn) (void* n, void* a, void* lda, void* s, void* scond, void* amax, void* info);

    *(void **) & fn = current_backend->lapack.spoequ.f77_blas_function;

    fn((void*) n, (void*) a, (void*) lda, (void*) s, (void*) scond, (void*) amax, (void*) info);

    return;
}
#ifndef __APPLE__
void flexiblas_real_spoequ(void* n, void* a, void* lda, void* s, void* scond, void* amax, void* info) __attribute__((alias("flexiblas_real_spoequ_")));
#else
void flexiblas_real_spoequ(void* n, void* a, void* lda, void* s, void* scond, void* amax, void* info){flexiblas_real_spoequ_((void*) n, (void*) a, (void*) lda, (void*) s, (void*) scond, (void*) amax, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_spoequ_(void* n, void* a, void* lda, void* s, void* scond, void* amax, void* info)
{
    void (*fn) (void* n, void* a, void* lda, void* s, void* scond, void* amax, void* info);
    void (*fn_hook) (void* n, void* a, void* lda, void* s, void* scond, void* amax, void* info);

    *(void **) &fn      = current_backend->lapack.spoequ.f77_blas_function;

    hook_pos_spoequ ++;
    if( hook_pos_spoequ < __flexiblas_hooks->spoequ.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->spoequ.f77_hook_function[hook_pos_spoequ];
        fn_hook((void*) n, (void*) a, (void*) lda, (void*) s, (void*) scond, (void*) amax, (void*) info);
    } else {
        hook_pos_spoequ = 0;
        fn((void*) n, (void*) a, (void*) lda, (void*) s, (void*) scond, (void*) amax, (void*) info);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_spoequ(void* n, void* a, void* lda, void* s, void* scond, void* amax, void* info) __attribute__((alias("flexiblas_chain_spoequ_")));
#else
void flexiblas_chain_spoequ(void* n, void* a, void* lda, void* s, void* scond, void* amax, void* info){flexiblas_chain_spoequ_((void*) n, (void*) a, (void*) lda, (void*) s, (void*) scond, (void*) amax, (void*) info);}
#endif



