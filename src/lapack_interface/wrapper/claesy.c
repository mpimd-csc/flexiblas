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


static TLS_STORE uint8_t hook_pos_claesy = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(claesy,CLAESY)(float complex* a, float complex* b, float complex* c, float complex* rt1, float complex* rt2, float complex* evscal, float complex* cs1, float complex* sn1)
#else
void FC_GLOBAL(claesy,CLAESY)(float complex* a, float complex* b, float complex* c, float complex* rt1, float complex* rt2, float complex* evscal, float complex* cs1, float complex* sn1)
#endif
{
    void (*fn) (void* a, void* b, void* c, void* rt1, void* rt2, void* evscal, void* cs1, void* sn1);
    void (*fn_hook) (void* a, void* b, void* c, void* rt1, void* rt2, void* evscal, void* cs1, void* sn1);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.claesy.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->claesy.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) evscal, (void*) cs1, (void*) sn1);
        return;
    } else {
        hook_pos_claesy = 0;
        fn_hook((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) evscal, (void*) cs1, (void*) sn1);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void claesy_(float complex* a, float complex* b, float complex* c, float complex* rt1, float complex* rt2, float complex* evscal, float complex* cs1, float complex* sn1) __attribute__((alias(MTS(FC_GLOBAL(claesy,CLAESY)))));
#else
#ifndef __APPLE__
void claesy(float complex* a, float complex* b, float complex* c, float complex* rt1, float complex* rt2, float complex* evscal, float complex* cs1, float complex* sn1) __attribute__((alias(MTS(FC_GLOBAL(claesy,CLAESY)))));
#else
void claesy(float complex* a, float complex* b, float complex* c, float complex* rt1, float complex* rt2, float complex* evscal, float complex* cs1, float complex* sn1){ FC_GLOBAL(claesy,CLAESY)((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) evscal, (void*) cs1, (void*) sn1); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_claesy_(void* a, void* b, void* c, void* rt1, void* rt2, void* evscal, void* cs1, void* sn1)
{
    void (*fn) (void* a, void* b, void* c, void* rt1, void* rt2, void* evscal, void* cs1, void* sn1);

    *(void **) & fn = current_backend->lapack.claesy.f77_blas_function;

    fn((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) evscal, (void*) cs1, (void*) sn1);

    return;
}
#ifndef __APPLE__
void flexiblas_real_claesy(void* a, void* b, void* c, void* rt1, void* rt2, void* evscal, void* cs1, void* sn1) __attribute__((alias("flexiblas_real_claesy_")));
#else
void flexiblas_real_claesy(void* a, void* b, void* c, void* rt1, void* rt2, void* evscal, void* cs1, void* sn1){flexiblas_real_claesy_((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) evscal, (void*) cs1, (void*) sn1);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_claesy_(void* a, void* b, void* c, void* rt1, void* rt2, void* evscal, void* cs1, void* sn1)
{
    void (*fn) (void* a, void* b, void* c, void* rt1, void* rt2, void* evscal, void* cs1, void* sn1);
    void (*fn_hook) (void* a, void* b, void* c, void* rt1, void* rt2, void* evscal, void* cs1, void* sn1);

    *(void **) &fn      = current_backend->lapack.claesy.f77_blas_function;

    hook_pos_claesy ++;
    if( hook_pos_claesy < __flexiblas_hooks->claesy.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->claesy.f77_hook_function[hook_pos_claesy];
        fn_hook((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) evscal, (void*) cs1, (void*) sn1);
    } else {
        hook_pos_claesy = 0;
        fn((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) evscal, (void*) cs1, (void*) sn1);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_claesy(void* a, void* b, void* c, void* rt1, void* rt2, void* evscal, void* cs1, void* sn1) __attribute__((alias("flexiblas_chain_claesy_")));
#else
void flexiblas_chain_claesy(void* a, void* b, void* c, void* rt1, void* rt2, void* evscal, void* cs1, void* sn1){flexiblas_chain_claesy_((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) evscal, (void*) cs1, (void*) sn1);}
#endif



