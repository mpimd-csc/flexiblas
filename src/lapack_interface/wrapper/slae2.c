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


static TLS_STORE uint8_t hook_pos_slae2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slae2,SLAE2)(float* a, float* b, float* c, float* rt1, float* rt2)
#else
void FC_GLOBAL(slae2,SLAE2)(float* a, float* b, float* c, float* rt1, float* rt2)
#endif
{
    void (*fn) (void* a, void* b, void* c, void* rt1, void* rt2);
    void (*fn_hook) (void* a, void* b, void* c, void* rt1, void* rt2);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.slae2.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->slae2.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2);
        return;
    } else {
        hook_pos_slae2 = 0;
        fn_hook((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(slae2,SLAE2)(float* a, float* b, float* c, float* rt1, float* rt2) __attribute__((alias(MTS(FC_GLOBAL(slae2,SLAE2)))));
void FC_GLOBAL3(slae2,SLAE2)(float* a, float* b, float* c, float* rt1, float* rt2) __attribute__((alias(MTS(FC_GLOBAL(slae2,SLAE2)))));
#else
void FC_GLOBAL2(slae2,SLAE2)(float* a, float* b, float* c, float* rt1, float* rt2){ FC_GLOBAL(slae2,SLAE2)((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2); }
void FC_GLOBAL3(slae2,SLAE2)(float* a, float* b, float* c, float* rt1, float* rt2){ FC_GLOBAL(slae2,SLAE2)((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slae2_(void* a, void* b, void* c, void* rt1, void* rt2)
{
    void (*fn) (void* a, void* b, void* c, void* rt1, void* rt2);

    *(void **) & fn = current_backend->lapack.slae2.f77_blas_function;

    fn((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2);

    return;
}
#ifndef __APPLE__
void flexiblas_real_slae2(void* a, void* b, void* c, void* rt1, void* rt2) __attribute__((alias("flexiblas_real_slae2_")));
#else
void flexiblas_real_slae2(void* a, void* b, void* c, void* rt1, void* rt2){flexiblas_real_slae2_((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_slae2_(void* a, void* b, void* c, void* rt1, void* rt2)
{
    void (*fn) (void* a, void* b, void* c, void* rt1, void* rt2);
    void (*fn_hook) (void* a, void* b, void* c, void* rt1, void* rt2);

    *(void **) &fn      = current_backend->lapack.slae2.f77_blas_function;

    hook_pos_slae2 ++;
    if( hook_pos_slae2 < __flexiblas_hooks->slae2.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->slae2.f77_hook_function[hook_pos_slae2];
        fn_hook((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2);
    } else {
        hook_pos_slae2 = 0;
        fn((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_slae2(void* a, void* b, void* c, void* rt1, void* rt2) __attribute__((alias("flexiblas_chain_slae2_")));
#else
void flexiblas_chain_slae2(void* a, void* b, void* c, void* rt1, void* rt2){flexiblas_chain_slae2_((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2);}
#endif



