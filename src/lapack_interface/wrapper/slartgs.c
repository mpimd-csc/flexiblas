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


static TLS_STORE uint8_t hook_pos_slartgs = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slartgs,SLARTGS)(float* x, float* y, float* sigma, float* cs, float* sn)
#else
void FC_GLOBAL(slartgs,SLARTGS)(float* x, float* y, float* sigma, float* cs, float* sn)
#endif
{
    void (*fn) (void* x, void* y, void* sigma, void* cs, void* sn);
    void (*fn_hook) (void* x, void* y, void* sigma, void* cs, void* sn);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.slartgs.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->slartgs.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) x, (void*) y, (void*) sigma, (void*) cs, (void*) sn);
        return;
    } else {
        hook_pos_slartgs = 0;
        fn_hook((void*) x, (void*) y, (void*) sigma, (void*) cs, (void*) sn);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(slartgs,SLARTGS)(float* x, float* y, float* sigma, float* cs, float* sn) __attribute__((alias(MTS(FC_GLOBAL(slartgs,SLARTGS)))));
void FC_GLOBAL3(slartgs,SLARTGS)(float* x, float* y, float* sigma, float* cs, float* sn) __attribute__((alias(MTS(FC_GLOBAL(slartgs,SLARTGS)))));
#else
void FC_GLOBAL2(slartgs,SLARTGS)(float* x, float* y, float* sigma, float* cs, float* sn){ FC_GLOBAL(slartgs,SLARTGS)((void*) x, (void*) y, (void*) sigma, (void*) cs, (void*) sn); }
void FC_GLOBAL3(slartgs,SLARTGS)(float* x, float* y, float* sigma, float* cs, float* sn){ FC_GLOBAL(slartgs,SLARTGS)((void*) x, (void*) y, (void*) sigma, (void*) cs, (void*) sn); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slartgs_(void* x, void* y, void* sigma, void* cs, void* sn)
{
    void (*fn) (void* x, void* y, void* sigma, void* cs, void* sn);

    *(void **) & fn = current_backend->lapack.slartgs.f77_blas_function;

    fn((void*) x, (void*) y, (void*) sigma, (void*) cs, (void*) sn);

    return;
}
#ifndef __APPLE__
void flexiblas_real_slartgs(void* x, void* y, void* sigma, void* cs, void* sn) __attribute__((alias("flexiblas_real_slartgs_")));
#else
void flexiblas_real_slartgs(void* x, void* y, void* sigma, void* cs, void* sn){flexiblas_real_slartgs_((void*) x, (void*) y, (void*) sigma, (void*) cs, (void*) sn);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_slartgs_(void* x, void* y, void* sigma, void* cs, void* sn)
{
    void (*fn) (void* x, void* y, void* sigma, void* cs, void* sn);
    void (*fn_hook) (void* x, void* y, void* sigma, void* cs, void* sn);

    *(void **) &fn      = current_backend->lapack.slartgs.f77_blas_function;

    hook_pos_slartgs ++;
    if( hook_pos_slartgs < __flexiblas_hooks->slartgs.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->slartgs.f77_hook_function[hook_pos_slartgs];
        fn_hook((void*) x, (void*) y, (void*) sigma, (void*) cs, (void*) sn);
    } else {
        hook_pos_slartgs = 0;
        fn((void*) x, (void*) y, (void*) sigma, (void*) cs, (void*) sn);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_slartgs(void* x, void* y, void* sigma, void* cs, void* sn) __attribute__((alias("flexiblas_chain_slartgs_")));
#else
void flexiblas_chain_slartgs(void* x, void* y, void* sigma, void* cs, void* sn){flexiblas_chain_slartgs_((void*) x, (void*) y, (void*) sigma, (void*) cs, (void*) sn);}
#endif



