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


static TLS_STORE uint8_t hook_pos_slapy3 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
float FC_GLOBAL(slapy3,SLAPY3)(float* x, float* y, float* z)
#else
float FC_GLOBAL(slapy3,SLAPY3)(float* x, float* y, float* z)
#endif
{
    float (*fn) (void* x, void* y, void* z);
    float (*fn_hook) (void* x, void* y, void* z);
    float ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.slapy3.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->slapy3.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) x, (void*) y, (void*) z);
        return ret;
    } else {
        hook_pos_slapy3 = 0;
        ret = fn_hook((void*) x, (void*) y, (void*) z);
        return ret;
    }
}
#ifndef __APPLE__
float FC_GLOBAL2(slapy3,SLAPY3)(float* x, float* y, float* z) __attribute__((alias(MTS(FC_GLOBAL(slapy3,SLAPY3)))));
float FC_GLOBAL3(slapy3,SLAPY3)(float* x, float* y, float* z) __attribute__((alias(MTS(FC_GLOBAL(slapy3,SLAPY3)))));
#else
float FC_GLOBAL2(slapy3,SLAPY3)(float* x, float* y, float* z){ return FC_GLOBAL(slapy3,SLAPY3)((void*) x, (void*) y, (void*) z); }
float FC_GLOBAL3(slapy3,SLAPY3)(float* x, float* y, float* z){ return FC_GLOBAL(slapy3,SLAPY3)((void*) x, (void*) y, (void*) z); }
#endif




/* Real Implementation for Hooks */


float flexiblas_real_slapy3_(void* x, void* y, void* z)
{
    float (*fn) (void* x, void* y, void* z);
    float ret;

    *(void **) & fn = current_backend->lapack.slapy3.f77_blas_function;

    ret = fn((void*) x, (void*) y, (void*) z);

    return ret;
}
#ifndef __APPLE__
float flexiblas_real_slapy3(void* x, void* y, void* z) __attribute__((alias("flexiblas_real_slapy3_")));
#else
float flexiblas_real_slapy3(void* x, void* y, void* z){return flexiblas_real_slapy3_((void*) x, (void*) y, (void*) z);}
#endif




/* Chainloader for Hooks */


float flexiblas_chain_slapy3_(void* x, void* y, void* z)
{
    float (*fn) (void* x, void* y, void* z);
    float (*fn_hook) (void* x, void* y, void* z);
    float ret;

    *(void **) &fn      = current_backend->lapack.slapy3.f77_blas_function;

    hook_pos_slapy3 ++;
    if( hook_pos_slapy3 < __flexiblas_hooks->slapy3.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->slapy3.f77_hook_function[hook_pos_slapy3];
        ret = fn_hook((void*) x, (void*) y, (void*) z);
    } else {
        hook_pos_slapy3 = 0;
        ret = fn((void*) x, (void*) y, (void*) z);
    }
    return ret;
}
#ifndef __APPLE__
float flexiblas_chain_slapy3(void* x, void* y, void* z) __attribute__((alias("flexiblas_chain_slapy3_")));
#else
float flexiblas_chain_slapy3(void* x, void* y, void* z){return flexiblas_chain_slapy3_((void*) x, (void*) y, (void*) z);}
#endif



