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


static TLS_STORE uint8_t hook_pos_sladiv2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
float FC_GLOBAL(sladiv2,SLADIV2)(float* a, float* b, float* c, float* d, float* r, float* t)
#else
float FC_GLOBAL(sladiv2,SLADIV2)(float* a, float* b, float* c, float* d, float* r, float* t)
#endif
{
    float (*fn) (void* a, void* b, void* c, void* d, void* r, void* t);
    float (*fn_hook) (void* a, void* b, void* c, void* d, void* r, void* t);
    float ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.sladiv2.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->sladiv2.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) a, (void*) b, (void*) c, (void*) d, (void*) r, (void*) t);
        return ret;
    } else {
        hook_pos_sladiv2 = 0;
        ret = fn_hook((void*) a, (void*) b, (void*) c, (void*) d, (void*) r, (void*) t);
        return ret;
    }
}
#ifndef __APPLE__
float FC_GLOBAL2(sladiv2,SLADIV2)(float* a, float* b, float* c, float* d, float* r, float* t) __attribute__((alias(MTS(FC_GLOBAL(sladiv2,SLADIV2)))));
float FC_GLOBAL3(sladiv2,SLADIV2)(float* a, float* b, float* c, float* d, float* r, float* t) __attribute__((alias(MTS(FC_GLOBAL(sladiv2,SLADIV2)))));
#else
float FC_GLOBAL2(sladiv2,SLADIV2)(float* a, float* b, float* c, float* d, float* r, float* t){ return FC_GLOBAL(sladiv2,SLADIV2)((void*) a, (void*) b, (void*) c, (void*) d, (void*) r, (void*) t); }
float FC_GLOBAL3(sladiv2,SLADIV2)(float* a, float* b, float* c, float* d, float* r, float* t){ return FC_GLOBAL(sladiv2,SLADIV2)((void*) a, (void*) b, (void*) c, (void*) d, (void*) r, (void*) t); }
#endif




/* Real Implementation for Hooks */


float flexiblas_real_sladiv2_(void* a, void* b, void* c, void* d, void* r, void* t)
{
    float (*fn) (void* a, void* b, void* c, void* d, void* r, void* t);
    float ret;

    *(void **) & fn = current_backend->lapack.sladiv2.f77_blas_function;

    ret = fn((void*) a, (void*) b, (void*) c, (void*) d, (void*) r, (void*) t);

    return ret;
}
#ifndef __APPLE__
float flexiblas_real_sladiv2(void* a, void* b, void* c, void* d, void* r, void* t) __attribute__((alias("flexiblas_real_sladiv2_")));
#else
float flexiblas_real_sladiv2(void* a, void* b, void* c, void* d, void* r, void* t){return flexiblas_real_sladiv2_((void*) a, (void*) b, (void*) c, (void*) d, (void*) r, (void*) t);}
#endif




/* Chainloader for Hooks */


float flexiblas_chain_sladiv2_(void* a, void* b, void* c, void* d, void* r, void* t)
{
    float (*fn) (void* a, void* b, void* c, void* d, void* r, void* t);
    float (*fn_hook) (void* a, void* b, void* c, void* d, void* r, void* t);
    float ret;

    *(void **) &fn      = current_backend->lapack.sladiv2.f77_blas_function;

    hook_pos_sladiv2 ++;
    if( hook_pos_sladiv2 < __flexiblas_hooks->sladiv2.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->sladiv2.f77_hook_function[hook_pos_sladiv2];
        ret = fn_hook((void*) a, (void*) b, (void*) c, (void*) d, (void*) r, (void*) t);
    } else {
        hook_pos_sladiv2 = 0;
        ret = fn((void*) a, (void*) b, (void*) c, (void*) d, (void*) r, (void*) t);
    }
    return ret;
}
#ifndef __APPLE__
float flexiblas_chain_sladiv2(void* a, void* b, void* c, void* d, void* r, void* t) __attribute__((alias("flexiblas_chain_sladiv2_")));
#else
float flexiblas_chain_sladiv2(void* a, void* b, void* c, void* d, void* r, void* t){return flexiblas_chain_sladiv2_((void*) a, (void*) b, (void*) c, (void*) d, (void*) r, (void*) t);}
#endif



