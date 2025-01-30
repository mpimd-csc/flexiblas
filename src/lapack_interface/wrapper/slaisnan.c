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


static TLS_STORE uint8_t hook_pos_slaisnan = 0;
#ifdef FLEXIBLAS_ABI_INTEL
blaslogical FC_GLOBAL(slaisnan,SLAISNAN)(float* sin1, float* sin2)
#else
blaslogical FC_GLOBAL(slaisnan,SLAISNAN)(float* sin1, float* sin2)
#endif
{
    blaslogical (*fn) (void* sin1, void* sin2);
    blaslogical (*fn_hook) (void* sin1, void* sin2);
    blaslogical ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.slaisnan.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->slaisnan.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) sin1, (void*) sin2);
        return ret;
    } else {
        hook_pos_slaisnan = 0;
        ret = fn_hook((void*) sin1, (void*) sin2);
        return ret;
    }
}
#ifndef __APPLE__
blaslogical FC_GLOBAL2(slaisnan,SLAISNAN)(float* sin1, float* sin2) __attribute__((alias(MTS(FC_GLOBAL(slaisnan,SLAISNAN)))));
blaslogical FC_GLOBAL3(slaisnan,SLAISNAN)(float* sin1, float* sin2) __attribute__((alias(MTS(FC_GLOBAL(slaisnan,SLAISNAN)))));
#else
blaslogical FC_GLOBAL2(slaisnan,SLAISNAN)(float* sin1, float* sin2){ return FC_GLOBAL(slaisnan,SLAISNAN)((void*) sin1, (void*) sin2); }
blaslogical FC_GLOBAL3(slaisnan,SLAISNAN)(float* sin1, float* sin2){ return FC_GLOBAL(slaisnan,SLAISNAN)((void*) sin1, (void*) sin2); }
#endif




/* Real Implementation for Hooks */


blaslogical flexiblas_real_slaisnan_(void* sin1, void* sin2)
{
    blaslogical (*fn) (void* sin1, void* sin2);
    blaslogical ret;

    *(void **) & fn = current_backend->lapack.slaisnan.f77_blas_function;

    ret = fn((void*) sin1, (void*) sin2);

    return ret;
}
#ifndef __APPLE__
blaslogical flexiblas_real_slaisnan(void* sin1, void* sin2) __attribute__((alias("flexiblas_real_slaisnan_")));
#else
blaslogical flexiblas_real_slaisnan(void* sin1, void* sin2){return flexiblas_real_slaisnan_((void*) sin1, (void*) sin2);}
#endif




/* Chainloader for Hooks */


blaslogical flexiblas_chain_slaisnan_(void* sin1, void* sin2)
{
    blaslogical (*fn) (void* sin1, void* sin2);
    blaslogical (*fn_hook) (void* sin1, void* sin2);
    blaslogical ret;

    *(void **) &fn      = current_backend->lapack.slaisnan.f77_blas_function;

    hook_pos_slaisnan ++;
    if( hook_pos_slaisnan < __flexiblas_hooks->slaisnan.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->slaisnan.f77_hook_function[hook_pos_slaisnan];
        ret = fn_hook((void*) sin1, (void*) sin2);
    } else {
        hook_pos_slaisnan = 0;
        ret = fn((void*) sin1, (void*) sin2);
    }
    return ret;
}
#ifndef __APPLE__
blaslogical flexiblas_chain_slaisnan(void* sin1, void* sin2) __attribute__((alias("flexiblas_chain_slaisnan_")));
#else
blaslogical flexiblas_chain_slaisnan(void* sin1, void* sin2){return flexiblas_chain_slaisnan_((void*) sin1, (void*) sin2);}
#endif



