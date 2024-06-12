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


static TLS_STORE uint8_t hook_pos_sisnan = 0;
#ifdef FLEXIBLAS_ABI_INTEL
int FC_GLOBAL(sisnan,SISNAN)(float* sin)
#else
int FC_GLOBAL(sisnan,SISNAN)(float* sin)
#endif
{
    blasint (*fn) (void* sin);
    blasint (*fn_hook) (void* sin);
    blasint ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.sisnan.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->sisnan.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) sin);
        return ret;
    } else {
        hook_pos_sisnan = 0;
        ret = fn_hook((void*) sin);
        return ret;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
int sisnan_(float* sin) __attribute__((alias(MTS(FC_GLOBAL(sisnan,SISNAN)))));
#else
#ifndef __APPLE__
int sisnan(float* sin) __attribute__((alias(MTS(FC_GLOBAL(sisnan,SISNAN)))));
#else
int sisnan(float* sin){ return FC_GLOBAL(sisnan,SISNAN)((void*) sin); }
#endif
#endif




/* Real Implementation for Hooks */


blasint flexiblas_real_sisnan_(void* sin)
{
    blasint (*fn) (void* sin);
    blasint ret;

    *(void **) & fn = current_backend->lapack.sisnan.f77_blas_function;

    ret = fn((void*) sin);

    return ret;
}
#ifndef __APPLE__
blasint flexiblas_real_sisnan(void* sin) __attribute__((alias("flexiblas_real_sisnan_")));
#else
blasint flexiblas_real_sisnan(void* sin){return flexiblas_real_sisnan_((void*) sin);}
#endif




/* Chainloader for Hooks */


blasint flexiblas_chain_sisnan_(void* sin)
{
    blasint (*fn) (void* sin);
    blasint (*fn_hook) (void* sin);
    blasint ret;

    *(void **) &fn      = current_backend->lapack.sisnan.f77_blas_function;

    hook_pos_sisnan ++;
    if( hook_pos_sisnan < __flexiblas_hooks->sisnan.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->sisnan.f77_hook_function[hook_pos_sisnan];
        ret = fn_hook((void*) sin);
    } else {
        hook_pos_sisnan = 0;
        ret = fn((void*) sin);
    }
    return ret;
}
#ifndef __APPLE__
blasint flexiblas_chain_sisnan(void* sin) __attribute__((alias("flexiblas_chain_sisnan_")));
#else
blasint flexiblas_chain_sisnan(void* sin){return flexiblas_chain_sisnan_((void*) sin);}
#endif



