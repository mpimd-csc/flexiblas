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


static TLS_STORE uint8_t hook_pos_dzsum1 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
double FC_GLOBAL(dzsum1,DZSUM1)(blasint* n, double complex* cx, blasint* incx)
#else
double FC_GLOBAL(dzsum1,DZSUM1)(blasint* n, double complex* cx, blasint* incx)
#endif
{
    double (*fn) (void* n, void* cx, void* incx);
    double (*fn_hook) (void* n, void* cx, void* incx);
    double ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dzsum1.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dzsum1.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) n, (void*) cx, (void*) incx);
        return ret;
    } else {
        hook_pos_dzsum1 = 0;
        ret = fn_hook((void*) n, (void*) cx, (void*) incx);
        return ret;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
double dzsum1_(blasint* n, double complex* cx, blasint* incx) __attribute__((alias(MTS(FC_GLOBAL(dzsum1,DZSUM1)))));
#else
#ifndef __APPLE__
double dzsum1(blasint* n, double complex* cx, blasint* incx) __attribute__((alias(MTS(FC_GLOBAL(dzsum1,DZSUM1)))));
#else
double dzsum1(blasint* n, double complex* cx, blasint* incx){ return FC_GLOBAL(dzsum1,DZSUM1)((void*) n, (void*) cx, (void*) incx); }
#endif
#endif




/* Real Implementation for Hooks */


double flexiblas_real_dzsum1_(void* n, void* cx, void* incx)
{
    double (*fn) (void* n, void* cx, void* incx);
    double ret;

    *(void **) & fn = current_backend->lapack.dzsum1.f77_blas_function;

    ret = fn((void*) n, (void*) cx, (void*) incx);

    return ret;
}
#ifndef __APPLE__
double flexiblas_real_dzsum1(void* n, void* cx, void* incx) __attribute__((alias("flexiblas_real_dzsum1_")));
#else
double flexiblas_real_dzsum1(void* n, void* cx, void* incx){return flexiblas_real_dzsum1_((void*) n, (void*) cx, (void*) incx);}
#endif




/* Chainloader for Hooks */


double flexiblas_chain_dzsum1_(void* n, void* cx, void* incx)
{
    double (*fn) (void* n, void* cx, void* incx);
    double (*fn_hook) (void* n, void* cx, void* incx);
    double ret;

    *(void **) &fn      = current_backend->lapack.dzsum1.f77_blas_function;

    hook_pos_dzsum1 ++;
    if( hook_pos_dzsum1 < __flexiblas_hooks->dzsum1.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dzsum1.f77_hook_function[hook_pos_dzsum1];
        ret = fn_hook((void*) n, (void*) cx, (void*) incx);
    } else {
        hook_pos_dzsum1 = 0;
        ret = fn((void*) n, (void*) cx, (void*) incx);
    }
    return ret;
}
#ifndef __APPLE__
double flexiblas_chain_dzsum1(void* n, void* cx, void* incx) __attribute__((alias("flexiblas_chain_dzsum1_")));
#else
double flexiblas_chain_dzsum1(void* n, void* cx, void* incx){return flexiblas_chain_dzsum1_((void*) n, (void*) cx, (void*) incx);}
#endif



