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


static TLS_STORE uint8_t hook_pos_dlapy3 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
double FC_GLOBAL(dlapy3,DLAPY3)(double* x, double* y, double* z)
#else
double FC_GLOBAL(dlapy3,DLAPY3)(double* x, double* y, double* z)
#endif
{
    double (*fn) (void* x, void* y, void* z);
    double (*fn_hook) (void* x, void* y, void* z);
    double ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dlapy3.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dlapy3.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) x, (void*) y, (void*) z);
        return ret;
    } else {
        hook_pos_dlapy3 = 0;
        ret = fn_hook((void*) x, (void*) y, (void*) z);
        return ret;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
double dlapy3_(double* x, double* y, double* z) __attribute__((alias(MTS(FC_GLOBAL(dlapy3,DLAPY3)))));
#else
#ifndef __APPLE__
double dlapy3(double* x, double* y, double* z) __attribute__((alias(MTS(FC_GLOBAL(dlapy3,DLAPY3)))));
#else
double dlapy3(double* x, double* y, double* z){ return FC_GLOBAL(dlapy3,DLAPY3)((void*) x, (void*) y, (void*) z); }
#endif
#endif




/* Real Implementation for Hooks */


double flexiblas_real_dlapy3_(void* x, void* y, void* z)
{
    double (*fn) (void* x, void* y, void* z);
    double ret;

    *(void **) & fn = current_backend->lapack.dlapy3.f77_blas_function;

    ret = fn((void*) x, (void*) y, (void*) z);

    return ret;
}
#ifndef __APPLE__
double flexiblas_real_dlapy3(void* x, void* y, void* z) __attribute__((alias("flexiblas_real_dlapy3_")));
#else
double flexiblas_real_dlapy3(void* x, void* y, void* z){return flexiblas_real_dlapy3_((void*) x, (void*) y, (void*) z);}
#endif




/* Chainloader for Hooks */


double flexiblas_chain_dlapy3_(void* x, void* y, void* z)
{
    double (*fn) (void* x, void* y, void* z);
    double (*fn_hook) (void* x, void* y, void* z);
    double ret;

    *(void **) &fn      = current_backend->lapack.dlapy3.f77_blas_function;

    hook_pos_dlapy3 ++;
    if( hook_pos_dlapy3 < __flexiblas_hooks->dlapy3.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlapy3.f77_hook_function[hook_pos_dlapy3];
        ret = fn_hook((void*) x, (void*) y, (void*) z);
    } else {
        hook_pos_dlapy3 = 0;
        ret = fn((void*) x, (void*) y, (void*) z);
    }
    return ret;
}
#ifndef __APPLE__
double flexiblas_chain_dlapy3(void* x, void* y, void* z) __attribute__((alias("flexiblas_chain_dlapy3_")));
#else
double flexiblas_chain_dlapy3(void* x, void* y, void* z){return flexiblas_chain_dlapy3_((void*) x, (void*) y, (void*) z);}
#endif



