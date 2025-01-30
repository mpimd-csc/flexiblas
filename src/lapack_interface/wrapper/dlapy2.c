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


static TLS_STORE uint8_t hook_pos_dlapy2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
double FC_GLOBAL(dlapy2,DLAPY2)(double* x, double* y)
#else
double FC_GLOBAL(dlapy2,DLAPY2)(double* x, double* y)
#endif
{
    double (*fn) (void* x, void* y);
    double (*fn_hook) (void* x, void* y);
    double ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dlapy2.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dlapy2.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) x, (void*) y);
        return ret;
    } else {
        hook_pos_dlapy2 = 0;
        ret = fn_hook((void*) x, (void*) y);
        return ret;
    }
}
#ifndef __APPLE__
double FC_GLOBAL2(dlapy2,DLAPY2)(double* x, double* y) __attribute__((alias(MTS(FC_GLOBAL(dlapy2,DLAPY2)))));
double FC_GLOBAL3(dlapy2,DLAPY2)(double* x, double* y) __attribute__((alias(MTS(FC_GLOBAL(dlapy2,DLAPY2)))));
#else
double FC_GLOBAL2(dlapy2,DLAPY2)(double* x, double* y){ return FC_GLOBAL(dlapy2,DLAPY2)((void*) x, (void*) y); }
double FC_GLOBAL3(dlapy2,DLAPY2)(double* x, double* y){ return FC_GLOBAL(dlapy2,DLAPY2)((void*) x, (void*) y); }
#endif




/* Real Implementation for Hooks */


double flexiblas_real_dlapy2_(void* x, void* y)
{
    double (*fn) (void* x, void* y);
    double ret;

    *(void **) & fn = current_backend->lapack.dlapy2.f77_blas_function;

    ret = fn((void*) x, (void*) y);

    return ret;
}
#ifndef __APPLE__
double flexiblas_real_dlapy2(void* x, void* y) __attribute__((alias("flexiblas_real_dlapy2_")));
#else
double flexiblas_real_dlapy2(void* x, void* y){return flexiblas_real_dlapy2_((void*) x, (void*) y);}
#endif




/* Chainloader for Hooks */


double flexiblas_chain_dlapy2_(void* x, void* y)
{
    double (*fn) (void* x, void* y);
    double (*fn_hook) (void* x, void* y);
    double ret;

    *(void **) &fn      = current_backend->lapack.dlapy2.f77_blas_function;

    hook_pos_dlapy2 ++;
    if( hook_pos_dlapy2 < __flexiblas_hooks->dlapy2.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlapy2.f77_hook_function[hook_pos_dlapy2];
        ret = fn_hook((void*) x, (void*) y);
    } else {
        hook_pos_dlapy2 = 0;
        ret = fn((void*) x, (void*) y);
    }
    return ret;
}
#ifndef __APPLE__
double flexiblas_chain_dlapy2(void* x, void* y) __attribute__((alias("flexiblas_chain_dlapy2_")));
#else
double flexiblas_chain_dlapy2(void* x, void* y){return flexiblas_chain_dlapy2_((void*) x, (void*) y);}
#endif



