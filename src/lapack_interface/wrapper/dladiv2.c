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


static TLS_STORE uint8_t hook_pos_dladiv2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
double FC_GLOBAL(dladiv2,DLADIV2)(double* a, double* b, double* c, double* d, double* r, double* t)
#else
double FC_GLOBAL(dladiv2,DLADIV2)(double* a, double* b, double* c, double* d, double* r, double* t)
#endif
{
    double (*fn) (void* a, void* b, void* c, void* d, void* r, void* t);
    double (*fn_hook) (void* a, void* b, void* c, void* d, void* r, void* t);
    double ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dladiv2.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dladiv2.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) a, (void*) b, (void*) c, (void*) d, (void*) r, (void*) t);
        return ret;
    } else {
        hook_pos_dladiv2 = 0;
        ret = fn_hook((void*) a, (void*) b, (void*) c, (void*) d, (void*) r, (void*) t);
        return ret;
    }
}
#ifndef __APPLE__
double FC_GLOBAL2(dladiv2,DLADIV2)(double* a, double* b, double* c, double* d, double* r, double* t) __attribute__((alias(MTS(FC_GLOBAL(dladiv2,DLADIV2)))));
double FC_GLOBAL3(dladiv2,DLADIV2)(double* a, double* b, double* c, double* d, double* r, double* t) __attribute__((alias(MTS(FC_GLOBAL(dladiv2,DLADIV2)))));
#else
double FC_GLOBAL2(dladiv2,DLADIV2)(double* a, double* b, double* c, double* d, double* r, double* t){ return FC_GLOBAL(dladiv2,DLADIV2)((void*) a, (void*) b, (void*) c, (void*) d, (void*) r, (void*) t); }
double FC_GLOBAL3(dladiv2,DLADIV2)(double* a, double* b, double* c, double* d, double* r, double* t){ return FC_GLOBAL(dladiv2,DLADIV2)((void*) a, (void*) b, (void*) c, (void*) d, (void*) r, (void*) t); }
#endif




/* Real Implementation for Hooks */


double flexiblas_real_dladiv2_(void* a, void* b, void* c, void* d, void* r, void* t)
{
    double (*fn) (void* a, void* b, void* c, void* d, void* r, void* t);
    double ret;

    *(void **) & fn = current_backend->lapack.dladiv2.f77_blas_function;

    ret = fn((void*) a, (void*) b, (void*) c, (void*) d, (void*) r, (void*) t);

    return ret;
}
#ifndef __APPLE__
double flexiblas_real_dladiv2(void* a, void* b, void* c, void* d, void* r, void* t) __attribute__((alias("flexiblas_real_dladiv2_")));
#else
double flexiblas_real_dladiv2(void* a, void* b, void* c, void* d, void* r, void* t){return flexiblas_real_dladiv2_((void*) a, (void*) b, (void*) c, (void*) d, (void*) r, (void*) t);}
#endif




/* Chainloader for Hooks */


double flexiblas_chain_dladiv2_(void* a, void* b, void* c, void* d, void* r, void* t)
{
    double (*fn) (void* a, void* b, void* c, void* d, void* r, void* t);
    double (*fn_hook) (void* a, void* b, void* c, void* d, void* r, void* t);
    double ret;

    *(void **) &fn      = current_backend->lapack.dladiv2.f77_blas_function;

    hook_pos_dladiv2 ++;
    if( hook_pos_dladiv2 < __flexiblas_hooks->dladiv2.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dladiv2.f77_hook_function[hook_pos_dladiv2];
        ret = fn_hook((void*) a, (void*) b, (void*) c, (void*) d, (void*) r, (void*) t);
    } else {
        hook_pos_dladiv2 = 0;
        ret = fn((void*) a, (void*) b, (void*) c, (void*) d, (void*) r, (void*) t);
    }
    return ret;
}
#ifndef __APPLE__
double flexiblas_chain_dladiv2(void* a, void* b, void* c, void* d, void* r, void* t) __attribute__((alias("flexiblas_chain_dladiv2_")));
#else
double flexiblas_chain_dladiv2(void* a, void* b, void* c, void* d, void* r, void* t){return flexiblas_chain_dladiv2_((void*) a, (void*) b, (void*) c, (void*) d, (void*) r, (void*) t);}
#endif



