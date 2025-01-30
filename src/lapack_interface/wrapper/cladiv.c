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


static TLS_STORE uint8_t hook_pos_cladiv = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(cladiv,CLADIV)( float complex* returnvalue, float complex* x, float complex* y)
#else
float complex FC_GLOBAL(cladiv,CLADIV)(float complex* x, float complex* y)
#endif
{
    float complex (*fn) (void* x, void* y);
    void (*fn_intel) (float complex *ret, void* x, void* y);
    void (*fn_hook) (float complex *ret, void* x, void* y);
    float complex ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.cladiv.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->cladiv.f77_hook_function[0];
    *(void **) & fn_intel = *(void **) &fn;
    if ( fn_hook == NULL ) {
        if(current_backend->info.intel_interface == 0 ) {
            ret = fn((void*) x, (void*) y);
        } else {
            fn_intel( &ret, (void*) x, (void*) y);
        }

#ifdef FLEXIBLAS_ABI_INTEL
        *returnvalue = ret;
        return;
#else
        return ret;
#endif
    } else {
        hook_pos_cladiv = 0;
        fn_hook(&ret, (void*) x, (void*) y);

#ifdef FLEXIBLAS_ABI_INTEL
        *returnvalue = ret;
        return;
#else
        return ret;
#endif
    }
}
#ifndef __APPLE__
float complex FC_GLOBAL2(cladiv,CLADIV)(float complex* x, float complex* y) __attribute__((alias(MTS(FC_GLOBAL(cladiv,CLADIV)))));
float complex FC_GLOBAL3(cladiv,CLADIV)(float complex* x, float complex* y) __attribute__((alias(MTS(FC_GLOBAL(cladiv,CLADIV)))));
#else
float complex FC_GLOBAL2(cladiv,CLADIV)(float complex* x, float complex* y){ return FC_GLOBAL(cladiv,CLADIV)((void*) x, (void*) y); }
float complex FC_GLOBAL3(cladiv,CLADIV)(float complex* x, float complex* y){ return FC_GLOBAL(cladiv,CLADIV)((void*) x, (void*) y); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_cladiv_( float complex*returnvalue, void* x, void* y)
{
    float complex (*fn) (void* x, void* y);
    void (*fn_intel) (float complex *ret, void* x, void* y);
    float complex ret;

    *(void **) & fn = current_backend->lapack.cladiv.f77_blas_function;
    *(void **) & fn_intel = *(void **) &fn;

    if(current_backend->info.intel_interface == 0 ) {
        ret = fn((void*) x, (void*) y);
    } else {
        fn_intel( &ret, (void*) x, (void*) y);
    }


    *((float complex *)returnvalue) = ret;
    return;
}
#ifndef __APPLE__
void flexiblas_real_cladiv( float complex*returnvalue, void* x, void* y) __attribute__((alias("flexiblas_real_cladiv_")));
#else
void flexiblas_real_cladiv( float complex*returnvalue, void* x, void* y){flexiblas_real_cladiv_( (float complex*)returnvalue, (void*) x, (void*) y);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_cladiv_( float complex* returnvalue, void* x, void* y)
{
    float complex (*fn) (void* x, void* y);
    void (*fn_intel) (float complex *ret, void* x, void* y);
    void (*fn_hook) (float complex *ret, void* x, void* y);
    float complex ret;

    *(void **) &fn      = current_backend->lapack.cladiv.f77_blas_function;
    *(void **) & fn_intel = *(void **) &fn;

    hook_pos_cladiv ++;
    if( hook_pos_cladiv < __flexiblas_hooks->cladiv.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->cladiv.f77_hook_function[hook_pos_cladiv];
        fn_hook( &ret, (void*) x, (void*) y);
    } else {
        hook_pos_cladiv = 0;
        if(current_backend->info.intel_interface == 0 ) {
            ret = fn((void*) x, (void*) y);
        } else {
            fn_intel( &ret, (void*) x, (void*) y);
        }
    }

    *((float complex *)returnvalue) = ret;
    return;
}
#ifndef __APPLE__
void flexiblas_chain_cladiv( float complex*returnvalue, void* x, void* y) __attribute__((alias("flexiblas_chain_cladiv_")));
#else
void flexiblas_chain_cladiv( float complex*returnvalue, void* x, void* y){flexiblas_chain_cladiv_( (float complex*)returnvalue, (void*) x, (void*) y);}
#endif



