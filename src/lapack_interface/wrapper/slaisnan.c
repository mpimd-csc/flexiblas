//    SPDX-License-Identifier: LGPL-3.0-or-later
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


#if __GNUC__ > 7
typedef size_t fortran_charlen_t;
#else
typedef int fortran_charlen_t;
#endif

#ifdef INTEGER8
#define blasint int64_t
#else
#define blasint int
#endif



static TLS_STORE uint8_t hook_pos_slaisnan = 0;
#ifdef FLEXIBLAS_ABI_INTEL
int FC_GLOBAL(slaisnan,SLAISNAN)(float* sin1, float* sin2)
#else
int FC_GLOBAL(slaisnan,SLAISNAN)(float* sin1, float* sin2)
#endif
{
	blasint (*fn) (void* sin1, void* sin2);
	blasint (*fn_hook) (void* sin1, void* sin2);
	blasint ret;

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
		ret=fn_hook((void*) sin1, (void*) sin2);
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
int slaisnan_(float* sin1, float* sin2) __attribute__((alias(MTS(FC_GLOBAL(slaisnan,SLAISNAN)))));
#else
#ifndef __APPLE__
int slaisnan(float* sin1, float* sin2) __attribute__((alias(MTS(FC_GLOBAL(slaisnan,SLAISNAN)))));
#else
int slaisnan(float* sin1, float* sin2){ return FC_GLOBAL(slaisnan,SLAISNAN)((void*) sin1, (void*) sin2); }
#endif
#endif




/* Real Implementation for Hooks */


blasint flexiblas_real_slaisnan_(void* sin1, void* sin2)
{
	blasint (*fn) (void* sin1, void* sin2);
	blasint ret;

	*(void **) & fn = current_backend->lapack.slaisnan.f77_blas_function; 

		ret = fn((void*) sin1, (void*) sin2); 

	return ret ;
}
#ifndef __APPLE__
blasint flexiblas_real_slaisnan(void* sin1, void* sin2) __attribute__((alias("flexiblas_real_slaisnan_")));
#else
blasint flexiblas_real_slaisnan(void* sin1, void* sin2){return flexiblas_real_slaisnan_((void*) sin1, (void*) sin2);}
#endif




/* Chainloader for Hooks */


blasint flexiblas_chain_slaisnan_(void* sin1, void* sin2)
{
	blasint (*fn) (void* sin1, void* sin2);
	blasint (*fn_hook) (void* sin1, void* sin2);
	blasint ret;

	*(void **) &fn      = current_backend->lapack.slaisnan.f77_blas_function; 

    hook_pos_slaisnan ++;
    if( hook_pos_slaisnan < __flexiblas_hooks->slaisnan.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->slaisnan.f77_hook_function[hook_pos_slaisnan];
        ret = fn_hook((void*) sin1, (void*) sin2);
    } else {
        hook_pos_slaisnan = 0;
		ret = fn((void*) sin1, (void*) sin2); 
	}
	return ret ;
}
#ifndef __APPLE__
blasint flexiblas_chain_slaisnan(void* sin1, void* sin2) __attribute__((alias("flexiblas_chain_slaisnan_")));
#else
blasint flexiblas_chain_slaisnan(void* sin1, void* sin2){return flexiblas_chain_slaisnan_((void*) sin1, (void*) sin2);}
#endif



