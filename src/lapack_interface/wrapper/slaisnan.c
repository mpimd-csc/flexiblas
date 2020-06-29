/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) Martin Koehler, 2013-2020
 */
 /* This file it automatically generated. Please do not edit. */
 /* Generated: Wed Mar 28 11:20:04 2018 */
        
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <complex.h>

#include "fortran_mangle.h"

#include "flexiblas.h"


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
	fn = current_backend->lapack.slaisnan.f77_blas_function; 
	fn_hook = __flexiblas_hooks->slaisnan.f77_hook_function[0]; 
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
int slaisnan(float* sin1, float* sin2) __attribute__((alias(MTS(FC_GLOBAL(slaisnan,SLAISNAN)))));
#endif




/* Real Implementation for Hooks */


blasint flexiblas_real_slaisnan_(void* sin1, void* sin2)
{
	blasint (*fn) (void* sin1, void* sin2);
	blasint ret;

	fn = current_backend->lapack.slaisnan.f77_blas_function; 

		ret = fn((void*) sin1, (void*) sin2); 

	return ret ;
}

blasint flexiblas_real_slaisnan(void* sin1, void* sin2)  __attribute__((alias("flexiblas_real_slaisnan_")));





/* Chainloader for Hooks */


blasint flexiblas_chain_slaisnan_(void* sin1, void* sin2)
{
	blasint (*fn) (void* sin1, void* sin2);
	blasint (*fn_hook) (void* sin1, void* sin2);
	blasint ret;

	fn      = current_backend->lapack.slaisnan.f77_blas_function; 

    hook_pos_slaisnan ++;
    if( hook_pos_slaisnan < __flexiblas_hooks->slaisnan.nhook) {
        fn_hook = __flexiblas_hooks->slaisnan.f77_hook_function[hook_pos_slaisnan];
        ret = fn_hook((void*) sin1, (void*) sin2);
    } else {
        hook_pos_slaisnan = 0;
		ret = fn((void*) sin1, (void*) sin2); 
	}
	return ret ;
}

blasint flexiblas_chain_slaisnan(void* sin1, void* sin2)  __attribute__((alias("flexiblas_chain_slaisnan_")));




