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



static TLS_STORE uint8_t hook_pos_slapy2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
float FC_GLOBAL(slapy2,SLAPY2)(float* x, float* y)
#else
float FC_GLOBAL(slapy2,SLAPY2)(float* x, float* y)
#endif
{
	float (*fn) (void* x, void* y);
	float (*fn_hook) (void* x, void* y);
	float ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.slapy2.f77_blas_function; 
	fn_hook = __flexiblas_hooks->slapy2.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		ret = fn((void*) x, (void*) y); 
		return ret; 
	} else {
		hook_pos_slapy2 = 0;
		ret=fn_hook((void*) x, (void*) y);
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
float slapy2_(float* x, float* y) __attribute__((alias(MTS(FC_GLOBAL(slapy2,SLAPY2)))));
#else
float slapy2(float* x, float* y) __attribute__((alias(MTS(FC_GLOBAL(slapy2,SLAPY2)))));
#endif




/* Real Implementation for Hooks */


float flexiblas_real_slapy2_(void* x, void* y)
{
	float (*fn) (void* x, void* y);
	float ret;

	fn = current_backend->lapack.slapy2.f77_blas_function; 

		ret = fn((void*) x, (void*) y); 

	return ret ;
}

float flexiblas_real_slapy2(void* x, void* y)  __attribute__((alias("flexiblas_real_slapy2_")));





/* Chainloader for Hooks */


float flexiblas_chain_slapy2_(void* x, void* y)
{
	float (*fn) (void* x, void* y);
	float (*fn_hook) (void* x, void* y);
	float ret;

	fn      = current_backend->lapack.slapy2.f77_blas_function; 

    hook_pos_slapy2 ++;
    if( hook_pos_slapy2 < __flexiblas_hooks->slapy2.nhook) {
        fn_hook = __flexiblas_hooks->slapy2.f77_hook_function[hook_pos_slapy2];
        ret = fn_hook((void*) x, (void*) y);
    } else {
        hook_pos_slapy2 = 0;
		ret = fn((void*) x, (void*) y); 
	}
	return ret ;
}

float flexiblas_chain_slapy2(void* x, void* y)  __attribute__((alias("flexiblas_chain_slapy2_")));




