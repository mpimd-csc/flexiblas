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



static TLS_STORE uint8_t hook_pos_slapy3 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
float FC_GLOBAL(slapy3,SLAPY3)(float* x, float* y, float* z)
#else
float FC_GLOBAL(slapy3,SLAPY3)(float* x, float* y, float* z)
#endif
{
	float (*fn) (void* x, void* y, void* z);
	float (*fn_hook) (void* x, void* y, void* z);
	float ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.slapy3.f77_blas_function; 
	fn_hook = __flexiblas_hooks->slapy3.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		ret = fn((void*) x, (void*) y, (void*) z); 
		return ret; 
	} else {
		hook_pos_slapy3 = 0;
		ret=fn_hook((void*) x, (void*) y, (void*) z);
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
float slapy3_(float* x, float* y, float* z) __attribute__((alias(MTS(FC_GLOBAL(slapy3,SLAPY3)))));
#else
float slapy3(float* x, float* y, float* z) __attribute__((alias(MTS(FC_GLOBAL(slapy3,SLAPY3)))));
#endif




/* Real Implementation for Hooks */


float flexiblas_real_slapy3_(void* x, void* y, void* z)
{
	float (*fn) (void* x, void* y, void* z);
	float ret;

	fn = current_backend->lapack.slapy3.f77_blas_function; 

		ret = fn((void*) x, (void*) y, (void*) z); 

	return ret ;
}

float flexiblas_real_slapy3(void* x, void* y, void* z)  __attribute__((alias("flexiblas_real_slapy3_")));





/* Chainloader for Hooks */


float flexiblas_chain_slapy3_(void* x, void* y, void* z)
{
	float (*fn) (void* x, void* y, void* z);
	float (*fn_hook) (void* x, void* y, void* z);
	float ret;

	fn      = current_backend->lapack.slapy3.f77_blas_function; 

    hook_pos_slapy3 ++;
    if( hook_pos_slapy3 < __flexiblas_hooks->slapy3.nhook) {
        fn_hook = __flexiblas_hooks->slapy3.f77_hook_function[hook_pos_slapy3];
        ret = fn_hook((void*) x, (void*) y, (void*) z);
    } else {
        hook_pos_slapy3 = 0;
		ret = fn((void*) x, (void*) y, (void*) z); 
	}
	return ret ;
}

float flexiblas_chain_slapy3(void* x, void* y, void* z)  __attribute__((alias("flexiblas_chain_slapy3_")));




