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



static TLS_STORE uint8_t hook_pos_sladiv2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
float FC_GLOBAL(sladiv2,SLADIV2)(float* a, float* b, float* c, float* d, float* r, float* t)
#else
float FC_GLOBAL(sladiv2,SLADIV2)(float* a, float* b, float* c, float* d, float* r, float* t)
#endif
{
	float (*fn) (void* a, void* b, void* c, void* d, void* r, void* t);
	float (*fn_hook) (void* a, void* b, void* c, void* d, void* r, void* t);
	float ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.sladiv2.f77_blas_function; 
	fn_hook = __flexiblas_hooks->sladiv2.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		ret = fn((void*) a, (void*) b, (void*) c, (void*) d, (void*) r, (void*) t); 
		return ret; 
	} else {
		hook_pos_sladiv2 = 0;
		ret=fn_hook((void*) a, (void*) b, (void*) c, (void*) d, (void*) r, (void*) t);
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
float sladiv2_(float* a, float* b, float* c, float* d, float* r, float* t) __attribute__((alias(MTS(FC_GLOBAL(sladiv2,SLADIV2)))));
#else
float sladiv2(float* a, float* b, float* c, float* d, float* r, float* t) __attribute__((alias(MTS(FC_GLOBAL(sladiv2,SLADIV2)))));
#endif




/* Real Implementation for Hooks */


float flexiblas_real_sladiv2_(void* a, void* b, void* c, void* d, void* r, void* t)
{
	float (*fn) (void* a, void* b, void* c, void* d, void* r, void* t);
	float ret;

	fn = current_backend->lapack.sladiv2.f77_blas_function; 

		ret = fn((void*) a, (void*) b, (void*) c, (void*) d, (void*) r, (void*) t); 

	return ret ;
}

float flexiblas_real_sladiv2(void* a, void* b, void* c, void* d, void* r, void* t)  __attribute__((alias("flexiblas_real_sladiv2_")));





/* Chainloader for Hooks */


float flexiblas_chain_sladiv2_(void* a, void* b, void* c, void* d, void* r, void* t)
{
	float (*fn) (void* a, void* b, void* c, void* d, void* r, void* t);
	float (*fn_hook) (void* a, void* b, void* c, void* d, void* r, void* t);
	float ret;

	fn      = current_backend->lapack.sladiv2.f77_blas_function; 

    hook_pos_sladiv2 ++;
    if( hook_pos_sladiv2 < __flexiblas_hooks->sladiv2.nhook) {
        fn_hook = __flexiblas_hooks->sladiv2.f77_hook_function[hook_pos_sladiv2];
        ret = fn_hook((void*) a, (void*) b, (void*) c, (void*) d, (void*) r, (void*) t);
    } else {
        hook_pos_sladiv2 = 0;
		ret = fn((void*) a, (void*) b, (void*) c, (void*) d, (void*) r, (void*) t); 
	}
	return ret ;
}

float flexiblas_chain_sladiv2(void* a, void* b, void* c, void* d, void* r, void* t)  __attribute__((alias("flexiblas_chain_sladiv2_")));




