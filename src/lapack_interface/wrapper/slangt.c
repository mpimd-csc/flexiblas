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



static TLS_STORE uint8_t hook_pos_slangt = 0;
#ifdef FLEXIBLAS_ABI_INTEL
float FC_GLOBAL(slangt,SLANGT)(char* norm, blasint* n, float* dl, float* d, float* du)
#else
float FC_GLOBAL(slangt,SLANGT)(char* norm, blasint* n, float* dl, float* d, float* du)
#endif
{
	float (*fn) (void* norm, void* n, void* dl, void* d, void* du);
	float (*fn_hook) (void* norm, void* n, void* dl, void* d, void* du);
	float ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.slangt.f77_blas_function; 
	fn_hook = __flexiblas_hooks->slangt.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		ret = fn((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du); 
		return ret; 
	} else {
		hook_pos_slangt = 0;
		ret=fn_hook((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du);
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
float slangt_(char* norm, blasint* n, float* dl, float* d, float* du) __attribute__((alias(MTS(FC_GLOBAL(slangt,SLANGT)))));
#else
float slangt(char* norm, blasint* n, float* dl, float* d, float* du) __attribute__((alias(MTS(FC_GLOBAL(slangt,SLANGT)))));
#endif




/* Real Implementation for Hooks */


float flexiblas_real_slangt_(void* norm, void* n, void* dl, void* d, void* du)
{
	float (*fn) (void* norm, void* n, void* dl, void* d, void* du);
	float ret;

	fn = current_backend->lapack.slangt.f77_blas_function; 

		ret = fn((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du); 

	return ret ;
}

float flexiblas_real_slangt(void* norm, void* n, void* dl, void* d, void* du)  __attribute__((alias("flexiblas_real_slangt_")));





/* Chainloader for Hooks */


float flexiblas_chain_slangt_(void* norm, void* n, void* dl, void* d, void* du)
{
	float (*fn) (void* norm, void* n, void* dl, void* d, void* du);
	float (*fn_hook) (void* norm, void* n, void* dl, void* d, void* du);
	float ret;

	fn      = current_backend->lapack.slangt.f77_blas_function; 

    hook_pos_slangt ++;
    if( hook_pos_slangt < __flexiblas_hooks->slangt.nhook) {
        fn_hook = __flexiblas_hooks->slangt.f77_hook_function[hook_pos_slangt];
        ret = fn_hook((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du);
    } else {
        hook_pos_slangt = 0;
		ret = fn((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du); 
	}
	return ret ;
}

float flexiblas_chain_slangt(void* norm, void* n, void* dl, void* d, void* du)  __attribute__((alias("flexiblas_chain_slangt_")));




