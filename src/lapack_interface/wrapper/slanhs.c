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



static TLS_STORE uint8_t hook_pos_slanhs = 0;
#ifdef FLEXIBLAS_ABI_INTEL
float FC_GLOBAL(slanhs,SLANHS)(char* norm, blasint* n, float* a, blasint* lda, float* work)
#else
float FC_GLOBAL(slanhs,SLANHS)(char* norm, blasint* n, float* a, blasint* lda, float* work)
#endif
{
	float (*fn) (void* norm, void* n, void* a, void* lda, void* work);
	float (*fn_hook) (void* norm, void* n, void* a, void* lda, void* work);
	float ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.slanhs.f77_blas_function; 
	fn_hook = __flexiblas_hooks->slanhs.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		ret = fn((void*) norm, (void*) n, (void*) a, (void*) lda, (void*) work); 
		return ret; 
	} else {
		hook_pos_slanhs = 0;
		ret=fn_hook((void*) norm, (void*) n, (void*) a, (void*) lda, (void*) work);
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
float slanhs_(char* norm, blasint* n, float* a, blasint* lda, float* work) __attribute__((alias(MTS(FC_GLOBAL(slanhs,SLANHS)))));
#else
float slanhs(char* norm, blasint* n, float* a, blasint* lda, float* work) __attribute__((alias(MTS(FC_GLOBAL(slanhs,SLANHS)))));
#endif




/* Real Implementation for Hooks */


float flexiblas_real_slanhs_(void* norm, void* n, void* a, void* lda, void* work)
{
	float (*fn) (void* norm, void* n, void* a, void* lda, void* work);
	float ret;

	fn = current_backend->lapack.slanhs.f77_blas_function; 

		ret = fn((void*) norm, (void*) n, (void*) a, (void*) lda, (void*) work); 

	return ret ;
}

float flexiblas_real_slanhs(void* norm, void* n, void* a, void* lda, void* work)  __attribute__((alias("flexiblas_real_slanhs_")));





/* Chainloader for Hooks */


float flexiblas_chain_slanhs_(void* norm, void* n, void* a, void* lda, void* work)
{
	float (*fn) (void* norm, void* n, void* a, void* lda, void* work);
	float (*fn_hook) (void* norm, void* n, void* a, void* lda, void* work);
	float ret;

	fn      = current_backend->lapack.slanhs.f77_blas_function; 

    hook_pos_slanhs ++;
    if( hook_pos_slanhs < __flexiblas_hooks->slanhs.nhook) {
        fn_hook = __flexiblas_hooks->slanhs.f77_hook_function[hook_pos_slanhs];
        ret = fn_hook((void*) norm, (void*) n, (void*) a, (void*) lda, (void*) work);
    } else {
        hook_pos_slanhs = 0;
		ret = fn((void*) norm, (void*) n, (void*) a, (void*) lda, (void*) work); 
	}
	return ret ;
}

float flexiblas_chain_slanhs(void* norm, void* n, void* a, void* lda, void* work)  __attribute__((alias("flexiblas_chain_slanhs_")));




