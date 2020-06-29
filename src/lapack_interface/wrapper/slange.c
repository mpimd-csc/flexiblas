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



static TLS_STORE uint8_t hook_pos_slange = 0;
#ifdef FLEXIBLAS_ABI_INTEL
float FC_GLOBAL(slange,SLANGE)(char* norm, blasint* m, blasint* n, float* a, blasint* lda, float* work)
#else
float FC_GLOBAL(slange,SLANGE)(char* norm, blasint* m, blasint* n, float* a, blasint* lda, float* work)
#endif
{
	float (*fn) (void* norm, void* m, void* n, void* a, void* lda, void* work);
	float (*fn_hook) (void* norm, void* m, void* n, void* a, void* lda, void* work);
	float ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.slange.f77_blas_function; 
	fn_hook = __flexiblas_hooks->slange.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		ret = fn((void*) norm, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) work); 
		return ret; 
	} else {
		hook_pos_slange = 0;
		ret=fn_hook((void*) norm, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) work);
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
float slange_(char* norm, blasint* m, blasint* n, float* a, blasint* lda, float* work) __attribute__((alias(MTS(FC_GLOBAL(slange,SLANGE)))));
#else
float slange(char* norm, blasint* m, blasint* n, float* a, blasint* lda, float* work) __attribute__((alias(MTS(FC_GLOBAL(slange,SLANGE)))));
#endif




/* Real Implementation for Hooks */


float flexiblas_real_slange_(void* norm, void* m, void* n, void* a, void* lda, void* work)
{
	float (*fn) (void* norm, void* m, void* n, void* a, void* lda, void* work);
	float ret;

	fn = current_backend->lapack.slange.f77_blas_function; 

		ret = fn((void*) norm, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) work); 

	return ret ;
}

float flexiblas_real_slange(void* norm, void* m, void* n, void* a, void* lda, void* work)  __attribute__((alias("flexiblas_real_slange_")));





/* Chainloader for Hooks */


float flexiblas_chain_slange_(void* norm, void* m, void* n, void* a, void* lda, void* work)
{
	float (*fn) (void* norm, void* m, void* n, void* a, void* lda, void* work);
	float (*fn_hook) (void* norm, void* m, void* n, void* a, void* lda, void* work);
	float ret;

	fn      = current_backend->lapack.slange.f77_blas_function; 

    hook_pos_slange ++;
    if( hook_pos_slange < __flexiblas_hooks->slange.nhook) {
        fn_hook = __flexiblas_hooks->slange.f77_hook_function[hook_pos_slange];
        ret = fn_hook((void*) norm, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) work);
    } else {
        hook_pos_slange = 0;
		ret = fn((void*) norm, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) work); 
	}
	return ret ;
}

float flexiblas_chain_slange(void* norm, void* m, void* n, void* a, void* lda, void* work)  __attribute__((alias("flexiblas_chain_slange_")));




