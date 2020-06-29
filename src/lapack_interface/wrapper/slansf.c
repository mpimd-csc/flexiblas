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



static TLS_STORE uint8_t hook_pos_slansf = 0;
#ifdef FLEXIBLAS_ABI_INTEL
float FC_GLOBAL(slansf,SLANSF)(char* norm, char* transr, char* uplo, blasint* n, float* a, float* work)
#else
float FC_GLOBAL(slansf,SLANSF)(char* norm, char* transr, char* uplo, blasint* n, float* a, float* work)
#endif
{
	float (*fn) (void* norm, void* transr, void* uplo, void* n, void* a, void* work);
	float (*fn_hook) (void* norm, void* transr, void* uplo, void* n, void* a, void* work);
	float ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.slansf.f77_blas_function; 
	fn_hook = __flexiblas_hooks->slansf.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		ret = fn((void*) norm, (void*) transr, (void*) uplo, (void*) n, (void*) a, (void*) work); 
		return ret; 
	} else {
		hook_pos_slansf = 0;
		ret=fn_hook((void*) norm, (void*) transr, (void*) uplo, (void*) n, (void*) a, (void*) work);
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
float slansf_(char* norm, char* transr, char* uplo, blasint* n, float* a, float* work) __attribute__((alias(MTS(FC_GLOBAL(slansf,SLANSF)))));
#else
float slansf(char* norm, char* transr, char* uplo, blasint* n, float* a, float* work) __attribute__((alias(MTS(FC_GLOBAL(slansf,SLANSF)))));
#endif




/* Real Implementation for Hooks */


float flexiblas_real_slansf_(void* norm, void* transr, void* uplo, void* n, void* a, void* work)
{
	float (*fn) (void* norm, void* transr, void* uplo, void* n, void* a, void* work);
	float ret;

	fn = current_backend->lapack.slansf.f77_blas_function; 

		ret = fn((void*) norm, (void*) transr, (void*) uplo, (void*) n, (void*) a, (void*) work); 

	return ret ;
}

float flexiblas_real_slansf(void* norm, void* transr, void* uplo, void* n, void* a, void* work)  __attribute__((alias("flexiblas_real_slansf_")));





/* Chainloader for Hooks */


float flexiblas_chain_slansf_(void* norm, void* transr, void* uplo, void* n, void* a, void* work)
{
	float (*fn) (void* norm, void* transr, void* uplo, void* n, void* a, void* work);
	float (*fn_hook) (void* norm, void* transr, void* uplo, void* n, void* a, void* work);
	float ret;

	fn      = current_backend->lapack.slansf.f77_blas_function; 

    hook_pos_slansf ++;
    if( hook_pos_slansf < __flexiblas_hooks->slansf.nhook) {
        fn_hook = __flexiblas_hooks->slansf.f77_hook_function[hook_pos_slansf];
        ret = fn_hook((void*) norm, (void*) transr, (void*) uplo, (void*) n, (void*) a, (void*) work);
    } else {
        hook_pos_slansf = 0;
		ret = fn((void*) norm, (void*) transr, (void*) uplo, (void*) n, (void*) a, (void*) work); 
	}
	return ret ;
}

float flexiblas_chain_slansf(void* norm, void* transr, void* uplo, void* n, void* a, void* work)  __attribute__((alias("flexiblas_chain_slansf_")));




