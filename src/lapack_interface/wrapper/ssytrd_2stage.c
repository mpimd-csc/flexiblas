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



static TLS_STORE uint8_t hook_pos_ssytrd_2stage = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL_(ssytrd_2stage,SSYTRD_2STAGE)(char* vect, char* uplo, blasint* n, float* a, blasint* lda, float* d, float* e, float* tau, float* hous2, blasint* lhous2, float* work, blasint* lwork, blasint* info)
#else
void FC_GLOBAL_(ssytrd_2stage,SSYTRD_2STAGE)(char* vect, char* uplo, blasint* n, float* a, blasint* lda, float* d, float* e, float* tau, float* hous2, blasint* lhous2, float* work, blasint* lwork, blasint* info)
#endif
{
	void (*fn) (void* vect, void* uplo, void* n, void* a, void* lda, void* d, void* e, void* tau, void* hous2, void* lhous2, void* work, void* lwork, void* info);
	void (*fn_hook) (void* vect, void* uplo, void* n, void* a, void* lda, void* d, void* e, void* tau, void* hous2, void* lhous2, void* work, void* lwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.ssytrd_2stage.f77_blas_function; 
	fn_hook = __flexiblas_hooks->ssytrd_2stage.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) vect, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) d, (void*) e, (void*) tau, (void*) hous2, (void*) lhous2, (void*) work, (void*) lwork, (void*) info); 
		return;
	} else {
		hook_pos_ssytrd_2stage = 0;
		fn_hook((void*) vect, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) d, (void*) e, (void*) tau, (void*) hous2, (void*) lhous2, (void*) work, (void*) lwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void ssytrd_2stage_(char* vect, char* uplo, blasint* n, float* a, blasint* lda, float* d, float* e, float* tau, float* hous2, blasint* lhous2, float* work, blasint* lwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL_(ssytrd_2stage,SSYTRD_2STAGE)))));
#else
void ssytrd_2stage(char* vect, char* uplo, blasint* n, float* a, blasint* lda, float* d, float* e, float* tau, float* hous2, blasint* lhous2, float* work, blasint* lwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL_(ssytrd_2stage,SSYTRD_2STAGE)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_ssytrd_2stage_(void* vect, void* uplo, void* n, void* a, void* lda, void* d, void* e, void* tau, void* hous2, void* lhous2, void* work, void* lwork, void* info)
{
	void (*fn) (void* vect, void* uplo, void* n, void* a, void* lda, void* d, void* e, void* tau, void* hous2, void* lhous2, void* work, void* lwork, void* info);

	fn = current_backend->lapack.ssytrd_2stage.f77_blas_function; 

		fn((void*) vect, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) d, (void*) e, (void*) tau, (void*) hous2, (void*) lhous2, (void*) work, (void*) lwork, (void*) info); 

	return;
}

void flexiblas_real_ssytrd_2stage(void* vect, void* uplo, void* n, void* a, void* lda, void* d, void* e, void* tau, void* hous2, void* lhous2, void* work, void* lwork, void* info)  __attribute__((alias("flexiblas_real_ssytrd_2stage_")));





/* Chainloader for Hooks */


void flexiblas_chain_ssytrd_2stage_(void* vect, void* uplo, void* n, void* a, void* lda, void* d, void* e, void* tau, void* hous2, void* lhous2, void* work, void* lwork, void* info)
{
	void (*fn) (void* vect, void* uplo, void* n, void* a, void* lda, void* d, void* e, void* tau, void* hous2, void* lhous2, void* work, void* lwork, void* info);
	void (*fn_hook) (void* vect, void* uplo, void* n, void* a, void* lda, void* d, void* e, void* tau, void* hous2, void* lhous2, void* work, void* lwork, void* info);

	fn      = current_backend->lapack.ssytrd_2stage.f77_blas_function; 

    hook_pos_ssytrd_2stage ++;
    if( hook_pos_ssytrd_2stage < __flexiblas_hooks->ssytrd_2stage.nhook) {
        fn_hook = __flexiblas_hooks->ssytrd_2stage.f77_hook_function[hook_pos_ssytrd_2stage];
        fn_hook((void*) vect, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) d, (void*) e, (void*) tau, (void*) hous2, (void*) lhous2, (void*) work, (void*) lwork, (void*) info);
    } else {
        hook_pos_ssytrd_2stage = 0;
		fn((void*) vect, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) d, (void*) e, (void*) tau, (void*) hous2, (void*) lhous2, (void*) work, (void*) lwork, (void*) info); 
	}
	return;
}

void flexiblas_chain_ssytrd_2stage(void* vect, void* uplo, void* n, void* a, void* lda, void* d, void* e, void* tau, void* hous2, void* lhous2, void* work, void* lwork, void* info)  __attribute__((alias("flexiblas_chain_ssytrd_2stage_")));




