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



static TLS_STORE uint8_t hook_pos_slaset = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slaset,SLASET)(char* uplo, blasint* m, blasint* n, float* alpha, float* beta, float* a, blasint* lda)
#else
void FC_GLOBAL(slaset,SLASET)(char* uplo, blasint* m, blasint* n, float* alpha, float* beta, float* a, blasint* lda)
#endif
{
	void (*fn) (void* uplo, void* m, void* n, void* alpha, void* beta, void* a, void* lda);
	void (*fn_hook) (void* uplo, void* m, void* n, void* alpha, void* beta, void* a, void* lda);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.slaset.f77_blas_function; 
	fn_hook = __flexiblas_hooks->slaset.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) uplo, (void*) m, (void*) n, (void*) alpha, (void*) beta, (void*) a, (void*) lda); 
		return;
	} else {
		hook_pos_slaset = 0;
		fn_hook((void*) uplo, (void*) m, (void*) n, (void*) alpha, (void*) beta, (void*) a, (void*) lda);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void slaset_(char* uplo, blasint* m, blasint* n, float* alpha, float* beta, float* a, blasint* lda) __attribute__((alias(MTS(FC_GLOBAL(slaset,SLASET)))));
#else
void slaset(char* uplo, blasint* m, blasint* n, float* alpha, float* beta, float* a, blasint* lda) __attribute__((alias(MTS(FC_GLOBAL(slaset,SLASET)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slaset_(void* uplo, void* m, void* n, void* alpha, void* beta, void* a, void* lda)
{
	void (*fn) (void* uplo, void* m, void* n, void* alpha, void* beta, void* a, void* lda);

	fn = current_backend->lapack.slaset.f77_blas_function; 

		fn((void*) uplo, (void*) m, (void*) n, (void*) alpha, (void*) beta, (void*) a, (void*) lda); 

	return;
}

void flexiblas_real_slaset(void* uplo, void* m, void* n, void* alpha, void* beta, void* a, void* lda)  __attribute__((alias("flexiblas_real_slaset_")));





/* Chainloader for Hooks */


void flexiblas_chain_slaset_(void* uplo, void* m, void* n, void* alpha, void* beta, void* a, void* lda)
{
	void (*fn) (void* uplo, void* m, void* n, void* alpha, void* beta, void* a, void* lda);
	void (*fn_hook) (void* uplo, void* m, void* n, void* alpha, void* beta, void* a, void* lda);

	fn      = current_backend->lapack.slaset.f77_blas_function; 

    hook_pos_slaset ++;
    if( hook_pos_slaset < __flexiblas_hooks->slaset.nhook) {
        fn_hook = __flexiblas_hooks->slaset.f77_hook_function[hook_pos_slaset];
        fn_hook((void*) uplo, (void*) m, (void*) n, (void*) alpha, (void*) beta, (void*) a, (void*) lda);
    } else {
        hook_pos_slaset = 0;
		fn((void*) uplo, (void*) m, (void*) n, (void*) alpha, (void*) beta, (void*) a, (void*) lda); 
	}
	return;
}

void flexiblas_chain_slaset(void* uplo, void* m, void* n, void* alpha, void* beta, void* a, void* lda)  __attribute__((alias("flexiblas_chain_slaset_")));




