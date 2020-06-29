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
 /* Generated: Wed Mar 28 11:20:05 2018 */
        
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



static TLS_STORE uint8_t hook_pos_zlaqsy = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zlaqsy,ZLAQSY)(char* uplo, blasint* n, double complex* a, blasint* lda, double* s, double* scond, double* amax, char* equed)
#else
void FC_GLOBAL(zlaqsy,ZLAQSY)(char* uplo, blasint* n, double complex* a, blasint* lda, double* s, double* scond, double* amax, char* equed)
#endif
{
	void (*fn) (void* uplo, void* n, void* a, void* lda, void* s, void* scond, void* amax, void* equed);
	void (*fn_hook) (void* uplo, void* n, void* a, void* lda, void* s, void* scond, void* amax, void* equed);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.zlaqsy.f77_blas_function; 
	fn_hook = __flexiblas_hooks->zlaqsy.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) scond, (void*) amax, (void*) equed); 
		return;
	} else {
		hook_pos_zlaqsy = 0;
		fn_hook((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) scond, (void*) amax, (void*) equed);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void zlaqsy_(char* uplo, blasint* n, double complex* a, blasint* lda, double* s, double* scond, double* amax, char* equed) __attribute__((alias(MTS(FC_GLOBAL(zlaqsy,ZLAQSY)))));
#else
void zlaqsy(char* uplo, blasint* n, double complex* a, blasint* lda, double* s, double* scond, double* amax, char* equed) __attribute__((alias(MTS(FC_GLOBAL(zlaqsy,ZLAQSY)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zlaqsy_(void* uplo, void* n, void* a, void* lda, void* s, void* scond, void* amax, void* equed)
{
	void (*fn) (void* uplo, void* n, void* a, void* lda, void* s, void* scond, void* amax, void* equed);

	fn = current_backend->lapack.zlaqsy.f77_blas_function; 

		fn((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) scond, (void*) amax, (void*) equed); 

	return;
}

void flexiblas_real_zlaqsy(void* uplo, void* n, void* a, void* lda, void* s, void* scond, void* amax, void* equed)  __attribute__((alias("flexiblas_real_zlaqsy_")));





/* Chainloader for Hooks */


void flexiblas_chain_zlaqsy_(void* uplo, void* n, void* a, void* lda, void* s, void* scond, void* amax, void* equed)
{
	void (*fn) (void* uplo, void* n, void* a, void* lda, void* s, void* scond, void* amax, void* equed);
	void (*fn_hook) (void* uplo, void* n, void* a, void* lda, void* s, void* scond, void* amax, void* equed);

	fn      = current_backend->lapack.zlaqsy.f77_blas_function; 

    hook_pos_zlaqsy ++;
    if( hook_pos_zlaqsy < __flexiblas_hooks->zlaqsy.nhook) {
        fn_hook = __flexiblas_hooks->zlaqsy.f77_hook_function[hook_pos_zlaqsy];
        fn_hook((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) scond, (void*) amax, (void*) equed);
    } else {
        hook_pos_zlaqsy = 0;
		fn((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) scond, (void*) amax, (void*) equed); 
	}
	return;
}

void flexiblas_chain_zlaqsy(void* uplo, void* n, void* a, void* lda, void* s, void* scond, void* amax, void* equed)  __attribute__((alias("flexiblas_chain_zlaqsy_")));




