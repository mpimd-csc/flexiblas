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



static TLS_STORE uint8_t hook_pos_zlaqhe = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zlaqhe,ZLAQHE)(char* uplo, blasint* n, double complex* a, blasint* lda, double* s, double* scond, double* amax, char* equed)
#else
void FC_GLOBAL(zlaqhe,ZLAQHE)(char* uplo, blasint* n, double complex* a, blasint* lda, double* s, double* scond, double* amax, char* equed)
#endif
{
	void (*fn) (void* uplo, void* n, void* a, void* lda, void* s, void* scond, void* amax, void* equed);
	void (*fn_hook) (void* uplo, void* n, void* a, void* lda, void* s, void* scond, void* amax, void* equed);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.zlaqhe.f77_blas_function; 
	fn_hook = __flexiblas_hooks->zlaqhe.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) scond, (void*) amax, (void*) equed); 
		return;
	} else {
		hook_pos_zlaqhe = 0;
		fn_hook((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) scond, (void*) amax, (void*) equed);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void zlaqhe_(char* uplo, blasint* n, double complex* a, blasint* lda, double* s, double* scond, double* amax, char* equed) __attribute__((alias(MTS(FC_GLOBAL(zlaqhe,ZLAQHE)))));
#else
void zlaqhe(char* uplo, blasint* n, double complex* a, blasint* lda, double* s, double* scond, double* amax, char* equed) __attribute__((alias(MTS(FC_GLOBAL(zlaqhe,ZLAQHE)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zlaqhe_(void* uplo, void* n, void* a, void* lda, void* s, void* scond, void* amax, void* equed)
{
	void (*fn) (void* uplo, void* n, void* a, void* lda, void* s, void* scond, void* amax, void* equed);

	fn = current_backend->lapack.zlaqhe.f77_blas_function; 

		fn((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) scond, (void*) amax, (void*) equed); 

	return;
}

void flexiblas_real_zlaqhe(void* uplo, void* n, void* a, void* lda, void* s, void* scond, void* amax, void* equed)  __attribute__((alias("flexiblas_real_zlaqhe_")));





/* Chainloader for Hooks */


void flexiblas_chain_zlaqhe_(void* uplo, void* n, void* a, void* lda, void* s, void* scond, void* amax, void* equed)
{
	void (*fn) (void* uplo, void* n, void* a, void* lda, void* s, void* scond, void* amax, void* equed);
	void (*fn_hook) (void* uplo, void* n, void* a, void* lda, void* s, void* scond, void* amax, void* equed);

	fn      = current_backend->lapack.zlaqhe.f77_blas_function; 

    hook_pos_zlaqhe ++;
    if( hook_pos_zlaqhe < __flexiblas_hooks->zlaqhe.nhook) {
        fn_hook = __flexiblas_hooks->zlaqhe.f77_hook_function[hook_pos_zlaqhe];
        fn_hook((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) scond, (void*) amax, (void*) equed);
    } else {
        hook_pos_zlaqhe = 0;
		fn((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) scond, (void*) amax, (void*) equed); 
	}
	return;
}

void flexiblas_chain_zlaqhe(void* uplo, void* n, void* a, void* lda, void* s, void* scond, void* amax, void* equed)  __attribute__((alias("flexiblas_chain_zlaqhe_")));




