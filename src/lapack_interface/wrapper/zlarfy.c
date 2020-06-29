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



static TLS_STORE uint8_t hook_pos_zlarfy = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zlarfy,ZLARFY)(char* uplo, blasint* n, double complex* v, blasint* incv, double complex* tau, double complex* c, blasint* ldc, double complex* work)
#else
void FC_GLOBAL(zlarfy,ZLARFY)(char* uplo, blasint* n, double complex* v, blasint* incv, double complex* tau, double complex* c, blasint* ldc, double complex* work)
#endif
{
	void (*fn) (void* uplo, void* n, void* v, void* incv, void* tau, void* c, void* ldc, void* work);
	void (*fn_hook) (void* uplo, void* n, void* v, void* incv, void* tau, void* c, void* ldc, void* work);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.zlarfy.f77_blas_function; 
	fn_hook = __flexiblas_hooks->zlarfy.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) uplo, (void*) n, (void*) v, (void*) incv, (void*) tau, (void*) c, (void*) ldc, (void*) work); 
		return;
	} else {
		hook_pos_zlarfy = 0;
		fn_hook((void*) uplo, (void*) n, (void*) v, (void*) incv, (void*) tau, (void*) c, (void*) ldc, (void*) work);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void zlarfy_(char* uplo, blasint* n, double complex* v, blasint* incv, double complex* tau, double complex* c, blasint* ldc, double complex* work) __attribute__((alias(MTS(FC_GLOBAL(zlarfy,ZLARFY)))));
#else
void zlarfy(char* uplo, blasint* n, double complex* v, blasint* incv, double complex* tau, double complex* c, blasint* ldc, double complex* work) __attribute__((alias(MTS(FC_GLOBAL(zlarfy,ZLARFY)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zlarfy_(void* uplo, void* n, void* v, void* incv, void* tau, void* c, void* ldc, void* work)
{
	void (*fn) (void* uplo, void* n, void* v, void* incv, void* tau, void* c, void* ldc, void* work);

	fn = current_backend->lapack.zlarfy.f77_blas_function; 

		fn((void*) uplo, (void*) n, (void*) v, (void*) incv, (void*) tau, (void*) c, (void*) ldc, (void*) work); 

	return;
}

void flexiblas_real_zlarfy(void* uplo, void* n, void* v, void* incv, void* tau, void* c, void* ldc, void* work)  __attribute__((alias("flexiblas_real_zlarfy_")));





/* Chainloader for Hooks */


void flexiblas_chain_zlarfy_(void* uplo, void* n, void* v, void* incv, void* tau, void* c, void* ldc, void* work)
{
	void (*fn) (void* uplo, void* n, void* v, void* incv, void* tau, void* c, void* ldc, void* work);
	void (*fn_hook) (void* uplo, void* n, void* v, void* incv, void* tau, void* c, void* ldc, void* work);

	fn      = current_backend->lapack.zlarfy.f77_blas_function; 

    hook_pos_zlarfy ++;
    if( hook_pos_zlarfy < __flexiblas_hooks->zlarfy.nhook) {
        fn_hook = __flexiblas_hooks->zlarfy.f77_hook_function[hook_pos_zlarfy];
        fn_hook((void*) uplo, (void*) n, (void*) v, (void*) incv, (void*) tau, (void*) c, (void*) ldc, (void*) work);
    } else {
        hook_pos_zlarfy = 0;
		fn((void*) uplo, (void*) n, (void*) v, (void*) incv, (void*) tau, (void*) c, (void*) ldc, (void*) work); 
	}
	return;
}

void flexiblas_chain_zlarfy(void* uplo, void* n, void* v, void* incv, void* tau, void* c, void* ldc, void* work)  __attribute__((alias("flexiblas_chain_zlarfy_")));




