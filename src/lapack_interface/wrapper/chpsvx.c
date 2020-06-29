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
 /* Generated: Wed Mar 28 11:20:03 2018 */
        
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



static TLS_STORE uint8_t hook_pos_chpsvx = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(chpsvx,CHPSVX)(char* fact, char* uplo, blasint* n, blasint* nrhs, float complex* ap, float complex* afp, blasint* ipiv, float complex* b, blasint* ldb, float complex* x, blasint* ldx, float* rcond, float* ferr, float* berr, float complex* work, float* rwork, blasint* info)
#else
void FC_GLOBAL(chpsvx,CHPSVX)(char* fact, char* uplo, blasint* n, blasint* nrhs, float complex* ap, float complex* afp, blasint* ipiv, float complex* b, blasint* ldb, float complex* x, blasint* ldx, float* rcond, float* ferr, float* berr, float complex* work, float* rwork, blasint* info)
#endif
{
	void (*fn) (void* fact, void* uplo, void* n, void* nrhs, void* ap, void* afp, void* ipiv, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* rwork, void* info);
	void (*fn_hook) (void* fact, void* uplo, void* n, void* nrhs, void* ap, void* afp, void* ipiv, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* rwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.chpsvx.f77_blas_function; 
	fn_hook = __flexiblas_hooks->chpsvx.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) fact, (void*) uplo, (void*) n, (void*) nrhs, (void*) ap, (void*) afp, (void*) ipiv, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) rwork, (void*) info); 
		return;
	} else {
		hook_pos_chpsvx = 0;
		fn_hook((void*) fact, (void*) uplo, (void*) n, (void*) nrhs, (void*) ap, (void*) afp, (void*) ipiv, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) rwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void chpsvx_(char* fact, char* uplo, blasint* n, blasint* nrhs, float complex* ap, float complex* afp, blasint* ipiv, float complex* b, blasint* ldb, float complex* x, blasint* ldx, float* rcond, float* ferr, float* berr, float complex* work, float* rwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(chpsvx,CHPSVX)))));
#else
void chpsvx(char* fact, char* uplo, blasint* n, blasint* nrhs, float complex* ap, float complex* afp, blasint* ipiv, float complex* b, blasint* ldb, float complex* x, blasint* ldx, float* rcond, float* ferr, float* berr, float complex* work, float* rwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(chpsvx,CHPSVX)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_chpsvx_(void* fact, void* uplo, void* n, void* nrhs, void* ap, void* afp, void* ipiv, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* rwork, void* info)
{
	void (*fn) (void* fact, void* uplo, void* n, void* nrhs, void* ap, void* afp, void* ipiv, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* rwork, void* info);

	fn = current_backend->lapack.chpsvx.f77_blas_function; 

		fn((void*) fact, (void*) uplo, (void*) n, (void*) nrhs, (void*) ap, (void*) afp, (void*) ipiv, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) rwork, (void*) info); 

	return;
}

void flexiblas_real_chpsvx(void* fact, void* uplo, void* n, void* nrhs, void* ap, void* afp, void* ipiv, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* rwork, void* info)  __attribute__((alias("flexiblas_real_chpsvx_")));





/* Chainloader for Hooks */


void flexiblas_chain_chpsvx_(void* fact, void* uplo, void* n, void* nrhs, void* ap, void* afp, void* ipiv, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* rwork, void* info)
{
	void (*fn) (void* fact, void* uplo, void* n, void* nrhs, void* ap, void* afp, void* ipiv, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* rwork, void* info);
	void (*fn_hook) (void* fact, void* uplo, void* n, void* nrhs, void* ap, void* afp, void* ipiv, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* rwork, void* info);

	fn      = current_backend->lapack.chpsvx.f77_blas_function; 

    hook_pos_chpsvx ++;
    if( hook_pos_chpsvx < __flexiblas_hooks->chpsvx.nhook) {
        fn_hook = __flexiblas_hooks->chpsvx.f77_hook_function[hook_pos_chpsvx];
        fn_hook((void*) fact, (void*) uplo, (void*) n, (void*) nrhs, (void*) ap, (void*) afp, (void*) ipiv, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) rwork, (void*) info);
    } else {
        hook_pos_chpsvx = 0;
		fn((void*) fact, (void*) uplo, (void*) n, (void*) nrhs, (void*) ap, (void*) afp, (void*) ipiv, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) rwork, (void*) info); 
	}
	return;
}

void flexiblas_chain_chpsvx(void* fact, void* uplo, void* n, void* nrhs, void* ap, void* afp, void* ipiv, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* rwork, void* info)  __attribute__((alias("flexiblas_chain_chpsvx_")));




