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



static TLS_STORE uint8_t hook_pos_cgesv = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(cgesv,CGESV)(blasint* n, blasint* nrhs, float complex* a, blasint* lda, blasint* ipiv, float complex* b, blasint* ldb, blasint* info)
#else
void FC_GLOBAL(cgesv,CGESV)(blasint* n, blasint* nrhs, float complex* a, blasint* lda, blasint* ipiv, float complex* b, blasint* ldb, blasint* info)
#endif
{
	void (*fn) (void* n, void* nrhs, void* a, void* lda, void* ipiv, void* b, void* ldb, void* info);
	void (*fn_hook) (void* n, void* nrhs, void* a, void* lda, void* ipiv, void* b, void* ldb, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.cgesv.f77_blas_function; 
	fn_hook = __flexiblas_hooks->cgesv.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) ipiv, (void*) b, (void*) ldb, (void*) info); 
		return;
	} else {
		hook_pos_cgesv = 0;
		fn_hook((void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) ipiv, (void*) b, (void*) ldb, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void cgesv_(blasint* n, blasint* nrhs, float complex* a, blasint* lda, blasint* ipiv, float complex* b, blasint* ldb, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(cgesv,CGESV)))));
#else
void cgesv(blasint* n, blasint* nrhs, float complex* a, blasint* lda, blasint* ipiv, float complex* b, blasint* ldb, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(cgesv,CGESV)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_cgesv_(void* n, void* nrhs, void* a, void* lda, void* ipiv, void* b, void* ldb, void* info)
{
	void (*fn) (void* n, void* nrhs, void* a, void* lda, void* ipiv, void* b, void* ldb, void* info);

	fn = current_backend->lapack.cgesv.f77_blas_function; 

		fn((void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) ipiv, (void*) b, (void*) ldb, (void*) info); 

	return;
}

void flexiblas_real_cgesv(void* n, void* nrhs, void* a, void* lda, void* ipiv, void* b, void* ldb, void* info)  __attribute__((alias("flexiblas_real_cgesv_")));





/* Chainloader for Hooks */


void flexiblas_chain_cgesv_(void* n, void* nrhs, void* a, void* lda, void* ipiv, void* b, void* ldb, void* info)
{
	void (*fn) (void* n, void* nrhs, void* a, void* lda, void* ipiv, void* b, void* ldb, void* info);
	void (*fn_hook) (void* n, void* nrhs, void* a, void* lda, void* ipiv, void* b, void* ldb, void* info);

	fn      = current_backend->lapack.cgesv.f77_blas_function; 

    hook_pos_cgesv ++;
    if( hook_pos_cgesv < __flexiblas_hooks->cgesv.nhook) {
        fn_hook = __flexiblas_hooks->cgesv.f77_hook_function[hook_pos_cgesv];
        fn_hook((void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) ipiv, (void*) b, (void*) ldb, (void*) info);
    } else {
        hook_pos_cgesv = 0;
		fn((void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) ipiv, (void*) b, (void*) ldb, (void*) info); 
	}
	return;
}

void flexiblas_chain_cgesv(void* n, void* nrhs, void* a, void* lda, void* ipiv, void* b, void* ldb, void* info)  __attribute__((alias("flexiblas_chain_cgesv_")));




