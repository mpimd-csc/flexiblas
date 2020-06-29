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



static TLS_STORE uint8_t hook_pos_zlahef_aa = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL_(zlahef_aa,ZLAHEF_AA)(char* uplo, blasint* j1, blasint* m, blasint* nb, double complex* a, blasint* lda, blasint* ipiv, double complex* h, blasint* ldh, double complex* work)
#else
void FC_GLOBAL_(zlahef_aa,ZLAHEF_AA)(char* uplo, blasint* j1, blasint* m, blasint* nb, double complex* a, blasint* lda, blasint* ipiv, double complex* h, blasint* ldh, double complex* work)
#endif
{
	void (*fn) (void* uplo, void* j1, void* m, void* nb, void* a, void* lda, void* ipiv, void* h, void* ldh, void* work);
	void (*fn_hook) (void* uplo, void* j1, void* m, void* nb, void* a, void* lda, void* ipiv, void* h, void* ldh, void* work);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.zlahef_aa.f77_blas_function; 
	fn_hook = __flexiblas_hooks->zlahef_aa.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) uplo, (void*) j1, (void*) m, (void*) nb, (void*) a, (void*) lda, (void*) ipiv, (void*) h, (void*) ldh, (void*) work); 
		return;
	} else {
		hook_pos_zlahef_aa = 0;
		fn_hook((void*) uplo, (void*) j1, (void*) m, (void*) nb, (void*) a, (void*) lda, (void*) ipiv, (void*) h, (void*) ldh, (void*) work);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void zlahef_aa_(char* uplo, blasint* j1, blasint* m, blasint* nb, double complex* a, blasint* lda, blasint* ipiv, double complex* h, blasint* ldh, double complex* work) __attribute__((alias(MTS(FC_GLOBAL_(zlahef_aa,ZLAHEF_AA)))));
#else
void zlahef_aa(char* uplo, blasint* j1, blasint* m, blasint* nb, double complex* a, blasint* lda, blasint* ipiv, double complex* h, blasint* ldh, double complex* work) __attribute__((alias(MTS(FC_GLOBAL_(zlahef_aa,ZLAHEF_AA)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zlahef_aa_(void* uplo, void* j1, void* m, void* nb, void* a, void* lda, void* ipiv, void* h, void* ldh, void* work)
{
	void (*fn) (void* uplo, void* j1, void* m, void* nb, void* a, void* lda, void* ipiv, void* h, void* ldh, void* work);

	fn = current_backend->lapack.zlahef_aa.f77_blas_function; 

		fn((void*) uplo, (void*) j1, (void*) m, (void*) nb, (void*) a, (void*) lda, (void*) ipiv, (void*) h, (void*) ldh, (void*) work); 

	return;
}

void flexiblas_real_zlahef_aa(void* uplo, void* j1, void* m, void* nb, void* a, void* lda, void* ipiv, void* h, void* ldh, void* work)  __attribute__((alias("flexiblas_real_zlahef_aa_")));





/* Chainloader for Hooks */


void flexiblas_chain_zlahef_aa_(void* uplo, void* j1, void* m, void* nb, void* a, void* lda, void* ipiv, void* h, void* ldh, void* work)
{
	void (*fn) (void* uplo, void* j1, void* m, void* nb, void* a, void* lda, void* ipiv, void* h, void* ldh, void* work);
	void (*fn_hook) (void* uplo, void* j1, void* m, void* nb, void* a, void* lda, void* ipiv, void* h, void* ldh, void* work);

	fn      = current_backend->lapack.zlahef_aa.f77_blas_function; 

    hook_pos_zlahef_aa ++;
    if( hook_pos_zlahef_aa < __flexiblas_hooks->zlahef_aa.nhook) {
        fn_hook = __flexiblas_hooks->zlahef_aa.f77_hook_function[hook_pos_zlahef_aa];
        fn_hook((void*) uplo, (void*) j1, (void*) m, (void*) nb, (void*) a, (void*) lda, (void*) ipiv, (void*) h, (void*) ldh, (void*) work);
    } else {
        hook_pos_zlahef_aa = 0;
		fn((void*) uplo, (void*) j1, (void*) m, (void*) nb, (void*) a, (void*) lda, (void*) ipiv, (void*) h, (void*) ldh, (void*) work); 
	}
	return;
}

void flexiblas_chain_zlahef_aa(void* uplo, void* j1, void* m, void* nb, void* a, void* lda, void* ipiv, void* h, void* ldh, void* work)  __attribute__((alias("flexiblas_chain_zlahef_aa_")));




