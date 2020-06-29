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



static TLS_STORE uint8_t hook_pos_zgesc2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zgesc2,ZGESC2)(blasint* n, double complex* a, blasint* lda, double complex* rhs, blasint* ipiv, blasint* jpiv, double* scale)
#else
void FC_GLOBAL(zgesc2,ZGESC2)(blasint* n, double complex* a, blasint* lda, double complex* rhs, blasint* ipiv, blasint* jpiv, double* scale)
#endif
{
	void (*fn) (void* n, void* a, void* lda, void* rhs, void* ipiv, void* jpiv, void* scale);
	void (*fn_hook) (void* n, void* a, void* lda, void* rhs, void* ipiv, void* jpiv, void* scale);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.zgesc2.f77_blas_function; 
	fn_hook = __flexiblas_hooks->zgesc2.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) a, (void*) lda, (void*) rhs, (void*) ipiv, (void*) jpiv, (void*) scale); 
		return;
	} else {
		hook_pos_zgesc2 = 0;
		fn_hook((void*) n, (void*) a, (void*) lda, (void*) rhs, (void*) ipiv, (void*) jpiv, (void*) scale);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void zgesc2_(blasint* n, double complex* a, blasint* lda, double complex* rhs, blasint* ipiv, blasint* jpiv, double* scale) __attribute__((alias(MTS(FC_GLOBAL(zgesc2,ZGESC2)))));
#else
void zgesc2(blasint* n, double complex* a, blasint* lda, double complex* rhs, blasint* ipiv, blasint* jpiv, double* scale) __attribute__((alias(MTS(FC_GLOBAL(zgesc2,ZGESC2)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zgesc2_(void* n, void* a, void* lda, void* rhs, void* ipiv, void* jpiv, void* scale)
{
	void (*fn) (void* n, void* a, void* lda, void* rhs, void* ipiv, void* jpiv, void* scale);

	fn = current_backend->lapack.zgesc2.f77_blas_function; 

		fn((void*) n, (void*) a, (void*) lda, (void*) rhs, (void*) ipiv, (void*) jpiv, (void*) scale); 

	return;
}

void flexiblas_real_zgesc2(void* n, void* a, void* lda, void* rhs, void* ipiv, void* jpiv, void* scale)  __attribute__((alias("flexiblas_real_zgesc2_")));





/* Chainloader for Hooks */


void flexiblas_chain_zgesc2_(void* n, void* a, void* lda, void* rhs, void* ipiv, void* jpiv, void* scale)
{
	void (*fn) (void* n, void* a, void* lda, void* rhs, void* ipiv, void* jpiv, void* scale);
	void (*fn_hook) (void* n, void* a, void* lda, void* rhs, void* ipiv, void* jpiv, void* scale);

	fn      = current_backend->lapack.zgesc2.f77_blas_function; 

    hook_pos_zgesc2 ++;
    if( hook_pos_zgesc2 < __flexiblas_hooks->zgesc2.nhook) {
        fn_hook = __flexiblas_hooks->zgesc2.f77_hook_function[hook_pos_zgesc2];
        fn_hook((void*) n, (void*) a, (void*) lda, (void*) rhs, (void*) ipiv, (void*) jpiv, (void*) scale);
    } else {
        hook_pos_zgesc2 = 0;
		fn((void*) n, (void*) a, (void*) lda, (void*) rhs, (void*) ipiv, (void*) jpiv, (void*) scale); 
	}
	return;
}

void flexiblas_chain_zgesc2(void* n, void* a, void* lda, void* rhs, void* ipiv, void* jpiv, void* scale)  __attribute__((alias("flexiblas_chain_zgesc2_")));




