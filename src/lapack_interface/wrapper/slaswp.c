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



static TLS_STORE uint8_t hook_pos_slaswp = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slaswp,SLASWP)(blasint* n, float* a, blasint* lda, blasint* k1, blasint* k2, blasint* ipiv, blasint* incx)
#else
void FC_GLOBAL(slaswp,SLASWP)(blasint* n, float* a, blasint* lda, blasint* k1, blasint* k2, blasint* ipiv, blasint* incx)
#endif
{
	void (*fn) (void* n, void* a, void* lda, void* k1, void* k2, void* ipiv, void* incx);
	void (*fn_hook) (void* n, void* a, void* lda, void* k1, void* k2, void* ipiv, void* incx);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.slaswp.f77_blas_function; 
	fn_hook = __flexiblas_hooks->slaswp.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) a, (void*) lda, (void*) k1, (void*) k2, (void*) ipiv, (void*) incx); 
		return;
	} else {
		hook_pos_slaswp = 0;
		fn_hook((void*) n, (void*) a, (void*) lda, (void*) k1, (void*) k2, (void*) ipiv, (void*) incx);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void slaswp_(blasint* n, float* a, blasint* lda, blasint* k1, blasint* k2, blasint* ipiv, blasint* incx) __attribute__((alias(MTS(FC_GLOBAL(slaswp,SLASWP)))));
#else
void slaswp(blasint* n, float* a, blasint* lda, blasint* k1, blasint* k2, blasint* ipiv, blasint* incx) __attribute__((alias(MTS(FC_GLOBAL(slaswp,SLASWP)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slaswp_(void* n, void* a, void* lda, void* k1, void* k2, void* ipiv, void* incx)
{
	void (*fn) (void* n, void* a, void* lda, void* k1, void* k2, void* ipiv, void* incx);

	fn = current_backend->lapack.slaswp.f77_blas_function; 

		fn((void*) n, (void*) a, (void*) lda, (void*) k1, (void*) k2, (void*) ipiv, (void*) incx); 

	return;
}

void flexiblas_real_slaswp(void* n, void* a, void* lda, void* k1, void* k2, void* ipiv, void* incx)  __attribute__((alias("flexiblas_real_slaswp_")));





/* Chainloader for Hooks */


void flexiblas_chain_slaswp_(void* n, void* a, void* lda, void* k1, void* k2, void* ipiv, void* incx)
{
	void (*fn) (void* n, void* a, void* lda, void* k1, void* k2, void* ipiv, void* incx);
	void (*fn_hook) (void* n, void* a, void* lda, void* k1, void* k2, void* ipiv, void* incx);

	fn      = current_backend->lapack.slaswp.f77_blas_function; 

    hook_pos_slaswp ++;
    if( hook_pos_slaswp < __flexiblas_hooks->slaswp.nhook) {
        fn_hook = __flexiblas_hooks->slaswp.f77_hook_function[hook_pos_slaswp];
        fn_hook((void*) n, (void*) a, (void*) lda, (void*) k1, (void*) k2, (void*) ipiv, (void*) incx);
    } else {
        hook_pos_slaswp = 0;
		fn((void*) n, (void*) a, (void*) lda, (void*) k1, (void*) k2, (void*) ipiv, (void*) incx); 
	}
	return;
}

void flexiblas_chain_slaswp(void* n, void* a, void* lda, void* k1, void* k2, void* ipiv, void* incx)  __attribute__((alias("flexiblas_chain_slaswp_")));




