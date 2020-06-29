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



static TLS_STORE uint8_t hook_pos_slahrd = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slahrd,SLAHRD)(blasint* n, blasint* k, blasint* nb, float* a, blasint* lda, float* tau, float* t, blasint* ldt, float* y, blasint* ldy)
#else
void FC_GLOBAL(slahrd,SLAHRD)(blasint* n, blasint* k, blasint* nb, float* a, blasint* lda, float* tau, float* t, blasint* ldt, float* y, blasint* ldy)
#endif
{
	void (*fn) (void* n, void* k, void* nb, void* a, void* lda, void* tau, void* t, void* ldt, void* y, void* ldy);
	void (*fn_hook) (void* n, void* k, void* nb, void* a, void* lda, void* tau, void* t, void* ldt, void* y, void* ldy);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.slahrd.f77_blas_function; 
	fn_hook = __flexiblas_hooks->slahrd.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) k, (void*) nb, (void*) a, (void*) lda, (void*) tau, (void*) t, (void*) ldt, (void*) y, (void*) ldy); 
		return;
	} else {
		hook_pos_slahrd = 0;
		fn_hook((void*) n, (void*) k, (void*) nb, (void*) a, (void*) lda, (void*) tau, (void*) t, (void*) ldt, (void*) y, (void*) ldy);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void slahrd_(blasint* n, blasint* k, blasint* nb, float* a, blasint* lda, float* tau, float* t, blasint* ldt, float* y, blasint* ldy) __attribute__((alias(MTS(FC_GLOBAL(slahrd,SLAHRD)))));
#else
void slahrd(blasint* n, blasint* k, blasint* nb, float* a, blasint* lda, float* tau, float* t, blasint* ldt, float* y, blasint* ldy) __attribute__((alias(MTS(FC_GLOBAL(slahrd,SLAHRD)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slahrd_(void* n, void* k, void* nb, void* a, void* lda, void* tau, void* t, void* ldt, void* y, void* ldy)
{
	void (*fn) (void* n, void* k, void* nb, void* a, void* lda, void* tau, void* t, void* ldt, void* y, void* ldy);

	fn = current_backend->lapack.slahrd.f77_blas_function; 

		fn((void*) n, (void*) k, (void*) nb, (void*) a, (void*) lda, (void*) tau, (void*) t, (void*) ldt, (void*) y, (void*) ldy); 

	return;
}

void flexiblas_real_slahrd(void* n, void* k, void* nb, void* a, void* lda, void* tau, void* t, void* ldt, void* y, void* ldy)  __attribute__((alias("flexiblas_real_slahrd_")));





/* Chainloader for Hooks */


void flexiblas_chain_slahrd_(void* n, void* k, void* nb, void* a, void* lda, void* tau, void* t, void* ldt, void* y, void* ldy)
{
	void (*fn) (void* n, void* k, void* nb, void* a, void* lda, void* tau, void* t, void* ldt, void* y, void* ldy);
	void (*fn_hook) (void* n, void* k, void* nb, void* a, void* lda, void* tau, void* t, void* ldt, void* y, void* ldy);

	fn      = current_backend->lapack.slahrd.f77_blas_function; 

    hook_pos_slahrd ++;
    if( hook_pos_slahrd < __flexiblas_hooks->slahrd.nhook) {
        fn_hook = __flexiblas_hooks->slahrd.f77_hook_function[hook_pos_slahrd];
        fn_hook((void*) n, (void*) k, (void*) nb, (void*) a, (void*) lda, (void*) tau, (void*) t, (void*) ldt, (void*) y, (void*) ldy);
    } else {
        hook_pos_slahrd = 0;
		fn((void*) n, (void*) k, (void*) nb, (void*) a, (void*) lda, (void*) tau, (void*) t, (void*) ldt, (void*) y, (void*) ldy); 
	}
	return;
}

void flexiblas_chain_slahrd(void* n, void* k, void* nb, void* a, void* lda, void* tau, void* t, void* ldt, void* y, void* ldy)  __attribute__((alias("flexiblas_chain_slahrd_")));




