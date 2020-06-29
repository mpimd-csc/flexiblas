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



static TLS_STORE uint8_t hook_pos_slaqp2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slaqp2,SLAQP2)(blasint* m, blasint* n, blasint* offset, float* a, blasint* lda, blasint* jpvt, float* tau, float* vn1, float* vn2, float* work)
#else
void FC_GLOBAL(slaqp2,SLAQP2)(blasint* m, blasint* n, blasint* offset, float* a, blasint* lda, blasint* jpvt, float* tau, float* vn1, float* vn2, float* work)
#endif
{
	void (*fn) (void* m, void* n, void* offset, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* work);
	void (*fn_hook) (void* m, void* n, void* offset, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* work);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.slaqp2.f77_blas_function; 
	fn_hook = __flexiblas_hooks->slaqp2.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) m, (void*) n, (void*) offset, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) vn1, (void*) vn2, (void*) work); 
		return;
	} else {
		hook_pos_slaqp2 = 0;
		fn_hook((void*) m, (void*) n, (void*) offset, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) vn1, (void*) vn2, (void*) work);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void slaqp2_(blasint* m, blasint* n, blasint* offset, float* a, blasint* lda, blasint* jpvt, float* tau, float* vn1, float* vn2, float* work) __attribute__((alias(MTS(FC_GLOBAL(slaqp2,SLAQP2)))));
#else
void slaqp2(blasint* m, blasint* n, blasint* offset, float* a, blasint* lda, blasint* jpvt, float* tau, float* vn1, float* vn2, float* work) __attribute__((alias(MTS(FC_GLOBAL(slaqp2,SLAQP2)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slaqp2_(void* m, void* n, void* offset, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* work)
{
	void (*fn) (void* m, void* n, void* offset, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* work);

	fn = current_backend->lapack.slaqp2.f77_blas_function; 

		fn((void*) m, (void*) n, (void*) offset, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) vn1, (void*) vn2, (void*) work); 

	return;
}

void flexiblas_real_slaqp2(void* m, void* n, void* offset, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* work)  __attribute__((alias("flexiblas_real_slaqp2_")));





/* Chainloader for Hooks */


void flexiblas_chain_slaqp2_(void* m, void* n, void* offset, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* work)
{
	void (*fn) (void* m, void* n, void* offset, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* work);
	void (*fn_hook) (void* m, void* n, void* offset, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* work);

	fn      = current_backend->lapack.slaqp2.f77_blas_function; 

    hook_pos_slaqp2 ++;
    if( hook_pos_slaqp2 < __flexiblas_hooks->slaqp2.nhook) {
        fn_hook = __flexiblas_hooks->slaqp2.f77_hook_function[hook_pos_slaqp2];
        fn_hook((void*) m, (void*) n, (void*) offset, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) vn1, (void*) vn2, (void*) work);
    } else {
        hook_pos_slaqp2 = 0;
		fn((void*) m, (void*) n, (void*) offset, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) vn1, (void*) vn2, (void*) work); 
	}
	return;
}

void flexiblas_chain_slaqp2(void* m, void* n, void* offset, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* work)  __attribute__((alias("flexiblas_chain_slaqp2_")));




