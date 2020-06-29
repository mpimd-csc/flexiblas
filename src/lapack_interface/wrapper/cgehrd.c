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



static TLS_STORE uint8_t hook_pos_cgehrd = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(cgehrd,CGEHRD)(blasint* n, blasint* ilo, blasint* ihi, float complex* a, blasint* lda, float complex* tau, float complex* work, blasint* lwork, blasint* info)
#else
void FC_GLOBAL(cgehrd,CGEHRD)(blasint* n, blasint* ilo, blasint* ihi, float complex* a, blasint* lda, float complex* tau, float complex* work, blasint* lwork, blasint* info)
#endif
{
	void (*fn) (void* n, void* ilo, void* ihi, void* a, void* lda, void* tau, void* work, void* lwork, void* info);
	void (*fn_hook) (void* n, void* ilo, void* ihi, void* a, void* lda, void* tau, void* work, void* lwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.cgehrd.f77_blas_function; 
	fn_hook = __flexiblas_hooks->cgehrd.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) tau, (void*) work, (void*) lwork, (void*) info); 
		return;
	} else {
		hook_pos_cgehrd = 0;
		fn_hook((void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) tau, (void*) work, (void*) lwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void cgehrd_(blasint* n, blasint* ilo, blasint* ihi, float complex* a, blasint* lda, float complex* tau, float complex* work, blasint* lwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(cgehrd,CGEHRD)))));
#else
void cgehrd(blasint* n, blasint* ilo, blasint* ihi, float complex* a, blasint* lda, float complex* tau, float complex* work, blasint* lwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(cgehrd,CGEHRD)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_cgehrd_(void* n, void* ilo, void* ihi, void* a, void* lda, void* tau, void* work, void* lwork, void* info)
{
	void (*fn) (void* n, void* ilo, void* ihi, void* a, void* lda, void* tau, void* work, void* lwork, void* info);

	fn = current_backend->lapack.cgehrd.f77_blas_function; 

		fn((void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) tau, (void*) work, (void*) lwork, (void*) info); 

	return;
}

void flexiblas_real_cgehrd(void* n, void* ilo, void* ihi, void* a, void* lda, void* tau, void* work, void* lwork, void* info)  __attribute__((alias("flexiblas_real_cgehrd_")));





/* Chainloader for Hooks */


void flexiblas_chain_cgehrd_(void* n, void* ilo, void* ihi, void* a, void* lda, void* tau, void* work, void* lwork, void* info)
{
	void (*fn) (void* n, void* ilo, void* ihi, void* a, void* lda, void* tau, void* work, void* lwork, void* info);
	void (*fn_hook) (void* n, void* ilo, void* ihi, void* a, void* lda, void* tau, void* work, void* lwork, void* info);

	fn      = current_backend->lapack.cgehrd.f77_blas_function; 

    hook_pos_cgehrd ++;
    if( hook_pos_cgehrd < __flexiblas_hooks->cgehrd.nhook) {
        fn_hook = __flexiblas_hooks->cgehrd.f77_hook_function[hook_pos_cgehrd];
        fn_hook((void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) tau, (void*) work, (void*) lwork, (void*) info);
    } else {
        hook_pos_cgehrd = 0;
		fn((void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) tau, (void*) work, (void*) lwork, (void*) info); 
	}
	return;
}

void flexiblas_chain_cgehrd(void* n, void* ilo, void* ihi, void* a, void* lda, void* tau, void* work, void* lwork, void* info)  __attribute__((alias("flexiblas_chain_cgehrd_")));




