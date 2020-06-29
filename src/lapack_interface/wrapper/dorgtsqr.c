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
 /* Generated: Mon Jun  8 14:12:34 2020 */
        
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



static TLS_STORE uint8_t hook_pos_dorgtsqr = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dorgtsqr,DORGTSQR)(blasint* m, blasint* n, blasint* mb, blasint* nb, double* a, blasint* lda, double* t, blasint* ldt, double* work, blasint* lwork, blasint* info)
#else
void FC_GLOBAL(dorgtsqr,DORGTSQR)(blasint* m, blasint* n, blasint* mb, blasint* nb, double* a, blasint* lda, double* t, blasint* ldt, double* work, blasint* lwork, blasint* info)
#endif
{
	void (*fn) (void* m, void* n, void* mb, void* nb, void* a, void* lda, void* t, void* ldt, void* work, void* lwork, void* info);
	void (*fn_hook) (void* m, void* n, void* mb, void* nb, void* a, void* lda, void* t, void* ldt, void* work, void* lwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.dorgtsqr.f77_blas_function; 
	fn_hook = __flexiblas_hooks->dorgtsqr.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) m, (void*) n, (void*) mb, (void*) nb, (void*) a, (void*) lda, (void*) t, (void*) ldt, (void*) work, (void*) lwork, (void*) info); 
		return;
	} else {
		hook_pos_dorgtsqr = 0;
		fn_hook((void*) m, (void*) n, (void*) mb, (void*) nb, (void*) a, (void*) lda, (void*) t, (void*) ldt, (void*) work, (void*) lwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dorgtsqr_(blasint* m, blasint* n, blasint* mb, blasint* nb, double* a, blasint* lda, double* t, blasint* ldt, double* work, blasint* lwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dorgtsqr,DORGTSQR)))));
#else
void dorgtsqr(blasint* m, blasint* n, blasint* mb, blasint* nb, double* a, blasint* lda, double* t, blasint* ldt, double* work, blasint* lwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dorgtsqr,DORGTSQR)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dorgtsqr_(void* m, void* n, void* mb, void* nb, void* a, void* lda, void* t, void* ldt, void* work, void* lwork, void* info)
{
	void (*fn) (void* m, void* n, void* mb, void* nb, void* a, void* lda, void* t, void* ldt, void* work, void* lwork, void* info);

	fn = current_backend->lapack.dorgtsqr.f77_blas_function; 

		fn((void*) m, (void*) n, (void*) mb, (void*) nb, (void*) a, (void*) lda, (void*) t, (void*) ldt, (void*) work, (void*) lwork, (void*) info); 

	return;
}

void flexiblas_real_dorgtsqr(void* m, void* n, void* mb, void* nb, void* a, void* lda, void* t, void* ldt, void* work, void* lwork, void* info)  __attribute__((alias("flexiblas_real_dorgtsqr_")));





/* Chainloader for Hooks */


void flexiblas_chain_dorgtsqr_(void* m, void* n, void* mb, void* nb, void* a, void* lda, void* t, void* ldt, void* work, void* lwork, void* info)
{
	void (*fn) (void* m, void* n, void* mb, void* nb, void* a, void* lda, void* t, void* ldt, void* work, void* lwork, void* info);
	void (*fn_hook) (void* m, void* n, void* mb, void* nb, void* a, void* lda, void* t, void* ldt, void* work, void* lwork, void* info);

	fn      = current_backend->lapack.dorgtsqr.f77_blas_function; 

    hook_pos_dorgtsqr ++;
    if( hook_pos_dorgtsqr < __flexiblas_hooks->dorgtsqr.nhook) {
        fn_hook = __flexiblas_hooks->dorgtsqr.f77_hook_function[hook_pos_dorgtsqr];
        fn_hook((void*) m, (void*) n, (void*) mb, (void*) nb, (void*) a, (void*) lda, (void*) t, (void*) ldt, (void*) work, (void*) lwork, (void*) info);
    } else {
        hook_pos_dorgtsqr = 0;
		fn((void*) m, (void*) n, (void*) mb, (void*) nb, (void*) a, (void*) lda, (void*) t, (void*) ldt, (void*) work, (void*) lwork, (void*) info); 
	}
	return;
}

void flexiblas_chain_dorgtsqr(void* m, void* n, void* mb, void* nb, void* a, void* lda, void* t, void* ldt, void* work, void* lwork, void* info)  __attribute__((alias("flexiblas_chain_dorgtsqr_")));




