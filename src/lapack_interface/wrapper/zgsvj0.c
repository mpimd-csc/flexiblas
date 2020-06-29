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



static TLS_STORE uint8_t hook_pos_zgsvj0 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zgsvj0,ZGSVJ0)(char* jobv, blasint* m, blasint* n, double complex* a, blasint* lda, double complex* d, double* sva, blasint* mv, double complex* v, blasint* ldv, double* eps, double* sfmin, double* tol, blasint* nsweep, double complex* work, blasint* lwork, blasint* info, blasint len_jobv)
#else
void FC_GLOBAL(zgsvj0,ZGSVJ0)(char* jobv, blasint* m, blasint* n, double complex* a, blasint* lda, double complex* d, double* sva, blasint* mv, double complex* v, blasint* ldv, double* eps, double* sfmin, double* tol, blasint* nsweep, double complex* work, blasint* lwork, blasint* info, blasint len_jobv)
#endif
{
	void (*fn) (void* jobv, void* m, void* n, void* a, void* lda, void* d, void* sva, void* mv, void* v, void* ldv, void* eps, void* sfmin, void* tol, void* nsweep, void* work, void* lwork, void* info, blasint len_jobv);
	void (*fn_hook) (void* jobv, void* m, void* n, void* a, void* lda, void* d, void* sva, void* mv, void* v, void* ldv, void* eps, void* sfmin, void* tol, void* nsweep, void* work, void* lwork, void* info, blasint len_jobv);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.zgsvj0.f77_blas_function; 
	fn_hook = __flexiblas_hooks->zgsvj0.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) d, (void*) sva, (void*) mv, (void*) v, (void*) ldv, (void*) eps, (void*) sfmin, (void*) tol, (void*) nsweep, (void*) work, (void*) lwork, (void*) info, (blasint) len_jobv); 
		return;
	} else {
		hook_pos_zgsvj0 = 0;
		fn_hook((void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) d, (void*) sva, (void*) mv, (void*) v, (void*) ldv, (void*) eps, (void*) sfmin, (void*) tol, (void*) nsweep, (void*) work, (void*) lwork, (void*) info, (blasint) len_jobv);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void zgsvj0_(char* jobv, blasint* m, blasint* n, double complex* a, blasint* lda, double complex* d, double* sva, blasint* mv, double complex* v, blasint* ldv, double* eps, double* sfmin, double* tol, blasint* nsweep, double complex* work, blasint* lwork, blasint* info, blasint len_jobv) __attribute__((alias(MTS(FC_GLOBAL(zgsvj0,ZGSVJ0)))));
#else
void zgsvj0(char* jobv, blasint* m, blasint* n, double complex* a, blasint* lda, double complex* d, double* sva, blasint* mv, double complex* v, blasint* ldv, double* eps, double* sfmin, double* tol, blasint* nsweep, double complex* work, blasint* lwork, blasint* info, blasint len_jobv) __attribute__((alias(MTS(FC_GLOBAL(zgsvj0,ZGSVJ0)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zgsvj0_(void* jobv, void* m, void* n, void* a, void* lda, void* d, void* sva, void* mv, void* v, void* ldv, void* eps, void* sfmin, void* tol, void* nsweep, void* work, void* lwork, void* info, blasint len_jobv)
{
	void (*fn) (void* jobv, void* m, void* n, void* a, void* lda, void* d, void* sva, void* mv, void* v, void* ldv, void* eps, void* sfmin, void* tol, void* nsweep, void* work, void* lwork, void* info, blasint len_jobv);

	fn = current_backend->lapack.zgsvj0.f77_blas_function; 

		fn((void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) d, (void*) sva, (void*) mv, (void*) v, (void*) ldv, (void*) eps, (void*) sfmin, (void*) tol, (void*) nsweep, (void*) work, (void*) lwork, (void*) info, (blasint) len_jobv); 

	return;
}

void flexiblas_real_zgsvj0(void* jobv, void* m, void* n, void* a, void* lda, void* d, void* sva, void* mv, void* v, void* ldv, void* eps, void* sfmin, void* tol, void* nsweep, void* work, void* lwork, void* info, blasint len_jobv)  __attribute__((alias("flexiblas_real_zgsvj0_")));





/* Chainloader for Hooks */


void flexiblas_chain_zgsvj0_(void* jobv, void* m, void* n, void* a, void* lda, void* d, void* sva, void* mv, void* v, void* ldv, void* eps, void* sfmin, void* tol, void* nsweep, void* work, void* lwork, void* info, blasint len_jobv)
{
	void (*fn) (void* jobv, void* m, void* n, void* a, void* lda, void* d, void* sva, void* mv, void* v, void* ldv, void* eps, void* sfmin, void* tol, void* nsweep, void* work, void* lwork, void* info, blasint len_jobv);
	void (*fn_hook) (void* jobv, void* m, void* n, void* a, void* lda, void* d, void* sva, void* mv, void* v, void* ldv, void* eps, void* sfmin, void* tol, void* nsweep, void* work, void* lwork, void* info, blasint len_jobv);

	fn      = current_backend->lapack.zgsvj0.f77_blas_function; 

    hook_pos_zgsvj0 ++;
    if( hook_pos_zgsvj0 < __flexiblas_hooks->zgsvj0.nhook) {
        fn_hook = __flexiblas_hooks->zgsvj0.f77_hook_function[hook_pos_zgsvj0];
        fn_hook((void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) d, (void*) sva, (void*) mv, (void*) v, (void*) ldv, (void*) eps, (void*) sfmin, (void*) tol, (void*) nsweep, (void*) work, (void*) lwork, (void*) info, (blasint) len_jobv);
    } else {
        hook_pos_zgsvj0 = 0;
		fn((void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) d, (void*) sva, (void*) mv, (void*) v, (void*) ldv, (void*) eps, (void*) sfmin, (void*) tol, (void*) nsweep, (void*) work, (void*) lwork, (void*) info, (blasint) len_jobv); 
	}
	return;
}

void flexiblas_chain_zgsvj0(void* jobv, void* m, void* n, void* a, void* lda, void* d, void* sva, void* mv, void* v, void* ldv, void* eps, void* sfmin, void* tol, void* nsweep, void* work, void* lwork, void* info, blasint len_jobv)  __attribute__((alias("flexiblas_chain_zgsvj0_")));




