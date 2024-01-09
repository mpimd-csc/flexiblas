//    SPDX-License-Identifier: LGPL-3.0-or-later
/*
    This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
    Copyright (C) 2013-2024 Martin Koehler

    This program is free software: you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by the Free
    Software Foundation, either version 3 of the License, or (at your option)
    any later version.

    This program is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
    more details.

    You should have received a copy of the GNU General Public License along
    with this program. If not, see <https://www.gnu.org/licenses/>.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <complex.h>

#include "flexiblas_fortran_mangle.h"

#include "flexiblas.h"


#if __GNUC__ > 7
typedef size_t fortran_charlen_t;
#else
typedef int fortran_charlen_t;
#endif

#ifdef INTEGER8
#define blasint int64_t
#else
#define blasint int
#endif



static TLS_STORE uint8_t hook_pos_cgsvj1 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(cgsvj1,CGSVJ1)(char* jobv, blasint* m, blasint* n, blasint* n1, float complex* a, blasint* lda, float complex* d, float* sva, blasint* mv, float complex* v, blasint* ldv, float* eps, float* sfmin, float* tol, blasint* nsweep, float complex* work, blasint* lwork, blasint* info, fortran_charlen_t len_jobv)
#else
void FC_GLOBAL(cgsvj1,CGSVJ1)(char* jobv, blasint* m, blasint* n, blasint* n1, float complex* a, blasint* lda, float complex* d, float* sva, blasint* mv, float complex* v, blasint* ldv, float* eps, float* sfmin, float* tol, blasint* nsweep, float complex* work, blasint* lwork, blasint* info, fortran_charlen_t len_jobv)
#endif
{
	void (*fn) (void* jobv, void* m, void* n, void* n1, void* a, void* lda, void* d, void* sva, void* mv, void* v, void* ldv, void* eps, void* sfmin, void* tol, void* nsweep, void* work, void* lwork, void* info, fortran_charlen_t len_jobv);
	void (*fn_hook) (void* jobv, void* m, void* n, void* n1, void* a, void* lda, void* d, void* sva, void* mv, void* v, void* ldv, void* eps, void* sfmin, void* tol, void* nsweep, void* work, void* lwork, void* info, fortran_charlen_t len_jobv);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.cgsvj1.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->cgsvj1.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) jobv, (void*) m, (void*) n, (void*) n1, (void*) a, (void*) lda, (void*) d, (void*) sva, (void*) mv, (void*) v, (void*) ldv, (void*) eps, (void*) sfmin, (void*) tol, (void*) nsweep, (void*) work, (void*) lwork, (void*) info, ( fortran_charlen_t ) len_jobv); 
		return;
	} else {
		hook_pos_cgsvj1 = 0;
		fn_hook((void*) jobv, (void*) m, (void*) n, (void*) n1, (void*) a, (void*) lda, (void*) d, (void*) sva, (void*) mv, (void*) v, (void*) ldv, (void*) eps, (void*) sfmin, (void*) tol, (void*) nsweep, (void*) work, (void*) lwork, (void*) info, ( fortran_charlen_t ) len_jobv);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void cgsvj1_(char* jobv, blasint* m, blasint* n, blasint* n1, float complex* a, blasint* lda, float complex* d, float* sva, blasint* mv, float complex* v, blasint* ldv, float* eps, float* sfmin, float* tol, blasint* nsweep, float complex* work, blasint* lwork, blasint* info, fortran_charlen_t len_jobv) __attribute__((alias(MTS(FC_GLOBAL(cgsvj1,CGSVJ1)))));
#else
#ifndef __APPLE__
void cgsvj1(char* jobv, blasint* m, blasint* n, blasint* n1, float complex* a, blasint* lda, float complex* d, float* sva, blasint* mv, float complex* v, blasint* ldv, float* eps, float* sfmin, float* tol, blasint* nsweep, float complex* work, blasint* lwork, blasint* info, fortran_charlen_t len_jobv) __attribute__((alias(MTS(FC_GLOBAL(cgsvj1,CGSVJ1)))));
#else
void cgsvj1(char* jobv, blasint* m, blasint* n, blasint* n1, float complex* a, blasint* lda, float complex* d, float* sva, blasint* mv, float complex* v, blasint* ldv, float* eps, float* sfmin, float* tol, blasint* nsweep, float complex* work, blasint* lwork, blasint* info, fortran_charlen_t len_jobv){ FC_GLOBAL(cgsvj1,CGSVJ1)((void*) jobv, (void*) m, (void*) n, (void*) n1, (void*) a, (void*) lda, (void*) d, (void*) sva, (void*) mv, (void*) v, (void*) ldv, (void*) eps, (void*) sfmin, (void*) tol, (void*) nsweep, (void*) work, (void*) lwork, (void*) info, (fortran_charlen_t) len_jobv); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_cgsvj1_(void* jobv, void* m, void* n, void* n1, void* a, void* lda, void* d, void* sva, void* mv, void* v, void* ldv, void* eps, void* sfmin, void* tol, void* nsweep, void* work, void* lwork, void* info, fortran_charlen_t len_jobv)
{
	void (*fn) (void* jobv, void* m, void* n, void* n1, void* a, void* lda, void* d, void* sva, void* mv, void* v, void* ldv, void* eps, void* sfmin, void* tol, void* nsweep, void* work, void* lwork, void* info, fortran_charlen_t len_jobv);

	*(void **) & fn = current_backend->lapack.cgsvj1.f77_blas_function; 

		fn((void*) jobv, (void*) m, (void*) n, (void*) n1, (void*) a, (void*) lda, (void*) d, (void*) sva, (void*) mv, (void*) v, (void*) ldv, (void*) eps, (void*) sfmin, (void*) tol, (void*) nsweep, (void*) work, (void*) lwork, (void*) info, ( fortran_charlen_t ) len_jobv); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_cgsvj1(void* jobv, void* m, void* n, void* n1, void* a, void* lda, void* d, void* sva, void* mv, void* v, void* ldv, void* eps, void* sfmin, void* tol, void* nsweep, void* work, void* lwork, void* info, fortran_charlen_t len_jobv) __attribute__((alias("flexiblas_real_cgsvj1_")));
#else
void flexiblas_real_cgsvj1(void* jobv, void* m, void* n, void* n1, void* a, void* lda, void* d, void* sva, void* mv, void* v, void* ldv, void* eps, void* sfmin, void* tol, void* nsweep, void* work, void* lwork, void* info, fortran_charlen_t len_jobv){flexiblas_real_cgsvj1_((void*) jobv, (void*) m, (void*) n, (void*) n1, (void*) a, (void*) lda, (void*) d, (void*) sva, (void*) mv, (void*) v, (void*) ldv, (void*) eps, (void*) sfmin, (void*) tol, (void*) nsweep, (void*) work, (void*) lwork, (void*) info, (fortran_charlen_t) len_jobv);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_cgsvj1_(void* jobv, void* m, void* n, void* n1, void* a, void* lda, void* d, void* sva, void* mv, void* v, void* ldv, void* eps, void* sfmin, void* tol, void* nsweep, void* work, void* lwork, void* info, fortran_charlen_t len_jobv)
{
	void (*fn) (void* jobv, void* m, void* n, void* n1, void* a, void* lda, void* d, void* sva, void* mv, void* v, void* ldv, void* eps, void* sfmin, void* tol, void* nsweep, void* work, void* lwork, void* info, fortran_charlen_t len_jobv);
	void (*fn_hook) (void* jobv, void* m, void* n, void* n1, void* a, void* lda, void* d, void* sva, void* mv, void* v, void* ldv, void* eps, void* sfmin, void* tol, void* nsweep, void* work, void* lwork, void* info, fortran_charlen_t len_jobv);

	*(void **) &fn      = current_backend->lapack.cgsvj1.f77_blas_function; 

    hook_pos_cgsvj1 ++;
    if( hook_pos_cgsvj1 < __flexiblas_hooks->cgsvj1.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->cgsvj1.f77_hook_function[hook_pos_cgsvj1];
        fn_hook((void*) jobv, (void*) m, (void*) n, (void*) n1, (void*) a, (void*) lda, (void*) d, (void*) sva, (void*) mv, (void*) v, (void*) ldv, (void*) eps, (void*) sfmin, (void*) tol, (void*) nsweep, (void*) work, (void*) lwork, (void*) info, ( fortran_charlen_t ) len_jobv);
    } else {
        hook_pos_cgsvj1 = 0;
		fn((void*) jobv, (void*) m, (void*) n, (void*) n1, (void*) a, (void*) lda, (void*) d, (void*) sva, (void*) mv, (void*) v, (void*) ldv, (void*) eps, (void*) sfmin, (void*) tol, (void*) nsweep, (void*) work, (void*) lwork, (void*) info, ( fortran_charlen_t ) len_jobv); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_cgsvj1(void* jobv, void* m, void* n, void* n1, void* a, void* lda, void* d, void* sva, void* mv, void* v, void* ldv, void* eps, void* sfmin, void* tol, void* nsweep, void* work, void* lwork, void* info, fortran_charlen_t len_jobv) __attribute__((alias("flexiblas_chain_cgsvj1_")));
#else
void flexiblas_chain_cgsvj1(void* jobv, void* m, void* n, void* n1, void* a, void* lda, void* d, void* sva, void* mv, void* v, void* ldv, void* eps, void* sfmin, void* tol, void* nsweep, void* work, void* lwork, void* info, fortran_charlen_t len_jobv){flexiblas_chain_cgsvj1_((void*) jobv, (void*) m, (void*) n, (void*) n1, (void*) a, (void*) lda, (void*) d, (void*) sva, (void*) mv, (void*) v, (void*) ldv, (void*) eps, (void*) sfmin, (void*) tol, (void*) nsweep, (void*) work, (void*) lwork, (void*) info, (fortran_charlen_t) len_jobv);}
#endif



