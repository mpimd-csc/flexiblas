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



static TLS_STORE uint8_t hook_pos_cgees = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(cgees,CGEES)(char* jobvs, char* sort, blasint* selectfunc, blasint* n, float complex* a, blasint* lda, blasint* sdim, float complex* w, float complex* vs, blasint* ldvs, float complex* work, blasint* lwork, float* rwork, blasint* bwork, blasint* info)
#else
void FC_GLOBAL(cgees,CGEES)(char* jobvs, char* sort, blasint* selectfunc, blasint* n, float complex* a, blasint* lda, blasint* sdim, float complex* w, float complex* vs, blasint* ldvs, float complex* work, blasint* lwork, float* rwork, blasint* bwork, blasint* info)
#endif
{
	void (*fn) (void* jobvs, void* sort, void* selectfunc, void* n, void* a, void* lda, void* sdim, void* w, void* vs, void* ldvs, void* work, void* lwork, void* rwork, void* bwork, void* info);
	void (*fn_hook) (void* jobvs, void* sort, void* selectfunc, void* n, void* a, void* lda, void* sdim, void* w, void* vs, void* ldvs, void* work, void* lwork, void* rwork, void* bwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.cgees.f77_blas_function; 
	fn_hook = __flexiblas_hooks->cgees.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) jobvs, (void*) sort, (void*) selectfunc, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) w, (void*) vs, (void*) ldvs, (void*) work, (void*) lwork, (void*) rwork, (void*) bwork, (void*) info); 
		return;
	} else {
		hook_pos_cgees = 0;
		fn_hook((void*) jobvs, (void*) sort, (void*) selectfunc, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) w, (void*) vs, (void*) ldvs, (void*) work, (void*) lwork, (void*) rwork, (void*) bwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void cgees_(char* jobvs, char* sort, blasint* selectfunc, blasint* n, float complex* a, blasint* lda, blasint* sdim, float complex* w, float complex* vs, blasint* ldvs, float complex* work, blasint* lwork, float* rwork, blasint* bwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(cgees,CGEES)))));
#else
void cgees(char* jobvs, char* sort, blasint* selectfunc, blasint* n, float complex* a, blasint* lda, blasint* sdim, float complex* w, float complex* vs, blasint* ldvs, float complex* work, blasint* lwork, float* rwork, blasint* bwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(cgees,CGEES)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_cgees_(void* jobvs, void* sort, void* selectfunc, void* n, void* a, void* lda, void* sdim, void* w, void* vs, void* ldvs, void* work, void* lwork, void* rwork, void* bwork, void* info)
{
	void (*fn) (void* jobvs, void* sort, void* selectfunc, void* n, void* a, void* lda, void* sdim, void* w, void* vs, void* ldvs, void* work, void* lwork, void* rwork, void* bwork, void* info);

	fn = current_backend->lapack.cgees.f77_blas_function; 

		fn((void*) jobvs, (void*) sort, (void*) selectfunc, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) w, (void*) vs, (void*) ldvs, (void*) work, (void*) lwork, (void*) rwork, (void*) bwork, (void*) info); 

	return;
}

void flexiblas_real_cgees(void* jobvs, void* sort, void* selectfunc, void* n, void* a, void* lda, void* sdim, void* w, void* vs, void* ldvs, void* work, void* lwork, void* rwork, void* bwork, void* info)  __attribute__((alias("flexiblas_real_cgees_")));





/* Chainloader for Hooks */


void flexiblas_chain_cgees_(void* jobvs, void* sort, void* selectfunc, void* n, void* a, void* lda, void* sdim, void* w, void* vs, void* ldvs, void* work, void* lwork, void* rwork, void* bwork, void* info)
{
	void (*fn) (void* jobvs, void* sort, void* selectfunc, void* n, void* a, void* lda, void* sdim, void* w, void* vs, void* ldvs, void* work, void* lwork, void* rwork, void* bwork, void* info);
	void (*fn_hook) (void* jobvs, void* sort, void* selectfunc, void* n, void* a, void* lda, void* sdim, void* w, void* vs, void* ldvs, void* work, void* lwork, void* rwork, void* bwork, void* info);

	fn      = current_backend->lapack.cgees.f77_blas_function; 

    hook_pos_cgees ++;
    if( hook_pos_cgees < __flexiblas_hooks->cgees.nhook) {
        fn_hook = __flexiblas_hooks->cgees.f77_hook_function[hook_pos_cgees];
        fn_hook((void*) jobvs, (void*) sort, (void*) selectfunc, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) w, (void*) vs, (void*) ldvs, (void*) work, (void*) lwork, (void*) rwork, (void*) bwork, (void*) info);
    } else {
        hook_pos_cgees = 0;
		fn((void*) jobvs, (void*) sort, (void*) selectfunc, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) w, (void*) vs, (void*) ldvs, (void*) work, (void*) lwork, (void*) rwork, (void*) bwork, (void*) info); 
	}
	return;
}

void flexiblas_chain_cgees(void* jobvs, void* sort, void* selectfunc, void* n, void* a, void* lda, void* sdim, void* w, void* vs, void* ldvs, void* work, void* lwork, void* rwork, void* bwork, void* info)  __attribute__((alias("flexiblas_chain_cgees_")));




