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



static TLS_STORE uint8_t hook_pos_sgesvj = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(sgesvj,SGESVJ)(char* joba, char* jobu, char* jobv, blasint* m, blasint* n, float* a, blasint* lda, float* sva, blasint* mv, float* v, blasint* ldv, float* work, blasint* lwork, blasint* info, blasint len_joba, blasint len_jobu, blasint len_jobv)
#else
void FC_GLOBAL(sgesvj,SGESVJ)(char* joba, char* jobu, char* jobv, blasint* m, blasint* n, float* a, blasint* lda, float* sva, blasint* mv, float* v, blasint* ldv, float* work, blasint* lwork, blasint* info, blasint len_joba, blasint len_jobu, blasint len_jobv)
#endif
{
	void (*fn) (void* joba, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* sva, void* mv, void* v, void* ldv, void* work, void* lwork, void* info, blasint len_joba, blasint len_jobu, blasint len_jobv);
	void (*fn_hook) (void* joba, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* sva, void* mv, void* v, void* ldv, void* work, void* lwork, void* info, blasint len_joba, blasint len_jobu, blasint len_jobv);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.sgesvj.f77_blas_function; 
	fn_hook = __flexiblas_hooks->sgesvj.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) joba, (void*) jobu, (void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) sva, (void*) mv, (void*) v, (void*) ldv, (void*) work, (void*) lwork, (void*) info, (blasint) len_joba, (blasint) len_jobu, (blasint) len_jobv); 
		return;
	} else {
		hook_pos_sgesvj = 0;
		fn_hook((void*) joba, (void*) jobu, (void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) sva, (void*) mv, (void*) v, (void*) ldv, (void*) work, (void*) lwork, (void*) info, (blasint) len_joba, (blasint) len_jobu, (blasint) len_jobv);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void sgesvj_(char* joba, char* jobu, char* jobv, blasint* m, blasint* n, float* a, blasint* lda, float* sva, blasint* mv, float* v, blasint* ldv, float* work, blasint* lwork, blasint* info, blasint len_joba, blasint len_jobu, blasint len_jobv) __attribute__((alias(MTS(FC_GLOBAL(sgesvj,SGESVJ)))));
#else
void sgesvj(char* joba, char* jobu, char* jobv, blasint* m, blasint* n, float* a, blasint* lda, float* sva, blasint* mv, float* v, blasint* ldv, float* work, blasint* lwork, blasint* info, blasint len_joba, blasint len_jobu, blasint len_jobv) __attribute__((alias(MTS(FC_GLOBAL(sgesvj,SGESVJ)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_sgesvj_(void* joba, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* sva, void* mv, void* v, void* ldv, void* work, void* lwork, void* info, blasint len_joba, blasint len_jobu, blasint len_jobv)
{
	void (*fn) (void* joba, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* sva, void* mv, void* v, void* ldv, void* work, void* lwork, void* info, blasint len_joba, blasint len_jobu, blasint len_jobv);

	fn = current_backend->lapack.sgesvj.f77_blas_function; 

		fn((void*) joba, (void*) jobu, (void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) sva, (void*) mv, (void*) v, (void*) ldv, (void*) work, (void*) lwork, (void*) info, (blasint) len_joba, (blasint) len_jobu, (blasint) len_jobv); 

	return;
}

void flexiblas_real_sgesvj(void* joba, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* sva, void* mv, void* v, void* ldv, void* work, void* lwork, void* info, blasint len_joba, blasint len_jobu, blasint len_jobv)  __attribute__((alias("flexiblas_real_sgesvj_")));





/* Chainloader for Hooks */


void flexiblas_chain_sgesvj_(void* joba, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* sva, void* mv, void* v, void* ldv, void* work, void* lwork, void* info, blasint len_joba, blasint len_jobu, blasint len_jobv)
{
	void (*fn) (void* joba, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* sva, void* mv, void* v, void* ldv, void* work, void* lwork, void* info, blasint len_joba, blasint len_jobu, blasint len_jobv);
	void (*fn_hook) (void* joba, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* sva, void* mv, void* v, void* ldv, void* work, void* lwork, void* info, blasint len_joba, blasint len_jobu, blasint len_jobv);

	fn      = current_backend->lapack.sgesvj.f77_blas_function; 

    hook_pos_sgesvj ++;
    if( hook_pos_sgesvj < __flexiblas_hooks->sgesvj.nhook) {
        fn_hook = __flexiblas_hooks->sgesvj.f77_hook_function[hook_pos_sgesvj];
        fn_hook((void*) joba, (void*) jobu, (void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) sva, (void*) mv, (void*) v, (void*) ldv, (void*) work, (void*) lwork, (void*) info, (blasint) len_joba, (blasint) len_jobu, (blasint) len_jobv);
    } else {
        hook_pos_sgesvj = 0;
		fn((void*) joba, (void*) jobu, (void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) sva, (void*) mv, (void*) v, (void*) ldv, (void*) work, (void*) lwork, (void*) info, (blasint) len_joba, (blasint) len_jobu, (blasint) len_jobv); 
	}
	return;
}

void flexiblas_chain_sgesvj(void* joba, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* sva, void* mv, void* v, void* ldv, void* work, void* lwork, void* info, blasint len_joba, blasint len_jobu, blasint len_jobv)  __attribute__((alias("flexiblas_chain_sgesvj_")));




