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



static TLS_STORE uint8_t hook_pos_sgesvdq = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(sgesvdq,SGESVDQ)(char* joba, char* jobp, char* jobr, char* jobu, char* jobv, blasint* m, blasint* n, float* a, blasint* lda, float* s, float* u, blasint* ldu, float* v, blasint* ldv, blasint* numrank, blasint* iwork, blasint* liwork, float* work, blasint* lwork, float* rwork, blasint* lrwork, blasint* info)
#else
void FC_GLOBAL(sgesvdq,SGESVDQ)(char* joba, char* jobp, char* jobr, char* jobu, char* jobv, blasint* m, blasint* n, float* a, blasint* lda, float* s, float* u, blasint* ldu, float* v, blasint* ldv, blasint* numrank, blasint* iwork, blasint* liwork, float* work, blasint* lwork, float* rwork, blasint* lrwork, blasint* info)
#endif
{
	void (*fn) (void* joba, void* jobp, void* jobr, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* v, void* ldv, void* numrank, void* iwork, void* liwork, void* work, void* lwork, void* rwork, void* lrwork, void* info);
	void (*fn_hook) (void* joba, void* jobp, void* jobr, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* v, void* ldv, void* numrank, void* iwork, void* liwork, void* work, void* lwork, void* rwork, void* lrwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.sgesvdq.f77_blas_function; 
	fn_hook = __flexiblas_hooks->sgesvdq.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) joba, (void*) jobp, (void*) jobr, (void*) jobu, (void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) numrank, (void*) iwork, (void*) liwork, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) info); 
		return;
	} else {
		hook_pos_sgesvdq = 0;
		fn_hook((void*) joba, (void*) jobp, (void*) jobr, (void*) jobu, (void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) numrank, (void*) iwork, (void*) liwork, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void sgesvdq_(char* joba, char* jobp, char* jobr, char* jobu, char* jobv, blasint* m, blasint* n, float* a, blasint* lda, float* s, float* u, blasint* ldu, float* v, blasint* ldv, blasint* numrank, blasint* iwork, blasint* liwork, float* work, blasint* lwork, float* rwork, blasint* lrwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(sgesvdq,SGESVDQ)))));
#else
void sgesvdq(char* joba, char* jobp, char* jobr, char* jobu, char* jobv, blasint* m, blasint* n, float* a, blasint* lda, float* s, float* u, blasint* ldu, float* v, blasint* ldv, blasint* numrank, blasint* iwork, blasint* liwork, float* work, blasint* lwork, float* rwork, blasint* lrwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(sgesvdq,SGESVDQ)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_sgesvdq_(void* joba, void* jobp, void* jobr, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* v, void* ldv, void* numrank, void* iwork, void* liwork, void* work, void* lwork, void* rwork, void* lrwork, void* info)
{
	void (*fn) (void* joba, void* jobp, void* jobr, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* v, void* ldv, void* numrank, void* iwork, void* liwork, void* work, void* lwork, void* rwork, void* lrwork, void* info);

	fn = current_backend->lapack.sgesvdq.f77_blas_function; 

		fn((void*) joba, (void*) jobp, (void*) jobr, (void*) jobu, (void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) numrank, (void*) iwork, (void*) liwork, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) info); 

	return;
}

void flexiblas_real_sgesvdq(void* joba, void* jobp, void* jobr, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* v, void* ldv, void* numrank, void* iwork, void* liwork, void* work, void* lwork, void* rwork, void* lrwork, void* info)  __attribute__((alias("flexiblas_real_sgesvdq_")));





/* Chainloader for Hooks */


void flexiblas_chain_sgesvdq_(void* joba, void* jobp, void* jobr, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* v, void* ldv, void* numrank, void* iwork, void* liwork, void* work, void* lwork, void* rwork, void* lrwork, void* info)
{
	void (*fn) (void* joba, void* jobp, void* jobr, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* v, void* ldv, void* numrank, void* iwork, void* liwork, void* work, void* lwork, void* rwork, void* lrwork, void* info);
	void (*fn_hook) (void* joba, void* jobp, void* jobr, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* v, void* ldv, void* numrank, void* iwork, void* liwork, void* work, void* lwork, void* rwork, void* lrwork, void* info);

	fn      = current_backend->lapack.sgesvdq.f77_blas_function; 

    hook_pos_sgesvdq ++;
    if( hook_pos_sgesvdq < __flexiblas_hooks->sgesvdq.nhook) {
        fn_hook = __flexiblas_hooks->sgesvdq.f77_hook_function[hook_pos_sgesvdq];
        fn_hook((void*) joba, (void*) jobp, (void*) jobr, (void*) jobu, (void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) numrank, (void*) iwork, (void*) liwork, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) info);
    } else {
        hook_pos_sgesvdq = 0;
		fn((void*) joba, (void*) jobp, (void*) jobr, (void*) jobu, (void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) numrank, (void*) iwork, (void*) liwork, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) info); 
	}
	return;
}

void flexiblas_chain_sgesvdq(void* joba, void* jobp, void* jobr, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* v, void* ldv, void* numrank, void* iwork, void* liwork, void* work, void* lwork, void* rwork, void* lrwork, void* info)  __attribute__((alias("flexiblas_chain_sgesvdq_")));




