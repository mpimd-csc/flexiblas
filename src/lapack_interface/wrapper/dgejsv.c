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



static TLS_STORE uint8_t hook_pos_dgejsv = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dgejsv,DGEJSV)(char* joba, char* jobu, char* jobv, char* jobr, char* jobt, char* jobp, blasint* m, blasint* n, double* a, blasint* lda, double* sva, double* u, blasint* ldu, double* v, blasint* ldv, double* work, blasint* lwork, blasint* iwork, blasint* info, blasint len_joba, blasint len_jobu, blasint len_jobv, blasint len_jobr, blasint len_jobt, blasint len_jobp)
#else
void FC_GLOBAL(dgejsv,DGEJSV)(char* joba, char* jobu, char* jobv, char* jobr, char* jobt, char* jobp, blasint* m, blasint* n, double* a, blasint* lda, double* sva, double* u, blasint* ldu, double* v, blasint* ldv, double* work, blasint* lwork, blasint* iwork, blasint* info, blasint len_joba, blasint len_jobu, blasint len_jobv, blasint len_jobr, blasint len_jobt, blasint len_jobp)
#endif
{
	void (*fn) (void* joba, void* jobu, void* jobv, void* jobr, void* jobt, void* jobp, void* m, void* n, void* a, void* lda, void* sva, void* u, void* ldu, void* v, void* ldv, void* work, void* lwork, void* iwork, void* info, blasint len_joba, blasint len_jobu, blasint len_jobv, blasint len_jobr, blasint len_jobt, blasint len_jobp);
	void (*fn_hook) (void* joba, void* jobu, void* jobv, void* jobr, void* jobt, void* jobp, void* m, void* n, void* a, void* lda, void* sva, void* u, void* ldu, void* v, void* ldv, void* work, void* lwork, void* iwork, void* info, blasint len_joba, blasint len_jobu, blasint len_jobv, blasint len_jobr, blasint len_jobt, blasint len_jobp);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.dgejsv.f77_blas_function; 
	fn_hook = __flexiblas_hooks->dgejsv.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) joba, (void*) jobu, (void*) jobv, (void*) jobr, (void*) jobt, (void*) jobp, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) sva, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) work, (void*) lwork, (void*) iwork, (void*) info, (blasint) len_joba, (blasint) len_jobu, (blasint) len_jobv, (blasint) len_jobr, (blasint) len_jobt, (blasint) len_jobp); 
		return;
	} else {
		hook_pos_dgejsv = 0;
		fn_hook((void*) joba, (void*) jobu, (void*) jobv, (void*) jobr, (void*) jobt, (void*) jobp, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) sva, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) work, (void*) lwork, (void*) iwork, (void*) info, (blasint) len_joba, (blasint) len_jobu, (blasint) len_jobv, (blasint) len_jobr, (blasint) len_jobt, (blasint) len_jobp);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dgejsv_(char* joba, char* jobu, char* jobv, char* jobr, char* jobt, char* jobp, blasint* m, blasint* n, double* a, blasint* lda, double* sva, double* u, blasint* ldu, double* v, blasint* ldv, double* work, blasint* lwork, blasint* iwork, blasint* info, blasint len_joba, blasint len_jobu, blasint len_jobv, blasint len_jobr, blasint len_jobt, blasint len_jobp) __attribute__((alias(MTS(FC_GLOBAL(dgejsv,DGEJSV)))));
#else
void dgejsv(char* joba, char* jobu, char* jobv, char* jobr, char* jobt, char* jobp, blasint* m, blasint* n, double* a, blasint* lda, double* sva, double* u, blasint* ldu, double* v, blasint* ldv, double* work, blasint* lwork, blasint* iwork, blasint* info, blasint len_joba, blasint len_jobu, blasint len_jobv, blasint len_jobr, blasint len_jobt, blasint len_jobp) __attribute__((alias(MTS(FC_GLOBAL(dgejsv,DGEJSV)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dgejsv_(void* joba, void* jobu, void* jobv, void* jobr, void* jobt, void* jobp, void* m, void* n, void* a, void* lda, void* sva, void* u, void* ldu, void* v, void* ldv, void* work, void* lwork, void* iwork, void* info, blasint len_joba, blasint len_jobu, blasint len_jobv, blasint len_jobr, blasint len_jobt, blasint len_jobp)
{
	void (*fn) (void* joba, void* jobu, void* jobv, void* jobr, void* jobt, void* jobp, void* m, void* n, void* a, void* lda, void* sva, void* u, void* ldu, void* v, void* ldv, void* work, void* lwork, void* iwork, void* info, blasint len_joba, blasint len_jobu, blasint len_jobv, blasint len_jobr, blasint len_jobt, blasint len_jobp);

	fn = current_backend->lapack.dgejsv.f77_blas_function; 

		fn((void*) joba, (void*) jobu, (void*) jobv, (void*) jobr, (void*) jobt, (void*) jobp, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) sva, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) work, (void*) lwork, (void*) iwork, (void*) info, (blasint) len_joba, (blasint) len_jobu, (blasint) len_jobv, (blasint) len_jobr, (blasint) len_jobt, (blasint) len_jobp); 

	return;
}

void flexiblas_real_dgejsv(void* joba, void* jobu, void* jobv, void* jobr, void* jobt, void* jobp, void* m, void* n, void* a, void* lda, void* sva, void* u, void* ldu, void* v, void* ldv, void* work, void* lwork, void* iwork, void* info, blasint len_joba, blasint len_jobu, blasint len_jobv, blasint len_jobr, blasint len_jobt, blasint len_jobp)  __attribute__((alias("flexiblas_real_dgejsv_")));





/* Chainloader for Hooks */


void flexiblas_chain_dgejsv_(void* joba, void* jobu, void* jobv, void* jobr, void* jobt, void* jobp, void* m, void* n, void* a, void* lda, void* sva, void* u, void* ldu, void* v, void* ldv, void* work, void* lwork, void* iwork, void* info, blasint len_joba, blasint len_jobu, blasint len_jobv, blasint len_jobr, blasint len_jobt, blasint len_jobp)
{
	void (*fn) (void* joba, void* jobu, void* jobv, void* jobr, void* jobt, void* jobp, void* m, void* n, void* a, void* lda, void* sva, void* u, void* ldu, void* v, void* ldv, void* work, void* lwork, void* iwork, void* info, blasint len_joba, blasint len_jobu, blasint len_jobv, blasint len_jobr, blasint len_jobt, blasint len_jobp);
	void (*fn_hook) (void* joba, void* jobu, void* jobv, void* jobr, void* jobt, void* jobp, void* m, void* n, void* a, void* lda, void* sva, void* u, void* ldu, void* v, void* ldv, void* work, void* lwork, void* iwork, void* info, blasint len_joba, blasint len_jobu, blasint len_jobv, blasint len_jobr, blasint len_jobt, blasint len_jobp);

	fn      = current_backend->lapack.dgejsv.f77_blas_function; 

    hook_pos_dgejsv ++;
    if( hook_pos_dgejsv < __flexiblas_hooks->dgejsv.nhook) {
        fn_hook = __flexiblas_hooks->dgejsv.f77_hook_function[hook_pos_dgejsv];
        fn_hook((void*) joba, (void*) jobu, (void*) jobv, (void*) jobr, (void*) jobt, (void*) jobp, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) sva, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) work, (void*) lwork, (void*) iwork, (void*) info, (blasint) len_joba, (blasint) len_jobu, (blasint) len_jobv, (blasint) len_jobr, (blasint) len_jobt, (blasint) len_jobp);
    } else {
        hook_pos_dgejsv = 0;
		fn((void*) joba, (void*) jobu, (void*) jobv, (void*) jobr, (void*) jobt, (void*) jobp, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) sva, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) work, (void*) lwork, (void*) iwork, (void*) info, (blasint) len_joba, (blasint) len_jobu, (blasint) len_jobv, (blasint) len_jobr, (blasint) len_jobt, (blasint) len_jobp); 
	}
	return;
}

void flexiblas_chain_dgejsv(void* joba, void* jobu, void* jobv, void* jobr, void* jobt, void* jobp, void* m, void* n, void* a, void* lda, void* sva, void* u, void* ldu, void* v, void* ldv, void* work, void* lwork, void* iwork, void* info, blasint len_joba, blasint len_jobu, blasint len_jobv, blasint len_jobr, blasint len_jobt, blasint len_jobp)  __attribute__((alias("flexiblas_chain_dgejsv_")));




