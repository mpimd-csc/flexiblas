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
 * Copyright (C) Martin Koehler, 2015-2017
 */
 /* This file it automatically generated. Please do not edit. */
 /* Generated: Tue Mar 28 16:07:32 2017 */ 
        
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



#ifdef FLEXIBLAS_ABI_INTEL 
void FC_GLOBAL(cggsvd3,CGGSVD3)(char* jobu, char* jobv, char* jobq, blasint* m, blasint* n, blasint* p, blasint* k, blasint* l, float complex* a, blasint* lda, float complex* b, blasint* ldb, float* alpha, float* beta, float complex* u, blasint* ldu, float complex* v, blasint* ldv, float complex* q, blasint* ldq, float complex* work, blasint* lwork, float* rwork, blasint* iwork, blasint* info)
#else
void FC_GLOBAL(cggsvd3,CGGSVD3)(char* jobu, char* jobv, char* jobq, blasint* m, blasint* n, blasint* p, blasint* k, blasint* l, float complex* a, blasint* lda, float complex* b, blasint* ldb, float* alpha, float* beta, float complex* u, blasint* ldu, float complex* v, blasint* ldv, float complex* q, blasint* ldq, float complex* work, blasint* lwork, float* rwork, blasint* iwork, blasint* info)
#endif 
{
    double ts;
	void (*fn) (void* jobu, void* jobv, void* jobq, void* m, void* n, void* p, void* k, void* l, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* work, void* lwork, void* rwork, void* iwork, void* info);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.cggsvd3.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) jobu, (void*) jobv, (void*) jobq, (void*) m, (void*) n, (void*) p, (void*) k, (void*) l, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alpha, (void*) beta, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) q, (void*) ldq, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) info); 
		current_backend->lapack.cggsvd3.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.cggsvd3.calls[0]++;
	} else { 
		fn((void*) jobu, (void*) jobv, (void*) jobq, (void*) m, (void*) n, (void*) p, (void*) k, (void*) l, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alpha, (void*) beta, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) q, (void*) ldq, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) info); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void cggsvd3_(char* jobu, char* jobv, char* jobq, blasint* m, blasint* n, blasint* p, blasint* k, blasint* l, float complex* a, blasint* lda, float complex* b, blasint* ldb, float* alpha, float* beta, float complex* u, blasint* ldu, float complex* v, blasint* ldv, float complex* q, blasint* ldq, float complex* work, blasint* lwork, float* rwork, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(cggsvd3,CGGSVD3)))));
#else
void cggsvd3(char* jobu, char* jobv, char* jobq, blasint* m, blasint* n, blasint* p, blasint* k, blasint* l, float complex* a, blasint* lda, float complex* b, blasint* ldb, float* alpha, float* beta, float complex* u, blasint* ldu, float complex* v, blasint* ldv, float complex* q, blasint* ldq, float complex* work, blasint* lwork, float* rwork, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(cggsvd3,CGGSVD3)))));
#endif



