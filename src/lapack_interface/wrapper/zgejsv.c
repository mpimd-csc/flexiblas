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
 /* Generated: Tue Mar 28 16:07:37 2017 */ 
        
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
void FC_GLOBAL(zgejsv,ZGEJSV)(char* joba, char* jobu, char* jobv, char* jobr, char* jobt, char* jobp, blasint* m, blasint* n, double complex* a, blasint* lda, double* sva, double complex* u, blasint* ldu, double complex* v, blasint* ldv, double complex* cwork, blasint* lwork, double* rwork, blasint* lrwork, blasint* iwork, blasint* info, blasint len_joba, blasint len_jobu, blasint len_jobv, blasint len_jobr, blasint len_jobt, blasint len_jobp)
#else
void FC_GLOBAL(zgejsv,ZGEJSV)(char* joba, char* jobu, char* jobv, char* jobr, char* jobt, char* jobp, blasint* m, blasint* n, double complex* a, blasint* lda, double* sva, double complex* u, blasint* ldu, double complex* v, blasint* ldv, double complex* cwork, blasint* lwork, double* rwork, blasint* lrwork, blasint* iwork, blasint* info, blasint len_joba, blasint len_jobu, blasint len_jobv, blasint len_jobr, blasint len_jobt, blasint len_jobp)
#endif 
{
    double ts;
	void (*fn) (void* joba, void* jobu, void* jobv, void* jobr, void* jobt, void* jobp, void* m, void* n, void* a, void* lda, void* sva, void* u, void* ldu, void* v, void* ldv, void* cwork, void* lwork, void* rwork, void* lrwork, void* iwork, void* info, blasint len_joba, blasint len_jobu, blasint len_jobv, blasint len_jobr, blasint len_jobt, blasint len_jobp);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.zgejsv.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) joba, (void*) jobu, (void*) jobv, (void*) jobr, (void*) jobt, (void*) jobp, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) sva, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) cwork, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) iwork, (void*) info, (blasint) len_joba, (blasint) len_jobu, (blasint) len_jobv, (blasint) len_jobr, (blasint) len_jobt, (blasint) len_jobp); 
		current_backend->lapack.zgejsv.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.zgejsv.calls[0]++;
	} else { 
		fn((void*) joba, (void*) jobu, (void*) jobv, (void*) jobr, (void*) jobt, (void*) jobp, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) sva, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) cwork, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) iwork, (void*) info, (blasint) len_joba, (blasint) len_jobu, (blasint) len_jobv, (blasint) len_jobr, (blasint) len_jobt, (blasint) len_jobp); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void zgejsv_(char* joba, char* jobu, char* jobv, char* jobr, char* jobt, char* jobp, blasint* m, blasint* n, double complex* a, blasint* lda, double* sva, double complex* u, blasint* ldu, double complex* v, blasint* ldv, double complex* cwork, blasint* lwork, double* rwork, blasint* lrwork, blasint* iwork, blasint* info, blasint len_joba, blasint len_jobu, blasint len_jobv, blasint len_jobr, blasint len_jobt, blasint len_jobp) __attribute__((alias(MTS(FC_GLOBAL(zgejsv,ZGEJSV)))));
#else
void zgejsv(char* joba, char* jobu, char* jobv, char* jobr, char* jobt, char* jobp, blasint* m, blasint* n, double complex* a, blasint* lda, double* sva, double complex* u, blasint* ldu, double complex* v, blasint* ldv, double complex* cwork, blasint* lwork, double* rwork, blasint* lrwork, blasint* iwork, blasint* info, blasint len_joba, blasint len_jobu, blasint len_jobv, blasint len_jobr, blasint len_jobt, blasint len_jobp) __attribute__((alias(MTS(FC_GLOBAL(zgejsv,ZGEJSV)))));
#endif



