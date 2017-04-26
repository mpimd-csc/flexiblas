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
 /* Generated: Tue Mar 28 16:07:38 2017 */ 
        
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
void FC_GLOBAL(ztgsja,ZTGSJA)(char* jobu, char* jobv, char* jobq, blasint* m, blasint* p, blasint* n, blasint* k, blasint* l, double complex* a, blasint* lda, double complex* b, blasint* ldb, double* tola, double* tolb, double* alpha, double* beta, double complex* u, blasint* ldu, double complex* v, blasint* ldv, double complex* q, blasint* ldq, double complex* work, blasint* ncycle, blasint* info)
#else
void FC_GLOBAL(ztgsja,ZTGSJA)(char* jobu, char* jobv, char* jobq, blasint* m, blasint* p, blasint* n, blasint* k, blasint* l, double complex* a, blasint* lda, double complex* b, blasint* ldb, double* tola, double* tolb, double* alpha, double* beta, double complex* u, blasint* ldu, double complex* v, blasint* ldv, double complex* q, blasint* ldq, double complex* work, blasint* ncycle, blasint* info)
#endif 
{
    double ts;
	void (*fn) (void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* k, void* l, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* alpha, void* beta, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* work, void* ncycle, void* info);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.ztgsja.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) jobu, (void*) jobv, (void*) jobq, (void*) m, (void*) p, (void*) n, (void*) k, (void*) l, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) tola, (void*) tolb, (void*) alpha, (void*) beta, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) q, (void*) ldq, (void*) work, (void*) ncycle, (void*) info); 
		current_backend->lapack.ztgsja.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.ztgsja.calls[0]++;
	} else { 
		fn((void*) jobu, (void*) jobv, (void*) jobq, (void*) m, (void*) p, (void*) n, (void*) k, (void*) l, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) tola, (void*) tolb, (void*) alpha, (void*) beta, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) q, (void*) ldq, (void*) work, (void*) ncycle, (void*) info); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void ztgsja_(char* jobu, char* jobv, char* jobq, blasint* m, blasint* p, blasint* n, blasint* k, blasint* l, double complex* a, blasint* lda, double complex* b, blasint* ldb, double* tola, double* tolb, double* alpha, double* beta, double complex* u, blasint* ldu, double complex* v, blasint* ldv, double complex* q, blasint* ldq, double complex* work, blasint* ncycle, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(ztgsja,ZTGSJA)))));
#else
void ztgsja(char* jobu, char* jobv, char* jobq, blasint* m, blasint* p, blasint* n, blasint* k, blasint* l, double complex* a, blasint* lda, double complex* b, blasint* ldb, double* tola, double* tolb, double* alpha, double* beta, double complex* u, blasint* ldu, double complex* v, blasint* ldv, double complex* q, blasint* ldq, double complex* work, blasint* ncycle, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(ztgsja,ZTGSJA)))));
#endif



