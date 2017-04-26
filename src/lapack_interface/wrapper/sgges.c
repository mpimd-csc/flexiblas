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
 /* Generated: Tue Mar 28 16:07:35 2017 */ 
        
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
void FC_GLOBAL(sgges,SGGES)(char* jobvsl, char* jobvsr, char* sort, blasint* selctg, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, blasint* sdim, float* alphar, float* alphai, float* beta, float* vsl, blasint* ldvsl, float* vsr, blasint* ldvsr, float* work, blasint* lwork, blasint* bwork, blasint* info)
#else
void FC_GLOBAL(sgges,SGGES)(char* jobvsl, char* jobvsr, char* sort, blasint* selctg, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, blasint* sdim, float* alphar, float* alphai, float* beta, float* vsl, blasint* ldvsl, float* vsr, blasint* ldvsr, float* work, blasint* lwork, blasint* bwork, blasint* info)
#endif 
{
    double ts;
	void (*fn) (void* jobvsl, void* jobvsr, void* sort, void* selctg, void* n, void* a, void* lda, void* b, void* ldb, void* sdim, void* alphar, void* alphai, void* beta, void* vsl, void* ldvsl, void* vsr, void* ldvsr, void* work, void* lwork, void* bwork, void* info);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.sgges.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) jobvsl, (void*) jobvsr, (void*) sort, (void*) selctg, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) sdim, (void*) alphar, (void*) alphai, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) work, (void*) lwork, (void*) bwork, (void*) info); 
		current_backend->lapack.sgges.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.sgges.calls[0]++;
	} else { 
		fn((void*) jobvsl, (void*) jobvsr, (void*) sort, (void*) selctg, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) sdim, (void*) alphar, (void*) alphai, (void*) beta, (void*) vsl, (void*) ldvsl, (void*) vsr, (void*) ldvsr, (void*) work, (void*) lwork, (void*) bwork, (void*) info); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void sgges_(char* jobvsl, char* jobvsr, char* sort, blasint* selctg, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, blasint* sdim, float* alphar, float* alphai, float* beta, float* vsl, blasint* ldvsl, float* vsr, blasint* ldvsr, float* work, blasint* lwork, blasint* bwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(sgges,SGGES)))));
#else
void sgges(char* jobvsl, char* jobvsr, char* sort, blasint* selctg, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, blasint* sdim, float* alphar, float* alphai, float* beta, float* vsl, blasint* ldvsl, float* vsr, blasint* ldvsr, float* work, blasint* lwork, blasint* bwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(sgges,SGGES)))));
#endif



