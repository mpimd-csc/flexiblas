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
void FC_GLOBAL(stgsen,STGSEN)(blasint* ijob, blasint* wantq, blasint* wantz, blasint* select, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, float* alphar, float* alphai, float* beta, float* q, blasint* ldq, float* z, blasint* ldz, blasint* m, float* pl, float* pr, float* dif, float* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info)
#else
void FC_GLOBAL(stgsen,STGSEN)(blasint* ijob, blasint* wantq, blasint* wantz, blasint* select, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, float* alphar, float* alphai, float* beta, float* q, blasint* ldq, float* z, blasint* ldz, blasint* m, float* pl, float* pr, float* dif, float* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info)
#endif 
{
    double ts;
	void (*fn) (void* ijob, void* wantq, void* wantz, void* select, void* n, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* q, void* ldq, void* z, void* ldz, void* m, void* pl, void* pr, void* dif, void* work, void* lwork, void* iwork, void* liwork, void* info);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.stgsen.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) ijob, (void*) wantq, (void*) wantz, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) m, (void*) pl, (void*) pr, (void*) dif, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info); 
		current_backend->lapack.stgsen.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.stgsen.calls[0]++;
	} else { 
		fn((void*) ijob, (void*) wantq, (void*) wantz, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) m, (void*) pl, (void*) pr, (void*) dif, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void stgsen_(blasint* ijob, blasint* wantq, blasint* wantz, blasint* select, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, float* alphar, float* alphai, float* beta, float* q, blasint* ldq, float* z, blasint* ldz, blasint* m, float* pl, float* pr, float* dif, float* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(stgsen,STGSEN)))));
#else
void stgsen(blasint* ijob, blasint* wantq, blasint* wantz, blasint* select, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, float* alphar, float* alphai, float* beta, float* q, blasint* ldq, float* z, blasint* ldz, blasint* m, float* pl, float* pr, float* dif, float* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(stgsen,STGSEN)))));
#endif



