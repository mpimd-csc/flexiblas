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
 /* Generated: Tue Mar 28 16:07:36 2017 */ 
        
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
void FC_GLOBAL_(ssb2st_kernels,SSB2ST_KERNELS)(char* uplo, blasint* wantz, blasint* ttype, blasint* st, blasint* ed, blasint* sweep, blasint* n, blasint* nb, blasint* ib, float* a, blasint* lda, float* v, float* tau, blasint* ldvt, float* work)
#else
void FC_GLOBAL_(ssb2st_kernels,SSB2ST_KERNELS)(char* uplo, blasint* wantz, blasint* ttype, blasint* st, blasint* ed, blasint* sweep, blasint* n, blasint* nb, blasint* ib, float* a, blasint* lda, float* v, float* tau, blasint* ldvt, float* work)
#endif 
{
    double ts;
	void (*fn) (void* uplo, void* wantz, void* ttype, void* st, void* ed, void* sweep, void* n, void* nb, void* ib, void* a, void* lda, void* v, void* tau, void* ldvt, void* work);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.ssb2st_kernels.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) uplo, (void*) wantz, (void*) ttype, (void*) st, (void*) ed, (void*) sweep, (void*) n, (void*) nb, (void*) ib, (void*) a, (void*) lda, (void*) v, (void*) tau, (void*) ldvt, (void*) work); 
		current_backend->lapack.ssb2st_kernels.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.ssb2st_kernels.calls[0]++;
	} else { 
		fn((void*) uplo, (void*) wantz, (void*) ttype, (void*) st, (void*) ed, (void*) sweep, (void*) n, (void*) nb, (void*) ib, (void*) a, (void*) lda, (void*) v, (void*) tau, (void*) ldvt, (void*) work); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void ssb2st_kernels_(char* uplo, blasint* wantz, blasint* ttype, blasint* st, blasint* ed, blasint* sweep, blasint* n, blasint* nb, blasint* ib, float* a, blasint* lda, float* v, float* tau, blasint* ldvt, float* work) __attribute__((alias(MTS(FC_GLOBAL_(ssb2st_kernels,SSB2ST_KERNELS)))));
#else
void ssb2st_kernels(char* uplo, blasint* wantz, blasint* ttype, blasint* st, blasint* ed, blasint* sweep, blasint* n, blasint* nb, blasint* ib, float* a, blasint* lda, float* v, float* tau, blasint* ldvt, float* work) __attribute__((alias(MTS(FC_GLOBAL_(ssb2st_kernels,SSB2ST_KERNELS)))));
#endif



