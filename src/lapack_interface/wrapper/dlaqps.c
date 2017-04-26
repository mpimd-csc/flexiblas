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
 /* Generated: Tue Mar 28 16:07:34 2017 */ 
        
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
void FC_GLOBAL(dlaqps,DLAQPS)(blasint* m, blasint* n, blasint* offset, blasint* nb, blasint* kb, double* a, blasint* lda, blasint* jpvt, double* tau, double* vn1, double* vn2, double* auxv, double* f, blasint* ldf)
#else
void FC_GLOBAL(dlaqps,DLAQPS)(blasint* m, blasint* n, blasint* offset, blasint* nb, blasint* kb, double* a, blasint* lda, blasint* jpvt, double* tau, double* vn1, double* vn2, double* auxv, double* f, blasint* ldf)
#endif 
{
    double ts;
	void (*fn) (void* m, void* n, void* offset, void* nb, void* kb, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.dlaqps.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) m, (void*) n, (void*) offset, (void*) nb, (void*) kb, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) vn1, (void*) vn2, (void*) auxv, (void*) f, (void*) ldf); 
		current_backend->lapack.dlaqps.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.dlaqps.calls[0]++;
	} else { 
		fn((void*) m, (void*) n, (void*) offset, (void*) nb, (void*) kb, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) vn1, (void*) vn2, (void*) auxv, (void*) f, (void*) ldf); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void dlaqps_(blasint* m, blasint* n, blasint* offset, blasint* nb, blasint* kb, double* a, blasint* lda, blasint* jpvt, double* tau, double* vn1, double* vn2, double* auxv, double* f, blasint* ldf) __attribute__((alias(MTS(FC_GLOBAL(dlaqps,DLAQPS)))));
#else
void dlaqps(blasint* m, blasint* n, blasint* offset, blasint* nb, blasint* kb, double* a, blasint* lda, blasint* jpvt, double* tau, double* vn1, double* vn2, double* auxv, double* f, blasint* ldf) __attribute__((alias(MTS(FC_GLOBAL(dlaqps,DLAQPS)))));
#endif



