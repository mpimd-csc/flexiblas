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
 /* Generated: Tue Mar 28 16:07:33 2017 */ 
        
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
void FC_GLOBAL(clabrd,CLABRD)(blasint* m, blasint* n, blasint* nb, float complex* a, blasint* lda, float* d, float* e, float complex* tauq, float complex* taup, float complex* x, blasint* ldx, float complex* y, blasint* ldy)
#else
void FC_GLOBAL(clabrd,CLABRD)(blasint* m, blasint* n, blasint* nb, float complex* a, blasint* lda, float* d, float* e, float complex* tauq, float complex* taup, float complex* x, blasint* ldx, float complex* y, blasint* ldy)
#endif 
{
    double ts;
	void (*fn) (void* m, void* n, void* nb, void* a, void* lda, void* d, void* e, void* tauq, void* taup, void* x, void* ldx, void* y, void* ldy);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.clabrd.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) m, (void*) n, (void*) nb, (void*) a, (void*) lda, (void*) d, (void*) e, (void*) tauq, (void*) taup, (void*) x, (void*) ldx, (void*) y, (void*) ldy); 
		current_backend->lapack.clabrd.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.clabrd.calls[0]++;
	} else { 
		fn((void*) m, (void*) n, (void*) nb, (void*) a, (void*) lda, (void*) d, (void*) e, (void*) tauq, (void*) taup, (void*) x, (void*) ldx, (void*) y, (void*) ldy); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void clabrd_(blasint* m, blasint* n, blasint* nb, float complex* a, blasint* lda, float* d, float* e, float complex* tauq, float complex* taup, float complex* x, blasint* ldx, float complex* y, blasint* ldy) __attribute__((alias(MTS(FC_GLOBAL(clabrd,CLABRD)))));
#else
void clabrd(blasint* m, blasint* n, blasint* nb, float complex* a, blasint* lda, float* d, float* e, float complex* tauq, float complex* taup, float complex* x, blasint* ldx, float complex* y, blasint* ldy) __attribute__((alias(MTS(FC_GLOBAL(clabrd,CLABRD)))));
#endif



