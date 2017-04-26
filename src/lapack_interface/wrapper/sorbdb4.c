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
void FC_GLOBAL(sorbdb4,SORBDB4)(blasint* m, blasint* p, blasint* q, float* x11, blasint* ldx11, float* x21, blasint* ldx21, float* theta, float* phi, float* taup1, float* taup2, float* tauq1, float* phantom, float* work, blasint* lwork, blasint* info)
#else
void FC_GLOBAL(sorbdb4,SORBDB4)(blasint* m, blasint* p, blasint* q, float* x11, blasint* ldx11, float* x21, blasint* ldx21, float* theta, float* phi, float* taup1, float* taup2, float* tauq1, float* phantom, float* work, blasint* lwork, blasint* info)
#endif 
{
    double ts;
	void (*fn) (void* m, void* p, void* q, void* x11, void* ldx11, void* x21, void* ldx21, void* theta, void* phi, void* taup1, void* taup2, void* tauq1, void* phantom, void* work, void* lwork, void* info);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.sorbdb4.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) m, (void*) p, (void*) q, (void*) x11, (void*) ldx11, (void*) x21, (void*) ldx21, (void*) theta, (void*) phi, (void*) taup1, (void*) taup2, (void*) tauq1, (void*) phantom, (void*) work, (void*) lwork, (void*) info); 
		current_backend->lapack.sorbdb4.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.sorbdb4.calls[0]++;
	} else { 
		fn((void*) m, (void*) p, (void*) q, (void*) x11, (void*) ldx11, (void*) x21, (void*) ldx21, (void*) theta, (void*) phi, (void*) taup1, (void*) taup2, (void*) tauq1, (void*) phantom, (void*) work, (void*) lwork, (void*) info); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void sorbdb4_(blasint* m, blasint* p, blasint* q, float* x11, blasint* ldx11, float* x21, blasint* ldx21, float* theta, float* phi, float* taup1, float* taup2, float* tauq1, float* phantom, float* work, blasint* lwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(sorbdb4,SORBDB4)))));
#else
void sorbdb4(blasint* m, blasint* p, blasint* q, float* x11, blasint* ldx11, float* x21, blasint* ldx21, float* theta, float* phi, float* taup1, float* taup2, float* tauq1, float* phantom, float* work, blasint* lwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(sorbdb4,SORBDB4)))));
#endif



