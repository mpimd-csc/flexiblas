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
void FC_GLOBAL(slasd2,SLASD2)(blasint* nl, blasint* nr, blasint* sqre, blasint* k, float* d, float* z, float* alpha, float* beta, float* u, blasint* ldu, float* vt, blasint* ldvt, float* dsigma, float* u2, blasint* ldu2, float* vt2, blasint* ldvt2, blasint* idxp, blasint* idx, blasint* idxc, blasint* idxq, blasint* coltyp, blasint* info)
#else
void FC_GLOBAL(slasd2,SLASD2)(blasint* nl, blasint* nr, blasint* sqre, blasint* k, float* d, float* z, float* alpha, float* beta, float* u, blasint* ldu, float* vt, blasint* ldvt, float* dsigma, float* u2, blasint* ldu2, float* vt2, blasint* ldvt2, blasint* idxp, blasint* idx, blasint* idxc, blasint* idxq, blasint* coltyp, blasint* info)
#endif 
{
    double ts;
	void (*fn) (void* nl, void* nr, void* sqre, void* k, void* d, void* z, void* alpha, void* beta, void* u, void* ldu, void* vt, void* ldvt, void* dsigma, void* u2, void* ldu2, void* vt2, void* ldvt2, void* idxp, void* idx, void* idxc, void* idxq, void* coltyp, void* info);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.slasd2.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) z, (void*) alpha, (void*) beta, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) dsigma, (void*) u2, (void*) ldu2, (void*) vt2, (void*) ldvt2, (void*) idxp, (void*) idx, (void*) idxc, (void*) idxq, (void*) coltyp, (void*) info); 
		current_backend->lapack.slasd2.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.slasd2.calls[0]++;
	} else { 
		fn((void*) nl, (void*) nr, (void*) sqre, (void*) k, (void*) d, (void*) z, (void*) alpha, (void*) beta, (void*) u, (void*) ldu, (void*) vt, (void*) ldvt, (void*) dsigma, (void*) u2, (void*) ldu2, (void*) vt2, (void*) ldvt2, (void*) idxp, (void*) idx, (void*) idxc, (void*) idxq, (void*) coltyp, (void*) info); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void slasd2_(blasint* nl, blasint* nr, blasint* sqre, blasint* k, float* d, float* z, float* alpha, float* beta, float* u, blasint* ldu, float* vt, blasint* ldvt, float* dsigma, float* u2, blasint* ldu2, float* vt2, blasint* ldvt2, blasint* idxp, blasint* idx, blasint* idxc, blasint* idxq, blasint* coltyp, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slasd2,SLASD2)))));
#else
void slasd2(blasint* nl, blasint* nr, blasint* sqre, blasint* k, float* d, float* z, float* alpha, float* beta, float* u, blasint* ldu, float* vt, blasint* ldvt, float* dsigma, float* u2, blasint* ldu2, float* vt2, blasint* ldvt2, blasint* idxp, blasint* idx, blasint* idxc, blasint* idxq, blasint* coltyp, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slasd2,SLASD2)))));
#endif



