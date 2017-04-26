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
void FC_GLOBAL(slaed8,SLAED8)(blasint* icompq, blasint* k, blasint* n, blasint* qsiz, float* d, float* q, blasint* ldq, blasint* indxq, float* rho, blasint* cutpnt, float* z, float* dlamda, float* q2, blasint* ldq2, float* w, blasint* perm, blasint* givptr, blasint* givcol, float* givnum, blasint* indxp, blasint* indx, blasint* info)
#else
void FC_GLOBAL(slaed8,SLAED8)(blasint* icompq, blasint* k, blasint* n, blasint* qsiz, float* d, float* q, blasint* ldq, blasint* indxq, float* rho, blasint* cutpnt, float* z, float* dlamda, float* q2, blasint* ldq2, float* w, blasint* perm, blasint* givptr, blasint* givcol, float* givnum, blasint* indxp, blasint* indx, blasint* info)
#endif 
{
    double ts;
	void (*fn) (void* icompq, void* k, void* n, void* qsiz, void* d, void* q, void* ldq, void* indxq, void* rho, void* cutpnt, void* z, void* dlamda, void* q2, void* ldq2, void* w, void* perm, void* givptr, void* givcol, void* givnum, void* indxp, void* indx, void* info);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.slaed8.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) icompq, (void*) k, (void*) n, (void*) qsiz, (void*) d, (void*) q, (void*) ldq, (void*) indxq, (void*) rho, (void*) cutpnt, (void*) z, (void*) dlamda, (void*) q2, (void*) ldq2, (void*) w, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) indxp, (void*) indx, (void*) info); 
		current_backend->lapack.slaed8.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.slaed8.calls[0]++;
	} else { 
		fn((void*) icompq, (void*) k, (void*) n, (void*) qsiz, (void*) d, (void*) q, (void*) ldq, (void*) indxq, (void*) rho, (void*) cutpnt, (void*) z, (void*) dlamda, (void*) q2, (void*) ldq2, (void*) w, (void*) perm, (void*) givptr, (void*) givcol, (void*) givnum, (void*) indxp, (void*) indx, (void*) info); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void slaed8_(blasint* icompq, blasint* k, blasint* n, blasint* qsiz, float* d, float* q, blasint* ldq, blasint* indxq, float* rho, blasint* cutpnt, float* z, float* dlamda, float* q2, blasint* ldq2, float* w, blasint* perm, blasint* givptr, blasint* givcol, float* givnum, blasint* indxp, blasint* indx, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slaed8,SLAED8)))));
#else
void slaed8(blasint* icompq, blasint* k, blasint* n, blasint* qsiz, float* d, float* q, blasint* ldq, blasint* indxq, float* rho, blasint* cutpnt, float* z, float* dlamda, float* q2, blasint* ldq2, float* w, blasint* perm, blasint* givptr, blasint* givcol, float* givnum, blasint* indxp, blasint* indx, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slaed8,SLAED8)))));
#endif



