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
void FC_GLOBAL(dlaed2,DLAED2)(blasint* k, blasint* n, blasint* n1, double* d, double* q, blasint* ldq, blasint* indxq, double* rho, double* z, double* dlamda, double* w, double* q2, blasint* indx, blasint* indxc, blasint* indxp, blasint* coltyp, blasint* info)
#else
void FC_GLOBAL(dlaed2,DLAED2)(blasint* k, blasint* n, blasint* n1, double* d, double* q, blasint* ldq, blasint* indxq, double* rho, double* z, double* dlamda, double* w, double* q2, blasint* indx, blasint* indxc, blasint* indxp, blasint* coltyp, blasint* info)
#endif 
{
    double ts;
	void (*fn) (void* k, void* n, void* n1, void* d, void* q, void* ldq, void* indxq, void* rho, void* z, void* dlamda, void* w, void* q2, void* indx, void* indxc, void* indxp, void* coltyp, void* info);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.dlaed2.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) k, (void*) n, (void*) n1, (void*) d, (void*) q, (void*) ldq, (void*) indxq, (void*) rho, (void*) z, (void*) dlamda, (void*) w, (void*) q2, (void*) indx, (void*) indxc, (void*) indxp, (void*) coltyp, (void*) info); 
		current_backend->lapack.dlaed2.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.dlaed2.calls[0]++;
	} else { 
		fn((void*) k, (void*) n, (void*) n1, (void*) d, (void*) q, (void*) ldq, (void*) indxq, (void*) rho, (void*) z, (void*) dlamda, (void*) w, (void*) q2, (void*) indx, (void*) indxc, (void*) indxp, (void*) coltyp, (void*) info); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void dlaed2_(blasint* k, blasint* n, blasint* n1, double* d, double* q, blasint* ldq, blasint* indxq, double* rho, double* z, double* dlamda, double* w, double* q2, blasint* indx, blasint* indxc, blasint* indxp, blasint* coltyp, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlaed2,DLAED2)))));
#else
void dlaed2(blasint* k, blasint* n, blasint* n1, double* d, double* q, blasint* ldq, blasint* indxq, double* rho, double* z, double* dlamda, double* w, double* q2, blasint* indx, blasint* indxc, blasint* indxp, blasint* coltyp, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlaed2,DLAED2)))));
#endif



