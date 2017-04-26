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
void FC_GLOBAL(dlasq4,DLASQ4)(blasint* i0, blasint* n0, double* z, blasint* pp, blasint* n0in, double* dmin, double* dmin1, double* dmin2, double* dn, double* dn1, double* dn2, double* tau, blasint* ttype, double* g)
#else
void FC_GLOBAL(dlasq4,DLASQ4)(blasint* i0, blasint* n0, double* z, blasint* pp, blasint* n0in, double* dmin, double* dmin1, double* dmin2, double* dn, double* dn1, double* dn2, double* tau, blasint* ttype, double* g)
#endif 
{
    double ts;
	void (*fn) (void* i0, void* n0, void* z, void* pp, void* n0in, void* dmin, void* dmin1, void* dmin2, void* dn, void* dn1, void* dn2, void* tau, void* ttype, void* g);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.dlasq4.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) n0in, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dn1, (void*) dn2, (void*) tau, (void*) ttype, (void*) g); 
		current_backend->lapack.dlasq4.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.dlasq4.calls[0]++;
	} else { 
		fn((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) n0in, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dn1, (void*) dn2, (void*) tau, (void*) ttype, (void*) g); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void dlasq4_(blasint* i0, blasint* n0, double* z, blasint* pp, blasint* n0in, double* dmin, double* dmin1, double* dmin2, double* dn, double* dn1, double* dn2, double* tau, blasint* ttype, double* g) __attribute__((alias(MTS(FC_GLOBAL(dlasq4,DLASQ4)))));
#else
void dlasq4(blasint* i0, blasint* n0, double* z, blasint* pp, blasint* n0in, double* dmin, double* dmin1, double* dmin2, double* dn, double* dn1, double* dn2, double* tau, blasint* ttype, double* g) __attribute__((alias(MTS(FC_GLOBAL(dlasq4,DLASQ4)))));
#endif



