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
void FC_GLOBAL(slasq5,SLASQ5)(blasint* i0, blasint* n0, float* z, blasint* pp, float* tau, float* sigma, float* dmin, float* dmin1, float* dmin2, float* dn, float* dnm1, float* dnm2, blasint* ieee, float* eps)
#else
void FC_GLOBAL(slasq5,SLASQ5)(blasint* i0, blasint* n0, float* z, blasint* pp, float* tau, float* sigma, float* dmin, float* dmin1, float* dmin2, float* dn, float* dnm1, float* dnm2, blasint* ieee, float* eps)
#endif 
{
    double ts;
	void (*fn) (void* i0, void* n0, void* z, void* pp, void* tau, void* sigma, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2, void* ieee, void* eps);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.slasq5.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) tau, (void*) sigma, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dnm1, (void*) dnm2, (void*) ieee, (void*) eps); 
		current_backend->lapack.slasq5.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.slasq5.calls[0]++;
	} else { 
		fn((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) tau, (void*) sigma, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dnm1, (void*) dnm2, (void*) ieee, (void*) eps); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void slasq5_(blasint* i0, blasint* n0, float* z, blasint* pp, float* tau, float* sigma, float* dmin, float* dmin1, float* dmin2, float* dn, float* dnm1, float* dnm2, blasint* ieee, float* eps) __attribute__((alias(MTS(FC_GLOBAL(slasq5,SLASQ5)))));
#else
void slasq5(blasint* i0, blasint* n0, float* z, blasint* pp, float* tau, float* sigma, float* dmin, float* dmin1, float* dmin2, float* dn, float* dnm1, float* dnm2, blasint* ieee, float* eps) __attribute__((alias(MTS(FC_GLOBAL(slasq5,SLASQ5)))));
#endif



