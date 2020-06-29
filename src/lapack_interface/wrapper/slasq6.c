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
 * Copyright (C) Martin Koehler, 2013-2020
 */
 /* This file it automatically generated. Please do not edit. */
 /* Generated: Wed Mar 28 11:20:04 2018 */
        
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



static TLS_STORE uint8_t hook_pos_slasq6 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slasq6,SLASQ6)(blasint* i0, blasint* n0, float* z, blasint* pp, float* dmin, float* dmin1, float* dmin2, float* dn, float* dnm1, float* dnm2)
#else
void FC_GLOBAL(slasq6,SLASQ6)(blasint* i0, blasint* n0, float* z, blasint* pp, float* dmin, float* dmin1, float* dmin2, float* dn, float* dnm1, float* dnm2)
#endif
{
	void (*fn) (void* i0, void* n0, void* z, void* pp, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2);
	void (*fn_hook) (void* i0, void* n0, void* z, void* pp, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.slasq6.f77_blas_function; 
	fn_hook = __flexiblas_hooks->slasq6.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dnm1, (void*) dnm2); 
		return;
	} else {
		hook_pos_slasq6 = 0;
		fn_hook((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dnm1, (void*) dnm2);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void slasq6_(blasint* i0, blasint* n0, float* z, blasint* pp, float* dmin, float* dmin1, float* dmin2, float* dn, float* dnm1, float* dnm2) __attribute__((alias(MTS(FC_GLOBAL(slasq6,SLASQ6)))));
#else
void slasq6(blasint* i0, blasint* n0, float* z, blasint* pp, float* dmin, float* dmin1, float* dmin2, float* dn, float* dnm1, float* dnm2) __attribute__((alias(MTS(FC_GLOBAL(slasq6,SLASQ6)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slasq6_(void* i0, void* n0, void* z, void* pp, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2)
{
	void (*fn) (void* i0, void* n0, void* z, void* pp, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2);

	fn = current_backend->lapack.slasq6.f77_blas_function; 

		fn((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dnm1, (void*) dnm2); 

	return;
}

void flexiblas_real_slasq6(void* i0, void* n0, void* z, void* pp, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2)  __attribute__((alias("flexiblas_real_slasq6_")));





/* Chainloader for Hooks */


void flexiblas_chain_slasq6_(void* i0, void* n0, void* z, void* pp, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2)
{
	void (*fn) (void* i0, void* n0, void* z, void* pp, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2);
	void (*fn_hook) (void* i0, void* n0, void* z, void* pp, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2);

	fn      = current_backend->lapack.slasq6.f77_blas_function; 

    hook_pos_slasq6 ++;
    if( hook_pos_slasq6 < __flexiblas_hooks->slasq6.nhook) {
        fn_hook = __flexiblas_hooks->slasq6.f77_hook_function[hook_pos_slasq6];
        fn_hook((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dnm1, (void*) dnm2);
    } else {
        hook_pos_slasq6 = 0;
		fn((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dnm1, (void*) dnm2); 
	}
	return;
}

void flexiblas_chain_slasq6(void* i0, void* n0, void* z, void* pp, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2)  __attribute__((alias("flexiblas_chain_slasq6_")));




