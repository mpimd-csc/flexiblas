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



static TLS_STORE uint8_t hook_pos_slasq4 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slasq4,SLASQ4)(blasint* i0, blasint* n0, float* z, blasint* pp, blasint* n0in, float* dmin, float* dmin1, float* dmin2, float* dn, float* dn1, float* dn2, float* tau, blasint* ttype, float* g)
#else
void FC_GLOBAL(slasq4,SLASQ4)(blasint* i0, blasint* n0, float* z, blasint* pp, blasint* n0in, float* dmin, float* dmin1, float* dmin2, float* dn, float* dn1, float* dn2, float* tau, blasint* ttype, float* g)
#endif
{
	void (*fn) (void* i0, void* n0, void* z, void* pp, void* n0in, void* dmin, void* dmin1, void* dmin2, void* dn, void* dn1, void* dn2, void* tau, void* ttype, void* g);
	void (*fn_hook) (void* i0, void* n0, void* z, void* pp, void* n0in, void* dmin, void* dmin1, void* dmin2, void* dn, void* dn1, void* dn2, void* tau, void* ttype, void* g);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.slasq4.f77_blas_function; 
	fn_hook = __flexiblas_hooks->slasq4.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) n0in, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dn1, (void*) dn2, (void*) tau, (void*) ttype, (void*) g); 
		return;
	} else {
		hook_pos_slasq4 = 0;
		fn_hook((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) n0in, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dn1, (void*) dn2, (void*) tau, (void*) ttype, (void*) g);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void slasq4_(blasint* i0, blasint* n0, float* z, blasint* pp, blasint* n0in, float* dmin, float* dmin1, float* dmin2, float* dn, float* dn1, float* dn2, float* tau, blasint* ttype, float* g) __attribute__((alias(MTS(FC_GLOBAL(slasq4,SLASQ4)))));
#else
void slasq4(blasint* i0, blasint* n0, float* z, blasint* pp, blasint* n0in, float* dmin, float* dmin1, float* dmin2, float* dn, float* dn1, float* dn2, float* tau, blasint* ttype, float* g) __attribute__((alias(MTS(FC_GLOBAL(slasq4,SLASQ4)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slasq4_(void* i0, void* n0, void* z, void* pp, void* n0in, void* dmin, void* dmin1, void* dmin2, void* dn, void* dn1, void* dn2, void* tau, void* ttype, void* g)
{
	void (*fn) (void* i0, void* n0, void* z, void* pp, void* n0in, void* dmin, void* dmin1, void* dmin2, void* dn, void* dn1, void* dn2, void* tau, void* ttype, void* g);

	fn = current_backend->lapack.slasq4.f77_blas_function; 

		fn((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) n0in, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dn1, (void*) dn2, (void*) tau, (void*) ttype, (void*) g); 

	return;
}

void flexiblas_real_slasq4(void* i0, void* n0, void* z, void* pp, void* n0in, void* dmin, void* dmin1, void* dmin2, void* dn, void* dn1, void* dn2, void* tau, void* ttype, void* g)  __attribute__((alias("flexiblas_real_slasq4_")));





/* Chainloader for Hooks */


void flexiblas_chain_slasq4_(void* i0, void* n0, void* z, void* pp, void* n0in, void* dmin, void* dmin1, void* dmin2, void* dn, void* dn1, void* dn2, void* tau, void* ttype, void* g)
{
	void (*fn) (void* i0, void* n0, void* z, void* pp, void* n0in, void* dmin, void* dmin1, void* dmin2, void* dn, void* dn1, void* dn2, void* tau, void* ttype, void* g);
	void (*fn_hook) (void* i0, void* n0, void* z, void* pp, void* n0in, void* dmin, void* dmin1, void* dmin2, void* dn, void* dn1, void* dn2, void* tau, void* ttype, void* g);

	fn      = current_backend->lapack.slasq4.f77_blas_function; 

    hook_pos_slasq4 ++;
    if( hook_pos_slasq4 < __flexiblas_hooks->slasq4.nhook) {
        fn_hook = __flexiblas_hooks->slasq4.f77_hook_function[hook_pos_slasq4];
        fn_hook((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) n0in, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dn1, (void*) dn2, (void*) tau, (void*) ttype, (void*) g);
    } else {
        hook_pos_slasq4 = 0;
		fn((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) n0in, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dn1, (void*) dn2, (void*) tau, (void*) ttype, (void*) g); 
	}
	return;
}

void flexiblas_chain_slasq4(void* i0, void* n0, void* z, void* pp, void* n0in, void* dmin, void* dmin1, void* dmin2, void* dn, void* dn1, void* dn2, void* tau, void* ttype, void* g)  __attribute__((alias("flexiblas_chain_slasq4_")));




