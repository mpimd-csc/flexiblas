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



static TLS_STORE uint8_t hook_pos_dlasq3 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlasq3,DLASQ3)(blasint* i0, blasint* n0, double* z, blasint* pp, double* dmin, double* sigma, double* desig, double* qmax, blasint* nfail, blasint* iter, blasint* ndiv, blasint* ieee, blasint* ttype, double* dmin1, double* dmin2, double* dn, double* dn1, double* dn2, double* g, double* tau)
#else
void FC_GLOBAL(dlasq3,DLASQ3)(blasint* i0, blasint* n0, double* z, blasint* pp, double* dmin, double* sigma, double* desig, double* qmax, blasint* nfail, blasint* iter, blasint* ndiv, blasint* ieee, blasint* ttype, double* dmin1, double* dmin2, double* dn, double* dn1, double* dn2, double* g, double* tau)
#endif
{
	void (*fn) (void* i0, void* n0, void* z, void* pp, void* dmin, void* sigma, void* desig, void* qmax, void* nfail, void* iter, void* ndiv, void* ieee, void* ttype, void* dmin1, void* dmin2, void* dn, void* dn1, void* dn2, void* g, void* tau);
	void (*fn_hook) (void* i0, void* n0, void* z, void* pp, void* dmin, void* sigma, void* desig, void* qmax, void* nfail, void* iter, void* ndiv, void* ieee, void* ttype, void* dmin1, void* dmin2, void* dn, void* dn1, void* dn2, void* g, void* tau);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.dlasq3.f77_blas_function; 
	fn_hook = __flexiblas_hooks->dlasq3.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) dmin, (void*) sigma, (void*) desig, (void*) qmax, (void*) nfail, (void*) iter, (void*) ndiv, (void*) ieee, (void*) ttype, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dn1, (void*) dn2, (void*) g, (void*) tau); 
		return;
	} else {
		hook_pos_dlasq3 = 0;
		fn_hook((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) dmin, (void*) sigma, (void*) desig, (void*) qmax, (void*) nfail, (void*) iter, (void*) ndiv, (void*) ieee, (void*) ttype, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dn1, (void*) dn2, (void*) g, (void*) tau);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dlasq3_(blasint* i0, blasint* n0, double* z, blasint* pp, double* dmin, double* sigma, double* desig, double* qmax, blasint* nfail, blasint* iter, blasint* ndiv, blasint* ieee, blasint* ttype, double* dmin1, double* dmin2, double* dn, double* dn1, double* dn2, double* g, double* tau) __attribute__((alias(MTS(FC_GLOBAL(dlasq3,DLASQ3)))));
#else
void dlasq3(blasint* i0, blasint* n0, double* z, blasint* pp, double* dmin, double* sigma, double* desig, double* qmax, blasint* nfail, blasint* iter, blasint* ndiv, blasint* ieee, blasint* ttype, double* dmin1, double* dmin2, double* dn, double* dn1, double* dn2, double* g, double* tau) __attribute__((alias(MTS(FC_GLOBAL(dlasq3,DLASQ3)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlasq3_(void* i0, void* n0, void* z, void* pp, void* dmin, void* sigma, void* desig, void* qmax, void* nfail, void* iter, void* ndiv, void* ieee, void* ttype, void* dmin1, void* dmin2, void* dn, void* dn1, void* dn2, void* g, void* tau)
{
	void (*fn) (void* i0, void* n0, void* z, void* pp, void* dmin, void* sigma, void* desig, void* qmax, void* nfail, void* iter, void* ndiv, void* ieee, void* ttype, void* dmin1, void* dmin2, void* dn, void* dn1, void* dn2, void* g, void* tau);

	fn = current_backend->lapack.dlasq3.f77_blas_function; 

		fn((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) dmin, (void*) sigma, (void*) desig, (void*) qmax, (void*) nfail, (void*) iter, (void*) ndiv, (void*) ieee, (void*) ttype, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dn1, (void*) dn2, (void*) g, (void*) tau); 

	return;
}

void flexiblas_real_dlasq3(void* i0, void* n0, void* z, void* pp, void* dmin, void* sigma, void* desig, void* qmax, void* nfail, void* iter, void* ndiv, void* ieee, void* ttype, void* dmin1, void* dmin2, void* dn, void* dn1, void* dn2, void* g, void* tau)  __attribute__((alias("flexiblas_real_dlasq3_")));





/* Chainloader for Hooks */


void flexiblas_chain_dlasq3_(void* i0, void* n0, void* z, void* pp, void* dmin, void* sigma, void* desig, void* qmax, void* nfail, void* iter, void* ndiv, void* ieee, void* ttype, void* dmin1, void* dmin2, void* dn, void* dn1, void* dn2, void* g, void* tau)
{
	void (*fn) (void* i0, void* n0, void* z, void* pp, void* dmin, void* sigma, void* desig, void* qmax, void* nfail, void* iter, void* ndiv, void* ieee, void* ttype, void* dmin1, void* dmin2, void* dn, void* dn1, void* dn2, void* g, void* tau);
	void (*fn_hook) (void* i0, void* n0, void* z, void* pp, void* dmin, void* sigma, void* desig, void* qmax, void* nfail, void* iter, void* ndiv, void* ieee, void* ttype, void* dmin1, void* dmin2, void* dn, void* dn1, void* dn2, void* g, void* tau);

	fn      = current_backend->lapack.dlasq3.f77_blas_function; 

    hook_pos_dlasq3 ++;
    if( hook_pos_dlasq3 < __flexiblas_hooks->dlasq3.nhook) {
        fn_hook = __flexiblas_hooks->dlasq3.f77_hook_function[hook_pos_dlasq3];
        fn_hook((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) dmin, (void*) sigma, (void*) desig, (void*) qmax, (void*) nfail, (void*) iter, (void*) ndiv, (void*) ieee, (void*) ttype, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dn1, (void*) dn2, (void*) g, (void*) tau);
    } else {
        hook_pos_dlasq3 = 0;
		fn((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) dmin, (void*) sigma, (void*) desig, (void*) qmax, (void*) nfail, (void*) iter, (void*) ndiv, (void*) ieee, (void*) ttype, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dn1, (void*) dn2, (void*) g, (void*) tau); 
	}
	return;
}

void flexiblas_chain_dlasq3(void* i0, void* n0, void* z, void* pp, void* dmin, void* sigma, void* desig, void* qmax, void* nfail, void* iter, void* ndiv, void* ieee, void* ttype, void* dmin1, void* dmin2, void* dn, void* dn1, void* dn2, void* g, void* tau)  __attribute__((alias("flexiblas_chain_dlasq3_")));




