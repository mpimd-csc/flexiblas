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



static TLS_STORE uint8_t hook_pos_dlasq5 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlasq5,DLASQ5)(blasint* i0, blasint* n0, double* z, blasint* pp, double* tau, double* sigma, double* dmin, double* dmin1, double* dmin2, double* dn, double* dnm1, double* dnm2, blasint* ieee, double* eps)
#else
void FC_GLOBAL(dlasq5,DLASQ5)(blasint* i0, blasint* n0, double* z, blasint* pp, double* tau, double* sigma, double* dmin, double* dmin1, double* dmin2, double* dn, double* dnm1, double* dnm2, blasint* ieee, double* eps)
#endif
{
	void (*fn) (void* i0, void* n0, void* z, void* pp, void* tau, void* sigma, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2, void* ieee, void* eps);
	void (*fn_hook) (void* i0, void* n0, void* z, void* pp, void* tau, void* sigma, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2, void* ieee, void* eps);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.dlasq5.f77_blas_function; 
	fn_hook = __flexiblas_hooks->dlasq5.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) tau, (void*) sigma, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dnm1, (void*) dnm2, (void*) ieee, (void*) eps); 
		return;
	} else {
		hook_pos_dlasq5 = 0;
		fn_hook((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) tau, (void*) sigma, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dnm1, (void*) dnm2, (void*) ieee, (void*) eps);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dlasq5_(blasint* i0, blasint* n0, double* z, blasint* pp, double* tau, double* sigma, double* dmin, double* dmin1, double* dmin2, double* dn, double* dnm1, double* dnm2, blasint* ieee, double* eps) __attribute__((alias(MTS(FC_GLOBAL(dlasq5,DLASQ5)))));
#else
void dlasq5(blasint* i0, blasint* n0, double* z, blasint* pp, double* tau, double* sigma, double* dmin, double* dmin1, double* dmin2, double* dn, double* dnm1, double* dnm2, blasint* ieee, double* eps) __attribute__((alias(MTS(FC_GLOBAL(dlasq5,DLASQ5)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlasq5_(void* i0, void* n0, void* z, void* pp, void* tau, void* sigma, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2, void* ieee, void* eps)
{
	void (*fn) (void* i0, void* n0, void* z, void* pp, void* tau, void* sigma, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2, void* ieee, void* eps);

	fn = current_backend->lapack.dlasq5.f77_blas_function; 

		fn((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) tau, (void*) sigma, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dnm1, (void*) dnm2, (void*) ieee, (void*) eps); 

	return;
}

void flexiblas_real_dlasq5(void* i0, void* n0, void* z, void* pp, void* tau, void* sigma, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2, void* ieee, void* eps)  __attribute__((alias("flexiblas_real_dlasq5_")));





/* Chainloader for Hooks */


void flexiblas_chain_dlasq5_(void* i0, void* n0, void* z, void* pp, void* tau, void* sigma, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2, void* ieee, void* eps)
{
	void (*fn) (void* i0, void* n0, void* z, void* pp, void* tau, void* sigma, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2, void* ieee, void* eps);
	void (*fn_hook) (void* i0, void* n0, void* z, void* pp, void* tau, void* sigma, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2, void* ieee, void* eps);

	fn      = current_backend->lapack.dlasq5.f77_blas_function; 

    hook_pos_dlasq5 ++;
    if( hook_pos_dlasq5 < __flexiblas_hooks->dlasq5.nhook) {
        fn_hook = __flexiblas_hooks->dlasq5.f77_hook_function[hook_pos_dlasq5];
        fn_hook((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) tau, (void*) sigma, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dnm1, (void*) dnm2, (void*) ieee, (void*) eps);
    } else {
        hook_pos_dlasq5 = 0;
		fn((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) tau, (void*) sigma, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dnm1, (void*) dnm2, (void*) ieee, (void*) eps); 
	}
	return;
}

void flexiblas_chain_dlasq5(void* i0, void* n0, void* z, void* pp, void* tau, void* sigma, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2, void* ieee, void* eps)  __attribute__((alias("flexiblas_chain_dlasq5_")));




