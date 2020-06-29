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



static TLS_STORE uint8_t hook_pos_slaed5 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slaed5,SLAED5)(blasint* i, float* d, float* z, float* delta, float* rho, float* dlam)
#else
void FC_GLOBAL(slaed5,SLAED5)(blasint* i, float* d, float* z, float* delta, float* rho, float* dlam)
#endif
{
	void (*fn) (void* i, void* d, void* z, void* delta, void* rho, void* dlam);
	void (*fn_hook) (void* i, void* d, void* z, void* delta, void* rho, void* dlam);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.slaed5.f77_blas_function; 
	fn_hook = __flexiblas_hooks->slaed5.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) i, (void*) d, (void*) z, (void*) delta, (void*) rho, (void*) dlam); 
		return;
	} else {
		hook_pos_slaed5 = 0;
		fn_hook((void*) i, (void*) d, (void*) z, (void*) delta, (void*) rho, (void*) dlam);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void slaed5_(blasint* i, float* d, float* z, float* delta, float* rho, float* dlam) __attribute__((alias(MTS(FC_GLOBAL(slaed5,SLAED5)))));
#else
void slaed5(blasint* i, float* d, float* z, float* delta, float* rho, float* dlam) __attribute__((alias(MTS(FC_GLOBAL(slaed5,SLAED5)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slaed5_(void* i, void* d, void* z, void* delta, void* rho, void* dlam)
{
	void (*fn) (void* i, void* d, void* z, void* delta, void* rho, void* dlam);

	fn = current_backend->lapack.slaed5.f77_blas_function; 

		fn((void*) i, (void*) d, (void*) z, (void*) delta, (void*) rho, (void*) dlam); 

	return;
}

void flexiblas_real_slaed5(void* i, void* d, void* z, void* delta, void* rho, void* dlam)  __attribute__((alias("flexiblas_real_slaed5_")));





/* Chainloader for Hooks */


void flexiblas_chain_slaed5_(void* i, void* d, void* z, void* delta, void* rho, void* dlam)
{
	void (*fn) (void* i, void* d, void* z, void* delta, void* rho, void* dlam);
	void (*fn_hook) (void* i, void* d, void* z, void* delta, void* rho, void* dlam);

	fn      = current_backend->lapack.slaed5.f77_blas_function; 

    hook_pos_slaed5 ++;
    if( hook_pos_slaed5 < __flexiblas_hooks->slaed5.nhook) {
        fn_hook = __flexiblas_hooks->slaed5.f77_hook_function[hook_pos_slaed5];
        fn_hook((void*) i, (void*) d, (void*) z, (void*) delta, (void*) rho, (void*) dlam);
    } else {
        hook_pos_slaed5 = 0;
		fn((void*) i, (void*) d, (void*) z, (void*) delta, (void*) rho, (void*) dlam); 
	}
	return;
}

void flexiblas_chain_slaed5(void* i, void* d, void* z, void* delta, void* rho, void* dlam)  __attribute__((alias("flexiblas_chain_slaed5_")));




