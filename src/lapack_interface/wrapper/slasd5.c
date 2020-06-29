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



static TLS_STORE uint8_t hook_pos_slasd5 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slasd5,SLASD5)(blasint* i, float* d, float* z, float* delta, float* rho, float* dsigma, float* work)
#else
void FC_GLOBAL(slasd5,SLASD5)(blasint* i, float* d, float* z, float* delta, float* rho, float* dsigma, float* work)
#endif
{
	void (*fn) (void* i, void* d, void* z, void* delta, void* rho, void* dsigma, void* work);
	void (*fn_hook) (void* i, void* d, void* z, void* delta, void* rho, void* dsigma, void* work);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.slasd5.f77_blas_function; 
	fn_hook = __flexiblas_hooks->slasd5.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) i, (void*) d, (void*) z, (void*) delta, (void*) rho, (void*) dsigma, (void*) work); 
		return;
	} else {
		hook_pos_slasd5 = 0;
		fn_hook((void*) i, (void*) d, (void*) z, (void*) delta, (void*) rho, (void*) dsigma, (void*) work);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void slasd5_(blasint* i, float* d, float* z, float* delta, float* rho, float* dsigma, float* work) __attribute__((alias(MTS(FC_GLOBAL(slasd5,SLASD5)))));
#else
void slasd5(blasint* i, float* d, float* z, float* delta, float* rho, float* dsigma, float* work) __attribute__((alias(MTS(FC_GLOBAL(slasd5,SLASD5)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slasd5_(void* i, void* d, void* z, void* delta, void* rho, void* dsigma, void* work)
{
	void (*fn) (void* i, void* d, void* z, void* delta, void* rho, void* dsigma, void* work);

	fn = current_backend->lapack.slasd5.f77_blas_function; 

		fn((void*) i, (void*) d, (void*) z, (void*) delta, (void*) rho, (void*) dsigma, (void*) work); 

	return;
}

void flexiblas_real_slasd5(void* i, void* d, void* z, void* delta, void* rho, void* dsigma, void* work)  __attribute__((alias("flexiblas_real_slasd5_")));





/* Chainloader for Hooks */


void flexiblas_chain_slasd5_(void* i, void* d, void* z, void* delta, void* rho, void* dsigma, void* work)
{
	void (*fn) (void* i, void* d, void* z, void* delta, void* rho, void* dsigma, void* work);
	void (*fn_hook) (void* i, void* d, void* z, void* delta, void* rho, void* dsigma, void* work);

	fn      = current_backend->lapack.slasd5.f77_blas_function; 

    hook_pos_slasd5 ++;
    if( hook_pos_slasd5 < __flexiblas_hooks->slasd5.nhook) {
        fn_hook = __flexiblas_hooks->slasd5.f77_hook_function[hook_pos_slasd5];
        fn_hook((void*) i, (void*) d, (void*) z, (void*) delta, (void*) rho, (void*) dsigma, (void*) work);
    } else {
        hook_pos_slasd5 = 0;
		fn((void*) i, (void*) d, (void*) z, (void*) delta, (void*) rho, (void*) dsigma, (void*) work); 
	}
	return;
}

void flexiblas_chain_slasd5(void* i, void* d, void* z, void* delta, void* rho, void* dsigma, void* work)  __attribute__((alias("flexiblas_chain_slasd5_")));




