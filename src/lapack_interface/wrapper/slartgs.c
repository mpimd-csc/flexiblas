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



static TLS_STORE uint8_t hook_pos_slartgs = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slartgs,SLARTGS)(float* x, float* y, float* sigma, float* cs, float* sn)
#else
void FC_GLOBAL(slartgs,SLARTGS)(float* x, float* y, float* sigma, float* cs, float* sn)
#endif
{
	void (*fn) (void* x, void* y, void* sigma, void* cs, void* sn);
	void (*fn_hook) (void* x, void* y, void* sigma, void* cs, void* sn);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.slartgs.f77_blas_function; 
	fn_hook = __flexiblas_hooks->slartgs.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) x, (void*) y, (void*) sigma, (void*) cs, (void*) sn); 
		return;
	} else {
		hook_pos_slartgs = 0;
		fn_hook((void*) x, (void*) y, (void*) sigma, (void*) cs, (void*) sn);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void slartgs_(float* x, float* y, float* sigma, float* cs, float* sn) __attribute__((alias(MTS(FC_GLOBAL(slartgs,SLARTGS)))));
#else
void slartgs(float* x, float* y, float* sigma, float* cs, float* sn) __attribute__((alias(MTS(FC_GLOBAL(slartgs,SLARTGS)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slartgs_(void* x, void* y, void* sigma, void* cs, void* sn)
{
	void (*fn) (void* x, void* y, void* sigma, void* cs, void* sn);

	fn = current_backend->lapack.slartgs.f77_blas_function; 

		fn((void*) x, (void*) y, (void*) sigma, (void*) cs, (void*) sn); 

	return;
}

void flexiblas_real_slartgs(void* x, void* y, void* sigma, void* cs, void* sn)  __attribute__((alias("flexiblas_real_slartgs_")));





/* Chainloader for Hooks */


void flexiblas_chain_slartgs_(void* x, void* y, void* sigma, void* cs, void* sn)
{
	void (*fn) (void* x, void* y, void* sigma, void* cs, void* sn);
	void (*fn_hook) (void* x, void* y, void* sigma, void* cs, void* sn);

	fn      = current_backend->lapack.slartgs.f77_blas_function; 

    hook_pos_slartgs ++;
    if( hook_pos_slartgs < __flexiblas_hooks->slartgs.nhook) {
        fn_hook = __flexiblas_hooks->slartgs.f77_hook_function[hook_pos_slartgs];
        fn_hook((void*) x, (void*) y, (void*) sigma, (void*) cs, (void*) sn);
    } else {
        hook_pos_slartgs = 0;
		fn((void*) x, (void*) y, (void*) sigma, (void*) cs, (void*) sn); 
	}
	return;
}

void flexiblas_chain_slartgs(void* x, void* y, void* sigma, void* cs, void* sn)  __attribute__((alias("flexiblas_chain_slartgs_")));




