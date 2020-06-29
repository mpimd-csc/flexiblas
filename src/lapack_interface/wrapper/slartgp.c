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



static TLS_STORE uint8_t hook_pos_slartgp = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slartgp,SLARTGP)(float* f, float* g, float* cs, float* sn, float* r)
#else
void FC_GLOBAL(slartgp,SLARTGP)(float* f, float* g, float* cs, float* sn, float* r)
#endif
{
	void (*fn) (void* f, void* g, void* cs, void* sn, void* r);
	void (*fn_hook) (void* f, void* g, void* cs, void* sn, void* r);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.slartgp.f77_blas_function; 
	fn_hook = __flexiblas_hooks->slartgp.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) f, (void*) g, (void*) cs, (void*) sn, (void*) r); 
		return;
	} else {
		hook_pos_slartgp = 0;
		fn_hook((void*) f, (void*) g, (void*) cs, (void*) sn, (void*) r);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void slartgp_(float* f, float* g, float* cs, float* sn, float* r) __attribute__((alias(MTS(FC_GLOBAL(slartgp,SLARTGP)))));
#else
void slartgp(float* f, float* g, float* cs, float* sn, float* r) __attribute__((alias(MTS(FC_GLOBAL(slartgp,SLARTGP)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slartgp_(void* f, void* g, void* cs, void* sn, void* r)
{
	void (*fn) (void* f, void* g, void* cs, void* sn, void* r);

	fn = current_backend->lapack.slartgp.f77_blas_function; 

		fn((void*) f, (void*) g, (void*) cs, (void*) sn, (void*) r); 

	return;
}

void flexiblas_real_slartgp(void* f, void* g, void* cs, void* sn, void* r)  __attribute__((alias("flexiblas_real_slartgp_")));





/* Chainloader for Hooks */


void flexiblas_chain_slartgp_(void* f, void* g, void* cs, void* sn, void* r)
{
	void (*fn) (void* f, void* g, void* cs, void* sn, void* r);
	void (*fn_hook) (void* f, void* g, void* cs, void* sn, void* r);

	fn      = current_backend->lapack.slartgp.f77_blas_function; 

    hook_pos_slartgp ++;
    if( hook_pos_slartgp < __flexiblas_hooks->slartgp.nhook) {
        fn_hook = __flexiblas_hooks->slartgp.f77_hook_function[hook_pos_slartgp];
        fn_hook((void*) f, (void*) g, (void*) cs, (void*) sn, (void*) r);
    } else {
        hook_pos_slartgp = 0;
		fn((void*) f, (void*) g, (void*) cs, (void*) sn, (void*) r); 
	}
	return;
}

void flexiblas_chain_slartgp(void* f, void* g, void* cs, void* sn, void* r)  __attribute__((alias("flexiblas_chain_slartgp_")));




