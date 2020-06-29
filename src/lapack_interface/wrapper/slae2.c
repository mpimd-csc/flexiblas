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



static TLS_STORE uint8_t hook_pos_slae2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slae2,SLAE2)(float* a, float* b, float* c, float* rt1, float* rt2)
#else
void FC_GLOBAL(slae2,SLAE2)(float* a, float* b, float* c, float* rt1, float* rt2)
#endif
{
	void (*fn) (void* a, void* b, void* c, void* rt1, void* rt2);
	void (*fn_hook) (void* a, void* b, void* c, void* rt1, void* rt2);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.slae2.f77_blas_function; 
	fn_hook = __flexiblas_hooks->slae2.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2); 
		return;
	} else {
		hook_pos_slae2 = 0;
		fn_hook((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void slae2_(float* a, float* b, float* c, float* rt1, float* rt2) __attribute__((alias(MTS(FC_GLOBAL(slae2,SLAE2)))));
#else
void slae2(float* a, float* b, float* c, float* rt1, float* rt2) __attribute__((alias(MTS(FC_GLOBAL(slae2,SLAE2)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slae2_(void* a, void* b, void* c, void* rt1, void* rt2)
{
	void (*fn) (void* a, void* b, void* c, void* rt1, void* rt2);

	fn = current_backend->lapack.slae2.f77_blas_function; 

		fn((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2); 

	return;
}

void flexiblas_real_slae2(void* a, void* b, void* c, void* rt1, void* rt2)  __attribute__((alias("flexiblas_real_slae2_")));





/* Chainloader for Hooks */


void flexiblas_chain_slae2_(void* a, void* b, void* c, void* rt1, void* rt2)
{
	void (*fn) (void* a, void* b, void* c, void* rt1, void* rt2);
	void (*fn_hook) (void* a, void* b, void* c, void* rt1, void* rt2);

	fn      = current_backend->lapack.slae2.f77_blas_function; 

    hook_pos_slae2 ++;
    if( hook_pos_slae2 < __flexiblas_hooks->slae2.nhook) {
        fn_hook = __flexiblas_hooks->slae2.f77_hook_function[hook_pos_slae2];
        fn_hook((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2);
    } else {
        hook_pos_slae2 = 0;
		fn((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2); 
	}
	return;
}

void flexiblas_chain_slae2(void* a, void* b, void* c, void* rt1, void* rt2)  __attribute__((alias("flexiblas_chain_slae2_")));




