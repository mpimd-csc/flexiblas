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
 /* Generated: Wed Mar 28 11:20:03 2018 */
        
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



static TLS_STORE uint8_t hook_pos_dlae2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlae2,DLAE2)(double* a, double* b, double* c, double* rt1, double* rt2)
#else
void FC_GLOBAL(dlae2,DLAE2)(double* a, double* b, double* c, double* rt1, double* rt2)
#endif
{
	void (*fn) (void* a, void* b, void* c, void* rt1, void* rt2);
	void (*fn_hook) (void* a, void* b, void* c, void* rt1, void* rt2);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.dlae2.f77_blas_function; 
	fn_hook = __flexiblas_hooks->dlae2.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2); 
		return;
	} else {
		hook_pos_dlae2 = 0;
		fn_hook((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dlae2_(double* a, double* b, double* c, double* rt1, double* rt2) __attribute__((alias(MTS(FC_GLOBAL(dlae2,DLAE2)))));
#else
void dlae2(double* a, double* b, double* c, double* rt1, double* rt2) __attribute__((alias(MTS(FC_GLOBAL(dlae2,DLAE2)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlae2_(void* a, void* b, void* c, void* rt1, void* rt2)
{
	void (*fn) (void* a, void* b, void* c, void* rt1, void* rt2);

	fn = current_backend->lapack.dlae2.f77_blas_function; 

		fn((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2); 

	return;
}

void flexiblas_real_dlae2(void* a, void* b, void* c, void* rt1, void* rt2)  __attribute__((alias("flexiblas_real_dlae2_")));





/* Chainloader for Hooks */


void flexiblas_chain_dlae2_(void* a, void* b, void* c, void* rt1, void* rt2)
{
	void (*fn) (void* a, void* b, void* c, void* rt1, void* rt2);
	void (*fn_hook) (void* a, void* b, void* c, void* rt1, void* rt2);

	fn      = current_backend->lapack.dlae2.f77_blas_function; 

    hook_pos_dlae2 ++;
    if( hook_pos_dlae2 < __flexiblas_hooks->dlae2.nhook) {
        fn_hook = __flexiblas_hooks->dlae2.f77_hook_function[hook_pos_dlae2];
        fn_hook((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2);
    } else {
        hook_pos_dlae2 = 0;
		fn((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2); 
	}
	return;
}

void flexiblas_chain_dlae2(void* a, void* b, void* c, void* rt1, void* rt2)  __attribute__((alias("flexiblas_chain_dlae2_")));




