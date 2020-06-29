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



static TLS_STORE uint8_t hook_pos_slasq2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slasq2,SLASQ2)(blasint* n, float* z, blasint* info)
#else
void FC_GLOBAL(slasq2,SLASQ2)(blasint* n, float* z, blasint* info)
#endif
{
	void (*fn) (void* n, void* z, void* info);
	void (*fn_hook) (void* n, void* z, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.slasq2.f77_blas_function; 
	fn_hook = __flexiblas_hooks->slasq2.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) z, (void*) info); 
		return;
	} else {
		hook_pos_slasq2 = 0;
		fn_hook((void*) n, (void*) z, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void slasq2_(blasint* n, float* z, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slasq2,SLASQ2)))));
#else
void slasq2(blasint* n, float* z, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slasq2,SLASQ2)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slasq2_(void* n, void* z, void* info)
{
	void (*fn) (void* n, void* z, void* info);

	fn = current_backend->lapack.slasq2.f77_blas_function; 

		fn((void*) n, (void*) z, (void*) info); 

	return;
}

void flexiblas_real_slasq2(void* n, void* z, void* info)  __attribute__((alias("flexiblas_real_slasq2_")));





/* Chainloader for Hooks */


void flexiblas_chain_slasq2_(void* n, void* z, void* info)
{
	void (*fn) (void* n, void* z, void* info);
	void (*fn_hook) (void* n, void* z, void* info);

	fn      = current_backend->lapack.slasq2.f77_blas_function; 

    hook_pos_slasq2 ++;
    if( hook_pos_slasq2 < __flexiblas_hooks->slasq2.nhook) {
        fn_hook = __flexiblas_hooks->slasq2.f77_hook_function[hook_pos_slasq2];
        fn_hook((void*) n, (void*) z, (void*) info);
    } else {
        hook_pos_slasq2 = 0;
		fn((void*) n, (void*) z, (void*) info); 
	}
	return;
}

void flexiblas_chain_slasq2(void* n, void* z, void* info)  __attribute__((alias("flexiblas_chain_slasq2_")));




