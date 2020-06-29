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
 /* Generated: Wed Mar 28 11:20:05 2018 */
        
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



static TLS_STORE uint8_t hook_pos_zpttrf = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zpttrf,ZPTTRF)(blasint* n, double* d, double complex* e, blasint* info)
#else
void FC_GLOBAL(zpttrf,ZPTTRF)(blasint* n, double* d, double complex* e, blasint* info)
#endif
{
	void (*fn) (void* n, void* d, void* e, void* info);
	void (*fn_hook) (void* n, void* d, void* e, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.zpttrf.f77_blas_function; 
	fn_hook = __flexiblas_hooks->zpttrf.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) d, (void*) e, (void*) info); 
		return;
	} else {
		hook_pos_zpttrf = 0;
		fn_hook((void*) n, (void*) d, (void*) e, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void zpttrf_(blasint* n, double* d, double complex* e, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(zpttrf,ZPTTRF)))));
#else
void zpttrf(blasint* n, double* d, double complex* e, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(zpttrf,ZPTTRF)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zpttrf_(void* n, void* d, void* e, void* info)
{
	void (*fn) (void* n, void* d, void* e, void* info);

	fn = current_backend->lapack.zpttrf.f77_blas_function; 

		fn((void*) n, (void*) d, (void*) e, (void*) info); 

	return;
}

void flexiblas_real_zpttrf(void* n, void* d, void* e, void* info)  __attribute__((alias("flexiblas_real_zpttrf_")));





/* Chainloader for Hooks */


void flexiblas_chain_zpttrf_(void* n, void* d, void* e, void* info)
{
	void (*fn) (void* n, void* d, void* e, void* info);
	void (*fn_hook) (void* n, void* d, void* e, void* info);

	fn      = current_backend->lapack.zpttrf.f77_blas_function; 

    hook_pos_zpttrf ++;
    if( hook_pos_zpttrf < __flexiblas_hooks->zpttrf.nhook) {
        fn_hook = __flexiblas_hooks->zpttrf.f77_hook_function[hook_pos_zpttrf];
        fn_hook((void*) n, (void*) d, (void*) e, (void*) info);
    } else {
        hook_pos_zpttrf = 0;
		fn((void*) n, (void*) d, (void*) e, (void*) info); 
	}
	return;
}

void flexiblas_chain_zpttrf(void* n, void* d, void* e, void* info)  __attribute__((alias("flexiblas_chain_zpttrf_")));




