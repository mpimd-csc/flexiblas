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



static TLS_STORE uint8_t hook_pos_zgttrf = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zgttrf,ZGTTRF)(blasint* n, double complex* dl, double complex* d, double complex* du, double complex* du2, blasint* ipiv, blasint* info)
#else
void FC_GLOBAL(zgttrf,ZGTTRF)(blasint* n, double complex* dl, double complex* d, double complex* du, double complex* du2, blasint* ipiv, blasint* info)
#endif
{
	void (*fn) (void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* info);
	void (*fn_hook) (void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.zgttrf.f77_blas_function; 
	fn_hook = __flexiblas_hooks->zgttrf.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) info); 
		return;
	} else {
		hook_pos_zgttrf = 0;
		fn_hook((void*) n, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void zgttrf_(blasint* n, double complex* dl, double complex* d, double complex* du, double complex* du2, blasint* ipiv, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(zgttrf,ZGTTRF)))));
#else
void zgttrf(blasint* n, double complex* dl, double complex* d, double complex* du, double complex* du2, blasint* ipiv, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(zgttrf,ZGTTRF)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zgttrf_(void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* info)
{
	void (*fn) (void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* info);

	fn = current_backend->lapack.zgttrf.f77_blas_function; 

		fn((void*) n, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) info); 

	return;
}

void flexiblas_real_zgttrf(void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* info)  __attribute__((alias("flexiblas_real_zgttrf_")));





/* Chainloader for Hooks */


void flexiblas_chain_zgttrf_(void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* info)
{
	void (*fn) (void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* info);
	void (*fn_hook) (void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* info);

	fn      = current_backend->lapack.zgttrf.f77_blas_function; 

    hook_pos_zgttrf ++;
    if( hook_pos_zgttrf < __flexiblas_hooks->zgttrf.nhook) {
        fn_hook = __flexiblas_hooks->zgttrf.f77_hook_function[hook_pos_zgttrf];
        fn_hook((void*) n, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) info);
    } else {
        hook_pos_zgttrf = 0;
		fn((void*) n, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) info); 
	}
	return;
}

void flexiblas_chain_zgttrf(void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* info)  __attribute__((alias("flexiblas_chain_zgttrf_")));




