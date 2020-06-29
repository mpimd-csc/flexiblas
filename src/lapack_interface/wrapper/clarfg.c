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



static TLS_STORE uint8_t hook_pos_clarfg = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(clarfg,CLARFG)(blasint* n, float complex* alpha, float complex* x, blasint* incx, float complex* tau)
#else
void FC_GLOBAL(clarfg,CLARFG)(blasint* n, float complex* alpha, float complex* x, blasint* incx, float complex* tau)
#endif
{
	void (*fn) (void* n, void* alpha, void* x, void* incx, void* tau);
	void (*fn_hook) (void* n, void* alpha, void* x, void* incx, void* tau);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.clarfg.f77_blas_function; 
	fn_hook = __flexiblas_hooks->clarfg.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) tau); 
		return;
	} else {
		hook_pos_clarfg = 0;
		fn_hook((void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) tau);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void clarfg_(blasint* n, float complex* alpha, float complex* x, blasint* incx, float complex* tau) __attribute__((alias(MTS(FC_GLOBAL(clarfg,CLARFG)))));
#else
void clarfg(blasint* n, float complex* alpha, float complex* x, blasint* incx, float complex* tau) __attribute__((alias(MTS(FC_GLOBAL(clarfg,CLARFG)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_clarfg_(void* n, void* alpha, void* x, void* incx, void* tau)
{
	void (*fn) (void* n, void* alpha, void* x, void* incx, void* tau);

	fn = current_backend->lapack.clarfg.f77_blas_function; 

		fn((void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) tau); 

	return;
}

void flexiblas_real_clarfg(void* n, void* alpha, void* x, void* incx, void* tau)  __attribute__((alias("flexiblas_real_clarfg_")));





/* Chainloader for Hooks */


void flexiblas_chain_clarfg_(void* n, void* alpha, void* x, void* incx, void* tau)
{
	void (*fn) (void* n, void* alpha, void* x, void* incx, void* tau);
	void (*fn_hook) (void* n, void* alpha, void* x, void* incx, void* tau);

	fn      = current_backend->lapack.clarfg.f77_blas_function; 

    hook_pos_clarfg ++;
    if( hook_pos_clarfg < __flexiblas_hooks->clarfg.nhook) {
        fn_hook = __flexiblas_hooks->clarfg.f77_hook_function[hook_pos_clarfg];
        fn_hook((void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) tau);
    } else {
        hook_pos_clarfg = 0;
		fn((void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) tau); 
	}
	return;
}

void flexiblas_chain_clarfg(void* n, void* alpha, void* x, void* incx, void* tau)  __attribute__((alias("flexiblas_chain_clarfg_")));




