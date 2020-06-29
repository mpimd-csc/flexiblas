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



static TLS_STORE uint8_t hook_pos_clapll = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(clapll,CLAPLL)(blasint* n, float complex* x, blasint* incx, float complex* y, blasint* incy, float* ssmin)
#else
void FC_GLOBAL(clapll,CLAPLL)(blasint* n, float complex* x, blasint* incx, float complex* y, blasint* incy, float* ssmin)
#endif
{
	void (*fn) (void* n, void* x, void* incx, void* y, void* incy, void* ssmin);
	void (*fn_hook) (void* n, void* x, void* incx, void* y, void* incy, void* ssmin);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.clapll.f77_blas_function; 
	fn_hook = __flexiblas_hooks->clapll.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) x, (void*) incx, (void*) y, (void*) incy, (void*) ssmin); 
		return;
	} else {
		hook_pos_clapll = 0;
		fn_hook((void*) n, (void*) x, (void*) incx, (void*) y, (void*) incy, (void*) ssmin);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void clapll_(blasint* n, float complex* x, blasint* incx, float complex* y, blasint* incy, float* ssmin) __attribute__((alias(MTS(FC_GLOBAL(clapll,CLAPLL)))));
#else
void clapll(blasint* n, float complex* x, blasint* incx, float complex* y, blasint* incy, float* ssmin) __attribute__((alias(MTS(FC_GLOBAL(clapll,CLAPLL)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_clapll_(void* n, void* x, void* incx, void* y, void* incy, void* ssmin)
{
	void (*fn) (void* n, void* x, void* incx, void* y, void* incy, void* ssmin);

	fn = current_backend->lapack.clapll.f77_blas_function; 

		fn((void*) n, (void*) x, (void*) incx, (void*) y, (void*) incy, (void*) ssmin); 

	return;
}

void flexiblas_real_clapll(void* n, void* x, void* incx, void* y, void* incy, void* ssmin)  __attribute__((alias("flexiblas_real_clapll_")));





/* Chainloader for Hooks */


void flexiblas_chain_clapll_(void* n, void* x, void* incx, void* y, void* incy, void* ssmin)
{
	void (*fn) (void* n, void* x, void* incx, void* y, void* incy, void* ssmin);
	void (*fn_hook) (void* n, void* x, void* incx, void* y, void* incy, void* ssmin);

	fn      = current_backend->lapack.clapll.f77_blas_function; 

    hook_pos_clapll ++;
    if( hook_pos_clapll < __flexiblas_hooks->clapll.nhook) {
        fn_hook = __flexiblas_hooks->clapll.f77_hook_function[hook_pos_clapll];
        fn_hook((void*) n, (void*) x, (void*) incx, (void*) y, (void*) incy, (void*) ssmin);
    } else {
        hook_pos_clapll = 0;
		fn((void*) n, (void*) x, (void*) incx, (void*) y, (void*) incy, (void*) ssmin); 
	}
	return;
}

void flexiblas_chain_clapll(void* n, void* x, void* incx, void* y, void* incy, void* ssmin)  __attribute__((alias("flexiblas_chain_clapll_")));




