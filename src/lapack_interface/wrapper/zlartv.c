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



static TLS_STORE uint8_t hook_pos_zlartv = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zlartv,ZLARTV)(blasint* n, double complex* x, blasint* incx, double complex* y, blasint* incy, double* c, double complex* s, blasint* incc)
#else
void FC_GLOBAL(zlartv,ZLARTV)(blasint* n, double complex* x, blasint* incx, double complex* y, blasint* incy, double* c, double complex* s, blasint* incc)
#endif
{
	void (*fn) (void* n, void* x, void* incx, void* y, void* incy, void* c, void* s, void* incc);
	void (*fn_hook) (void* n, void* x, void* incx, void* y, void* incy, void* c, void* s, void* incc);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.zlartv.f77_blas_function; 
	fn_hook = __flexiblas_hooks->zlartv.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) x, (void*) incx, (void*) y, (void*) incy, (void*) c, (void*) s, (void*) incc); 
		return;
	} else {
		hook_pos_zlartv = 0;
		fn_hook((void*) n, (void*) x, (void*) incx, (void*) y, (void*) incy, (void*) c, (void*) s, (void*) incc);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void zlartv_(blasint* n, double complex* x, blasint* incx, double complex* y, blasint* incy, double* c, double complex* s, blasint* incc) __attribute__((alias(MTS(FC_GLOBAL(zlartv,ZLARTV)))));
#else
void zlartv(blasint* n, double complex* x, blasint* incx, double complex* y, blasint* incy, double* c, double complex* s, blasint* incc) __attribute__((alias(MTS(FC_GLOBAL(zlartv,ZLARTV)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zlartv_(void* n, void* x, void* incx, void* y, void* incy, void* c, void* s, void* incc)
{
	void (*fn) (void* n, void* x, void* incx, void* y, void* incy, void* c, void* s, void* incc);

	fn = current_backend->lapack.zlartv.f77_blas_function; 

		fn((void*) n, (void*) x, (void*) incx, (void*) y, (void*) incy, (void*) c, (void*) s, (void*) incc); 

	return;
}

void flexiblas_real_zlartv(void* n, void* x, void* incx, void* y, void* incy, void* c, void* s, void* incc)  __attribute__((alias("flexiblas_real_zlartv_")));





/* Chainloader for Hooks */


void flexiblas_chain_zlartv_(void* n, void* x, void* incx, void* y, void* incy, void* c, void* s, void* incc)
{
	void (*fn) (void* n, void* x, void* incx, void* y, void* incy, void* c, void* s, void* incc);
	void (*fn_hook) (void* n, void* x, void* incx, void* y, void* incy, void* c, void* s, void* incc);

	fn      = current_backend->lapack.zlartv.f77_blas_function; 

    hook_pos_zlartv ++;
    if( hook_pos_zlartv < __flexiblas_hooks->zlartv.nhook) {
        fn_hook = __flexiblas_hooks->zlartv.f77_hook_function[hook_pos_zlartv];
        fn_hook((void*) n, (void*) x, (void*) incx, (void*) y, (void*) incy, (void*) c, (void*) s, (void*) incc);
    } else {
        hook_pos_zlartv = 0;
		fn((void*) n, (void*) x, (void*) incx, (void*) y, (void*) incy, (void*) c, (void*) s, (void*) incc); 
	}
	return;
}

void flexiblas_chain_zlartv(void* n, void* x, void* incx, void* y, void* incy, void* c, void* s, void* incc)  __attribute__((alias("flexiblas_chain_zlartv_")));




