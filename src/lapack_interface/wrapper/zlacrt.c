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



static TLS_STORE uint8_t hook_pos_zlacrt = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zlacrt,ZLACRT)(blasint* n, double complex* cx, blasint* incx, double complex* cy, blasint* incy, double complex* c, double complex* s)
#else
void FC_GLOBAL(zlacrt,ZLACRT)(blasint* n, double complex* cx, blasint* incx, double complex* cy, blasint* incy, double complex* c, double complex* s)
#endif
{
	void (*fn) (void* n, void* cx, void* incx, void* cy, void* incy, void* c, void* s);
	void (*fn_hook) (void* n, void* cx, void* incx, void* cy, void* incy, void* c, void* s);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.zlacrt.f77_blas_function; 
	fn_hook = __flexiblas_hooks->zlacrt.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) cx, (void*) incx, (void*) cy, (void*) incy, (void*) c, (void*) s); 
		return;
	} else {
		hook_pos_zlacrt = 0;
		fn_hook((void*) n, (void*) cx, (void*) incx, (void*) cy, (void*) incy, (void*) c, (void*) s);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void zlacrt_(blasint* n, double complex* cx, blasint* incx, double complex* cy, blasint* incy, double complex* c, double complex* s) __attribute__((alias(MTS(FC_GLOBAL(zlacrt,ZLACRT)))));
#else
void zlacrt(blasint* n, double complex* cx, blasint* incx, double complex* cy, blasint* incy, double complex* c, double complex* s) __attribute__((alias(MTS(FC_GLOBAL(zlacrt,ZLACRT)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zlacrt_(void* n, void* cx, void* incx, void* cy, void* incy, void* c, void* s)
{
	void (*fn) (void* n, void* cx, void* incx, void* cy, void* incy, void* c, void* s);

	fn = current_backend->lapack.zlacrt.f77_blas_function; 

		fn((void*) n, (void*) cx, (void*) incx, (void*) cy, (void*) incy, (void*) c, (void*) s); 

	return;
}

void flexiblas_real_zlacrt(void* n, void* cx, void* incx, void* cy, void* incy, void* c, void* s)  __attribute__((alias("flexiblas_real_zlacrt_")));





/* Chainloader for Hooks */


void flexiblas_chain_zlacrt_(void* n, void* cx, void* incx, void* cy, void* incy, void* c, void* s)
{
	void (*fn) (void* n, void* cx, void* incx, void* cy, void* incy, void* c, void* s);
	void (*fn_hook) (void* n, void* cx, void* incx, void* cy, void* incy, void* c, void* s);

	fn      = current_backend->lapack.zlacrt.f77_blas_function; 

    hook_pos_zlacrt ++;
    if( hook_pos_zlacrt < __flexiblas_hooks->zlacrt.nhook) {
        fn_hook = __flexiblas_hooks->zlacrt.f77_hook_function[hook_pos_zlacrt];
        fn_hook((void*) n, (void*) cx, (void*) incx, (void*) cy, (void*) incy, (void*) c, (void*) s);
    } else {
        hook_pos_zlacrt = 0;
		fn((void*) n, (void*) cx, (void*) incx, (void*) cy, (void*) incy, (void*) c, (void*) s); 
	}
	return;
}

void flexiblas_chain_zlacrt(void* n, void* cx, void* incx, void* cy, void* incy, void* c, void* s)  __attribute__((alias("flexiblas_chain_zlacrt_")));




