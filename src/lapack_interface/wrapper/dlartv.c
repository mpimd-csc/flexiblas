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



static TLS_STORE uint8_t hook_pos_dlartv = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlartv,DLARTV)(blasint* n, double* x, blasint* incx, double* y, blasint* incy, double* c, double* s, blasint* incc)
#else
void FC_GLOBAL(dlartv,DLARTV)(blasint* n, double* x, blasint* incx, double* y, blasint* incy, double* c, double* s, blasint* incc)
#endif
{
	void (*fn) (void* n, void* x, void* incx, void* y, void* incy, void* c, void* s, void* incc);
	void (*fn_hook) (void* n, void* x, void* incx, void* y, void* incy, void* c, void* s, void* incc);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.dlartv.f77_blas_function; 
	fn_hook = __flexiblas_hooks->dlartv.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) x, (void*) incx, (void*) y, (void*) incy, (void*) c, (void*) s, (void*) incc); 
		return;
	} else {
		hook_pos_dlartv = 0;
		fn_hook((void*) n, (void*) x, (void*) incx, (void*) y, (void*) incy, (void*) c, (void*) s, (void*) incc);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dlartv_(blasint* n, double* x, blasint* incx, double* y, blasint* incy, double* c, double* s, blasint* incc) __attribute__((alias(MTS(FC_GLOBAL(dlartv,DLARTV)))));
#else
void dlartv(blasint* n, double* x, blasint* incx, double* y, blasint* incy, double* c, double* s, blasint* incc) __attribute__((alias(MTS(FC_GLOBAL(dlartv,DLARTV)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlartv_(void* n, void* x, void* incx, void* y, void* incy, void* c, void* s, void* incc)
{
	void (*fn) (void* n, void* x, void* incx, void* y, void* incy, void* c, void* s, void* incc);

	fn = current_backend->lapack.dlartv.f77_blas_function; 

		fn((void*) n, (void*) x, (void*) incx, (void*) y, (void*) incy, (void*) c, (void*) s, (void*) incc); 

	return;
}

void flexiblas_real_dlartv(void* n, void* x, void* incx, void* y, void* incy, void* c, void* s, void* incc)  __attribute__((alias("flexiblas_real_dlartv_")));





/* Chainloader for Hooks */


void flexiblas_chain_dlartv_(void* n, void* x, void* incx, void* y, void* incy, void* c, void* s, void* incc)
{
	void (*fn) (void* n, void* x, void* incx, void* y, void* incy, void* c, void* s, void* incc);
	void (*fn_hook) (void* n, void* x, void* incx, void* y, void* incy, void* c, void* s, void* incc);

	fn      = current_backend->lapack.dlartv.f77_blas_function; 

    hook_pos_dlartv ++;
    if( hook_pos_dlartv < __flexiblas_hooks->dlartv.nhook) {
        fn_hook = __flexiblas_hooks->dlartv.f77_hook_function[hook_pos_dlartv];
        fn_hook((void*) n, (void*) x, (void*) incx, (void*) y, (void*) incy, (void*) c, (void*) s, (void*) incc);
    } else {
        hook_pos_dlartv = 0;
		fn((void*) n, (void*) x, (void*) incx, (void*) y, (void*) incy, (void*) c, (void*) s, (void*) incc); 
	}
	return;
}

void flexiblas_chain_dlartv(void* n, void* x, void* incx, void* y, void* incy, void* c, void* s, void* incc)  __attribute__((alias("flexiblas_chain_dlartv_")));




