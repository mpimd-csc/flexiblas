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



static TLS_STORE uint8_t hook_pos_dlarnv = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlarnv,DLARNV)(blasint* idist, blasint* iseed, blasint* n, double* x)
#else
void FC_GLOBAL(dlarnv,DLARNV)(blasint* idist, blasint* iseed, blasint* n, double* x)
#endif
{
	void (*fn) (void* idist, void* iseed, void* n, void* x);
	void (*fn_hook) (void* idist, void* iseed, void* n, void* x);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.dlarnv.f77_blas_function; 
	fn_hook = __flexiblas_hooks->dlarnv.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) idist, (void*) iseed, (void*) n, (void*) x); 
		return;
	} else {
		hook_pos_dlarnv = 0;
		fn_hook((void*) idist, (void*) iseed, (void*) n, (void*) x);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dlarnv_(blasint* idist, blasint* iseed, blasint* n, double* x) __attribute__((alias(MTS(FC_GLOBAL(dlarnv,DLARNV)))));
#else
void dlarnv(blasint* idist, blasint* iseed, blasint* n, double* x) __attribute__((alias(MTS(FC_GLOBAL(dlarnv,DLARNV)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlarnv_(void* idist, void* iseed, void* n, void* x)
{
	void (*fn) (void* idist, void* iseed, void* n, void* x);

	fn = current_backend->lapack.dlarnv.f77_blas_function; 

		fn((void*) idist, (void*) iseed, (void*) n, (void*) x); 

	return;
}

void flexiblas_real_dlarnv(void* idist, void* iseed, void* n, void* x)  __attribute__((alias("flexiblas_real_dlarnv_")));





/* Chainloader for Hooks */


void flexiblas_chain_dlarnv_(void* idist, void* iseed, void* n, void* x)
{
	void (*fn) (void* idist, void* iseed, void* n, void* x);
	void (*fn_hook) (void* idist, void* iseed, void* n, void* x);

	fn      = current_backend->lapack.dlarnv.f77_blas_function; 

    hook_pos_dlarnv ++;
    if( hook_pos_dlarnv < __flexiblas_hooks->dlarnv.nhook) {
        fn_hook = __flexiblas_hooks->dlarnv.f77_hook_function[hook_pos_dlarnv];
        fn_hook((void*) idist, (void*) iseed, (void*) n, (void*) x);
    } else {
        hook_pos_dlarnv = 0;
		fn((void*) idist, (void*) iseed, (void*) n, (void*) x); 
	}
	return;
}

void flexiblas_chain_dlarnv(void* idist, void* iseed, void* n, void* x)  __attribute__((alias("flexiblas_chain_dlarnv_")));




