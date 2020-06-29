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



static TLS_STORE uint8_t hook_pos_zlacn2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zlacn2,ZLACN2)(blasint* n, double complex* v, double complex* x, double* est, blasint* kase, blasint* isave)
#else
void FC_GLOBAL(zlacn2,ZLACN2)(blasint* n, double complex* v, double complex* x, double* est, blasint* kase, blasint* isave)
#endif
{
	void (*fn) (void* n, void* v, void* x, void* est, void* kase, void* isave);
	void (*fn_hook) (void* n, void* v, void* x, void* est, void* kase, void* isave);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.zlacn2.f77_blas_function; 
	fn_hook = __flexiblas_hooks->zlacn2.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) v, (void*) x, (void*) est, (void*) kase, (void*) isave); 
		return;
	} else {
		hook_pos_zlacn2 = 0;
		fn_hook((void*) n, (void*) v, (void*) x, (void*) est, (void*) kase, (void*) isave);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void zlacn2_(blasint* n, double complex* v, double complex* x, double* est, blasint* kase, blasint* isave) __attribute__((alias(MTS(FC_GLOBAL(zlacn2,ZLACN2)))));
#else
void zlacn2(blasint* n, double complex* v, double complex* x, double* est, blasint* kase, blasint* isave) __attribute__((alias(MTS(FC_GLOBAL(zlacn2,ZLACN2)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zlacn2_(void* n, void* v, void* x, void* est, void* kase, void* isave)
{
	void (*fn) (void* n, void* v, void* x, void* est, void* kase, void* isave);

	fn = current_backend->lapack.zlacn2.f77_blas_function; 

		fn((void*) n, (void*) v, (void*) x, (void*) est, (void*) kase, (void*) isave); 

	return;
}

void flexiblas_real_zlacn2(void* n, void* v, void* x, void* est, void* kase, void* isave)  __attribute__((alias("flexiblas_real_zlacn2_")));





/* Chainloader for Hooks */


void flexiblas_chain_zlacn2_(void* n, void* v, void* x, void* est, void* kase, void* isave)
{
	void (*fn) (void* n, void* v, void* x, void* est, void* kase, void* isave);
	void (*fn_hook) (void* n, void* v, void* x, void* est, void* kase, void* isave);

	fn      = current_backend->lapack.zlacn2.f77_blas_function; 

    hook_pos_zlacn2 ++;
    if( hook_pos_zlacn2 < __flexiblas_hooks->zlacn2.nhook) {
        fn_hook = __flexiblas_hooks->zlacn2.f77_hook_function[hook_pos_zlacn2];
        fn_hook((void*) n, (void*) v, (void*) x, (void*) est, (void*) kase, (void*) isave);
    } else {
        hook_pos_zlacn2 = 0;
		fn((void*) n, (void*) v, (void*) x, (void*) est, (void*) kase, (void*) isave); 
	}
	return;
}

void flexiblas_chain_zlacn2(void* n, void* v, void* x, void* est, void* kase, void* isave)  __attribute__((alias("flexiblas_chain_zlacn2_")));




