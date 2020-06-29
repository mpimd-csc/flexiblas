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



static TLS_STORE uint8_t hook_pos_slacon = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slacon,SLACON)(blasint* n, float* v, float* x, blasint* isgn, float* est, blasint* kase)
#else
void FC_GLOBAL(slacon,SLACON)(blasint* n, float* v, float* x, blasint* isgn, float* est, blasint* kase)
#endif
{
	void (*fn) (void* n, void* v, void* x, void* isgn, void* est, void* kase);
	void (*fn_hook) (void* n, void* v, void* x, void* isgn, void* est, void* kase);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.slacon.f77_blas_function; 
	fn_hook = __flexiblas_hooks->slacon.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) v, (void*) x, (void*) isgn, (void*) est, (void*) kase); 
		return;
	} else {
		hook_pos_slacon = 0;
		fn_hook((void*) n, (void*) v, (void*) x, (void*) isgn, (void*) est, (void*) kase);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void slacon_(blasint* n, float* v, float* x, blasint* isgn, float* est, blasint* kase) __attribute__((alias(MTS(FC_GLOBAL(slacon,SLACON)))));
#else
void slacon(blasint* n, float* v, float* x, blasint* isgn, float* est, blasint* kase) __attribute__((alias(MTS(FC_GLOBAL(slacon,SLACON)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slacon_(void* n, void* v, void* x, void* isgn, void* est, void* kase)
{
	void (*fn) (void* n, void* v, void* x, void* isgn, void* est, void* kase);

	fn = current_backend->lapack.slacon.f77_blas_function; 

		fn((void*) n, (void*) v, (void*) x, (void*) isgn, (void*) est, (void*) kase); 

	return;
}

void flexiblas_real_slacon(void* n, void* v, void* x, void* isgn, void* est, void* kase)  __attribute__((alias("flexiblas_real_slacon_")));





/* Chainloader for Hooks */


void flexiblas_chain_slacon_(void* n, void* v, void* x, void* isgn, void* est, void* kase)
{
	void (*fn) (void* n, void* v, void* x, void* isgn, void* est, void* kase);
	void (*fn_hook) (void* n, void* v, void* x, void* isgn, void* est, void* kase);

	fn      = current_backend->lapack.slacon.f77_blas_function; 

    hook_pos_slacon ++;
    if( hook_pos_slacon < __flexiblas_hooks->slacon.nhook) {
        fn_hook = __flexiblas_hooks->slacon.f77_hook_function[hook_pos_slacon];
        fn_hook((void*) n, (void*) v, (void*) x, (void*) isgn, (void*) est, (void*) kase);
    } else {
        hook_pos_slacon = 0;
		fn((void*) n, (void*) v, (void*) x, (void*) isgn, (void*) est, (void*) kase); 
	}
	return;
}

void flexiblas_chain_slacon(void* n, void* v, void* x, void* isgn, void* est, void* kase)  __attribute__((alias("flexiblas_chain_slacon_")));




