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



static TLS_STORE uint8_t hook_pos_slaruv = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slaruv,SLARUV)(blasint* iseed, blasint* n, float* x)
#else
void FC_GLOBAL(slaruv,SLARUV)(blasint* iseed, blasint* n, float* x)
#endif
{
	void (*fn) (void* iseed, void* n, void* x);
	void (*fn_hook) (void* iseed, void* n, void* x);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.slaruv.f77_blas_function; 
	fn_hook = __flexiblas_hooks->slaruv.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) iseed, (void*) n, (void*) x); 
		return;
	} else {
		hook_pos_slaruv = 0;
		fn_hook((void*) iseed, (void*) n, (void*) x);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void slaruv_(blasint* iseed, blasint* n, float* x) __attribute__((alias(MTS(FC_GLOBAL(slaruv,SLARUV)))));
#else
void slaruv(blasint* iseed, blasint* n, float* x) __attribute__((alias(MTS(FC_GLOBAL(slaruv,SLARUV)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slaruv_(void* iseed, void* n, void* x)
{
	void (*fn) (void* iseed, void* n, void* x);

	fn = current_backend->lapack.slaruv.f77_blas_function; 

		fn((void*) iseed, (void*) n, (void*) x); 

	return;
}

void flexiblas_real_slaruv(void* iseed, void* n, void* x)  __attribute__((alias("flexiblas_real_slaruv_")));





/* Chainloader for Hooks */


void flexiblas_chain_slaruv_(void* iseed, void* n, void* x)
{
	void (*fn) (void* iseed, void* n, void* x);
	void (*fn_hook) (void* iseed, void* n, void* x);

	fn      = current_backend->lapack.slaruv.f77_blas_function; 

    hook_pos_slaruv ++;
    if( hook_pos_slaruv < __flexiblas_hooks->slaruv.nhook) {
        fn_hook = __flexiblas_hooks->slaruv.f77_hook_function[hook_pos_slaruv];
        fn_hook((void*) iseed, (void*) n, (void*) x);
    } else {
        hook_pos_slaruv = 0;
		fn((void*) iseed, (void*) n, (void*) x); 
	}
	return;
}

void flexiblas_chain_slaruv(void* iseed, void* n, void* x)  __attribute__((alias("flexiblas_chain_slaruv_")));




