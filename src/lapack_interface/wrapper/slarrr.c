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



static TLS_STORE uint8_t hook_pos_slarrr = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slarrr,SLARRR)(blasint* n, float* d, float* e, blasint* info)
#else
void FC_GLOBAL(slarrr,SLARRR)(blasint* n, float* d, float* e, blasint* info)
#endif
{
	void (*fn) (void* n, void* d, void* e, void* info);
	void (*fn_hook) (void* n, void* d, void* e, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.slarrr.f77_blas_function; 
	fn_hook = __flexiblas_hooks->slarrr.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) d, (void*) e, (void*) info); 
		return;
	} else {
		hook_pos_slarrr = 0;
		fn_hook((void*) n, (void*) d, (void*) e, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void slarrr_(blasint* n, float* d, float* e, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slarrr,SLARRR)))));
#else
void slarrr(blasint* n, float* d, float* e, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slarrr,SLARRR)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slarrr_(void* n, void* d, void* e, void* info)
{
	void (*fn) (void* n, void* d, void* e, void* info);

	fn = current_backend->lapack.slarrr.f77_blas_function; 

		fn((void*) n, (void*) d, (void*) e, (void*) info); 

	return;
}

void flexiblas_real_slarrr(void* n, void* d, void* e, void* info)  __attribute__((alias("flexiblas_real_slarrr_")));





/* Chainloader for Hooks */


void flexiblas_chain_slarrr_(void* n, void* d, void* e, void* info)
{
	void (*fn) (void* n, void* d, void* e, void* info);
	void (*fn_hook) (void* n, void* d, void* e, void* info);

	fn      = current_backend->lapack.slarrr.f77_blas_function; 

    hook_pos_slarrr ++;
    if( hook_pos_slarrr < __flexiblas_hooks->slarrr.nhook) {
        fn_hook = __flexiblas_hooks->slarrr.f77_hook_function[hook_pos_slarrr];
        fn_hook((void*) n, (void*) d, (void*) e, (void*) info);
    } else {
        hook_pos_slarrr = 0;
		fn((void*) n, (void*) d, (void*) e, (void*) info); 
	}
	return;
}

void flexiblas_chain_slarrr(void* n, void* d, void* e, void* info)  __attribute__((alias("flexiblas_chain_slarrr_")));




