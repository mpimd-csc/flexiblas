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



static TLS_STORE uint8_t hook_pos_slagts = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slagts,SLAGTS)(blasint* job, blasint* n, float* a, float* b, float* c, float* d, blasint* in, float* y, float* tol, blasint* info)
#else
void FC_GLOBAL(slagts,SLAGTS)(blasint* job, blasint* n, float* a, float* b, float* c, float* d, blasint* in, float* y, float* tol, blasint* info)
#endif
{
	void (*fn) (void* job, void* n, void* a, void* b, void* c, void* d, void* in, void* y, void* tol, void* info);
	void (*fn_hook) (void* job, void* n, void* a, void* b, void* c, void* d, void* in, void* y, void* tol, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.slagts.f77_blas_function; 
	fn_hook = __flexiblas_hooks->slagts.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) job, (void*) n, (void*) a, (void*) b, (void*) c, (void*) d, (void*) in, (void*) y, (void*) tol, (void*) info); 
		return;
	} else {
		hook_pos_slagts = 0;
		fn_hook((void*) job, (void*) n, (void*) a, (void*) b, (void*) c, (void*) d, (void*) in, (void*) y, (void*) tol, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void slagts_(blasint* job, blasint* n, float* a, float* b, float* c, float* d, blasint* in, float* y, float* tol, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slagts,SLAGTS)))));
#else
void slagts(blasint* job, blasint* n, float* a, float* b, float* c, float* d, blasint* in, float* y, float* tol, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slagts,SLAGTS)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slagts_(void* job, void* n, void* a, void* b, void* c, void* d, void* in, void* y, void* tol, void* info)
{
	void (*fn) (void* job, void* n, void* a, void* b, void* c, void* d, void* in, void* y, void* tol, void* info);

	fn = current_backend->lapack.slagts.f77_blas_function; 

		fn((void*) job, (void*) n, (void*) a, (void*) b, (void*) c, (void*) d, (void*) in, (void*) y, (void*) tol, (void*) info); 

	return;
}

void flexiblas_real_slagts(void* job, void* n, void* a, void* b, void* c, void* d, void* in, void* y, void* tol, void* info)  __attribute__((alias("flexiblas_real_slagts_")));





/* Chainloader for Hooks */


void flexiblas_chain_slagts_(void* job, void* n, void* a, void* b, void* c, void* d, void* in, void* y, void* tol, void* info)
{
	void (*fn) (void* job, void* n, void* a, void* b, void* c, void* d, void* in, void* y, void* tol, void* info);
	void (*fn_hook) (void* job, void* n, void* a, void* b, void* c, void* d, void* in, void* y, void* tol, void* info);

	fn      = current_backend->lapack.slagts.f77_blas_function; 

    hook_pos_slagts ++;
    if( hook_pos_slagts < __flexiblas_hooks->slagts.nhook) {
        fn_hook = __flexiblas_hooks->slagts.f77_hook_function[hook_pos_slagts];
        fn_hook((void*) job, (void*) n, (void*) a, (void*) b, (void*) c, (void*) d, (void*) in, (void*) y, (void*) tol, (void*) info);
    } else {
        hook_pos_slagts = 0;
		fn((void*) job, (void*) n, (void*) a, (void*) b, (void*) c, (void*) d, (void*) in, (void*) y, (void*) tol, (void*) info); 
	}
	return;
}

void flexiblas_chain_slagts(void* job, void* n, void* a, void* b, void* c, void* d, void* in, void* y, void* tol, void* info)  __attribute__((alias("flexiblas_chain_slagts_")));




