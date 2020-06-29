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



static TLS_STORE uint8_t hook_pos_slarfx = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slarfx,SLARFX)(char* side, blasint* m, blasint* n, float* v, float* tau, float* c, blasint* ldc, float* work)
#else
void FC_GLOBAL(slarfx,SLARFX)(char* side, blasint* m, blasint* n, float* v, float* tau, float* c, blasint* ldc, float* work)
#endif
{
	void (*fn) (void* side, void* m, void* n, void* v, void* tau, void* c, void* ldc, void* work);
	void (*fn_hook) (void* side, void* m, void* n, void* v, void* tau, void* c, void* ldc, void* work);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.slarfx.f77_blas_function; 
	fn_hook = __flexiblas_hooks->slarfx.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) side, (void*) m, (void*) n, (void*) v, (void*) tau, (void*) c, (void*) ldc, (void*) work); 
		return;
	} else {
		hook_pos_slarfx = 0;
		fn_hook((void*) side, (void*) m, (void*) n, (void*) v, (void*) tau, (void*) c, (void*) ldc, (void*) work);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void slarfx_(char* side, blasint* m, blasint* n, float* v, float* tau, float* c, blasint* ldc, float* work) __attribute__((alias(MTS(FC_GLOBAL(slarfx,SLARFX)))));
#else
void slarfx(char* side, blasint* m, blasint* n, float* v, float* tau, float* c, blasint* ldc, float* work) __attribute__((alias(MTS(FC_GLOBAL(slarfx,SLARFX)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slarfx_(void* side, void* m, void* n, void* v, void* tau, void* c, void* ldc, void* work)
{
	void (*fn) (void* side, void* m, void* n, void* v, void* tau, void* c, void* ldc, void* work);

	fn = current_backend->lapack.slarfx.f77_blas_function; 

		fn((void*) side, (void*) m, (void*) n, (void*) v, (void*) tau, (void*) c, (void*) ldc, (void*) work); 

	return;
}

void flexiblas_real_slarfx(void* side, void* m, void* n, void* v, void* tau, void* c, void* ldc, void* work)  __attribute__((alias("flexiblas_real_slarfx_")));





/* Chainloader for Hooks */


void flexiblas_chain_slarfx_(void* side, void* m, void* n, void* v, void* tau, void* c, void* ldc, void* work)
{
	void (*fn) (void* side, void* m, void* n, void* v, void* tau, void* c, void* ldc, void* work);
	void (*fn_hook) (void* side, void* m, void* n, void* v, void* tau, void* c, void* ldc, void* work);

	fn      = current_backend->lapack.slarfx.f77_blas_function; 

    hook_pos_slarfx ++;
    if( hook_pos_slarfx < __flexiblas_hooks->slarfx.nhook) {
        fn_hook = __flexiblas_hooks->slarfx.f77_hook_function[hook_pos_slarfx];
        fn_hook((void*) side, (void*) m, (void*) n, (void*) v, (void*) tau, (void*) c, (void*) ldc, (void*) work);
    } else {
        hook_pos_slarfx = 0;
		fn((void*) side, (void*) m, (void*) n, (void*) v, (void*) tau, (void*) c, (void*) ldc, (void*) work); 
	}
	return;
}

void flexiblas_chain_slarfx(void* side, void* m, void* n, void* v, void* tau, void* c, void* ldc, void* work)  __attribute__((alias("flexiblas_chain_slarfx_")));




