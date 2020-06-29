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
 /* Generated: Wed Mar 28 11:20:03 2018 */
        
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



static TLS_STORE uint8_t hook_pos_clarz = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(clarz,CLARZ)(char* side, blasint* m, blasint* n, blasint* l, float complex* v, blasint* incv, float complex* tau, float complex* c, blasint* ldc, float complex* work)
#else
void FC_GLOBAL(clarz,CLARZ)(char* side, blasint* m, blasint* n, blasint* l, float complex* v, blasint* incv, float complex* tau, float complex* c, blasint* ldc, float complex* work)
#endif
{
	void (*fn) (void* side, void* m, void* n, void* l, void* v, void* incv, void* tau, void* c, void* ldc, void* work);
	void (*fn_hook) (void* side, void* m, void* n, void* l, void* v, void* incv, void* tau, void* c, void* ldc, void* work);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.clarz.f77_blas_function; 
	fn_hook = __flexiblas_hooks->clarz.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) side, (void*) m, (void*) n, (void*) l, (void*) v, (void*) incv, (void*) tau, (void*) c, (void*) ldc, (void*) work); 
		return;
	} else {
		hook_pos_clarz = 0;
		fn_hook((void*) side, (void*) m, (void*) n, (void*) l, (void*) v, (void*) incv, (void*) tau, (void*) c, (void*) ldc, (void*) work);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void clarz_(char* side, blasint* m, blasint* n, blasint* l, float complex* v, blasint* incv, float complex* tau, float complex* c, blasint* ldc, float complex* work) __attribute__((alias(MTS(FC_GLOBAL(clarz,CLARZ)))));
#else
void clarz(char* side, blasint* m, blasint* n, blasint* l, float complex* v, blasint* incv, float complex* tau, float complex* c, blasint* ldc, float complex* work) __attribute__((alias(MTS(FC_GLOBAL(clarz,CLARZ)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_clarz_(void* side, void* m, void* n, void* l, void* v, void* incv, void* tau, void* c, void* ldc, void* work)
{
	void (*fn) (void* side, void* m, void* n, void* l, void* v, void* incv, void* tau, void* c, void* ldc, void* work);

	fn = current_backend->lapack.clarz.f77_blas_function; 

		fn((void*) side, (void*) m, (void*) n, (void*) l, (void*) v, (void*) incv, (void*) tau, (void*) c, (void*) ldc, (void*) work); 

	return;
}

void flexiblas_real_clarz(void* side, void* m, void* n, void* l, void* v, void* incv, void* tau, void* c, void* ldc, void* work)  __attribute__((alias("flexiblas_real_clarz_")));





/* Chainloader for Hooks */


void flexiblas_chain_clarz_(void* side, void* m, void* n, void* l, void* v, void* incv, void* tau, void* c, void* ldc, void* work)
{
	void (*fn) (void* side, void* m, void* n, void* l, void* v, void* incv, void* tau, void* c, void* ldc, void* work);
	void (*fn_hook) (void* side, void* m, void* n, void* l, void* v, void* incv, void* tau, void* c, void* ldc, void* work);

	fn      = current_backend->lapack.clarz.f77_blas_function; 

    hook_pos_clarz ++;
    if( hook_pos_clarz < __flexiblas_hooks->clarz.nhook) {
        fn_hook = __flexiblas_hooks->clarz.f77_hook_function[hook_pos_clarz];
        fn_hook((void*) side, (void*) m, (void*) n, (void*) l, (void*) v, (void*) incv, (void*) tau, (void*) c, (void*) ldc, (void*) work);
    } else {
        hook_pos_clarz = 0;
		fn((void*) side, (void*) m, (void*) n, (void*) l, (void*) v, (void*) incv, (void*) tau, (void*) c, (void*) ldc, (void*) work); 
	}
	return;
}

void flexiblas_chain_clarz(void* side, void* m, void* n, void* l, void* v, void* incv, void* tau, void* c, void* ldc, void* work)  __attribute__((alias("flexiblas_chain_clarz_")));




