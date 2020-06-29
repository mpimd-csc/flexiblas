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



static TLS_STORE uint8_t hook_pos_clapmt = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(clapmt,CLAPMT)(blasint* forwrd, blasint* m, blasint* n, float complex* x, blasint* ldx, blasint* k)
#else
void FC_GLOBAL(clapmt,CLAPMT)(blasint* forwrd, blasint* m, blasint* n, float complex* x, blasint* ldx, blasint* k)
#endif
{
	void (*fn) (void* forwrd, void* m, void* n, void* x, void* ldx, void* k);
	void (*fn_hook) (void* forwrd, void* m, void* n, void* x, void* ldx, void* k);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.clapmt.f77_blas_function; 
	fn_hook = __flexiblas_hooks->clapmt.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) forwrd, (void*) m, (void*) n, (void*) x, (void*) ldx, (void*) k); 
		return;
	} else {
		hook_pos_clapmt = 0;
		fn_hook((void*) forwrd, (void*) m, (void*) n, (void*) x, (void*) ldx, (void*) k);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void clapmt_(blasint* forwrd, blasint* m, blasint* n, float complex* x, blasint* ldx, blasint* k) __attribute__((alias(MTS(FC_GLOBAL(clapmt,CLAPMT)))));
#else
void clapmt(blasint* forwrd, blasint* m, blasint* n, float complex* x, blasint* ldx, blasint* k) __attribute__((alias(MTS(FC_GLOBAL(clapmt,CLAPMT)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_clapmt_(void* forwrd, void* m, void* n, void* x, void* ldx, void* k)
{
	void (*fn) (void* forwrd, void* m, void* n, void* x, void* ldx, void* k);

	fn = current_backend->lapack.clapmt.f77_blas_function; 

		fn((void*) forwrd, (void*) m, (void*) n, (void*) x, (void*) ldx, (void*) k); 

	return;
}

void flexiblas_real_clapmt(void* forwrd, void* m, void* n, void* x, void* ldx, void* k)  __attribute__((alias("flexiblas_real_clapmt_")));





/* Chainloader for Hooks */


void flexiblas_chain_clapmt_(void* forwrd, void* m, void* n, void* x, void* ldx, void* k)
{
	void (*fn) (void* forwrd, void* m, void* n, void* x, void* ldx, void* k);
	void (*fn_hook) (void* forwrd, void* m, void* n, void* x, void* ldx, void* k);

	fn      = current_backend->lapack.clapmt.f77_blas_function; 

    hook_pos_clapmt ++;
    if( hook_pos_clapmt < __flexiblas_hooks->clapmt.nhook) {
        fn_hook = __flexiblas_hooks->clapmt.f77_hook_function[hook_pos_clapmt];
        fn_hook((void*) forwrd, (void*) m, (void*) n, (void*) x, (void*) ldx, (void*) k);
    } else {
        hook_pos_clapmt = 0;
		fn((void*) forwrd, (void*) m, (void*) n, (void*) x, (void*) ldx, (void*) k); 
	}
	return;
}

void flexiblas_chain_clapmt(void* forwrd, void* m, void* n, void* x, void* ldx, void* k)  __attribute__((alias("flexiblas_chain_clapmt_")));




