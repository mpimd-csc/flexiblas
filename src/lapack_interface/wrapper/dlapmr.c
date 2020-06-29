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



static TLS_STORE uint8_t hook_pos_dlapmr = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlapmr,DLAPMR)(blasint* forwrd, blasint* m, blasint* n, double* x, blasint* ldx, blasint* k)
#else
void FC_GLOBAL(dlapmr,DLAPMR)(blasint* forwrd, blasint* m, blasint* n, double* x, blasint* ldx, blasint* k)
#endif
{
	void (*fn) (void* forwrd, void* m, void* n, void* x, void* ldx, void* k);
	void (*fn_hook) (void* forwrd, void* m, void* n, void* x, void* ldx, void* k);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.dlapmr.f77_blas_function; 
	fn_hook = __flexiblas_hooks->dlapmr.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) forwrd, (void*) m, (void*) n, (void*) x, (void*) ldx, (void*) k); 
		return;
	} else {
		hook_pos_dlapmr = 0;
		fn_hook((void*) forwrd, (void*) m, (void*) n, (void*) x, (void*) ldx, (void*) k);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dlapmr_(blasint* forwrd, blasint* m, blasint* n, double* x, blasint* ldx, blasint* k) __attribute__((alias(MTS(FC_GLOBAL(dlapmr,DLAPMR)))));
#else
void dlapmr(blasint* forwrd, blasint* m, blasint* n, double* x, blasint* ldx, blasint* k) __attribute__((alias(MTS(FC_GLOBAL(dlapmr,DLAPMR)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlapmr_(void* forwrd, void* m, void* n, void* x, void* ldx, void* k)
{
	void (*fn) (void* forwrd, void* m, void* n, void* x, void* ldx, void* k);

	fn = current_backend->lapack.dlapmr.f77_blas_function; 

		fn((void*) forwrd, (void*) m, (void*) n, (void*) x, (void*) ldx, (void*) k); 

	return;
}

void flexiblas_real_dlapmr(void* forwrd, void* m, void* n, void* x, void* ldx, void* k)  __attribute__((alias("flexiblas_real_dlapmr_")));





/* Chainloader for Hooks */


void flexiblas_chain_dlapmr_(void* forwrd, void* m, void* n, void* x, void* ldx, void* k)
{
	void (*fn) (void* forwrd, void* m, void* n, void* x, void* ldx, void* k);
	void (*fn_hook) (void* forwrd, void* m, void* n, void* x, void* ldx, void* k);

	fn      = current_backend->lapack.dlapmr.f77_blas_function; 

    hook_pos_dlapmr ++;
    if( hook_pos_dlapmr < __flexiblas_hooks->dlapmr.nhook) {
        fn_hook = __flexiblas_hooks->dlapmr.f77_hook_function[hook_pos_dlapmr];
        fn_hook((void*) forwrd, (void*) m, (void*) n, (void*) x, (void*) ldx, (void*) k);
    } else {
        hook_pos_dlapmr = 0;
		fn((void*) forwrd, (void*) m, (void*) n, (void*) x, (void*) ldx, (void*) k); 
	}
	return;
}

void flexiblas_chain_dlapmr(void* forwrd, void* m, void* n, void* x, void* ldx, void* k)  __attribute__((alias("flexiblas_chain_dlapmr_")));




