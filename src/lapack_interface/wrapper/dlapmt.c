//    SPDX-License-Identifier: LGPL-3.0-or-later
/*
    This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
    Copyright (C) 2013-2024 Martin Koehler

    This program is free software: you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by the Free
    Software Foundation, either version 3 of the License, or (at your option)
    any later version.

    This program is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
    more details.

    You should have received a copy of the GNU General Public License along
    with this program. If not, see <https://www.gnu.org/licenses/>.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <complex.h>

#include "flexiblas_fortran_mangle.h"

#include "flexiblas.h"


#if __GNUC__ > 7
typedef size_t fortran_charlen_t;
#else
typedef int fortran_charlen_t;
#endif

#ifdef INTEGER8
#define blasint int64_t
#else
#define blasint int
#endif



static TLS_STORE uint8_t hook_pos_dlapmt = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlapmt,DLAPMT)(blasint* forwrd, blasint* m, blasint* n, double* x, blasint* ldx, blasint* k)
#else
void FC_GLOBAL(dlapmt,DLAPMT)(blasint* forwrd, blasint* m, blasint* n, double* x, blasint* ldx, blasint* k)
#endif
{
	void (*fn) (void* forwrd, void* m, void* n, void* x, void* ldx, void* k);
	void (*fn_hook) (void* forwrd, void* m, void* n, void* x, void* ldx, void* k);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dlapmt.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dlapmt.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) forwrd, (void*) m, (void*) n, (void*) x, (void*) ldx, (void*) k); 
		return;
	} else {
		hook_pos_dlapmt = 0;
		fn_hook((void*) forwrd, (void*) m, (void*) n, (void*) x, (void*) ldx, (void*) k);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dlapmt_(blasint* forwrd, blasint* m, blasint* n, double* x, blasint* ldx, blasint* k) __attribute__((alias(MTS(FC_GLOBAL(dlapmt,DLAPMT)))));
#else
#ifndef __APPLE__
void dlapmt(blasint* forwrd, blasint* m, blasint* n, double* x, blasint* ldx, blasint* k) __attribute__((alias(MTS(FC_GLOBAL(dlapmt,DLAPMT)))));
#else
void dlapmt(blasint* forwrd, blasint* m, blasint* n, double* x, blasint* ldx, blasint* k){ FC_GLOBAL(dlapmt,DLAPMT)((void*) forwrd, (void*) m, (void*) n, (void*) x, (void*) ldx, (void*) k); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlapmt_(void* forwrd, void* m, void* n, void* x, void* ldx, void* k)
{
	void (*fn) (void* forwrd, void* m, void* n, void* x, void* ldx, void* k);

	*(void **) & fn = current_backend->lapack.dlapmt.f77_blas_function; 

		fn((void*) forwrd, (void*) m, (void*) n, (void*) x, (void*) ldx, (void*) k); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_dlapmt(void* forwrd, void* m, void* n, void* x, void* ldx, void* k) __attribute__((alias("flexiblas_real_dlapmt_")));
#else
void flexiblas_real_dlapmt(void* forwrd, void* m, void* n, void* x, void* ldx, void* k){flexiblas_real_dlapmt_((void*) forwrd, (void*) m, (void*) n, (void*) x, (void*) ldx, (void*) k);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlapmt_(void* forwrd, void* m, void* n, void* x, void* ldx, void* k)
{
	void (*fn) (void* forwrd, void* m, void* n, void* x, void* ldx, void* k);
	void (*fn_hook) (void* forwrd, void* m, void* n, void* x, void* ldx, void* k);

	*(void **) &fn      = current_backend->lapack.dlapmt.f77_blas_function; 

    hook_pos_dlapmt ++;
    if( hook_pos_dlapmt < __flexiblas_hooks->dlapmt.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlapmt.f77_hook_function[hook_pos_dlapmt];
        fn_hook((void*) forwrd, (void*) m, (void*) n, (void*) x, (void*) ldx, (void*) k);
    } else {
        hook_pos_dlapmt = 0;
		fn((void*) forwrd, (void*) m, (void*) n, (void*) x, (void*) ldx, (void*) k); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_dlapmt(void* forwrd, void* m, void* n, void* x, void* ldx, void* k) __attribute__((alias("flexiblas_chain_dlapmt_")));
#else
void flexiblas_chain_dlapmt(void* forwrd, void* m, void* n, void* x, void* ldx, void* k){flexiblas_chain_dlapmt_((void*) forwrd, (void*) m, (void*) n, (void*) x, (void*) ldx, (void*) k);}
#endif



