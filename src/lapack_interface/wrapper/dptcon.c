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


#ifndef FLEXIBLAS_CHARLEN_T
#define FLEXIBLAS_CHARLEN_T
#if __GNUC__ > 7
typedef size_t flexiblas_fortran_charlen_t;
#else
typedef int flexiblas_fortran_charlen_t;
#endif
#endif

#ifndef blasint
#ifdef FLEXIBLAS_INTEGER8
#define blasint int64_t
#else
#define blasint int
#endif
#endif



static TLS_STORE uint8_t hook_pos_dptcon = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dptcon,DPTCON)(blasint* n, double* d, double* e, double* anorm, double* rcond, double* work, blasint* info)
#else
void FC_GLOBAL(dptcon,DPTCON)(blasint* n, double* d, double* e, double* anorm, double* rcond, double* work, blasint* info)
#endif
{
	void (*fn) (void* n, void* d, void* e, void* anorm, void* rcond, void* work, void* info);
	void (*fn_hook) (void* n, void* d, void* e, void* anorm, void* rcond, void* work, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dptcon.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dptcon.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) d, (void*) e, (void*) anorm, (void*) rcond, (void*) work, (void*) info); 
		return;
	} else {
		hook_pos_dptcon = 0;
		fn_hook((void*) n, (void*) d, (void*) e, (void*) anorm, (void*) rcond, (void*) work, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dptcon_(blasint* n, double* d, double* e, double* anorm, double* rcond, double* work, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dptcon,DPTCON)))));
#else
#ifndef __APPLE__
void dptcon(blasint* n, double* d, double* e, double* anorm, double* rcond, double* work, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dptcon,DPTCON)))));
#else
void dptcon(blasint* n, double* d, double* e, double* anorm, double* rcond, double* work, blasint* info){ FC_GLOBAL(dptcon,DPTCON)((void*) n, (void*) d, (void*) e, (void*) anorm, (void*) rcond, (void*) work, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dptcon_(void* n, void* d, void* e, void* anorm, void* rcond, void* work, void* info)
{
	void (*fn) (void* n, void* d, void* e, void* anorm, void* rcond, void* work, void* info);

	*(void **) & fn = current_backend->lapack.dptcon.f77_blas_function; 

		fn((void*) n, (void*) d, (void*) e, (void*) anorm, (void*) rcond, (void*) work, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_dptcon(void* n, void* d, void* e, void* anorm, void* rcond, void* work, void* info) __attribute__((alias("flexiblas_real_dptcon_")));
#else
void flexiblas_real_dptcon(void* n, void* d, void* e, void* anorm, void* rcond, void* work, void* info){flexiblas_real_dptcon_((void*) n, (void*) d, (void*) e, (void*) anorm, (void*) rcond, (void*) work, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dptcon_(void* n, void* d, void* e, void* anorm, void* rcond, void* work, void* info)
{
	void (*fn) (void* n, void* d, void* e, void* anorm, void* rcond, void* work, void* info);
	void (*fn_hook) (void* n, void* d, void* e, void* anorm, void* rcond, void* work, void* info);

	*(void **) &fn      = current_backend->lapack.dptcon.f77_blas_function; 

    hook_pos_dptcon ++;
    if( hook_pos_dptcon < __flexiblas_hooks->dptcon.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dptcon.f77_hook_function[hook_pos_dptcon];
        fn_hook((void*) n, (void*) d, (void*) e, (void*) anorm, (void*) rcond, (void*) work, (void*) info);
    } else {
        hook_pos_dptcon = 0;
		fn((void*) n, (void*) d, (void*) e, (void*) anorm, (void*) rcond, (void*) work, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_dptcon(void* n, void* d, void* e, void* anorm, void* rcond, void* work, void* info) __attribute__((alias("flexiblas_chain_dptcon_")));
#else
void flexiblas_chain_dptcon(void* n, void* d, void* e, void* anorm, void* rcond, void* work, void* info){flexiblas_chain_dptcon_((void*) n, (void*) d, (void*) e, (void*) anorm, (void*) rcond, (void*) work, (void*) info);}
#endif



