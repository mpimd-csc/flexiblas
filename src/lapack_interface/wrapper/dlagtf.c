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



static TLS_STORE uint8_t hook_pos_dlagtf = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlagtf,DLAGTF)(blasint* n, double* a, double* lambda, double* b, double* c, double* tol, double* d, blasint* in, blasint* info)
#else
void FC_GLOBAL(dlagtf,DLAGTF)(blasint* n, double* a, double* lambda, double* b, double* c, double* tol, double* d, blasint* in, blasint* info)
#endif
{
	void (*fn) (void* n, void* a, void* lambda, void* b, void* c, void* tol, void* d, void* in, void* info);
	void (*fn_hook) (void* n, void* a, void* lambda, void* b, void* c, void* tol, void* d, void* in, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dlagtf.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dlagtf.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) a, (void*) lambda, (void*) b, (void*) c, (void*) tol, (void*) d, (void*) in, (void*) info); 
		return;
	} else {
		hook_pos_dlagtf = 0;
		fn_hook((void*) n, (void*) a, (void*) lambda, (void*) b, (void*) c, (void*) tol, (void*) d, (void*) in, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dlagtf_(blasint* n, double* a, double* lambda, double* b, double* c, double* tol, double* d, blasint* in, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlagtf,DLAGTF)))));
#else
#ifndef __APPLE__
void dlagtf(blasint* n, double* a, double* lambda, double* b, double* c, double* tol, double* d, blasint* in, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlagtf,DLAGTF)))));
#else
void dlagtf(blasint* n, double* a, double* lambda, double* b, double* c, double* tol, double* d, blasint* in, blasint* info){ FC_GLOBAL(dlagtf,DLAGTF)((void*) n, (void*) a, (void*) lambda, (void*) b, (void*) c, (void*) tol, (void*) d, (void*) in, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlagtf_(void* n, void* a, void* lambda, void* b, void* c, void* tol, void* d, void* in, void* info)
{
	void (*fn) (void* n, void* a, void* lambda, void* b, void* c, void* tol, void* d, void* in, void* info);

	*(void **) & fn = current_backend->lapack.dlagtf.f77_blas_function; 

		fn((void*) n, (void*) a, (void*) lambda, (void*) b, (void*) c, (void*) tol, (void*) d, (void*) in, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_dlagtf(void* n, void* a, void* lambda, void* b, void* c, void* tol, void* d, void* in, void* info) __attribute__((alias("flexiblas_real_dlagtf_")));
#else
void flexiblas_real_dlagtf(void* n, void* a, void* lambda, void* b, void* c, void* tol, void* d, void* in, void* info){flexiblas_real_dlagtf_((void*) n, (void*) a, (void*) lambda, (void*) b, (void*) c, (void*) tol, (void*) d, (void*) in, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlagtf_(void* n, void* a, void* lambda, void* b, void* c, void* tol, void* d, void* in, void* info)
{
	void (*fn) (void* n, void* a, void* lambda, void* b, void* c, void* tol, void* d, void* in, void* info);
	void (*fn_hook) (void* n, void* a, void* lambda, void* b, void* c, void* tol, void* d, void* in, void* info);

	*(void **) &fn      = current_backend->lapack.dlagtf.f77_blas_function; 

    hook_pos_dlagtf ++;
    if( hook_pos_dlagtf < __flexiblas_hooks->dlagtf.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlagtf.f77_hook_function[hook_pos_dlagtf];
        fn_hook((void*) n, (void*) a, (void*) lambda, (void*) b, (void*) c, (void*) tol, (void*) d, (void*) in, (void*) info);
    } else {
        hook_pos_dlagtf = 0;
		fn((void*) n, (void*) a, (void*) lambda, (void*) b, (void*) c, (void*) tol, (void*) d, (void*) in, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_dlagtf(void* n, void* a, void* lambda, void* b, void* c, void* tol, void* d, void* in, void* info) __attribute__((alias("flexiblas_chain_dlagtf_")));
#else
void flexiblas_chain_dlagtf(void* n, void* a, void* lambda, void* b, void* c, void* tol, void* d, void* in, void* info){flexiblas_chain_dlagtf_((void*) n, (void*) a, (void*) lambda, (void*) b, (void*) c, (void*) tol, (void*) d, (void*) in, (void*) info);}
#endif



