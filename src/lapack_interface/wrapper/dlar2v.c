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



static TLS_STORE uint8_t hook_pos_dlar2v = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlar2v,DLAR2V)(blasint* n, double* x, double* y, double* z, blasint* incx, double* c, double* s, blasint* incc)
#else
void FC_GLOBAL(dlar2v,DLAR2V)(blasint* n, double* x, double* y, double* z, blasint* incx, double* c, double* s, blasint* incc)
#endif
{
	void (*fn) (void* n, void* x, void* y, void* z, void* incx, void* c, void* s, void* incc);
	void (*fn_hook) (void* n, void* x, void* y, void* z, void* incx, void* c, void* s, void* incc);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dlar2v.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dlar2v.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) x, (void*) y, (void*) z, (void*) incx, (void*) c, (void*) s, (void*) incc); 
		return;
	} else {
		hook_pos_dlar2v = 0;
		fn_hook((void*) n, (void*) x, (void*) y, (void*) z, (void*) incx, (void*) c, (void*) s, (void*) incc);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dlar2v_(blasint* n, double* x, double* y, double* z, blasint* incx, double* c, double* s, blasint* incc) __attribute__((alias(MTS(FC_GLOBAL(dlar2v,DLAR2V)))));
#else
#ifndef __APPLE__
void dlar2v(blasint* n, double* x, double* y, double* z, blasint* incx, double* c, double* s, blasint* incc) __attribute__((alias(MTS(FC_GLOBAL(dlar2v,DLAR2V)))));
#else
void dlar2v(blasint* n, double* x, double* y, double* z, blasint* incx, double* c, double* s, blasint* incc){ FC_GLOBAL(dlar2v,DLAR2V)((void*) n, (void*) x, (void*) y, (void*) z, (void*) incx, (void*) c, (void*) s, (void*) incc); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlar2v_(void* n, void* x, void* y, void* z, void* incx, void* c, void* s, void* incc)
{
	void (*fn) (void* n, void* x, void* y, void* z, void* incx, void* c, void* s, void* incc);

	*(void **) & fn = current_backend->lapack.dlar2v.f77_blas_function; 

		fn((void*) n, (void*) x, (void*) y, (void*) z, (void*) incx, (void*) c, (void*) s, (void*) incc); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_dlar2v(void* n, void* x, void* y, void* z, void* incx, void* c, void* s, void* incc) __attribute__((alias("flexiblas_real_dlar2v_")));
#else
void flexiblas_real_dlar2v(void* n, void* x, void* y, void* z, void* incx, void* c, void* s, void* incc){flexiblas_real_dlar2v_((void*) n, (void*) x, (void*) y, (void*) z, (void*) incx, (void*) c, (void*) s, (void*) incc);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlar2v_(void* n, void* x, void* y, void* z, void* incx, void* c, void* s, void* incc)
{
	void (*fn) (void* n, void* x, void* y, void* z, void* incx, void* c, void* s, void* incc);
	void (*fn_hook) (void* n, void* x, void* y, void* z, void* incx, void* c, void* s, void* incc);

	*(void **) &fn      = current_backend->lapack.dlar2v.f77_blas_function; 

    hook_pos_dlar2v ++;
    if( hook_pos_dlar2v < __flexiblas_hooks->dlar2v.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlar2v.f77_hook_function[hook_pos_dlar2v];
        fn_hook((void*) n, (void*) x, (void*) y, (void*) z, (void*) incx, (void*) c, (void*) s, (void*) incc);
    } else {
        hook_pos_dlar2v = 0;
		fn((void*) n, (void*) x, (void*) y, (void*) z, (void*) incx, (void*) c, (void*) s, (void*) incc); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_dlar2v(void* n, void* x, void* y, void* z, void* incx, void* c, void* s, void* incc) __attribute__((alias("flexiblas_chain_dlar2v_")));
#else
void flexiblas_chain_dlar2v(void* n, void* x, void* y, void* z, void* incx, void* c, void* s, void* incc){flexiblas_chain_dlar2v_((void*) n, (void*) x, (void*) y, (void*) z, (void*) incx, (void*) c, (void*) s, (void*) incc);}
#endif



