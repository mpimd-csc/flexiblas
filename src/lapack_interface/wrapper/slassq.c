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



static TLS_STORE uint8_t hook_pos_slassq = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slassq,SLASSQ)(blasint* n, float* x, blasint* incx, float* scale, float* sumsq)
#else
void FC_GLOBAL(slassq,SLASSQ)(blasint* n, float* x, blasint* incx, float* scale, float* sumsq)
#endif
{
	void (*fn) (void* n, void* x, void* incx, void* scale, void* sumsq);
	void (*fn_hook) (void* n, void* x, void* incx, void* scale, void* sumsq);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.slassq.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->slassq.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) x, (void*) incx, (void*) scale, (void*) sumsq); 
		return;
	} else {
		hook_pos_slassq = 0;
		fn_hook((void*) n, (void*) x, (void*) incx, (void*) scale, (void*) sumsq);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void slassq_(blasint* n, float* x, blasint* incx, float* scale, float* sumsq) __attribute__((alias(MTS(FC_GLOBAL(slassq,SLASSQ)))));
#else
#ifndef __APPLE__
void slassq(blasint* n, float* x, blasint* incx, float* scale, float* sumsq) __attribute__((alias(MTS(FC_GLOBAL(slassq,SLASSQ)))));
#else
void slassq(blasint* n, float* x, blasint* incx, float* scale, float* sumsq){ FC_GLOBAL(slassq,SLASSQ)((void*) n, (void*) x, (void*) incx, (void*) scale, (void*) sumsq); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slassq_(void* n, void* x, void* incx, void* scale, void* sumsq)
{
	void (*fn) (void* n, void* x, void* incx, void* scale, void* sumsq);

	*(void **) & fn = current_backend->lapack.slassq.f77_blas_function; 

		fn((void*) n, (void*) x, (void*) incx, (void*) scale, (void*) sumsq); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_slassq(void* n, void* x, void* incx, void* scale, void* sumsq) __attribute__((alias("flexiblas_real_slassq_")));
#else
void flexiblas_real_slassq(void* n, void* x, void* incx, void* scale, void* sumsq){flexiblas_real_slassq_((void*) n, (void*) x, (void*) incx, (void*) scale, (void*) sumsq);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_slassq_(void* n, void* x, void* incx, void* scale, void* sumsq)
{
	void (*fn) (void* n, void* x, void* incx, void* scale, void* sumsq);
	void (*fn_hook) (void* n, void* x, void* incx, void* scale, void* sumsq);

	*(void **) &fn      = current_backend->lapack.slassq.f77_blas_function; 

    hook_pos_slassq ++;
    if( hook_pos_slassq < __flexiblas_hooks->slassq.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->slassq.f77_hook_function[hook_pos_slassq];
        fn_hook((void*) n, (void*) x, (void*) incx, (void*) scale, (void*) sumsq);
    } else {
        hook_pos_slassq = 0;
		fn((void*) n, (void*) x, (void*) incx, (void*) scale, (void*) sumsq); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_slassq(void* n, void* x, void* incx, void* scale, void* sumsq) __attribute__((alias("flexiblas_chain_slassq_")));
#else
void flexiblas_chain_slassq(void* n, void* x, void* incx, void* scale, void* sumsq){flexiblas_chain_slassq_((void*) n, (void*) x, (void*) incx, (void*) scale, (void*) sumsq);}
#endif



