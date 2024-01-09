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



static TLS_STORE uint8_t hook_pos_zlarfgp = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zlarfgp,ZLARFGP)(blasint* n, double complex* alpha, double complex* x, blasint* incx, double complex* tau)
#else
void FC_GLOBAL(zlarfgp,ZLARFGP)(blasint* n, double complex* alpha, double complex* x, blasint* incx, double complex* tau)
#endif
{
	void (*fn) (void* n, void* alpha, void* x, void* incx, void* tau);
	void (*fn_hook) (void* n, void* alpha, void* x, void* incx, void* tau);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.zlarfgp.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->zlarfgp.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) tau); 
		return;
	} else {
		hook_pos_zlarfgp = 0;
		fn_hook((void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) tau);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void zlarfgp_(blasint* n, double complex* alpha, double complex* x, blasint* incx, double complex* tau) __attribute__((alias(MTS(FC_GLOBAL(zlarfgp,ZLARFGP)))));
#else
#ifndef __APPLE__
void zlarfgp(blasint* n, double complex* alpha, double complex* x, blasint* incx, double complex* tau) __attribute__((alias(MTS(FC_GLOBAL(zlarfgp,ZLARFGP)))));
#else
void zlarfgp(blasint* n, double complex* alpha, double complex* x, blasint* incx, double complex* tau){ FC_GLOBAL(zlarfgp,ZLARFGP)((void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) tau); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zlarfgp_(void* n, void* alpha, void* x, void* incx, void* tau)
{
	void (*fn) (void* n, void* alpha, void* x, void* incx, void* tau);

	*(void **) & fn = current_backend->lapack.zlarfgp.f77_blas_function; 

		fn((void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) tau); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_zlarfgp(void* n, void* alpha, void* x, void* incx, void* tau) __attribute__((alias("flexiblas_real_zlarfgp_")));
#else
void flexiblas_real_zlarfgp(void* n, void* alpha, void* x, void* incx, void* tau){flexiblas_real_zlarfgp_((void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) tau);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zlarfgp_(void* n, void* alpha, void* x, void* incx, void* tau)
{
	void (*fn) (void* n, void* alpha, void* x, void* incx, void* tau);
	void (*fn_hook) (void* n, void* alpha, void* x, void* incx, void* tau);

	*(void **) &fn      = current_backend->lapack.zlarfgp.f77_blas_function; 

    hook_pos_zlarfgp ++;
    if( hook_pos_zlarfgp < __flexiblas_hooks->zlarfgp.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zlarfgp.f77_hook_function[hook_pos_zlarfgp];
        fn_hook((void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) tau);
    } else {
        hook_pos_zlarfgp = 0;
		fn((void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) tau); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_zlarfgp(void* n, void* alpha, void* x, void* incx, void* tau) __attribute__((alias("flexiblas_chain_zlarfgp_")));
#else
void flexiblas_chain_zlarfgp(void* n, void* alpha, void* x, void* incx, void* tau){flexiblas_chain_zlarfgp_((void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) tau);}
#endif



