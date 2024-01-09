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



static TLS_STORE uint8_t hook_pos_cspr = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(cspr,CSPR)(char* uplo, blasint* n, float complex* alpha, float complex* x, blasint* incx, float complex* ap)
#else
void FC_GLOBAL(cspr,CSPR)(char* uplo, blasint* n, float complex* alpha, float complex* x, blasint* incx, float complex* ap)
#endif
{
	void (*fn) (void* uplo, void* n, void* alpha, void* x, void* incx, void* ap);
	void (*fn_hook) (void* uplo, void* n, void* alpha, void* x, void* incx, void* ap);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.cspr.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->cspr.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) uplo, (void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) ap); 
		return;
	} else {
		hook_pos_cspr = 0;
		fn_hook((void*) uplo, (void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) ap);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void cspr_(char* uplo, blasint* n, float complex* alpha, float complex* x, blasint* incx, float complex* ap) __attribute__((alias(MTS(FC_GLOBAL(cspr,CSPR)))));
#else
#ifndef __APPLE__
void cspr(char* uplo, blasint* n, float complex* alpha, float complex* x, blasint* incx, float complex* ap) __attribute__((alias(MTS(FC_GLOBAL(cspr,CSPR)))));
#else
void cspr(char* uplo, blasint* n, float complex* alpha, float complex* x, blasint* incx, float complex* ap){ FC_GLOBAL(cspr,CSPR)((void*) uplo, (void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) ap); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_cspr_(void* uplo, void* n, void* alpha, void* x, void* incx, void* ap)
{
	void (*fn) (void* uplo, void* n, void* alpha, void* x, void* incx, void* ap);

	*(void **) & fn = current_backend->lapack.cspr.f77_blas_function; 

		fn((void*) uplo, (void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) ap); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_cspr(void* uplo, void* n, void* alpha, void* x, void* incx, void* ap) __attribute__((alias("flexiblas_real_cspr_")));
#else
void flexiblas_real_cspr(void* uplo, void* n, void* alpha, void* x, void* incx, void* ap){flexiblas_real_cspr_((void*) uplo, (void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) ap);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_cspr_(void* uplo, void* n, void* alpha, void* x, void* incx, void* ap)
{
	void (*fn) (void* uplo, void* n, void* alpha, void* x, void* incx, void* ap);
	void (*fn_hook) (void* uplo, void* n, void* alpha, void* x, void* incx, void* ap);

	*(void **) &fn      = current_backend->lapack.cspr.f77_blas_function; 

    hook_pos_cspr ++;
    if( hook_pos_cspr < __flexiblas_hooks->cspr.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->cspr.f77_hook_function[hook_pos_cspr];
        fn_hook((void*) uplo, (void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) ap);
    } else {
        hook_pos_cspr = 0;
		fn((void*) uplo, (void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) ap); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_cspr(void* uplo, void* n, void* alpha, void* x, void* incx, void* ap) __attribute__((alias("flexiblas_chain_cspr_")));
#else
void flexiblas_chain_cspr(void* uplo, void* n, void* alpha, void* x, void* incx, void* ap){flexiblas_chain_cspr_((void*) uplo, (void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) ap);}
#endif



