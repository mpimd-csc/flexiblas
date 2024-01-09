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



static TLS_STORE uint8_t hook_pos_csrscl = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(csrscl,CSRSCL)(blasint* n, float* sa, float complex* sx, blasint* incx)
#else
void FC_GLOBAL(csrscl,CSRSCL)(blasint* n, float* sa, float complex* sx, blasint* incx)
#endif
{
	void (*fn) (void* n, void* sa, void* sx, void* incx);
	void (*fn_hook) (void* n, void* sa, void* sx, void* incx);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.csrscl.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->csrscl.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) sa, (void*) sx, (void*) incx); 
		return;
	} else {
		hook_pos_csrscl = 0;
		fn_hook((void*) n, (void*) sa, (void*) sx, (void*) incx);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void csrscl_(blasint* n, float* sa, float complex* sx, blasint* incx) __attribute__((alias(MTS(FC_GLOBAL(csrscl,CSRSCL)))));
#else
#ifndef __APPLE__
void csrscl(blasint* n, float* sa, float complex* sx, blasint* incx) __attribute__((alias(MTS(FC_GLOBAL(csrscl,CSRSCL)))));
#else
void csrscl(blasint* n, float* sa, float complex* sx, blasint* incx){ FC_GLOBAL(csrscl,CSRSCL)((void*) n, (void*) sa, (void*) sx, (void*) incx); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_csrscl_(void* n, void* sa, void* sx, void* incx)
{
	void (*fn) (void* n, void* sa, void* sx, void* incx);

	*(void **) & fn = current_backend->lapack.csrscl.f77_blas_function; 

		fn((void*) n, (void*) sa, (void*) sx, (void*) incx); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_csrscl(void* n, void* sa, void* sx, void* incx) __attribute__((alias("flexiblas_real_csrscl_")));
#else
void flexiblas_real_csrscl(void* n, void* sa, void* sx, void* incx){flexiblas_real_csrscl_((void*) n, (void*) sa, (void*) sx, (void*) incx);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_csrscl_(void* n, void* sa, void* sx, void* incx)
{
	void (*fn) (void* n, void* sa, void* sx, void* incx);
	void (*fn_hook) (void* n, void* sa, void* sx, void* incx);

	*(void **) &fn      = current_backend->lapack.csrscl.f77_blas_function; 

    hook_pos_csrscl ++;
    if( hook_pos_csrscl < __flexiblas_hooks->csrscl.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->csrscl.f77_hook_function[hook_pos_csrscl];
        fn_hook((void*) n, (void*) sa, (void*) sx, (void*) incx);
    } else {
        hook_pos_csrscl = 0;
		fn((void*) n, (void*) sa, (void*) sx, (void*) incx); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_csrscl(void* n, void* sa, void* sx, void* incx) __attribute__((alias("flexiblas_chain_csrscl_")));
#else
void flexiblas_chain_csrscl(void* n, void* sa, void* sx, void* incx){flexiblas_chain_csrscl_((void*) n, (void*) sa, (void*) sx, (void*) incx);}
#endif



