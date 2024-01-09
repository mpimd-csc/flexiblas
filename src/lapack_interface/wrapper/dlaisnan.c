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



static TLS_STORE uint8_t hook_pos_dlaisnan = 0;
#ifdef FLEXIBLAS_ABI_INTEL
int FC_GLOBAL(dlaisnan,DLAISNAN)(double* din1, double* din2)
#else
int FC_GLOBAL(dlaisnan,DLAISNAN)(double* din1, double* din2)
#endif
{
	blasint (*fn) (void* din1, void* din2);
	blasint (*fn_hook) (void* din1, void* din2);
	blasint ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dlaisnan.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dlaisnan.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		ret = fn((void*) din1, (void*) din2); 
		return ret; 
	} else {
		hook_pos_dlaisnan = 0;
		ret=fn_hook((void*) din1, (void*) din2);
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
int dlaisnan_(double* din1, double* din2) __attribute__((alias(MTS(FC_GLOBAL(dlaisnan,DLAISNAN)))));
#else
#ifndef __APPLE__
int dlaisnan(double* din1, double* din2) __attribute__((alias(MTS(FC_GLOBAL(dlaisnan,DLAISNAN)))));
#else
int dlaisnan(double* din1, double* din2){ return FC_GLOBAL(dlaisnan,DLAISNAN)((void*) din1, (void*) din2); }
#endif
#endif




/* Real Implementation for Hooks */


blasint flexiblas_real_dlaisnan_(void* din1, void* din2)
{
	blasint (*fn) (void* din1, void* din2);
	blasint ret;

	*(void **) & fn = current_backend->lapack.dlaisnan.f77_blas_function; 

		ret = fn((void*) din1, (void*) din2); 

	return ret ;
}
#ifndef __APPLE__
blasint flexiblas_real_dlaisnan(void* din1, void* din2) __attribute__((alias("flexiblas_real_dlaisnan_")));
#else
blasint flexiblas_real_dlaisnan(void* din1, void* din2){return flexiblas_real_dlaisnan_((void*) din1, (void*) din2);}
#endif




/* Chainloader for Hooks */


blasint flexiblas_chain_dlaisnan_(void* din1, void* din2)
{
	blasint (*fn) (void* din1, void* din2);
	blasint (*fn_hook) (void* din1, void* din2);
	blasint ret;

	*(void **) &fn      = current_backend->lapack.dlaisnan.f77_blas_function; 

    hook_pos_dlaisnan ++;
    if( hook_pos_dlaisnan < __flexiblas_hooks->dlaisnan.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlaisnan.f77_hook_function[hook_pos_dlaisnan];
        ret = fn_hook((void*) din1, (void*) din2);
    } else {
        hook_pos_dlaisnan = 0;
		ret = fn((void*) din1, (void*) din2); 
	}
	return ret ;
}
#ifndef __APPLE__
blasint flexiblas_chain_dlaisnan(void* din1, void* din2) __attribute__((alias("flexiblas_chain_dlaisnan_")));
#else
blasint flexiblas_chain_dlaisnan(void* din1, void* din2){return flexiblas_chain_dlaisnan_((void*) din1, (void*) din2);}
#endif



