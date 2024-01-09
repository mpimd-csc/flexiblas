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



static TLS_STORE uint8_t hook_pos_zladiv = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zladiv,ZLADIV)( double complex* returnvalue, double complex* x, double complex* y)
#else
double complex FC_GLOBAL(zladiv,ZLADIV)(double complex* x, double complex* y)
#endif
{
	double complex (*fn) (void* x, void* y);
	void (*fn_intel) (double complex *ret, void* x, void* y);
	void (*fn_hook) (double complex *ret, void* x, void* y);
	double complex ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.zladiv.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->zladiv.f77_hook_function[0]; 
	*(void **) & fn_intel = *(void **) &fn;
	if ( fn_hook == NULL ) { 
		if(current_backend->info.intel_interface == 0 ) {
			ret = fn((void*) x, (void*) y); 
		} else {
			fn_intel( &ret, (void*) x, (void*) y);
		}

#ifdef FLEXIBLAS_ABI_INTEL
        *returnvalue = ret;
        return;
#else
        return ret;
#endif
	} else {
		hook_pos_zladiv = 0;
		fn_hook(&ret, (void*) x, (void*) y);

#ifdef FLEXIBLAS_ABI_INTEL
        *returnvalue = ret;
        return;
#else
        return ret;
#endif
	}
}
#ifdef FLEXIBLAS_ABI_IBM
double complex zladiv_(double complex* x, double complex* y) __attribute__((alias(MTS(FC_GLOBAL(zladiv,ZLADIV)))));
#else
#ifndef __APPLE__
double complex zladiv(double complex* x, double complex* y) __attribute__((alias(MTS(FC_GLOBAL(zladiv,ZLADIV)))));
#else
double complex zladiv(double complex* x, double complex* y){ return FC_GLOBAL(zladiv,ZLADIV)((void*) x, (void*) y); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zladiv_( double complex*returnvalue, void* x, void* y)
{
	double complex (*fn) (void* x, void* y);
	void (*fn_intel) (double complex *ret, void* x, void* y);
	double complex ret;

	*(void **) & fn = current_backend->lapack.zladiv.f77_blas_function; 
	*(void **) & fn_intel = *(void **) &fn;

		if(current_backend->info.intel_interface == 0 ) {
			ret = fn((void*) x, (void*) y); 
		} else {
			fn_intel( &ret, (void*) x, (void*) y);
		}


    *((double complex *)returnvalue) = ret;
    return;
}
#ifndef __APPLE__
void flexiblas_real_zladiv( double complex*returnvalue, void* x, void* y) __attribute__((alias("flexiblas_real_zladiv_")));
#else
void flexiblas_real_zladiv( double complex*returnvalue, void* x, void* y){flexiblas_real_zladiv_( (double complex*)returnvalue, (void*) x, (void*) y);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zladiv_( double complex* returnvalue, void* x, void* y)
{
	double complex (*fn) (void* x, void* y);
	void (*fn_intel) (double complex *ret, void* x, void* y);
	void (*fn_hook) (double complex *ret, void* x, void* y);
	double complex ret;

	*(void **) &fn      = current_backend->lapack.zladiv.f77_blas_function; 
	*(void **) & fn_intel = *(void **) &fn;

    hook_pos_zladiv ++;
    if( hook_pos_zladiv < __flexiblas_hooks->zladiv.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zladiv.f77_hook_function[hook_pos_zladiv];
        fn_hook( &ret, (void*) x, (void*) y);
    } else {
        hook_pos_zladiv = 0;
		if(current_backend->info.intel_interface == 0 ) {
			ret = fn((void*) x, (void*) y); 
		} else {
			fn_intel( &ret, (void*) x, (void*) y);
		}
	}

    *((double complex *)returnvalue) = ret;
    return;
}
#ifndef __APPLE__
void flexiblas_chain_zladiv( double complex*returnvalue, void* x, void* y) __attribute__((alias("flexiblas_chain_zladiv_")));
#else
void flexiblas_chain_zladiv( double complex*returnvalue, void* x, void* y){flexiblas_chain_zladiv_( (double complex*)returnvalue, (void*) x, (void*) y);}
#endif



