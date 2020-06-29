/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) Martin Koehler, 2013-2020
 */
 /* This file it automatically generated. Please do not edit. */
 /* Generated: Wed Mar 28 11:20:03 2018 */
        
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <complex.h>

#include "fortran_mangle.h"

#include "flexiblas.h"


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
	fn = current_backend->lapack.dlaisnan.f77_blas_function; 
	fn_hook = __flexiblas_hooks->dlaisnan.f77_hook_function[0]; 
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
int dlaisnan(double* din1, double* din2) __attribute__((alias(MTS(FC_GLOBAL(dlaisnan,DLAISNAN)))));
#endif




/* Real Implementation for Hooks */


blasint flexiblas_real_dlaisnan_(void* din1, void* din2)
{
	blasint (*fn) (void* din1, void* din2);
	blasint ret;

	fn = current_backend->lapack.dlaisnan.f77_blas_function; 

		ret = fn((void*) din1, (void*) din2); 

	return ret ;
}

blasint flexiblas_real_dlaisnan(void* din1, void* din2)  __attribute__((alias("flexiblas_real_dlaisnan_")));





/* Chainloader for Hooks */


blasint flexiblas_chain_dlaisnan_(void* din1, void* din2)
{
	blasint (*fn) (void* din1, void* din2);
	blasint (*fn_hook) (void* din1, void* din2);
	blasint ret;

	fn      = current_backend->lapack.dlaisnan.f77_blas_function; 

    hook_pos_dlaisnan ++;
    if( hook_pos_dlaisnan < __flexiblas_hooks->dlaisnan.nhook) {
        fn_hook = __flexiblas_hooks->dlaisnan.f77_hook_function[hook_pos_dlaisnan];
        ret = fn_hook((void*) din1, (void*) din2);
    } else {
        hook_pos_dlaisnan = 0;
		ret = fn((void*) din1, (void*) din2); 
	}
	return ret ;
}

blasint flexiblas_chain_dlaisnan(void* din1, void* din2)  __attribute__((alias("flexiblas_chain_dlaisnan_")));




