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



static TLS_STORE uint8_t hook_pos_disnan = 0;
#ifdef FLEXIBLAS_ABI_INTEL
int FC_GLOBAL(disnan,DISNAN)(double* din)
#else
int FC_GLOBAL(disnan,DISNAN)(double* din)
#endif
{
	blasint (*fn) (void* din);
	blasint (*fn_hook) (void* din);
	blasint ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.disnan.f77_blas_function; 
	fn_hook = __flexiblas_hooks->disnan.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		ret = fn((void*) din); 
		return ret; 
	} else {
		hook_pos_disnan = 0;
		ret=fn_hook((void*) din);
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
int disnan_(double* din) __attribute__((alias(MTS(FC_GLOBAL(disnan,DISNAN)))));
#else
int disnan(double* din) __attribute__((alias(MTS(FC_GLOBAL(disnan,DISNAN)))));
#endif




/* Real Implementation for Hooks */


blasint flexiblas_real_disnan_(void* din)
{
	blasint (*fn) (void* din);
	blasint ret;

	fn = current_backend->lapack.disnan.f77_blas_function; 

		ret = fn((void*) din); 

	return ret ;
}

blasint flexiblas_real_disnan(void* din)  __attribute__((alias("flexiblas_real_disnan_")));





/* Chainloader for Hooks */


blasint flexiblas_chain_disnan_(void* din)
{
	blasint (*fn) (void* din);
	blasint (*fn_hook) (void* din);
	blasint ret;

	fn      = current_backend->lapack.disnan.f77_blas_function; 

    hook_pos_disnan ++;
    if( hook_pos_disnan < __flexiblas_hooks->disnan.nhook) {
        fn_hook = __flexiblas_hooks->disnan.f77_hook_function[hook_pos_disnan];
        ret = fn_hook((void*) din);
    } else {
        hook_pos_disnan = 0;
		ret = fn((void*) din); 
	}
	return ret ;
}

blasint flexiblas_chain_disnan(void* din)  __attribute__((alias("flexiblas_chain_disnan_")));




