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
 /* Generated: Wed Mar 28 11:20:04 2018 */
        
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



static TLS_STORE uint8_t hook_pos_slamch = 0;
#ifdef FLEXIBLAS_ABI_INTEL
float FC_GLOBAL(slamch,SLAMCH)(char* cmach)
#else
float FC_GLOBAL(slamch,SLAMCH)(char* cmach)
#endif
{
	float (*fn) (void* cmach);
	float (*fn_hook) (void* cmach);
	float ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.slamch.f77_blas_function; 
	fn_hook = __flexiblas_hooks->slamch.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		ret = fn((void*) cmach); 
		return ret; 
	} else {
		hook_pos_slamch = 0;
		ret=fn_hook((void*) cmach);
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
float slamch_(char* cmach) __attribute__((alias(MTS(FC_GLOBAL(slamch,SLAMCH)))));
#else
float slamch(char* cmach) __attribute__((alias(MTS(FC_GLOBAL(slamch,SLAMCH)))));
#endif




/* Real Implementation for Hooks */


float flexiblas_real_slamch_(void* cmach)
{
	float (*fn) (void* cmach);
	float ret;

	fn = current_backend->lapack.slamch.f77_blas_function; 

		ret = fn((void*) cmach); 

	return ret ;
}

float flexiblas_real_slamch(void* cmach)  __attribute__((alias("flexiblas_real_slamch_")));





/* Chainloader for Hooks */


float flexiblas_chain_slamch_(void* cmach)
{
	float (*fn) (void* cmach);
	float (*fn_hook) (void* cmach);
	float ret;

	fn      = current_backend->lapack.slamch.f77_blas_function; 

    hook_pos_slamch ++;
    if( hook_pos_slamch < __flexiblas_hooks->slamch.nhook) {
        fn_hook = __flexiblas_hooks->slamch.f77_hook_function[hook_pos_slamch];
        ret = fn_hook((void*) cmach);
    } else {
        hook_pos_slamch = 0;
		ret = fn((void*) cmach); 
	}
	return ret ;
}

float flexiblas_chain_slamch(void* cmach)  __attribute__((alias("flexiblas_chain_slamch_")));




