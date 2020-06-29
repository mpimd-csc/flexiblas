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



static TLS_STORE uint8_t hook_pos_dlamc3 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
double FC_GLOBAL(dlamc3,DLAMC3)(double* a, double* b)
#else
double FC_GLOBAL(dlamc3,DLAMC3)(double* a, double* b)
#endif
{
	double (*fn) (void* a, void* b);
	double (*fn_hook) (void* a, void* b);
	double ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.dlamc3.f77_blas_function; 
	fn_hook = __flexiblas_hooks->dlamc3.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		ret = fn((void*) a, (void*) b); 
		return ret; 
	} else {
		hook_pos_dlamc3 = 0;
		ret=fn_hook((void*) a, (void*) b);
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
double dlamc3_(double* a, double* b) __attribute__((alias(MTS(FC_GLOBAL(dlamc3,DLAMC3)))));
#else
double dlamc3(double* a, double* b) __attribute__((alias(MTS(FC_GLOBAL(dlamc3,DLAMC3)))));
#endif




/* Real Implementation for Hooks */


double flexiblas_real_dlamc3_(void* a, void* b)
{
	double (*fn) (void* a, void* b);
	double ret;

	fn = current_backend->lapack.dlamc3.f77_blas_function; 

		ret = fn((void*) a, (void*) b); 

	return ret ;
}

double flexiblas_real_dlamc3(void* a, void* b)  __attribute__((alias("flexiblas_real_dlamc3_")));





/* Chainloader for Hooks */


double flexiblas_chain_dlamc3_(void* a, void* b)
{
	double (*fn) (void* a, void* b);
	double (*fn_hook) (void* a, void* b);
	double ret;

	fn      = current_backend->lapack.dlamc3.f77_blas_function; 

    hook_pos_dlamc3 ++;
    if( hook_pos_dlamc3 < __flexiblas_hooks->dlamc3.nhook) {
        fn_hook = __flexiblas_hooks->dlamc3.f77_hook_function[hook_pos_dlamc3];
        ret = fn_hook((void*) a, (void*) b);
    } else {
        hook_pos_dlamc3 = 0;
		ret = fn((void*) a, (void*) b); 
	}
	return ret ;
}

double flexiblas_chain_dlamc3(void* a, void* b)  __attribute__((alias("flexiblas_chain_dlamc3_")));




