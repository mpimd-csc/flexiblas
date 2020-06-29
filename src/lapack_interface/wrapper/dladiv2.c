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



static TLS_STORE uint8_t hook_pos_dladiv2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
double FC_GLOBAL(dladiv2,DLADIV2)(double* a, double* b, double* c, double* d, double* r, double* t)
#else
double FC_GLOBAL(dladiv2,DLADIV2)(double* a, double* b, double* c, double* d, double* r, double* t)
#endif
{
	double (*fn) (void* a, void* b, void* c, void* d, void* r, void* t);
	double (*fn_hook) (void* a, void* b, void* c, void* d, void* r, void* t);
	double ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.dladiv2.f77_blas_function; 
	fn_hook = __flexiblas_hooks->dladiv2.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		ret = fn((void*) a, (void*) b, (void*) c, (void*) d, (void*) r, (void*) t); 
		return ret; 
	} else {
		hook_pos_dladiv2 = 0;
		ret=fn_hook((void*) a, (void*) b, (void*) c, (void*) d, (void*) r, (void*) t);
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
double dladiv2_(double* a, double* b, double* c, double* d, double* r, double* t) __attribute__((alias(MTS(FC_GLOBAL(dladiv2,DLADIV2)))));
#else
double dladiv2(double* a, double* b, double* c, double* d, double* r, double* t) __attribute__((alias(MTS(FC_GLOBAL(dladiv2,DLADIV2)))));
#endif




/* Real Implementation for Hooks */


double flexiblas_real_dladiv2_(void* a, void* b, void* c, void* d, void* r, void* t)
{
	double (*fn) (void* a, void* b, void* c, void* d, void* r, void* t);
	double ret;

	fn = current_backend->lapack.dladiv2.f77_blas_function; 

		ret = fn((void*) a, (void*) b, (void*) c, (void*) d, (void*) r, (void*) t); 

	return ret ;
}

double flexiblas_real_dladiv2(void* a, void* b, void* c, void* d, void* r, void* t)  __attribute__((alias("flexiblas_real_dladiv2_")));





/* Chainloader for Hooks */


double flexiblas_chain_dladiv2_(void* a, void* b, void* c, void* d, void* r, void* t)
{
	double (*fn) (void* a, void* b, void* c, void* d, void* r, void* t);
	double (*fn_hook) (void* a, void* b, void* c, void* d, void* r, void* t);
	double ret;

	fn      = current_backend->lapack.dladiv2.f77_blas_function; 

    hook_pos_dladiv2 ++;
    if( hook_pos_dladiv2 < __flexiblas_hooks->dladiv2.nhook) {
        fn_hook = __flexiblas_hooks->dladiv2.f77_hook_function[hook_pos_dladiv2];
        ret = fn_hook((void*) a, (void*) b, (void*) c, (void*) d, (void*) r, (void*) t);
    } else {
        hook_pos_dladiv2 = 0;
		ret = fn((void*) a, (void*) b, (void*) c, (void*) d, (void*) r, (void*) t); 
	}
	return ret ;
}

double flexiblas_chain_dladiv2(void* a, void* b, void* c, void* d, void* r, void* t)  __attribute__((alias("flexiblas_chain_dladiv2_")));




