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
 /* Generated: Wed Mar 28 11:20:05 2018 */
        
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



static TLS_STORE uint8_t hook_pos_zlanhs = 0;
#ifdef FLEXIBLAS_ABI_INTEL
double FC_GLOBAL(zlanhs,ZLANHS)(char* norm, blasint* n, double complex* a, blasint* lda, double* work)
#else
double FC_GLOBAL(zlanhs,ZLANHS)(char* norm, blasint* n, double complex* a, blasint* lda, double* work)
#endif
{
	double (*fn) (void* norm, void* n, void* a, void* lda, void* work);
	double (*fn_hook) (void* norm, void* n, void* a, void* lda, void* work);
	double ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.zlanhs.f77_blas_function; 
	fn_hook = __flexiblas_hooks->zlanhs.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		ret = fn((void*) norm, (void*) n, (void*) a, (void*) lda, (void*) work); 
		return ret; 
	} else {
		hook_pos_zlanhs = 0;
		ret=fn_hook((void*) norm, (void*) n, (void*) a, (void*) lda, (void*) work);
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
double zlanhs_(char* norm, blasint* n, double complex* a, blasint* lda, double* work) __attribute__((alias(MTS(FC_GLOBAL(zlanhs,ZLANHS)))));
#else
double zlanhs(char* norm, blasint* n, double complex* a, blasint* lda, double* work) __attribute__((alias(MTS(FC_GLOBAL(zlanhs,ZLANHS)))));
#endif




/* Real Implementation for Hooks */


double flexiblas_real_zlanhs_(void* norm, void* n, void* a, void* lda, void* work)
{
	double (*fn) (void* norm, void* n, void* a, void* lda, void* work);
	double ret;

	fn = current_backend->lapack.zlanhs.f77_blas_function; 

		ret = fn((void*) norm, (void*) n, (void*) a, (void*) lda, (void*) work); 

	return ret ;
}

double flexiblas_real_zlanhs(void* norm, void* n, void* a, void* lda, void* work)  __attribute__((alias("flexiblas_real_zlanhs_")));





/* Chainloader for Hooks */


double flexiblas_chain_zlanhs_(void* norm, void* n, void* a, void* lda, void* work)
{
	double (*fn) (void* norm, void* n, void* a, void* lda, void* work);
	double (*fn_hook) (void* norm, void* n, void* a, void* lda, void* work);
	double ret;

	fn      = current_backend->lapack.zlanhs.f77_blas_function; 

    hook_pos_zlanhs ++;
    if( hook_pos_zlanhs < __flexiblas_hooks->zlanhs.nhook) {
        fn_hook = __flexiblas_hooks->zlanhs.f77_hook_function[hook_pos_zlanhs];
        ret = fn_hook((void*) norm, (void*) n, (void*) a, (void*) lda, (void*) work);
    } else {
        hook_pos_zlanhs = 0;
		ret = fn((void*) norm, (void*) n, (void*) a, (void*) lda, (void*) work); 
	}
	return ret ;
}

double flexiblas_chain_zlanhs(void* norm, void* n, void* a, void* lda, void* work)  __attribute__((alias("flexiblas_chain_zlanhs_")));




