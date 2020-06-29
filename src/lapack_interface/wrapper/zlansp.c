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



static TLS_STORE uint8_t hook_pos_zlansp = 0;
#ifdef FLEXIBLAS_ABI_INTEL
double FC_GLOBAL(zlansp,ZLANSP)(char* norm, char* uplo, blasint* n, double complex* ap, double* work)
#else
double FC_GLOBAL(zlansp,ZLANSP)(char* norm, char* uplo, blasint* n, double complex* ap, double* work)
#endif
{
	double (*fn) (void* norm, void* uplo, void* n, void* ap, void* work);
	double (*fn_hook) (void* norm, void* uplo, void* n, void* ap, void* work);
	double ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.zlansp.f77_blas_function; 
	fn_hook = __flexiblas_hooks->zlansp.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		ret = fn((void*) norm, (void*) uplo, (void*) n, (void*) ap, (void*) work); 
		return ret; 
	} else {
		hook_pos_zlansp = 0;
		ret=fn_hook((void*) norm, (void*) uplo, (void*) n, (void*) ap, (void*) work);
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
double zlansp_(char* norm, char* uplo, blasint* n, double complex* ap, double* work) __attribute__((alias(MTS(FC_GLOBAL(zlansp,ZLANSP)))));
#else
double zlansp(char* norm, char* uplo, blasint* n, double complex* ap, double* work) __attribute__((alias(MTS(FC_GLOBAL(zlansp,ZLANSP)))));
#endif




/* Real Implementation for Hooks */


double flexiblas_real_zlansp_(void* norm, void* uplo, void* n, void* ap, void* work)
{
	double (*fn) (void* norm, void* uplo, void* n, void* ap, void* work);
	double ret;

	fn = current_backend->lapack.zlansp.f77_blas_function; 

		ret = fn((void*) norm, (void*) uplo, (void*) n, (void*) ap, (void*) work); 

	return ret ;
}

double flexiblas_real_zlansp(void* norm, void* uplo, void* n, void* ap, void* work)  __attribute__((alias("flexiblas_real_zlansp_")));





/* Chainloader for Hooks */


double flexiblas_chain_zlansp_(void* norm, void* uplo, void* n, void* ap, void* work)
{
	double (*fn) (void* norm, void* uplo, void* n, void* ap, void* work);
	double (*fn_hook) (void* norm, void* uplo, void* n, void* ap, void* work);
	double ret;

	fn      = current_backend->lapack.zlansp.f77_blas_function; 

    hook_pos_zlansp ++;
    if( hook_pos_zlansp < __flexiblas_hooks->zlansp.nhook) {
        fn_hook = __flexiblas_hooks->zlansp.f77_hook_function[hook_pos_zlansp];
        ret = fn_hook((void*) norm, (void*) uplo, (void*) n, (void*) ap, (void*) work);
    } else {
        hook_pos_zlansp = 0;
		ret = fn((void*) norm, (void*) uplo, (void*) n, (void*) ap, (void*) work); 
	}
	return ret ;
}

double flexiblas_chain_zlansp(void* norm, void* uplo, void* n, void* ap, void* work)  __attribute__((alias("flexiblas_chain_zlansp_")));




