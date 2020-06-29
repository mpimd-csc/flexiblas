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



static TLS_STORE uint8_t hook_pos_ilauplo = 0;
#ifdef FLEXIBLAS_ABI_INTEL
int FC_GLOBAL(ilauplo,ILAUPLO)(char* uplo)
#else
int FC_GLOBAL(ilauplo,ILAUPLO)(char* uplo)
#endif
{
	blasint (*fn) (void* uplo);
	blasint (*fn_hook) (void* uplo);
	blasint ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.ilauplo.f77_blas_function; 
	fn_hook = __flexiblas_hooks->ilauplo.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		ret = fn((void*) uplo); 
		return ret; 
	} else {
		hook_pos_ilauplo = 0;
		ret=fn_hook((void*) uplo);
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
int ilauplo_(char* uplo) __attribute__((alias(MTS(FC_GLOBAL(ilauplo,ILAUPLO)))));
#else
int ilauplo(char* uplo) __attribute__((alias(MTS(FC_GLOBAL(ilauplo,ILAUPLO)))));
#endif




/* Real Implementation for Hooks */


blasint flexiblas_real_ilauplo_(void* uplo)
{
	blasint (*fn) (void* uplo);
	blasint ret;

	fn = current_backend->lapack.ilauplo.f77_blas_function; 

		ret = fn((void*) uplo); 

	return ret ;
}

blasint flexiblas_real_ilauplo(void* uplo)  __attribute__((alias("flexiblas_real_ilauplo_")));





/* Chainloader for Hooks */


blasint flexiblas_chain_ilauplo_(void* uplo)
{
	blasint (*fn) (void* uplo);
	blasint (*fn_hook) (void* uplo);
	blasint ret;

	fn      = current_backend->lapack.ilauplo.f77_blas_function; 

    hook_pos_ilauplo ++;
    if( hook_pos_ilauplo < __flexiblas_hooks->ilauplo.nhook) {
        fn_hook = __flexiblas_hooks->ilauplo.f77_hook_function[hook_pos_ilauplo];
        ret = fn_hook((void*) uplo);
    } else {
        hook_pos_ilauplo = 0;
		ret = fn((void*) uplo); 
	}
	return ret ;
}

blasint flexiblas_chain_ilauplo(void* uplo)  __attribute__((alias("flexiblas_chain_ilauplo_")));




