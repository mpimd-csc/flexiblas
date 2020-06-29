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



static TLS_STORE uint8_t hook_pos_iladiag = 0;
#ifdef FLEXIBLAS_ABI_INTEL
int FC_GLOBAL(iladiag,ILADIAG)(char* diag)
#else
int FC_GLOBAL(iladiag,ILADIAG)(char* diag)
#endif
{
	blasint (*fn) (void* diag);
	blasint (*fn_hook) (void* diag);
	blasint ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.iladiag.f77_blas_function; 
	fn_hook = __flexiblas_hooks->iladiag.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		ret = fn((void*) diag); 
		return ret; 
	} else {
		hook_pos_iladiag = 0;
		ret=fn_hook((void*) diag);
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
int iladiag_(char* diag) __attribute__((alias(MTS(FC_GLOBAL(iladiag,ILADIAG)))));
#else
int iladiag(char* diag) __attribute__((alias(MTS(FC_GLOBAL(iladiag,ILADIAG)))));
#endif




/* Real Implementation for Hooks */


blasint flexiblas_real_iladiag_(void* diag)
{
	blasint (*fn) (void* diag);
	blasint ret;

	fn = current_backend->lapack.iladiag.f77_blas_function; 

		ret = fn((void*) diag); 

	return ret ;
}

blasint flexiblas_real_iladiag(void* diag)  __attribute__((alias("flexiblas_real_iladiag_")));





/* Chainloader for Hooks */


blasint flexiblas_chain_iladiag_(void* diag)
{
	blasint (*fn) (void* diag);
	blasint (*fn_hook) (void* diag);
	blasint ret;

	fn      = current_backend->lapack.iladiag.f77_blas_function; 

    hook_pos_iladiag ++;
    if( hook_pos_iladiag < __flexiblas_hooks->iladiag.nhook) {
        fn_hook = __flexiblas_hooks->iladiag.f77_hook_function[hook_pos_iladiag];
        ret = fn_hook((void*) diag);
    } else {
        hook_pos_iladiag = 0;
		ret = fn((void*) diag); 
	}
	return ret ;
}

blasint flexiblas_chain_iladiag(void* diag)  __attribute__((alias("flexiblas_chain_iladiag_")));




