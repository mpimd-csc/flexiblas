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



static TLS_STORE uint8_t hook_pos_chla_transtype = 0;
#ifdef FLEXIBLAS_ABI_INTEL
char FC_GLOBAL_(chla_transtype,CHLA_TRANSTYPE)(blasint* trans)
#else
char FC_GLOBAL_(chla_transtype,CHLA_TRANSTYPE)(blasint* trans)
#endif
{
	char (*fn) (void* trans);
	char (*fn_hook) (void* trans);
	char ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.chla_transtype.f77_blas_function; 
	fn_hook = __flexiblas_hooks->chla_transtype.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		ret = fn((void*) trans); 
		return ret; 
	} else {
		hook_pos_chla_transtype = 0;
		ret=fn_hook((void*) trans);
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
char chla_transtype_(blasint* trans) __attribute__((alias(MTS(FC_GLOBAL_(chla_transtype,CHLA_TRANSTYPE)))));
#else
char chla_transtype(blasint* trans) __attribute__((alias(MTS(FC_GLOBAL_(chla_transtype,CHLA_TRANSTYPE)))));
#endif




/* Real Implementation for Hooks */


char flexiblas_real_chla_transtype_(void* trans)
{
	char (*fn) (void* trans);
	char ret;

	fn = current_backend->lapack.chla_transtype.f77_blas_function; 

		ret = fn((void*) trans); 

	return ret ;
}

char flexiblas_real_chla_transtype(void* trans)  __attribute__((alias("flexiblas_real_chla_transtype_")));





/* Chainloader for Hooks */


char flexiblas_chain_chla_transtype_(void* trans)
{
	char (*fn) (void* trans);
	char (*fn_hook) (void* trans);
	char ret;

	fn      = current_backend->lapack.chla_transtype.f77_blas_function; 

    hook_pos_chla_transtype ++;
    if( hook_pos_chla_transtype < __flexiblas_hooks->chla_transtype.nhook) {
        fn_hook = __flexiblas_hooks->chla_transtype.f77_hook_function[hook_pos_chla_transtype];
        ret = fn_hook((void*) trans);
    } else {
        hook_pos_chla_transtype = 0;
		ret = fn((void*) trans); 
	}
	return ret ;
}

char flexiblas_chain_chla_transtype(void* trans)  __attribute__((alias("flexiblas_chain_chla_transtype_")));




