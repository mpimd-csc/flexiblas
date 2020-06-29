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



static TLS_STORE uint8_t hook_pos_xerbla_array = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL_(xerbla_array,XERBLA_ARRAY)(char* srname_array, blasint* srname_len, blasint* info, blasint len_srname_array)
#else
void FC_GLOBAL_(xerbla_array,XERBLA_ARRAY)(char* srname_array, blasint* srname_len, blasint* info, blasint len_srname_array)
#endif
{
	void (*fn) (void* srname_array, void* srname_len, void* info, blasint len_srname_array);
	void (*fn_hook) (void* srname_array, void* srname_len, void* info, blasint len_srname_array);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.xerbla_array.f77_blas_function; 
	fn_hook = __flexiblas_hooks->xerbla_array.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) srname_array, (void*) srname_len, (void*) info, (blasint) len_srname_array); 
		return;
	} else {
		hook_pos_xerbla_array = 0;
		fn_hook((void*) srname_array, (void*) srname_len, (void*) info, (blasint) len_srname_array);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void xerbla_array_(char* srname_array, blasint* srname_len, blasint* info, blasint len_srname_array) __attribute__((alias(MTS(FC_GLOBAL_(xerbla_array,XERBLA_ARRAY)))));
#else
void xerbla_array(char* srname_array, blasint* srname_len, blasint* info, blasint len_srname_array) __attribute__((alias(MTS(FC_GLOBAL_(xerbla_array,XERBLA_ARRAY)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_xerbla_array_(void* srname_array, void* srname_len, void* info, blasint len_srname_array)
{
	void (*fn) (void* srname_array, void* srname_len, void* info, blasint len_srname_array);

	fn = current_backend->lapack.xerbla_array.f77_blas_function; 

		fn((void*) srname_array, (void*) srname_len, (void*) info, (blasint) len_srname_array); 

	return;
}

void flexiblas_real_xerbla_array(void* srname_array, void* srname_len, void* info, blasint len_srname_array)  __attribute__((alias("flexiblas_real_xerbla_array_")));





/* Chainloader for Hooks */


void flexiblas_chain_xerbla_array_(void* srname_array, void* srname_len, void* info, blasint len_srname_array)
{
	void (*fn) (void* srname_array, void* srname_len, void* info, blasint len_srname_array);
	void (*fn_hook) (void* srname_array, void* srname_len, void* info, blasint len_srname_array);

	fn      = current_backend->lapack.xerbla_array.f77_blas_function; 

    hook_pos_xerbla_array ++;
    if( hook_pos_xerbla_array < __flexiblas_hooks->xerbla_array.nhook) {
        fn_hook = __flexiblas_hooks->xerbla_array.f77_hook_function[hook_pos_xerbla_array];
        fn_hook((void*) srname_array, (void*) srname_len, (void*) info, (blasint) len_srname_array);
    } else {
        hook_pos_xerbla_array = 0;
		fn((void*) srname_array, (void*) srname_len, (void*) info, (blasint) len_srname_array); 
	}
	return;
}

void flexiblas_chain_xerbla_array(void* srname_array, void* srname_len, void* info, blasint len_srname_array)  __attribute__((alias("flexiblas_chain_xerbla_array_")));




