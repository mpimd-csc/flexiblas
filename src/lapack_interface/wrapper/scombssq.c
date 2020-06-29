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
 /* Generated: Mon Jun  8 14:12:34 2020 */
        
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



static TLS_STORE uint8_t hook_pos_scombssq = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(scombssq,SCOMBSSQ)(float* v1, float* v2)
#else
void FC_GLOBAL(scombssq,SCOMBSSQ)(float* v1, float* v2)
#endif
{
	void (*fn) (void* v1, void* v2);
	void (*fn_hook) (void* v1, void* v2);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.scombssq.f77_blas_function; 
	fn_hook = __flexiblas_hooks->scombssq.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) v1, (void*) v2); 
		return;
	} else {
		hook_pos_scombssq = 0;
		fn_hook((void*) v1, (void*) v2);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void scombssq_(float* v1, float* v2) __attribute__((alias(MTS(FC_GLOBAL(scombssq,SCOMBSSQ)))));
#else
void scombssq(float* v1, float* v2) __attribute__((alias(MTS(FC_GLOBAL(scombssq,SCOMBSSQ)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_scombssq_(void* v1, void* v2)
{
	void (*fn) (void* v1, void* v2);

	fn = current_backend->lapack.scombssq.f77_blas_function; 

		fn((void*) v1, (void*) v2); 

	return;
}

void flexiblas_real_scombssq(void* v1, void* v2)  __attribute__((alias("flexiblas_real_scombssq_")));





/* Chainloader for Hooks */


void flexiblas_chain_scombssq_(void* v1, void* v2)
{
	void (*fn) (void* v1, void* v2);
	void (*fn_hook) (void* v1, void* v2);

	fn      = current_backend->lapack.scombssq.f77_blas_function; 

    hook_pos_scombssq ++;
    if( hook_pos_scombssq < __flexiblas_hooks->scombssq.nhook) {
        fn_hook = __flexiblas_hooks->scombssq.f77_hook_function[hook_pos_scombssq];
        fn_hook((void*) v1, (void*) v2);
    } else {
        hook_pos_scombssq = 0;
		fn((void*) v1, (void*) v2); 
	}
	return;
}

void flexiblas_chain_scombssq(void* v1, void* v2)  __attribute__((alias("flexiblas_chain_scombssq_")));




