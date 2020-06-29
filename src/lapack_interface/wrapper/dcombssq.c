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



static TLS_STORE uint8_t hook_pos_dcombssq = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dcombssq,DCOMBSSQ)(double* v1, double* v2)
#else
void FC_GLOBAL(dcombssq,DCOMBSSQ)(double* v1, double* v2)
#endif
{
	void (*fn) (void* v1, void* v2);
	void (*fn_hook) (void* v1, void* v2);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.dcombssq.f77_blas_function; 
	fn_hook = __flexiblas_hooks->dcombssq.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) v1, (void*) v2); 
		return;
	} else {
		hook_pos_dcombssq = 0;
		fn_hook((void*) v1, (void*) v2);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dcombssq_(double* v1, double* v2) __attribute__((alias(MTS(FC_GLOBAL(dcombssq,DCOMBSSQ)))));
#else
void dcombssq(double* v1, double* v2) __attribute__((alias(MTS(FC_GLOBAL(dcombssq,DCOMBSSQ)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dcombssq_(void* v1, void* v2)
{
	void (*fn) (void* v1, void* v2);

	fn = current_backend->lapack.dcombssq.f77_blas_function; 

		fn((void*) v1, (void*) v2); 

	return;
}

void flexiblas_real_dcombssq(void* v1, void* v2)  __attribute__((alias("flexiblas_real_dcombssq_")));





/* Chainloader for Hooks */


void flexiblas_chain_dcombssq_(void* v1, void* v2)
{
	void (*fn) (void* v1, void* v2);
	void (*fn_hook) (void* v1, void* v2);

	fn      = current_backend->lapack.dcombssq.f77_blas_function; 

    hook_pos_dcombssq ++;
    if( hook_pos_dcombssq < __flexiblas_hooks->dcombssq.nhook) {
        fn_hook = __flexiblas_hooks->dcombssq.f77_hook_function[hook_pos_dcombssq];
        fn_hook((void*) v1, (void*) v2);
    } else {
        hook_pos_dcombssq = 0;
		fn((void*) v1, (void*) v2); 
	}
	return;
}

void flexiblas_chain_dcombssq(void* v1, void* v2)  __attribute__((alias("flexiblas_chain_dcombssq_")));




