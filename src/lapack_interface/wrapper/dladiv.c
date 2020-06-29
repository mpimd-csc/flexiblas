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



static TLS_STORE uint8_t hook_pos_dladiv = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dladiv,DLADIV)(double* a, double* b, double* c, double* d, double* p, double* q)
#else
void FC_GLOBAL(dladiv,DLADIV)(double* a, double* b, double* c, double* d, double* p, double* q)
#endif
{
	void (*fn) (void* a, void* b, void* c, void* d, void* p, void* q);
	void (*fn_hook) (void* a, void* b, void* c, void* d, void* p, void* q);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.dladiv.f77_blas_function; 
	fn_hook = __flexiblas_hooks->dladiv.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) a, (void*) b, (void*) c, (void*) d, (void*) p, (void*) q); 
		return;
	} else {
		hook_pos_dladiv = 0;
		fn_hook((void*) a, (void*) b, (void*) c, (void*) d, (void*) p, (void*) q);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dladiv_(double* a, double* b, double* c, double* d, double* p, double* q) __attribute__((alias(MTS(FC_GLOBAL(dladiv,DLADIV)))));
#else
void dladiv(double* a, double* b, double* c, double* d, double* p, double* q) __attribute__((alias(MTS(FC_GLOBAL(dladiv,DLADIV)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dladiv_(void* a, void* b, void* c, void* d, void* p, void* q)
{
	void (*fn) (void* a, void* b, void* c, void* d, void* p, void* q);

	fn = current_backend->lapack.dladiv.f77_blas_function; 

		fn((void*) a, (void*) b, (void*) c, (void*) d, (void*) p, (void*) q); 

	return;
}

void flexiblas_real_dladiv(void* a, void* b, void* c, void* d, void* p, void* q)  __attribute__((alias("flexiblas_real_dladiv_")));





/* Chainloader for Hooks */


void flexiblas_chain_dladiv_(void* a, void* b, void* c, void* d, void* p, void* q)
{
	void (*fn) (void* a, void* b, void* c, void* d, void* p, void* q);
	void (*fn_hook) (void* a, void* b, void* c, void* d, void* p, void* q);

	fn      = current_backend->lapack.dladiv.f77_blas_function; 

    hook_pos_dladiv ++;
    if( hook_pos_dladiv < __flexiblas_hooks->dladiv.nhook) {
        fn_hook = __flexiblas_hooks->dladiv.f77_hook_function[hook_pos_dladiv];
        fn_hook((void*) a, (void*) b, (void*) c, (void*) d, (void*) p, (void*) q);
    } else {
        hook_pos_dladiv = 0;
		fn((void*) a, (void*) b, (void*) c, (void*) d, (void*) p, (void*) q); 
	}
	return;
}

void flexiblas_chain_dladiv(void* a, void* b, void* c, void* d, void* p, void* q)  __attribute__((alias("flexiblas_chain_dladiv_")));




