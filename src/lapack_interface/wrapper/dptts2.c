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



static TLS_STORE uint8_t hook_pos_dptts2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dptts2,DPTTS2)(blasint* n, blasint* nrhs, double* d, double* e, double* b, blasint* ldb)
#else
void FC_GLOBAL(dptts2,DPTTS2)(blasint* n, blasint* nrhs, double* d, double* e, double* b, blasint* ldb)
#endif
{
	void (*fn) (void* n, void* nrhs, void* d, void* e, void* b, void* ldb);
	void (*fn_hook) (void* n, void* nrhs, void* d, void* e, void* b, void* ldb);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.dptts2.f77_blas_function; 
	fn_hook = __flexiblas_hooks->dptts2.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) b, (void*) ldb); 
		return;
	} else {
		hook_pos_dptts2 = 0;
		fn_hook((void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) b, (void*) ldb);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dptts2_(blasint* n, blasint* nrhs, double* d, double* e, double* b, blasint* ldb) __attribute__((alias(MTS(FC_GLOBAL(dptts2,DPTTS2)))));
#else
void dptts2(blasint* n, blasint* nrhs, double* d, double* e, double* b, blasint* ldb) __attribute__((alias(MTS(FC_GLOBAL(dptts2,DPTTS2)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dptts2_(void* n, void* nrhs, void* d, void* e, void* b, void* ldb)
{
	void (*fn) (void* n, void* nrhs, void* d, void* e, void* b, void* ldb);

	fn = current_backend->lapack.dptts2.f77_blas_function; 

		fn((void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) b, (void*) ldb); 

	return;
}

void flexiblas_real_dptts2(void* n, void* nrhs, void* d, void* e, void* b, void* ldb)  __attribute__((alias("flexiblas_real_dptts2_")));





/* Chainloader for Hooks */


void flexiblas_chain_dptts2_(void* n, void* nrhs, void* d, void* e, void* b, void* ldb)
{
	void (*fn) (void* n, void* nrhs, void* d, void* e, void* b, void* ldb);
	void (*fn_hook) (void* n, void* nrhs, void* d, void* e, void* b, void* ldb);

	fn      = current_backend->lapack.dptts2.f77_blas_function; 

    hook_pos_dptts2 ++;
    if( hook_pos_dptts2 < __flexiblas_hooks->dptts2.nhook) {
        fn_hook = __flexiblas_hooks->dptts2.f77_hook_function[hook_pos_dptts2];
        fn_hook((void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) b, (void*) ldb);
    } else {
        hook_pos_dptts2 = 0;
		fn((void*) n, (void*) nrhs, (void*) d, (void*) e, (void*) b, (void*) ldb); 
	}
	return;
}

void flexiblas_chain_dptts2(void* n, void* nrhs, void* d, void* e, void* b, void* ldb)  __attribute__((alias("flexiblas_chain_dptts2_")));




