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



static TLS_STORE uint8_t hook_pos_sgtts2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(sgtts2,SGTTS2)(blasint* itrans, blasint* n, blasint* nrhs, float* dl, float* d, float* du, float* du2, blasint* ipiv, float* b, blasint* ldb)
#else
void FC_GLOBAL(sgtts2,SGTTS2)(blasint* itrans, blasint* n, blasint* nrhs, float* dl, float* d, float* du, float* du2, blasint* ipiv, float* b, blasint* ldb)
#endif
{
	void (*fn) (void* itrans, void* n, void* nrhs, void* dl, void* d, void* du, void* du2, void* ipiv, void* b, void* ldb);
	void (*fn_hook) (void* itrans, void* n, void* nrhs, void* dl, void* d, void* du, void* du2, void* ipiv, void* b, void* ldb);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.sgtts2.f77_blas_function; 
	fn_hook = __flexiblas_hooks->sgtts2.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) itrans, (void*) n, (void*) nrhs, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) b, (void*) ldb); 
		return;
	} else {
		hook_pos_sgtts2 = 0;
		fn_hook((void*) itrans, (void*) n, (void*) nrhs, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) b, (void*) ldb);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void sgtts2_(blasint* itrans, blasint* n, blasint* nrhs, float* dl, float* d, float* du, float* du2, blasint* ipiv, float* b, blasint* ldb) __attribute__((alias(MTS(FC_GLOBAL(sgtts2,SGTTS2)))));
#else
void sgtts2(blasint* itrans, blasint* n, blasint* nrhs, float* dl, float* d, float* du, float* du2, blasint* ipiv, float* b, blasint* ldb) __attribute__((alias(MTS(FC_GLOBAL(sgtts2,SGTTS2)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_sgtts2_(void* itrans, void* n, void* nrhs, void* dl, void* d, void* du, void* du2, void* ipiv, void* b, void* ldb)
{
	void (*fn) (void* itrans, void* n, void* nrhs, void* dl, void* d, void* du, void* du2, void* ipiv, void* b, void* ldb);

	fn = current_backend->lapack.sgtts2.f77_blas_function; 

		fn((void*) itrans, (void*) n, (void*) nrhs, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) b, (void*) ldb); 

	return;
}

void flexiblas_real_sgtts2(void* itrans, void* n, void* nrhs, void* dl, void* d, void* du, void* du2, void* ipiv, void* b, void* ldb)  __attribute__((alias("flexiblas_real_sgtts2_")));





/* Chainloader for Hooks */


void flexiblas_chain_sgtts2_(void* itrans, void* n, void* nrhs, void* dl, void* d, void* du, void* du2, void* ipiv, void* b, void* ldb)
{
	void (*fn) (void* itrans, void* n, void* nrhs, void* dl, void* d, void* du, void* du2, void* ipiv, void* b, void* ldb);
	void (*fn_hook) (void* itrans, void* n, void* nrhs, void* dl, void* d, void* du, void* du2, void* ipiv, void* b, void* ldb);

	fn      = current_backend->lapack.sgtts2.f77_blas_function; 

    hook_pos_sgtts2 ++;
    if( hook_pos_sgtts2 < __flexiblas_hooks->sgtts2.nhook) {
        fn_hook = __flexiblas_hooks->sgtts2.f77_hook_function[hook_pos_sgtts2];
        fn_hook((void*) itrans, (void*) n, (void*) nrhs, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) b, (void*) ldb);
    } else {
        hook_pos_sgtts2 = 0;
		fn((void*) itrans, (void*) n, (void*) nrhs, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) b, (void*) ldb); 
	}
	return;
}

void flexiblas_chain_sgtts2(void* itrans, void* n, void* nrhs, void* dl, void* d, void* du, void* du2, void* ipiv, void* b, void* ldb)  __attribute__((alias("flexiblas_chain_sgtts2_")));




