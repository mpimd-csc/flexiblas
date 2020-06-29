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



static TLS_STORE uint8_t hook_pos_dlatdf = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlatdf,DLATDF)(blasint* ijob, blasint* n, double* z, blasint* ldz, double* rhs, double* rdsum, double* rdscal, blasint* ipiv, blasint* jpiv)
#else
void FC_GLOBAL(dlatdf,DLATDF)(blasint* ijob, blasint* n, double* z, blasint* ldz, double* rhs, double* rdsum, double* rdscal, blasint* ipiv, blasint* jpiv)
#endif
{
	void (*fn) (void* ijob, void* n, void* z, void* ldz, void* rhs, void* rdsum, void* rdscal, void* ipiv, void* jpiv);
	void (*fn_hook) (void* ijob, void* n, void* z, void* ldz, void* rhs, void* rdsum, void* rdscal, void* ipiv, void* jpiv);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.dlatdf.f77_blas_function; 
	fn_hook = __flexiblas_hooks->dlatdf.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) ijob, (void*) n, (void*) z, (void*) ldz, (void*) rhs, (void*) rdsum, (void*) rdscal, (void*) ipiv, (void*) jpiv); 
		return;
	} else {
		hook_pos_dlatdf = 0;
		fn_hook((void*) ijob, (void*) n, (void*) z, (void*) ldz, (void*) rhs, (void*) rdsum, (void*) rdscal, (void*) ipiv, (void*) jpiv);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dlatdf_(blasint* ijob, blasint* n, double* z, blasint* ldz, double* rhs, double* rdsum, double* rdscal, blasint* ipiv, blasint* jpiv) __attribute__((alias(MTS(FC_GLOBAL(dlatdf,DLATDF)))));
#else
void dlatdf(blasint* ijob, blasint* n, double* z, blasint* ldz, double* rhs, double* rdsum, double* rdscal, blasint* ipiv, blasint* jpiv) __attribute__((alias(MTS(FC_GLOBAL(dlatdf,DLATDF)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlatdf_(void* ijob, void* n, void* z, void* ldz, void* rhs, void* rdsum, void* rdscal, void* ipiv, void* jpiv)
{
	void (*fn) (void* ijob, void* n, void* z, void* ldz, void* rhs, void* rdsum, void* rdscal, void* ipiv, void* jpiv);

	fn = current_backend->lapack.dlatdf.f77_blas_function; 

		fn((void*) ijob, (void*) n, (void*) z, (void*) ldz, (void*) rhs, (void*) rdsum, (void*) rdscal, (void*) ipiv, (void*) jpiv); 

	return;
}

void flexiblas_real_dlatdf(void* ijob, void* n, void* z, void* ldz, void* rhs, void* rdsum, void* rdscal, void* ipiv, void* jpiv)  __attribute__((alias("flexiblas_real_dlatdf_")));





/* Chainloader for Hooks */


void flexiblas_chain_dlatdf_(void* ijob, void* n, void* z, void* ldz, void* rhs, void* rdsum, void* rdscal, void* ipiv, void* jpiv)
{
	void (*fn) (void* ijob, void* n, void* z, void* ldz, void* rhs, void* rdsum, void* rdscal, void* ipiv, void* jpiv);
	void (*fn_hook) (void* ijob, void* n, void* z, void* ldz, void* rhs, void* rdsum, void* rdscal, void* ipiv, void* jpiv);

	fn      = current_backend->lapack.dlatdf.f77_blas_function; 

    hook_pos_dlatdf ++;
    if( hook_pos_dlatdf < __flexiblas_hooks->dlatdf.nhook) {
        fn_hook = __flexiblas_hooks->dlatdf.f77_hook_function[hook_pos_dlatdf];
        fn_hook((void*) ijob, (void*) n, (void*) z, (void*) ldz, (void*) rhs, (void*) rdsum, (void*) rdscal, (void*) ipiv, (void*) jpiv);
    } else {
        hook_pos_dlatdf = 0;
		fn((void*) ijob, (void*) n, (void*) z, (void*) ldz, (void*) rhs, (void*) rdsum, (void*) rdscal, (void*) ipiv, (void*) jpiv); 
	}
	return;
}

void flexiblas_chain_dlatdf(void* ijob, void* n, void* z, void* ldz, void* rhs, void* rdsum, void* rdscal, void* ipiv, void* jpiv)  __attribute__((alias("flexiblas_chain_dlatdf_")));




