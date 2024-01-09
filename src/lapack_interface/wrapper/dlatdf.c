//    SPDX-License-Identifier: LGPL-3.0-or-later
/*
    This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
    Copyright (C) 2013-2024 Martin Koehler

    This program is free software: you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by the Free
    Software Foundation, either version 3 of the License, or (at your option)
    any later version.

    This program is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
    more details.

    You should have received a copy of the GNU General Public License along
    with this program. If not, see <https://www.gnu.org/licenses/>.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <complex.h>

#include "flexiblas_fortran_mangle.h"

#include "flexiblas.h"


#if __GNUC__ > 7
typedef size_t fortran_charlen_t;
#else
typedef int fortran_charlen_t;
#endif

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
	*(void **) & fn = current_backend->lapack.dlatdf.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dlatdf.f77_hook_function[0]; 
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
#ifndef __APPLE__
void dlatdf(blasint* ijob, blasint* n, double* z, blasint* ldz, double* rhs, double* rdsum, double* rdscal, blasint* ipiv, blasint* jpiv) __attribute__((alias(MTS(FC_GLOBAL(dlatdf,DLATDF)))));
#else
void dlatdf(blasint* ijob, blasint* n, double* z, blasint* ldz, double* rhs, double* rdsum, double* rdscal, blasint* ipiv, blasint* jpiv){ FC_GLOBAL(dlatdf,DLATDF)((void*) ijob, (void*) n, (void*) z, (void*) ldz, (void*) rhs, (void*) rdsum, (void*) rdscal, (void*) ipiv, (void*) jpiv); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlatdf_(void* ijob, void* n, void* z, void* ldz, void* rhs, void* rdsum, void* rdscal, void* ipiv, void* jpiv)
{
	void (*fn) (void* ijob, void* n, void* z, void* ldz, void* rhs, void* rdsum, void* rdscal, void* ipiv, void* jpiv);

	*(void **) & fn = current_backend->lapack.dlatdf.f77_blas_function; 

		fn((void*) ijob, (void*) n, (void*) z, (void*) ldz, (void*) rhs, (void*) rdsum, (void*) rdscal, (void*) ipiv, (void*) jpiv); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_dlatdf(void* ijob, void* n, void* z, void* ldz, void* rhs, void* rdsum, void* rdscal, void* ipiv, void* jpiv) __attribute__((alias("flexiblas_real_dlatdf_")));
#else
void flexiblas_real_dlatdf(void* ijob, void* n, void* z, void* ldz, void* rhs, void* rdsum, void* rdscal, void* ipiv, void* jpiv){flexiblas_real_dlatdf_((void*) ijob, (void*) n, (void*) z, (void*) ldz, (void*) rhs, (void*) rdsum, (void*) rdscal, (void*) ipiv, (void*) jpiv);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlatdf_(void* ijob, void* n, void* z, void* ldz, void* rhs, void* rdsum, void* rdscal, void* ipiv, void* jpiv)
{
	void (*fn) (void* ijob, void* n, void* z, void* ldz, void* rhs, void* rdsum, void* rdscal, void* ipiv, void* jpiv);
	void (*fn_hook) (void* ijob, void* n, void* z, void* ldz, void* rhs, void* rdsum, void* rdscal, void* ipiv, void* jpiv);

	*(void **) &fn      = current_backend->lapack.dlatdf.f77_blas_function; 

    hook_pos_dlatdf ++;
    if( hook_pos_dlatdf < __flexiblas_hooks->dlatdf.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlatdf.f77_hook_function[hook_pos_dlatdf];
        fn_hook((void*) ijob, (void*) n, (void*) z, (void*) ldz, (void*) rhs, (void*) rdsum, (void*) rdscal, (void*) ipiv, (void*) jpiv);
    } else {
        hook_pos_dlatdf = 0;
		fn((void*) ijob, (void*) n, (void*) z, (void*) ldz, (void*) rhs, (void*) rdsum, (void*) rdscal, (void*) ipiv, (void*) jpiv); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_dlatdf(void* ijob, void* n, void* z, void* ldz, void* rhs, void* rdsum, void* rdscal, void* ipiv, void* jpiv) __attribute__((alias("flexiblas_chain_dlatdf_")));
#else
void flexiblas_chain_dlatdf(void* ijob, void* n, void* z, void* ldz, void* rhs, void* rdsum, void* rdscal, void* ipiv, void* jpiv){flexiblas_chain_dlatdf_((void*) ijob, (void*) n, (void*) z, (void*) ldz, (void*) rhs, (void*) rdsum, (void*) rdscal, (void*) ipiv, (void*) jpiv);}
#endif



