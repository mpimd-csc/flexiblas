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



static TLS_STORE uint8_t hook_pos_cgesc2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(cgesc2,CGESC2)(blasint* n, float complex* a, blasint* lda, float complex* rhs, blasint* ipiv, blasint* jpiv, float* scale)
#else
void FC_GLOBAL(cgesc2,CGESC2)(blasint* n, float complex* a, blasint* lda, float complex* rhs, blasint* ipiv, blasint* jpiv, float* scale)
#endif
{
	void (*fn) (void* n, void* a, void* lda, void* rhs, void* ipiv, void* jpiv, void* scale);
	void (*fn_hook) (void* n, void* a, void* lda, void* rhs, void* ipiv, void* jpiv, void* scale);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.cgesc2.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->cgesc2.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) a, (void*) lda, (void*) rhs, (void*) ipiv, (void*) jpiv, (void*) scale); 
		return;
	} else {
		hook_pos_cgesc2 = 0;
		fn_hook((void*) n, (void*) a, (void*) lda, (void*) rhs, (void*) ipiv, (void*) jpiv, (void*) scale);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void cgesc2_(blasint* n, float complex* a, blasint* lda, float complex* rhs, blasint* ipiv, blasint* jpiv, float* scale) __attribute__((alias(MTS(FC_GLOBAL(cgesc2,CGESC2)))));
#else
#ifndef __APPLE__
void cgesc2(blasint* n, float complex* a, blasint* lda, float complex* rhs, blasint* ipiv, blasint* jpiv, float* scale) __attribute__((alias(MTS(FC_GLOBAL(cgesc2,CGESC2)))));
#else
void cgesc2(blasint* n, float complex* a, blasint* lda, float complex* rhs, blasint* ipiv, blasint* jpiv, float* scale){ FC_GLOBAL(cgesc2,CGESC2)((void*) n, (void*) a, (void*) lda, (void*) rhs, (void*) ipiv, (void*) jpiv, (void*) scale); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_cgesc2_(void* n, void* a, void* lda, void* rhs, void* ipiv, void* jpiv, void* scale)
{
	void (*fn) (void* n, void* a, void* lda, void* rhs, void* ipiv, void* jpiv, void* scale);

	*(void **) & fn = current_backend->lapack.cgesc2.f77_blas_function; 

		fn((void*) n, (void*) a, (void*) lda, (void*) rhs, (void*) ipiv, (void*) jpiv, (void*) scale); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_cgesc2(void* n, void* a, void* lda, void* rhs, void* ipiv, void* jpiv, void* scale) __attribute__((alias("flexiblas_real_cgesc2_")));
#else
void flexiblas_real_cgesc2(void* n, void* a, void* lda, void* rhs, void* ipiv, void* jpiv, void* scale){flexiblas_real_cgesc2_((void*) n, (void*) a, (void*) lda, (void*) rhs, (void*) ipiv, (void*) jpiv, (void*) scale);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_cgesc2_(void* n, void* a, void* lda, void* rhs, void* ipiv, void* jpiv, void* scale)
{
	void (*fn) (void* n, void* a, void* lda, void* rhs, void* ipiv, void* jpiv, void* scale);
	void (*fn_hook) (void* n, void* a, void* lda, void* rhs, void* ipiv, void* jpiv, void* scale);

	*(void **) &fn      = current_backend->lapack.cgesc2.f77_blas_function; 

    hook_pos_cgesc2 ++;
    if( hook_pos_cgesc2 < __flexiblas_hooks->cgesc2.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->cgesc2.f77_hook_function[hook_pos_cgesc2];
        fn_hook((void*) n, (void*) a, (void*) lda, (void*) rhs, (void*) ipiv, (void*) jpiv, (void*) scale);
    } else {
        hook_pos_cgesc2 = 0;
		fn((void*) n, (void*) a, (void*) lda, (void*) rhs, (void*) ipiv, (void*) jpiv, (void*) scale); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_cgesc2(void* n, void* a, void* lda, void* rhs, void* ipiv, void* jpiv, void* scale) __attribute__((alias("flexiblas_chain_cgesc2_")));
#else
void flexiblas_chain_cgesc2(void* n, void* a, void* lda, void* rhs, void* ipiv, void* jpiv, void* scale){flexiblas_chain_cgesc2_((void*) n, (void*) a, (void*) lda, (void*) rhs, (void*) ipiv, (void*) jpiv, (void*) scale);}
#endif



