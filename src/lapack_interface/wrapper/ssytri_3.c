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



static TLS_STORE uint8_t hook_pos_ssytri_3 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL_(ssytri_3,SSYTRI_3)(char* uplo, blasint* n, float* a, blasint* lda, float* e, blasint* ipiv, float* work, blasint* lwork, blasint* info)
#else
void FC_GLOBAL_(ssytri_3,SSYTRI_3)(char* uplo, blasint* n, float* a, blasint* lda, float* e, blasint* ipiv, float* work, blasint* lwork, blasint* info)
#endif
{
	void (*fn) (void* uplo, void* n, void* a, void* lda, void* e, void* ipiv, void* work, void* lwork, void* info);
	void (*fn_hook) (void* uplo, void* n, void* a, void* lda, void* e, void* ipiv, void* work, void* lwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.ssytri_3.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->ssytri_3.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) e, (void*) ipiv, (void*) work, (void*) lwork, (void*) info); 
		return;
	} else {
		hook_pos_ssytri_3 = 0;
		fn_hook((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) e, (void*) ipiv, (void*) work, (void*) lwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void ssytri_3_(char* uplo, blasint* n, float* a, blasint* lda, float* e, blasint* ipiv, float* work, blasint* lwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL_(ssytri_3,SSYTRI_3)))));
#else
#ifndef __APPLE__
void ssytri_3(char* uplo, blasint* n, float* a, blasint* lda, float* e, blasint* ipiv, float* work, blasint* lwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL_(ssytri_3,SSYTRI_3)))));
#else
void ssytri_3(char* uplo, blasint* n, float* a, blasint* lda, float* e, blasint* ipiv, float* work, blasint* lwork, blasint* info){ FC_GLOBAL_(ssytri_3,SSYTRI_3)((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) e, (void*) ipiv, (void*) work, (void*) lwork, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_ssytri_3_(void* uplo, void* n, void* a, void* lda, void* e, void* ipiv, void* work, void* lwork, void* info)
{
	void (*fn) (void* uplo, void* n, void* a, void* lda, void* e, void* ipiv, void* work, void* lwork, void* info);

	*(void **) & fn = current_backend->lapack.ssytri_3.f77_blas_function; 

		fn((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) e, (void*) ipiv, (void*) work, (void*) lwork, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_ssytri_3(void* uplo, void* n, void* a, void* lda, void* e, void* ipiv, void* work, void* lwork, void* info) __attribute__((alias("flexiblas_real_ssytri_3_")));
#else
void flexiblas_real_ssytri_3(void* uplo, void* n, void* a, void* lda, void* e, void* ipiv, void* work, void* lwork, void* info){flexiblas_real_ssytri_3_((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) e, (void*) ipiv, (void*) work, (void*) lwork, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_ssytri_3_(void* uplo, void* n, void* a, void* lda, void* e, void* ipiv, void* work, void* lwork, void* info)
{
	void (*fn) (void* uplo, void* n, void* a, void* lda, void* e, void* ipiv, void* work, void* lwork, void* info);
	void (*fn_hook) (void* uplo, void* n, void* a, void* lda, void* e, void* ipiv, void* work, void* lwork, void* info);

	*(void **) &fn      = current_backend->lapack.ssytri_3.f77_blas_function; 

    hook_pos_ssytri_3 ++;
    if( hook_pos_ssytri_3 < __flexiblas_hooks->ssytri_3.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->ssytri_3.f77_hook_function[hook_pos_ssytri_3];
        fn_hook((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) e, (void*) ipiv, (void*) work, (void*) lwork, (void*) info);
    } else {
        hook_pos_ssytri_3 = 0;
		fn((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) e, (void*) ipiv, (void*) work, (void*) lwork, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_ssytri_3(void* uplo, void* n, void* a, void* lda, void* e, void* ipiv, void* work, void* lwork, void* info) __attribute__((alias("flexiblas_chain_ssytri_3_")));
#else
void flexiblas_chain_ssytri_3(void* uplo, void* n, void* a, void* lda, void* e, void* ipiv, void* work, void* lwork, void* info){flexiblas_chain_ssytri_3_((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) e, (void*) ipiv, (void*) work, (void*) lwork, (void*) info);}
#endif



