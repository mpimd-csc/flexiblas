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



static TLS_STORE uint8_t hook_pos_ssyevd = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(ssyevd,SSYEVD)(char* jobz, char* uplo, blasint* n, float* a, blasint* lda, float* w, float* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info)
#else
void FC_GLOBAL(ssyevd,SSYEVD)(char* jobz, char* uplo, blasint* n, float* a, blasint* lda, float* w, float* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info)
#endif
{
	void (*fn) (void* jobz, void* uplo, void* n, void* a, void* lda, void* w, void* work, void* lwork, void* iwork, void* liwork, void* info);
	void (*fn_hook) (void* jobz, void* uplo, void* n, void* a, void* lda, void* w, void* work, void* lwork, void* iwork, void* liwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.ssyevd.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->ssyevd.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) jobz, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) w, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info); 
		return;
	} else {
		hook_pos_ssyevd = 0;
		fn_hook((void*) jobz, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) w, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void ssyevd_(char* jobz, char* uplo, blasint* n, float* a, blasint* lda, float* w, float* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(ssyevd,SSYEVD)))));
#else
#ifndef __APPLE__
void ssyevd(char* jobz, char* uplo, blasint* n, float* a, blasint* lda, float* w, float* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(ssyevd,SSYEVD)))));
#else
void ssyevd(char* jobz, char* uplo, blasint* n, float* a, blasint* lda, float* w, float* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info){ FC_GLOBAL(ssyevd,SSYEVD)((void*) jobz, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) w, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_ssyevd_(void* jobz, void* uplo, void* n, void* a, void* lda, void* w, void* work, void* lwork, void* iwork, void* liwork, void* info)
{
	void (*fn) (void* jobz, void* uplo, void* n, void* a, void* lda, void* w, void* work, void* lwork, void* iwork, void* liwork, void* info);

	*(void **) & fn = current_backend->lapack.ssyevd.f77_blas_function; 

		fn((void*) jobz, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) w, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_ssyevd(void* jobz, void* uplo, void* n, void* a, void* lda, void* w, void* work, void* lwork, void* iwork, void* liwork, void* info) __attribute__((alias("flexiblas_real_ssyevd_")));
#else
void flexiblas_real_ssyevd(void* jobz, void* uplo, void* n, void* a, void* lda, void* w, void* work, void* lwork, void* iwork, void* liwork, void* info){flexiblas_real_ssyevd_((void*) jobz, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) w, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_ssyevd_(void* jobz, void* uplo, void* n, void* a, void* lda, void* w, void* work, void* lwork, void* iwork, void* liwork, void* info)
{
	void (*fn) (void* jobz, void* uplo, void* n, void* a, void* lda, void* w, void* work, void* lwork, void* iwork, void* liwork, void* info);
	void (*fn_hook) (void* jobz, void* uplo, void* n, void* a, void* lda, void* w, void* work, void* lwork, void* iwork, void* liwork, void* info);

	*(void **) &fn      = current_backend->lapack.ssyevd.f77_blas_function; 

    hook_pos_ssyevd ++;
    if( hook_pos_ssyevd < __flexiblas_hooks->ssyevd.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->ssyevd.f77_hook_function[hook_pos_ssyevd];
        fn_hook((void*) jobz, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) w, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info);
    } else {
        hook_pos_ssyevd = 0;
		fn((void*) jobz, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) w, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_ssyevd(void* jobz, void* uplo, void* n, void* a, void* lda, void* w, void* work, void* lwork, void* iwork, void* liwork, void* info) __attribute__((alias("flexiblas_chain_ssyevd_")));
#else
void flexiblas_chain_ssyevd(void* jobz, void* uplo, void* n, void* a, void* lda, void* w, void* work, void* lwork, void* iwork, void* liwork, void* info){flexiblas_chain_ssyevd_((void*) jobz, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) w, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info);}
#endif



