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



static TLS_STORE uint8_t hook_pos_cheevd_2stage = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL_(cheevd_2stage,CHEEVD_2STAGE)(char* jobz, char* uplo, blasint* n, float complex* a, blasint* lda, float* w, float complex* work, blasint* lwork, float* rwork, blasint* lrwork, blasint* iwork, blasint* liwork, blasint* info)
#else
void FC_GLOBAL_(cheevd_2stage,CHEEVD_2STAGE)(char* jobz, char* uplo, blasint* n, float complex* a, blasint* lda, float* w, float complex* work, blasint* lwork, float* rwork, blasint* lrwork, blasint* iwork, blasint* liwork, blasint* info)
#endif
{
	void (*fn) (void* jobz, void* uplo, void* n, void* a, void* lda, void* w, void* work, void* lwork, void* rwork, void* lrwork, void* iwork, void* liwork, void* info);
	void (*fn_hook) (void* jobz, void* uplo, void* n, void* a, void* lda, void* w, void* work, void* lwork, void* rwork, void* lrwork, void* iwork, void* liwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.cheevd_2stage.f77_blas_function; 
	fn_hook = __flexiblas_hooks->cheevd_2stage.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) jobz, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) w, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) iwork, (void*) liwork, (void*) info); 
		return;
	} else {
		hook_pos_cheevd_2stage = 0;
		fn_hook((void*) jobz, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) w, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) iwork, (void*) liwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void cheevd_2stage_(char* jobz, char* uplo, blasint* n, float complex* a, blasint* lda, float* w, float complex* work, blasint* lwork, float* rwork, blasint* lrwork, blasint* iwork, blasint* liwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL_(cheevd_2stage,CHEEVD_2STAGE)))));
#else
void cheevd_2stage(char* jobz, char* uplo, blasint* n, float complex* a, blasint* lda, float* w, float complex* work, blasint* lwork, float* rwork, blasint* lrwork, blasint* iwork, blasint* liwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL_(cheevd_2stage,CHEEVD_2STAGE)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_cheevd_2stage_(void* jobz, void* uplo, void* n, void* a, void* lda, void* w, void* work, void* lwork, void* rwork, void* lrwork, void* iwork, void* liwork, void* info)
{
	void (*fn) (void* jobz, void* uplo, void* n, void* a, void* lda, void* w, void* work, void* lwork, void* rwork, void* lrwork, void* iwork, void* liwork, void* info);

	fn = current_backend->lapack.cheevd_2stage.f77_blas_function; 

		fn((void*) jobz, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) w, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) iwork, (void*) liwork, (void*) info); 

	return;
}

void flexiblas_real_cheevd_2stage(void* jobz, void* uplo, void* n, void* a, void* lda, void* w, void* work, void* lwork, void* rwork, void* lrwork, void* iwork, void* liwork, void* info)  __attribute__((alias("flexiblas_real_cheevd_2stage_")));





/* Chainloader for Hooks */


void flexiblas_chain_cheevd_2stage_(void* jobz, void* uplo, void* n, void* a, void* lda, void* w, void* work, void* lwork, void* rwork, void* lrwork, void* iwork, void* liwork, void* info)
{
	void (*fn) (void* jobz, void* uplo, void* n, void* a, void* lda, void* w, void* work, void* lwork, void* rwork, void* lrwork, void* iwork, void* liwork, void* info);
	void (*fn_hook) (void* jobz, void* uplo, void* n, void* a, void* lda, void* w, void* work, void* lwork, void* rwork, void* lrwork, void* iwork, void* liwork, void* info);

	fn      = current_backend->lapack.cheevd_2stage.f77_blas_function; 

    hook_pos_cheevd_2stage ++;
    if( hook_pos_cheevd_2stage < __flexiblas_hooks->cheevd_2stage.nhook) {
        fn_hook = __flexiblas_hooks->cheevd_2stage.f77_hook_function[hook_pos_cheevd_2stage];
        fn_hook((void*) jobz, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) w, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) iwork, (void*) liwork, (void*) info);
    } else {
        hook_pos_cheevd_2stage = 0;
		fn((void*) jobz, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) w, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) iwork, (void*) liwork, (void*) info); 
	}
	return;
}

void flexiblas_chain_cheevd_2stage(void* jobz, void* uplo, void* n, void* a, void* lda, void* w, void* work, void* lwork, void* rwork, void* lrwork, void* iwork, void* liwork, void* info)  __attribute__((alias("flexiblas_chain_cheevd_2stage_")));




