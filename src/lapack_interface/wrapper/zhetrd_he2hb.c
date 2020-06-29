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



static TLS_STORE uint8_t hook_pos_zhetrd_he2hb = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL_(zhetrd_he2hb,ZHETRD_HE2HB)(char* uplo, blasint* n, blasint* kd, double complex* a, blasint* lda, double complex* ab, blasint* ldab, double complex* tau, double complex* work, blasint* lwork, blasint* info)
#else
void FC_GLOBAL_(zhetrd_he2hb,ZHETRD_HE2HB)(char* uplo, blasint* n, blasint* kd, double complex* a, blasint* lda, double complex* ab, blasint* ldab, double complex* tau, double complex* work, blasint* lwork, blasint* info)
#endif
{
	void (*fn) (void* uplo, void* n, void* kd, void* a, void* lda, void* ab, void* ldab, void* tau, void* work, void* lwork, void* info);
	void (*fn_hook) (void* uplo, void* n, void* kd, void* a, void* lda, void* ab, void* ldab, void* tau, void* work, void* lwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.zhetrd_he2hb.f77_blas_function; 
	fn_hook = __flexiblas_hooks->zhetrd_he2hb.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) uplo, (void*) n, (void*) kd, (void*) a, (void*) lda, (void*) ab, (void*) ldab, (void*) tau, (void*) work, (void*) lwork, (void*) info); 
		return;
	} else {
		hook_pos_zhetrd_he2hb = 0;
		fn_hook((void*) uplo, (void*) n, (void*) kd, (void*) a, (void*) lda, (void*) ab, (void*) ldab, (void*) tau, (void*) work, (void*) lwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void zhetrd_he2hb_(char* uplo, blasint* n, blasint* kd, double complex* a, blasint* lda, double complex* ab, blasint* ldab, double complex* tau, double complex* work, blasint* lwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL_(zhetrd_he2hb,ZHETRD_HE2HB)))));
#else
void zhetrd_he2hb(char* uplo, blasint* n, blasint* kd, double complex* a, blasint* lda, double complex* ab, blasint* ldab, double complex* tau, double complex* work, blasint* lwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL_(zhetrd_he2hb,ZHETRD_HE2HB)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zhetrd_he2hb_(void* uplo, void* n, void* kd, void* a, void* lda, void* ab, void* ldab, void* tau, void* work, void* lwork, void* info)
{
	void (*fn) (void* uplo, void* n, void* kd, void* a, void* lda, void* ab, void* ldab, void* tau, void* work, void* lwork, void* info);

	fn = current_backend->lapack.zhetrd_he2hb.f77_blas_function; 

		fn((void*) uplo, (void*) n, (void*) kd, (void*) a, (void*) lda, (void*) ab, (void*) ldab, (void*) tau, (void*) work, (void*) lwork, (void*) info); 

	return;
}

void flexiblas_real_zhetrd_he2hb(void* uplo, void* n, void* kd, void* a, void* lda, void* ab, void* ldab, void* tau, void* work, void* lwork, void* info)  __attribute__((alias("flexiblas_real_zhetrd_he2hb_")));





/* Chainloader for Hooks */


void flexiblas_chain_zhetrd_he2hb_(void* uplo, void* n, void* kd, void* a, void* lda, void* ab, void* ldab, void* tau, void* work, void* lwork, void* info)
{
	void (*fn) (void* uplo, void* n, void* kd, void* a, void* lda, void* ab, void* ldab, void* tau, void* work, void* lwork, void* info);
	void (*fn_hook) (void* uplo, void* n, void* kd, void* a, void* lda, void* ab, void* ldab, void* tau, void* work, void* lwork, void* info);

	fn      = current_backend->lapack.zhetrd_he2hb.f77_blas_function; 

    hook_pos_zhetrd_he2hb ++;
    if( hook_pos_zhetrd_he2hb < __flexiblas_hooks->zhetrd_he2hb.nhook) {
        fn_hook = __flexiblas_hooks->zhetrd_he2hb.f77_hook_function[hook_pos_zhetrd_he2hb];
        fn_hook((void*) uplo, (void*) n, (void*) kd, (void*) a, (void*) lda, (void*) ab, (void*) ldab, (void*) tau, (void*) work, (void*) lwork, (void*) info);
    } else {
        hook_pos_zhetrd_he2hb = 0;
		fn((void*) uplo, (void*) n, (void*) kd, (void*) a, (void*) lda, (void*) ab, (void*) ldab, (void*) tau, (void*) work, (void*) lwork, (void*) info); 
	}
	return;
}

void flexiblas_chain_zhetrd_he2hb(void* uplo, void* n, void* kd, void* a, void* lda, void* ab, void* ldab, void* tau, void* work, void* lwork, void* info)  __attribute__((alias("flexiblas_chain_zhetrd_he2hb_")));




