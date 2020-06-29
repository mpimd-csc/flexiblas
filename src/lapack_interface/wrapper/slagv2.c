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



static TLS_STORE uint8_t hook_pos_slagv2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slagv2,SLAGV2)(float* a, blasint* lda, float* b, blasint* ldb, float* alphar, float* alphai, float* beta, float* csl, float* snl, float* csr, float* snr)
#else
void FC_GLOBAL(slagv2,SLAGV2)(float* a, blasint* lda, float* b, blasint* ldb, float* alphar, float* alphai, float* beta, float* csl, float* snl, float* csr, float* snr)
#endif
{
	void (*fn) (void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* csl, void* snl, void* csr, void* snr);
	void (*fn_hook) (void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* csl, void* snl, void* csr, void* snr);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.slagv2.f77_blas_function; 
	fn_hook = __flexiblas_hooks->slagv2.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) csl, (void*) snl, (void*) csr, (void*) snr); 
		return;
	} else {
		hook_pos_slagv2 = 0;
		fn_hook((void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) csl, (void*) snl, (void*) csr, (void*) snr);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void slagv2_(float* a, blasint* lda, float* b, blasint* ldb, float* alphar, float* alphai, float* beta, float* csl, float* snl, float* csr, float* snr) __attribute__((alias(MTS(FC_GLOBAL(slagv2,SLAGV2)))));
#else
void slagv2(float* a, blasint* lda, float* b, blasint* ldb, float* alphar, float* alphai, float* beta, float* csl, float* snl, float* csr, float* snr) __attribute__((alias(MTS(FC_GLOBAL(slagv2,SLAGV2)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slagv2_(void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* csl, void* snl, void* csr, void* snr)
{
	void (*fn) (void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* csl, void* snl, void* csr, void* snr);

	fn = current_backend->lapack.slagv2.f77_blas_function; 

		fn((void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) csl, (void*) snl, (void*) csr, (void*) snr); 

	return;
}

void flexiblas_real_slagv2(void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* csl, void* snl, void* csr, void* snr)  __attribute__((alias("flexiblas_real_slagv2_")));





/* Chainloader for Hooks */


void flexiblas_chain_slagv2_(void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* csl, void* snl, void* csr, void* snr)
{
	void (*fn) (void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* csl, void* snl, void* csr, void* snr);
	void (*fn_hook) (void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* csl, void* snl, void* csr, void* snr);

	fn      = current_backend->lapack.slagv2.f77_blas_function; 

    hook_pos_slagv2 ++;
    if( hook_pos_slagv2 < __flexiblas_hooks->slagv2.nhook) {
        fn_hook = __flexiblas_hooks->slagv2.f77_hook_function[hook_pos_slagv2];
        fn_hook((void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) csl, (void*) snl, (void*) csr, (void*) snr);
    } else {
        hook_pos_slagv2 = 0;
		fn((void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) csl, (void*) snl, (void*) csr, (void*) snr); 
	}
	return;
}

void flexiblas_chain_slagv2(void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* csl, void* snl, void* csr, void* snr)  __attribute__((alias("flexiblas_chain_slagv2_")));




