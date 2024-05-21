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

#include "flexiblas_fortran_char_len.h"


static TLS_STORE uint8_t hook_pos_cgeqpf = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(cgeqpf,CGEQPF)(blasint* m, blasint* n, float complex* a, blasint* lda, blasint* jpvt, float complex* tau, float complex* work, float* rwork, blasint* info)
#else
void FC_GLOBAL(cgeqpf,CGEQPF)(blasint* m, blasint* n, float complex* a, blasint* lda, blasint* jpvt, float complex* tau, float complex* work, float* rwork, blasint* info)
#endif
{
	void (*fn) (void* m, void* n, void* a, void* lda, void* jpvt, void* tau, void* work, void* rwork, void* info);
	void (*fn_hook) (void* m, void* n, void* a, void* lda, void* jpvt, void* tau, void* work, void* rwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.cgeqpf.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->cgeqpf.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) m, (void*) n, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) work, (void*) rwork, (void*) info); 
		return;
	} else {
		hook_pos_cgeqpf = 0;
		fn_hook((void*) m, (void*) n, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) work, (void*) rwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void cgeqpf_(blasint* m, blasint* n, float complex* a, blasint* lda, blasint* jpvt, float complex* tau, float complex* work, float* rwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(cgeqpf,CGEQPF)))));
#else
#ifndef __APPLE__
void cgeqpf(blasint* m, blasint* n, float complex* a, blasint* lda, blasint* jpvt, float complex* tau, float complex* work, float* rwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(cgeqpf,CGEQPF)))));
#else
void cgeqpf(blasint* m, blasint* n, float complex* a, blasint* lda, blasint* jpvt, float complex* tau, float complex* work, float* rwork, blasint* info){ FC_GLOBAL(cgeqpf,CGEQPF)((void*) m, (void*) n, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) work, (void*) rwork, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_cgeqpf_(void* m, void* n, void* a, void* lda, void* jpvt, void* tau, void* work, void* rwork, void* info)
{
	void (*fn) (void* m, void* n, void* a, void* lda, void* jpvt, void* tau, void* work, void* rwork, void* info);

	*(void **) & fn = current_backend->lapack.cgeqpf.f77_blas_function; 

		fn((void*) m, (void*) n, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) work, (void*) rwork, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_cgeqpf(void* m, void* n, void* a, void* lda, void* jpvt, void* tau, void* work, void* rwork, void* info) __attribute__((alias("flexiblas_real_cgeqpf_")));
#else
void flexiblas_real_cgeqpf(void* m, void* n, void* a, void* lda, void* jpvt, void* tau, void* work, void* rwork, void* info){flexiblas_real_cgeqpf_((void*) m, (void*) n, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) work, (void*) rwork, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_cgeqpf_(void* m, void* n, void* a, void* lda, void* jpvt, void* tau, void* work, void* rwork, void* info)
{
	void (*fn) (void* m, void* n, void* a, void* lda, void* jpvt, void* tau, void* work, void* rwork, void* info);
	void (*fn_hook) (void* m, void* n, void* a, void* lda, void* jpvt, void* tau, void* work, void* rwork, void* info);

	*(void **) &fn      = current_backend->lapack.cgeqpf.f77_blas_function; 

    hook_pos_cgeqpf ++;
    if( hook_pos_cgeqpf < __flexiblas_hooks->cgeqpf.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->cgeqpf.f77_hook_function[hook_pos_cgeqpf];
        fn_hook((void*) m, (void*) n, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) work, (void*) rwork, (void*) info);
    } else {
        hook_pos_cgeqpf = 0;
		fn((void*) m, (void*) n, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) work, (void*) rwork, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_cgeqpf(void* m, void* n, void* a, void* lda, void* jpvt, void* tau, void* work, void* rwork, void* info) __attribute__((alias("flexiblas_chain_cgeqpf_")));
#else
void flexiblas_chain_cgeqpf(void* m, void* n, void* a, void* lda, void* jpvt, void* tau, void* work, void* rwork, void* info){flexiblas_chain_cgeqpf_((void*) m, (void*) n, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) work, (void*) rwork, (void*) info);}
#endif



