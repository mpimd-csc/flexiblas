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



static TLS_STORE uint8_t hook_pos_zhb2st_kernels = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL_(zhb2st_kernels,ZHB2ST_KERNELS)(char* uplo, blasint* wantz, blasint* ttype, blasint* st, blasint* ed, blasint* sweep, blasint* n, blasint* nb, blasint* ib, double complex* a, blasint* lda, double complex* v, double complex* tau, blasint* ldvt, double complex* work)
#else
void FC_GLOBAL_(zhb2st_kernels,ZHB2ST_KERNELS)(char* uplo, blasint* wantz, blasint* ttype, blasint* st, blasint* ed, blasint* sweep, blasint* n, blasint* nb, blasint* ib, double complex* a, blasint* lda, double complex* v, double complex* tau, blasint* ldvt, double complex* work)
#endif
{
	void (*fn) (void* uplo, void* wantz, void* ttype, void* st, void* ed, void* sweep, void* n, void* nb, void* ib, void* a, void* lda, void* v, void* tau, void* ldvt, void* work);
	void (*fn_hook) (void* uplo, void* wantz, void* ttype, void* st, void* ed, void* sweep, void* n, void* nb, void* ib, void* a, void* lda, void* v, void* tau, void* ldvt, void* work);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.zhb2st_kernels.f77_blas_function; 
	fn_hook = __flexiblas_hooks->zhb2st_kernels.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) uplo, (void*) wantz, (void*) ttype, (void*) st, (void*) ed, (void*) sweep, (void*) n, (void*) nb, (void*) ib, (void*) a, (void*) lda, (void*) v, (void*) tau, (void*) ldvt, (void*) work); 
		return;
	} else {
		hook_pos_zhb2st_kernels = 0;
		fn_hook((void*) uplo, (void*) wantz, (void*) ttype, (void*) st, (void*) ed, (void*) sweep, (void*) n, (void*) nb, (void*) ib, (void*) a, (void*) lda, (void*) v, (void*) tau, (void*) ldvt, (void*) work);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void zhb2st_kernels_(char* uplo, blasint* wantz, blasint* ttype, blasint* st, blasint* ed, blasint* sweep, blasint* n, blasint* nb, blasint* ib, double complex* a, blasint* lda, double complex* v, double complex* tau, blasint* ldvt, double complex* work) __attribute__((alias(MTS(FC_GLOBAL_(zhb2st_kernels,ZHB2ST_KERNELS)))));
#else
void zhb2st_kernels(char* uplo, blasint* wantz, blasint* ttype, blasint* st, blasint* ed, blasint* sweep, blasint* n, blasint* nb, blasint* ib, double complex* a, blasint* lda, double complex* v, double complex* tau, blasint* ldvt, double complex* work) __attribute__((alias(MTS(FC_GLOBAL_(zhb2st_kernels,ZHB2ST_KERNELS)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zhb2st_kernels_(void* uplo, void* wantz, void* ttype, void* st, void* ed, void* sweep, void* n, void* nb, void* ib, void* a, void* lda, void* v, void* tau, void* ldvt, void* work)
{
	void (*fn) (void* uplo, void* wantz, void* ttype, void* st, void* ed, void* sweep, void* n, void* nb, void* ib, void* a, void* lda, void* v, void* tau, void* ldvt, void* work);

	fn = current_backend->lapack.zhb2st_kernels.f77_blas_function; 

		fn((void*) uplo, (void*) wantz, (void*) ttype, (void*) st, (void*) ed, (void*) sweep, (void*) n, (void*) nb, (void*) ib, (void*) a, (void*) lda, (void*) v, (void*) tau, (void*) ldvt, (void*) work); 

	return;
}

void flexiblas_real_zhb2st_kernels(void* uplo, void* wantz, void* ttype, void* st, void* ed, void* sweep, void* n, void* nb, void* ib, void* a, void* lda, void* v, void* tau, void* ldvt, void* work)  __attribute__((alias("flexiblas_real_zhb2st_kernels_")));





/* Chainloader for Hooks */


void flexiblas_chain_zhb2st_kernels_(void* uplo, void* wantz, void* ttype, void* st, void* ed, void* sweep, void* n, void* nb, void* ib, void* a, void* lda, void* v, void* tau, void* ldvt, void* work)
{
	void (*fn) (void* uplo, void* wantz, void* ttype, void* st, void* ed, void* sweep, void* n, void* nb, void* ib, void* a, void* lda, void* v, void* tau, void* ldvt, void* work);
	void (*fn_hook) (void* uplo, void* wantz, void* ttype, void* st, void* ed, void* sweep, void* n, void* nb, void* ib, void* a, void* lda, void* v, void* tau, void* ldvt, void* work);

	fn      = current_backend->lapack.zhb2st_kernels.f77_blas_function; 

    hook_pos_zhb2st_kernels ++;
    if( hook_pos_zhb2st_kernels < __flexiblas_hooks->zhb2st_kernels.nhook) {
        fn_hook = __flexiblas_hooks->zhb2st_kernels.f77_hook_function[hook_pos_zhb2st_kernels];
        fn_hook((void*) uplo, (void*) wantz, (void*) ttype, (void*) st, (void*) ed, (void*) sweep, (void*) n, (void*) nb, (void*) ib, (void*) a, (void*) lda, (void*) v, (void*) tau, (void*) ldvt, (void*) work);
    } else {
        hook_pos_zhb2st_kernels = 0;
		fn((void*) uplo, (void*) wantz, (void*) ttype, (void*) st, (void*) ed, (void*) sweep, (void*) n, (void*) nb, (void*) ib, (void*) a, (void*) lda, (void*) v, (void*) tau, (void*) ldvt, (void*) work); 
	}
	return;
}

void flexiblas_chain_zhb2st_kernels(void* uplo, void* wantz, void* ttype, void* st, void* ed, void* sweep, void* n, void* nb, void* ib, void* a, void* lda, void* v, void* tau, void* ldvt, void* work)  __attribute__((alias("flexiblas_chain_zhb2st_kernels_")));




