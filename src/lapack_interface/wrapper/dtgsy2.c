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



static TLS_STORE uint8_t hook_pos_dtgsy2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dtgsy2,DTGSY2)(char* trans, blasint* ijob, blasint* m, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* c, blasint* ldc, double* d, blasint* ldd, double* e, blasint* lde, double* f, blasint* ldf, double* scale, double* rdsum, double* rdscal, blasint* iwork, blasint* pq, blasint* info)
#else
void FC_GLOBAL(dtgsy2,DTGSY2)(char* trans, blasint* ijob, blasint* m, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* c, blasint* ldc, double* d, blasint* ldd, double* e, blasint* lde, double* f, blasint* ldf, double* scale, double* rdsum, double* rdscal, blasint* iwork, blasint* pq, blasint* info)
#endif
{
	void (*fn) (void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* iwork, void* pq, void* info);
	void (*fn_hook) (void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* iwork, void* pq, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.dtgsy2.f77_blas_function; 
	fn_hook = __flexiblas_hooks->dtgsy2.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) rdsum, (void*) rdscal, (void*) iwork, (void*) pq, (void*) info); 
		return;
	} else {
		hook_pos_dtgsy2 = 0;
		fn_hook((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) rdsum, (void*) rdscal, (void*) iwork, (void*) pq, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dtgsy2_(char* trans, blasint* ijob, blasint* m, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* c, blasint* ldc, double* d, blasint* ldd, double* e, blasint* lde, double* f, blasint* ldf, double* scale, double* rdsum, double* rdscal, blasint* iwork, blasint* pq, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dtgsy2,DTGSY2)))));
#else
void dtgsy2(char* trans, blasint* ijob, blasint* m, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* c, blasint* ldc, double* d, blasint* ldd, double* e, blasint* lde, double* f, blasint* ldf, double* scale, double* rdsum, double* rdscal, blasint* iwork, blasint* pq, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dtgsy2,DTGSY2)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dtgsy2_(void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* iwork, void* pq, void* info)
{
	void (*fn) (void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* iwork, void* pq, void* info);

	fn = current_backend->lapack.dtgsy2.f77_blas_function; 

		fn((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) rdsum, (void*) rdscal, (void*) iwork, (void*) pq, (void*) info); 

	return;
}

void flexiblas_real_dtgsy2(void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* iwork, void* pq, void* info)  __attribute__((alias("flexiblas_real_dtgsy2_")));





/* Chainloader for Hooks */


void flexiblas_chain_dtgsy2_(void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* iwork, void* pq, void* info)
{
	void (*fn) (void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* iwork, void* pq, void* info);
	void (*fn_hook) (void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* iwork, void* pq, void* info);

	fn      = current_backend->lapack.dtgsy2.f77_blas_function; 

    hook_pos_dtgsy2 ++;
    if( hook_pos_dtgsy2 < __flexiblas_hooks->dtgsy2.nhook) {
        fn_hook = __flexiblas_hooks->dtgsy2.f77_hook_function[hook_pos_dtgsy2];
        fn_hook((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) rdsum, (void*) rdscal, (void*) iwork, (void*) pq, (void*) info);
    } else {
        hook_pos_dtgsy2 = 0;
		fn((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) rdsum, (void*) rdscal, (void*) iwork, (void*) pq, (void*) info); 
	}
	return;
}

void flexiblas_chain_dtgsy2(void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* iwork, void* pq, void* info)  __attribute__((alias("flexiblas_chain_dtgsy2_")));




