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



static TLS_STORE uint8_t hook_pos_stgsy2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(stgsy2,STGSY2)(char* trans, blasint* ijob, blasint* m, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, float* c, blasint* ldc, float* d, blasint* ldd, float* e, blasint* lde, float* f, blasint* ldf, float* scale, float* rdsum, float* rdscal, blasint* iwork, blasint* pq, blasint* info)
#else
void FC_GLOBAL(stgsy2,STGSY2)(char* trans, blasint* ijob, blasint* m, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, float* c, blasint* ldc, float* d, blasint* ldd, float* e, blasint* lde, float* f, blasint* ldf, float* scale, float* rdsum, float* rdscal, blasint* iwork, blasint* pq, blasint* info)
#endif
{
	void (*fn) (void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* iwork, void* pq, void* info);
	void (*fn_hook) (void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* iwork, void* pq, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.stgsy2.f77_blas_function; 
	fn_hook = __flexiblas_hooks->stgsy2.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) rdsum, (void*) rdscal, (void*) iwork, (void*) pq, (void*) info); 
		return;
	} else {
		hook_pos_stgsy2 = 0;
		fn_hook((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) rdsum, (void*) rdscal, (void*) iwork, (void*) pq, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void stgsy2_(char* trans, blasint* ijob, blasint* m, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, float* c, blasint* ldc, float* d, blasint* ldd, float* e, blasint* lde, float* f, blasint* ldf, float* scale, float* rdsum, float* rdscal, blasint* iwork, blasint* pq, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(stgsy2,STGSY2)))));
#else
void stgsy2(char* trans, blasint* ijob, blasint* m, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, float* c, blasint* ldc, float* d, blasint* ldd, float* e, blasint* lde, float* f, blasint* ldf, float* scale, float* rdsum, float* rdscal, blasint* iwork, blasint* pq, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(stgsy2,STGSY2)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_stgsy2_(void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* iwork, void* pq, void* info)
{
	void (*fn) (void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* iwork, void* pq, void* info);

	fn = current_backend->lapack.stgsy2.f77_blas_function; 

		fn((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) rdsum, (void*) rdscal, (void*) iwork, (void*) pq, (void*) info); 

	return;
}

void flexiblas_real_stgsy2(void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* iwork, void* pq, void* info)  __attribute__((alias("flexiblas_real_stgsy2_")));





/* Chainloader for Hooks */


void flexiblas_chain_stgsy2_(void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* iwork, void* pq, void* info)
{
	void (*fn) (void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* iwork, void* pq, void* info);
	void (*fn_hook) (void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* iwork, void* pq, void* info);

	fn      = current_backend->lapack.stgsy2.f77_blas_function; 

    hook_pos_stgsy2 ++;
    if( hook_pos_stgsy2 < __flexiblas_hooks->stgsy2.nhook) {
        fn_hook = __flexiblas_hooks->stgsy2.f77_hook_function[hook_pos_stgsy2];
        fn_hook((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) rdsum, (void*) rdscal, (void*) iwork, (void*) pq, (void*) info);
    } else {
        hook_pos_stgsy2 = 0;
		fn((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) rdsum, (void*) rdscal, (void*) iwork, (void*) pq, (void*) info); 
	}
	return;
}

void flexiblas_chain_stgsy2(void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* iwork, void* pq, void* info)  __attribute__((alias("flexiblas_chain_stgsy2_")));




