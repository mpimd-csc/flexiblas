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
 /* Generated: Wed Mar 28 11:20:05 2018 */
        
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



static TLS_STORE uint8_t hook_pos_ztgsyl = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(ztgsyl,ZTGSYL)(char* trans, blasint* ijob, blasint* m, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* c, blasint* ldc, double complex* d, blasint* ldd, double complex* e, blasint* lde, double complex* f, blasint* ldf, double* scale, double* dif, double complex* work, blasint* lwork, blasint* iwork, blasint* info)
#else
void FC_GLOBAL(ztgsyl,ZTGSYL)(char* trans, blasint* ijob, blasint* m, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* c, blasint* ldc, double complex* d, blasint* ldd, double complex* e, blasint* lde, double complex* f, blasint* ldf, double* scale, double* dif, double complex* work, blasint* lwork, blasint* iwork, blasint* info)
#endif
{
	void (*fn) (void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* dif, void* work, void* lwork, void* iwork, void* info);
	void (*fn_hook) (void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* dif, void* work, void* lwork, void* iwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.ztgsyl.f77_blas_function; 
	fn_hook = __flexiblas_hooks->ztgsyl.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) dif, (void*) work, (void*) lwork, (void*) iwork, (void*) info); 
		return;
	} else {
		hook_pos_ztgsyl = 0;
		fn_hook((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) dif, (void*) work, (void*) lwork, (void*) iwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void ztgsyl_(char* trans, blasint* ijob, blasint* m, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* c, blasint* ldc, double complex* d, blasint* ldd, double complex* e, blasint* lde, double complex* f, blasint* ldf, double* scale, double* dif, double complex* work, blasint* lwork, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(ztgsyl,ZTGSYL)))));
#else
void ztgsyl(char* trans, blasint* ijob, blasint* m, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* c, blasint* ldc, double complex* d, blasint* ldd, double complex* e, blasint* lde, double complex* f, blasint* ldf, double* scale, double* dif, double complex* work, blasint* lwork, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(ztgsyl,ZTGSYL)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_ztgsyl_(void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* dif, void* work, void* lwork, void* iwork, void* info)
{
	void (*fn) (void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* dif, void* work, void* lwork, void* iwork, void* info);

	fn = current_backend->lapack.ztgsyl.f77_blas_function; 

		fn((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) dif, (void*) work, (void*) lwork, (void*) iwork, (void*) info); 

	return;
}

void flexiblas_real_ztgsyl(void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* dif, void* work, void* lwork, void* iwork, void* info)  __attribute__((alias("flexiblas_real_ztgsyl_")));





/* Chainloader for Hooks */


void flexiblas_chain_ztgsyl_(void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* dif, void* work, void* lwork, void* iwork, void* info)
{
	void (*fn) (void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* dif, void* work, void* lwork, void* iwork, void* info);
	void (*fn_hook) (void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* dif, void* work, void* lwork, void* iwork, void* info);

	fn      = current_backend->lapack.ztgsyl.f77_blas_function; 

    hook_pos_ztgsyl ++;
    if( hook_pos_ztgsyl < __flexiblas_hooks->ztgsyl.nhook) {
        fn_hook = __flexiblas_hooks->ztgsyl.f77_hook_function[hook_pos_ztgsyl];
        fn_hook((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) dif, (void*) work, (void*) lwork, (void*) iwork, (void*) info);
    } else {
        hook_pos_ztgsyl = 0;
		fn((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) dif, (void*) work, (void*) lwork, (void*) iwork, (void*) info); 
	}
	return;
}

void flexiblas_chain_ztgsyl(void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* dif, void* work, void* lwork, void* iwork, void* info)  __attribute__((alias("flexiblas_chain_ztgsyl_")));




