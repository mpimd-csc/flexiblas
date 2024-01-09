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



static TLS_STORE uint8_t hook_pos_ztgsy2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(ztgsy2,ZTGSY2)(char* trans, blasint* ijob, blasint* m, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* c, blasint* ldc, double complex* d, blasint* ldd, double complex* e, blasint* lde, double complex* f, blasint* ldf, double* scale, double* rdsum, double* rdscal, blasint* info)
#else
void FC_GLOBAL(ztgsy2,ZTGSY2)(char* trans, blasint* ijob, blasint* m, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* c, blasint* ldc, double complex* d, blasint* ldd, double complex* e, blasint* lde, double complex* f, blasint* ldf, double* scale, double* rdsum, double* rdscal, blasint* info)
#endif
{
	void (*fn) (void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* info);
	void (*fn_hook) (void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.ztgsy2.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->ztgsy2.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) rdsum, (void*) rdscal, (void*) info); 
		return;
	} else {
		hook_pos_ztgsy2 = 0;
		fn_hook((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) rdsum, (void*) rdscal, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void ztgsy2_(char* trans, blasint* ijob, blasint* m, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* c, blasint* ldc, double complex* d, blasint* ldd, double complex* e, blasint* lde, double complex* f, blasint* ldf, double* scale, double* rdsum, double* rdscal, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(ztgsy2,ZTGSY2)))));
#else
#ifndef __APPLE__
void ztgsy2(char* trans, blasint* ijob, blasint* m, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* c, blasint* ldc, double complex* d, blasint* ldd, double complex* e, blasint* lde, double complex* f, blasint* ldf, double* scale, double* rdsum, double* rdscal, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(ztgsy2,ZTGSY2)))));
#else
void ztgsy2(char* trans, blasint* ijob, blasint* m, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* c, blasint* ldc, double complex* d, blasint* ldd, double complex* e, blasint* lde, double complex* f, blasint* ldf, double* scale, double* rdsum, double* rdscal, blasint* info){ FC_GLOBAL(ztgsy2,ZTGSY2)((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) rdsum, (void*) rdscal, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_ztgsy2_(void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* info)
{
	void (*fn) (void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* info);

	*(void **) & fn = current_backend->lapack.ztgsy2.f77_blas_function; 

		fn((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) rdsum, (void*) rdscal, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_ztgsy2(void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* info) __attribute__((alias("flexiblas_real_ztgsy2_")));
#else
void flexiblas_real_ztgsy2(void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* info){flexiblas_real_ztgsy2_((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) rdsum, (void*) rdscal, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_ztgsy2_(void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* info)
{
	void (*fn) (void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* info);
	void (*fn_hook) (void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* info);

	*(void **) &fn      = current_backend->lapack.ztgsy2.f77_blas_function; 

    hook_pos_ztgsy2 ++;
    if( hook_pos_ztgsy2 < __flexiblas_hooks->ztgsy2.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->ztgsy2.f77_hook_function[hook_pos_ztgsy2];
        fn_hook((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) rdsum, (void*) rdscal, (void*) info);
    } else {
        hook_pos_ztgsy2 = 0;
		fn((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) rdsum, (void*) rdscal, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_ztgsy2(void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* info) __attribute__((alias("flexiblas_chain_ztgsy2_")));
#else
void flexiblas_chain_ztgsy2(void* trans, void* ijob, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* d, void* ldd, void* e, void* lde, void* f, void* ldf, void* scale, void* rdsum, void* rdscal, void* info){flexiblas_chain_ztgsy2_((void*) trans, (void*) ijob, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) d, (void*) ldd, (void*) e, (void*) lde, (void*) f, (void*) ldf, (void*) scale, (void*) rdsum, (void*) rdscal, (void*) info);}
#endif



