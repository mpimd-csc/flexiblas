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



static TLS_STORE uint8_t hook_pos_ztgsen = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(ztgsen,ZTGSEN)(blasint* ijob, blasint* wantq, blasint* wantz, blasint* select, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* alpha, double complex* beta, double complex* q, blasint* ldq, double complex* z, blasint* ldz, blasint* m, double* pl, double* pr, double* dif, double complex* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info)
#else
void FC_GLOBAL(ztgsen,ZTGSEN)(blasint* ijob, blasint* wantq, blasint* wantz, blasint* select, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* alpha, double complex* beta, double complex* q, blasint* ldq, double complex* z, blasint* ldz, blasint* m, double* pl, double* pr, double* dif, double complex* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info)
#endif
{
	void (*fn) (void* ijob, void* wantq, void* wantz, void* select, void* n, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* m, void* pl, void* pr, void* dif, void* work, void* lwork, void* iwork, void* liwork, void* info);
	void (*fn_hook) (void* ijob, void* wantq, void* wantz, void* select, void* n, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* m, void* pl, void* pr, void* dif, void* work, void* lwork, void* iwork, void* liwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.ztgsen.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->ztgsen.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) ijob, (void*) wantq, (void*) wantz, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alpha, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) m, (void*) pl, (void*) pr, (void*) dif, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info); 
		return;
	} else {
		hook_pos_ztgsen = 0;
		fn_hook((void*) ijob, (void*) wantq, (void*) wantz, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alpha, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) m, (void*) pl, (void*) pr, (void*) dif, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void ztgsen_(blasint* ijob, blasint* wantq, blasint* wantz, blasint* select, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* alpha, double complex* beta, double complex* q, blasint* ldq, double complex* z, blasint* ldz, blasint* m, double* pl, double* pr, double* dif, double complex* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(ztgsen,ZTGSEN)))));
#else
#ifndef __APPLE__
void ztgsen(blasint* ijob, blasint* wantq, blasint* wantz, blasint* select, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* alpha, double complex* beta, double complex* q, blasint* ldq, double complex* z, blasint* ldz, blasint* m, double* pl, double* pr, double* dif, double complex* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(ztgsen,ZTGSEN)))));
#else
void ztgsen(blasint* ijob, blasint* wantq, blasint* wantz, blasint* select, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* alpha, double complex* beta, double complex* q, blasint* ldq, double complex* z, blasint* ldz, blasint* m, double* pl, double* pr, double* dif, double complex* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info){ FC_GLOBAL(ztgsen,ZTGSEN)((void*) ijob, (void*) wantq, (void*) wantz, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alpha, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) m, (void*) pl, (void*) pr, (void*) dif, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_ztgsen_(void* ijob, void* wantq, void* wantz, void* select, void* n, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* m, void* pl, void* pr, void* dif, void* work, void* lwork, void* iwork, void* liwork, void* info)
{
	void (*fn) (void* ijob, void* wantq, void* wantz, void* select, void* n, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* m, void* pl, void* pr, void* dif, void* work, void* lwork, void* iwork, void* liwork, void* info);

	*(void **) & fn = current_backend->lapack.ztgsen.f77_blas_function; 

		fn((void*) ijob, (void*) wantq, (void*) wantz, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alpha, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) m, (void*) pl, (void*) pr, (void*) dif, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_ztgsen(void* ijob, void* wantq, void* wantz, void* select, void* n, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* m, void* pl, void* pr, void* dif, void* work, void* lwork, void* iwork, void* liwork, void* info) __attribute__((alias("flexiblas_real_ztgsen_")));
#else
void flexiblas_real_ztgsen(void* ijob, void* wantq, void* wantz, void* select, void* n, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* m, void* pl, void* pr, void* dif, void* work, void* lwork, void* iwork, void* liwork, void* info){flexiblas_real_ztgsen_((void*) ijob, (void*) wantq, (void*) wantz, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alpha, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) m, (void*) pl, (void*) pr, (void*) dif, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_ztgsen_(void* ijob, void* wantq, void* wantz, void* select, void* n, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* m, void* pl, void* pr, void* dif, void* work, void* lwork, void* iwork, void* liwork, void* info)
{
	void (*fn) (void* ijob, void* wantq, void* wantz, void* select, void* n, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* m, void* pl, void* pr, void* dif, void* work, void* lwork, void* iwork, void* liwork, void* info);
	void (*fn_hook) (void* ijob, void* wantq, void* wantz, void* select, void* n, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* m, void* pl, void* pr, void* dif, void* work, void* lwork, void* iwork, void* liwork, void* info);

	*(void **) &fn      = current_backend->lapack.ztgsen.f77_blas_function; 

    hook_pos_ztgsen ++;
    if( hook_pos_ztgsen < __flexiblas_hooks->ztgsen.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->ztgsen.f77_hook_function[hook_pos_ztgsen];
        fn_hook((void*) ijob, (void*) wantq, (void*) wantz, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alpha, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) m, (void*) pl, (void*) pr, (void*) dif, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info);
    } else {
        hook_pos_ztgsen = 0;
		fn((void*) ijob, (void*) wantq, (void*) wantz, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alpha, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) m, (void*) pl, (void*) pr, (void*) dif, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_ztgsen(void* ijob, void* wantq, void* wantz, void* select, void* n, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* m, void* pl, void* pr, void* dif, void* work, void* lwork, void* iwork, void* liwork, void* info) __attribute__((alias("flexiblas_chain_ztgsen_")));
#else
void flexiblas_chain_ztgsen(void* ijob, void* wantq, void* wantz, void* select, void* n, void* a, void* lda, void* b, void* ldb, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* m, void* pl, void* pr, void* dif, void* work, void* lwork, void* iwork, void* liwork, void* info){flexiblas_chain_ztgsen_((void*) ijob, (void*) wantq, (void*) wantz, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alpha, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) m, (void*) pl, (void*) pr, (void*) dif, (void*) work, (void*) lwork, (void*) iwork, (void*) liwork, (void*) info);}
#endif



