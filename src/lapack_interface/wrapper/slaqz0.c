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



static TLS_STORE uint8_t hook_pos_slaqz0 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slaqz0,SLAQZ0)(char* wants, char* wantq, char* wantz, blasint* n, blasint* ilo, blasint* ihi, float* a, blasint* lda, float* b, blasint* ldb, float* alphar, float* alphai, float* beta, float* q, blasint* ldq, float* z, blasint* ldz, float* work, blasint* lwork, blasint* rec, blasint* info)
#else
void FC_GLOBAL(slaqz0,SLAQZ0)(char* wants, char* wantq, char* wantz, blasint* n, blasint* ilo, blasint* ihi, float* a, blasint* lda, float* b, blasint* ldb, float* alphar, float* alphai, float* beta, float* q, blasint* ldq, float* z, blasint* ldz, float* work, blasint* lwork, blasint* rec, blasint* info)
#endif
{
	void (*fn) (void* wants, void* wantq, void* wantz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rec, void* info);
	void (*fn_hook) (void* wants, void* wantq, void* wantz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rec, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.slaqz0.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->slaqz0.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) wants, (void*) wantq, (void*) wantz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rec, (void*) info); 
		return;
	} else {
		hook_pos_slaqz0 = 0;
		fn_hook((void*) wants, (void*) wantq, (void*) wantz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rec, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void slaqz0_(char* wants, char* wantq, char* wantz, blasint* n, blasint* ilo, blasint* ihi, float* a, blasint* lda, float* b, blasint* ldb, float* alphar, float* alphai, float* beta, float* q, blasint* ldq, float* z, blasint* ldz, float* work, blasint* lwork, blasint* rec, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slaqz0,SLAQZ0)))));
#else
#ifndef __APPLE__
void slaqz0(char* wants, char* wantq, char* wantz, blasint* n, blasint* ilo, blasint* ihi, float* a, blasint* lda, float* b, blasint* ldb, float* alphar, float* alphai, float* beta, float* q, blasint* ldq, float* z, blasint* ldz, float* work, blasint* lwork, blasint* rec, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slaqz0,SLAQZ0)))));
#else
void slaqz0(char* wants, char* wantq, char* wantz, blasint* n, blasint* ilo, blasint* ihi, float* a, blasint* lda, float* b, blasint* ldb, float* alphar, float* alphai, float* beta, float* q, blasint* ldq, float* z, blasint* ldz, float* work, blasint* lwork, blasint* rec, blasint* info){ FC_GLOBAL(slaqz0,SLAQZ0)((void*) wants, (void*) wantq, (void*) wantz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rec, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slaqz0_(void* wants, void* wantq, void* wantz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rec, void* info)
{
	void (*fn) (void* wants, void* wantq, void* wantz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rec, void* info);

	*(void **) & fn = current_backend->lapack.slaqz0.f77_blas_function; 

		fn((void*) wants, (void*) wantq, (void*) wantz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rec, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_slaqz0(void* wants, void* wantq, void* wantz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rec, void* info) __attribute__((alias("flexiblas_real_slaqz0_")));
#else
void flexiblas_real_slaqz0(void* wants, void* wantq, void* wantz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rec, void* info){flexiblas_real_slaqz0_((void*) wants, (void*) wantq, (void*) wantz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rec, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_slaqz0_(void* wants, void* wantq, void* wantz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rec, void* info)
{
	void (*fn) (void* wants, void* wantq, void* wantz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rec, void* info);
	void (*fn_hook) (void* wants, void* wantq, void* wantz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rec, void* info);

	*(void **) &fn      = current_backend->lapack.slaqz0.f77_blas_function; 

    hook_pos_slaqz0 ++;
    if( hook_pos_slaqz0 < __flexiblas_hooks->slaqz0.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->slaqz0.f77_hook_function[hook_pos_slaqz0];
        fn_hook((void*) wants, (void*) wantq, (void*) wantz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rec, (void*) info);
    } else {
        hook_pos_slaqz0 = 0;
		fn((void*) wants, (void*) wantq, (void*) wantz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rec, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_slaqz0(void* wants, void* wantq, void* wantz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rec, void* info) __attribute__((alias("flexiblas_chain_slaqz0_")));
#else
void flexiblas_chain_slaqz0(void* wants, void* wantq, void* wantz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* alphar, void* alphai, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rec, void* info){flexiblas_chain_slaqz0_((void*) wants, (void*) wantq, (void*) wantz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) alphar, (void*) alphai, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rec, (void*) info);}
#endif



