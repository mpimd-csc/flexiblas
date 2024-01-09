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



static TLS_STORE uint8_t hook_pos_zgghd3 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zgghd3,ZGGHD3)(char* compq, char* compz, blasint* n, blasint* ilo, blasint* ihi, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* q, blasint* ldq, double complex* z, blasint* ldz, double complex* work, blasint* lwork, blasint* info)
#else
void FC_GLOBAL(zgghd3,ZGGHD3)(char* compq, char* compz, blasint* n, blasint* ilo, blasint* ihi, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* q, blasint* ldq, double complex* z, blasint* ldz, double complex* work, blasint* lwork, blasint* info)
#endif
{
	void (*fn) (void* compq, void* compz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* info);
	void (*fn_hook) (void* compq, void* compz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.zgghd3.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->zgghd3.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) compq, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) info); 
		return;
	} else {
		hook_pos_zgghd3 = 0;
		fn_hook((void*) compq, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void zgghd3_(char* compq, char* compz, blasint* n, blasint* ilo, blasint* ihi, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* q, blasint* ldq, double complex* z, blasint* ldz, double complex* work, blasint* lwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(zgghd3,ZGGHD3)))));
#else
#ifndef __APPLE__
void zgghd3(char* compq, char* compz, blasint* n, blasint* ilo, blasint* ihi, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* q, blasint* ldq, double complex* z, blasint* ldz, double complex* work, blasint* lwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(zgghd3,ZGGHD3)))));
#else
void zgghd3(char* compq, char* compz, blasint* n, blasint* ilo, blasint* ihi, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* q, blasint* ldq, double complex* z, blasint* ldz, double complex* work, blasint* lwork, blasint* info){ FC_GLOBAL(zgghd3,ZGGHD3)((void*) compq, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zgghd3_(void* compq, void* compz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* info)
{
	void (*fn) (void* compq, void* compz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* info);

	*(void **) & fn = current_backend->lapack.zgghd3.f77_blas_function; 

		fn((void*) compq, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_zgghd3(void* compq, void* compz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* info) __attribute__((alias("flexiblas_real_zgghd3_")));
#else
void flexiblas_real_zgghd3(void* compq, void* compz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* info){flexiblas_real_zgghd3_((void*) compq, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zgghd3_(void* compq, void* compz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* info)
{
	void (*fn) (void* compq, void* compz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* info);
	void (*fn_hook) (void* compq, void* compz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* info);

	*(void **) &fn      = current_backend->lapack.zgghd3.f77_blas_function; 

    hook_pos_zgghd3 ++;
    if( hook_pos_zgghd3 < __flexiblas_hooks->zgghd3.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zgghd3.f77_hook_function[hook_pos_zgghd3];
        fn_hook((void*) compq, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) info);
    } else {
        hook_pos_zgghd3 = 0;
		fn((void*) compq, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_zgghd3(void* compq, void* compz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* info) __attribute__((alias("flexiblas_chain_zgghd3_")));
#else
void flexiblas_chain_zgghd3(void* compq, void* compz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* info){flexiblas_chain_zgghd3_((void*) compq, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) info);}
#endif



