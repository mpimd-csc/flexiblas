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


#ifndef FLEXIBLAS_CHARLEN_T
#define FLEXIBLAS_CHARLEN_T
#if __GNUC__ > 7
typedef size_t flexiblas_fortran_charlen_t;
#else
typedef int flexiblas_fortran_charlen_t;
#endif
#endif

#ifndef blasint
#ifdef FLEXIBLAS_INTEGER8
#define blasint int64_t
#else
#define blasint int
#endif
#endif



static TLS_STORE uint8_t hook_pos_clacpy = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(clacpy,CLACPY)(char* uplo, blasint* m, blasint* n, float complex* a, blasint* lda, float complex* b, blasint* ldb, flexiblas_fortran_charlen_t len_uplo)
#else
void FC_GLOBAL(clacpy,CLACPY)(char* uplo, blasint* m, blasint* n, float complex* a, blasint* lda, float complex* b, blasint* ldb, flexiblas_fortran_charlen_t len_uplo)
#endif
{
	void (*fn) (void* uplo, void* m, void* n, void* a, void* lda, void* b, void* ldb, flexiblas_fortran_charlen_t len_uplo);
	void (*fn_hook) (void* uplo, void* m, void* n, void* a, void* lda, void* b, void* ldb, flexiblas_fortran_charlen_t len_uplo);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.clacpy.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->clacpy.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) uplo, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, ( flexiblas_fortran_charlen_t ) len_uplo); 
		return;
	} else {
		hook_pos_clacpy = 0;
		fn_hook((void*) uplo, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, ( flexiblas_fortran_charlen_t ) len_uplo);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void clacpy_(char* uplo, blasint* m, blasint* n, float complex* a, blasint* lda, float complex* b, blasint* ldb, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL(clacpy,CLACPY)))));
#else
#ifndef __APPLE__
void clacpy(char* uplo, blasint* m, blasint* n, float complex* a, blasint* lda, float complex* b, blasint* ldb, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL(clacpy,CLACPY)))));
#else
void clacpy(char* uplo, blasint* m, blasint* n, float complex* a, blasint* lda, float complex* b, blasint* ldb, flexiblas_fortran_charlen_t len_uplo){ FC_GLOBAL(clacpy,CLACPY)((void*) uplo, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (flexiblas_fortran_charlen_t) len_uplo); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_clacpy_(void* uplo, void* m, void* n, void* a, void* lda, void* b, void* ldb, flexiblas_fortran_charlen_t len_uplo)
{
	void (*fn) (void* uplo, void* m, void* n, void* a, void* lda, void* b, void* ldb, flexiblas_fortran_charlen_t len_uplo);

	*(void **) & fn = current_backend->lapack.clacpy.f77_blas_function; 

		fn((void*) uplo, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, ( flexiblas_fortran_charlen_t ) len_uplo); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_clacpy(void* uplo, void* m, void* n, void* a, void* lda, void* b, void* ldb, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_real_clacpy_")));
#else
void flexiblas_real_clacpy(void* uplo, void* m, void* n, void* a, void* lda, void* b, void* ldb, flexiblas_fortran_charlen_t len_uplo){flexiblas_real_clacpy_((void*) uplo, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (flexiblas_fortran_charlen_t) len_uplo);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_clacpy_(void* uplo, void* m, void* n, void* a, void* lda, void* b, void* ldb, flexiblas_fortran_charlen_t len_uplo)
{
	void (*fn) (void* uplo, void* m, void* n, void* a, void* lda, void* b, void* ldb, flexiblas_fortran_charlen_t len_uplo);
	void (*fn_hook) (void* uplo, void* m, void* n, void* a, void* lda, void* b, void* ldb, flexiblas_fortran_charlen_t len_uplo);

	*(void **) &fn      = current_backend->lapack.clacpy.f77_blas_function; 

    hook_pos_clacpy ++;
    if( hook_pos_clacpy < __flexiblas_hooks->clacpy.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->clacpy.f77_hook_function[hook_pos_clacpy];
        fn_hook((void*) uplo, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, ( flexiblas_fortran_charlen_t ) len_uplo);
    } else {
        hook_pos_clacpy = 0;
		fn((void*) uplo, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, ( flexiblas_fortran_charlen_t ) len_uplo); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_clacpy(void* uplo, void* m, void* n, void* a, void* lda, void* b, void* ldb, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_chain_clacpy_")));
#else
void flexiblas_chain_clacpy(void* uplo, void* m, void* n, void* a, void* lda, void* b, void* ldb, flexiblas_fortran_charlen_t len_uplo){flexiblas_chain_clacpy_((void*) uplo, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (flexiblas_fortran_charlen_t) len_uplo);}
#endif



