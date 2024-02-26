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



static TLS_STORE uint8_t hook_pos_dlaqz1 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlaqz1,DLAQZ1)(double* a, blasint* lda, double* b, blasint* ldb, double* sr1, double* sr2, double* si, double* beta1, double* beta2, double* v)
#else
void FC_GLOBAL(dlaqz1,DLAQZ1)(double* a, blasint* lda, double* b, blasint* ldb, double* sr1, double* sr2, double* si, double* beta1, double* beta2, double* v)
#endif
{
	void (*fn) (void* a, void* lda, void* b, void* ldb, void* sr1, void* sr2, void* si, void* beta1, void* beta2, void* v);
	void (*fn_hook) (void* a, void* lda, void* b, void* ldb, void* sr1, void* sr2, void* si, void* beta1, void* beta2, void* v);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dlaqz1.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dlaqz1.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) sr1, (void*) sr2, (void*) si, (void*) beta1, (void*) beta2, (void*) v); 
		return;
	} else {
		hook_pos_dlaqz1 = 0;
		fn_hook((void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) sr1, (void*) sr2, (void*) si, (void*) beta1, (void*) beta2, (void*) v);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dlaqz1_(double* a, blasint* lda, double* b, blasint* ldb, double* sr1, double* sr2, double* si, double* beta1, double* beta2, double* v) __attribute__((alias(MTS(FC_GLOBAL(dlaqz1,DLAQZ1)))));
#else
#ifndef __APPLE__
void dlaqz1(double* a, blasint* lda, double* b, blasint* ldb, double* sr1, double* sr2, double* si, double* beta1, double* beta2, double* v) __attribute__((alias(MTS(FC_GLOBAL(dlaqz1,DLAQZ1)))));
#else
void dlaqz1(double* a, blasint* lda, double* b, blasint* ldb, double* sr1, double* sr2, double* si, double* beta1, double* beta2, double* v){ FC_GLOBAL(dlaqz1,DLAQZ1)((void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) sr1, (void*) sr2, (void*) si, (void*) beta1, (void*) beta2, (void*) v); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlaqz1_(void* a, void* lda, void* b, void* ldb, void* sr1, void* sr2, void* si, void* beta1, void* beta2, void* v)
{
	void (*fn) (void* a, void* lda, void* b, void* ldb, void* sr1, void* sr2, void* si, void* beta1, void* beta2, void* v);

	*(void **) & fn = current_backend->lapack.dlaqz1.f77_blas_function; 

		fn((void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) sr1, (void*) sr2, (void*) si, (void*) beta1, (void*) beta2, (void*) v); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_dlaqz1(void* a, void* lda, void* b, void* ldb, void* sr1, void* sr2, void* si, void* beta1, void* beta2, void* v) __attribute__((alias("flexiblas_real_dlaqz1_")));
#else
void flexiblas_real_dlaqz1(void* a, void* lda, void* b, void* ldb, void* sr1, void* sr2, void* si, void* beta1, void* beta2, void* v){flexiblas_real_dlaqz1_((void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) sr1, (void*) sr2, (void*) si, (void*) beta1, (void*) beta2, (void*) v);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlaqz1_(void* a, void* lda, void* b, void* ldb, void* sr1, void* sr2, void* si, void* beta1, void* beta2, void* v)
{
	void (*fn) (void* a, void* lda, void* b, void* ldb, void* sr1, void* sr2, void* si, void* beta1, void* beta2, void* v);
	void (*fn_hook) (void* a, void* lda, void* b, void* ldb, void* sr1, void* sr2, void* si, void* beta1, void* beta2, void* v);

	*(void **) &fn      = current_backend->lapack.dlaqz1.f77_blas_function; 

    hook_pos_dlaqz1 ++;
    if( hook_pos_dlaqz1 < __flexiblas_hooks->dlaqz1.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlaqz1.f77_hook_function[hook_pos_dlaqz1];
        fn_hook((void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) sr1, (void*) sr2, (void*) si, (void*) beta1, (void*) beta2, (void*) v);
    } else {
        hook_pos_dlaqz1 = 0;
		fn((void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) sr1, (void*) sr2, (void*) si, (void*) beta1, (void*) beta2, (void*) v); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_dlaqz1(void* a, void* lda, void* b, void* ldb, void* sr1, void* sr2, void* si, void* beta1, void* beta2, void* v) __attribute__((alias("flexiblas_chain_dlaqz1_")));
#else
void flexiblas_chain_dlaqz1(void* a, void* lda, void* b, void* ldb, void* sr1, void* sr2, void* si, void* beta1, void* beta2, void* v){flexiblas_chain_dlaqz1_((void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) sr1, (void*) sr2, (void*) si, (void*) beta1, (void*) beta2, (void*) v);}
#endif



