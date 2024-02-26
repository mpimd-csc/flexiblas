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



static TLS_STORE uint8_t hook_pos_dlag2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlag2,DLAG2)(double* a, blasint* lda, double* b, blasint* ldb, double* safmin, double* scale1, double* scale2, double* wr1, double* wr2, double* wi)
#else
void FC_GLOBAL(dlag2,DLAG2)(double* a, blasint* lda, double* b, blasint* ldb, double* safmin, double* scale1, double* scale2, double* wr1, double* wr2, double* wi)
#endif
{
	void (*fn) (void* a, void* lda, void* b, void* ldb, void* safmin, void* scale1, void* scale2, void* wr1, void* wr2, void* wi);
	void (*fn_hook) (void* a, void* lda, void* b, void* ldb, void* safmin, void* scale1, void* scale2, void* wr1, void* wr2, void* wi);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dlag2.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dlag2.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) safmin, (void*) scale1, (void*) scale2, (void*) wr1, (void*) wr2, (void*) wi); 
		return;
	} else {
		hook_pos_dlag2 = 0;
		fn_hook((void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) safmin, (void*) scale1, (void*) scale2, (void*) wr1, (void*) wr2, (void*) wi);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dlag2_(double* a, blasint* lda, double* b, blasint* ldb, double* safmin, double* scale1, double* scale2, double* wr1, double* wr2, double* wi) __attribute__((alias(MTS(FC_GLOBAL(dlag2,DLAG2)))));
#else
#ifndef __APPLE__
void dlag2(double* a, blasint* lda, double* b, blasint* ldb, double* safmin, double* scale1, double* scale2, double* wr1, double* wr2, double* wi) __attribute__((alias(MTS(FC_GLOBAL(dlag2,DLAG2)))));
#else
void dlag2(double* a, blasint* lda, double* b, blasint* ldb, double* safmin, double* scale1, double* scale2, double* wr1, double* wr2, double* wi){ FC_GLOBAL(dlag2,DLAG2)((void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) safmin, (void*) scale1, (void*) scale2, (void*) wr1, (void*) wr2, (void*) wi); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlag2_(void* a, void* lda, void* b, void* ldb, void* safmin, void* scale1, void* scale2, void* wr1, void* wr2, void* wi)
{
	void (*fn) (void* a, void* lda, void* b, void* ldb, void* safmin, void* scale1, void* scale2, void* wr1, void* wr2, void* wi);

	*(void **) & fn = current_backend->lapack.dlag2.f77_blas_function; 

		fn((void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) safmin, (void*) scale1, (void*) scale2, (void*) wr1, (void*) wr2, (void*) wi); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_dlag2(void* a, void* lda, void* b, void* ldb, void* safmin, void* scale1, void* scale2, void* wr1, void* wr2, void* wi) __attribute__((alias("flexiblas_real_dlag2_")));
#else
void flexiblas_real_dlag2(void* a, void* lda, void* b, void* ldb, void* safmin, void* scale1, void* scale2, void* wr1, void* wr2, void* wi){flexiblas_real_dlag2_((void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) safmin, (void*) scale1, (void*) scale2, (void*) wr1, (void*) wr2, (void*) wi);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlag2_(void* a, void* lda, void* b, void* ldb, void* safmin, void* scale1, void* scale2, void* wr1, void* wr2, void* wi)
{
	void (*fn) (void* a, void* lda, void* b, void* ldb, void* safmin, void* scale1, void* scale2, void* wr1, void* wr2, void* wi);
	void (*fn_hook) (void* a, void* lda, void* b, void* ldb, void* safmin, void* scale1, void* scale2, void* wr1, void* wr2, void* wi);

	*(void **) &fn      = current_backend->lapack.dlag2.f77_blas_function; 

    hook_pos_dlag2 ++;
    if( hook_pos_dlag2 < __flexiblas_hooks->dlag2.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlag2.f77_hook_function[hook_pos_dlag2];
        fn_hook((void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) safmin, (void*) scale1, (void*) scale2, (void*) wr1, (void*) wr2, (void*) wi);
    } else {
        hook_pos_dlag2 = 0;
		fn((void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) safmin, (void*) scale1, (void*) scale2, (void*) wr1, (void*) wr2, (void*) wi); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_dlag2(void* a, void* lda, void* b, void* ldb, void* safmin, void* scale1, void* scale2, void* wr1, void* wr2, void* wi) __attribute__((alias("flexiblas_chain_dlag2_")));
#else
void flexiblas_chain_dlag2(void* a, void* lda, void* b, void* ldb, void* safmin, void* scale1, void* scale2, void* wr1, void* wr2, void* wi){flexiblas_chain_dlag2_((void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) safmin, (void*) scale1, (void*) scale2, (void*) wr1, (void*) wr2, (void*) wi);}
#endif



