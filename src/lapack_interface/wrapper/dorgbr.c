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



static TLS_STORE uint8_t hook_pos_dorgbr = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dorgbr,DORGBR)(char* vect, blasint* m, blasint* n, blasint* k, double* a, blasint* lda, double* tau, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_vect)
#else
void FC_GLOBAL(dorgbr,DORGBR)(char* vect, blasint* m, blasint* n, blasint* k, double* a, blasint* lda, double* tau, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_vect)
#endif
{
	void (*fn) (void* vect, void* m, void* n, void* k, void* a, void* lda, void* tau, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_vect);
	void (*fn_hook) (void* vect, void* m, void* n, void* k, void* a, void* lda, void* tau, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_vect);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dorgbr.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dorgbr.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) vect, (void*) m, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) tau, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_vect); 
		return;
	} else {
		hook_pos_dorgbr = 0;
		fn_hook((void*) vect, (void*) m, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) tau, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_vect);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dorgbr_(char* vect, blasint* m, blasint* n, blasint* k, double* a, blasint* lda, double* tau, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_vect) __attribute__((alias(MTS(FC_GLOBAL(dorgbr,DORGBR)))));
#else
#ifndef __APPLE__
void dorgbr(char* vect, blasint* m, blasint* n, blasint* k, double* a, blasint* lda, double* tau, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_vect) __attribute__((alias(MTS(FC_GLOBAL(dorgbr,DORGBR)))));
#else
void dorgbr(char* vect, blasint* m, blasint* n, blasint* k, double* a, blasint* lda, double* tau, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_vect){ FC_GLOBAL(dorgbr,DORGBR)((void*) vect, (void*) m, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) tau, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_vect); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dorgbr_(void* vect, void* m, void* n, void* k, void* a, void* lda, void* tau, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_vect)
{
	void (*fn) (void* vect, void* m, void* n, void* k, void* a, void* lda, void* tau, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_vect);

	*(void **) & fn = current_backend->lapack.dorgbr.f77_blas_function; 

		fn((void*) vect, (void*) m, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) tau, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_vect); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_dorgbr(void* vect, void* m, void* n, void* k, void* a, void* lda, void* tau, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_vect) __attribute__((alias("flexiblas_real_dorgbr_")));
#else
void flexiblas_real_dorgbr(void* vect, void* m, void* n, void* k, void* a, void* lda, void* tau, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_vect){flexiblas_real_dorgbr_((void*) vect, (void*) m, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) tau, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_vect);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dorgbr_(void* vect, void* m, void* n, void* k, void* a, void* lda, void* tau, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_vect)
{
	void (*fn) (void* vect, void* m, void* n, void* k, void* a, void* lda, void* tau, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_vect);
	void (*fn_hook) (void* vect, void* m, void* n, void* k, void* a, void* lda, void* tau, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_vect);

	*(void **) &fn      = current_backend->lapack.dorgbr.f77_blas_function; 

    hook_pos_dorgbr ++;
    if( hook_pos_dorgbr < __flexiblas_hooks->dorgbr.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dorgbr.f77_hook_function[hook_pos_dorgbr];
        fn_hook((void*) vect, (void*) m, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) tau, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_vect);
    } else {
        hook_pos_dorgbr = 0;
		fn((void*) vect, (void*) m, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) tau, (void*) work, (void*) lwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_vect); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_dorgbr(void* vect, void* m, void* n, void* k, void* a, void* lda, void* tau, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_vect) __attribute__((alias("flexiblas_chain_dorgbr_")));
#else
void flexiblas_chain_dorgbr(void* vect, void* m, void* n, void* k, void* a, void* lda, void* tau, void* work, void* lwork, void* info, flexiblas_fortran_charlen_t len_vect){flexiblas_chain_dorgbr_((void*) vect, (void*) m, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) tau, (void*) work, (void*) lwork, (void*) info, (flexiblas_fortran_charlen_t) len_vect);}
#endif



