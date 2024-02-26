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



static TLS_STORE uint8_t hook_pos_cgecon = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(cgecon,CGECON)(char* norm, blasint* n, float complex* a, blasint* lda, float* anorm, float* rcond, float complex* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_norm)
#else
void FC_GLOBAL(cgecon,CGECON)(char* norm, blasint* n, float complex* a, blasint* lda, float* anorm, float* rcond, float complex* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_norm)
#endif
{
	void (*fn) (void* norm, void* n, void* a, void* lda, void* anorm, void* rcond, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_norm);
	void (*fn_hook) (void* norm, void* n, void* a, void* lda, void* anorm, void* rcond, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_norm);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.cgecon.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->cgecon.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) norm, (void*) n, (void*) a, (void*) lda, (void*) anorm, (void*) rcond, (void*) work, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_norm); 
		return;
	} else {
		hook_pos_cgecon = 0;
		fn_hook((void*) norm, (void*) n, (void*) a, (void*) lda, (void*) anorm, (void*) rcond, (void*) work, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_norm);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void cgecon_(char* norm, blasint* n, float complex* a, blasint* lda, float* anorm, float* rcond, float complex* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_norm) __attribute__((alias(MTS(FC_GLOBAL(cgecon,CGECON)))));
#else
#ifndef __APPLE__
void cgecon(char* norm, blasint* n, float complex* a, blasint* lda, float* anorm, float* rcond, float complex* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_norm) __attribute__((alias(MTS(FC_GLOBAL(cgecon,CGECON)))));
#else
void cgecon(char* norm, blasint* n, float complex* a, blasint* lda, float* anorm, float* rcond, float complex* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_norm){ FC_GLOBAL(cgecon,CGECON)((void*) norm, (void*) n, (void*) a, (void*) lda, (void*) anorm, (void*) rcond, (void*) work, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_norm); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_cgecon_(void* norm, void* n, void* a, void* lda, void* anorm, void* rcond, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_norm)
{
	void (*fn) (void* norm, void* n, void* a, void* lda, void* anorm, void* rcond, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_norm);

	*(void **) & fn = current_backend->lapack.cgecon.f77_blas_function; 

		fn((void*) norm, (void*) n, (void*) a, (void*) lda, (void*) anorm, (void*) rcond, (void*) work, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_norm); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_cgecon(void* norm, void* n, void* a, void* lda, void* anorm, void* rcond, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_norm) __attribute__((alias("flexiblas_real_cgecon_")));
#else
void flexiblas_real_cgecon(void* norm, void* n, void* a, void* lda, void* anorm, void* rcond, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_norm){flexiblas_real_cgecon_((void*) norm, (void*) n, (void*) a, (void*) lda, (void*) anorm, (void*) rcond, (void*) work, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_norm);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_cgecon_(void* norm, void* n, void* a, void* lda, void* anorm, void* rcond, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_norm)
{
	void (*fn) (void* norm, void* n, void* a, void* lda, void* anorm, void* rcond, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_norm);
	void (*fn_hook) (void* norm, void* n, void* a, void* lda, void* anorm, void* rcond, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_norm);

	*(void **) &fn      = current_backend->lapack.cgecon.f77_blas_function; 

    hook_pos_cgecon ++;
    if( hook_pos_cgecon < __flexiblas_hooks->cgecon.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->cgecon.f77_hook_function[hook_pos_cgecon];
        fn_hook((void*) norm, (void*) n, (void*) a, (void*) lda, (void*) anorm, (void*) rcond, (void*) work, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_norm);
    } else {
        hook_pos_cgecon = 0;
		fn((void*) norm, (void*) n, (void*) a, (void*) lda, (void*) anorm, (void*) rcond, (void*) work, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_norm); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_cgecon(void* norm, void* n, void* a, void* lda, void* anorm, void* rcond, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_norm) __attribute__((alias("flexiblas_chain_cgecon_")));
#else
void flexiblas_chain_cgecon(void* norm, void* n, void* a, void* lda, void* anorm, void* rcond, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_norm){flexiblas_chain_cgecon_((void*) norm, (void*) n, (void*) a, (void*) lda, (void*) anorm, (void*) rcond, (void*) work, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_norm);}
#endif



