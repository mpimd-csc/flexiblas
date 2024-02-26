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



static TLS_STORE uint8_t hook_pos_cgeqp3rk = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(cgeqp3rk,CGEQP3RK)(blasint* m, blasint* n, blasint* nrhs, blasint* kmax, float* abstol, float* reltol, float complex* a, blasint* lda, blasint* k, float* maxc2nrmk, float* relmaxc2nrmk, blasint* jpiv, float complex* tau, float complex* work, blasint* lwork, float* rwork, blasint* iwork, blasint* info)
#else
void FC_GLOBAL(cgeqp3rk,CGEQP3RK)(blasint* m, blasint* n, blasint* nrhs, blasint* kmax, float* abstol, float* reltol, float complex* a, blasint* lda, blasint* k, float* maxc2nrmk, float* relmaxc2nrmk, blasint* jpiv, float complex* tau, float complex* work, blasint* lwork, float* rwork, blasint* iwork, blasint* info)
#endif
{
	void (*fn) (void* m, void* n, void* nrhs, void* kmax, void* abstol, void* reltol, void* a, void* lda, void* k, void* maxc2nrmk, void* relmaxc2nrmk, void* jpiv, void* tau, void* work, void* lwork, void* rwork, void* iwork, void* info);
	void (*fn_hook) (void* m, void* n, void* nrhs, void* kmax, void* abstol, void* reltol, void* a, void* lda, void* k, void* maxc2nrmk, void* relmaxc2nrmk, void* jpiv, void* tau, void* work, void* lwork, void* rwork, void* iwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.cgeqp3rk.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->cgeqp3rk.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) m, (void*) n, (void*) nrhs, (void*) kmax, (void*) abstol, (void*) reltol, (void*) a, (void*) lda, (void*) k, (void*) maxc2nrmk, (void*) relmaxc2nrmk, (void*) jpiv, (void*) tau, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) info); 
		return;
	} else {
		hook_pos_cgeqp3rk = 0;
		fn_hook((void*) m, (void*) n, (void*) nrhs, (void*) kmax, (void*) abstol, (void*) reltol, (void*) a, (void*) lda, (void*) k, (void*) maxc2nrmk, (void*) relmaxc2nrmk, (void*) jpiv, (void*) tau, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void cgeqp3rk_(blasint* m, blasint* n, blasint* nrhs, blasint* kmax, float* abstol, float* reltol, float complex* a, blasint* lda, blasint* k, float* maxc2nrmk, float* relmaxc2nrmk, blasint* jpiv, float complex* tau, float complex* work, blasint* lwork, float* rwork, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(cgeqp3rk,CGEQP3RK)))));
#else
#ifndef __APPLE__
void cgeqp3rk(blasint* m, blasint* n, blasint* nrhs, blasint* kmax, float* abstol, float* reltol, float complex* a, blasint* lda, blasint* k, float* maxc2nrmk, float* relmaxc2nrmk, blasint* jpiv, float complex* tau, float complex* work, blasint* lwork, float* rwork, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(cgeqp3rk,CGEQP3RK)))));
#else
void cgeqp3rk(blasint* m, blasint* n, blasint* nrhs, blasint* kmax, float* abstol, float* reltol, float complex* a, blasint* lda, blasint* k, float* maxc2nrmk, float* relmaxc2nrmk, blasint* jpiv, float complex* tau, float complex* work, blasint* lwork, float* rwork, blasint* iwork, blasint* info){ FC_GLOBAL(cgeqp3rk,CGEQP3RK)((void*) m, (void*) n, (void*) nrhs, (void*) kmax, (void*) abstol, (void*) reltol, (void*) a, (void*) lda, (void*) k, (void*) maxc2nrmk, (void*) relmaxc2nrmk, (void*) jpiv, (void*) tau, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_cgeqp3rk_(void* m, void* n, void* nrhs, void* kmax, void* abstol, void* reltol, void* a, void* lda, void* k, void* maxc2nrmk, void* relmaxc2nrmk, void* jpiv, void* tau, void* work, void* lwork, void* rwork, void* iwork, void* info)
{
	void (*fn) (void* m, void* n, void* nrhs, void* kmax, void* abstol, void* reltol, void* a, void* lda, void* k, void* maxc2nrmk, void* relmaxc2nrmk, void* jpiv, void* tau, void* work, void* lwork, void* rwork, void* iwork, void* info);

	*(void **) & fn = current_backend->lapack.cgeqp3rk.f77_blas_function; 

		fn((void*) m, (void*) n, (void*) nrhs, (void*) kmax, (void*) abstol, (void*) reltol, (void*) a, (void*) lda, (void*) k, (void*) maxc2nrmk, (void*) relmaxc2nrmk, (void*) jpiv, (void*) tau, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_cgeqp3rk(void* m, void* n, void* nrhs, void* kmax, void* abstol, void* reltol, void* a, void* lda, void* k, void* maxc2nrmk, void* relmaxc2nrmk, void* jpiv, void* tau, void* work, void* lwork, void* rwork, void* iwork, void* info) __attribute__((alias("flexiblas_real_cgeqp3rk_")));
#else
void flexiblas_real_cgeqp3rk(void* m, void* n, void* nrhs, void* kmax, void* abstol, void* reltol, void* a, void* lda, void* k, void* maxc2nrmk, void* relmaxc2nrmk, void* jpiv, void* tau, void* work, void* lwork, void* rwork, void* iwork, void* info){flexiblas_real_cgeqp3rk_((void*) m, (void*) n, (void*) nrhs, (void*) kmax, (void*) abstol, (void*) reltol, (void*) a, (void*) lda, (void*) k, (void*) maxc2nrmk, (void*) relmaxc2nrmk, (void*) jpiv, (void*) tau, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_cgeqp3rk_(void* m, void* n, void* nrhs, void* kmax, void* abstol, void* reltol, void* a, void* lda, void* k, void* maxc2nrmk, void* relmaxc2nrmk, void* jpiv, void* tau, void* work, void* lwork, void* rwork, void* iwork, void* info)
{
	void (*fn) (void* m, void* n, void* nrhs, void* kmax, void* abstol, void* reltol, void* a, void* lda, void* k, void* maxc2nrmk, void* relmaxc2nrmk, void* jpiv, void* tau, void* work, void* lwork, void* rwork, void* iwork, void* info);
	void (*fn_hook) (void* m, void* n, void* nrhs, void* kmax, void* abstol, void* reltol, void* a, void* lda, void* k, void* maxc2nrmk, void* relmaxc2nrmk, void* jpiv, void* tau, void* work, void* lwork, void* rwork, void* iwork, void* info);

	*(void **) &fn      = current_backend->lapack.cgeqp3rk.f77_blas_function; 

    hook_pos_cgeqp3rk ++;
    if( hook_pos_cgeqp3rk < __flexiblas_hooks->cgeqp3rk.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->cgeqp3rk.f77_hook_function[hook_pos_cgeqp3rk];
        fn_hook((void*) m, (void*) n, (void*) nrhs, (void*) kmax, (void*) abstol, (void*) reltol, (void*) a, (void*) lda, (void*) k, (void*) maxc2nrmk, (void*) relmaxc2nrmk, (void*) jpiv, (void*) tau, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) info);
    } else {
        hook_pos_cgeqp3rk = 0;
		fn((void*) m, (void*) n, (void*) nrhs, (void*) kmax, (void*) abstol, (void*) reltol, (void*) a, (void*) lda, (void*) k, (void*) maxc2nrmk, (void*) relmaxc2nrmk, (void*) jpiv, (void*) tau, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_cgeqp3rk(void* m, void* n, void* nrhs, void* kmax, void* abstol, void* reltol, void* a, void* lda, void* k, void* maxc2nrmk, void* relmaxc2nrmk, void* jpiv, void* tau, void* work, void* lwork, void* rwork, void* iwork, void* info) __attribute__((alias("flexiblas_chain_cgeqp3rk_")));
#else
void flexiblas_chain_cgeqp3rk(void* m, void* n, void* nrhs, void* kmax, void* abstol, void* reltol, void* a, void* lda, void* k, void* maxc2nrmk, void* relmaxc2nrmk, void* jpiv, void* tau, void* work, void* lwork, void* rwork, void* iwork, void* info){flexiblas_chain_cgeqp3rk_((void*) m, (void*) n, (void*) nrhs, (void*) kmax, (void*) abstol, (void*) reltol, (void*) a, (void*) lda, (void*) k, (void*) maxc2nrmk, (void*) relmaxc2nrmk, (void*) jpiv, (void*) tau, (void*) work, (void*) lwork, (void*) rwork, (void*) iwork, (void*) info);}
#endif



