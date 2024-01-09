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



static TLS_STORE uint8_t hook_pos_ctprfs = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(ctprfs,CTPRFS)(char* uplo, char* trans, char* diag, blasint* n, blasint* nrhs, float complex* ap, float complex* b, blasint* ldb, float complex* x, blasint* ldx, float* ferr, float* berr, float complex* work, float* rwork, blasint* info)
#else
void FC_GLOBAL(ctprfs,CTPRFS)(char* uplo, char* trans, char* diag, blasint* n, blasint* nrhs, float complex* ap, float complex* b, blasint* ldb, float complex* x, blasint* ldx, float* ferr, float* berr, float complex* work, float* rwork, blasint* info)
#endif
{
	void (*fn) (void* uplo, void* trans, void* diag, void* n, void* nrhs, void* ap, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* rwork, void* info);
	void (*fn_hook) (void* uplo, void* trans, void* diag, void* n, void* nrhs, void* ap, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* rwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.ctprfs.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->ctprfs.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) nrhs, (void*) ap, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) rwork, (void*) info); 
		return;
	} else {
		hook_pos_ctprfs = 0;
		fn_hook((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) nrhs, (void*) ap, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) rwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void ctprfs_(char* uplo, char* trans, char* diag, blasint* n, blasint* nrhs, float complex* ap, float complex* b, blasint* ldb, float complex* x, blasint* ldx, float* ferr, float* berr, float complex* work, float* rwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(ctprfs,CTPRFS)))));
#else
#ifndef __APPLE__
void ctprfs(char* uplo, char* trans, char* diag, blasint* n, blasint* nrhs, float complex* ap, float complex* b, blasint* ldb, float complex* x, blasint* ldx, float* ferr, float* berr, float complex* work, float* rwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(ctprfs,CTPRFS)))));
#else
void ctprfs(char* uplo, char* trans, char* diag, blasint* n, blasint* nrhs, float complex* ap, float complex* b, blasint* ldb, float complex* x, blasint* ldx, float* ferr, float* berr, float complex* work, float* rwork, blasint* info){ FC_GLOBAL(ctprfs,CTPRFS)((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) nrhs, (void*) ap, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) rwork, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_ctprfs_(void* uplo, void* trans, void* diag, void* n, void* nrhs, void* ap, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* rwork, void* info)
{
	void (*fn) (void* uplo, void* trans, void* diag, void* n, void* nrhs, void* ap, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* rwork, void* info);

	*(void **) & fn = current_backend->lapack.ctprfs.f77_blas_function; 

		fn((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) nrhs, (void*) ap, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) rwork, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_ctprfs(void* uplo, void* trans, void* diag, void* n, void* nrhs, void* ap, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* rwork, void* info) __attribute__((alias("flexiblas_real_ctprfs_")));
#else
void flexiblas_real_ctprfs(void* uplo, void* trans, void* diag, void* n, void* nrhs, void* ap, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* rwork, void* info){flexiblas_real_ctprfs_((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) nrhs, (void*) ap, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) rwork, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_ctprfs_(void* uplo, void* trans, void* diag, void* n, void* nrhs, void* ap, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* rwork, void* info)
{
	void (*fn) (void* uplo, void* trans, void* diag, void* n, void* nrhs, void* ap, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* rwork, void* info);
	void (*fn_hook) (void* uplo, void* trans, void* diag, void* n, void* nrhs, void* ap, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* rwork, void* info);

	*(void **) &fn      = current_backend->lapack.ctprfs.f77_blas_function; 

    hook_pos_ctprfs ++;
    if( hook_pos_ctprfs < __flexiblas_hooks->ctprfs.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->ctprfs.f77_hook_function[hook_pos_ctprfs];
        fn_hook((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) nrhs, (void*) ap, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) rwork, (void*) info);
    } else {
        hook_pos_ctprfs = 0;
		fn((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) nrhs, (void*) ap, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) rwork, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_ctprfs(void* uplo, void* trans, void* diag, void* n, void* nrhs, void* ap, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* rwork, void* info) __attribute__((alias("flexiblas_chain_ctprfs_")));
#else
void flexiblas_chain_ctprfs(void* uplo, void* trans, void* diag, void* n, void* nrhs, void* ap, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* rwork, void* info){flexiblas_chain_ctprfs_((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) nrhs, (void*) ap, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) rwork, (void*) info);}
#endif



