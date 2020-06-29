/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) Martin Koehler, 2013-2020
 */
 /* This file it automatically generated. Please do not edit. */
 /* Generated: Wed Mar 28 11:20:04 2018 */
        
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <complex.h>

#include "fortran_mangle.h"

#include "flexiblas.h"


#ifdef INTEGER8
#define blasint int64_t
#else
#define blasint int
#endif



static TLS_STORE uint8_t hook_pos_zgerfs = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zgerfs,ZGERFS)(char* trans, blasint* n, blasint* nrhs, double complex* a, blasint* lda, double complex* af, blasint* ldaf, blasint* ipiv, double complex* b, blasint* ldb, double complex* x, blasint* ldx, double* ferr, double* berr, double complex* work, double* rwork, blasint* info)
#else
void FC_GLOBAL(zgerfs,ZGERFS)(char* trans, blasint* n, blasint* nrhs, double complex* a, blasint* lda, double complex* af, blasint* ldaf, blasint* ipiv, double complex* b, blasint* ldb, double complex* x, blasint* ldx, double* ferr, double* berr, double complex* work, double* rwork, blasint* info)
#endif
{
	void (*fn) (void* trans, void* n, void* nrhs, void* a, void* lda, void* af, void* ldaf, void* ipiv, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* rwork, void* info);
	void (*fn_hook) (void* trans, void* n, void* nrhs, void* a, void* lda, void* af, void* ldaf, void* ipiv, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* rwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.zgerfs.f77_blas_function; 
	fn_hook = __flexiblas_hooks->zgerfs.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) trans, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) af, (void*) ldaf, (void*) ipiv, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) rwork, (void*) info); 
		return;
	} else {
		hook_pos_zgerfs = 0;
		fn_hook((void*) trans, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) af, (void*) ldaf, (void*) ipiv, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) rwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void zgerfs_(char* trans, blasint* n, blasint* nrhs, double complex* a, blasint* lda, double complex* af, blasint* ldaf, blasint* ipiv, double complex* b, blasint* ldb, double complex* x, blasint* ldx, double* ferr, double* berr, double complex* work, double* rwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(zgerfs,ZGERFS)))));
#else
void zgerfs(char* trans, blasint* n, blasint* nrhs, double complex* a, blasint* lda, double complex* af, blasint* ldaf, blasint* ipiv, double complex* b, blasint* ldb, double complex* x, blasint* ldx, double* ferr, double* berr, double complex* work, double* rwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(zgerfs,ZGERFS)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zgerfs_(void* trans, void* n, void* nrhs, void* a, void* lda, void* af, void* ldaf, void* ipiv, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* rwork, void* info)
{
	void (*fn) (void* trans, void* n, void* nrhs, void* a, void* lda, void* af, void* ldaf, void* ipiv, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* rwork, void* info);

	fn = current_backend->lapack.zgerfs.f77_blas_function; 

		fn((void*) trans, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) af, (void*) ldaf, (void*) ipiv, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) rwork, (void*) info); 

	return;
}

void flexiblas_real_zgerfs(void* trans, void* n, void* nrhs, void* a, void* lda, void* af, void* ldaf, void* ipiv, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* rwork, void* info)  __attribute__((alias("flexiblas_real_zgerfs_")));





/* Chainloader for Hooks */


void flexiblas_chain_zgerfs_(void* trans, void* n, void* nrhs, void* a, void* lda, void* af, void* ldaf, void* ipiv, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* rwork, void* info)
{
	void (*fn) (void* trans, void* n, void* nrhs, void* a, void* lda, void* af, void* ldaf, void* ipiv, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* rwork, void* info);
	void (*fn_hook) (void* trans, void* n, void* nrhs, void* a, void* lda, void* af, void* ldaf, void* ipiv, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* rwork, void* info);

	fn      = current_backend->lapack.zgerfs.f77_blas_function; 

    hook_pos_zgerfs ++;
    if( hook_pos_zgerfs < __flexiblas_hooks->zgerfs.nhook) {
        fn_hook = __flexiblas_hooks->zgerfs.f77_hook_function[hook_pos_zgerfs];
        fn_hook((void*) trans, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) af, (void*) ldaf, (void*) ipiv, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) rwork, (void*) info);
    } else {
        hook_pos_zgerfs = 0;
		fn((void*) trans, (void*) n, (void*) nrhs, (void*) a, (void*) lda, (void*) af, (void*) ldaf, (void*) ipiv, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) ferr, (void*) berr, (void*) work, (void*) rwork, (void*) info); 
	}
	return;
}

void flexiblas_chain_zgerfs(void* trans, void* n, void* nrhs, void* a, void* lda, void* af, void* ldaf, void* ipiv, void* b, void* ldb, void* x, void* ldx, void* ferr, void* berr, void* work, void* rwork, void* info)  __attribute__((alias("flexiblas_chain_zgerfs_")));




