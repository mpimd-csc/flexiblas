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



static TLS_STORE uint8_t hook_pos_dsygvx = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dsygvx,DSYGVX)(blasint* itype, char* jobz, char* range, char* uplo, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, blasint* m, double* w, double* z, blasint* ldz, double* work, blasint* lwork, blasint* iwork, blasint* ifail, blasint* info)
#else
void FC_GLOBAL(dsygvx,DSYGVX)(blasint* itype, char* jobz, char* range, char* uplo, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, blasint* m, double* w, double* z, blasint* ldz, double* work, blasint* lwork, blasint* iwork, blasint* ifail, blasint* info)
#endif
{
	void (*fn) (void* itype, void* jobz, void* range, void* uplo, void* n, void* a, void* lda, void* b, void* ldb, void* vl, void* vu, void* il, void* iu, void* abstol, void* m, void* w, void* z, void* ldz, void* work, void* lwork, void* iwork, void* ifail, void* info);
	void (*fn_hook) (void* itype, void* jobz, void* range, void* uplo, void* n, void* a, void* lda, void* b, void* ldb, void* vl, void* vu, void* il, void* iu, void* abstol, void* m, void* w, void* z, void* ldz, void* work, void* lwork, void* iwork, void* ifail, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.dsygvx.f77_blas_function; 
	fn_hook = __flexiblas_hooks->dsygvx.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) itype, (void*) jobz, (void*) range, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) abstol, (void*) m, (void*) w, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) iwork, (void*) ifail, (void*) info); 
		return;
	} else {
		hook_pos_dsygvx = 0;
		fn_hook((void*) itype, (void*) jobz, (void*) range, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) abstol, (void*) m, (void*) w, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) iwork, (void*) ifail, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dsygvx_(blasint* itype, char* jobz, char* range, char* uplo, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, blasint* m, double* w, double* z, blasint* ldz, double* work, blasint* lwork, blasint* iwork, blasint* ifail, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dsygvx,DSYGVX)))));
#else
void dsygvx(blasint* itype, char* jobz, char* range, char* uplo, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, blasint* m, double* w, double* z, blasint* ldz, double* work, blasint* lwork, blasint* iwork, blasint* ifail, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dsygvx,DSYGVX)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dsygvx_(void* itype, void* jobz, void* range, void* uplo, void* n, void* a, void* lda, void* b, void* ldb, void* vl, void* vu, void* il, void* iu, void* abstol, void* m, void* w, void* z, void* ldz, void* work, void* lwork, void* iwork, void* ifail, void* info)
{
	void (*fn) (void* itype, void* jobz, void* range, void* uplo, void* n, void* a, void* lda, void* b, void* ldb, void* vl, void* vu, void* il, void* iu, void* abstol, void* m, void* w, void* z, void* ldz, void* work, void* lwork, void* iwork, void* ifail, void* info);

	fn = current_backend->lapack.dsygvx.f77_blas_function; 

		fn((void*) itype, (void*) jobz, (void*) range, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) abstol, (void*) m, (void*) w, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) iwork, (void*) ifail, (void*) info); 

	return;
}

void flexiblas_real_dsygvx(void* itype, void* jobz, void* range, void* uplo, void* n, void* a, void* lda, void* b, void* ldb, void* vl, void* vu, void* il, void* iu, void* abstol, void* m, void* w, void* z, void* ldz, void* work, void* lwork, void* iwork, void* ifail, void* info)  __attribute__((alias("flexiblas_real_dsygvx_")));





/* Chainloader for Hooks */


void flexiblas_chain_dsygvx_(void* itype, void* jobz, void* range, void* uplo, void* n, void* a, void* lda, void* b, void* ldb, void* vl, void* vu, void* il, void* iu, void* abstol, void* m, void* w, void* z, void* ldz, void* work, void* lwork, void* iwork, void* ifail, void* info)
{
	void (*fn) (void* itype, void* jobz, void* range, void* uplo, void* n, void* a, void* lda, void* b, void* ldb, void* vl, void* vu, void* il, void* iu, void* abstol, void* m, void* w, void* z, void* ldz, void* work, void* lwork, void* iwork, void* ifail, void* info);
	void (*fn_hook) (void* itype, void* jobz, void* range, void* uplo, void* n, void* a, void* lda, void* b, void* ldb, void* vl, void* vu, void* il, void* iu, void* abstol, void* m, void* w, void* z, void* ldz, void* work, void* lwork, void* iwork, void* ifail, void* info);

	fn      = current_backend->lapack.dsygvx.f77_blas_function; 

    hook_pos_dsygvx ++;
    if( hook_pos_dsygvx < __flexiblas_hooks->dsygvx.nhook) {
        fn_hook = __flexiblas_hooks->dsygvx.f77_hook_function[hook_pos_dsygvx];
        fn_hook((void*) itype, (void*) jobz, (void*) range, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) abstol, (void*) m, (void*) w, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) iwork, (void*) ifail, (void*) info);
    } else {
        hook_pos_dsygvx = 0;
		fn((void*) itype, (void*) jobz, (void*) range, (void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) abstol, (void*) m, (void*) w, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) iwork, (void*) ifail, (void*) info); 
	}
	return;
}

void flexiblas_chain_dsygvx(void* itype, void* jobz, void* range, void* uplo, void* n, void* a, void* lda, void* b, void* ldb, void* vl, void* vu, void* il, void* iu, void* abstol, void* m, void* w, void* z, void* ldz, void* work, void* lwork, void* iwork, void* ifail, void* info)  __attribute__((alias("flexiblas_chain_dsygvx_")));




