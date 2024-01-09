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



static TLS_STORE uint8_t hook_pos_dsycon = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dsycon,DSYCON)(char* uplo, blasint* n, double* a, blasint* lda, blasint* ipiv, double* anorm, double* rcond, double* work, blasint* iwork, blasint* info)
#else
void FC_GLOBAL(dsycon,DSYCON)(char* uplo, blasint* n, double* a, blasint* lda, blasint* ipiv, double* anorm, double* rcond, double* work, blasint* iwork, blasint* info)
#endif
{
	void (*fn) (void* uplo, void* n, void* a, void* lda, void* ipiv, void* anorm, void* rcond, void* work, void* iwork, void* info);
	void (*fn_hook) (void* uplo, void* n, void* a, void* lda, void* ipiv, void* anorm, void* rcond, void* work, void* iwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dsycon.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dsycon.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) iwork, (void*) info); 
		return;
	} else {
		hook_pos_dsycon = 0;
		fn_hook((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) iwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dsycon_(char* uplo, blasint* n, double* a, blasint* lda, blasint* ipiv, double* anorm, double* rcond, double* work, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dsycon,DSYCON)))));
#else
#ifndef __APPLE__
void dsycon(char* uplo, blasint* n, double* a, blasint* lda, blasint* ipiv, double* anorm, double* rcond, double* work, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dsycon,DSYCON)))));
#else
void dsycon(char* uplo, blasint* n, double* a, blasint* lda, blasint* ipiv, double* anorm, double* rcond, double* work, blasint* iwork, blasint* info){ FC_GLOBAL(dsycon,DSYCON)((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) iwork, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dsycon_(void* uplo, void* n, void* a, void* lda, void* ipiv, void* anorm, void* rcond, void* work, void* iwork, void* info)
{
	void (*fn) (void* uplo, void* n, void* a, void* lda, void* ipiv, void* anorm, void* rcond, void* work, void* iwork, void* info);

	*(void **) & fn = current_backend->lapack.dsycon.f77_blas_function; 

		fn((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) iwork, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_dsycon(void* uplo, void* n, void* a, void* lda, void* ipiv, void* anorm, void* rcond, void* work, void* iwork, void* info) __attribute__((alias("flexiblas_real_dsycon_")));
#else
void flexiblas_real_dsycon(void* uplo, void* n, void* a, void* lda, void* ipiv, void* anorm, void* rcond, void* work, void* iwork, void* info){flexiblas_real_dsycon_((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) iwork, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dsycon_(void* uplo, void* n, void* a, void* lda, void* ipiv, void* anorm, void* rcond, void* work, void* iwork, void* info)
{
	void (*fn) (void* uplo, void* n, void* a, void* lda, void* ipiv, void* anorm, void* rcond, void* work, void* iwork, void* info);
	void (*fn_hook) (void* uplo, void* n, void* a, void* lda, void* ipiv, void* anorm, void* rcond, void* work, void* iwork, void* info);

	*(void **) &fn      = current_backend->lapack.dsycon.f77_blas_function; 

    hook_pos_dsycon ++;
    if( hook_pos_dsycon < __flexiblas_hooks->dsycon.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dsycon.f77_hook_function[hook_pos_dsycon];
        fn_hook((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) iwork, (void*) info);
    } else {
        hook_pos_dsycon = 0;
		fn((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) iwork, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_dsycon(void* uplo, void* n, void* a, void* lda, void* ipiv, void* anorm, void* rcond, void* work, void* iwork, void* info) __attribute__((alias("flexiblas_chain_dsycon_")));
#else
void flexiblas_chain_dsycon(void* uplo, void* n, void* a, void* lda, void* ipiv, void* anorm, void* rcond, void* work, void* iwork, void* info){flexiblas_chain_dsycon_((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) iwork, (void*) info);}
#endif



