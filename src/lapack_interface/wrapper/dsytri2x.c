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



static TLS_STORE uint8_t hook_pos_dsytri2x = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dsytri2x,DSYTRI2X)(char* uplo, blasint* n, double* a, blasint* lda, blasint* ipiv, double* work, blasint* nb, blasint* info)
#else
void FC_GLOBAL(dsytri2x,DSYTRI2X)(char* uplo, blasint* n, double* a, blasint* lda, blasint* ipiv, double* work, blasint* nb, blasint* info)
#endif
{
	void (*fn) (void* uplo, void* n, void* a, void* lda, void* ipiv, void* work, void* nb, void* info);
	void (*fn_hook) (void* uplo, void* n, void* a, void* lda, void* ipiv, void* work, void* nb, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dsytri2x.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dsytri2x.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) ipiv, (void*) work, (void*) nb, (void*) info); 
		return;
	} else {
		hook_pos_dsytri2x = 0;
		fn_hook((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) ipiv, (void*) work, (void*) nb, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dsytri2x_(char* uplo, blasint* n, double* a, blasint* lda, blasint* ipiv, double* work, blasint* nb, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dsytri2x,DSYTRI2X)))));
#else
#ifndef __APPLE__
void dsytri2x(char* uplo, blasint* n, double* a, blasint* lda, blasint* ipiv, double* work, blasint* nb, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dsytri2x,DSYTRI2X)))));
#else
void dsytri2x(char* uplo, blasint* n, double* a, blasint* lda, blasint* ipiv, double* work, blasint* nb, blasint* info){ FC_GLOBAL(dsytri2x,DSYTRI2X)((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) ipiv, (void*) work, (void*) nb, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dsytri2x_(void* uplo, void* n, void* a, void* lda, void* ipiv, void* work, void* nb, void* info)
{
	void (*fn) (void* uplo, void* n, void* a, void* lda, void* ipiv, void* work, void* nb, void* info);

	*(void **) & fn = current_backend->lapack.dsytri2x.f77_blas_function; 

		fn((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) ipiv, (void*) work, (void*) nb, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_dsytri2x(void* uplo, void* n, void* a, void* lda, void* ipiv, void* work, void* nb, void* info) __attribute__((alias("flexiblas_real_dsytri2x_")));
#else
void flexiblas_real_dsytri2x(void* uplo, void* n, void* a, void* lda, void* ipiv, void* work, void* nb, void* info){flexiblas_real_dsytri2x_((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) ipiv, (void*) work, (void*) nb, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dsytri2x_(void* uplo, void* n, void* a, void* lda, void* ipiv, void* work, void* nb, void* info)
{
	void (*fn) (void* uplo, void* n, void* a, void* lda, void* ipiv, void* work, void* nb, void* info);
	void (*fn_hook) (void* uplo, void* n, void* a, void* lda, void* ipiv, void* work, void* nb, void* info);

	*(void **) &fn      = current_backend->lapack.dsytri2x.f77_blas_function; 

    hook_pos_dsytri2x ++;
    if( hook_pos_dsytri2x < __flexiblas_hooks->dsytri2x.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dsytri2x.f77_hook_function[hook_pos_dsytri2x];
        fn_hook((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) ipiv, (void*) work, (void*) nb, (void*) info);
    } else {
        hook_pos_dsytri2x = 0;
		fn((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) ipiv, (void*) work, (void*) nb, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_dsytri2x(void* uplo, void* n, void* a, void* lda, void* ipiv, void* work, void* nb, void* info) __attribute__((alias("flexiblas_chain_dsytri2x_")));
#else
void flexiblas_chain_dsytri2x(void* uplo, void* n, void* a, void* lda, void* ipiv, void* work, void* nb, void* info){flexiblas_chain_dsytri2x_((void*) uplo, (void*) n, (void*) a, (void*) lda, (void*) ipiv, (void*) work, (void*) nb, (void*) info);}
#endif



