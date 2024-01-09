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



static TLS_STORE uint8_t hook_pos_dspcon = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dspcon,DSPCON)(char* uplo, blasint* n, double* ap, blasint* ipiv, double* anorm, double* rcond, double* work, blasint* iwork, blasint* info)
#else
void FC_GLOBAL(dspcon,DSPCON)(char* uplo, blasint* n, double* ap, blasint* ipiv, double* anorm, double* rcond, double* work, blasint* iwork, blasint* info)
#endif
{
	void (*fn) (void* uplo, void* n, void* ap, void* ipiv, void* anorm, void* rcond, void* work, void* iwork, void* info);
	void (*fn_hook) (void* uplo, void* n, void* ap, void* ipiv, void* anorm, void* rcond, void* work, void* iwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dspcon.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dspcon.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) uplo, (void*) n, (void*) ap, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) iwork, (void*) info); 
		return;
	} else {
		hook_pos_dspcon = 0;
		fn_hook((void*) uplo, (void*) n, (void*) ap, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) iwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dspcon_(char* uplo, blasint* n, double* ap, blasint* ipiv, double* anorm, double* rcond, double* work, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dspcon,DSPCON)))));
#else
#ifndef __APPLE__
void dspcon(char* uplo, blasint* n, double* ap, blasint* ipiv, double* anorm, double* rcond, double* work, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dspcon,DSPCON)))));
#else
void dspcon(char* uplo, blasint* n, double* ap, blasint* ipiv, double* anorm, double* rcond, double* work, blasint* iwork, blasint* info){ FC_GLOBAL(dspcon,DSPCON)((void*) uplo, (void*) n, (void*) ap, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) iwork, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dspcon_(void* uplo, void* n, void* ap, void* ipiv, void* anorm, void* rcond, void* work, void* iwork, void* info)
{
	void (*fn) (void* uplo, void* n, void* ap, void* ipiv, void* anorm, void* rcond, void* work, void* iwork, void* info);

	*(void **) & fn = current_backend->lapack.dspcon.f77_blas_function; 

		fn((void*) uplo, (void*) n, (void*) ap, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) iwork, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_dspcon(void* uplo, void* n, void* ap, void* ipiv, void* anorm, void* rcond, void* work, void* iwork, void* info) __attribute__((alias("flexiblas_real_dspcon_")));
#else
void flexiblas_real_dspcon(void* uplo, void* n, void* ap, void* ipiv, void* anorm, void* rcond, void* work, void* iwork, void* info){flexiblas_real_dspcon_((void*) uplo, (void*) n, (void*) ap, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) iwork, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dspcon_(void* uplo, void* n, void* ap, void* ipiv, void* anorm, void* rcond, void* work, void* iwork, void* info)
{
	void (*fn) (void* uplo, void* n, void* ap, void* ipiv, void* anorm, void* rcond, void* work, void* iwork, void* info);
	void (*fn_hook) (void* uplo, void* n, void* ap, void* ipiv, void* anorm, void* rcond, void* work, void* iwork, void* info);

	*(void **) &fn      = current_backend->lapack.dspcon.f77_blas_function; 

    hook_pos_dspcon ++;
    if( hook_pos_dspcon < __flexiblas_hooks->dspcon.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dspcon.f77_hook_function[hook_pos_dspcon];
        fn_hook((void*) uplo, (void*) n, (void*) ap, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) iwork, (void*) info);
    } else {
        hook_pos_dspcon = 0;
		fn((void*) uplo, (void*) n, (void*) ap, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) iwork, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_dspcon(void* uplo, void* n, void* ap, void* ipiv, void* anorm, void* rcond, void* work, void* iwork, void* info) __attribute__((alias("flexiblas_chain_dspcon_")));
#else
void flexiblas_chain_dspcon(void* uplo, void* n, void* ap, void* ipiv, void* anorm, void* rcond, void* work, void* iwork, void* info){flexiblas_chain_dspcon_((void*) uplo, (void*) n, (void*) ap, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) iwork, (void*) info);}
#endif



