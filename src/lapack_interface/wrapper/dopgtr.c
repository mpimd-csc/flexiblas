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



static TLS_STORE uint8_t hook_pos_dopgtr = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dopgtr,DOPGTR)(char* uplo, blasint* n, double* ap, double* tau, double* q, blasint* ldq, double* work, blasint* info)
#else
void FC_GLOBAL(dopgtr,DOPGTR)(char* uplo, blasint* n, double* ap, double* tau, double* q, blasint* ldq, double* work, blasint* info)
#endif
{
	void (*fn) (void* uplo, void* n, void* ap, void* tau, void* q, void* ldq, void* work, void* info);
	void (*fn_hook) (void* uplo, void* n, void* ap, void* tau, void* q, void* ldq, void* work, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dopgtr.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dopgtr.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) uplo, (void*) n, (void*) ap, (void*) tau, (void*) q, (void*) ldq, (void*) work, (void*) info); 
		return;
	} else {
		hook_pos_dopgtr = 0;
		fn_hook((void*) uplo, (void*) n, (void*) ap, (void*) tau, (void*) q, (void*) ldq, (void*) work, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dopgtr_(char* uplo, blasint* n, double* ap, double* tau, double* q, blasint* ldq, double* work, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dopgtr,DOPGTR)))));
#else
#ifndef __APPLE__
void dopgtr(char* uplo, blasint* n, double* ap, double* tau, double* q, blasint* ldq, double* work, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dopgtr,DOPGTR)))));
#else
void dopgtr(char* uplo, blasint* n, double* ap, double* tau, double* q, blasint* ldq, double* work, blasint* info){ FC_GLOBAL(dopgtr,DOPGTR)((void*) uplo, (void*) n, (void*) ap, (void*) tau, (void*) q, (void*) ldq, (void*) work, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dopgtr_(void* uplo, void* n, void* ap, void* tau, void* q, void* ldq, void* work, void* info)
{
	void (*fn) (void* uplo, void* n, void* ap, void* tau, void* q, void* ldq, void* work, void* info);

	*(void **) & fn = current_backend->lapack.dopgtr.f77_blas_function; 

		fn((void*) uplo, (void*) n, (void*) ap, (void*) tau, (void*) q, (void*) ldq, (void*) work, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_dopgtr(void* uplo, void* n, void* ap, void* tau, void* q, void* ldq, void* work, void* info) __attribute__((alias("flexiblas_real_dopgtr_")));
#else
void flexiblas_real_dopgtr(void* uplo, void* n, void* ap, void* tau, void* q, void* ldq, void* work, void* info){flexiblas_real_dopgtr_((void*) uplo, (void*) n, (void*) ap, (void*) tau, (void*) q, (void*) ldq, (void*) work, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dopgtr_(void* uplo, void* n, void* ap, void* tau, void* q, void* ldq, void* work, void* info)
{
	void (*fn) (void* uplo, void* n, void* ap, void* tau, void* q, void* ldq, void* work, void* info);
	void (*fn_hook) (void* uplo, void* n, void* ap, void* tau, void* q, void* ldq, void* work, void* info);

	*(void **) &fn      = current_backend->lapack.dopgtr.f77_blas_function; 

    hook_pos_dopgtr ++;
    if( hook_pos_dopgtr < __flexiblas_hooks->dopgtr.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dopgtr.f77_hook_function[hook_pos_dopgtr];
        fn_hook((void*) uplo, (void*) n, (void*) ap, (void*) tau, (void*) q, (void*) ldq, (void*) work, (void*) info);
    } else {
        hook_pos_dopgtr = 0;
		fn((void*) uplo, (void*) n, (void*) ap, (void*) tau, (void*) q, (void*) ldq, (void*) work, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_dopgtr(void* uplo, void* n, void* ap, void* tau, void* q, void* ldq, void* work, void* info) __attribute__((alias("flexiblas_chain_dopgtr_")));
#else
void flexiblas_chain_dopgtr(void* uplo, void* n, void* ap, void* tau, void* q, void* ldq, void* work, void* info){flexiblas_chain_dopgtr_((void*) uplo, (void*) n, (void*) ap, (void*) tau, (void*) q, (void*) ldq, (void*) work, (void*) info);}
#endif



