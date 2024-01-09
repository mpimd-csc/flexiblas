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



static TLS_STORE uint8_t hook_pos_dpbsvx = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dpbsvx,DPBSVX)(char* fact, char* uplo, blasint* n, blasint* kd, blasint* nrhs, double* ab, blasint* ldab, double* afb, blasint* ldafb, char* equed, double* s, double* b, blasint* ldb, double* x, blasint* ldx, double* rcond, double* ferr, double* berr, double* work, blasint* iwork, blasint* info)
#else
void FC_GLOBAL(dpbsvx,DPBSVX)(char* fact, char* uplo, blasint* n, blasint* kd, blasint* nrhs, double* ab, blasint* ldab, double* afb, blasint* ldafb, char* equed, double* s, double* b, blasint* ldb, double* x, blasint* ldx, double* rcond, double* ferr, double* berr, double* work, blasint* iwork, blasint* info)
#endif
{
	void (*fn) (void* fact, void* uplo, void* n, void* kd, void* nrhs, void* ab, void* ldab, void* afb, void* ldafb, void* equed, void* s, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* iwork, void* info);
	void (*fn_hook) (void* fact, void* uplo, void* n, void* kd, void* nrhs, void* ab, void* ldab, void* afb, void* ldafb, void* equed, void* s, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* iwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dpbsvx.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dpbsvx.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) fact, (void*) uplo, (void*) n, (void*) kd, (void*) nrhs, (void*) ab, (void*) ldab, (void*) afb, (void*) ldafb, (void*) equed, (void*) s, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) iwork, (void*) info); 
		return;
	} else {
		hook_pos_dpbsvx = 0;
		fn_hook((void*) fact, (void*) uplo, (void*) n, (void*) kd, (void*) nrhs, (void*) ab, (void*) ldab, (void*) afb, (void*) ldafb, (void*) equed, (void*) s, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) iwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dpbsvx_(char* fact, char* uplo, blasint* n, blasint* kd, blasint* nrhs, double* ab, blasint* ldab, double* afb, blasint* ldafb, char* equed, double* s, double* b, blasint* ldb, double* x, blasint* ldx, double* rcond, double* ferr, double* berr, double* work, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dpbsvx,DPBSVX)))));
#else
#ifndef __APPLE__
void dpbsvx(char* fact, char* uplo, blasint* n, blasint* kd, blasint* nrhs, double* ab, blasint* ldab, double* afb, blasint* ldafb, char* equed, double* s, double* b, blasint* ldb, double* x, blasint* ldx, double* rcond, double* ferr, double* berr, double* work, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dpbsvx,DPBSVX)))));
#else
void dpbsvx(char* fact, char* uplo, blasint* n, blasint* kd, blasint* nrhs, double* ab, blasint* ldab, double* afb, blasint* ldafb, char* equed, double* s, double* b, blasint* ldb, double* x, blasint* ldx, double* rcond, double* ferr, double* berr, double* work, blasint* iwork, blasint* info){ FC_GLOBAL(dpbsvx,DPBSVX)((void*) fact, (void*) uplo, (void*) n, (void*) kd, (void*) nrhs, (void*) ab, (void*) ldab, (void*) afb, (void*) ldafb, (void*) equed, (void*) s, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) iwork, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dpbsvx_(void* fact, void* uplo, void* n, void* kd, void* nrhs, void* ab, void* ldab, void* afb, void* ldafb, void* equed, void* s, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* iwork, void* info)
{
	void (*fn) (void* fact, void* uplo, void* n, void* kd, void* nrhs, void* ab, void* ldab, void* afb, void* ldafb, void* equed, void* s, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* iwork, void* info);

	*(void **) & fn = current_backend->lapack.dpbsvx.f77_blas_function; 

		fn((void*) fact, (void*) uplo, (void*) n, (void*) kd, (void*) nrhs, (void*) ab, (void*) ldab, (void*) afb, (void*) ldafb, (void*) equed, (void*) s, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) iwork, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_dpbsvx(void* fact, void* uplo, void* n, void* kd, void* nrhs, void* ab, void* ldab, void* afb, void* ldafb, void* equed, void* s, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* iwork, void* info) __attribute__((alias("flexiblas_real_dpbsvx_")));
#else
void flexiblas_real_dpbsvx(void* fact, void* uplo, void* n, void* kd, void* nrhs, void* ab, void* ldab, void* afb, void* ldafb, void* equed, void* s, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* iwork, void* info){flexiblas_real_dpbsvx_((void*) fact, (void*) uplo, (void*) n, (void*) kd, (void*) nrhs, (void*) ab, (void*) ldab, (void*) afb, (void*) ldafb, (void*) equed, (void*) s, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) iwork, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dpbsvx_(void* fact, void* uplo, void* n, void* kd, void* nrhs, void* ab, void* ldab, void* afb, void* ldafb, void* equed, void* s, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* iwork, void* info)
{
	void (*fn) (void* fact, void* uplo, void* n, void* kd, void* nrhs, void* ab, void* ldab, void* afb, void* ldafb, void* equed, void* s, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* iwork, void* info);
	void (*fn_hook) (void* fact, void* uplo, void* n, void* kd, void* nrhs, void* ab, void* ldab, void* afb, void* ldafb, void* equed, void* s, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* iwork, void* info);

	*(void **) &fn      = current_backend->lapack.dpbsvx.f77_blas_function; 

    hook_pos_dpbsvx ++;
    if( hook_pos_dpbsvx < __flexiblas_hooks->dpbsvx.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dpbsvx.f77_hook_function[hook_pos_dpbsvx];
        fn_hook((void*) fact, (void*) uplo, (void*) n, (void*) kd, (void*) nrhs, (void*) ab, (void*) ldab, (void*) afb, (void*) ldafb, (void*) equed, (void*) s, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) iwork, (void*) info);
    } else {
        hook_pos_dpbsvx = 0;
		fn((void*) fact, (void*) uplo, (void*) n, (void*) kd, (void*) nrhs, (void*) ab, (void*) ldab, (void*) afb, (void*) ldafb, (void*) equed, (void*) s, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) iwork, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_dpbsvx(void* fact, void* uplo, void* n, void* kd, void* nrhs, void* ab, void* ldab, void* afb, void* ldafb, void* equed, void* s, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* iwork, void* info) __attribute__((alias("flexiblas_chain_dpbsvx_")));
#else
void flexiblas_chain_dpbsvx(void* fact, void* uplo, void* n, void* kd, void* nrhs, void* ab, void* ldab, void* afb, void* ldafb, void* equed, void* s, void* b, void* ldb, void* x, void* ldx, void* rcond, void* ferr, void* berr, void* work, void* iwork, void* info){flexiblas_chain_dpbsvx_((void*) fact, (void*) uplo, (void*) n, (void*) kd, (void*) nrhs, (void*) ab, (void*) ldab, (void*) afb, (void*) ldafb, (void*) equed, (void*) s, (void*) b, (void*) ldb, (void*) x, (void*) ldx, (void*) rcond, (void*) ferr, (void*) berr, (void*) work, (void*) iwork, (void*) info);}
#endif



