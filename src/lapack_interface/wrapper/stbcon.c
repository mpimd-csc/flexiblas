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



static TLS_STORE uint8_t hook_pos_stbcon = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(stbcon,STBCON)(char* norm, char* uplo, char* diag, blasint* n, blasint* kd, float* ab, blasint* ldab, float* rcond, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag)
#else
void FC_GLOBAL(stbcon,STBCON)(char* norm, char* uplo, char* diag, blasint* n, blasint* kd, float* ab, blasint* ldab, float* rcond, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag)
#endif
{
	void (*fn) (void* norm, void* uplo, void* diag, void* n, void* kd, void* ab, void* ldab, void* rcond, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);
	void (*fn_hook) (void* norm, void* uplo, void* diag, void* n, void* kd, void* ab, void* ldab, void* rcond, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.stbcon.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->stbcon.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) norm, (void*) uplo, (void*) diag, (void*) n, (void*) kd, (void*) ab, (void*) ldab, (void*) rcond, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_norm, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_diag); 
		return;
	} else {
		hook_pos_stbcon = 0;
		fn_hook((void*) norm, (void*) uplo, (void*) diag, (void*) n, (void*) kd, (void*) ab, (void*) ldab, (void*) rcond, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_norm, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_diag);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void stbcon_(char* norm, char* uplo, char* diag, blasint* n, blasint* kd, float* ab, blasint* ldab, float* rcond, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag) __attribute__((alias(MTS(FC_GLOBAL(stbcon,STBCON)))));
#else
#ifndef __APPLE__
void stbcon(char* norm, char* uplo, char* diag, blasint* n, blasint* kd, float* ab, blasint* ldab, float* rcond, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag) __attribute__((alias(MTS(FC_GLOBAL(stbcon,STBCON)))));
#else
void stbcon(char* norm, char* uplo, char* diag, blasint* n, blasint* kd, float* ab, blasint* ldab, float* rcond, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag){ FC_GLOBAL(stbcon,STBCON)((void*) norm, (void*) uplo, (void*) diag, (void*) n, (void*) kd, (void*) ab, (void*) ldab, (void*) rcond, (void*) work, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_norm, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_diag); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_stbcon_(void* norm, void* uplo, void* diag, void* n, void* kd, void* ab, void* ldab, void* rcond, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag)
{
	void (*fn) (void* norm, void* uplo, void* diag, void* n, void* kd, void* ab, void* ldab, void* rcond, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

	*(void **) & fn = current_backend->lapack.stbcon.f77_blas_function; 

		fn((void*) norm, (void*) uplo, (void*) diag, (void*) n, (void*) kd, (void*) ab, (void*) ldab, (void*) rcond, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_norm, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_diag); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_stbcon(void* norm, void* uplo, void* diag, void* n, void* kd, void* ab, void* ldab, void* rcond, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag) __attribute__((alias("flexiblas_real_stbcon_")));
#else
void flexiblas_real_stbcon(void* norm, void* uplo, void* diag, void* n, void* kd, void* ab, void* ldab, void* rcond, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag){flexiblas_real_stbcon_((void*) norm, (void*) uplo, (void*) diag, (void*) n, (void*) kd, (void*) ab, (void*) ldab, (void*) rcond, (void*) work, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_norm, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_diag);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_stbcon_(void* norm, void* uplo, void* diag, void* n, void* kd, void* ab, void* ldab, void* rcond, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag)
{
	void (*fn) (void* norm, void* uplo, void* diag, void* n, void* kd, void* ab, void* ldab, void* rcond, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);
	void (*fn_hook) (void* norm, void* uplo, void* diag, void* n, void* kd, void* ab, void* ldab, void* rcond, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

	*(void **) &fn      = current_backend->lapack.stbcon.f77_blas_function; 

    hook_pos_stbcon ++;
    if( hook_pos_stbcon < __flexiblas_hooks->stbcon.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->stbcon.f77_hook_function[hook_pos_stbcon];
        fn_hook((void*) norm, (void*) uplo, (void*) diag, (void*) n, (void*) kd, (void*) ab, (void*) ldab, (void*) rcond, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_norm, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_diag);
    } else {
        hook_pos_stbcon = 0;
		fn((void*) norm, (void*) uplo, (void*) diag, (void*) n, (void*) kd, (void*) ab, (void*) ldab, (void*) rcond, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_norm, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_diag); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_stbcon(void* norm, void* uplo, void* diag, void* n, void* kd, void* ab, void* ldab, void* rcond, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag) __attribute__((alias("flexiblas_chain_stbcon_")));
#else
void flexiblas_chain_stbcon(void* norm, void* uplo, void* diag, void* n, void* kd, void* ab, void* ldab, void* rcond, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag){flexiblas_chain_stbcon_((void*) norm, (void*) uplo, (void*) diag, (void*) n, (void*) kd, (void*) ab, (void*) ldab, (void*) rcond, (void*) work, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_norm, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_diag);}
#endif



