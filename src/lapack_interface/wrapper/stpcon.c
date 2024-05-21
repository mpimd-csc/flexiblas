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

#include "flexiblas_fortran_char_len.h"


static TLS_STORE uint8_t hook_pos_stpcon = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(stpcon,STPCON)(char* norm, char* uplo, char* diag, blasint* n, float* ap, float* rcond, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag)
#else
void FC_GLOBAL(stpcon,STPCON)(char* norm, char* uplo, char* diag, blasint* n, float* ap, float* rcond, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag)
#endif
{
	void (*fn) (void* norm, void* uplo, void* diag, void* n, void* ap, void* rcond, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);
	void (*fn_hook) (void* norm, void* uplo, void* diag, void* n, void* ap, void* rcond, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.stpcon.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->stpcon.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) norm, (void*) uplo, (void*) diag, (void*) n, (void*) ap, (void*) rcond, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_norm, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_diag); 
		return;
	} else {
		hook_pos_stpcon = 0;
		fn_hook((void*) norm, (void*) uplo, (void*) diag, (void*) n, (void*) ap, (void*) rcond, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_norm, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_diag);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void stpcon_(char* norm, char* uplo, char* diag, blasint* n, float* ap, float* rcond, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag) __attribute__((alias(MTS(FC_GLOBAL(stpcon,STPCON)))));
#else
#ifndef __APPLE__
void stpcon(char* norm, char* uplo, char* diag, blasint* n, float* ap, float* rcond, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag) __attribute__((alias(MTS(FC_GLOBAL(stpcon,STPCON)))));
#else
void stpcon(char* norm, char* uplo, char* diag, blasint* n, float* ap, float* rcond, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag){ FC_GLOBAL(stpcon,STPCON)((void*) norm, (void*) uplo, (void*) diag, (void*) n, (void*) ap, (void*) rcond, (void*) work, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_norm, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_diag); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_stpcon_(void* norm, void* uplo, void* diag, void* n, void* ap, void* rcond, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag)
{
	void (*fn) (void* norm, void* uplo, void* diag, void* n, void* ap, void* rcond, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

	*(void **) & fn = current_backend->lapack.stpcon.f77_blas_function; 

		fn((void*) norm, (void*) uplo, (void*) diag, (void*) n, (void*) ap, (void*) rcond, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_norm, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_diag); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_stpcon(void* norm, void* uplo, void* diag, void* n, void* ap, void* rcond, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag) __attribute__((alias("flexiblas_real_stpcon_")));
#else
void flexiblas_real_stpcon(void* norm, void* uplo, void* diag, void* n, void* ap, void* rcond, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag){flexiblas_real_stpcon_((void*) norm, (void*) uplo, (void*) diag, (void*) n, (void*) ap, (void*) rcond, (void*) work, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_norm, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_diag);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_stpcon_(void* norm, void* uplo, void* diag, void* n, void* ap, void* rcond, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag)
{
	void (*fn) (void* norm, void* uplo, void* diag, void* n, void* ap, void* rcond, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);
	void (*fn_hook) (void* norm, void* uplo, void* diag, void* n, void* ap, void* rcond, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

	*(void **) &fn      = current_backend->lapack.stpcon.f77_blas_function; 

    hook_pos_stpcon ++;
    if( hook_pos_stpcon < __flexiblas_hooks->stpcon.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->stpcon.f77_hook_function[hook_pos_stpcon];
        fn_hook((void*) norm, (void*) uplo, (void*) diag, (void*) n, (void*) ap, (void*) rcond, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_norm, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_diag);
    } else {
        hook_pos_stpcon = 0;
		fn((void*) norm, (void*) uplo, (void*) diag, (void*) n, (void*) ap, (void*) rcond, (void*) work, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_norm, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_diag); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_stpcon(void* norm, void* uplo, void* diag, void* n, void* ap, void* rcond, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag) __attribute__((alias("flexiblas_chain_stpcon_")));
#else
void flexiblas_chain_stpcon(void* norm, void* uplo, void* diag, void* n, void* ap, void* rcond, void* work, void* iwork, void* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag){flexiblas_chain_stpcon_((void*) norm, (void*) uplo, (void*) diag, (void*) n, (void*) ap, (void*) rcond, (void*) work, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_norm, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_diag);}
#endif



