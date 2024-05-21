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


static TLS_STORE uint8_t hook_pos_clantr = 0;
#ifdef FLEXIBLAS_ABI_INTEL
float FC_GLOBAL(clantr,CLANTR)(char* norm, char* uplo, char* diag, blasint* m, blasint* n, float complex* a, blasint* lda, float* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag)
#else
float FC_GLOBAL(clantr,CLANTR)(char* norm, char* uplo, char* diag, blasint* m, blasint* n, float complex* a, blasint* lda, float* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag)
#endif
{
	float (*fn) (void* norm, void* uplo, void* diag, void* m, void* n, void* a, void* lda, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);
	float (*fn_hook) (void* norm, void* uplo, void* diag, void* m, void* n, void* a, void* lda, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);
	float ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.clantr.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->clantr.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		ret = fn((void*) norm, (void*) uplo, (void*) diag, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) work, ( flexiblas_fortran_charlen_t ) len_norm, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_diag); 
		return ret; 
	} else {
		hook_pos_clantr = 0;
		ret=fn_hook((void*) norm, (void*) uplo, (void*) diag, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) work, ( flexiblas_fortran_charlen_t ) len_norm, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_diag);
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
float clantr_(char* norm, char* uplo, char* diag, blasint* m, blasint* n, float complex* a, blasint* lda, float* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag) __attribute__((alias(MTS(FC_GLOBAL(clantr,CLANTR)))));
#else
#ifndef __APPLE__
float clantr(char* norm, char* uplo, char* diag, blasint* m, blasint* n, float complex* a, blasint* lda, float* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag) __attribute__((alias(MTS(FC_GLOBAL(clantr,CLANTR)))));
#else
float clantr(char* norm, char* uplo, char* diag, blasint* m, blasint* n, float complex* a, blasint* lda, float* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag){ return FC_GLOBAL(clantr,CLANTR)((void*) norm, (void*) uplo, (void*) diag, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) work, (flexiblas_fortran_charlen_t) len_norm, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_diag); }
#endif
#endif




/* Real Implementation for Hooks */


float flexiblas_real_clantr_(void* norm, void* uplo, void* diag, void* m, void* n, void* a, void* lda, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag)
{
	float (*fn) (void* norm, void* uplo, void* diag, void* m, void* n, void* a, void* lda, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);
	float ret;

	*(void **) & fn = current_backend->lapack.clantr.f77_blas_function; 

		ret = fn((void*) norm, (void*) uplo, (void*) diag, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) work, ( flexiblas_fortran_charlen_t ) len_norm, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_diag); 

	return ret ;
}
#ifndef __APPLE__
float flexiblas_real_clantr(void* norm, void* uplo, void* diag, void* m, void* n, void* a, void* lda, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag) __attribute__((alias("flexiblas_real_clantr_")));
#else
float flexiblas_real_clantr(void* norm, void* uplo, void* diag, void* m, void* n, void* a, void* lda, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag){return flexiblas_real_clantr_((void*) norm, (void*) uplo, (void*) diag, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) work, (flexiblas_fortran_charlen_t) len_norm, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_diag);}
#endif




/* Chainloader for Hooks */


float flexiblas_chain_clantr_(void* norm, void* uplo, void* diag, void* m, void* n, void* a, void* lda, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag)
{
	float (*fn) (void* norm, void* uplo, void* diag, void* m, void* n, void* a, void* lda, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);
	float (*fn_hook) (void* norm, void* uplo, void* diag, void* m, void* n, void* a, void* lda, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);
	float ret;

	*(void **) &fn      = current_backend->lapack.clantr.f77_blas_function; 

    hook_pos_clantr ++;
    if( hook_pos_clantr < __flexiblas_hooks->clantr.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->clantr.f77_hook_function[hook_pos_clantr];
        ret = fn_hook((void*) norm, (void*) uplo, (void*) diag, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) work, ( flexiblas_fortran_charlen_t )len_norm, ( flexiblas_fortran_charlen_t )len_uplo, ( flexiblas_fortran_charlen_t )len_diag);
    } else {
        hook_pos_clantr = 0;
		ret = fn((void*) norm, (void*) uplo, (void*) diag, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) work, ( flexiblas_fortran_charlen_t ) len_norm, ( flexiblas_fortran_charlen_t ) len_uplo, ( flexiblas_fortran_charlen_t ) len_diag); 
	}
	return ret ;
}
#ifndef __APPLE__
float flexiblas_chain_clantr(void* norm, void* uplo, void* diag, void* m, void* n, void* a, void* lda, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag) __attribute__((alias("flexiblas_chain_clantr_")));
#else
float flexiblas_chain_clantr(void* norm, void* uplo, void* diag, void* m, void* n, void* a, void* lda, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag){return flexiblas_chain_clantr_((void*) norm, (void*) uplo, (void*) diag, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) work, (flexiblas_fortran_charlen_t) len_norm, (flexiblas_fortran_charlen_t) len_uplo, (flexiblas_fortran_charlen_t) len_diag);}
#endif



