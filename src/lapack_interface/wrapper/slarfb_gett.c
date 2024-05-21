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


static TLS_STORE uint8_t hook_pos_slarfb_gett = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL_(slarfb_gett,SLARFB_GETT)(char* ident, blasint* m, blasint* n, blasint* k, float* t, blasint* ldt, float* a, blasint* lda, float* b, blasint* ldb, float* work, blasint* ldwork, flexiblas_fortran_charlen_t len_ident)
#else
void FC_GLOBAL_(slarfb_gett,SLARFB_GETT)(char* ident, blasint* m, blasint* n, blasint* k, float* t, blasint* ldt, float* a, blasint* lda, float* b, blasint* ldb, float* work, blasint* ldwork, flexiblas_fortran_charlen_t len_ident)
#endif
{
	void (*fn) (void* ident, void* m, void* n, void* k, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork, flexiblas_fortran_charlen_t len_ident);
	void (*fn_hook) (void* ident, void* m, void* n, void* k, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork, flexiblas_fortran_charlen_t len_ident);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.slarfb_gett.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->slarfb_gett.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) ident, (void*) m, (void*) n, (void*) k, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) ldwork, ( flexiblas_fortran_charlen_t ) len_ident); 
		return;
	} else {
		hook_pos_slarfb_gett = 0;
		fn_hook((void*) ident, (void*) m, (void*) n, (void*) k, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) ldwork, ( flexiblas_fortran_charlen_t ) len_ident);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void slarfb_gett_(char* ident, blasint* m, blasint* n, blasint* k, float* t, blasint* ldt, float* a, blasint* lda, float* b, blasint* ldb, float* work, blasint* ldwork, flexiblas_fortran_charlen_t len_ident) __attribute__((alias(MTS(FC_GLOBAL_(slarfb_gett,SLARFB_GETT)))));
#else
#ifndef __APPLE__
void slarfb_gett(char* ident, blasint* m, blasint* n, blasint* k, float* t, blasint* ldt, float* a, blasint* lda, float* b, blasint* ldb, float* work, blasint* ldwork, flexiblas_fortran_charlen_t len_ident) __attribute__((alias(MTS(FC_GLOBAL_(slarfb_gett,SLARFB_GETT)))));
#else
void slarfb_gett(char* ident, blasint* m, blasint* n, blasint* k, float* t, blasint* ldt, float* a, blasint* lda, float* b, blasint* ldb, float* work, blasint* ldwork, flexiblas_fortran_charlen_t len_ident){ FC_GLOBAL_(slarfb_gett,SLARFB_GETT)((void*) ident, (void*) m, (void*) n, (void*) k, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) ldwork, (flexiblas_fortran_charlen_t) len_ident); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slarfb_gett_(void* ident, void* m, void* n, void* k, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork, flexiblas_fortran_charlen_t len_ident)
{
	void (*fn) (void* ident, void* m, void* n, void* k, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork, flexiblas_fortran_charlen_t len_ident);

	*(void **) & fn = current_backend->lapack.slarfb_gett.f77_blas_function; 

		fn((void*) ident, (void*) m, (void*) n, (void*) k, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) ldwork, ( flexiblas_fortran_charlen_t ) len_ident); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_slarfb_gett(void* ident, void* m, void* n, void* k, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork, flexiblas_fortran_charlen_t len_ident) __attribute__((alias("flexiblas_real_slarfb_gett_")));
#else
void flexiblas_real_slarfb_gett(void* ident, void* m, void* n, void* k, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork, flexiblas_fortran_charlen_t len_ident){flexiblas_real_slarfb_gett_((void*) ident, (void*) m, (void*) n, (void*) k, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) ldwork, (flexiblas_fortran_charlen_t) len_ident);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_slarfb_gett_(void* ident, void* m, void* n, void* k, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork, flexiblas_fortran_charlen_t len_ident)
{
	void (*fn) (void* ident, void* m, void* n, void* k, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork, flexiblas_fortran_charlen_t len_ident);
	void (*fn_hook) (void* ident, void* m, void* n, void* k, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork, flexiblas_fortran_charlen_t len_ident);

	*(void **) &fn      = current_backend->lapack.slarfb_gett.f77_blas_function; 

    hook_pos_slarfb_gett ++;
    if( hook_pos_slarfb_gett < __flexiblas_hooks->slarfb_gett.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->slarfb_gett.f77_hook_function[hook_pos_slarfb_gett];
        fn_hook((void*) ident, (void*) m, (void*) n, (void*) k, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) ldwork, ( flexiblas_fortran_charlen_t ) len_ident);
    } else {
        hook_pos_slarfb_gett = 0;
		fn((void*) ident, (void*) m, (void*) n, (void*) k, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) ldwork, ( flexiblas_fortran_charlen_t ) len_ident); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_slarfb_gett(void* ident, void* m, void* n, void* k, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork, flexiblas_fortran_charlen_t len_ident) __attribute__((alias("flexiblas_chain_slarfb_gett_")));
#else
void flexiblas_chain_slarfb_gett(void* ident, void* m, void* n, void* k, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork, flexiblas_fortran_charlen_t len_ident){flexiblas_chain_slarfb_gett_((void*) ident, (void*) m, (void*) n, (void*) k, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) ldwork, (flexiblas_fortran_charlen_t) len_ident);}
#endif



