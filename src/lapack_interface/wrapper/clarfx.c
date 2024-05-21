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


static TLS_STORE uint8_t hook_pos_clarfx = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(clarfx,CLARFX)(char* side, blasint* m, blasint* n, float complex* v, float complex* tau, float complex* c, blasint* ldc, float complex* work, flexiblas_fortran_charlen_t len_side)
#else
void FC_GLOBAL(clarfx,CLARFX)(char* side, blasint* m, blasint* n, float complex* v, float complex* tau, float complex* c, blasint* ldc, float complex* work, flexiblas_fortran_charlen_t len_side)
#endif
{
	void (*fn) (void* side, void* m, void* n, void* v, void* tau, void* c, void* ldc, void* work, flexiblas_fortran_charlen_t len_side);
	void (*fn_hook) (void* side, void* m, void* n, void* v, void* tau, void* c, void* ldc, void* work, flexiblas_fortran_charlen_t len_side);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.clarfx.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->clarfx.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) side, (void*) m, (void*) n, (void*) v, (void*) tau, (void*) c, (void*) ldc, (void*) work, ( flexiblas_fortran_charlen_t ) len_side); 
		return;
	} else {
		hook_pos_clarfx = 0;
		fn_hook((void*) side, (void*) m, (void*) n, (void*) v, (void*) tau, (void*) c, (void*) ldc, (void*) work, ( flexiblas_fortran_charlen_t ) len_side);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void clarfx_(char* side, blasint* m, blasint* n, float complex* v, float complex* tau, float complex* c, blasint* ldc, float complex* work, flexiblas_fortran_charlen_t len_side) __attribute__((alias(MTS(FC_GLOBAL(clarfx,CLARFX)))));
#else
#ifndef __APPLE__
void clarfx(char* side, blasint* m, blasint* n, float complex* v, float complex* tau, float complex* c, blasint* ldc, float complex* work, flexiblas_fortran_charlen_t len_side) __attribute__((alias(MTS(FC_GLOBAL(clarfx,CLARFX)))));
#else
void clarfx(char* side, blasint* m, blasint* n, float complex* v, float complex* tau, float complex* c, blasint* ldc, float complex* work, flexiblas_fortran_charlen_t len_side){ FC_GLOBAL(clarfx,CLARFX)((void*) side, (void*) m, (void*) n, (void*) v, (void*) tau, (void*) c, (void*) ldc, (void*) work, (flexiblas_fortran_charlen_t) len_side); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_clarfx_(void* side, void* m, void* n, void* v, void* tau, void* c, void* ldc, void* work, flexiblas_fortran_charlen_t len_side)
{
	void (*fn) (void* side, void* m, void* n, void* v, void* tau, void* c, void* ldc, void* work, flexiblas_fortran_charlen_t len_side);

	*(void **) & fn = current_backend->lapack.clarfx.f77_blas_function; 

		fn((void*) side, (void*) m, (void*) n, (void*) v, (void*) tau, (void*) c, (void*) ldc, (void*) work, ( flexiblas_fortran_charlen_t ) len_side); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_clarfx(void* side, void* m, void* n, void* v, void* tau, void* c, void* ldc, void* work, flexiblas_fortran_charlen_t len_side) __attribute__((alias("flexiblas_real_clarfx_")));
#else
void flexiblas_real_clarfx(void* side, void* m, void* n, void* v, void* tau, void* c, void* ldc, void* work, flexiblas_fortran_charlen_t len_side){flexiblas_real_clarfx_((void*) side, (void*) m, (void*) n, (void*) v, (void*) tau, (void*) c, (void*) ldc, (void*) work, (flexiblas_fortran_charlen_t) len_side);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_clarfx_(void* side, void* m, void* n, void* v, void* tau, void* c, void* ldc, void* work, flexiblas_fortran_charlen_t len_side)
{
	void (*fn) (void* side, void* m, void* n, void* v, void* tau, void* c, void* ldc, void* work, flexiblas_fortran_charlen_t len_side);
	void (*fn_hook) (void* side, void* m, void* n, void* v, void* tau, void* c, void* ldc, void* work, flexiblas_fortran_charlen_t len_side);

	*(void **) &fn      = current_backend->lapack.clarfx.f77_blas_function; 

    hook_pos_clarfx ++;
    if( hook_pos_clarfx < __flexiblas_hooks->clarfx.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->clarfx.f77_hook_function[hook_pos_clarfx];
        fn_hook((void*) side, (void*) m, (void*) n, (void*) v, (void*) tau, (void*) c, (void*) ldc, (void*) work, ( flexiblas_fortran_charlen_t ) len_side);
    } else {
        hook_pos_clarfx = 0;
		fn((void*) side, (void*) m, (void*) n, (void*) v, (void*) tau, (void*) c, (void*) ldc, (void*) work, ( flexiblas_fortran_charlen_t ) len_side); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_clarfx(void* side, void* m, void* n, void* v, void* tau, void* c, void* ldc, void* work, flexiblas_fortran_charlen_t len_side) __attribute__((alias("flexiblas_chain_clarfx_")));
#else
void flexiblas_chain_clarfx(void* side, void* m, void* n, void* v, void* tau, void* c, void* ldc, void* work, flexiblas_fortran_charlen_t len_side){flexiblas_chain_clarfx_((void*) side, (void*) m, (void*) n, (void*) v, (void*) tau, (void*) c, (void*) ldc, (void*) work, (flexiblas_fortran_charlen_t) len_side);}
#endif



