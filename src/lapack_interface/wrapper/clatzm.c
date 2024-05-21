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


static TLS_STORE uint8_t hook_pos_clatzm = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(clatzm,CLATZM)(char* side, blasint* m, blasint* n, float complex* v, blasint* incv, float complex* tau, float complex* c1, float complex* c2, blasint* ldc, float complex* work, flexiblas_fortran_charlen_t len_side)
#else
void FC_GLOBAL(clatzm,CLATZM)(char* side, blasint* m, blasint* n, float complex* v, blasint* incv, float complex* tau, float complex* c1, float complex* c2, blasint* ldc, float complex* work, flexiblas_fortran_charlen_t len_side)
#endif
{
	void (*fn) (void* side, void* m, void* n, void* v, void* incv, void* tau, void* c1, void* c2, void* ldc, void* work, flexiblas_fortran_charlen_t len_side);
	void (*fn_hook) (void* side, void* m, void* n, void* v, void* incv, void* tau, void* c1, void* c2, void* ldc, void* work, flexiblas_fortran_charlen_t len_side);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.clatzm.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->clatzm.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) side, (void*) m, (void*) n, (void*) v, (void*) incv, (void*) tau, (void*) c1, (void*) c2, (void*) ldc, (void*) work, ( flexiblas_fortran_charlen_t ) len_side); 
		return;
	} else {
		hook_pos_clatzm = 0;
		fn_hook((void*) side, (void*) m, (void*) n, (void*) v, (void*) incv, (void*) tau, (void*) c1, (void*) c2, (void*) ldc, (void*) work, ( flexiblas_fortran_charlen_t ) len_side);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void clatzm_(char* side, blasint* m, blasint* n, float complex* v, blasint* incv, float complex* tau, float complex* c1, float complex* c2, blasint* ldc, float complex* work, flexiblas_fortran_charlen_t len_side) __attribute__((alias(MTS(FC_GLOBAL(clatzm,CLATZM)))));
#else
#ifndef __APPLE__
void clatzm(char* side, blasint* m, blasint* n, float complex* v, blasint* incv, float complex* tau, float complex* c1, float complex* c2, blasint* ldc, float complex* work, flexiblas_fortran_charlen_t len_side) __attribute__((alias(MTS(FC_GLOBAL(clatzm,CLATZM)))));
#else
void clatzm(char* side, blasint* m, blasint* n, float complex* v, blasint* incv, float complex* tau, float complex* c1, float complex* c2, blasint* ldc, float complex* work, flexiblas_fortran_charlen_t len_side){ FC_GLOBAL(clatzm,CLATZM)((void*) side, (void*) m, (void*) n, (void*) v, (void*) incv, (void*) tau, (void*) c1, (void*) c2, (void*) ldc, (void*) work, (flexiblas_fortran_charlen_t) len_side); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_clatzm_(void* side, void* m, void* n, void* v, void* incv, void* tau, void* c1, void* c2, void* ldc, void* work, flexiblas_fortran_charlen_t len_side)
{
	void (*fn) (void* side, void* m, void* n, void* v, void* incv, void* tau, void* c1, void* c2, void* ldc, void* work, flexiblas_fortran_charlen_t len_side);

	*(void **) & fn = current_backend->lapack.clatzm.f77_blas_function; 

		fn((void*) side, (void*) m, (void*) n, (void*) v, (void*) incv, (void*) tau, (void*) c1, (void*) c2, (void*) ldc, (void*) work, ( flexiblas_fortran_charlen_t ) len_side); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_clatzm(void* side, void* m, void* n, void* v, void* incv, void* tau, void* c1, void* c2, void* ldc, void* work, flexiblas_fortran_charlen_t len_side) __attribute__((alias("flexiblas_real_clatzm_")));
#else
void flexiblas_real_clatzm(void* side, void* m, void* n, void* v, void* incv, void* tau, void* c1, void* c2, void* ldc, void* work, flexiblas_fortran_charlen_t len_side){flexiblas_real_clatzm_((void*) side, (void*) m, (void*) n, (void*) v, (void*) incv, (void*) tau, (void*) c1, (void*) c2, (void*) ldc, (void*) work, (flexiblas_fortran_charlen_t) len_side);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_clatzm_(void* side, void* m, void* n, void* v, void* incv, void* tau, void* c1, void* c2, void* ldc, void* work, flexiblas_fortran_charlen_t len_side)
{
	void (*fn) (void* side, void* m, void* n, void* v, void* incv, void* tau, void* c1, void* c2, void* ldc, void* work, flexiblas_fortran_charlen_t len_side);
	void (*fn_hook) (void* side, void* m, void* n, void* v, void* incv, void* tau, void* c1, void* c2, void* ldc, void* work, flexiblas_fortran_charlen_t len_side);

	*(void **) &fn      = current_backend->lapack.clatzm.f77_blas_function; 

    hook_pos_clatzm ++;
    if( hook_pos_clatzm < __flexiblas_hooks->clatzm.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->clatzm.f77_hook_function[hook_pos_clatzm];
        fn_hook((void*) side, (void*) m, (void*) n, (void*) v, (void*) incv, (void*) tau, (void*) c1, (void*) c2, (void*) ldc, (void*) work, ( flexiblas_fortran_charlen_t ) len_side);
    } else {
        hook_pos_clatzm = 0;
		fn((void*) side, (void*) m, (void*) n, (void*) v, (void*) incv, (void*) tau, (void*) c1, (void*) c2, (void*) ldc, (void*) work, ( flexiblas_fortran_charlen_t ) len_side); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_clatzm(void* side, void* m, void* n, void* v, void* incv, void* tau, void* c1, void* c2, void* ldc, void* work, flexiblas_fortran_charlen_t len_side) __attribute__((alias("flexiblas_chain_clatzm_")));
#else
void flexiblas_chain_clatzm(void* side, void* m, void* n, void* v, void* incv, void* tau, void* c1, void* c2, void* ldc, void* work, flexiblas_fortran_charlen_t len_side){flexiblas_chain_clatzm_((void*) side, (void*) m, (void*) n, (void*) v, (void*) incv, (void*) tau, (void*) c1, (void*) c2, (void*) ldc, (void*) work, (flexiblas_fortran_charlen_t) len_side);}
#endif



