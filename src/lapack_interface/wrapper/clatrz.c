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


static TLS_STORE uint8_t hook_pos_clatrz = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(clatrz,CLATRZ)(blasint* m, blasint* n, blasint* l, float complex* a, blasint* lda, float complex* tau, float complex* work)
#else
void FC_GLOBAL(clatrz,CLATRZ)(blasint* m, blasint* n, blasint* l, float complex* a, blasint* lda, float complex* tau, float complex* work)
#endif
{
	void (*fn) (void* m, void* n, void* l, void* a, void* lda, void* tau, void* work);
	void (*fn_hook) (void* m, void* n, void* l, void* a, void* lda, void* tau, void* work);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.clatrz.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->clatrz.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) m, (void*) n, (void*) l, (void*) a, (void*) lda, (void*) tau, (void*) work); 
		return;
	} else {
		hook_pos_clatrz = 0;
		fn_hook((void*) m, (void*) n, (void*) l, (void*) a, (void*) lda, (void*) tau, (void*) work);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void clatrz_(blasint* m, blasint* n, blasint* l, float complex* a, blasint* lda, float complex* tau, float complex* work) __attribute__((alias(MTS(FC_GLOBAL(clatrz,CLATRZ)))));
#else
#ifndef __APPLE__
void clatrz(blasint* m, blasint* n, blasint* l, float complex* a, blasint* lda, float complex* tau, float complex* work) __attribute__((alias(MTS(FC_GLOBAL(clatrz,CLATRZ)))));
#else
void clatrz(blasint* m, blasint* n, blasint* l, float complex* a, blasint* lda, float complex* tau, float complex* work){ FC_GLOBAL(clatrz,CLATRZ)((void*) m, (void*) n, (void*) l, (void*) a, (void*) lda, (void*) tau, (void*) work); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_clatrz_(void* m, void* n, void* l, void* a, void* lda, void* tau, void* work)
{
	void (*fn) (void* m, void* n, void* l, void* a, void* lda, void* tau, void* work);

	*(void **) & fn = current_backend->lapack.clatrz.f77_blas_function; 

		fn((void*) m, (void*) n, (void*) l, (void*) a, (void*) lda, (void*) tau, (void*) work); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_clatrz(void* m, void* n, void* l, void* a, void* lda, void* tau, void* work) __attribute__((alias("flexiblas_real_clatrz_")));
#else
void flexiblas_real_clatrz(void* m, void* n, void* l, void* a, void* lda, void* tau, void* work){flexiblas_real_clatrz_((void*) m, (void*) n, (void*) l, (void*) a, (void*) lda, (void*) tau, (void*) work);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_clatrz_(void* m, void* n, void* l, void* a, void* lda, void* tau, void* work)
{
	void (*fn) (void* m, void* n, void* l, void* a, void* lda, void* tau, void* work);
	void (*fn_hook) (void* m, void* n, void* l, void* a, void* lda, void* tau, void* work);

	*(void **) &fn      = current_backend->lapack.clatrz.f77_blas_function; 

    hook_pos_clatrz ++;
    if( hook_pos_clatrz < __flexiblas_hooks->clatrz.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->clatrz.f77_hook_function[hook_pos_clatrz];
        fn_hook((void*) m, (void*) n, (void*) l, (void*) a, (void*) lda, (void*) tau, (void*) work);
    } else {
        hook_pos_clatrz = 0;
		fn((void*) m, (void*) n, (void*) l, (void*) a, (void*) lda, (void*) tau, (void*) work); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_clatrz(void* m, void* n, void* l, void* a, void* lda, void* tau, void* work) __attribute__((alias("flexiblas_chain_clatrz_")));
#else
void flexiblas_chain_clatrz(void* m, void* n, void* l, void* a, void* lda, void* tau, void* work){flexiblas_chain_clatrz_((void*) m, (void*) n, (void*) l, (void*) a, (void*) lda, (void*) tau, (void*) work);}
#endif



