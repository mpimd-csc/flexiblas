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


static TLS_STORE uint8_t hook_pos_dorg2r = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dorg2r,DORG2R)(blasint* m, blasint* n, blasint* k, double* a, blasint* lda, double* tau, double* work, blasint* info)
#else
void FC_GLOBAL(dorg2r,DORG2R)(blasint* m, blasint* n, blasint* k, double* a, blasint* lda, double* tau, double* work, blasint* info)
#endif
{
	void (*fn) (void* m, void* n, void* k, void* a, void* lda, void* tau, void* work, void* info);
	void (*fn_hook) (void* m, void* n, void* k, void* a, void* lda, void* tau, void* work, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dorg2r.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dorg2r.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) m, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) tau, (void*) work, (void*) info); 
		return;
	} else {
		hook_pos_dorg2r = 0;
		fn_hook((void*) m, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) tau, (void*) work, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dorg2r_(blasint* m, blasint* n, blasint* k, double* a, blasint* lda, double* tau, double* work, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dorg2r,DORG2R)))));
#else
#ifndef __APPLE__
void dorg2r(blasint* m, blasint* n, blasint* k, double* a, blasint* lda, double* tau, double* work, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dorg2r,DORG2R)))));
#else
void dorg2r(blasint* m, blasint* n, blasint* k, double* a, blasint* lda, double* tau, double* work, blasint* info){ FC_GLOBAL(dorg2r,DORG2R)((void*) m, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) tau, (void*) work, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dorg2r_(void* m, void* n, void* k, void* a, void* lda, void* tau, void* work, void* info)
{
	void (*fn) (void* m, void* n, void* k, void* a, void* lda, void* tau, void* work, void* info);

	*(void **) & fn = current_backend->lapack.dorg2r.f77_blas_function; 

		fn((void*) m, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) tau, (void*) work, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_dorg2r(void* m, void* n, void* k, void* a, void* lda, void* tau, void* work, void* info) __attribute__((alias("flexiblas_real_dorg2r_")));
#else
void flexiblas_real_dorg2r(void* m, void* n, void* k, void* a, void* lda, void* tau, void* work, void* info){flexiblas_real_dorg2r_((void*) m, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) tau, (void*) work, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dorg2r_(void* m, void* n, void* k, void* a, void* lda, void* tau, void* work, void* info)
{
	void (*fn) (void* m, void* n, void* k, void* a, void* lda, void* tau, void* work, void* info);
	void (*fn_hook) (void* m, void* n, void* k, void* a, void* lda, void* tau, void* work, void* info);

	*(void **) &fn      = current_backend->lapack.dorg2r.f77_blas_function; 

    hook_pos_dorg2r ++;
    if( hook_pos_dorg2r < __flexiblas_hooks->dorg2r.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dorg2r.f77_hook_function[hook_pos_dorg2r];
        fn_hook((void*) m, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) tau, (void*) work, (void*) info);
    } else {
        hook_pos_dorg2r = 0;
		fn((void*) m, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) tau, (void*) work, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_dorg2r(void* m, void* n, void* k, void* a, void* lda, void* tau, void* work, void* info) __attribute__((alias("flexiblas_chain_dorg2r_")));
#else
void flexiblas_chain_dorg2r(void* m, void* n, void* k, void* a, void* lda, void* tau, void* work, void* info){flexiblas_chain_dorg2r_((void*) m, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) tau, (void*) work, (void*) info);}
#endif



