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



static TLS_STORE uint8_t hook_pos_slatzm = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slatzm,SLATZM)(char* side, blasint* m, blasint* n, float* v, blasint* incv, float* tau, float* c1, float* c2, blasint* ldc, float* work)
#else
void FC_GLOBAL(slatzm,SLATZM)(char* side, blasint* m, blasint* n, float* v, blasint* incv, float* tau, float* c1, float* c2, blasint* ldc, float* work)
#endif
{
	void (*fn) (void* side, void* m, void* n, void* v, void* incv, void* tau, void* c1, void* c2, void* ldc, void* work);
	void (*fn_hook) (void* side, void* m, void* n, void* v, void* incv, void* tau, void* c1, void* c2, void* ldc, void* work);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.slatzm.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->slatzm.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) side, (void*) m, (void*) n, (void*) v, (void*) incv, (void*) tau, (void*) c1, (void*) c2, (void*) ldc, (void*) work); 
		return;
	} else {
		hook_pos_slatzm = 0;
		fn_hook((void*) side, (void*) m, (void*) n, (void*) v, (void*) incv, (void*) tau, (void*) c1, (void*) c2, (void*) ldc, (void*) work);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void slatzm_(char* side, blasint* m, blasint* n, float* v, blasint* incv, float* tau, float* c1, float* c2, blasint* ldc, float* work) __attribute__((alias(MTS(FC_GLOBAL(slatzm,SLATZM)))));
#else
#ifndef __APPLE__
void slatzm(char* side, blasint* m, blasint* n, float* v, blasint* incv, float* tau, float* c1, float* c2, blasint* ldc, float* work) __attribute__((alias(MTS(FC_GLOBAL(slatzm,SLATZM)))));
#else
void slatzm(char* side, blasint* m, blasint* n, float* v, blasint* incv, float* tau, float* c1, float* c2, blasint* ldc, float* work){ FC_GLOBAL(slatzm,SLATZM)((void*) side, (void*) m, (void*) n, (void*) v, (void*) incv, (void*) tau, (void*) c1, (void*) c2, (void*) ldc, (void*) work); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slatzm_(void* side, void* m, void* n, void* v, void* incv, void* tau, void* c1, void* c2, void* ldc, void* work)
{
	void (*fn) (void* side, void* m, void* n, void* v, void* incv, void* tau, void* c1, void* c2, void* ldc, void* work);

	*(void **) & fn = current_backend->lapack.slatzm.f77_blas_function; 

		fn((void*) side, (void*) m, (void*) n, (void*) v, (void*) incv, (void*) tau, (void*) c1, (void*) c2, (void*) ldc, (void*) work); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_slatzm(void* side, void* m, void* n, void* v, void* incv, void* tau, void* c1, void* c2, void* ldc, void* work) __attribute__((alias("flexiblas_real_slatzm_")));
#else
void flexiblas_real_slatzm(void* side, void* m, void* n, void* v, void* incv, void* tau, void* c1, void* c2, void* ldc, void* work){flexiblas_real_slatzm_((void*) side, (void*) m, (void*) n, (void*) v, (void*) incv, (void*) tau, (void*) c1, (void*) c2, (void*) ldc, (void*) work);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_slatzm_(void* side, void* m, void* n, void* v, void* incv, void* tau, void* c1, void* c2, void* ldc, void* work)
{
	void (*fn) (void* side, void* m, void* n, void* v, void* incv, void* tau, void* c1, void* c2, void* ldc, void* work);
	void (*fn_hook) (void* side, void* m, void* n, void* v, void* incv, void* tau, void* c1, void* c2, void* ldc, void* work);

	*(void **) &fn      = current_backend->lapack.slatzm.f77_blas_function; 

    hook_pos_slatzm ++;
    if( hook_pos_slatzm < __flexiblas_hooks->slatzm.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->slatzm.f77_hook_function[hook_pos_slatzm];
        fn_hook((void*) side, (void*) m, (void*) n, (void*) v, (void*) incv, (void*) tau, (void*) c1, (void*) c2, (void*) ldc, (void*) work);
    } else {
        hook_pos_slatzm = 0;
		fn((void*) side, (void*) m, (void*) n, (void*) v, (void*) incv, (void*) tau, (void*) c1, (void*) c2, (void*) ldc, (void*) work); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_slatzm(void* side, void* m, void* n, void* v, void* incv, void* tau, void* c1, void* c2, void* ldc, void* work) __attribute__((alias("flexiblas_chain_slatzm_")));
#else
void flexiblas_chain_slatzm(void* side, void* m, void* n, void* v, void* incv, void* tau, void* c1, void* c2, void* ldc, void* work){flexiblas_chain_slatzm_((void*) side, (void*) m, (void*) n, (void*) v, (void*) incv, (void*) tau, (void*) c1, (void*) c2, (void*) ldc, (void*) work);}
#endif



