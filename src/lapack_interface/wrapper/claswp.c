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


static TLS_STORE uint8_t hook_pos_claswp = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(claswp,CLASWP)(blasint* n, float complex* a, blasint* lda, blasint* k1, blasint* k2, blasint* ipiv, blasint* incx)
#else
void FC_GLOBAL(claswp,CLASWP)(blasint* n, float complex* a, blasint* lda, blasint* k1, blasint* k2, blasint* ipiv, blasint* incx)
#endif
{
	void (*fn) (void* n, void* a, void* lda, void* k1, void* k2, void* ipiv, void* incx);
	void (*fn_hook) (void* n, void* a, void* lda, void* k1, void* k2, void* ipiv, void* incx);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.claswp.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->claswp.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) a, (void*) lda, (void*) k1, (void*) k2, (void*) ipiv, (void*) incx); 
		return;
	} else {
		hook_pos_claswp = 0;
		fn_hook((void*) n, (void*) a, (void*) lda, (void*) k1, (void*) k2, (void*) ipiv, (void*) incx);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void claswp_(blasint* n, float complex* a, blasint* lda, blasint* k1, blasint* k2, blasint* ipiv, blasint* incx) __attribute__((alias(MTS(FC_GLOBAL(claswp,CLASWP)))));
#else
#ifndef __APPLE__
void claswp(blasint* n, float complex* a, blasint* lda, blasint* k1, blasint* k2, blasint* ipiv, blasint* incx) __attribute__((alias(MTS(FC_GLOBAL(claswp,CLASWP)))));
#else
void claswp(blasint* n, float complex* a, blasint* lda, blasint* k1, blasint* k2, blasint* ipiv, blasint* incx){ FC_GLOBAL(claswp,CLASWP)((void*) n, (void*) a, (void*) lda, (void*) k1, (void*) k2, (void*) ipiv, (void*) incx); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_claswp_(void* n, void* a, void* lda, void* k1, void* k2, void* ipiv, void* incx)
{
	void (*fn) (void* n, void* a, void* lda, void* k1, void* k2, void* ipiv, void* incx);

	*(void **) & fn = current_backend->lapack.claswp.f77_blas_function; 

		fn((void*) n, (void*) a, (void*) lda, (void*) k1, (void*) k2, (void*) ipiv, (void*) incx); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_claswp(void* n, void* a, void* lda, void* k1, void* k2, void* ipiv, void* incx) __attribute__((alias("flexiblas_real_claswp_")));
#else
void flexiblas_real_claswp(void* n, void* a, void* lda, void* k1, void* k2, void* ipiv, void* incx){flexiblas_real_claswp_((void*) n, (void*) a, (void*) lda, (void*) k1, (void*) k2, (void*) ipiv, (void*) incx);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_claswp_(void* n, void* a, void* lda, void* k1, void* k2, void* ipiv, void* incx)
{
	void (*fn) (void* n, void* a, void* lda, void* k1, void* k2, void* ipiv, void* incx);
	void (*fn_hook) (void* n, void* a, void* lda, void* k1, void* k2, void* ipiv, void* incx);

	*(void **) &fn      = current_backend->lapack.claswp.f77_blas_function; 

    hook_pos_claswp ++;
    if( hook_pos_claswp < __flexiblas_hooks->claswp.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->claswp.f77_hook_function[hook_pos_claswp];
        fn_hook((void*) n, (void*) a, (void*) lda, (void*) k1, (void*) k2, (void*) ipiv, (void*) incx);
    } else {
        hook_pos_claswp = 0;
		fn((void*) n, (void*) a, (void*) lda, (void*) k1, (void*) k2, (void*) ipiv, (void*) incx); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_claswp(void* n, void* a, void* lda, void* k1, void* k2, void* ipiv, void* incx) __attribute__((alias("flexiblas_chain_claswp_")));
#else
void flexiblas_chain_claswp(void* n, void* a, void* lda, void* k1, void* k2, void* ipiv, void* incx){flexiblas_chain_claswp_((void*) n, (void*) a, (void*) lda, (void*) k1, (void*) k2, (void*) ipiv, (void*) incx);}
#endif



