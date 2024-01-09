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



static TLS_STORE uint8_t hook_pos_cgebal = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(cgebal,CGEBAL)(char* job, blasint* n, float complex* a, blasint* lda, blasint* ilo, blasint* ihi, float* scale, blasint* info)
#else
void FC_GLOBAL(cgebal,CGEBAL)(char* job, blasint* n, float complex* a, blasint* lda, blasint* ilo, blasint* ihi, float* scale, blasint* info)
#endif
{
	void (*fn) (void* job, void* n, void* a, void* lda, void* ilo, void* ihi, void* scale, void* info);
	void (*fn_hook) (void* job, void* n, void* a, void* lda, void* ilo, void* ihi, void* scale, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.cgebal.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->cgebal.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) job, (void*) n, (void*) a, (void*) lda, (void*) ilo, (void*) ihi, (void*) scale, (void*) info); 
		return;
	} else {
		hook_pos_cgebal = 0;
		fn_hook((void*) job, (void*) n, (void*) a, (void*) lda, (void*) ilo, (void*) ihi, (void*) scale, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void cgebal_(char* job, blasint* n, float complex* a, blasint* lda, blasint* ilo, blasint* ihi, float* scale, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(cgebal,CGEBAL)))));
#else
#ifndef __APPLE__
void cgebal(char* job, blasint* n, float complex* a, blasint* lda, blasint* ilo, blasint* ihi, float* scale, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(cgebal,CGEBAL)))));
#else
void cgebal(char* job, blasint* n, float complex* a, blasint* lda, blasint* ilo, blasint* ihi, float* scale, blasint* info){ FC_GLOBAL(cgebal,CGEBAL)((void*) job, (void*) n, (void*) a, (void*) lda, (void*) ilo, (void*) ihi, (void*) scale, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_cgebal_(void* job, void* n, void* a, void* lda, void* ilo, void* ihi, void* scale, void* info)
{
	void (*fn) (void* job, void* n, void* a, void* lda, void* ilo, void* ihi, void* scale, void* info);

	*(void **) & fn = current_backend->lapack.cgebal.f77_blas_function; 

		fn((void*) job, (void*) n, (void*) a, (void*) lda, (void*) ilo, (void*) ihi, (void*) scale, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_cgebal(void* job, void* n, void* a, void* lda, void* ilo, void* ihi, void* scale, void* info) __attribute__((alias("flexiblas_real_cgebal_")));
#else
void flexiblas_real_cgebal(void* job, void* n, void* a, void* lda, void* ilo, void* ihi, void* scale, void* info){flexiblas_real_cgebal_((void*) job, (void*) n, (void*) a, (void*) lda, (void*) ilo, (void*) ihi, (void*) scale, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_cgebal_(void* job, void* n, void* a, void* lda, void* ilo, void* ihi, void* scale, void* info)
{
	void (*fn) (void* job, void* n, void* a, void* lda, void* ilo, void* ihi, void* scale, void* info);
	void (*fn_hook) (void* job, void* n, void* a, void* lda, void* ilo, void* ihi, void* scale, void* info);

	*(void **) &fn      = current_backend->lapack.cgebal.f77_blas_function; 

    hook_pos_cgebal ++;
    if( hook_pos_cgebal < __flexiblas_hooks->cgebal.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->cgebal.f77_hook_function[hook_pos_cgebal];
        fn_hook((void*) job, (void*) n, (void*) a, (void*) lda, (void*) ilo, (void*) ihi, (void*) scale, (void*) info);
    } else {
        hook_pos_cgebal = 0;
		fn((void*) job, (void*) n, (void*) a, (void*) lda, (void*) ilo, (void*) ihi, (void*) scale, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_cgebal(void* job, void* n, void* a, void* lda, void* ilo, void* ihi, void* scale, void* info) __attribute__((alias("flexiblas_chain_cgebal_")));
#else
void flexiblas_chain_cgebal(void* job, void* n, void* a, void* lda, void* ilo, void* ihi, void* scale, void* info){flexiblas_chain_cgebal_((void*) job, (void*) n, (void*) a, (void*) lda, (void*) ilo, (void*) ihi, (void*) scale, (void*) info);}
#endif



