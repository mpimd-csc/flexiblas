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



static TLS_STORE uint8_t hook_pos_clange = 0;
#ifdef FLEXIBLAS_ABI_INTEL
float FC_GLOBAL(clange,CLANGE)(char* norm, blasint* m, blasint* n, float complex* a, blasint* lda, float* work)
#else
float FC_GLOBAL(clange,CLANGE)(char* norm, blasint* m, blasint* n, float complex* a, blasint* lda, float* work)
#endif
{
	float (*fn) (void* norm, void* m, void* n, void* a, void* lda, void* work);
	float (*fn_hook) (void* norm, void* m, void* n, void* a, void* lda, void* work);
	float ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.clange.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->clange.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		ret = fn((void*) norm, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) work); 
		return ret; 
	} else {
		hook_pos_clange = 0;
		ret=fn_hook((void*) norm, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) work);
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
float clange_(char* norm, blasint* m, blasint* n, float complex* a, blasint* lda, float* work) __attribute__((alias(MTS(FC_GLOBAL(clange,CLANGE)))));
#else
#ifndef __APPLE__
float clange(char* norm, blasint* m, blasint* n, float complex* a, blasint* lda, float* work) __attribute__((alias(MTS(FC_GLOBAL(clange,CLANGE)))));
#else
float clange(char* norm, blasint* m, blasint* n, float complex* a, blasint* lda, float* work){ return FC_GLOBAL(clange,CLANGE)((void*) norm, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) work); }
#endif
#endif




/* Real Implementation for Hooks */


float flexiblas_real_clange_(void* norm, void* m, void* n, void* a, void* lda, void* work)
{
	float (*fn) (void* norm, void* m, void* n, void* a, void* lda, void* work);
	float ret;

	*(void **) & fn = current_backend->lapack.clange.f77_blas_function; 

		ret = fn((void*) norm, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) work); 

	return ret ;
}
#ifndef __APPLE__
float flexiblas_real_clange(void* norm, void* m, void* n, void* a, void* lda, void* work) __attribute__((alias("flexiblas_real_clange_")));
#else
float flexiblas_real_clange(void* norm, void* m, void* n, void* a, void* lda, void* work){return flexiblas_real_clange_((void*) norm, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) work);}
#endif




/* Chainloader for Hooks */


float flexiblas_chain_clange_(void* norm, void* m, void* n, void* a, void* lda, void* work)
{
	float (*fn) (void* norm, void* m, void* n, void* a, void* lda, void* work);
	float (*fn_hook) (void* norm, void* m, void* n, void* a, void* lda, void* work);
	float ret;

	*(void **) &fn      = current_backend->lapack.clange.f77_blas_function; 

    hook_pos_clange ++;
    if( hook_pos_clange < __flexiblas_hooks->clange.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->clange.f77_hook_function[hook_pos_clange];
        ret = fn_hook((void*) norm, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) work);
    } else {
        hook_pos_clange = 0;
		ret = fn((void*) norm, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) work); 
	}
	return ret ;
}
#ifndef __APPLE__
float flexiblas_chain_clange(void* norm, void* m, void* n, void* a, void* lda, void* work) __attribute__((alias("flexiblas_chain_clange_")));
#else
float flexiblas_chain_clange(void* norm, void* m, void* n, void* a, void* lda, void* work){return flexiblas_chain_clange_((void*) norm, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) work);}
#endif



