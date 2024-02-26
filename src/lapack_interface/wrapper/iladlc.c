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


#ifndef FLEXIBLAS_CHARLEN_T
#define FLEXIBLAS_CHARLEN_T
#if __GNUC__ > 7
typedef size_t flexiblas_fortran_charlen_t;
#else
typedef int flexiblas_fortran_charlen_t;
#endif
#endif

#ifndef blasint
#ifdef FLEXIBLAS_INTEGER8
#define blasint int64_t
#else
#define blasint int
#endif
#endif



static TLS_STORE uint8_t hook_pos_iladlc = 0;
#ifdef FLEXIBLAS_ABI_INTEL
int FC_GLOBAL(iladlc,ILADLC)(blasint* m, blasint* n, double* a, blasint* lda)
#else
int FC_GLOBAL(iladlc,ILADLC)(blasint* m, blasint* n, double* a, blasint* lda)
#endif
{
	blasint (*fn) (void* m, void* n, void* a, void* lda);
	blasint (*fn_hook) (void* m, void* n, void* a, void* lda);
	blasint ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.iladlc.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->iladlc.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		ret = fn((void*) m, (void*) n, (void*) a, (void*) lda); 
		return ret; 
	} else {
		hook_pos_iladlc = 0;
		ret=fn_hook((void*) m, (void*) n, (void*) a, (void*) lda);
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
int iladlc_(blasint* m, blasint* n, double* a, blasint* lda) __attribute__((alias(MTS(FC_GLOBAL(iladlc,ILADLC)))));
#else
#ifndef __APPLE__
int iladlc(blasint* m, blasint* n, double* a, blasint* lda) __attribute__((alias(MTS(FC_GLOBAL(iladlc,ILADLC)))));
#else
int iladlc(blasint* m, blasint* n, double* a, blasint* lda){ return FC_GLOBAL(iladlc,ILADLC)((void*) m, (void*) n, (void*) a, (void*) lda); }
#endif
#endif




/* Real Implementation for Hooks */


blasint flexiblas_real_iladlc_(void* m, void* n, void* a, void* lda)
{
	blasint (*fn) (void* m, void* n, void* a, void* lda);
	blasint ret;

	*(void **) & fn = current_backend->lapack.iladlc.f77_blas_function; 

		ret = fn((void*) m, (void*) n, (void*) a, (void*) lda); 

	return ret ;
}
#ifndef __APPLE__
blasint flexiblas_real_iladlc(void* m, void* n, void* a, void* lda) __attribute__((alias("flexiblas_real_iladlc_")));
#else
blasint flexiblas_real_iladlc(void* m, void* n, void* a, void* lda){return flexiblas_real_iladlc_((void*) m, (void*) n, (void*) a, (void*) lda);}
#endif




/* Chainloader for Hooks */


blasint flexiblas_chain_iladlc_(void* m, void* n, void* a, void* lda)
{
	blasint (*fn) (void* m, void* n, void* a, void* lda);
	blasint (*fn_hook) (void* m, void* n, void* a, void* lda);
	blasint ret;

	*(void **) &fn      = current_backend->lapack.iladlc.f77_blas_function; 

    hook_pos_iladlc ++;
    if( hook_pos_iladlc < __flexiblas_hooks->iladlc.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->iladlc.f77_hook_function[hook_pos_iladlc];
        ret = fn_hook((void*) m, (void*) n, (void*) a, (void*) lda);
    } else {
        hook_pos_iladlc = 0;
		ret = fn((void*) m, (void*) n, (void*) a, (void*) lda); 
	}
	return ret ;
}
#ifndef __APPLE__
blasint flexiblas_chain_iladlc(void* m, void* n, void* a, void* lda) __attribute__((alias("flexiblas_chain_iladlc_")));
#else
blasint flexiblas_chain_iladlc(void* m, void* n, void* a, void* lda){return flexiblas_chain_iladlc_((void*) m, (void*) n, (void*) a, (void*) lda);}
#endif



