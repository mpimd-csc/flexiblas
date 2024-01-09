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



static TLS_STORE uint8_t hook_pos_zlanhb = 0;
#ifdef FLEXIBLAS_ABI_INTEL
double FC_GLOBAL(zlanhb,ZLANHB)(char* norm, char* uplo, blasint* n, blasint* k, double complex* ab, blasint* ldab, double* work)
#else
double FC_GLOBAL(zlanhb,ZLANHB)(char* norm, char* uplo, blasint* n, blasint* k, double complex* ab, blasint* ldab, double* work)
#endif
{
	double (*fn) (void* norm, void* uplo, void* n, void* k, void* ab, void* ldab, void* work);
	double (*fn_hook) (void* norm, void* uplo, void* n, void* k, void* ab, void* ldab, void* work);
	double ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.zlanhb.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->zlanhb.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		ret = fn((void*) norm, (void*) uplo, (void*) n, (void*) k, (void*) ab, (void*) ldab, (void*) work); 
		return ret; 
	} else {
		hook_pos_zlanhb = 0;
		ret=fn_hook((void*) norm, (void*) uplo, (void*) n, (void*) k, (void*) ab, (void*) ldab, (void*) work);
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
double zlanhb_(char* norm, char* uplo, blasint* n, blasint* k, double complex* ab, blasint* ldab, double* work) __attribute__((alias(MTS(FC_GLOBAL(zlanhb,ZLANHB)))));
#else
#ifndef __APPLE__
double zlanhb(char* norm, char* uplo, blasint* n, blasint* k, double complex* ab, blasint* ldab, double* work) __attribute__((alias(MTS(FC_GLOBAL(zlanhb,ZLANHB)))));
#else
double zlanhb(char* norm, char* uplo, blasint* n, blasint* k, double complex* ab, blasint* ldab, double* work){ return FC_GLOBAL(zlanhb,ZLANHB)((void*) norm, (void*) uplo, (void*) n, (void*) k, (void*) ab, (void*) ldab, (void*) work); }
#endif
#endif




/* Real Implementation for Hooks */


double flexiblas_real_zlanhb_(void* norm, void* uplo, void* n, void* k, void* ab, void* ldab, void* work)
{
	double (*fn) (void* norm, void* uplo, void* n, void* k, void* ab, void* ldab, void* work);
	double ret;

	*(void **) & fn = current_backend->lapack.zlanhb.f77_blas_function; 

		ret = fn((void*) norm, (void*) uplo, (void*) n, (void*) k, (void*) ab, (void*) ldab, (void*) work); 

	return ret ;
}
#ifndef __APPLE__
double flexiblas_real_zlanhb(void* norm, void* uplo, void* n, void* k, void* ab, void* ldab, void* work) __attribute__((alias("flexiblas_real_zlanhb_")));
#else
double flexiblas_real_zlanhb(void* norm, void* uplo, void* n, void* k, void* ab, void* ldab, void* work){return flexiblas_real_zlanhb_((void*) norm, (void*) uplo, (void*) n, (void*) k, (void*) ab, (void*) ldab, (void*) work);}
#endif




/* Chainloader for Hooks */


double flexiblas_chain_zlanhb_(void* norm, void* uplo, void* n, void* k, void* ab, void* ldab, void* work)
{
	double (*fn) (void* norm, void* uplo, void* n, void* k, void* ab, void* ldab, void* work);
	double (*fn_hook) (void* norm, void* uplo, void* n, void* k, void* ab, void* ldab, void* work);
	double ret;

	*(void **) &fn      = current_backend->lapack.zlanhb.f77_blas_function; 

    hook_pos_zlanhb ++;
    if( hook_pos_zlanhb < __flexiblas_hooks->zlanhb.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zlanhb.f77_hook_function[hook_pos_zlanhb];
        ret = fn_hook((void*) norm, (void*) uplo, (void*) n, (void*) k, (void*) ab, (void*) ldab, (void*) work);
    } else {
        hook_pos_zlanhb = 0;
		ret = fn((void*) norm, (void*) uplo, (void*) n, (void*) k, (void*) ab, (void*) ldab, (void*) work); 
	}
	return ret ;
}
#ifndef __APPLE__
double flexiblas_chain_zlanhb(void* norm, void* uplo, void* n, void* k, void* ab, void* ldab, void* work) __attribute__((alias("flexiblas_chain_zlanhb_")));
#else
double flexiblas_chain_zlanhb(void* norm, void* uplo, void* n, void* k, void* ab, void* ldab, void* work){return flexiblas_chain_zlanhb_((void*) norm, (void*) uplo, (void*) n, (void*) k, (void*) ab, (void*) ldab, (void*) work);}
#endif



