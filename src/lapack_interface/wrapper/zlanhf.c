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



static TLS_STORE uint8_t hook_pos_zlanhf = 0;
#ifdef FLEXIBLAS_ABI_INTEL
double FC_GLOBAL(zlanhf,ZLANHF)(char* norm, char* transr, char* uplo, blasint* n, double complex* a, double* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo)
#else
double FC_GLOBAL(zlanhf,ZLANHF)(char* norm, char* transr, char* uplo, blasint* n, double complex* a, double* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo)
#endif
{
	double (*fn) (void* norm, void* transr, void* uplo, void* n, void* a, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);
	double (*fn_hook) (void* norm, void* transr, void* uplo, void* n, void* a, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);
	double ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.zlanhf.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->zlanhf.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		ret = fn((void*) norm, (void*) transr, (void*) uplo, (void*) n, (void*) a, (void*) work, ( flexiblas_fortran_charlen_t ) len_norm, ( flexiblas_fortran_charlen_t ) len_transr, ( flexiblas_fortran_charlen_t ) len_uplo); 
		return ret; 
	} else {
		hook_pos_zlanhf = 0;
		ret=fn_hook((void*) norm, (void*) transr, (void*) uplo, (void*) n, (void*) a, (void*) work, ( flexiblas_fortran_charlen_t ) len_norm, ( flexiblas_fortran_charlen_t ) len_transr, ( flexiblas_fortran_charlen_t ) len_uplo);
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
double zlanhf_(char* norm, char* transr, char* uplo, blasint* n, double complex* a, double* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL(zlanhf,ZLANHF)))));
#else
#ifndef __APPLE__
double zlanhf(char* norm, char* transr, char* uplo, blasint* n, double complex* a, double* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL(zlanhf,ZLANHF)))));
#else
double zlanhf(char* norm, char* transr, char* uplo, blasint* n, double complex* a, double* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo){ return FC_GLOBAL(zlanhf,ZLANHF)((void*) norm, (void*) transr, (void*) uplo, (void*) n, (void*) a, (void*) work, (flexiblas_fortran_charlen_t) len_norm, (flexiblas_fortran_charlen_t) len_transr, (flexiblas_fortran_charlen_t) len_uplo); }
#endif
#endif




/* Real Implementation for Hooks */


double flexiblas_real_zlanhf_(void* norm, void* transr, void* uplo, void* n, void* a, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo)
{
	double (*fn) (void* norm, void* transr, void* uplo, void* n, void* a, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);
	double ret;

	*(void **) & fn = current_backend->lapack.zlanhf.f77_blas_function; 

		ret = fn((void*) norm, (void*) transr, (void*) uplo, (void*) n, (void*) a, (void*) work, ( flexiblas_fortran_charlen_t ) len_norm, ( flexiblas_fortran_charlen_t ) len_transr, ( flexiblas_fortran_charlen_t ) len_uplo); 

	return ret ;
}
#ifndef __APPLE__
double flexiblas_real_zlanhf(void* norm, void* transr, void* uplo, void* n, void* a, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_real_zlanhf_")));
#else
double flexiblas_real_zlanhf(void* norm, void* transr, void* uplo, void* n, void* a, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo){return flexiblas_real_zlanhf_((void*) norm, (void*) transr, (void*) uplo, (void*) n, (void*) a, (void*) work, (flexiblas_fortran_charlen_t) len_norm, (flexiblas_fortran_charlen_t) len_transr, (flexiblas_fortran_charlen_t) len_uplo);}
#endif




/* Chainloader for Hooks */


double flexiblas_chain_zlanhf_(void* norm, void* transr, void* uplo, void* n, void* a, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo)
{
	double (*fn) (void* norm, void* transr, void* uplo, void* n, void* a, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);
	double (*fn_hook) (void* norm, void* transr, void* uplo, void* n, void* a, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);
	double ret;

	*(void **) &fn      = current_backend->lapack.zlanhf.f77_blas_function; 

    hook_pos_zlanhf ++;
    if( hook_pos_zlanhf < __flexiblas_hooks->zlanhf.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zlanhf.f77_hook_function[hook_pos_zlanhf];
        ret = fn_hook((void*) norm, (void*) transr, (void*) uplo, (void*) n, (void*) a, (void*) work, ( flexiblas_fortran_charlen_t )len_norm, ( flexiblas_fortran_charlen_t )len_transr, ( flexiblas_fortran_charlen_t )len_uplo);
    } else {
        hook_pos_zlanhf = 0;
		ret = fn((void*) norm, (void*) transr, (void*) uplo, (void*) n, (void*) a, (void*) work, ( flexiblas_fortran_charlen_t ) len_norm, ( flexiblas_fortran_charlen_t ) len_transr, ( flexiblas_fortran_charlen_t ) len_uplo); 
	}
	return ret ;
}
#ifndef __APPLE__
double flexiblas_chain_zlanhf(void* norm, void* transr, void* uplo, void* n, void* a, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_chain_zlanhf_")));
#else
double flexiblas_chain_zlanhf(void* norm, void* transr, void* uplo, void* n, void* a, void* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo){return flexiblas_chain_zlanhf_((void*) norm, (void*) transr, (void*) uplo, (void*) n, (void*) a, (void*) work, (flexiblas_fortran_charlen_t) len_norm, (flexiblas_fortran_charlen_t) len_transr, (flexiblas_fortran_charlen_t) len_uplo);}
#endif



