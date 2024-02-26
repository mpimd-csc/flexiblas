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



static TLS_STORE uint8_t hook_pos_dlarmm = 0;
#ifdef FLEXIBLAS_ABI_INTEL
double FC_GLOBAL(dlarmm,DLARMM)(double* anorm, double* bnorm, double* cnorm)
#else
double FC_GLOBAL(dlarmm,DLARMM)(double* anorm, double* bnorm, double* cnorm)
#endif
{
	double (*fn) (void* anorm, void* bnorm, void* cnorm);
	double (*fn_hook) (void* anorm, void* bnorm, void* cnorm);
	double ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dlarmm.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dlarmm.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		ret = fn((void*) anorm, (void*) bnorm, (void*) cnorm); 
		return ret; 
	} else {
		hook_pos_dlarmm = 0;
		ret=fn_hook((void*) anorm, (void*) bnorm, (void*) cnorm);
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
double dlarmm_(double* anorm, double* bnorm, double* cnorm) __attribute__((alias(MTS(FC_GLOBAL(dlarmm,DLARMM)))));
#else
#ifndef __APPLE__
double dlarmm(double* anorm, double* bnorm, double* cnorm) __attribute__((alias(MTS(FC_GLOBAL(dlarmm,DLARMM)))));
#else
double dlarmm(double* anorm, double* bnorm, double* cnorm){ return FC_GLOBAL(dlarmm,DLARMM)((void*) anorm, (void*) bnorm, (void*) cnorm); }
#endif
#endif




/* Real Implementation for Hooks */


double flexiblas_real_dlarmm_(void* anorm, void* bnorm, void* cnorm)
{
	double (*fn) (void* anorm, void* bnorm, void* cnorm);
	double ret;

	*(void **) & fn = current_backend->lapack.dlarmm.f77_blas_function; 

		ret = fn((void*) anorm, (void*) bnorm, (void*) cnorm); 

	return ret ;
}
#ifndef __APPLE__
double flexiblas_real_dlarmm(void* anorm, void* bnorm, void* cnorm) __attribute__((alias("flexiblas_real_dlarmm_")));
#else
double flexiblas_real_dlarmm(void* anorm, void* bnorm, void* cnorm){return flexiblas_real_dlarmm_((void*) anorm, (void*) bnorm, (void*) cnorm);}
#endif




/* Chainloader for Hooks */


double flexiblas_chain_dlarmm_(void* anorm, void* bnorm, void* cnorm)
{
	double (*fn) (void* anorm, void* bnorm, void* cnorm);
	double (*fn_hook) (void* anorm, void* bnorm, void* cnorm);
	double ret;

	*(void **) &fn      = current_backend->lapack.dlarmm.f77_blas_function; 

    hook_pos_dlarmm ++;
    if( hook_pos_dlarmm < __flexiblas_hooks->dlarmm.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlarmm.f77_hook_function[hook_pos_dlarmm];
        ret = fn_hook((void*) anorm, (void*) bnorm, (void*) cnorm);
    } else {
        hook_pos_dlarmm = 0;
		ret = fn((void*) anorm, (void*) bnorm, (void*) cnorm); 
	}
	return ret ;
}
#ifndef __APPLE__
double flexiblas_chain_dlarmm(void* anorm, void* bnorm, void* cnorm) __attribute__((alias("flexiblas_chain_dlarmm_")));
#else
double flexiblas_chain_dlarmm(void* anorm, void* bnorm, void* cnorm){return flexiblas_chain_dlarmm_((void*) anorm, (void*) bnorm, (void*) cnorm);}
#endif



