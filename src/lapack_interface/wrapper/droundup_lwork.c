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



static TLS_STORE uint8_t hook_pos_droundup_lwork = 0;
#ifdef FLEXIBLAS_ABI_INTEL
double FC_GLOBAL_(droundup_lwork,DROUNDUP_LWORK)(blasint* lwork)
#else
double FC_GLOBAL_(droundup_lwork,DROUNDUP_LWORK)(blasint* lwork)
#endif
{
	double (*fn) (void* lwork);
	double (*fn_hook) (void* lwork);
	double ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.droundup_lwork.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->droundup_lwork.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		ret = fn((void*) lwork); 
		return ret; 
	} else {
		hook_pos_droundup_lwork = 0;
		ret=fn_hook((void*) lwork);
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
double droundup_lwork_(blasint* lwork) __attribute__((alias(MTS(FC_GLOBAL_(droundup_lwork,DROUNDUP_LWORK)))));
#else
#ifndef __APPLE__
double droundup_lwork(blasint* lwork) __attribute__((alias(MTS(FC_GLOBAL_(droundup_lwork,DROUNDUP_LWORK)))));
#else
double droundup_lwork(blasint* lwork){ return FC_GLOBAL_(droundup_lwork,DROUNDUP_LWORK)((void*) lwork); }
#endif
#endif




/* Real Implementation for Hooks */


double flexiblas_real_droundup_lwork_(void* lwork)
{
	double (*fn) (void* lwork);
	double ret;

	*(void **) & fn = current_backend->lapack.droundup_lwork.f77_blas_function; 

		ret = fn((void*) lwork); 

	return ret ;
}
#ifndef __APPLE__
double flexiblas_real_droundup_lwork(void* lwork) __attribute__((alias("flexiblas_real_droundup_lwork_")));
#else
double flexiblas_real_droundup_lwork(void* lwork){return flexiblas_real_droundup_lwork_((void*) lwork);}
#endif




/* Chainloader for Hooks */


double flexiblas_chain_droundup_lwork_(void* lwork)
{
	double (*fn) (void* lwork);
	double (*fn_hook) (void* lwork);
	double ret;

	*(void **) &fn      = current_backend->lapack.droundup_lwork.f77_blas_function; 

    hook_pos_droundup_lwork ++;
    if( hook_pos_droundup_lwork < __flexiblas_hooks->droundup_lwork.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->droundup_lwork.f77_hook_function[hook_pos_droundup_lwork];
        ret = fn_hook((void*) lwork);
    } else {
        hook_pos_droundup_lwork = 0;
		ret = fn((void*) lwork); 
	}
	return ret ;
}
#ifndef __APPLE__
double flexiblas_chain_droundup_lwork(void* lwork) __attribute__((alias("flexiblas_chain_droundup_lwork_")));
#else
double flexiblas_chain_droundup_lwork(void* lwork){return flexiblas_chain_droundup_lwork_((void*) lwork);}
#endif



