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


static TLS_STORE uint8_t hook_pos_sroundup_lwork = 0;
#ifdef FLEXIBLAS_ABI_INTEL
float FC_GLOBAL_(sroundup_lwork,SROUNDUP_LWORK)(blasint* lwork)
#else
float FC_GLOBAL_(sroundup_lwork,SROUNDUP_LWORK)(blasint* lwork)
#endif
{
	float (*fn) (void* lwork);
	float (*fn_hook) (void* lwork);
	float ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.sroundup_lwork.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->sroundup_lwork.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		ret = fn((void*) lwork); 
		return ret; 
	} else {
		hook_pos_sroundup_lwork = 0;
		ret=fn_hook((void*) lwork);
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
float sroundup_lwork_(blasint* lwork) __attribute__((alias(MTS(FC_GLOBAL_(sroundup_lwork,SROUNDUP_LWORK)))));
#else
#ifndef __APPLE__
float sroundup_lwork(blasint* lwork) __attribute__((alias(MTS(FC_GLOBAL_(sroundup_lwork,SROUNDUP_LWORK)))));
#else
float sroundup_lwork(blasint* lwork){ return FC_GLOBAL_(sroundup_lwork,SROUNDUP_LWORK)((void*) lwork); }
#endif
#endif




/* Real Implementation for Hooks */


float flexiblas_real_sroundup_lwork_(void* lwork)
{
	float (*fn) (void* lwork);
	float ret;

	*(void **) & fn = current_backend->lapack.sroundup_lwork.f77_blas_function; 

		ret = fn((void*) lwork); 

	return ret ;
}
#ifndef __APPLE__
float flexiblas_real_sroundup_lwork(void* lwork) __attribute__((alias("flexiblas_real_sroundup_lwork_")));
#else
float flexiblas_real_sroundup_lwork(void* lwork){return flexiblas_real_sroundup_lwork_((void*) lwork);}
#endif




/* Chainloader for Hooks */


float flexiblas_chain_sroundup_lwork_(void* lwork)
{
	float (*fn) (void* lwork);
	float (*fn_hook) (void* lwork);
	float ret;

	*(void **) &fn      = current_backend->lapack.sroundup_lwork.f77_blas_function; 

    hook_pos_sroundup_lwork ++;
    if( hook_pos_sroundup_lwork < __flexiblas_hooks->sroundup_lwork.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->sroundup_lwork.f77_hook_function[hook_pos_sroundup_lwork];
        ret = fn_hook((void*) lwork);
    } else {
        hook_pos_sroundup_lwork = 0;
		ret = fn((void*) lwork); 
	}
	return ret ;
}
#ifndef __APPLE__
float flexiblas_chain_sroundup_lwork(void* lwork) __attribute__((alias("flexiblas_chain_sroundup_lwork_")));
#else
float flexiblas_chain_sroundup_lwork(void* lwork){return flexiblas_chain_sroundup_lwork_((void*) lwork);}
#endif



