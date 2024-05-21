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


static TLS_STORE uint8_t hook_pos_slamch = 0;
#ifdef FLEXIBLAS_ABI_INTEL
float FC_GLOBAL(slamch,SLAMCH)(char* cmach, flexiblas_fortran_charlen_t len_cmach)
#else
float FC_GLOBAL(slamch,SLAMCH)(char* cmach, flexiblas_fortran_charlen_t len_cmach)
#endif
{
	float (*fn) (void* cmach, flexiblas_fortran_charlen_t len_cmach);
	float (*fn_hook) (void* cmach, flexiblas_fortran_charlen_t len_cmach);
	float ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.slamch.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->slamch.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		ret = fn((void*) cmach, ( flexiblas_fortran_charlen_t ) len_cmach); 
		return ret; 
	} else {
		hook_pos_slamch = 0;
		ret=fn_hook((void*) cmach, ( flexiblas_fortran_charlen_t ) len_cmach);
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
float slamch_(char* cmach, flexiblas_fortran_charlen_t len_cmach) __attribute__((alias(MTS(FC_GLOBAL(slamch,SLAMCH)))));
#else
#ifndef __APPLE__
float slamch(char* cmach, flexiblas_fortran_charlen_t len_cmach) __attribute__((alias(MTS(FC_GLOBAL(slamch,SLAMCH)))));
#else
float slamch(char* cmach, flexiblas_fortran_charlen_t len_cmach){ return FC_GLOBAL(slamch,SLAMCH)((void*) cmach, (flexiblas_fortran_charlen_t) len_cmach); }
#endif
#endif




/* Real Implementation for Hooks */


float flexiblas_real_slamch_(void* cmach, flexiblas_fortran_charlen_t len_cmach)
{
	float (*fn) (void* cmach, flexiblas_fortran_charlen_t len_cmach);
	float ret;

	*(void **) & fn = current_backend->lapack.slamch.f77_blas_function; 

		ret = fn((void*) cmach, ( flexiblas_fortran_charlen_t ) len_cmach); 

	return ret ;
}
#ifndef __APPLE__
float flexiblas_real_slamch(void* cmach, flexiblas_fortran_charlen_t len_cmach) __attribute__((alias("flexiblas_real_slamch_")));
#else
float flexiblas_real_slamch(void* cmach, flexiblas_fortran_charlen_t len_cmach){return flexiblas_real_slamch_((void*) cmach, (flexiblas_fortran_charlen_t) len_cmach);}
#endif




/* Chainloader for Hooks */


float flexiblas_chain_slamch_(void* cmach, flexiblas_fortran_charlen_t len_cmach)
{
	float (*fn) (void* cmach, flexiblas_fortran_charlen_t len_cmach);
	float (*fn_hook) (void* cmach, flexiblas_fortran_charlen_t len_cmach);
	float ret;

	*(void **) &fn      = current_backend->lapack.slamch.f77_blas_function; 

    hook_pos_slamch ++;
    if( hook_pos_slamch < __flexiblas_hooks->slamch.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->slamch.f77_hook_function[hook_pos_slamch];
        ret = fn_hook((void*) cmach, ( flexiblas_fortran_charlen_t )len_cmach);
    } else {
        hook_pos_slamch = 0;
		ret = fn((void*) cmach, ( flexiblas_fortran_charlen_t ) len_cmach); 
	}
	return ret ;
}
#ifndef __APPLE__
float flexiblas_chain_slamch(void* cmach, flexiblas_fortran_charlen_t len_cmach) __attribute__((alias("flexiblas_chain_slamch_")));
#else
float flexiblas_chain_slamch(void* cmach, flexiblas_fortran_charlen_t len_cmach){return flexiblas_chain_slamch_((void*) cmach, (flexiblas_fortran_charlen_t) len_cmach);}
#endif



