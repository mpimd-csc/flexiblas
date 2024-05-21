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


static TLS_STORE uint8_t hook_pos_slangt = 0;
#ifdef FLEXIBLAS_ABI_INTEL
float FC_GLOBAL(slangt,SLANGT)(char* norm, blasint* n, float* dl, float* d, float* du, flexiblas_fortran_charlen_t len_norm)
#else
float FC_GLOBAL(slangt,SLANGT)(char* norm, blasint* n, float* dl, float* d, float* du, flexiblas_fortran_charlen_t len_norm)
#endif
{
	float (*fn) (void* norm, void* n, void* dl, void* d, void* du, flexiblas_fortran_charlen_t len_norm);
	float (*fn_hook) (void* norm, void* n, void* dl, void* d, void* du, flexiblas_fortran_charlen_t len_norm);
	float ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.slangt.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->slangt.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		ret = fn((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du, ( flexiblas_fortran_charlen_t ) len_norm); 
		return ret; 
	} else {
		hook_pos_slangt = 0;
		ret=fn_hook((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du, ( flexiblas_fortran_charlen_t ) len_norm);
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
float slangt_(char* norm, blasint* n, float* dl, float* d, float* du, flexiblas_fortran_charlen_t len_norm) __attribute__((alias(MTS(FC_GLOBAL(slangt,SLANGT)))));
#else
#ifndef __APPLE__
float slangt(char* norm, blasint* n, float* dl, float* d, float* du, flexiblas_fortran_charlen_t len_norm) __attribute__((alias(MTS(FC_GLOBAL(slangt,SLANGT)))));
#else
float slangt(char* norm, blasint* n, float* dl, float* d, float* du, flexiblas_fortran_charlen_t len_norm){ return FC_GLOBAL(slangt,SLANGT)((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du, (flexiblas_fortran_charlen_t) len_norm); }
#endif
#endif




/* Real Implementation for Hooks */


float flexiblas_real_slangt_(void* norm, void* n, void* dl, void* d, void* du, flexiblas_fortran_charlen_t len_norm)
{
	float (*fn) (void* norm, void* n, void* dl, void* d, void* du, flexiblas_fortran_charlen_t len_norm);
	float ret;

	*(void **) & fn = current_backend->lapack.slangt.f77_blas_function; 

		ret = fn((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du, ( flexiblas_fortran_charlen_t ) len_norm); 

	return ret ;
}
#ifndef __APPLE__
float flexiblas_real_slangt(void* norm, void* n, void* dl, void* d, void* du, flexiblas_fortran_charlen_t len_norm) __attribute__((alias("flexiblas_real_slangt_")));
#else
float flexiblas_real_slangt(void* norm, void* n, void* dl, void* d, void* du, flexiblas_fortran_charlen_t len_norm){return flexiblas_real_slangt_((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du, (flexiblas_fortran_charlen_t) len_norm);}
#endif




/* Chainloader for Hooks */


float flexiblas_chain_slangt_(void* norm, void* n, void* dl, void* d, void* du, flexiblas_fortran_charlen_t len_norm)
{
	float (*fn) (void* norm, void* n, void* dl, void* d, void* du, flexiblas_fortran_charlen_t len_norm);
	float (*fn_hook) (void* norm, void* n, void* dl, void* d, void* du, flexiblas_fortran_charlen_t len_norm);
	float ret;

	*(void **) &fn      = current_backend->lapack.slangt.f77_blas_function; 

    hook_pos_slangt ++;
    if( hook_pos_slangt < __flexiblas_hooks->slangt.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->slangt.f77_hook_function[hook_pos_slangt];
        ret = fn_hook((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du, ( flexiblas_fortran_charlen_t )len_norm);
    } else {
        hook_pos_slangt = 0;
		ret = fn((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du, ( flexiblas_fortran_charlen_t ) len_norm); 
	}
	return ret ;
}
#ifndef __APPLE__
float flexiblas_chain_slangt(void* norm, void* n, void* dl, void* d, void* du, flexiblas_fortran_charlen_t len_norm) __attribute__((alias("flexiblas_chain_slangt_")));
#else
float flexiblas_chain_slangt(void* norm, void* n, void* dl, void* d, void* du, flexiblas_fortran_charlen_t len_norm){return flexiblas_chain_slangt_((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du, (flexiblas_fortran_charlen_t) len_norm);}
#endif



