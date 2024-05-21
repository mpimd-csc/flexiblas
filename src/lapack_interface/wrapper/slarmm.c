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


static TLS_STORE uint8_t hook_pos_slarmm = 0;
#ifdef FLEXIBLAS_ABI_INTEL
float FC_GLOBAL(slarmm,SLARMM)(float* anorm, float* bnorm, float* cnorm)
#else
float FC_GLOBAL(slarmm,SLARMM)(float* anorm, float* bnorm, float* cnorm)
#endif
{
	float (*fn) (void* anorm, void* bnorm, void* cnorm);
	float (*fn_hook) (void* anorm, void* bnorm, void* cnorm);
	float ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.slarmm.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->slarmm.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		ret = fn((void*) anorm, (void*) bnorm, (void*) cnorm); 
		return ret; 
	} else {
		hook_pos_slarmm = 0;
		ret=fn_hook((void*) anorm, (void*) bnorm, (void*) cnorm);
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
float slarmm_(float* anorm, float* bnorm, float* cnorm) __attribute__((alias(MTS(FC_GLOBAL(slarmm,SLARMM)))));
#else
#ifndef __APPLE__
float slarmm(float* anorm, float* bnorm, float* cnorm) __attribute__((alias(MTS(FC_GLOBAL(slarmm,SLARMM)))));
#else
float slarmm(float* anorm, float* bnorm, float* cnorm){ return FC_GLOBAL(slarmm,SLARMM)((void*) anorm, (void*) bnorm, (void*) cnorm); }
#endif
#endif




/* Real Implementation for Hooks */


float flexiblas_real_slarmm_(void* anorm, void* bnorm, void* cnorm)
{
	float (*fn) (void* anorm, void* bnorm, void* cnorm);
	float ret;

	*(void **) & fn = current_backend->lapack.slarmm.f77_blas_function; 

		ret = fn((void*) anorm, (void*) bnorm, (void*) cnorm); 

	return ret ;
}
#ifndef __APPLE__
float flexiblas_real_slarmm(void* anorm, void* bnorm, void* cnorm) __attribute__((alias("flexiblas_real_slarmm_")));
#else
float flexiblas_real_slarmm(void* anorm, void* bnorm, void* cnorm){return flexiblas_real_slarmm_((void*) anorm, (void*) bnorm, (void*) cnorm);}
#endif




/* Chainloader for Hooks */


float flexiblas_chain_slarmm_(void* anorm, void* bnorm, void* cnorm)
{
	float (*fn) (void* anorm, void* bnorm, void* cnorm);
	float (*fn_hook) (void* anorm, void* bnorm, void* cnorm);
	float ret;

	*(void **) &fn      = current_backend->lapack.slarmm.f77_blas_function; 

    hook_pos_slarmm ++;
    if( hook_pos_slarmm < __flexiblas_hooks->slarmm.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->slarmm.f77_hook_function[hook_pos_slarmm];
        ret = fn_hook((void*) anorm, (void*) bnorm, (void*) cnorm);
    } else {
        hook_pos_slarmm = 0;
		ret = fn((void*) anorm, (void*) bnorm, (void*) cnorm); 
	}
	return ret ;
}
#ifndef __APPLE__
float flexiblas_chain_slarmm(void* anorm, void* bnorm, void* cnorm) __attribute__((alias("flexiblas_chain_slarmm_")));
#else
float flexiblas_chain_slarmm(void* anorm, void* bnorm, void* cnorm){return flexiblas_chain_slarmm_((void*) anorm, (void*) bnorm, (void*) cnorm);}
#endif



