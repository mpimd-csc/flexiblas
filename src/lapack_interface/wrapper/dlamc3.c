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


static TLS_STORE uint8_t hook_pos_dlamc3 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
double FC_GLOBAL(dlamc3,DLAMC3)(double* a, double* b)
#else
double FC_GLOBAL(dlamc3,DLAMC3)(double* a, double* b)
#endif
{
	double (*fn) (void* a, void* b);
	double (*fn_hook) (void* a, void* b);
	double ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dlamc3.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dlamc3.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		ret = fn((void*) a, (void*) b); 
		return ret; 
	} else {
		hook_pos_dlamc3 = 0;
		ret=fn_hook((void*) a, (void*) b);
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
double dlamc3_(double* a, double* b) __attribute__((alias(MTS(FC_GLOBAL(dlamc3,DLAMC3)))));
#else
#ifndef __APPLE__
double dlamc3(double* a, double* b) __attribute__((alias(MTS(FC_GLOBAL(dlamc3,DLAMC3)))));
#else
double dlamc3(double* a, double* b){ return FC_GLOBAL(dlamc3,DLAMC3)((void*) a, (void*) b); }
#endif
#endif




/* Real Implementation for Hooks */


double flexiblas_real_dlamc3_(void* a, void* b)
{
	double (*fn) (void* a, void* b);
	double ret;

	*(void **) & fn = current_backend->lapack.dlamc3.f77_blas_function; 

		ret = fn((void*) a, (void*) b); 

	return ret ;
}
#ifndef __APPLE__
double flexiblas_real_dlamc3(void* a, void* b) __attribute__((alias("flexiblas_real_dlamc3_")));
#else
double flexiblas_real_dlamc3(void* a, void* b){return flexiblas_real_dlamc3_((void*) a, (void*) b);}
#endif




/* Chainloader for Hooks */


double flexiblas_chain_dlamc3_(void* a, void* b)
{
	double (*fn) (void* a, void* b);
	double (*fn_hook) (void* a, void* b);
	double ret;

	*(void **) &fn      = current_backend->lapack.dlamc3.f77_blas_function; 

    hook_pos_dlamc3 ++;
    if( hook_pos_dlamc3 < __flexiblas_hooks->dlamc3.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlamc3.f77_hook_function[hook_pos_dlamc3];
        ret = fn_hook((void*) a, (void*) b);
    } else {
        hook_pos_dlamc3 = 0;
		ret = fn((void*) a, (void*) b); 
	}
	return ret ;
}
#ifndef __APPLE__
double flexiblas_chain_dlamc3(void* a, void* b) __attribute__((alias("flexiblas_chain_dlamc3_")));
#else
double flexiblas_chain_dlamc3(void* a, void* b){return flexiblas_chain_dlamc3_((void*) a, (void*) b);}
#endif



