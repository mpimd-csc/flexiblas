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


static TLS_STORE uint8_t hook_pos_disnan = 0;
#ifdef FLEXIBLAS_ABI_INTEL
int FC_GLOBAL(disnan,DISNAN)(double* din)
#else
int FC_GLOBAL(disnan,DISNAN)(double* din)
#endif
{
	blasint (*fn) (void* din);
	blasint (*fn_hook) (void* din);
	blasint ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.disnan.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->disnan.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		ret = fn((void*) din); 
		return ret; 
	} else {
		hook_pos_disnan = 0;
		ret=fn_hook((void*) din);
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
int disnan_(double* din) __attribute__((alias(MTS(FC_GLOBAL(disnan,DISNAN)))));
#else
#ifndef __APPLE__
int disnan(double* din) __attribute__((alias(MTS(FC_GLOBAL(disnan,DISNAN)))));
#else
int disnan(double* din){ return FC_GLOBAL(disnan,DISNAN)((void*) din); }
#endif
#endif




/* Real Implementation for Hooks */


blasint flexiblas_real_disnan_(void* din)
{
	blasint (*fn) (void* din);
	blasint ret;

	*(void **) & fn = current_backend->lapack.disnan.f77_blas_function; 

		ret = fn((void*) din); 

	return ret ;
}
#ifndef __APPLE__
blasint flexiblas_real_disnan(void* din) __attribute__((alias("flexiblas_real_disnan_")));
#else
blasint flexiblas_real_disnan(void* din){return flexiblas_real_disnan_((void*) din);}
#endif




/* Chainloader for Hooks */


blasint flexiblas_chain_disnan_(void* din)
{
	blasint (*fn) (void* din);
	blasint (*fn_hook) (void* din);
	blasint ret;

	*(void **) &fn      = current_backend->lapack.disnan.f77_blas_function; 

    hook_pos_disnan ++;
    if( hook_pos_disnan < __flexiblas_hooks->disnan.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->disnan.f77_hook_function[hook_pos_disnan];
        ret = fn_hook((void*) din);
    } else {
        hook_pos_disnan = 0;
		ret = fn((void*) din); 
	}
	return ret ;
}
#ifndef __APPLE__
blasint flexiblas_chain_disnan(void* din) __attribute__((alias("flexiblas_chain_disnan_")));
#else
blasint flexiblas_chain_disnan(void* din){return flexiblas_chain_disnan_((void*) din);}
#endif



