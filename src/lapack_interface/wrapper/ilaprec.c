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



static TLS_STORE uint8_t hook_pos_ilaprec = 0;
#ifdef FLEXIBLAS_ABI_INTEL
int FC_GLOBAL(ilaprec,ILAPREC)(char* prec)
#else
int FC_GLOBAL(ilaprec,ILAPREC)(char* prec)
#endif
{
	blasint (*fn) (void* prec);
	blasint (*fn_hook) (void* prec);
	blasint ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.ilaprec.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->ilaprec.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		ret = fn((void*) prec); 
		return ret; 
	} else {
		hook_pos_ilaprec = 0;
		ret=fn_hook((void*) prec);
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
int ilaprec_(char* prec) __attribute__((alias(MTS(FC_GLOBAL(ilaprec,ILAPREC)))));
#else
#ifndef __APPLE__
int ilaprec(char* prec) __attribute__((alias(MTS(FC_GLOBAL(ilaprec,ILAPREC)))));
#else
int ilaprec(char* prec){ return FC_GLOBAL(ilaprec,ILAPREC)((void*) prec); }
#endif
#endif




/* Real Implementation for Hooks */


blasint flexiblas_real_ilaprec_(void* prec)
{
	blasint (*fn) (void* prec);
	blasint ret;

	*(void **) & fn = current_backend->lapack.ilaprec.f77_blas_function; 

		ret = fn((void*) prec); 

	return ret ;
}
#ifndef __APPLE__
blasint flexiblas_real_ilaprec(void* prec) __attribute__((alias("flexiblas_real_ilaprec_")));
#else
blasint flexiblas_real_ilaprec(void* prec){return flexiblas_real_ilaprec_((void*) prec);}
#endif




/* Chainloader for Hooks */


blasint flexiblas_chain_ilaprec_(void* prec)
{
	blasint (*fn) (void* prec);
	blasint (*fn_hook) (void* prec);
	blasint ret;

	*(void **) &fn      = current_backend->lapack.ilaprec.f77_blas_function; 

    hook_pos_ilaprec ++;
    if( hook_pos_ilaprec < __flexiblas_hooks->ilaprec.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->ilaprec.f77_hook_function[hook_pos_ilaprec];
        ret = fn_hook((void*) prec);
    } else {
        hook_pos_ilaprec = 0;
		ret = fn((void*) prec); 
	}
	return ret ;
}
#ifndef __APPLE__
blasint flexiblas_chain_ilaprec(void* prec) __attribute__((alias("flexiblas_chain_ilaprec_")));
#else
blasint flexiblas_chain_ilaprec(void* prec){return flexiblas_chain_ilaprec_((void*) prec);}
#endif



