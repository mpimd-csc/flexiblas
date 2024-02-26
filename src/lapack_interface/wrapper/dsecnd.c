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



static TLS_STORE uint8_t hook_pos_dsecnd = 0;
#ifdef FLEXIBLAS_ABI_INTEL
double FC_GLOBAL(dsecnd,DSECND)(void)
#else
double FC_GLOBAL(dsecnd,DSECND)(void)
#endif
{
	double (*fn) (void);
	double (*fn_hook) (void);
	double ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dsecnd.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dsecnd.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		ret = fn(); 
		return ret; 
	} else {
		hook_pos_dsecnd = 0;
		ret=fn_hook();
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
double dsecnd_(void) __attribute__((alias(MTS(FC_GLOBAL(dsecnd,DSECND)))));
#else
#ifndef __APPLE__
double dsecnd(void) __attribute__((alias(MTS(FC_GLOBAL(dsecnd,DSECND)))));
#else
double dsecnd(void){ return FC_GLOBAL(dsecnd,DSECND)(); }
#endif
#endif




/* Real Implementation for Hooks */


double flexiblas_real_dsecnd_(void)
{
	double (*fn) (void);
	double ret;

	*(void **) & fn = current_backend->lapack.dsecnd.f77_blas_function; 

		ret = fn(); 

	return ret ;
}
#ifndef __APPLE__
double flexiblas_real_dsecnd(void) __attribute__((alias("flexiblas_real_dsecnd_")));
#else
double flexiblas_real_dsecnd(void){return flexiblas_real_dsecnd_();}
#endif




/* Chainloader for Hooks */


double flexiblas_chain_dsecnd_(void)
{
	double (*fn) (void);
	double (*fn_hook) (void);
	double ret;

	*(void **) &fn      = current_backend->lapack.dsecnd.f77_blas_function; 

    hook_pos_dsecnd ++;
    if( hook_pos_dsecnd < __flexiblas_hooks->dsecnd.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dsecnd.f77_hook_function[hook_pos_dsecnd];
        ret = fn_hook();
    } else {
        hook_pos_dsecnd = 0;
		ret = fn(); 
	}
	return ret ;
}
#ifndef __APPLE__
double flexiblas_chain_dsecnd(void) __attribute__((alias("flexiblas_chain_dsecnd_")));
#else
double flexiblas_chain_dsecnd(void){return flexiblas_chain_dsecnd_();}
#endif



