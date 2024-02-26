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



static TLS_STORE uint8_t hook_pos_zlangt = 0;
#ifdef FLEXIBLAS_ABI_INTEL
double FC_GLOBAL(zlangt,ZLANGT)(char* norm, blasint* n, double complex* dl, double complex* d, double complex* du, flexiblas_fortran_charlen_t len_norm)
#else
double FC_GLOBAL(zlangt,ZLANGT)(char* norm, blasint* n, double complex* dl, double complex* d, double complex* du, flexiblas_fortran_charlen_t len_norm)
#endif
{
	double (*fn) (void* norm, void* n, void* dl, void* d, void* du, flexiblas_fortran_charlen_t len_norm);
	double (*fn_hook) (void* norm, void* n, void* dl, void* d, void* du, flexiblas_fortran_charlen_t len_norm);
	double ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.zlangt.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->zlangt.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		ret = fn((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du, ( flexiblas_fortran_charlen_t ) len_norm); 
		return ret; 
	} else {
		hook_pos_zlangt = 0;
		ret=fn_hook((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du, ( flexiblas_fortran_charlen_t ) len_norm);
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
double zlangt_(char* norm, blasint* n, double complex* dl, double complex* d, double complex* du, flexiblas_fortran_charlen_t len_norm) __attribute__((alias(MTS(FC_GLOBAL(zlangt,ZLANGT)))));
#else
#ifndef __APPLE__
double zlangt(char* norm, blasint* n, double complex* dl, double complex* d, double complex* du, flexiblas_fortran_charlen_t len_norm) __attribute__((alias(MTS(FC_GLOBAL(zlangt,ZLANGT)))));
#else
double zlangt(char* norm, blasint* n, double complex* dl, double complex* d, double complex* du, flexiblas_fortran_charlen_t len_norm){ return FC_GLOBAL(zlangt,ZLANGT)((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du, (flexiblas_fortran_charlen_t) len_norm); }
#endif
#endif




/* Real Implementation for Hooks */


double flexiblas_real_zlangt_(void* norm, void* n, void* dl, void* d, void* du, flexiblas_fortran_charlen_t len_norm)
{
	double (*fn) (void* norm, void* n, void* dl, void* d, void* du, flexiblas_fortran_charlen_t len_norm);
	double ret;

	*(void **) & fn = current_backend->lapack.zlangt.f77_blas_function; 

		ret = fn((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du, ( flexiblas_fortran_charlen_t ) len_norm); 

	return ret ;
}
#ifndef __APPLE__
double flexiblas_real_zlangt(void* norm, void* n, void* dl, void* d, void* du, flexiblas_fortran_charlen_t len_norm) __attribute__((alias("flexiblas_real_zlangt_")));
#else
double flexiblas_real_zlangt(void* norm, void* n, void* dl, void* d, void* du, flexiblas_fortran_charlen_t len_norm){return flexiblas_real_zlangt_((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du, (flexiblas_fortran_charlen_t) len_norm);}
#endif




/* Chainloader for Hooks */


double flexiblas_chain_zlangt_(void* norm, void* n, void* dl, void* d, void* du, flexiblas_fortran_charlen_t len_norm)
{
	double (*fn) (void* norm, void* n, void* dl, void* d, void* du, flexiblas_fortran_charlen_t len_norm);
	double (*fn_hook) (void* norm, void* n, void* dl, void* d, void* du, flexiblas_fortran_charlen_t len_norm);
	double ret;

	*(void **) &fn      = current_backend->lapack.zlangt.f77_blas_function; 

    hook_pos_zlangt ++;
    if( hook_pos_zlangt < __flexiblas_hooks->zlangt.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zlangt.f77_hook_function[hook_pos_zlangt];
        ret = fn_hook((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du, ( flexiblas_fortran_charlen_t )len_norm);
    } else {
        hook_pos_zlangt = 0;
		ret = fn((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du, ( flexiblas_fortran_charlen_t ) len_norm); 
	}
	return ret ;
}
#ifndef __APPLE__
double flexiblas_chain_zlangt(void* norm, void* n, void* dl, void* d, void* du, flexiblas_fortran_charlen_t len_norm) __attribute__((alias("flexiblas_chain_zlangt_")));
#else
double flexiblas_chain_zlangt(void* norm, void* n, void* dl, void* d, void* du, flexiblas_fortran_charlen_t len_norm){return flexiblas_chain_zlangt_((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du, (flexiblas_fortran_charlen_t) len_norm);}
#endif



