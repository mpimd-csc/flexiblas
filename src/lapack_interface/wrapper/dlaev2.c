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



static TLS_STORE uint8_t hook_pos_dlaev2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlaev2,DLAEV2)(double* a, double* b, double* c, double* rt1, double* rt2, double* cs1, double* sn1)
#else
void FC_GLOBAL(dlaev2,DLAEV2)(double* a, double* b, double* c, double* rt1, double* rt2, double* cs1, double* sn1)
#endif
{
	void (*fn) (void* a, void* b, void* c, void* rt1, void* rt2, void* cs1, void* sn1);
	void (*fn_hook) (void* a, void* b, void* c, void* rt1, void* rt2, void* cs1, void* sn1);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dlaev2.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dlaev2.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) cs1, (void*) sn1); 
		return;
	} else {
		hook_pos_dlaev2 = 0;
		fn_hook((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) cs1, (void*) sn1);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dlaev2_(double* a, double* b, double* c, double* rt1, double* rt2, double* cs1, double* sn1) __attribute__((alias(MTS(FC_GLOBAL(dlaev2,DLAEV2)))));
#else
#ifndef __APPLE__
void dlaev2(double* a, double* b, double* c, double* rt1, double* rt2, double* cs1, double* sn1) __attribute__((alias(MTS(FC_GLOBAL(dlaev2,DLAEV2)))));
#else
void dlaev2(double* a, double* b, double* c, double* rt1, double* rt2, double* cs1, double* sn1){ FC_GLOBAL(dlaev2,DLAEV2)((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) cs1, (void*) sn1); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlaev2_(void* a, void* b, void* c, void* rt1, void* rt2, void* cs1, void* sn1)
{
	void (*fn) (void* a, void* b, void* c, void* rt1, void* rt2, void* cs1, void* sn1);

	*(void **) & fn = current_backend->lapack.dlaev2.f77_blas_function; 

		fn((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) cs1, (void*) sn1); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_dlaev2(void* a, void* b, void* c, void* rt1, void* rt2, void* cs1, void* sn1) __attribute__((alias("flexiblas_real_dlaev2_")));
#else
void flexiblas_real_dlaev2(void* a, void* b, void* c, void* rt1, void* rt2, void* cs1, void* sn1){flexiblas_real_dlaev2_((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) cs1, (void*) sn1);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlaev2_(void* a, void* b, void* c, void* rt1, void* rt2, void* cs1, void* sn1)
{
	void (*fn) (void* a, void* b, void* c, void* rt1, void* rt2, void* cs1, void* sn1);
	void (*fn_hook) (void* a, void* b, void* c, void* rt1, void* rt2, void* cs1, void* sn1);

	*(void **) &fn      = current_backend->lapack.dlaev2.f77_blas_function; 

    hook_pos_dlaev2 ++;
    if( hook_pos_dlaev2 < __flexiblas_hooks->dlaev2.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlaev2.f77_hook_function[hook_pos_dlaev2];
        fn_hook((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) cs1, (void*) sn1);
    } else {
        hook_pos_dlaev2 = 0;
		fn((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) cs1, (void*) sn1); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_dlaev2(void* a, void* b, void* c, void* rt1, void* rt2, void* cs1, void* sn1) __attribute__((alias("flexiblas_chain_dlaev2_")));
#else
void flexiblas_chain_dlaev2(void* a, void* b, void* c, void* rt1, void* rt2, void* cs1, void* sn1){flexiblas_chain_dlaev2_((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) cs1, (void*) sn1);}
#endif



