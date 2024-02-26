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



static TLS_STORE uint8_t hook_pos_slaev2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slaev2,SLAEV2)(float* a, float* b, float* c, float* rt1, float* rt2, float* cs1, float* sn1)
#else
void FC_GLOBAL(slaev2,SLAEV2)(float* a, float* b, float* c, float* rt1, float* rt2, float* cs1, float* sn1)
#endif
{
	void (*fn) (void* a, void* b, void* c, void* rt1, void* rt2, void* cs1, void* sn1);
	void (*fn_hook) (void* a, void* b, void* c, void* rt1, void* rt2, void* cs1, void* sn1);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.slaev2.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->slaev2.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) cs1, (void*) sn1); 
		return;
	} else {
		hook_pos_slaev2 = 0;
		fn_hook((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) cs1, (void*) sn1);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void slaev2_(float* a, float* b, float* c, float* rt1, float* rt2, float* cs1, float* sn1) __attribute__((alias(MTS(FC_GLOBAL(slaev2,SLAEV2)))));
#else
#ifndef __APPLE__
void slaev2(float* a, float* b, float* c, float* rt1, float* rt2, float* cs1, float* sn1) __attribute__((alias(MTS(FC_GLOBAL(slaev2,SLAEV2)))));
#else
void slaev2(float* a, float* b, float* c, float* rt1, float* rt2, float* cs1, float* sn1){ FC_GLOBAL(slaev2,SLAEV2)((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) cs1, (void*) sn1); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slaev2_(void* a, void* b, void* c, void* rt1, void* rt2, void* cs1, void* sn1)
{
	void (*fn) (void* a, void* b, void* c, void* rt1, void* rt2, void* cs1, void* sn1);

	*(void **) & fn = current_backend->lapack.slaev2.f77_blas_function; 

		fn((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) cs1, (void*) sn1); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_slaev2(void* a, void* b, void* c, void* rt1, void* rt2, void* cs1, void* sn1) __attribute__((alias("flexiblas_real_slaev2_")));
#else
void flexiblas_real_slaev2(void* a, void* b, void* c, void* rt1, void* rt2, void* cs1, void* sn1){flexiblas_real_slaev2_((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) cs1, (void*) sn1);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_slaev2_(void* a, void* b, void* c, void* rt1, void* rt2, void* cs1, void* sn1)
{
	void (*fn) (void* a, void* b, void* c, void* rt1, void* rt2, void* cs1, void* sn1);
	void (*fn_hook) (void* a, void* b, void* c, void* rt1, void* rt2, void* cs1, void* sn1);

	*(void **) &fn      = current_backend->lapack.slaev2.f77_blas_function; 

    hook_pos_slaev2 ++;
    if( hook_pos_slaev2 < __flexiblas_hooks->slaev2.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->slaev2.f77_hook_function[hook_pos_slaev2];
        fn_hook((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) cs1, (void*) sn1);
    } else {
        hook_pos_slaev2 = 0;
		fn((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) cs1, (void*) sn1); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_slaev2(void* a, void* b, void* c, void* rt1, void* rt2, void* cs1, void* sn1) __attribute__((alias("flexiblas_chain_slaev2_")));
#else
void flexiblas_chain_slaev2(void* a, void* b, void* c, void* rt1, void* rt2, void* cs1, void* sn1){flexiblas_chain_slaev2_((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) cs1, (void*) sn1);}
#endif



