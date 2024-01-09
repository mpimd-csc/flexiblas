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



static TLS_STORE uint8_t hook_pos_zlaesy = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zlaesy,ZLAESY)(double complex* a, double complex* b, double complex* c, double complex* rt1, double complex* rt2, double complex* evscal, double complex* cs1, double complex* sn1)
#else
void FC_GLOBAL(zlaesy,ZLAESY)(double complex* a, double complex* b, double complex* c, double complex* rt1, double complex* rt2, double complex* evscal, double complex* cs1, double complex* sn1)
#endif
{
	void (*fn) (void* a, void* b, void* c, void* rt1, void* rt2, void* evscal, void* cs1, void* sn1);
	void (*fn_hook) (void* a, void* b, void* c, void* rt1, void* rt2, void* evscal, void* cs1, void* sn1);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.zlaesy.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->zlaesy.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) evscal, (void*) cs1, (void*) sn1); 
		return;
	} else {
		hook_pos_zlaesy = 0;
		fn_hook((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) evscal, (void*) cs1, (void*) sn1);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void zlaesy_(double complex* a, double complex* b, double complex* c, double complex* rt1, double complex* rt2, double complex* evscal, double complex* cs1, double complex* sn1) __attribute__((alias(MTS(FC_GLOBAL(zlaesy,ZLAESY)))));
#else
#ifndef __APPLE__
void zlaesy(double complex* a, double complex* b, double complex* c, double complex* rt1, double complex* rt2, double complex* evscal, double complex* cs1, double complex* sn1) __attribute__((alias(MTS(FC_GLOBAL(zlaesy,ZLAESY)))));
#else
void zlaesy(double complex* a, double complex* b, double complex* c, double complex* rt1, double complex* rt2, double complex* evscal, double complex* cs1, double complex* sn1){ FC_GLOBAL(zlaesy,ZLAESY)((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) evscal, (void*) cs1, (void*) sn1); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zlaesy_(void* a, void* b, void* c, void* rt1, void* rt2, void* evscal, void* cs1, void* sn1)
{
	void (*fn) (void* a, void* b, void* c, void* rt1, void* rt2, void* evscal, void* cs1, void* sn1);

	*(void **) & fn = current_backend->lapack.zlaesy.f77_blas_function; 

		fn((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) evscal, (void*) cs1, (void*) sn1); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_zlaesy(void* a, void* b, void* c, void* rt1, void* rt2, void* evscal, void* cs1, void* sn1) __attribute__((alias("flexiblas_real_zlaesy_")));
#else
void flexiblas_real_zlaesy(void* a, void* b, void* c, void* rt1, void* rt2, void* evscal, void* cs1, void* sn1){flexiblas_real_zlaesy_((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) evscal, (void*) cs1, (void*) sn1);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zlaesy_(void* a, void* b, void* c, void* rt1, void* rt2, void* evscal, void* cs1, void* sn1)
{
	void (*fn) (void* a, void* b, void* c, void* rt1, void* rt2, void* evscal, void* cs1, void* sn1);
	void (*fn_hook) (void* a, void* b, void* c, void* rt1, void* rt2, void* evscal, void* cs1, void* sn1);

	*(void **) &fn      = current_backend->lapack.zlaesy.f77_blas_function; 

    hook_pos_zlaesy ++;
    if( hook_pos_zlaesy < __flexiblas_hooks->zlaesy.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zlaesy.f77_hook_function[hook_pos_zlaesy];
        fn_hook((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) evscal, (void*) cs1, (void*) sn1);
    } else {
        hook_pos_zlaesy = 0;
		fn((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) evscal, (void*) cs1, (void*) sn1); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_zlaesy(void* a, void* b, void* c, void* rt1, void* rt2, void* evscal, void* cs1, void* sn1) __attribute__((alias("flexiblas_chain_zlaesy_")));
#else
void flexiblas_chain_zlaesy(void* a, void* b, void* c, void* rt1, void* rt2, void* evscal, void* cs1, void* sn1){flexiblas_chain_zlaesy_((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) evscal, (void*) cs1, (void*) sn1);}
#endif



