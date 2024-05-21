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


static TLS_STORE uint8_t hook_pos_zlaev2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zlaev2,ZLAEV2)(double complex* a, double complex* b, double complex* c, double* rt1, double* rt2, double* cs1, double complex* sn1)
#else
void FC_GLOBAL(zlaev2,ZLAEV2)(double complex* a, double complex* b, double complex* c, double* rt1, double* rt2, double* cs1, double complex* sn1)
#endif
{
	void (*fn) (void* a, void* b, void* c, void* rt1, void* rt2, void* cs1, void* sn1);
	void (*fn_hook) (void* a, void* b, void* c, void* rt1, void* rt2, void* cs1, void* sn1);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.zlaev2.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->zlaev2.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) cs1, (void*) sn1); 
		return;
	} else {
		hook_pos_zlaev2 = 0;
		fn_hook((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) cs1, (void*) sn1);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void zlaev2_(double complex* a, double complex* b, double complex* c, double* rt1, double* rt2, double* cs1, double complex* sn1) __attribute__((alias(MTS(FC_GLOBAL(zlaev2,ZLAEV2)))));
#else
#ifndef __APPLE__
void zlaev2(double complex* a, double complex* b, double complex* c, double* rt1, double* rt2, double* cs1, double complex* sn1) __attribute__((alias(MTS(FC_GLOBAL(zlaev2,ZLAEV2)))));
#else
void zlaev2(double complex* a, double complex* b, double complex* c, double* rt1, double* rt2, double* cs1, double complex* sn1){ FC_GLOBAL(zlaev2,ZLAEV2)((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) cs1, (void*) sn1); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zlaev2_(void* a, void* b, void* c, void* rt1, void* rt2, void* cs1, void* sn1)
{
	void (*fn) (void* a, void* b, void* c, void* rt1, void* rt2, void* cs1, void* sn1);

	*(void **) & fn = current_backend->lapack.zlaev2.f77_blas_function; 

		fn((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) cs1, (void*) sn1); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_zlaev2(void* a, void* b, void* c, void* rt1, void* rt2, void* cs1, void* sn1) __attribute__((alias("flexiblas_real_zlaev2_")));
#else
void flexiblas_real_zlaev2(void* a, void* b, void* c, void* rt1, void* rt2, void* cs1, void* sn1){flexiblas_real_zlaev2_((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) cs1, (void*) sn1);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zlaev2_(void* a, void* b, void* c, void* rt1, void* rt2, void* cs1, void* sn1)
{
	void (*fn) (void* a, void* b, void* c, void* rt1, void* rt2, void* cs1, void* sn1);
	void (*fn_hook) (void* a, void* b, void* c, void* rt1, void* rt2, void* cs1, void* sn1);

	*(void **) &fn      = current_backend->lapack.zlaev2.f77_blas_function; 

    hook_pos_zlaev2 ++;
    if( hook_pos_zlaev2 < __flexiblas_hooks->zlaev2.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zlaev2.f77_hook_function[hook_pos_zlaev2];
        fn_hook((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) cs1, (void*) sn1);
    } else {
        hook_pos_zlaev2 = 0;
		fn((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) cs1, (void*) sn1); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_zlaev2(void* a, void* b, void* c, void* rt1, void* rt2, void* cs1, void* sn1) __attribute__((alias("flexiblas_chain_zlaev2_")));
#else
void flexiblas_chain_zlaev2(void* a, void* b, void* c, void* rt1, void* rt2, void* cs1, void* sn1){flexiblas_chain_zlaev2_((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) cs1, (void*) sn1);}
#endif



