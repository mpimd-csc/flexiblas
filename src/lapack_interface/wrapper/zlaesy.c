/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) Martin Koehler, 2013-2020
 */
 /* This file it automatically generated. Please do not edit. */
 /* Generated: Wed Mar 28 11:20:05 2018 */
        
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <complex.h>

#include "fortran_mangle.h"

#include "flexiblas.h"


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
	fn = current_backend->lapack.zlaesy.f77_blas_function; 
	fn_hook = __flexiblas_hooks->zlaesy.f77_hook_function[0]; 
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
void zlaesy(double complex* a, double complex* b, double complex* c, double complex* rt1, double complex* rt2, double complex* evscal, double complex* cs1, double complex* sn1) __attribute__((alias(MTS(FC_GLOBAL(zlaesy,ZLAESY)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zlaesy_(void* a, void* b, void* c, void* rt1, void* rt2, void* evscal, void* cs1, void* sn1)
{
	void (*fn) (void* a, void* b, void* c, void* rt1, void* rt2, void* evscal, void* cs1, void* sn1);

	fn = current_backend->lapack.zlaesy.f77_blas_function; 

		fn((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) evscal, (void*) cs1, (void*) sn1); 

	return;
}

void flexiblas_real_zlaesy(void* a, void* b, void* c, void* rt1, void* rt2, void* evscal, void* cs1, void* sn1)  __attribute__((alias("flexiblas_real_zlaesy_")));





/* Chainloader for Hooks */


void flexiblas_chain_zlaesy_(void* a, void* b, void* c, void* rt1, void* rt2, void* evscal, void* cs1, void* sn1)
{
	void (*fn) (void* a, void* b, void* c, void* rt1, void* rt2, void* evscal, void* cs1, void* sn1);
	void (*fn_hook) (void* a, void* b, void* c, void* rt1, void* rt2, void* evscal, void* cs1, void* sn1);

	fn      = current_backend->lapack.zlaesy.f77_blas_function; 

    hook_pos_zlaesy ++;
    if( hook_pos_zlaesy < __flexiblas_hooks->zlaesy.nhook) {
        fn_hook = __flexiblas_hooks->zlaesy.f77_hook_function[hook_pos_zlaesy];
        fn_hook((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) evscal, (void*) cs1, (void*) sn1);
    } else {
        hook_pos_zlaesy = 0;
		fn((void*) a, (void*) b, (void*) c, (void*) rt1, (void*) rt2, (void*) evscal, (void*) cs1, (void*) sn1); 
	}
	return;
}

void flexiblas_chain_zlaesy(void* a, void* b, void* c, void* rt1, void* rt2, void* evscal, void* cs1, void* sn1)  __attribute__((alias("flexiblas_chain_zlaesy_")));




