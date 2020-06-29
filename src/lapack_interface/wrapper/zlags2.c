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



static TLS_STORE uint8_t hook_pos_zlags2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zlags2,ZLAGS2)(blasint* upper, double* a1, double complex* a2, double* a3, double* b1, double complex* b2, double* b3, double* csu, double complex* snu, double* csv, double complex* snv, double* csq, double complex* snq)
#else
void FC_GLOBAL(zlags2,ZLAGS2)(blasint* upper, double* a1, double complex* a2, double* a3, double* b1, double complex* b2, double* b3, double* csu, double complex* snu, double* csv, double complex* snv, double* csq, double complex* snq)
#endif
{
	void (*fn) (void* upper, void* a1, void* a2, void* a3, void* b1, void* b2, void* b3, void* csu, void* snu, void* csv, void* snv, void* csq, void* snq);
	void (*fn_hook) (void* upper, void* a1, void* a2, void* a3, void* b1, void* b2, void* b3, void* csu, void* snu, void* csv, void* snv, void* csq, void* snq);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.zlags2.f77_blas_function; 
	fn_hook = __flexiblas_hooks->zlags2.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) upper, (void*) a1, (void*) a2, (void*) a3, (void*) b1, (void*) b2, (void*) b3, (void*) csu, (void*) snu, (void*) csv, (void*) snv, (void*) csq, (void*) snq); 
		return;
	} else {
		hook_pos_zlags2 = 0;
		fn_hook((void*) upper, (void*) a1, (void*) a2, (void*) a3, (void*) b1, (void*) b2, (void*) b3, (void*) csu, (void*) snu, (void*) csv, (void*) snv, (void*) csq, (void*) snq);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void zlags2_(blasint* upper, double* a1, double complex* a2, double* a3, double* b1, double complex* b2, double* b3, double* csu, double complex* snu, double* csv, double complex* snv, double* csq, double complex* snq) __attribute__((alias(MTS(FC_GLOBAL(zlags2,ZLAGS2)))));
#else
void zlags2(blasint* upper, double* a1, double complex* a2, double* a3, double* b1, double complex* b2, double* b3, double* csu, double complex* snu, double* csv, double complex* snv, double* csq, double complex* snq) __attribute__((alias(MTS(FC_GLOBAL(zlags2,ZLAGS2)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zlags2_(void* upper, void* a1, void* a2, void* a3, void* b1, void* b2, void* b3, void* csu, void* snu, void* csv, void* snv, void* csq, void* snq)
{
	void (*fn) (void* upper, void* a1, void* a2, void* a3, void* b1, void* b2, void* b3, void* csu, void* snu, void* csv, void* snv, void* csq, void* snq);

	fn = current_backend->lapack.zlags2.f77_blas_function; 

		fn((void*) upper, (void*) a1, (void*) a2, (void*) a3, (void*) b1, (void*) b2, (void*) b3, (void*) csu, (void*) snu, (void*) csv, (void*) snv, (void*) csq, (void*) snq); 

	return;
}

void flexiblas_real_zlags2(void* upper, void* a1, void* a2, void* a3, void* b1, void* b2, void* b3, void* csu, void* snu, void* csv, void* snv, void* csq, void* snq)  __attribute__((alias("flexiblas_real_zlags2_")));





/* Chainloader for Hooks */


void flexiblas_chain_zlags2_(void* upper, void* a1, void* a2, void* a3, void* b1, void* b2, void* b3, void* csu, void* snu, void* csv, void* snv, void* csq, void* snq)
{
	void (*fn) (void* upper, void* a1, void* a2, void* a3, void* b1, void* b2, void* b3, void* csu, void* snu, void* csv, void* snv, void* csq, void* snq);
	void (*fn_hook) (void* upper, void* a1, void* a2, void* a3, void* b1, void* b2, void* b3, void* csu, void* snu, void* csv, void* snv, void* csq, void* snq);

	fn      = current_backend->lapack.zlags2.f77_blas_function; 

    hook_pos_zlags2 ++;
    if( hook_pos_zlags2 < __flexiblas_hooks->zlags2.nhook) {
        fn_hook = __flexiblas_hooks->zlags2.f77_hook_function[hook_pos_zlags2];
        fn_hook((void*) upper, (void*) a1, (void*) a2, (void*) a3, (void*) b1, (void*) b2, (void*) b3, (void*) csu, (void*) snu, (void*) csv, (void*) snv, (void*) csq, (void*) snq);
    } else {
        hook_pos_zlags2 = 0;
		fn((void*) upper, (void*) a1, (void*) a2, (void*) a3, (void*) b1, (void*) b2, (void*) b3, (void*) csu, (void*) snu, (void*) csv, (void*) snv, (void*) csq, (void*) snq); 
	}
	return;
}

void flexiblas_chain_zlags2(void* upper, void* a1, void* a2, void* a3, void* b1, void* b2, void* b3, void* csu, void* snu, void* csv, void* snv, void* csq, void* snq)  __attribute__((alias("flexiblas_chain_zlags2_")));




