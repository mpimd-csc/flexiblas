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
 /* Generated: Wed Mar 28 11:20:04 2018 */
        
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



static TLS_STORE uint8_t hook_pos_sorbdb5 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(sorbdb5,SORBDB5)(blasint* m1, blasint* m2, blasint* n, float* x1, blasint* incx1, float* x2, blasint* incx2, float* q1, blasint* ldq1, float* q2, blasint* ldq2, float* work, blasint* lwork, blasint* info)
#else
void FC_GLOBAL(sorbdb5,SORBDB5)(blasint* m1, blasint* m2, blasint* n, float* x1, blasint* incx1, float* x2, blasint* incx2, float* q1, blasint* ldq1, float* q2, blasint* ldq2, float* work, blasint* lwork, blasint* info)
#endif
{
	void (*fn) (void* m1, void* m2, void* n, void* x1, void* incx1, void* x2, void* incx2, void* q1, void* ldq1, void* q2, void* ldq2, void* work, void* lwork, void* info);
	void (*fn_hook) (void* m1, void* m2, void* n, void* x1, void* incx1, void* x2, void* incx2, void* q1, void* ldq1, void* q2, void* ldq2, void* work, void* lwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.sorbdb5.f77_blas_function; 
	fn_hook = __flexiblas_hooks->sorbdb5.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) m1, (void*) m2, (void*) n, (void*) x1, (void*) incx1, (void*) x2, (void*) incx2, (void*) q1, (void*) ldq1, (void*) q2, (void*) ldq2, (void*) work, (void*) lwork, (void*) info); 
		return;
	} else {
		hook_pos_sorbdb5 = 0;
		fn_hook((void*) m1, (void*) m2, (void*) n, (void*) x1, (void*) incx1, (void*) x2, (void*) incx2, (void*) q1, (void*) ldq1, (void*) q2, (void*) ldq2, (void*) work, (void*) lwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void sorbdb5_(blasint* m1, blasint* m2, blasint* n, float* x1, blasint* incx1, float* x2, blasint* incx2, float* q1, blasint* ldq1, float* q2, blasint* ldq2, float* work, blasint* lwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(sorbdb5,SORBDB5)))));
#else
void sorbdb5(blasint* m1, blasint* m2, blasint* n, float* x1, blasint* incx1, float* x2, blasint* incx2, float* q1, blasint* ldq1, float* q2, blasint* ldq2, float* work, blasint* lwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(sorbdb5,SORBDB5)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_sorbdb5_(void* m1, void* m2, void* n, void* x1, void* incx1, void* x2, void* incx2, void* q1, void* ldq1, void* q2, void* ldq2, void* work, void* lwork, void* info)
{
	void (*fn) (void* m1, void* m2, void* n, void* x1, void* incx1, void* x2, void* incx2, void* q1, void* ldq1, void* q2, void* ldq2, void* work, void* lwork, void* info);

	fn = current_backend->lapack.sorbdb5.f77_blas_function; 

		fn((void*) m1, (void*) m2, (void*) n, (void*) x1, (void*) incx1, (void*) x2, (void*) incx2, (void*) q1, (void*) ldq1, (void*) q2, (void*) ldq2, (void*) work, (void*) lwork, (void*) info); 

	return;
}

void flexiblas_real_sorbdb5(void* m1, void* m2, void* n, void* x1, void* incx1, void* x2, void* incx2, void* q1, void* ldq1, void* q2, void* ldq2, void* work, void* lwork, void* info)  __attribute__((alias("flexiblas_real_sorbdb5_")));





/* Chainloader for Hooks */


void flexiblas_chain_sorbdb5_(void* m1, void* m2, void* n, void* x1, void* incx1, void* x2, void* incx2, void* q1, void* ldq1, void* q2, void* ldq2, void* work, void* lwork, void* info)
{
	void (*fn) (void* m1, void* m2, void* n, void* x1, void* incx1, void* x2, void* incx2, void* q1, void* ldq1, void* q2, void* ldq2, void* work, void* lwork, void* info);
	void (*fn_hook) (void* m1, void* m2, void* n, void* x1, void* incx1, void* x2, void* incx2, void* q1, void* ldq1, void* q2, void* ldq2, void* work, void* lwork, void* info);

	fn      = current_backend->lapack.sorbdb5.f77_blas_function; 

    hook_pos_sorbdb5 ++;
    if( hook_pos_sorbdb5 < __flexiblas_hooks->sorbdb5.nhook) {
        fn_hook = __flexiblas_hooks->sorbdb5.f77_hook_function[hook_pos_sorbdb5];
        fn_hook((void*) m1, (void*) m2, (void*) n, (void*) x1, (void*) incx1, (void*) x2, (void*) incx2, (void*) q1, (void*) ldq1, (void*) q2, (void*) ldq2, (void*) work, (void*) lwork, (void*) info);
    } else {
        hook_pos_sorbdb5 = 0;
		fn((void*) m1, (void*) m2, (void*) n, (void*) x1, (void*) incx1, (void*) x2, (void*) incx2, (void*) q1, (void*) ldq1, (void*) q2, (void*) ldq2, (void*) work, (void*) lwork, (void*) info); 
	}
	return;
}

void flexiblas_chain_sorbdb5(void* m1, void* m2, void* n, void* x1, void* incx1, void* x2, void* incx2, void* q1, void* ldq1, void* q2, void* ldq2, void* work, void* lwork, void* info)  __attribute__((alias("flexiblas_chain_sorbdb5_")));




