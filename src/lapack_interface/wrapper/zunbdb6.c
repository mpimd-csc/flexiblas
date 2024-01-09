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



static TLS_STORE uint8_t hook_pos_zunbdb6 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zunbdb6,ZUNBDB6)(blasint* m1, blasint* m2, blasint* n, double complex* x1, blasint* incx1, double complex* x2, blasint* incx2, double complex* q1, blasint* ldq1, double complex* q2, blasint* ldq2, double complex* work, blasint* lwork, blasint* info)
#else
void FC_GLOBAL(zunbdb6,ZUNBDB6)(blasint* m1, blasint* m2, blasint* n, double complex* x1, blasint* incx1, double complex* x2, blasint* incx2, double complex* q1, blasint* ldq1, double complex* q2, blasint* ldq2, double complex* work, blasint* lwork, blasint* info)
#endif
{
	void (*fn) (void* m1, void* m2, void* n, void* x1, void* incx1, void* x2, void* incx2, void* q1, void* ldq1, void* q2, void* ldq2, void* work, void* lwork, void* info);
	void (*fn_hook) (void* m1, void* m2, void* n, void* x1, void* incx1, void* x2, void* incx2, void* q1, void* ldq1, void* q2, void* ldq2, void* work, void* lwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.zunbdb6.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->zunbdb6.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) m1, (void*) m2, (void*) n, (void*) x1, (void*) incx1, (void*) x2, (void*) incx2, (void*) q1, (void*) ldq1, (void*) q2, (void*) ldq2, (void*) work, (void*) lwork, (void*) info); 
		return;
	} else {
		hook_pos_zunbdb6 = 0;
		fn_hook((void*) m1, (void*) m2, (void*) n, (void*) x1, (void*) incx1, (void*) x2, (void*) incx2, (void*) q1, (void*) ldq1, (void*) q2, (void*) ldq2, (void*) work, (void*) lwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void zunbdb6_(blasint* m1, blasint* m2, blasint* n, double complex* x1, blasint* incx1, double complex* x2, blasint* incx2, double complex* q1, blasint* ldq1, double complex* q2, blasint* ldq2, double complex* work, blasint* lwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(zunbdb6,ZUNBDB6)))));
#else
#ifndef __APPLE__
void zunbdb6(blasint* m1, blasint* m2, blasint* n, double complex* x1, blasint* incx1, double complex* x2, blasint* incx2, double complex* q1, blasint* ldq1, double complex* q2, blasint* ldq2, double complex* work, blasint* lwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(zunbdb6,ZUNBDB6)))));
#else
void zunbdb6(blasint* m1, blasint* m2, blasint* n, double complex* x1, blasint* incx1, double complex* x2, blasint* incx2, double complex* q1, blasint* ldq1, double complex* q2, blasint* ldq2, double complex* work, blasint* lwork, blasint* info){ FC_GLOBAL(zunbdb6,ZUNBDB6)((void*) m1, (void*) m2, (void*) n, (void*) x1, (void*) incx1, (void*) x2, (void*) incx2, (void*) q1, (void*) ldq1, (void*) q2, (void*) ldq2, (void*) work, (void*) lwork, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zunbdb6_(void* m1, void* m2, void* n, void* x1, void* incx1, void* x2, void* incx2, void* q1, void* ldq1, void* q2, void* ldq2, void* work, void* lwork, void* info)
{
	void (*fn) (void* m1, void* m2, void* n, void* x1, void* incx1, void* x2, void* incx2, void* q1, void* ldq1, void* q2, void* ldq2, void* work, void* lwork, void* info);

	*(void **) & fn = current_backend->lapack.zunbdb6.f77_blas_function; 

		fn((void*) m1, (void*) m2, (void*) n, (void*) x1, (void*) incx1, (void*) x2, (void*) incx2, (void*) q1, (void*) ldq1, (void*) q2, (void*) ldq2, (void*) work, (void*) lwork, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_zunbdb6(void* m1, void* m2, void* n, void* x1, void* incx1, void* x2, void* incx2, void* q1, void* ldq1, void* q2, void* ldq2, void* work, void* lwork, void* info) __attribute__((alias("flexiblas_real_zunbdb6_")));
#else
void flexiblas_real_zunbdb6(void* m1, void* m2, void* n, void* x1, void* incx1, void* x2, void* incx2, void* q1, void* ldq1, void* q2, void* ldq2, void* work, void* lwork, void* info){flexiblas_real_zunbdb6_((void*) m1, (void*) m2, (void*) n, (void*) x1, (void*) incx1, (void*) x2, (void*) incx2, (void*) q1, (void*) ldq1, (void*) q2, (void*) ldq2, (void*) work, (void*) lwork, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zunbdb6_(void* m1, void* m2, void* n, void* x1, void* incx1, void* x2, void* incx2, void* q1, void* ldq1, void* q2, void* ldq2, void* work, void* lwork, void* info)
{
	void (*fn) (void* m1, void* m2, void* n, void* x1, void* incx1, void* x2, void* incx2, void* q1, void* ldq1, void* q2, void* ldq2, void* work, void* lwork, void* info);
	void (*fn_hook) (void* m1, void* m2, void* n, void* x1, void* incx1, void* x2, void* incx2, void* q1, void* ldq1, void* q2, void* ldq2, void* work, void* lwork, void* info);

	*(void **) &fn      = current_backend->lapack.zunbdb6.f77_blas_function; 

    hook_pos_zunbdb6 ++;
    if( hook_pos_zunbdb6 < __flexiblas_hooks->zunbdb6.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zunbdb6.f77_hook_function[hook_pos_zunbdb6];
        fn_hook((void*) m1, (void*) m2, (void*) n, (void*) x1, (void*) incx1, (void*) x2, (void*) incx2, (void*) q1, (void*) ldq1, (void*) q2, (void*) ldq2, (void*) work, (void*) lwork, (void*) info);
    } else {
        hook_pos_zunbdb6 = 0;
		fn((void*) m1, (void*) m2, (void*) n, (void*) x1, (void*) incx1, (void*) x2, (void*) incx2, (void*) q1, (void*) ldq1, (void*) q2, (void*) ldq2, (void*) work, (void*) lwork, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_zunbdb6(void* m1, void* m2, void* n, void* x1, void* incx1, void* x2, void* incx2, void* q1, void* ldq1, void* q2, void* ldq2, void* work, void* lwork, void* info) __attribute__((alias("flexiblas_chain_zunbdb6_")));
#else
void flexiblas_chain_zunbdb6(void* m1, void* m2, void* n, void* x1, void* incx1, void* x2, void* incx2, void* q1, void* ldq1, void* q2, void* ldq2, void* work, void* lwork, void* info){flexiblas_chain_zunbdb6_((void*) m1, (void*) m2, (void*) n, (void*) x1, (void*) incx1, (void*) x2, (void*) incx2, (void*) q1, (void*) ldq1, (void*) q2, (void*) ldq2, (void*) work, (void*) lwork, (void*) info);}
#endif



