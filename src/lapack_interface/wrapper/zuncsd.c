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



static TLS_STORE uint8_t hook_pos_zuncsd = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zuncsd,ZUNCSD)(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, char* signs, blasint* m, blasint* p, blasint* q, double complex* x11, blasint* ldx11, double complex* x12, blasint* ldx12, double complex* x21, blasint* ldx21, double complex* x22, blasint* ldx22, double* theta, double complex* u1, blasint* ldu1, double complex* u2, blasint* ldu2, double complex* v1t, blasint* ldv1t, double complex* v2t, blasint* ldv2t, double complex* work, blasint* lwork, double* rwork, blasint* lrwork, blasint* iwork, blasint* info)
#else
void FC_GLOBAL(zuncsd,ZUNCSD)(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, char* signs, blasint* m, blasint* p, blasint* q, double complex* x11, blasint* ldx11, double complex* x12, blasint* ldx12, double complex* x21, blasint* ldx21, double complex* x22, blasint* ldx22, double* theta, double complex* u1, blasint* ldu1, double complex* u2, blasint* ldu2, double complex* v1t, blasint* ldv1t, double complex* v2t, blasint* ldv2t, double complex* work, blasint* lwork, double* rwork, blasint* lrwork, blasint* iwork, blasint* info)
#endif
{
	void (*fn) (void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* signs, void* m, void* p, void* q, void* x11, void* ldx11, void* x12, void* ldx12, void* x21, void* ldx21, void* x22, void* ldx22, void* theta, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* work, void* lwork, void* rwork, void* lrwork, void* iwork, void* info);
	void (*fn_hook) (void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* signs, void* m, void* p, void* q, void* x11, void* ldx11, void* x12, void* ldx12, void* x21, void* ldx21, void* x22, void* ldx22, void* theta, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* work, void* lwork, void* rwork, void* lrwork, void* iwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.zuncsd.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->zuncsd.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) signs, (void*) m, (void*) p, (void*) q, (void*) x11, (void*) ldx11, (void*) x12, (void*) ldx12, (void*) x21, (void*) ldx21, (void*) x22, (void*) ldx22, (void*) theta, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) iwork, (void*) info); 
		return;
	} else {
		hook_pos_zuncsd = 0;
		fn_hook((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) signs, (void*) m, (void*) p, (void*) q, (void*) x11, (void*) ldx11, (void*) x12, (void*) ldx12, (void*) x21, (void*) ldx21, (void*) x22, (void*) ldx22, (void*) theta, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) iwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void zuncsd_(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, char* signs, blasint* m, blasint* p, blasint* q, double complex* x11, blasint* ldx11, double complex* x12, blasint* ldx12, double complex* x21, blasint* ldx21, double complex* x22, blasint* ldx22, double* theta, double complex* u1, blasint* ldu1, double complex* u2, blasint* ldu2, double complex* v1t, blasint* ldv1t, double complex* v2t, blasint* ldv2t, double complex* work, blasint* lwork, double* rwork, blasint* lrwork, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(zuncsd,ZUNCSD)))));
#else
#ifndef __APPLE__
void zuncsd(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, char* signs, blasint* m, blasint* p, blasint* q, double complex* x11, blasint* ldx11, double complex* x12, blasint* ldx12, double complex* x21, blasint* ldx21, double complex* x22, blasint* ldx22, double* theta, double complex* u1, blasint* ldu1, double complex* u2, blasint* ldu2, double complex* v1t, blasint* ldv1t, double complex* v2t, blasint* ldv2t, double complex* work, blasint* lwork, double* rwork, blasint* lrwork, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(zuncsd,ZUNCSD)))));
#else
void zuncsd(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, char* signs, blasint* m, blasint* p, blasint* q, double complex* x11, blasint* ldx11, double complex* x12, blasint* ldx12, double complex* x21, blasint* ldx21, double complex* x22, blasint* ldx22, double* theta, double complex* u1, blasint* ldu1, double complex* u2, blasint* ldu2, double complex* v1t, blasint* ldv1t, double complex* v2t, blasint* ldv2t, double complex* work, blasint* lwork, double* rwork, blasint* lrwork, blasint* iwork, blasint* info){ FC_GLOBAL(zuncsd,ZUNCSD)((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) signs, (void*) m, (void*) p, (void*) q, (void*) x11, (void*) ldx11, (void*) x12, (void*) ldx12, (void*) x21, (void*) ldx21, (void*) x22, (void*) ldx22, (void*) theta, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) iwork, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zuncsd_(void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* signs, void* m, void* p, void* q, void* x11, void* ldx11, void* x12, void* ldx12, void* x21, void* ldx21, void* x22, void* ldx22, void* theta, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* work, void* lwork, void* rwork, void* lrwork, void* iwork, void* info)
{
	void (*fn) (void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* signs, void* m, void* p, void* q, void* x11, void* ldx11, void* x12, void* ldx12, void* x21, void* ldx21, void* x22, void* ldx22, void* theta, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* work, void* lwork, void* rwork, void* lrwork, void* iwork, void* info);

	*(void **) & fn = current_backend->lapack.zuncsd.f77_blas_function; 

		fn((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) signs, (void*) m, (void*) p, (void*) q, (void*) x11, (void*) ldx11, (void*) x12, (void*) ldx12, (void*) x21, (void*) ldx21, (void*) x22, (void*) ldx22, (void*) theta, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) iwork, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_zuncsd(void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* signs, void* m, void* p, void* q, void* x11, void* ldx11, void* x12, void* ldx12, void* x21, void* ldx21, void* x22, void* ldx22, void* theta, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* work, void* lwork, void* rwork, void* lrwork, void* iwork, void* info) __attribute__((alias("flexiblas_real_zuncsd_")));
#else
void flexiblas_real_zuncsd(void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* signs, void* m, void* p, void* q, void* x11, void* ldx11, void* x12, void* ldx12, void* x21, void* ldx21, void* x22, void* ldx22, void* theta, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* work, void* lwork, void* rwork, void* lrwork, void* iwork, void* info){flexiblas_real_zuncsd_((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) signs, (void*) m, (void*) p, (void*) q, (void*) x11, (void*) ldx11, (void*) x12, (void*) ldx12, (void*) x21, (void*) ldx21, (void*) x22, (void*) ldx22, (void*) theta, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) iwork, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zuncsd_(void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* signs, void* m, void* p, void* q, void* x11, void* ldx11, void* x12, void* ldx12, void* x21, void* ldx21, void* x22, void* ldx22, void* theta, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* work, void* lwork, void* rwork, void* lrwork, void* iwork, void* info)
{
	void (*fn) (void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* signs, void* m, void* p, void* q, void* x11, void* ldx11, void* x12, void* ldx12, void* x21, void* ldx21, void* x22, void* ldx22, void* theta, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* work, void* lwork, void* rwork, void* lrwork, void* iwork, void* info);
	void (*fn_hook) (void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* signs, void* m, void* p, void* q, void* x11, void* ldx11, void* x12, void* ldx12, void* x21, void* ldx21, void* x22, void* ldx22, void* theta, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* work, void* lwork, void* rwork, void* lrwork, void* iwork, void* info);

	*(void **) &fn      = current_backend->lapack.zuncsd.f77_blas_function; 

    hook_pos_zuncsd ++;
    if( hook_pos_zuncsd < __flexiblas_hooks->zuncsd.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zuncsd.f77_hook_function[hook_pos_zuncsd];
        fn_hook((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) signs, (void*) m, (void*) p, (void*) q, (void*) x11, (void*) ldx11, (void*) x12, (void*) ldx12, (void*) x21, (void*) ldx21, (void*) x22, (void*) ldx22, (void*) theta, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) iwork, (void*) info);
    } else {
        hook_pos_zuncsd = 0;
		fn((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) signs, (void*) m, (void*) p, (void*) q, (void*) x11, (void*) ldx11, (void*) x12, (void*) ldx12, (void*) x21, (void*) ldx21, (void*) x22, (void*) ldx22, (void*) theta, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) iwork, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_zuncsd(void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* signs, void* m, void* p, void* q, void* x11, void* ldx11, void* x12, void* ldx12, void* x21, void* ldx21, void* x22, void* ldx22, void* theta, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* work, void* lwork, void* rwork, void* lrwork, void* iwork, void* info) __attribute__((alias("flexiblas_chain_zuncsd_")));
#else
void flexiblas_chain_zuncsd(void* jobu1, void* jobu2, void* jobv1t, void* jobv2t, void* trans, void* signs, void* m, void* p, void* q, void* x11, void* ldx11, void* x12, void* ldx12, void* x21, void* ldx21, void* x22, void* ldx22, void* theta, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* v2t, void* ldv2t, void* work, void* lwork, void* rwork, void* lrwork, void* iwork, void* info){flexiblas_chain_zuncsd_((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) jobv2t, (void*) trans, (void*) signs, (void*) m, (void*) p, (void*) q, (void*) x11, (void*) ldx11, (void*) x12, (void*) ldx12, (void*) x21, (void*) ldx21, (void*) x22, (void*) ldx22, (void*) theta, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) v2t, (void*) ldv2t, (void*) work, (void*) lwork, (void*) rwork, (void*) lrwork, (void*) iwork, (void*) info);}
#endif



