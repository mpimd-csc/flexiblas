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



static TLS_STORE uint8_t hook_pos_sorcsd2by1 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(sorcsd2by1,SORCSD2BY1)(char* jobu1, char* jobu2, char* jobv1t, blasint* m, blasint* p, blasint* q, float* x11, blasint* ldx11, float* x21, blasint* ldx21, float* theta, float* u1, blasint* ldu1, float* u2, blasint* ldu2, float* v1t, blasint* ldv1t, float* work, blasint* lwork, blasint* iwork, blasint* info)
#else
void FC_GLOBAL(sorcsd2by1,SORCSD2BY1)(char* jobu1, char* jobu2, char* jobv1t, blasint* m, blasint* p, blasint* q, float* x11, blasint* ldx11, float* x21, blasint* ldx21, float* theta, float* u1, blasint* ldu1, float* u2, blasint* ldu2, float* v1t, blasint* ldv1t, float* work, blasint* lwork, blasint* iwork, blasint* info)
#endif
{
	void (*fn) (void* jobu1, void* jobu2, void* jobv1t, void* m, void* p, void* q, void* x11, void* ldx11, void* x21, void* ldx21, void* theta, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* work, void* lwork, void* iwork, void* info);
	void (*fn_hook) (void* jobu1, void* jobu2, void* jobv1t, void* m, void* p, void* q, void* x11, void* ldx11, void* x21, void* ldx21, void* theta, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* work, void* lwork, void* iwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.sorcsd2by1.f77_blas_function; 
	fn_hook = __flexiblas_hooks->sorcsd2by1.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) m, (void*) p, (void*) q, (void*) x11, (void*) ldx11, (void*) x21, (void*) ldx21, (void*) theta, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) work, (void*) lwork, (void*) iwork, (void*) info); 
		return;
	} else {
		hook_pos_sorcsd2by1 = 0;
		fn_hook((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) m, (void*) p, (void*) q, (void*) x11, (void*) ldx11, (void*) x21, (void*) ldx21, (void*) theta, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) work, (void*) lwork, (void*) iwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void sorcsd2by1_(char* jobu1, char* jobu2, char* jobv1t, blasint* m, blasint* p, blasint* q, float* x11, blasint* ldx11, float* x21, blasint* ldx21, float* theta, float* u1, blasint* ldu1, float* u2, blasint* ldu2, float* v1t, blasint* ldv1t, float* work, blasint* lwork, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(sorcsd2by1,SORCSD2BY1)))));
#else
void sorcsd2by1(char* jobu1, char* jobu2, char* jobv1t, blasint* m, blasint* p, blasint* q, float* x11, blasint* ldx11, float* x21, blasint* ldx21, float* theta, float* u1, blasint* ldu1, float* u2, blasint* ldu2, float* v1t, blasint* ldv1t, float* work, blasint* lwork, blasint* iwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(sorcsd2by1,SORCSD2BY1)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_sorcsd2by1_(void* jobu1, void* jobu2, void* jobv1t, void* m, void* p, void* q, void* x11, void* ldx11, void* x21, void* ldx21, void* theta, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* work, void* lwork, void* iwork, void* info)
{
	void (*fn) (void* jobu1, void* jobu2, void* jobv1t, void* m, void* p, void* q, void* x11, void* ldx11, void* x21, void* ldx21, void* theta, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* work, void* lwork, void* iwork, void* info);

	fn = current_backend->lapack.sorcsd2by1.f77_blas_function; 

		fn((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) m, (void*) p, (void*) q, (void*) x11, (void*) ldx11, (void*) x21, (void*) ldx21, (void*) theta, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) work, (void*) lwork, (void*) iwork, (void*) info); 

	return;
}

void flexiblas_real_sorcsd2by1(void* jobu1, void* jobu2, void* jobv1t, void* m, void* p, void* q, void* x11, void* ldx11, void* x21, void* ldx21, void* theta, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* work, void* lwork, void* iwork, void* info)  __attribute__((alias("flexiblas_real_sorcsd2by1_")));





/* Chainloader for Hooks */


void flexiblas_chain_sorcsd2by1_(void* jobu1, void* jobu2, void* jobv1t, void* m, void* p, void* q, void* x11, void* ldx11, void* x21, void* ldx21, void* theta, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* work, void* lwork, void* iwork, void* info)
{
	void (*fn) (void* jobu1, void* jobu2, void* jobv1t, void* m, void* p, void* q, void* x11, void* ldx11, void* x21, void* ldx21, void* theta, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* work, void* lwork, void* iwork, void* info);
	void (*fn_hook) (void* jobu1, void* jobu2, void* jobv1t, void* m, void* p, void* q, void* x11, void* ldx11, void* x21, void* ldx21, void* theta, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* work, void* lwork, void* iwork, void* info);

	fn      = current_backend->lapack.sorcsd2by1.f77_blas_function; 

    hook_pos_sorcsd2by1 ++;
    if( hook_pos_sorcsd2by1 < __flexiblas_hooks->sorcsd2by1.nhook) {
        fn_hook = __flexiblas_hooks->sorcsd2by1.f77_hook_function[hook_pos_sorcsd2by1];
        fn_hook((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) m, (void*) p, (void*) q, (void*) x11, (void*) ldx11, (void*) x21, (void*) ldx21, (void*) theta, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) work, (void*) lwork, (void*) iwork, (void*) info);
    } else {
        hook_pos_sorcsd2by1 = 0;
		fn((void*) jobu1, (void*) jobu2, (void*) jobv1t, (void*) m, (void*) p, (void*) q, (void*) x11, (void*) ldx11, (void*) x21, (void*) ldx21, (void*) theta, (void*) u1, (void*) ldu1, (void*) u2, (void*) ldu2, (void*) v1t, (void*) ldv1t, (void*) work, (void*) lwork, (void*) iwork, (void*) info); 
	}
	return;
}

void flexiblas_chain_sorcsd2by1(void* jobu1, void* jobu2, void* jobv1t, void* m, void* p, void* q, void* x11, void* ldx11, void* x21, void* ldx21, void* theta, void* u1, void* ldu1, void* u2, void* ldu2, void* v1t, void* ldv1t, void* work, void* lwork, void* iwork, void* info)  __attribute__((alias("flexiblas_chain_sorcsd2by1_")));




