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



static TLS_STORE uint8_t hook_pos_stprfb = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(stprfb,STPRFB)(char* side, char* trans, char* direct, char* storev, blasint* m, blasint* n, blasint* k, blasint* l, float* v, blasint* ldv, float* t, blasint* ldt, float* a, blasint* lda, float* b, blasint* ldb, float* work, blasint* ldwork)
#else
void FC_GLOBAL(stprfb,STPRFB)(char* side, char* trans, char* direct, char* storev, blasint* m, blasint* n, blasint* k, blasint* l, float* v, blasint* ldv, float* t, blasint* ldt, float* a, blasint* lda, float* b, blasint* ldb, float* work, blasint* ldwork)
#endif
{
	void (*fn) (void* side, void* trans, void* direct, void* storev, void* m, void* n, void* k, void* l, void* v, void* ldv, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork);
	void (*fn_hook) (void* side, void* trans, void* direct, void* storev, void* m, void* n, void* k, void* l, void* v, void* ldv, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.stprfb.f77_blas_function; 
	fn_hook = __flexiblas_hooks->stprfb.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) side, (void*) trans, (void*) direct, (void*) storev, (void*) m, (void*) n, (void*) k, (void*) l, (void*) v, (void*) ldv, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) ldwork); 
		return;
	} else {
		hook_pos_stprfb = 0;
		fn_hook((void*) side, (void*) trans, (void*) direct, (void*) storev, (void*) m, (void*) n, (void*) k, (void*) l, (void*) v, (void*) ldv, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) ldwork);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void stprfb_(char* side, char* trans, char* direct, char* storev, blasint* m, blasint* n, blasint* k, blasint* l, float* v, blasint* ldv, float* t, blasint* ldt, float* a, blasint* lda, float* b, blasint* ldb, float* work, blasint* ldwork) __attribute__((alias(MTS(FC_GLOBAL(stprfb,STPRFB)))));
#else
void stprfb(char* side, char* trans, char* direct, char* storev, blasint* m, blasint* n, blasint* k, blasint* l, float* v, blasint* ldv, float* t, blasint* ldt, float* a, blasint* lda, float* b, blasint* ldb, float* work, blasint* ldwork) __attribute__((alias(MTS(FC_GLOBAL(stprfb,STPRFB)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_stprfb_(void* side, void* trans, void* direct, void* storev, void* m, void* n, void* k, void* l, void* v, void* ldv, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork)
{
	void (*fn) (void* side, void* trans, void* direct, void* storev, void* m, void* n, void* k, void* l, void* v, void* ldv, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork);

	fn = current_backend->lapack.stprfb.f77_blas_function; 

		fn((void*) side, (void*) trans, (void*) direct, (void*) storev, (void*) m, (void*) n, (void*) k, (void*) l, (void*) v, (void*) ldv, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) ldwork); 

	return;
}

void flexiblas_real_stprfb(void* side, void* trans, void* direct, void* storev, void* m, void* n, void* k, void* l, void* v, void* ldv, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork)  __attribute__((alias("flexiblas_real_stprfb_")));





/* Chainloader for Hooks */


void flexiblas_chain_stprfb_(void* side, void* trans, void* direct, void* storev, void* m, void* n, void* k, void* l, void* v, void* ldv, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork)
{
	void (*fn) (void* side, void* trans, void* direct, void* storev, void* m, void* n, void* k, void* l, void* v, void* ldv, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork);
	void (*fn_hook) (void* side, void* trans, void* direct, void* storev, void* m, void* n, void* k, void* l, void* v, void* ldv, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork);

	fn      = current_backend->lapack.stprfb.f77_blas_function; 

    hook_pos_stprfb ++;
    if( hook_pos_stprfb < __flexiblas_hooks->stprfb.nhook) {
        fn_hook = __flexiblas_hooks->stprfb.f77_hook_function[hook_pos_stprfb];
        fn_hook((void*) side, (void*) trans, (void*) direct, (void*) storev, (void*) m, (void*) n, (void*) k, (void*) l, (void*) v, (void*) ldv, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) ldwork);
    } else {
        hook_pos_stprfb = 0;
		fn((void*) side, (void*) trans, (void*) direct, (void*) storev, (void*) m, (void*) n, (void*) k, (void*) l, (void*) v, (void*) ldv, (void*) t, (void*) ldt, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) work, (void*) ldwork); 
	}
	return;
}

void flexiblas_chain_stprfb(void* side, void* trans, void* direct, void* storev, void* m, void* n, void* k, void* l, void* v, void* ldv, void* t, void* ldt, void* a, void* lda, void* b, void* ldb, void* work, void* ldwork)  __attribute__((alias("flexiblas_chain_stprfb_")));




