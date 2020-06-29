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



static TLS_STORE uint8_t hook_pos_zlarzb = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zlarzb,ZLARZB)(char* side, char* trans, char* direct, char* storev, blasint* m, blasint* n, blasint* k, blasint* l, double complex* v, blasint* ldv, double complex* t, blasint* ldt, double complex* c, blasint* ldc, double complex* work, blasint* ldwork)
#else
void FC_GLOBAL(zlarzb,ZLARZB)(char* side, char* trans, char* direct, char* storev, blasint* m, blasint* n, blasint* k, blasint* l, double complex* v, blasint* ldv, double complex* t, blasint* ldt, double complex* c, blasint* ldc, double complex* work, blasint* ldwork)
#endif
{
	void (*fn) (void* side, void* trans, void* direct, void* storev, void* m, void* n, void* k, void* l, void* v, void* ldv, void* t, void* ldt, void* c, void* ldc, void* work, void* ldwork);
	void (*fn_hook) (void* side, void* trans, void* direct, void* storev, void* m, void* n, void* k, void* l, void* v, void* ldv, void* t, void* ldt, void* c, void* ldc, void* work, void* ldwork);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.zlarzb.f77_blas_function; 
	fn_hook = __flexiblas_hooks->zlarzb.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) side, (void*) trans, (void*) direct, (void*) storev, (void*) m, (void*) n, (void*) k, (void*) l, (void*) v, (void*) ldv, (void*) t, (void*) ldt, (void*) c, (void*) ldc, (void*) work, (void*) ldwork); 
		return;
	} else {
		hook_pos_zlarzb = 0;
		fn_hook((void*) side, (void*) trans, (void*) direct, (void*) storev, (void*) m, (void*) n, (void*) k, (void*) l, (void*) v, (void*) ldv, (void*) t, (void*) ldt, (void*) c, (void*) ldc, (void*) work, (void*) ldwork);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void zlarzb_(char* side, char* trans, char* direct, char* storev, blasint* m, blasint* n, blasint* k, blasint* l, double complex* v, blasint* ldv, double complex* t, blasint* ldt, double complex* c, blasint* ldc, double complex* work, blasint* ldwork) __attribute__((alias(MTS(FC_GLOBAL(zlarzb,ZLARZB)))));
#else
void zlarzb(char* side, char* trans, char* direct, char* storev, blasint* m, blasint* n, blasint* k, blasint* l, double complex* v, blasint* ldv, double complex* t, blasint* ldt, double complex* c, blasint* ldc, double complex* work, blasint* ldwork) __attribute__((alias(MTS(FC_GLOBAL(zlarzb,ZLARZB)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zlarzb_(void* side, void* trans, void* direct, void* storev, void* m, void* n, void* k, void* l, void* v, void* ldv, void* t, void* ldt, void* c, void* ldc, void* work, void* ldwork)
{
	void (*fn) (void* side, void* trans, void* direct, void* storev, void* m, void* n, void* k, void* l, void* v, void* ldv, void* t, void* ldt, void* c, void* ldc, void* work, void* ldwork);

	fn = current_backend->lapack.zlarzb.f77_blas_function; 

		fn((void*) side, (void*) trans, (void*) direct, (void*) storev, (void*) m, (void*) n, (void*) k, (void*) l, (void*) v, (void*) ldv, (void*) t, (void*) ldt, (void*) c, (void*) ldc, (void*) work, (void*) ldwork); 

	return;
}

void flexiblas_real_zlarzb(void* side, void* trans, void* direct, void* storev, void* m, void* n, void* k, void* l, void* v, void* ldv, void* t, void* ldt, void* c, void* ldc, void* work, void* ldwork)  __attribute__((alias("flexiblas_real_zlarzb_")));





/* Chainloader for Hooks */


void flexiblas_chain_zlarzb_(void* side, void* trans, void* direct, void* storev, void* m, void* n, void* k, void* l, void* v, void* ldv, void* t, void* ldt, void* c, void* ldc, void* work, void* ldwork)
{
	void (*fn) (void* side, void* trans, void* direct, void* storev, void* m, void* n, void* k, void* l, void* v, void* ldv, void* t, void* ldt, void* c, void* ldc, void* work, void* ldwork);
	void (*fn_hook) (void* side, void* trans, void* direct, void* storev, void* m, void* n, void* k, void* l, void* v, void* ldv, void* t, void* ldt, void* c, void* ldc, void* work, void* ldwork);

	fn      = current_backend->lapack.zlarzb.f77_blas_function; 

    hook_pos_zlarzb ++;
    if( hook_pos_zlarzb < __flexiblas_hooks->zlarzb.nhook) {
        fn_hook = __flexiblas_hooks->zlarzb.f77_hook_function[hook_pos_zlarzb];
        fn_hook((void*) side, (void*) trans, (void*) direct, (void*) storev, (void*) m, (void*) n, (void*) k, (void*) l, (void*) v, (void*) ldv, (void*) t, (void*) ldt, (void*) c, (void*) ldc, (void*) work, (void*) ldwork);
    } else {
        hook_pos_zlarzb = 0;
		fn((void*) side, (void*) trans, (void*) direct, (void*) storev, (void*) m, (void*) n, (void*) k, (void*) l, (void*) v, (void*) ldv, (void*) t, (void*) ldt, (void*) c, (void*) ldc, (void*) work, (void*) ldwork); 
	}
	return;
}

void flexiblas_chain_zlarzb(void* side, void* trans, void* direct, void* storev, void* m, void* n, void* k, void* l, void* v, void* ldv, void* t, void* ldt, void* c, void* ldc, void* work, void* ldwork)  __attribute__((alias("flexiblas_chain_zlarzb_")));




