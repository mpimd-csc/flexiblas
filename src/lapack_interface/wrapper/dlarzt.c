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



static TLS_STORE uint8_t hook_pos_dlarzt = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlarzt,DLARZT)(char* direct, char* storev, blasint* n, blasint* k, double* v, blasint* ldv, double* tau, double* t, blasint* ldt)
#else
void FC_GLOBAL(dlarzt,DLARZT)(char* direct, char* storev, blasint* n, blasint* k, double* v, blasint* ldv, double* tau, double* t, blasint* ldt)
#endif
{
	void (*fn) (void* direct, void* storev, void* n, void* k, void* v, void* ldv, void* tau, void* t, void* ldt);
	void (*fn_hook) (void* direct, void* storev, void* n, void* k, void* v, void* ldv, void* tau, void* t, void* ldt);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.dlarzt.f77_blas_function; 
	fn_hook = __flexiblas_hooks->dlarzt.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) direct, (void*) storev, (void*) n, (void*) k, (void*) v, (void*) ldv, (void*) tau, (void*) t, (void*) ldt); 
		return;
	} else {
		hook_pos_dlarzt = 0;
		fn_hook((void*) direct, (void*) storev, (void*) n, (void*) k, (void*) v, (void*) ldv, (void*) tau, (void*) t, (void*) ldt);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dlarzt_(char* direct, char* storev, blasint* n, blasint* k, double* v, blasint* ldv, double* tau, double* t, blasint* ldt) __attribute__((alias(MTS(FC_GLOBAL(dlarzt,DLARZT)))));
#else
void dlarzt(char* direct, char* storev, blasint* n, blasint* k, double* v, blasint* ldv, double* tau, double* t, blasint* ldt) __attribute__((alias(MTS(FC_GLOBAL(dlarzt,DLARZT)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlarzt_(void* direct, void* storev, void* n, void* k, void* v, void* ldv, void* tau, void* t, void* ldt)
{
	void (*fn) (void* direct, void* storev, void* n, void* k, void* v, void* ldv, void* tau, void* t, void* ldt);

	fn = current_backend->lapack.dlarzt.f77_blas_function; 

		fn((void*) direct, (void*) storev, (void*) n, (void*) k, (void*) v, (void*) ldv, (void*) tau, (void*) t, (void*) ldt); 

	return;
}

void flexiblas_real_dlarzt(void* direct, void* storev, void* n, void* k, void* v, void* ldv, void* tau, void* t, void* ldt)  __attribute__((alias("flexiblas_real_dlarzt_")));





/* Chainloader for Hooks */


void flexiblas_chain_dlarzt_(void* direct, void* storev, void* n, void* k, void* v, void* ldv, void* tau, void* t, void* ldt)
{
	void (*fn) (void* direct, void* storev, void* n, void* k, void* v, void* ldv, void* tau, void* t, void* ldt);
	void (*fn_hook) (void* direct, void* storev, void* n, void* k, void* v, void* ldv, void* tau, void* t, void* ldt);

	fn      = current_backend->lapack.dlarzt.f77_blas_function; 

    hook_pos_dlarzt ++;
    if( hook_pos_dlarzt < __flexiblas_hooks->dlarzt.nhook) {
        fn_hook = __flexiblas_hooks->dlarzt.f77_hook_function[hook_pos_dlarzt];
        fn_hook((void*) direct, (void*) storev, (void*) n, (void*) k, (void*) v, (void*) ldv, (void*) tau, (void*) t, (void*) ldt);
    } else {
        hook_pos_dlarzt = 0;
		fn((void*) direct, (void*) storev, (void*) n, (void*) k, (void*) v, (void*) ldv, (void*) tau, (void*) t, (void*) ldt); 
	}
	return;
}

void flexiblas_chain_dlarzt(void* direct, void* storev, void* n, void* k, void* v, void* ldv, void* tau, void* t, void* ldt)  __attribute__((alias("flexiblas_chain_dlarzt_")));




