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
 /* Generated: Wed Mar 28 11:20:03 2018 */
        
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



static TLS_STORE uint8_t hook_pos_claqr1 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(claqr1,CLAQR1)(blasint* n, float complex* h, blasint* ldh, float complex* s1, float complex* s2, float complex* v)
#else
void FC_GLOBAL(claqr1,CLAQR1)(blasint* n, float complex* h, blasint* ldh, float complex* s1, float complex* s2, float complex* v)
#endif
{
	void (*fn) (void* n, void* h, void* ldh, void* s1, void* s2, void* v);
	void (*fn_hook) (void* n, void* h, void* ldh, void* s1, void* s2, void* v);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.claqr1.f77_blas_function; 
	fn_hook = __flexiblas_hooks->claqr1.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) h, (void*) ldh, (void*) s1, (void*) s2, (void*) v); 
		return;
	} else {
		hook_pos_claqr1 = 0;
		fn_hook((void*) n, (void*) h, (void*) ldh, (void*) s1, (void*) s2, (void*) v);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void claqr1_(blasint* n, float complex* h, blasint* ldh, float complex* s1, float complex* s2, float complex* v) __attribute__((alias(MTS(FC_GLOBAL(claqr1,CLAQR1)))));
#else
void claqr1(blasint* n, float complex* h, blasint* ldh, float complex* s1, float complex* s2, float complex* v) __attribute__((alias(MTS(FC_GLOBAL(claqr1,CLAQR1)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_claqr1_(void* n, void* h, void* ldh, void* s1, void* s2, void* v)
{
	void (*fn) (void* n, void* h, void* ldh, void* s1, void* s2, void* v);

	fn = current_backend->lapack.claqr1.f77_blas_function; 

		fn((void*) n, (void*) h, (void*) ldh, (void*) s1, (void*) s2, (void*) v); 

	return;
}

void flexiblas_real_claqr1(void* n, void* h, void* ldh, void* s1, void* s2, void* v)  __attribute__((alias("flexiblas_real_claqr1_")));





/* Chainloader for Hooks */


void flexiblas_chain_claqr1_(void* n, void* h, void* ldh, void* s1, void* s2, void* v)
{
	void (*fn) (void* n, void* h, void* ldh, void* s1, void* s2, void* v);
	void (*fn_hook) (void* n, void* h, void* ldh, void* s1, void* s2, void* v);

	fn      = current_backend->lapack.claqr1.f77_blas_function; 

    hook_pos_claqr1 ++;
    if( hook_pos_claqr1 < __flexiblas_hooks->claqr1.nhook) {
        fn_hook = __flexiblas_hooks->claqr1.f77_hook_function[hook_pos_claqr1];
        fn_hook((void*) n, (void*) h, (void*) ldh, (void*) s1, (void*) s2, (void*) v);
    } else {
        hook_pos_claqr1 = 0;
		fn((void*) n, (void*) h, (void*) ldh, (void*) s1, (void*) s2, (void*) v); 
	}
	return;
}

void flexiblas_chain_claqr1(void* n, void* h, void* ldh, void* s1, void* s2, void* v)  __attribute__((alias("flexiblas_chain_claqr1_")));




