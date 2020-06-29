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



static TLS_STORE uint8_t hook_pos_slasr = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slasr,SLASR)(char* side, char* pivot, char* direct, blasint* m, blasint* n, float* c, float* s, float* a, blasint* lda)
#else
void FC_GLOBAL(slasr,SLASR)(char* side, char* pivot, char* direct, blasint* m, blasint* n, float* c, float* s, float* a, blasint* lda)
#endif
{
	void (*fn) (void* side, void* pivot, void* direct, void* m, void* n, void* c, void* s, void* a, void* lda);
	void (*fn_hook) (void* side, void* pivot, void* direct, void* m, void* n, void* c, void* s, void* a, void* lda);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.slasr.f77_blas_function; 
	fn_hook = __flexiblas_hooks->slasr.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) side, (void*) pivot, (void*) direct, (void*) m, (void*) n, (void*) c, (void*) s, (void*) a, (void*) lda); 
		return;
	} else {
		hook_pos_slasr = 0;
		fn_hook((void*) side, (void*) pivot, (void*) direct, (void*) m, (void*) n, (void*) c, (void*) s, (void*) a, (void*) lda);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void slasr_(char* side, char* pivot, char* direct, blasint* m, blasint* n, float* c, float* s, float* a, blasint* lda) __attribute__((alias(MTS(FC_GLOBAL(slasr,SLASR)))));
#else
void slasr(char* side, char* pivot, char* direct, blasint* m, blasint* n, float* c, float* s, float* a, blasint* lda) __attribute__((alias(MTS(FC_GLOBAL(slasr,SLASR)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slasr_(void* side, void* pivot, void* direct, void* m, void* n, void* c, void* s, void* a, void* lda)
{
	void (*fn) (void* side, void* pivot, void* direct, void* m, void* n, void* c, void* s, void* a, void* lda);

	fn = current_backend->lapack.slasr.f77_blas_function; 

		fn((void*) side, (void*) pivot, (void*) direct, (void*) m, (void*) n, (void*) c, (void*) s, (void*) a, (void*) lda); 

	return;
}

void flexiblas_real_slasr(void* side, void* pivot, void* direct, void* m, void* n, void* c, void* s, void* a, void* lda)  __attribute__((alias("flexiblas_real_slasr_")));





/* Chainloader for Hooks */


void flexiblas_chain_slasr_(void* side, void* pivot, void* direct, void* m, void* n, void* c, void* s, void* a, void* lda)
{
	void (*fn) (void* side, void* pivot, void* direct, void* m, void* n, void* c, void* s, void* a, void* lda);
	void (*fn_hook) (void* side, void* pivot, void* direct, void* m, void* n, void* c, void* s, void* a, void* lda);

	fn      = current_backend->lapack.slasr.f77_blas_function; 

    hook_pos_slasr ++;
    if( hook_pos_slasr < __flexiblas_hooks->slasr.nhook) {
        fn_hook = __flexiblas_hooks->slasr.f77_hook_function[hook_pos_slasr];
        fn_hook((void*) side, (void*) pivot, (void*) direct, (void*) m, (void*) n, (void*) c, (void*) s, (void*) a, (void*) lda);
    } else {
        hook_pos_slasr = 0;
		fn((void*) side, (void*) pivot, (void*) direct, (void*) m, (void*) n, (void*) c, (void*) s, (void*) a, (void*) lda); 
	}
	return;
}

void flexiblas_chain_slasr(void* side, void* pivot, void* direct, void* m, void* n, void* c, void* s, void* a, void* lda)  __attribute__((alias("flexiblas_chain_slasr_")));




