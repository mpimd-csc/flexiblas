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



static TLS_STORE uint8_t hook_pos_sgebal = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(sgebal,SGEBAL)(char* job, blasint* n, float* a, blasint* lda, blasint* ilo, blasint* ihi, float* scale, blasint* info)
#else
void FC_GLOBAL(sgebal,SGEBAL)(char* job, blasint* n, float* a, blasint* lda, blasint* ilo, blasint* ihi, float* scale, blasint* info)
#endif
{
	void (*fn) (void* job, void* n, void* a, void* lda, void* ilo, void* ihi, void* scale, void* info);
	void (*fn_hook) (void* job, void* n, void* a, void* lda, void* ilo, void* ihi, void* scale, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.sgebal.f77_blas_function; 
	fn_hook = __flexiblas_hooks->sgebal.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) job, (void*) n, (void*) a, (void*) lda, (void*) ilo, (void*) ihi, (void*) scale, (void*) info); 
		return;
	} else {
		hook_pos_sgebal = 0;
		fn_hook((void*) job, (void*) n, (void*) a, (void*) lda, (void*) ilo, (void*) ihi, (void*) scale, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void sgebal_(char* job, blasint* n, float* a, blasint* lda, blasint* ilo, blasint* ihi, float* scale, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(sgebal,SGEBAL)))));
#else
void sgebal(char* job, blasint* n, float* a, blasint* lda, blasint* ilo, blasint* ihi, float* scale, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(sgebal,SGEBAL)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_sgebal_(void* job, void* n, void* a, void* lda, void* ilo, void* ihi, void* scale, void* info)
{
	void (*fn) (void* job, void* n, void* a, void* lda, void* ilo, void* ihi, void* scale, void* info);

	fn = current_backend->lapack.sgebal.f77_blas_function; 

		fn((void*) job, (void*) n, (void*) a, (void*) lda, (void*) ilo, (void*) ihi, (void*) scale, (void*) info); 

	return;
}

void flexiblas_real_sgebal(void* job, void* n, void* a, void* lda, void* ilo, void* ihi, void* scale, void* info)  __attribute__((alias("flexiblas_real_sgebal_")));





/* Chainloader for Hooks */


void flexiblas_chain_sgebal_(void* job, void* n, void* a, void* lda, void* ilo, void* ihi, void* scale, void* info)
{
	void (*fn) (void* job, void* n, void* a, void* lda, void* ilo, void* ihi, void* scale, void* info);
	void (*fn_hook) (void* job, void* n, void* a, void* lda, void* ilo, void* ihi, void* scale, void* info);

	fn      = current_backend->lapack.sgebal.f77_blas_function; 

    hook_pos_sgebal ++;
    if( hook_pos_sgebal < __flexiblas_hooks->sgebal.nhook) {
        fn_hook = __flexiblas_hooks->sgebal.f77_hook_function[hook_pos_sgebal];
        fn_hook((void*) job, (void*) n, (void*) a, (void*) lda, (void*) ilo, (void*) ihi, (void*) scale, (void*) info);
    } else {
        hook_pos_sgebal = 0;
		fn((void*) job, (void*) n, (void*) a, (void*) lda, (void*) ilo, (void*) ihi, (void*) scale, (void*) info); 
	}
	return;
}

void flexiblas_chain_sgebal(void* job, void* n, void* a, void* lda, void* ilo, void* ihi, void* scale, void* info)  __attribute__((alias("flexiblas_chain_sgebal_")));




