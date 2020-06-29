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



static TLS_STORE uint8_t hook_pos_stftri = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(stftri,STFTRI)(char* transr, char* uplo, char* diag, blasint* n, float* a, blasint* info)
#else
void FC_GLOBAL(stftri,STFTRI)(char* transr, char* uplo, char* diag, blasint* n, float* a, blasint* info)
#endif
{
	void (*fn) (void* transr, void* uplo, void* diag, void* n, void* a, void* info);
	void (*fn_hook) (void* transr, void* uplo, void* diag, void* n, void* a, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.stftri.f77_blas_function; 
	fn_hook = __flexiblas_hooks->stftri.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) transr, (void*) uplo, (void*) diag, (void*) n, (void*) a, (void*) info); 
		return;
	} else {
		hook_pos_stftri = 0;
		fn_hook((void*) transr, (void*) uplo, (void*) diag, (void*) n, (void*) a, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void stftri_(char* transr, char* uplo, char* diag, blasint* n, float* a, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(stftri,STFTRI)))));
#else
void stftri(char* transr, char* uplo, char* diag, blasint* n, float* a, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(stftri,STFTRI)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_stftri_(void* transr, void* uplo, void* diag, void* n, void* a, void* info)
{
	void (*fn) (void* transr, void* uplo, void* diag, void* n, void* a, void* info);

	fn = current_backend->lapack.stftri.f77_blas_function; 

		fn((void*) transr, (void*) uplo, (void*) diag, (void*) n, (void*) a, (void*) info); 

	return;
}

void flexiblas_real_stftri(void* transr, void* uplo, void* diag, void* n, void* a, void* info)  __attribute__((alias("flexiblas_real_stftri_")));





/* Chainloader for Hooks */


void flexiblas_chain_stftri_(void* transr, void* uplo, void* diag, void* n, void* a, void* info)
{
	void (*fn) (void* transr, void* uplo, void* diag, void* n, void* a, void* info);
	void (*fn_hook) (void* transr, void* uplo, void* diag, void* n, void* a, void* info);

	fn      = current_backend->lapack.stftri.f77_blas_function; 

    hook_pos_stftri ++;
    if( hook_pos_stftri < __flexiblas_hooks->stftri.nhook) {
        fn_hook = __flexiblas_hooks->stftri.f77_hook_function[hook_pos_stftri];
        fn_hook((void*) transr, (void*) uplo, (void*) diag, (void*) n, (void*) a, (void*) info);
    } else {
        hook_pos_stftri = 0;
		fn((void*) transr, (void*) uplo, (void*) diag, (void*) n, (void*) a, (void*) info); 
	}
	return;
}

void flexiblas_chain_stftri(void* transr, void* uplo, void* diag, void* n, void* a, void* info)  __attribute__((alias("flexiblas_chain_stftri_")));




