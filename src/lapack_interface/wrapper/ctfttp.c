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



static TLS_STORE uint8_t hook_pos_ctfttp = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(ctfttp,CTFTTP)(char* transr, char* uplo, blasint* n, float complex* arf, float complex* ap, blasint* info)
#else
void FC_GLOBAL(ctfttp,CTFTTP)(char* transr, char* uplo, blasint* n, float complex* arf, float complex* ap, blasint* info)
#endif
{
	void (*fn) (void* transr, void* uplo, void* n, void* arf, void* ap, void* info);
	void (*fn_hook) (void* transr, void* uplo, void* n, void* arf, void* ap, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.ctfttp.f77_blas_function; 
	fn_hook = __flexiblas_hooks->ctfttp.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) transr, (void*) uplo, (void*) n, (void*) arf, (void*) ap, (void*) info); 
		return;
	} else {
		hook_pos_ctfttp = 0;
		fn_hook((void*) transr, (void*) uplo, (void*) n, (void*) arf, (void*) ap, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void ctfttp_(char* transr, char* uplo, blasint* n, float complex* arf, float complex* ap, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(ctfttp,CTFTTP)))));
#else
void ctfttp(char* transr, char* uplo, blasint* n, float complex* arf, float complex* ap, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(ctfttp,CTFTTP)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_ctfttp_(void* transr, void* uplo, void* n, void* arf, void* ap, void* info)
{
	void (*fn) (void* transr, void* uplo, void* n, void* arf, void* ap, void* info);

	fn = current_backend->lapack.ctfttp.f77_blas_function; 

		fn((void*) transr, (void*) uplo, (void*) n, (void*) arf, (void*) ap, (void*) info); 

	return;
}

void flexiblas_real_ctfttp(void* transr, void* uplo, void* n, void* arf, void* ap, void* info)  __attribute__((alias("flexiblas_real_ctfttp_")));





/* Chainloader for Hooks */


void flexiblas_chain_ctfttp_(void* transr, void* uplo, void* n, void* arf, void* ap, void* info)
{
	void (*fn) (void* transr, void* uplo, void* n, void* arf, void* ap, void* info);
	void (*fn_hook) (void* transr, void* uplo, void* n, void* arf, void* ap, void* info);

	fn      = current_backend->lapack.ctfttp.f77_blas_function; 

    hook_pos_ctfttp ++;
    if( hook_pos_ctfttp < __flexiblas_hooks->ctfttp.nhook) {
        fn_hook = __flexiblas_hooks->ctfttp.f77_hook_function[hook_pos_ctfttp];
        fn_hook((void*) transr, (void*) uplo, (void*) n, (void*) arf, (void*) ap, (void*) info);
    } else {
        hook_pos_ctfttp = 0;
		fn((void*) transr, (void*) uplo, (void*) n, (void*) arf, (void*) ap, (void*) info); 
	}
	return;
}

void flexiblas_chain_ctfttp(void* transr, void* uplo, void* n, void* arf, void* ap, void* info)  __attribute__((alias("flexiblas_chain_ctfttp_")));




