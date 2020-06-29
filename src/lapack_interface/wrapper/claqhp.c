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



static TLS_STORE uint8_t hook_pos_claqhp = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(claqhp,CLAQHP)(char* uplo, blasint* n, float complex* ap, float* s, float* scond, float* amax, char* equed)
#else
void FC_GLOBAL(claqhp,CLAQHP)(char* uplo, blasint* n, float complex* ap, float* s, float* scond, float* amax, char* equed)
#endif
{
	void (*fn) (void* uplo, void* n, void* ap, void* s, void* scond, void* amax, void* equed);
	void (*fn_hook) (void* uplo, void* n, void* ap, void* s, void* scond, void* amax, void* equed);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.claqhp.f77_blas_function; 
	fn_hook = __flexiblas_hooks->claqhp.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) uplo, (void*) n, (void*) ap, (void*) s, (void*) scond, (void*) amax, (void*) equed); 
		return;
	} else {
		hook_pos_claqhp = 0;
		fn_hook((void*) uplo, (void*) n, (void*) ap, (void*) s, (void*) scond, (void*) amax, (void*) equed);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void claqhp_(char* uplo, blasint* n, float complex* ap, float* s, float* scond, float* amax, char* equed) __attribute__((alias(MTS(FC_GLOBAL(claqhp,CLAQHP)))));
#else
void claqhp(char* uplo, blasint* n, float complex* ap, float* s, float* scond, float* amax, char* equed) __attribute__((alias(MTS(FC_GLOBAL(claqhp,CLAQHP)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_claqhp_(void* uplo, void* n, void* ap, void* s, void* scond, void* amax, void* equed)
{
	void (*fn) (void* uplo, void* n, void* ap, void* s, void* scond, void* amax, void* equed);

	fn = current_backend->lapack.claqhp.f77_blas_function; 

		fn((void*) uplo, (void*) n, (void*) ap, (void*) s, (void*) scond, (void*) amax, (void*) equed); 

	return;
}

void flexiblas_real_claqhp(void* uplo, void* n, void* ap, void* s, void* scond, void* amax, void* equed)  __attribute__((alias("flexiblas_real_claqhp_")));





/* Chainloader for Hooks */


void flexiblas_chain_claqhp_(void* uplo, void* n, void* ap, void* s, void* scond, void* amax, void* equed)
{
	void (*fn) (void* uplo, void* n, void* ap, void* s, void* scond, void* amax, void* equed);
	void (*fn_hook) (void* uplo, void* n, void* ap, void* s, void* scond, void* amax, void* equed);

	fn      = current_backend->lapack.claqhp.f77_blas_function; 

    hook_pos_claqhp ++;
    if( hook_pos_claqhp < __flexiblas_hooks->claqhp.nhook) {
        fn_hook = __flexiblas_hooks->claqhp.f77_hook_function[hook_pos_claqhp];
        fn_hook((void*) uplo, (void*) n, (void*) ap, (void*) s, (void*) scond, (void*) amax, (void*) equed);
    } else {
        hook_pos_claqhp = 0;
		fn((void*) uplo, (void*) n, (void*) ap, (void*) s, (void*) scond, (void*) amax, (void*) equed); 
	}
	return;
}

void flexiblas_chain_claqhp(void* uplo, void* n, void* ap, void* s, void* scond, void* amax, void* equed)  __attribute__((alias("flexiblas_chain_claqhp_")));




