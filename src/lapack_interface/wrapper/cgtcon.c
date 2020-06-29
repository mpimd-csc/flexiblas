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



static TLS_STORE uint8_t hook_pos_cgtcon = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(cgtcon,CGTCON)(char* norm, blasint* n, float complex* dl, float complex* d, float complex* du, float complex* du2, blasint* ipiv, float* anorm, float* rcond, float complex* work, blasint* info)
#else
void FC_GLOBAL(cgtcon,CGTCON)(char* norm, blasint* n, float complex* dl, float complex* d, float complex* du, float complex* du2, blasint* ipiv, float* anorm, float* rcond, float complex* work, blasint* info)
#endif
{
	void (*fn) (void* norm, void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* anorm, void* rcond, void* work, void* info);
	void (*fn_hook) (void* norm, void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* anorm, void* rcond, void* work, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.cgtcon.f77_blas_function; 
	fn_hook = __flexiblas_hooks->cgtcon.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) info); 
		return;
	} else {
		hook_pos_cgtcon = 0;
		fn_hook((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void cgtcon_(char* norm, blasint* n, float complex* dl, float complex* d, float complex* du, float complex* du2, blasint* ipiv, float* anorm, float* rcond, float complex* work, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(cgtcon,CGTCON)))));
#else
void cgtcon(char* norm, blasint* n, float complex* dl, float complex* d, float complex* du, float complex* du2, blasint* ipiv, float* anorm, float* rcond, float complex* work, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(cgtcon,CGTCON)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_cgtcon_(void* norm, void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* anorm, void* rcond, void* work, void* info)
{
	void (*fn) (void* norm, void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* anorm, void* rcond, void* work, void* info);

	fn = current_backend->lapack.cgtcon.f77_blas_function; 

		fn((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) info); 

	return;
}

void flexiblas_real_cgtcon(void* norm, void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* anorm, void* rcond, void* work, void* info)  __attribute__((alias("flexiblas_real_cgtcon_")));





/* Chainloader for Hooks */


void flexiblas_chain_cgtcon_(void* norm, void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* anorm, void* rcond, void* work, void* info)
{
	void (*fn) (void* norm, void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* anorm, void* rcond, void* work, void* info);
	void (*fn_hook) (void* norm, void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* anorm, void* rcond, void* work, void* info);

	fn      = current_backend->lapack.cgtcon.f77_blas_function; 

    hook_pos_cgtcon ++;
    if( hook_pos_cgtcon < __flexiblas_hooks->cgtcon.nhook) {
        fn_hook = __flexiblas_hooks->cgtcon.f77_hook_function[hook_pos_cgtcon];
        fn_hook((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) info);
    } else {
        hook_pos_cgtcon = 0;
		fn((void*) norm, (void*) n, (void*) dl, (void*) d, (void*) du, (void*) du2, (void*) ipiv, (void*) anorm, (void*) rcond, (void*) work, (void*) info); 
	}
	return;
}

void flexiblas_chain_cgtcon(void* norm, void* n, void* dl, void* d, void* du, void* du2, void* ipiv, void* anorm, void* rcond, void* work, void* info)  __attribute__((alias("flexiblas_chain_cgtcon_")));




