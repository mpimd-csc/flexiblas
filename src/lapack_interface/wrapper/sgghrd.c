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



static TLS_STORE uint8_t hook_pos_sgghrd = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(sgghrd,SGGHRD)(char* compq, char* compz, blasint* n, blasint* ilo, blasint* ihi, float* a, blasint* lda, float* b, blasint* ldb, float* q, blasint* ldq, float* z, blasint* ldz, blasint* info)
#else
void FC_GLOBAL(sgghrd,SGGHRD)(char* compq, char* compz, blasint* n, blasint* ilo, blasint* ihi, float* a, blasint* lda, float* b, blasint* ldb, float* q, blasint* ldq, float* z, blasint* ldz, blasint* info)
#endif
{
	void (*fn) (void* compq, void* compz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* info);
	void (*fn_hook) (void* compq, void* compz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.sgghrd.f77_blas_function; 
	fn_hook = __flexiblas_hooks->sgghrd.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) compq, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) info); 
		return;
	} else {
		hook_pos_sgghrd = 0;
		fn_hook((void*) compq, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void sgghrd_(char* compq, char* compz, blasint* n, blasint* ilo, blasint* ihi, float* a, blasint* lda, float* b, blasint* ldb, float* q, blasint* ldq, float* z, blasint* ldz, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(sgghrd,SGGHRD)))));
#else
void sgghrd(char* compq, char* compz, blasint* n, blasint* ilo, blasint* ihi, float* a, blasint* lda, float* b, blasint* ldb, float* q, blasint* ldq, float* z, blasint* ldz, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(sgghrd,SGGHRD)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_sgghrd_(void* compq, void* compz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* info)
{
	void (*fn) (void* compq, void* compz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* info);

	fn = current_backend->lapack.sgghrd.f77_blas_function; 

		fn((void*) compq, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) info); 

	return;
}

void flexiblas_real_sgghrd(void* compq, void* compz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* info)  __attribute__((alias("flexiblas_real_sgghrd_")));





/* Chainloader for Hooks */


void flexiblas_chain_sgghrd_(void* compq, void* compz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* info)
{
	void (*fn) (void* compq, void* compz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* info);
	void (*fn_hook) (void* compq, void* compz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* info);

	fn      = current_backend->lapack.sgghrd.f77_blas_function; 

    hook_pos_sgghrd ++;
    if( hook_pos_sgghrd < __flexiblas_hooks->sgghrd.nhook) {
        fn_hook = __flexiblas_hooks->sgghrd.f77_hook_function[hook_pos_sgghrd];
        fn_hook((void*) compq, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) info);
    } else {
        hook_pos_sgghrd = 0;
		fn((void*) compq, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) info); 
	}
	return;
}

void flexiblas_chain_sgghrd(void* compq, void* compz, void* n, void* ilo, void* ihi, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* info)  __attribute__((alias("flexiblas_chain_sgghrd_")));




