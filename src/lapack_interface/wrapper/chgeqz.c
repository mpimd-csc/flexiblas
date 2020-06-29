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



static TLS_STORE uint8_t hook_pos_chgeqz = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(chgeqz,CHGEQZ)(char* job, char* compq, char* compz, blasint* n, blasint* ilo, blasint* ihi, float complex* h, blasint* ldh, float complex* t, blasint* ldt, float complex* alpha, float complex* beta, float complex* q, blasint* ldq, float complex* z, blasint* ldz, float complex* work, blasint* lwork, float* rwork, blasint* info)
#else
void FC_GLOBAL(chgeqz,CHGEQZ)(char* job, char* compq, char* compz, blasint* n, blasint* ilo, blasint* ihi, float complex* h, blasint* ldh, float complex* t, blasint* ldt, float complex* alpha, float complex* beta, float complex* q, blasint* ldq, float complex* z, blasint* ldz, float complex* work, blasint* lwork, float* rwork, blasint* info)
#endif
{
	void (*fn) (void* job, void* compq, void* compz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* t, void* ldt, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rwork, void* info);
	void (*fn_hook) (void* job, void* compq, void* compz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* t, void* ldt, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.chgeqz.f77_blas_function; 
	fn_hook = __flexiblas_hooks->chgeqz.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) job, (void*) compq, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) h, (void*) ldh, (void*) t, (void*) ldt, (void*) alpha, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rwork, (void*) info); 
		return;
	} else {
		hook_pos_chgeqz = 0;
		fn_hook((void*) job, (void*) compq, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) h, (void*) ldh, (void*) t, (void*) ldt, (void*) alpha, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void chgeqz_(char* job, char* compq, char* compz, blasint* n, blasint* ilo, blasint* ihi, float complex* h, blasint* ldh, float complex* t, blasint* ldt, float complex* alpha, float complex* beta, float complex* q, blasint* ldq, float complex* z, blasint* ldz, float complex* work, blasint* lwork, float* rwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(chgeqz,CHGEQZ)))));
#else
void chgeqz(char* job, char* compq, char* compz, blasint* n, blasint* ilo, blasint* ihi, float complex* h, blasint* ldh, float complex* t, blasint* ldt, float complex* alpha, float complex* beta, float complex* q, blasint* ldq, float complex* z, blasint* ldz, float complex* work, blasint* lwork, float* rwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(chgeqz,CHGEQZ)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_chgeqz_(void* job, void* compq, void* compz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* t, void* ldt, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rwork, void* info)
{
	void (*fn) (void* job, void* compq, void* compz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* t, void* ldt, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rwork, void* info);

	fn = current_backend->lapack.chgeqz.f77_blas_function; 

		fn((void*) job, (void*) compq, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) h, (void*) ldh, (void*) t, (void*) ldt, (void*) alpha, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rwork, (void*) info); 

	return;
}

void flexiblas_real_chgeqz(void* job, void* compq, void* compz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* t, void* ldt, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rwork, void* info)  __attribute__((alias("flexiblas_real_chgeqz_")));





/* Chainloader for Hooks */


void flexiblas_chain_chgeqz_(void* job, void* compq, void* compz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* t, void* ldt, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rwork, void* info)
{
	void (*fn) (void* job, void* compq, void* compz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* t, void* ldt, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rwork, void* info);
	void (*fn_hook) (void* job, void* compq, void* compz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* t, void* ldt, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rwork, void* info);

	fn      = current_backend->lapack.chgeqz.f77_blas_function; 

    hook_pos_chgeqz ++;
    if( hook_pos_chgeqz < __flexiblas_hooks->chgeqz.nhook) {
        fn_hook = __flexiblas_hooks->chgeqz.f77_hook_function[hook_pos_chgeqz];
        fn_hook((void*) job, (void*) compq, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) h, (void*) ldh, (void*) t, (void*) ldt, (void*) alpha, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rwork, (void*) info);
    } else {
        hook_pos_chgeqz = 0;
		fn((void*) job, (void*) compq, (void*) compz, (void*) n, (void*) ilo, (void*) ihi, (void*) h, (void*) ldh, (void*) t, (void*) ldt, (void*) alpha, (void*) beta, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) work, (void*) lwork, (void*) rwork, (void*) info); 
	}
	return;
}

void flexiblas_chain_chgeqz(void* job, void* compq, void* compz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* t, void* ldt, void* alpha, void* beta, void* q, void* ldq, void* z, void* ldz, void* work, void* lwork, void* rwork, void* info)  __attribute__((alias("flexiblas_chain_chgeqz_")));




