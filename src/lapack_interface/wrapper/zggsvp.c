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



static TLS_STORE uint8_t hook_pos_zggsvp = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zggsvp,ZGGSVP)(char* jobu, char* jobv, char* jobq, blasint* m, blasint* p, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, double* tola, double* tolb, blasint* k, blasint* l, double complex* u, blasint* ldu, double complex* v, blasint* ldv, double complex* q, blasint* ldq, blasint* iwork, double* rwork, double complex* tau, double complex* work, blasint* info)
#else
void FC_GLOBAL(zggsvp,ZGGSVP)(char* jobu, char* jobv, char* jobq, blasint* m, blasint* p, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, double* tola, double* tolb, blasint* k, blasint* l, double complex* u, blasint* ldu, double complex* v, blasint* ldv, double complex* q, blasint* ldq, blasint* iwork, double* rwork, double complex* tau, double complex* work, blasint* info)
#endif
{
	void (*fn) (void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* k, void* l, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* iwork, void* rwork, void* tau, void* work, void* info);
	void (*fn_hook) (void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* k, void* l, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* iwork, void* rwork, void* tau, void* work, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.zggsvp.f77_blas_function; 
	fn_hook = __flexiblas_hooks->zggsvp.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) jobu, (void*) jobv, (void*) jobq, (void*) m, (void*) p, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) tola, (void*) tolb, (void*) k, (void*) l, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) q, (void*) ldq, (void*) iwork, (void*) rwork, (void*) tau, (void*) work, (void*) info); 
		return;
	} else {
		hook_pos_zggsvp = 0;
		fn_hook((void*) jobu, (void*) jobv, (void*) jobq, (void*) m, (void*) p, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) tola, (void*) tolb, (void*) k, (void*) l, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) q, (void*) ldq, (void*) iwork, (void*) rwork, (void*) tau, (void*) work, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void zggsvp_(char* jobu, char* jobv, char* jobq, blasint* m, blasint* p, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, double* tola, double* tolb, blasint* k, blasint* l, double complex* u, blasint* ldu, double complex* v, blasint* ldv, double complex* q, blasint* ldq, blasint* iwork, double* rwork, double complex* tau, double complex* work, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(zggsvp,ZGGSVP)))));
#else
void zggsvp(char* jobu, char* jobv, char* jobq, blasint* m, blasint* p, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, double* tola, double* tolb, blasint* k, blasint* l, double complex* u, blasint* ldu, double complex* v, blasint* ldv, double complex* q, blasint* ldq, blasint* iwork, double* rwork, double complex* tau, double complex* work, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(zggsvp,ZGGSVP)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zggsvp_(void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* k, void* l, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* iwork, void* rwork, void* tau, void* work, void* info)
{
	void (*fn) (void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* k, void* l, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* iwork, void* rwork, void* tau, void* work, void* info);

	fn = current_backend->lapack.zggsvp.f77_blas_function; 

		fn((void*) jobu, (void*) jobv, (void*) jobq, (void*) m, (void*) p, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) tola, (void*) tolb, (void*) k, (void*) l, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) q, (void*) ldq, (void*) iwork, (void*) rwork, (void*) tau, (void*) work, (void*) info); 

	return;
}

void flexiblas_real_zggsvp(void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* k, void* l, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* iwork, void* rwork, void* tau, void* work, void* info)  __attribute__((alias("flexiblas_real_zggsvp_")));





/* Chainloader for Hooks */


void flexiblas_chain_zggsvp_(void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* k, void* l, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* iwork, void* rwork, void* tau, void* work, void* info)
{
	void (*fn) (void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* k, void* l, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* iwork, void* rwork, void* tau, void* work, void* info);
	void (*fn_hook) (void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* k, void* l, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* iwork, void* rwork, void* tau, void* work, void* info);

	fn      = current_backend->lapack.zggsvp.f77_blas_function; 

    hook_pos_zggsvp ++;
    if( hook_pos_zggsvp < __flexiblas_hooks->zggsvp.nhook) {
        fn_hook = __flexiblas_hooks->zggsvp.f77_hook_function[hook_pos_zggsvp];
        fn_hook((void*) jobu, (void*) jobv, (void*) jobq, (void*) m, (void*) p, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) tola, (void*) tolb, (void*) k, (void*) l, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) q, (void*) ldq, (void*) iwork, (void*) rwork, (void*) tau, (void*) work, (void*) info);
    } else {
        hook_pos_zggsvp = 0;
		fn((void*) jobu, (void*) jobv, (void*) jobq, (void*) m, (void*) p, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) tola, (void*) tolb, (void*) k, (void*) l, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) q, (void*) ldq, (void*) iwork, (void*) rwork, (void*) tau, (void*) work, (void*) info); 
	}
	return;
}

void flexiblas_chain_zggsvp(void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* k, void* l, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* iwork, void* rwork, void* tau, void* work, void* info)  __attribute__((alias("flexiblas_chain_zggsvp_")));




